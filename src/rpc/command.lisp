(defpackage #:ultralisp/rpc/command
  (:use #:cl)
  (:import-from #:serapeum)
  (:import-from #:ultralisp/slynk)
  (:import-from #:cl-strings
                #:starts-with)
  (:import-from #:log4cl-extras/error
                #:with-log-unhandled)
  (:import-from #:ultralisp/db
                #:with-connection)
  (:export
   #:defcommand
   #:submit-task-with-commands
   #:with-commands-processor
   #:task-with-commands))
(in-package ultralisp/rpc/command)


(defparameter *catch-commands* nil)


(serapeum:defvar-unbound *catched*
  "A buffer to store all commands to be executed in master process.
   Bound by task-with-commands function.")


(defun process-special-symbols (list)
  "Receives a list and returns another list with keyword arguments expanded like this.

   (list foo bar &key force) -> (list foo bar :force force)"
  (loop with key-mode = nil
        with result = nil
        for item in list
        do (cond
             ((string-equal (symbol-name item) "&key")
              (setf key-mode t))
             ((starts-with (symbol-name item) "&")
              (error "Special symbols like \"~A\" aren't supported yet." item))
             (key-mode (uiop:appendf result
                                     (list (alexandria:make-keyword item)
                                           item)))
             (t (uiop:appendf result
                              (list item))))
        finally (return result)))


(defmacro defcommand (name (&rest args) &body body)
  "Defines a function which can be executed immediately,
   or delayed if *catch-commands* is t.

   We need this to allow remote worker which has a read-only
   access to the database to store some data by sending
   commands back to the main process.

   defcommand macro makes it easier to write and test code
   in the main process. The code will work same way as executed.
   on the worker with read-only connect.

   Command should't return any value, because on worker their
   execution will be delayed and command will be executed in master
   process.

   This is why this defun return empty value list."
  `(defun ,name (,@args)
     (cond (*catch-commands*
            (unless (boundp '*catched*)
              (error "Variable *catched* command is unbound. Seems you've set *catch-command* manually. Use task-with-commands instead!"))
            (push (cons ',name
                        (list ,@(process-special-symbols args)))
                  *catched*))
           (t
            ,@body))
     (values)))


(defun task-with-commands (db-host db-user db-pass name &rest args)
  "A helper task to catch all commands executed by a worker."
  (let ((*catch-commands* t)
        (*catched* nil))

    (handler-bind ((error (lambda (condition)
                            ;; We want debugger to popup if we've connected to
                            ;; the process from SLY
                            ;; (invoke-debugger condition)
                            (when ultralisp/slynk:*connections*
                              (invoke-debugger condition)))))
      (with-log-unhandled ()
        (with-connection (:host db-host
                          :username db-user
                          :password db-pass)
          (let ((result (apply name args)))
            (cons result *catched*)))))))


(defmacro with-commands-processor (&body body)
  "Executes body and it should return a cons.
   Car of this cons is returned as a result,
   but before it will be returned, all catched
   commands are executed."
  ;; TODO: make this code a function and call it using the macro
  `(let ((response
           (progn ,@body)))
     (check-type response cons)
     
     (let ((result (car response))
           (commands (cdr response)))
       (loop for command in commands
             for func-name = (car command)
             for func-args = (cdr command)
             do (apply func-name
                       func-args))
       result)))



