(defpackage #:ultralisp/lfarm/command
  (:use #:cl)
  (:import-from #:serapeum)
  (:export
   #:defcommand
   #:submit-task-with-commands
   #:with-commands-processor
   #:task-with-commands))
(in-package ultralisp/lfarm/command)


(defparameter *catch-commands* nil)


(serapeum:defvar-unbound *catched*
  "A buffer to store all commands to be executed in master process.
   Bound by task-with-commands function.")


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
                        (list ,@args))
                  *catched*))
           (t
            ,@body))
     (values)))


(defun task-with-commands (name &rest args)
  "A helper task to catch all commands executed by a worker."
  (let ((*catch-commands* t)
        (*catched* nil))

    (let ((result (apply name args)))
      (cons result *catched*))))


;; (defun submit-task-with-commands (name &rest args)
;;   (log:info "Submitting task to remote machine")
;;   (let* ((response
;;            (apply #'submit-task 'ultralisp/lfarm/command::task-with-commands
;;                   name
;;                   args))
;;          (result (car response))
;;          (commands (cdr response)))
;;     (loop for command in commands
;;           for func-name = (car command)
;;           for func-args = (cdr command)
;;           do (apply func-name
;;                     func-args))
;;     result))


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



