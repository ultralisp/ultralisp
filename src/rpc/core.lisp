(defpackage #:ultralisp/rpc/core
  (:use #:cl)
  (:import-from #:log4cl)
  (:import-from #:slynk)
  (:import-from #:cl-store)
  (:import-from #:cl-gearman)
  
  (:import-from #:ultralisp/slynk)
  (:import-from #:ultralisp/rpc/command
                #:task-with-commands
                #:with-commands-processor)
  (:import-from #:ultralisp/variables
                #:get-gearman-server
                #:get-postgres-ro-pass
                #:get-postgres-ro-user
                #:get-postgres-host
                #:get-postgres-dbname)
  (:export
   #:submit-task))
(in-package ultralisp/rpc/core)


(defun serialize (object)
  (base64:usb8-array-to-base64-string
   (flex:with-output-to-sequence (stream)
     (cl-store:store object stream))))

(defun deserialize (base64-string)
  (flex:with-input-from-sequence
      (stream (base64:base64-string-to-usb8-array base64-string))
    (cl-store:restore stream)))


(defun gearman-call (function-name &rest args)
  (let ((returned-value nil))
    ;; May be we need to process a CL-GEARMAN::GEARMAN-CONNECTION-ERROR
    ;; here. This condition will be thrown in case, if gearman server is
    ;; unavailable.
    (cl-gearman:with-client (client (get-gearman-server))
      (log:info "Submitting job")
      (let* ((raw-result (cl-gearman:submit-job client
                                                function-name
                                                :arg (serialize args)))
             (result (deserialize raw-result)))
        (log:info "Result received" result)
        (setf returned-value result)))
    returned-value))


(defun submit-task (func &rest args)
  "Submits a task to one of remote workers and waits for the result."
  (check-type func symbol)
  
  (with-commands-processor 
    (apply #'gearman-call
           "task-with-commands"
           (get-postgres-host)
           (get-postgres-ro-user)
           (get-postgres-ro-pass)
           (get-postgres-dbname)
           func args)))

