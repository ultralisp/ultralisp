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
  (:import-from #:rutils
                #:fmt)
  (:export
   #:submit-task
   #:serialize
   #:deserialize
   #:submit-task-to-lispworks))
(in-package ultralisp/rpc/core)


;; We need this code to store/restore secret values,
;; because internally secret value is stored in the
;; closure, attached to an uninterned symbol.
;;
;; Without this code, we'll unable to pass secret values
;; from the app to a worker, because the will be restored
;; in a wrong state and we'll receive this error on revail:
;; 
;;     Condition: The function COMMON-LISP:NIL is undefined.
;;
;; NOTE: Don't use this code for a permanent store!
;; For storing secret values in a permanent store, they
;; should be encrypted.
(defparameter +secret-value-code+
  (cl-store:register-code 200
                          'secret-values:secret-value))

(cl-store:defstore-cl-store (obj secret-values:secret-value stream)
  (cl-store:output-type-code +secret-value-code+ stream)
  (cl-store:store-object (secret-values:reveal-value obj)
                         stream))


(cl-store:defrestore-cl-store (secret-values:secret-value stream)
  (secret-values:conceal-value
   (cl-store:restore-object stream)))


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


(defun submit-task (func &key (lisp-implementation :sbcl)
                              args)
  "Submits a task to one of remote workers and waits for the result."
  (check-type func symbol)
  
  (with-commands-processor 
    (apply #'gearman-call
           (fmt "~A-task-with-commands"
                (string-downcase
                 (symbol-name lisp-implementation)))
           (get-postgres-host)
           (get-postgres-ro-user)
           (get-postgres-ro-pass)
           (get-postgres-dbname)
           func args)))


