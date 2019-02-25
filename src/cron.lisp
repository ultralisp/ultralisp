(defpackage #:ultralisp/cron
  (:use #:cl)
  (:import-from #:cl-cron)
  (:import-from #:log4cl)
  (:import-from #:ultralisp/models/project
                #:get-all-projects)
  (:import-from #:ultralisp/models/check
                #:make-via-cron-check)
  (:import-from #:ultralisp/utils
                #:make-request-id)
  (:import-from #:log4cl-json
                #:with-log-unhandled
                #:with-fields)
  (:import-from #:ultralisp/pipeline/checking
                #:perform-pending-checks)
  (:import-from #:ultralisp/db
                #:with-connection)
  (:import-from #:ultralisp/builder
                #:prepare-pending-version
                #:build-prepared-versions)
  (:export
   #:list-cron-jobs
   #:delete-all-cron-jobs
   #:setup
   #:stop
   #:start
   #:*cron-jobs-hash*))
(in-package ultralisp/cron)


(defmacro deftask (name (&key (need-connection t)) &body body)
  "Defines a cron task function with following properties:

   * Each call has it's own unique id in log messages.
   * Unhandled exceptions will be logged along with their tracebacks.
   * A new database connection and trasaction will be started for each execution."
  
  (let ((body (if need-connection
                  `(with-connection ()
                     ,@body)
                  `(progn ,@body))))
    `(defun ,name ()
       (with-fields (:request-id (make-request-id))
         (log:debug "Running cron task" ',name)
         (handler-bind ((error (lambda (condition)
                                 (if slynk-api:*emacs-connection*
                                     (invoke-debugger condition)
                                     (return-from ,name nil)))))
           (with-log-unhandled ()
             ,body))
         (log:debug "Cron task is done" ',name)))))


(deftask perform-checks ()
  (perform-pending-checks))


(deftask build-version (:need-connection nil)
  ;; Here we get separate connections and transaction
  ;; because when we do version build, it will be
  ;; performed by a remote worker and prepared version
  ;; should be already committed to the database.
  (with-connection ()
    (prepare-pending-version))
  (with-connection ()
    (build-prepared-versions)))


(deftask create-cron-checks ()
  (mapc #'make-via-cron-check
        (get-all-projects :only-enabled t)))


(defun list-cron-jobs ()
  (loop for key being the hash-key of cl-cron::*cron-jobs-hash*
        collect key))


(defun delete-all-cron-jobs ()
  (loop for key in (list-cron-jobs)
        do (cl-cron:delete-cron-job key)))


(defun setup ()
  "Creates all cron jobs needed for Ultralisp. Does not start them. Call start for that."
  (log:debug "Creating cron jobs")
  ;; Run every minute
  (cl-cron:make-cron-job 'perform-checks
                         :hash-key 'perform-checks)
  ;; Run every 5 minutes
  (cl-cron:make-cron-job 'build-version
                         :hash-key 'build-version
                         :step-min 5)
  
  ;; Every 24 hour we'll recheck all projects by cron
  (cl-cron:make-cron-job 'create-cron-checks
                         :hash-key 'create-cron-checks
                         :hour 0
                         :minute 0))


(defun start ()
  "Creates all cron jobs needed for Ultralisp. Does not start them. Call start for that."
  (log:debug "Starting cron thread")
  (cl-cron:start-cron))


(defun stop ()
  "Creates all cron jobs needed for Ultralisp. Does not start them. Call start for that."
  (log:debug "Stopping cron thread")
  (cl-cron:stop-cron))


;; Here we patch this function and replace it because
;; original tries to write into a file cl-cron.log
(defun cl-cron:log-cron-message (message &optional (type "error"))
  (if (string-equal type "error")
      (log:error message)
      (log:info message)))
