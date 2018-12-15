(defpackage #:ultralisp/cron
  (:use #:cl)
  (:import-from #:cl-cron)
  (:import-from #:log4cl)
  (:import-from #:ultralisp/utils
                #:make-request-id)
  (:import-from #:log4cl-json
                #:with-log-unhandled
                #:with-fields)
  (:import-from #:ultralisp/downloader/base
                #:perform-pending-checks-and-trigger-version-build)
  (:import-from #:ultralisp/db
                #:with-connection)
  (:import-from #:ultralisp/builder
                #:build-pending-version)
  (:export
   #:list-cron-jobs
   #:delete-all-cron-jobs
   #:setup
   #:stop
   #:start))
(in-package ultralisp/cron)


(defmacro deftask (name &body body)
  "Defines a cron task function with following properties:

   * Each call has it's own unique id in log messages.
   * Unhandles exceptions will be logged along with their tracebacks.
   * A new database connection and trasaction will be started."
  
  `(defun ,name ()
     (with-fields (:request-id (make-request-id))
       (log:debug "Running cron task" ',name)
       (ignore-errors
         (with-log-unhandled ()
           (with-connection ()
                ,@body))))))


(deftask perform-checks ()
  (perform-pending-checks-and-trigger-version-build))


(deftask build-version ()
  (build-pending-version))


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
                         :step-min 5))


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
