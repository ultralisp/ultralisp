(defpackage #:ultralisp/cron
  (:use #:cl)
  (:import-from #:cl-cron)
  (:import-from #:log4cl)
  (:import-from #:ultralisp/utils
                #:make-request-id)
  (:import-from #:log4cl-json
                #:with-fields)
  (:import-from #:ultralisp/downloader/base
                #:perform-pending-checks-and-trigger-version-build)
  (:import-from #:ultralisp/db
                #:with-connection)
  (:export
   #:list-cron-jobs
   #:delete-all-cron-jobs
   #:setup
   #:stop
   #:start))
(in-package ultralisp/cron)


(defun perform-checks ()
  (with-fields (:request-id (make-request-id))
    (log4cl-json:with-log-unhandled ()
      (with-connection
          (perform-pending-checks-and-trigger-version-build)))))


(defun list-cron-jobs ()
  (loop for key being the hash-key of cl-cron::*cron-jobs-hash*
        collect key))


(defun delete-all-cron-jobs ()
  (loop for key in (list-cron-jobs)
        do (cl-cron:delete-cron-job key)))


(defun setup ()
  "Creates all cron jobs needed for Ultralisp. Does not start them. Call start for that."
  (log:info "Creating cron jobs")
  (cl-cron:make-cron-job 'perform-checks
                         :hash-key 'perform-checks))


(defun start ()
  "Creates all cron jobs needed for Ultralisp. Does not start them. Call start for that."
  (log:info "Starting cron thread")
  (cl-cron:start-cron))


(defun stop ()
  "Creates all cron jobs needed for Ultralisp. Does not start them. Call start for that."
  (log:info "Stopping cron thread")
  (cl-cron:stop-cron))


;; Here we patch this function and replace it because
;; original tries to write into a file cl-cron.log
(defun cl-cron:log-cron-message (message &optional (type "error"))
  (if (string-equal type "error")
      (log:error message)
      (log:info message)))
