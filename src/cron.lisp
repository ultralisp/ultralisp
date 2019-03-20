(defpackage #:ultralisp/cron
  (:use #:cl)
  (:import-from #:cl-cron)
  (:import-from #:log4cl)
  (:import-from #:ultralisp/models/project
                #:get-disable-reason
                #:get-all-projects)
  (:import-from #:ultralisp/models/check
                #:get-last-project-check
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
  (:import-from #:local-time
                #:now)
  (:import-from #:mito
                #:object-updated-at)
  (:import-from #:local-time-duration
                #:duration-maximum
                #:duration-as
                #:duration-
                #:timestamp-difference
                #:duration-minimum)
  (:export
   #:list-cron-jobs
   #:delete-all-cron-jobs
   #:setup
   #:stop
   #:start
   #:*cron-jobs-hash*
   #:get-time-of-the-next-check))
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


(defun get-time-of-the-next-check (project)
  "Время проверки должно быть не больше недели и не меньше 1 часа.
   При этом, на него должны влиять:

   * Время когда последний раз обновлялся проект. Если обновлялся
     недавно, то скорее всего он активно развивается, но и его надо
     проверять чаще.
   * Время которое было затрачено на предыдущую проверку. Чем оно больше
     тем реже надо проверять."
  (check-type project ultralisp/models/project:project)
  (let* ((year-ago (ultralisp/utils:time-in-past :day 365))
         (week (local-time-duration:duration :week 1))
         (hour (local-time-duration:duration :hour 1))
         (last-check (get-last-project-check project))
         (checked-at (if last-check
                         (object-updated-at last-check)
                         ;; If project never checked,
                         ;; then pretend the check was a year ago.
                         ;; We need this do do the timestamp math correct.
                         year-ago))
         (updated-at (object-updated-at project))
         (update-interval (duration-minimum
                           (duration-maximum
                            (timestamp-difference checked-at
                                                  updated-at)
                            hour)
                           week))
         (time-for-check (local-time-duration:timestamp-duration+
                           checked-at
                           update-interval)))
    (values time-for-check)))


(deftask create-cron-checks ()
  (loop with now = (now)
        for project in (get-all-projects)
        for time-for-check = (get-time-of-the-next-check project)
        when (and (local-time:timestamp< time-for-check
                                         now)
                  (not (eql (get-disable-reason project)
                            :manual)))
          do (log:info "Creating cron check for" project)
             (make-via-cron-check project)))


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
  
  ;; Every 15 minutes we'll create checks for project which need it
  (cl-cron:make-cron-job 'create-cron-checks
                         :hash-key 'create-cron-checks
                         :step-min 15))


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


