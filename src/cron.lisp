(defpackage #:ultralisp/cron
  (:use #:cl)
  (:import-from #:cl-cron)
  (:import-from #:log4cl)
  (:import-from #:ultralisp/search)
  (:import-from #:ultralisp/models/project
                #:get-disable-reason
                #:get-all-projects)
  (:import-from #:ultralisp/models/check
                #:get-last-project-check
                #:make-via-cron-check)
  (:import-from #:ultralisp/utils
                #:make-request-id)
  (:import-from #:log4cl-extras/error
                #:with-log-unhandled)
  (:import-from #:log4cl-extras/context
                #:with-fields)
  (:import-from #:ultralisp/pipeline/checking
                #:perform-pending-checks)
  (:import-from #:ultralisp/db
                #:with-lock
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
       (with-fields (:request-id (make-request-id)
                     :check-name ,(string-downcase
                                   (symbol-name name)))
         ;; TODO: Temporary this is logged as WARN
         ;;       we need to implement "crossfinger" logging facility
         ;;       in the log4cl-extras and change it back to the INFO
         (log:warn "Running cron task" ',name)
         (handler-bind ((error (lambda (condition)
                                 (if slynk-api:*emacs-connection*
                                     (invoke-debugger condition)
                                     (return-from ,name nil)))))
           (with-log-unhandled ()
             (handler-case (progn ,body)
               (ultralisp/db:lock-timeout ()
                 (log:debug "Unable to acquire log, seems task already in progress.")))))
         (log:debug "Cron task is done" ',name)))))


(deftask perform-checks ()
  (perform-pending-checks))


(deftask remove-old-checks ()
  (mito:execute-sql "DELETE FROM public.check WHERE processed_at < now() - '7 day'::interval"))


(deftask build-version (:need-connection nil)
  ;; Here we get separate connections and transaction
  ;; because when we do version build, it will be
  ;; performed by a remote worker and prepared version
  ;; should be already committed to the database.
  
  (log:info "Building a new version if needed")
  (with-connection ()
    (prepare-pending-version))
  (with-connection ()
    (build-prepared-versions))
  (log:info "Building a new version if needed DONE"))


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


(deftask index-projects ()
  (log:info "Trying to index projects")
  (with-lock ("indexing-projects")
    (log:info "Log aquired")
    (unwind-protect
         (ultralisp/search:index-projects :limit 1)
      (log:info "Unwinding after the indexing projects"))
    (log:info "Task is done")))


(defun list-cron-jobs ()
  (loop for key being the hash-key of cl-cron::*cron-jobs-hash*
        collect key))


(defvar *stopped* nil
  "This flag will be set by `stop' function to true and used by `setup' to not recreate cron jobs on server restart if they were stopeed manually.")


(defun delete-all-cron-jobs ()
  (loop for key in (list-cron-jobs)
        do (cl-cron:delete-cron-job key)))


(defun setup (&key (force nil))
  "Creates all cron jobs needed for Ultralisp. Does not start them. Call start for that."
  (log:debug "Creating cron jobs")
  (when (or (null (uiop:getenv "CRON_DISABLED"))
            force)
    ;; Run every minute
    (cl-cron:make-cron-job 'perform-checks
                           :hash-key 'perform-checks)

    ;; Evey hour remove old checks
    (cl-cron:make-cron-job 'remove-old-checks
                           :hash-key 'remove-old-checks
                           :step-min 60)

    ;; Run every 5 minutes
    (cl-cron:make-cron-job 'build-version
                           :hash-key 'build-version
                           :step-min 5)
  
    ;; Every 15 minutes we'll create checks for project which need it
    (cl-cron:make-cron-job 'create-cron-checks
                           :hash-key 'create-cron-checks
                           :step-min 15)

    ;; Every five minutes we'll index projects to make them searchable
    (cl-cron:make-cron-job 'index-projects
                           :hash-key 'index-projects
                           :step-min 1))
  (values))


(defun start (&key force)
  "Creates all cron jobs needed for Ultralisp. Does not start them. Call start for that."
  (cond
    ((and *stopped*
          (not force))
     (log:warn "Cron was stopped manually, add :force t to start it agaun"))
    (t 
     (log:debug "Starting cron thread")
     (cl-cron:start-cron)
     (setf *stopped* nil))))


(defun stop ()
  "Creates all cron jobs needed for Ultralisp. Does not start them. Call start for that."
  (log:debug "Stopping cron thread")
  (cl-cron:stop-cron)
  (setf *stopped* t))


;; Here we patch this function and replace it because
;; original tries to write into a file cl-cron.log
(defun cl-cron:log-cron-message (message &optional (type "error"))
  (if (string-equal type "error")
      (log:error message)
      (log:info message)))
