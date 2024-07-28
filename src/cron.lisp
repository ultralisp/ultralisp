(defpackage #:ultralisp/cron
  (:use #:cl)
  (:import-from #:cl-cron)
  (:import-from #:log)
  (:import-from #:slynk-api)
  (:import-from #:ultralisp/search)
  (:import-from #:ultralisp/models/project
                #:project-name
                #:source->project
                #:get-disable-reason
                #:get-all-projects)
  (:import-from #:ultralisp/models/check
                #:get-last-source-check
                #:get-last-project-checks
                #:make-check)
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
                #:prepare-pending-dists
                #:build-prepared-dists)
  (:import-from #:local-time
                #:now)
  (:import-from #:mito
                #:execute-sql
                #:object-updated-at)
  (:import-from #:local-time-duration
                #:duration-maximum
                #:duration-as
                #:duration-
                #:timestamp-difference
                #:duration-minimum)
  (:import-from #:ultralisp/models/source
                #:get-all-sources)
  (:import-from #:alexandria
                #:with-output-to-file)
  (:import-from #:mito.class
                #:table-primary-key)
  (:import-from #:mito.class
                #:table-name)
  (:import-from #:mito.class
                #:table-name)
  (:import-from #:sxql
                #:where
                #:update
                #:make-sql-symbol)
  (:import-from #:mito.dao
                #:make-set-clause)
  (:import-from #:ultralisp/models/versioned
                #:object-version)
  (:export
   #:list-cron-jobs
   #:delete-all-cron-jobs
   #:setup
   #:stop
   #:start
   #:*cron-jobs-hash*
   #:get-time-of-the-next-check
   #:simulate-cron))
(in-package #:ultralisp/cron)


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
         ;;       because only WARN and ERROR are logged on production.
         ;;       We need to implement "crossfinger" logging facility
         ;;       in the log4cl-extras and change it back to the INFO.
         (log:info "Running cron task" ',name)
         (handler-bind ((serious-condition
                          (lambda (condition)
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


(deftask perform-lispworks-checks ()
  (perform-pending-checks :lisp-implementation :lispworks))


(deftask remove-old-checks ()
  (mito:execute-sql "DELETE FROM public.check2 WHERE processed_at < now() - '180 day'::interval"))


(deftask build-dists (:need-connection nil)
  ;; Here we get separate connections and transaction
  ;; because when we do version build, it will be
  ;; performed by a remote worker and prepared version
  ;; should be already committed to the database.
  
  (log:info "Building a new version if needed")
  (with-connection ()
    (prepare-pending-dists))
  
  (with-connection ()
    (build-prepared-dists))
  (log:info "Building a new version if needed DONE"))


(defun get-time-of-the-next-check (source)
  "Время проверки должно быть не больше недели и не меньше 1 часа.
   При этом, на него должны влиять:

   * Время когда последний раз обновлялся source. Если обновлялся
     недавно, то скорее всего он активно развивается, но и его надо
     проверять чаще.
   * Время которое было затрачено на предыдущую проверку. Чем оно больше
     тем реже надо проверять."
  
  (check-type source ultralisp/models/source:source)
  
  (let* ((year-ago (ultralisp/utils:time-in-past :day 365))
         (week (local-time-duration:duration :week 1))
         (hour (local-time-duration:duration :hour 1))
         (last-check (get-last-source-check source))
         (checked-at (if last-check
                         (object-updated-at last-check)
                         ;; If project never checked,
                         ;; then pretend the check was a year ago.
                         ;; We need this do do the timestamp math correct.
                         year-ago))
         (updated-at (object-updated-at source))
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
  "TODO: Rewrite this cron task to create checks in future.

   Otherwise it everytime goes through all source every 5 minutes,
   and overtime this operation will take longer time because there
   will be more sources.

   Good place for optimization :)"
  (loop with now = (now)
        with all-sources = (get-all-sources)
        for source in all-sources
        for project = (source->project source)
        for time-for-check = (get-time-of-the-next-check source)
        when (local-time:timestamp< time-for-check
                                    now)
        do (with-fields (:project-name (project-name project)
                         :project-id (mito:object-id project)
                         :project-version (object-version project))
             (log:info "Creating cron check for" source)
             (make-check source :via-cron))))


(defun get-check-times (&optional (filename "/tmp/timestamps"))
  (with-connection ()
    (with-output-to-file (s filename)
      (loop with now = (now)
            with all-sources = (get-all-sources)
            for source in all-sources
            for time-for-check = (get-time-of-the-next-check source)
            when (local-time:timestamp> time-for-check
                                        now)
            do (format s "~A~%" time-for-check)))))


(defun save-updated (obj)
  (let ((primary-key (table-primary-key (class-of obj))))
    (unless primary-key
      (error 'no-primary-keys :table (table-name (class-of obj))))

    (execute-sql
     (update (make-sql-symbol (table-name (class-of obj)))
       (make-set-clause obj)
       (where
        `(:and ,@(mapcar (lambda (key)
                           `(:= ,(mito.util:unlispify key) ,(slot-value obj key)))
                         primary-key)))))))

(defun normalize-check-times ()
  "Resets checked-at fields for all sources, to make them evenly distributed along the week.

   This should make project checking less stressful for Ultralisp."
  (with-connection ()
    (loop with now = (now)
          with start-from = (ultralisp/utils:time-in-past :day 7)
          with current-ts = start-from
          with all-sources = (get-all-sources)
          with step = (let ((d (local-time-duration:duration/
                                (local-time-duration:timestamp-difference now start-from)
                                (length all-sources))))
                        (local-time-duration:duration
                         :sec (local-time-duration:duration-as d :sec)))
          for source in all-sources
          for last-check = (get-last-source-check source)
          when last-check
            do (setf (object-updated-at last-check)
                     current-ts)
               (setf current-ts
                     (local-time-duration:timestamp-duration+ current-ts
                                                              step))
               (save-updated last-check))))


(deftask index-projects ()
  (log:info "Trying to index projects")
  (with-lock ("indexing-projects")
    (log:info "Log aquired")
    (ultralisp/search:index-projects :limit 1)
    (log:info "Task is done")))


(deftask delete-old-docs-from-index ()
  (log:info "Cleaning elastic search index")
  (with-lock ("indexing-projects")
    (log:info "Log aquired")
    (ultralisp/search:delete-documents-which-should-not-be-in-the-index)
    (log:info "Task is done")))


(defun list-cron-jobs ()
  (loop for key being the hash-key of cl-cron::*cron-jobs-hash*
        collect key))


(defvar *stopped* nil
  "This flag will be set by `stop' function to true and used by `setup' to not recreate cron jobs on server restart if they were stopeed manually.")


(defun delete-all-cron-jobs ()
  (loop for key in (list-cron-jobs)
        do (cl-cron:delete-cron-job key)))


(defun simulate-cron (&key (index t)
                           (create-checks t)
                           (lispworks t))
  "When cron is disabled, you can use this function in the REPL
   to do everything for complete Ultralisp update cycle."

  (remove-old-checks)
  (when create-checks
    (create-cron-checks))
  (perform-checks)
  (when lispworks
    (perform-lispworks-checks))
  (build-dists)
  
  (when index
    (index-projects)
    (delete-old-docs-from-index))
  
  (values))

(defun setup (&key (force nil))
  "Creates all cron jobs needed for Ultralisp. Does not start them. Call start for that."
  (log:debug "Creating cron jobs")
  (when (or (null (uiop:getenv "CRON_DISABLED"))
            force)
    ;; Run every minute
    (cl-cron:make-cron-job 'perform-checks
                           :hash-key 'perform-checks)
    ;; LispWorks checks will run less frequently, because
    ;; their worker is not in production yet
    (cl-cron:make-cron-job 'perform-lispworks-checks
                           :hash-key 'perform-lispworks-checks
                           :step-min 15)

    ;; Every hour remove old checks
    (cl-cron:make-cron-job 'remove-old-checks
                           :hash-key 'remove-old-checks
                           :step-min 60)
    
    ;; Run every 5 minutes
    (cl-cron:make-cron-job 'build-dists
                           :hash-key 'build-dists
                           :step-min 5)
  
    ;; Every 15 minutes we'll create checks for project which need it
    (cl-cron:make-cron-job 'create-cron-checks
                           :hash-key 'create-cron-checks
                           :step-min 15)

    ;; Every five minutes we'll index projects to make them searchable
    (cl-cron:make-cron-job 'index-projects
                           :hash-key 'index-projects
                           :step-min 1)

    ;; Cleaning elastic search once a day
    (cl-cron:make-cron-job 'delete-old-docs-from-index
                           :hash-key 'delete-old-docs-from-index
                           :step-hour 24))
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
