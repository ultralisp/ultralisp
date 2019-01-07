(defpackage #:ultralisp/builder
  (:use #:cl)
  (:import-from #:local-time
                #:now
                #:format-timestring)
  (:import-from #:quickdist
                #:quickdist)
  (:import-from #:ultralisp/downloader/base
                #:remove-disabled-projects
                #:find-project-by-path
                #:download)
  (:import-from #:ultralisp/models/version
                #:get-prepared-versions
                #:get-type
                #:make-version-number
                #:get-pending-version
                #:get-built-at
                #:get-number
                #:version)
  (:import-from #:mito
                #:save-dao)
  (:import-from #:ultralisp/db
                #:with-connection
                #:with-lock
                #:with-transaction)
  (:import-from #:ultralisp/uploader/base
                #:upload)
  (:import-from #:ultralisp/variables
                #:get-postgres-ro-pass
                #:get-postgres-ro-user
                #:get-postgres-host
                #:get-dist-dir
                #:get-base-url
                #:get-dist-name
                #:get-projects-dir)
  (:import-from #:ultralisp/lfarm
                #:submit-task)
  (:import-from #:trivial-backtrace
                #:print-backtrace)
  (:import-from #:ultralisp/models/project
                #:disable-project
                #:create-projects-snapshots-for
                #:project)
  (:import-from #:uiop
                #:truename*)
  (:import-from #:log4cl-json
                #:with-log-unhandled
                #:with-fields)
  (:import-from #:ultralisp/utils
                #:make-request-id)
  (:export
   #:build
   #:build-version
   #:build-prepared-versions
   #:prepare-pending-version))
(in-package ultralisp/builder)


(defun get-new-version-number ()
  (format-timestring nil
                     (now)
                     :format '((:year 4) (:month 2) (:day 2) #\-
                               (:hour 2) (:min 2) (:sec 2))
                     :timezone local-time:+utc-zone+))


(defun build (&key
                (projects :all)
                (projects-dir (get-projects-dir))
                (name (get-dist-name))
                (base-url (get-base-url))
                (dist-dir (get-dist-dir)))
  (download projects projects-dir)
  (quickdist :name name
             :base-url base-url
             :projects-dir projects-dir
             :dists-dir dist-dir
             :version (get-new-version-number)))


(defclass save-version-command ()
  ((version :initarg :version
            :type version
            :reader get-version)))

(defun make-save-version-command (version)
  (check-type version version)
  (make-instance 'save-version-command :version version))


(defclass disable-project-command ()
  ((project :initarg :project
            :type project
            :reader get-project)
   (reason :initarg :reason
           :type keyword
           :reader get-reason)
   (description :initarg :description
                :type string
                :reader get-description)))


(defun make-disable-project-command (project reason description)
  (check-type project project)
  (check-type reason keyword)
  (check-type description string)
  (make-instance 'disable-project-command
                 :project project
                 :reason reason
                 :description description))


(defgeneric perform (command)
  (:documentation "This method is called by the main process
                   to change state of the database. Because worker
                   is unable to do it itself."))


(defmethod perform ((command save-version-command))
  (save-dao (get-version command)))


(defmethod perform ((command disable-project-command))
  (disable-project (get-project command)))


(defun build-version-remotely (version
                               &key
                                 (projects-dir (get-projects-dir))
                                 (name (get-dist-name))
                                 (base-url (get-base-url))
                                 (dist-dir (get-dist-dir))
                                 db-user
                                 db-pass
                                 db-host)
  "This function will be performed on a worker with
   read-only access to the database.

   It should return a list of commands to the main process to
   modify the database."
  (with-fields (:request-id (make-request-id))
    (check-type version version)

    (handler-bind ((error (lambda (condition)
                            ;; We want debugger to popup if we've connected to
                            ;; the process from SLY
                            ;; (invoke-debugger condition)
                            (when ultralisp/slynk:*connections*
                              (invoke-debugger condition)))))
      (with-log-unhandled ()
        (log:info "Building a new version" version)
    
        (with-connection (:username db-user
                          :password db-pass
                          :host db-host)
          (uiop:ensure-all-directories-exist (list projects-dir))
          
          (let* ((projects-dir (truename* projects-dir))
                 (_ (log:info "Downloading projects"))
                 (downloaded-projects (download :all projects-dir))
                 commands)
            (declare (ignorable _))

            (log:info "Removing disabled projects from disk")
            (remove-disabled-projects projects-dir
                                      downloaded-projects)

            (handler-bind ((error (lambda (condition)
                                    (let ((restart (find-restart 'quickdist:skip-project)))
                                      (cond
                                        (restart
                                         (log:error "Error catched during processing" quickdist:*project-path* condition)
                                         (let ((project (find-project-by-path downloaded-projects
                                                                              quickdist:*project-path*)))
                                           (log:info "Sending back command which will disable project" project)
                                           (push (make-disable-project-command project
                                                                               :build-error
                                                                               (print-backtrace condition
                                                                                                :output nil))
                                                 commands))
                                         (invoke-restart restart))
                                        (t (error "No skip-project restart found!")))))))

              (let ((version-number (make-version-number)))
                (setf (get-number version)
                      version-number)
                
                (log:info "Starting quickdist build" version-number)
                (quickdist :name name
                           :base-url base-url
                           :projects-dir projects-dir
                           :dists-dir dist-dir
                           :version version-number)))
            
            (setf (get-built-at version)
                  (local-time:now)
                  (get-type version)
                  :ready)

            ;; TODO: probably it is not the best idea to upload dist-dir
            ;;       every time, because there can be previously built distributions
            ;;       May be we need to minimize network traffic here and upload
            ;;       only a part of it or make a selective upload which will not
            ;;       transfer files which already on the S3.
            (log:info "Uploading distribution")
            (upload dist-dir)
            ;; Here we don't save version object because
            ;; this function will be called on a remote worker
            ;; without "write" access to the database.
            (log:info "Sending back command which will save version to the database")
            (push (make-save-version-command version)
                  commands)
            commands))))))



(defun prepare-pending-version ()
  (with-transaction
    (with-lock ("performing-pending-checks-or-version-build")
      (let ((version (get-pending-version)))
        (when version
          (log:info "Preparing version for build" version)
          (create-projects-snapshots-for version)
          (setf (get-type version)
                :prepared)
          (mito:save-dao version))))))


(defun build-prepared-versions ()
  "Searches and builds a pending version if any."
  (with-transaction
    (with-lock ("performing-pending-checks-or-version-build")
      (log:info "Checking if there is a version to build")
      
      (loop for version in (get-prepared-versions)
            do (log:info "Submitting task to worker")
               (let ((commands
                       (submit-task
                        'build-version-remotely
                        version
                        ;; Here we are passing all these settings
                        ;; explicitly, to not have to specify
                        ;; any environment variables for the "workers".
                        :projects-dir (get-projects-dir)
                        :name (get-dist-name)
                        :base-url (get-base-url)
                        :dist-dir (get-dist-dir)
                        :db-user (get-postgres-ro-user)
                        :db-pass (get-postgres-ro-pass)
                        :db-host (get-postgres-host))))
                 ;; Our worker has read-only connection to the database.
                 ;; That is why it need to return back all actions which
                 ;; require a database modification.
                 (log:info "Applying commands")
                 (loop for command in commands
                       do (perform command)))))))


(defun test-build (&key
                     (projects-dir (get-projects-dir))
                     (name (get-dist-name))
                     (base-url (get-base-url))
                     (dist-dir (get-dist-dir)))
  (quickdist :name name
             :base-url base-url
             :projects-dir projects-dir
             :dists-dir dist-dir
             :version (get-new-version-number)))
