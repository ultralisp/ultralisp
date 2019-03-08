(defpackage #:ultralisp/builder
  (:use #:cl)
  (:import-from #:local-time
                #:now
                #:format-timestring)
  (:import-from #:quickdist
                #:render-template
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
  (:import-from #:ultralisp/lfarm/core
                #:submit-task)
  (:import-from #:trivial-backtrace
                #:print-backtrace)
  (:import-from #:ultralisp/models/project
                #:get-systems-info
                #:get-release-info
                #:get-name
                #:project-version
                #:disable-project
                #:create-projects-snapshots-for
                #:project)
  (:import-from #:uiop
                #:truename*)
  (:import-from #:log4cl-json
                #:with-log-unhandled
                #:with-fields)
  (:import-from #:ultralisp/utils
                #:delete-file-if-exists
                #:remove-last-slash
                #:make-request-id)
  (:import-from #:alexandria
                #:write-string-into-file)
  (:import-from #:cl-arrows
                #:->)
  (:export
   #:build
   #:build-version
   #:build-prepared-versions
   #:prepare-pending-version))
(in-package ultralisp/builder)


(defparameter *releases-header-line*
  (format nil
          "# project url size file-md5 content-sha1 prefix [system-file1..system-fileN]~%"))


(defparameter *systems-header-line*
  (format nil
          "# project system-file system-name [dependency1..dependencyN]~%"))


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


(defclass upload-command ()
  ((dir :initarg :dir
        :reader get-dir)))


(defun make-upload-command (dir)
  (make-instance 'upload-command :dir dir))


(defclass disable-project-command ()
  ((project :initarg :project
            :type project-version
            :reader get-project)
   (reason :initarg :reason
           :type keyword
           :reader get-reason)
   (description :initarg :description
                :type string
                :reader get-description)))


(defun make-disable-project-command (project reason description)
  (check-type project project-version)
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
  (let* ((project-version (get-project command))
         (reason (get-reason command))
         (description (get-description command))
         (project (ultralisp/models/project:get-project project-version)))
    (log:error "Disabling project" project reason description)
    (disable-project project
                     :reason reason
                     :traceback description)))


(defmethod perform ((command upload-command))
  (let* ((dir (get-dir command)))
    (log:info "Uploading" dir)
    (upload dir)))


;; TODO: remove this function and probably all commands above
(defun build-version-remotely (version
                               &key
                                 (projects-dir (get-projects-dir))
                                 (name (get-dist-name))
                                 (base-url (get-base-url))
                                 (dist-dir (get-dist-dir))
                                 db-user
                                 db-pass
                                 db-host
                                 (download-p t))
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
                 (_ (when download-p
                      (log:info "Downloading projects")))
                 (downloaded-projects (when download-p
                                        (download version projects-dir)))
                 (num-downloaded-projects (length downloaded-projects))
                 commands)
            (declare (ignorable _))
            
            (when download-p
              (log:info "Projects were downloaded" num-downloaded-projects)

              (log:info "Removing disabled projects from disk")
              (remove-disabled-projects projects-dir
                                        downloaded-projects))

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
            (log:info "Sending back command which will upload distribution")
            (push (make-upload-command dist-dir)
                  commands)
            ;; Here we don't save version object because
            ;; this function will be called on a remote worker
            ;; without "write" access to the database.
            (log:info "Sending back command which will save version to the database")
            (push (make-save-version-command version)
                  commands)
            commands))))))


(defun create-metadata-for (version path &key (version-number (get-number version)))
  (log:info "Creating metadata for version" version-number)
  (let* ((all-projects (ultralisp/models/project:get-projects version))
         (projects (remove-if-not 'get-release-info all-projects))
         (dist-name (get-dist-name))
         (base-url (remove-last-slash (get-base-url)))
         (template-data (list :name dist-name
                              :version version-number
                              :base-url base-url))
         (dist-info-content (render-template quickdist:*distinfo-template*
                                             template-data)))
    (flet ((make-path (template &rest args)
             (ensure-directories-exist
              (merge-pathnames (apply #'format
                                      nil
                                      template
                                      args)
                               path)))
           (to-string (obj)
             (format nil "~A" obj))
           
           (write-file (filename content &key
                                 (if-exists :supersede))
             (log:info "Writing file" filename)
             (write-string-into-file content filename
                                     :if-exists if-exists
                                     :if-does-not-exist :create)
             (values filename)))
      
      (write-file (make-path "~A.txt" dist-name)
                  dist-info-content)
      (write-file (make-path "~A/~A/distinfo.txt" dist-name version-number)
                  dist-info-content)
      
      (loop with release-path = (-> "~A/~A/releases.txt"
                                    (make-path dist-name version-number)
                                    (delete-file-if-exists)
                                    (write-file *releases-header-line*))
            with systems-path = (-> "~A/~A/systems.txt"
                                    (make-path dist-name version-number)
                                    (delete-file-if-exists)
                                    (write-file *systems-header-line*))
            with *print-pretty* = nil
            for project in projects
            for release-info = (get-release-info project)
            for systems-info = (get-systems-info project)
            do (write-file release-path
                           (to-string release-info)
                           :if-exists :append)
               (dolist (system-info systems-info)
                 (write-file systems-path
                             (to-string system-info)
                             :if-exists :append))))
    projects))


(defun build-version (version)
  "This function generates all necessary metadata for the version and uploads them to the server."
  (check-type version version)
  (ultralisp/utils:with-tmp-directory (path)
    (log:info "Building a new" version "in the" path)
  
    (let ((version-number (make-version-number)))
      (setf (get-built-at version) (local-time:now)
            (get-type version) :ready
            (get-number version) version-number)
    
      (create-metadata-for version path)
      (upload path "/")
      (save-dao version))))


(defun prepare-pending-version ()
  (with-transaction
    (with-lock ("performing-pending-checks-or-version-build")
      (let ((version (get-pending-version)))
        (when version
          (log:info "Preparing version for build" version)
          (create-projects-snapshots-for version)
          (setf (get-type version)
                :prepared)
          (save-dao version))))))


(defun build-prepared-versions ()
  "Searches and builds a pending version if any."
  (with-transaction
    (with-lock ("performing-pending-checks-or-version-build")
      (log:info "Checking if there is a version to build")
      
      (loop for version in (get-prepared-versions)
            do (build-version version)))))


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


(ultralisp/lfarm/command:defcommand say-hello (name)
  (log:info "Hello" name))


(defun foo-task (the-arg)
  (say-hello "Petya")
  (say-hello "Masha")
  (list :bar the-arg))
