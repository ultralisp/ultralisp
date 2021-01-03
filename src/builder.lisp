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
  (:import-from #:log4cl-extras/error
                #:with-log-unhandled)
  (:import-from #:log4cl-extras/context
                #:with-fields)
  (:import-from #:ultralisp/utils
                #:delete-file-if-exists
                #:remove-last-slash
                #:make-request-id)
  (:import-from #:alexandria
                #:write-string-into-file)
  (:import-from #:arrows
                #:->)
  (:import-from #:ultralisp/models/dist
                #:get-prepared-dists
                #:dist-state
                #:dist-built-at
                #:get-pending-dists
                #:dist-quicklisp-version)
  (:import-from #:ultralisp/models/source
                #:source-systems-info
                #:source-release-info)
  (:import-from #:ultralisp/models/dist-source
                #:dist->sources)
  (:export
   #:build
   #:build-version
   #:build-prepared-versions
   #:prepare-pending-version
   #:build-pending-dists))
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
    (error "This code was broken at some moment, upload requires two parameters. But everything seems work. Probably it is not executed.")
    ;; (upload dir)
    ))


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


(defun create-metadata-for (dist path &key (version-number (dist-quicklisp-version dist)))
  "This function writes a Quicklisp compatible metadata describing all
   project sources included into the dist."
  (log:info "Creating metadata for version" version-number "of" dist)
  
  (let* ((sources (dist->sources dist))
         ;; TODO: remove, seems we don't need this anymore
         ;; (projects (remove-if-not 'get-release-info all-projects))
         (dist-name (ultralisp/models/dist:dist-name dist))
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
            for source in sources
            for release-info = (source-release-info source)
            for systems-info = (source-systems-info source)
            do (write-file release-path
                           (to-string release-info)
                           :if-exists :append)
               (dolist (system-info systems-info)
                 (write-file systems-path
                             (to-string system-info)
                             :if-exists :append))))))


;; TODO: remove after migraion
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


;; TODO: remove after migration 
(defun prepare-pending-version ()
  (with-transaction
    (log:info "Trying to acquire a lock performing-pending-checks-or-version-build from prepare pending version")
    (with-lock ("performing-pending-checks-or-version-build")
      (let ((version (get-pending-version)))
        (when version
          (log:info "Preparing version for build" version)
          (create-projects-snapshots-for version)
          (setf (get-type version)
                :prepared)
          (save-dao version))))))

;; TODO: remove after migration 
(defun build-prepared-versions ()
  "Searches and builds a pending version if any."
  (with-transaction
    (log:info "Trying to acquire a lock performing-pending-checks-or-version-build")
    (with-lock ("performing-pending-checks-or-version-build")
      (log:info "Checking if there is a version to build")
      
      (loop for version in (get-prepared-versions)
            do (build-version version)))))



(defun prepare-dist (dist)
  (check-type dist ultralisp/models/dist:dist)
  
  (when (eql (dist-state dist)
             :pending)
    (log:info "Preparing the" dist)

    ;; We only want to "prepare" dist if it has
    ;; some changes. Sources added and not checked yet
    ;; aren't considered as a "change".
    (let ((changes (loop for source in (dist->sources dist :this-version t)
                         for disable-reason = (ultralisp/models/source:disable-reason source)
                         for disable-reason-type = (ultralisp/models/dist-source:disable-reason-type disable-reason)
                         unless (eql disable-reason-type :just-added)
                         collect source)))
    
      (when changes
        (let ((version-number (make-version-number)))
          (setf (dist-state dist) :prepared
                (dist-quicklisp-version dist) version-number)
          (save-dao dist))))))


(defun build-dist (dist)
  (check-type dist ultralisp/models/dist:dist)

  (let ((state (dist-state dist)))
    (unless (member state '(:prepared
                            ;; It is OK to rebuild the dist in the
                            ;; ready state, because it will not hurt,
                            ;; but sometimes can be useful for debugging:
                            :ready))
      (error "Unable to build dist with state ~S" state)))
  
  (ultralisp/utils:with-tmp-directory (path)
    (log:info "Building the" dist "in the" path)
     
    (setf (dist-built-at dist) (local-time:now)
          (dist-state dist) :ready)

    (create-metadata-for dist path)
    (upload path "/")
    ;; NOTE: in case of error in the database
    ;; we might end with situation when the release
    ;; was uploaded but database not updated.
    ;; This can lead to situation when there are multiple
    ;; distribution versions in the storage, but only one
    ;; dist version in the database.
    (save-dao dist)))


(defun prepare-pending-dists ()
  "Searches and prepares a pending versions for all distributions."
  (with-transaction
    (log:info "Trying to acquire a lock performing-pending-checks-or-version-build")
    (with-lock ("performing-pending-checks-or-version-build")
      (log:info "Checking if there is a version to build")
      
      (mapc #'prepare-dist
            (get-pending-dists)))))


(defun build-prepared-dists ()
  "Searches and builds a pending versions for all distributions."
  (with-transaction
    (log:info "Trying to acquire a lock performing-pending-checks-or-version-build")
    (with-lock ("performing-pending-checks-or-version-build")
      (log:info "Checking if there is a version to build")
      
      (mapc #'build-dist
            (get-prepared-dists)))))


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
