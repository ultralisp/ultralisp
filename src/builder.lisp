(defpackage #:ultralisp/builder
  (:use #:cl)
  (:import-from #:ultralisp/clpi)
  (:import-from #:log)
  (:import-from #:trivial-garbage
                #:gc)
  (:import-from #:local-time
                #:now
                #:format-timestring)
  (:import-from #:ultralisp/clpi)
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
                #:print-backtrace
                #:with-log-unhandled)
  (:import-from #:log4cl-extras/context
                #:with-fields)
  (:import-from #:ultralisp/utils
                #:delete-file-if-exists
                #:remove-last-slash)
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
  (:export #:build
           #:build-pending-dists))
(in-package #:ultralisp/builder)


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


(defun create-metadata-for (dist path &key (version-number (dist-quicklisp-version dist)))
  "This function writes a Quicklisp compatible metadata describing all
   project sources included into the dist."
  (log:info "Creating metadata for version" version-number "of" dist)
  
  (let* ((sources (dist->sources dist :enabled t))
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
            ;; This protect us from sources which are enabled but not having release-info for some reason
            when (and release-info
                      systems-info)
            do (write-file release-path
                           (to-string release-info)
                           :if-exists :append)
               (dolist (system-info systems-info)
                 (write-file systems-path
                             (to-string system-info)
                             :if-exists :append))))))


(defun prepare-dist (dist)
  (check-type dist ultralisp/models/dist:dist)
  
  (when (eql (dist-state dist)
             :pending)
    (log:info "Preparing the" dist)

    ;; We only want to "prepare" dist if it has
    ;; some changes. Sources added and not checked yet
    ;; aren't considered as a "change".
    (let* ((sources (dist->sources dist :this-version t))
           (changes (loop for source in sources
                          for disable-reason = (ultralisp/models/source:disable-reason source)
                          for disable-reason-type = (ultralisp/models/dist-source:disable-reason-type disable-reason)
                          ;; We don't build release only because some source was added to.
                          ;; This added source have to be checked first.
                          unless (eql disable-reason-type
                                      :just-added)
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

  ;; First, we'll update CLPI metadata
  (log:info "Updating CLPI index for" dist)
  (ultralisp/clpi::write-index-for-dist dist)

  ;; And then generate metadata in the Quicklisp format
  (ultralisp/utils:with-tmp-directory (path)
    (log:info "Building the" dist "in the" path)
     
    (setf (dist-built-at dist) (local-time:now)
          (dist-state dist) :ready)

    (create-metadata-for dist path)
    (upload path :quicklisp "/")
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
    (log:info "Trying to acquire a lock performing-pending-checks")
    (with-lock ("prepare-pending-dists" :timeout (* 4 60 1000))
      (log:info "Checking if there is a version to build")
      
      (mapc #'prepare-dist
            (get-pending-dists)))))


(defun build-prepared-dists ()
  "Searches and builds a pending versions for all distributions."
  (with-transaction
    (log:info "Trying to acquire a lock performing-pending-checks")
    (with-lock ("build-prepared-dists")
      (log:info "Checking if there is a version to build")

      (loop for dist in (get-prepared-dists)
            ;; Building each dist in a separate transaction,
            ;; to commmit intermediate results:
            do (with-connection (:cached nil)
                 (build-dist dist))
               (gc :full t)))))


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
