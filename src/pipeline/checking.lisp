(defpackage #:ultralisp/pipeline/checking
  (:use #:cl)
  (:import-from #:cl-fad)
  (:import-from #:ultralisp/models/action)
  (:import-from #:ultralisp/lfarm/core
                #:submit-task)
  (:import-from #:ultralisp/models/project
                #:is-enabled-p
                #:disable-project
                #:get-source)
  (:import-from #:ultralisp/db
                #:with-lock
                #:with-transaction)
  (:import-from #:ultralisp/models/check
                #:get-error
                #:added-project-check
                #:get-pending-checks
                #:get-processed-at
                #:check
                #:get-project)
  (:import-from #:mito
                #:save-dao)
  (:import-from #:ultralisp/utils
                #:first-letter-of
                #:get-traceback)
  (:import-from #:ultralisp/variables
                #:get-dist-name)
  (:export
   #:perform-pending-checks
   #:perform-check
   #:perform-project-check))
(in-package ultralisp/pipeline/checking)


(defgeneric perform-project-check (source project check)
  (:documentation "Performs actual check if project has changed in the remote source.

If it changed, then method should set `processed-at' attribute of the check and exit.
Otherwise, it should set an error description to the `error' attribute of the check.

Should return a check object."))


(defun perform-check (check)
  "Performs a project check. After this call, parameters
of the project linked to the check object can be changed
as well as check's attributes such like `project-has-changes'
and `description'."
  
  (let* ((project (get-project check))
         (source (get-source project)))
    
    (handler-case
        (prog1 (perform-project-check source
                                      project
                                      check)
          (setf (get-error check)
                nil))
      (error (condition)
        (let ((reason :check-error)
              (traceback (get-traceback condition)))
          (log:error "Check failed, disabling project" project traceback)
          (setf (get-error check)
                traceback)
          (disable-project project
                           :reason reason
                           :traceback traceback))))
    
    (setf (get-processed-at check)
          (local-time:now))
    (save-dao check)))


(defvar *check* nil)

(defun catch-check (check)
  ;; FOR DEBUG
  (setf *check* check)
  (setf ultralisp/lfarm/core::*after-last-task* nil)
  :catched)


(defun perform-check-remotely (check)
  "Performs a project check. After this call, parameters
of the project linked to the check object can be changed
as well as check's attributes such like `project-has-changes'
and `description'."
  (let* ((project (get-project check))
         (source (get-source project)))

    (handler-case
        (prog1 (perform-project-check source
                                      project
                                      check)
          (setf (get-error check)
                nil))
      (error (condition)
        (let ((reason :check-error)
              (traceback (get-traceback condition)))
          (log:error "Check failed, disabling project" project traceback)
          (setf (get-error check)
                traceback)
          ;; TODO: Send command
          ;; (disable-project project
          ;;                  :reason reason
          ;;                  :traceback traceback)
          )))
    
    (setf (get-processed-at check)
          (local-time:now))
    ;; TODO: Send command
    ;; (save-dao check)

    ))


(defun perform-pending-checks ()
  "Performs all pending checks and creates a new Ultralisp version
   if some projects were updated."
  (with-transaction
    (with-lock ("performing-pending-checks-or-version-build")
      (let ((checks (get-pending-checks)))
        (loop for check in checks
              do (perform-check check))))))


(defun check-if-project-was-changed (project downloaded)
  (check-type project ultralisp/models/project:project)
  (check-type downloaded ultralisp/downloader/base:downloaded-project)
  (let ((downloaded-params (ultralisp/downloader/base:downloaded-project-params downloaded))
        (project-params (ultralisp/models/project:get-params project)))
    (ultralisp/utils:make-update-diff project-params
                                      downloaded-params)))


(ultralisp/lfarm/command:defcommand save-project-systems (project systems)
  (log:warn "Saving systems for" project)
  ;; TODO: make real changes to the database
  )


(ultralisp/lfarm/command:defcommand save-release-info (project release-info)
  (log:warn "Saving release info for" project)
  ;; TODO: make real changes to the database
  )


(defun collect-systems (path)
  (quickdist:make-systems-info path))


(defun get-tmp-directory-name ()
  (cl-fad:pathname-as-directory
   (translate-logical-pathname
    (cl-fad::generate-random-pathname cl-fad::*default-template*
                                      'cl-fad::generate-random-string))))


(ultralisp/lfarm/command:defcommand make-release (project systems)
  "Downloads the project into the temporary directory, builts a tarball and uploads it to the storage."
  (let* ((path (get-tmp-directory-name))
         (system-files (quickdist:get-system-files systems)))
    (unwind-protect
         (let* ((downloaded (ultralisp/downloader/base:download project path :latest t))
                (archive-dir (uiop:ensure-pathname (merge-pathnames ".archive/" path)
                                                   :ensure-directories-exist t))
                (release-info (quickdist:make-archive (ultralisp/downloader/base:downloaded-project-path downloaded)
                                                      (ultralisp/models/project:get-name project)
                                                      system-files
                                                      ;; (ultralisp/variables:get-dist-dir)
                                                      archive-dir
                                                      (ultralisp/variables:get-base-url)))
                (archive-path (quickdist:get-archive-path release-info))
                (project-name (quickdist:get-project-name release-info))
                (archive-destination (format nil "/~A/archive/~A/"
                                             (get-dist-name)
                                             (first-letter-of project-name))))

           (ultralisp/uploader/base:upload archive-path
                                           archive-destination)
           (save-release-info project release-info)
           path)
      (uiop:delete-directory-tree path
                                  :validate t))))


(defun perform (check)
  (let* ((tmp-dir "/tmp/checker")
         (project (get-project check))
         (downloaded (ultralisp/downloader/base:download project tmp-dir :latest t))
         (path (ultralisp/downloader/base:downloaded-project-path downloaded))
         (changes (check-if-project-was-changed project downloaded)))
    
    (unwind-protect
         (when changes
           (let* ((systems (collect-systems path))
                  (release-info (make-release path project systems)))
             (save-project-systems project systems)
             (save-release-info project release-info)
             
             (values systems
                     release-info)))
      ;; Here we need to make a clean up to not clutter the file system
      (log:info "Deleting checked out" path)
      (uiop:delete-directory-tree path
                                  :validate t))))
