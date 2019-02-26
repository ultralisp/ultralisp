(defpackage #:ultralisp/pipeline/checking
  (:use #:cl)
  (:import-from #:ultralisp/models/action)
  (:import-from #:ultralisp/lfarm/core
                #:submit-task)
  (:import-from #:ultralisp/models/project
                #:update-and-enable-project
                #:get-name
                #:get-systems-info
                #:get-release-info
                #:get-params
                #:project
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
                #:base-check
                #:get-project)
  (:import-from #:mito
                #:save-dao)
  (:import-from #:ultralisp/utils
                #:remove-last-slash
                #:make-update-diff
                #:first-letter-of
                #:get-traceback)
  (:import-from #:ultralisp/variables
                #:get-base-url
                #:get-dist-name)
  (:import-from #:ultralisp/downloader/base
                #:downloaded-project-path
                #:download
                #:downloaded-project-params
                #:downloaded-project)
  (:import-from #:quickdist
                #:get-project-name
                #:get-archive-path
                #:get-system-files)
  (:import-from #:ultralisp/uploader/base
                #:upload)
  (:import-from #:uiop
                #:delete-directory-tree)
  (:import-from #:ultralisp/lfarm/command
                #:defcommand)
  (:export
   #:perform-pending-checks
   #:perform))
(in-package ultralisp/pipeline/checking)


(defvar *check* nil)

(defun catch-check (check)
  ;; FOR DEBUG
  (setf *check* check)
  (setf ultralisp/lfarm/core::*after-last-task* nil)
  :catched)


(defun perform-pending-checks (&key force)
  "Performs all pending checks and creates a new Ultralisp version
   if some projects were updated."
  (with-transaction
    (with-lock ("performing-pending-checks-or-version-build")
      (let ((checks (get-pending-checks)))
        (loop for check in checks
              do (submit-task 'perform
                              check
                              :force force))))))


(defun check-if-project-was-changed (project downloaded)
  (check-type project project)
  (check-type downloaded downloaded-project)
  (let ((downloaded-params (downloaded-project-params downloaded))
        (project-params (get-params project)))
    (make-update-diff project-params
                      downloaded-params)))


(ultralisp/lfarm/command:defcommand save-project-systems (project systems)
  (log:info "Saving systems for" project)
  (setf (get-systems-info project)
        systems)
  (save-dao project))


(ultralisp/lfarm/command:defcommand save-release-info (project release-info)
  (log:info "Saving release info for" project)
  (setf (get-release-info project)
        release-info)
  (save-dao project))


(defun collect-systems (path)
  (quickdist:make-systems-info path))


(defcommand make-release (project systems)
  "Downloads the project into the temporary directory, builts a tarball and uploads it to the storage."
  (ultralisp/utils:with-tmp-directory (path)
    (unwind-protect
         (let* ((system-files (get-system-files systems))
                (downloaded (download project path :latest t))
                (archive-dir (uiop:ensure-pathname (merge-pathnames ".archive/" path)
                                                   :ensure-directories-exist t))
                (project-name (get-name project))
                (release-info (quickdist:make-archive (downloaded-project-path downloaded)
                                                      project-name
                                                      system-files
                                                      archive-dir
                                                      (format nil "~A/archive/~A"
                                                              (remove-last-slash (get-base-url))
                                                              (first-letter-of project-name))))
                (archive-path (get-archive-path release-info))
                (archive-destination (format nil "/~A/archive/~A/"
                                             (get-dist-name)
                                             (first-letter-of project-name))))

           (upload archive-path
                   archive-destination)
           (save-release-info project release-info)
           path)
      (delete-directory-tree path
                             :validate t))))


(defcommand update-check-as-successful (check)
  (setf (get-error check) nil
        (get-processed-at check) (local-time:now))
  (save-dao check))


(defcommand update-check-as-failed (check traceback)
  (check-type check base-check)
  (check-type traceback string)
  
  (let ((project (get-project check)))
    (log:error "Check failed, disabling project" project traceback)
    (setf (get-error check) traceback
          (get-processed-at check) (local-time:now))
    (save-dao check)
    (disable-project project
                     :reason :check-error
                     :traceback traceback)))


(defun perform (check &key force)
  (handler-bind ((error (lambda (condition)
                          (update-check-as-failed check
                                                  (get-traceback condition)))))
      (let* ((tmp-dir "/tmp/checker")
             (project (get-project check))
             (downloaded (download project tmp-dir :latest t))
             (path (downloaded-project-path downloaded)))
     
        (unwind-protect
             (when (or (check-if-project-was-changed project downloaded)
                       force)
               (let* ((systems (collect-systems path)))
                 (save-project-systems project systems)
                 (make-release project systems)
                 (update-and-enable-project project
                                            (downloaded-project-params downloaded)
                                            :force force)
                 (update-check-as-successful check)
                 (values t)))
          ;; Here we need to make a clean up to not clutter the file system
          (log:info "Deleting checked out" path)
          (delete-directory-tree path
                                 :validate t)))))
