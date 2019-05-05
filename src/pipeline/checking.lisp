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
                #:with-connection
                #:with-lock
                #:with-transaction)
  (:import-from #:ultralisp/models/check
                #:get-processed-in
                #:get-error
                #:added-project-check
                #:get-pending-checks
                #:get-processed-at
                #:any-check
                #:get-project)
  (:import-from #:mito
                #:save-dao)
  (:import-from #:ultralisp/utils
                #:in-repl
                #:remove-last-slash
                #:make-update-diff
                #:first-letter-of
                #:get-traceback)
  (:import-from #:ultralisp/variables
                #:get-base-url
                #:get-dist-name)
  (:import-from #:ultralisp/downloader/base
                #:remove-vcs-files
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
  (:import-from #:log4cl-json
                #:with-log-unhandled)
  (:import-from #:ultralisp/stats
                #:increment-counter)
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
  (with-lock ("performing-pending-checks-or-version-build")
    (let ((checks (get-pending-checks)))
      (loop for check in checks
            ;; Here we need to establish a connection
            ;; to process each check in a separate transaction.
            ;; This way, errors during some checks will not affect
            ;; others
            do (ignore-errors
                (with-log-unhandled ()
                  (with-connection (:cached nil)
                    (log4cl-json:with-fields (:check-id (mito:object-id check))
                      (log:info "Submitting check to remote worker")
                      (submit-task 'perform
                                   check
                                   :force force))))))
      (length checks))))


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
                (_ (remove-vcs-files downloaded))
                (project-name (get-name project))
                (dist-name (get-dist-name))
                (archive-url (format nil "~A/~A/archive/~A"
                                     (remove-last-slash (get-base-url))
                                     dist-name
                                     (first-letter-of project-name)))
                (release-info (quickdist:make-archive (downloaded-project-path downloaded)
                                                      (cl-strings:replace-all project-name
                                                                              "/"
                                                                              "-")
                                                      system-files
                                                      archive-dir
                                                      archive-url))
                (archive-path (get-archive-path release-info))
                (archive-destination (format nil "/~A/archive/~A/"
                                             dist-name
                                             (first-letter-of project-name))))
           (declare (ignorable _))

           (upload archive-path
                   archive-destination)
           (save-release-info project release-info)
           path)
      (delete-directory-tree path
                             :validate t))))


(defcommand update-check-as-successful (check processed-in)
  (log:info "Updating check as successful" check)

  (increment-counter :checks-processed)

  (setf (get-error check) nil
        (get-processed-at check) (local-time:now)
        (get-processed-in check) processed-in)
  (save-dao check))


(defcommand update-check-as-failed (check traceback processed-in)
  (check-type check any-check)
  (check-type traceback string)
  (log:info "Updating check as failed" check)

  (increment-counter :checks-failed)
  
  (let ((project (get-project check)))
    (log:error "Check failed, disabling project" project traceback)
    (setf (get-error check) traceback
          (get-processed-at check) (local-time:now)
          (get-processed-in check) processed-in)
    (save-dao check)
    (disable-project project
                     :reason :check-error
                     :traceback traceback)))


(defun perform (check &key force)
  (let ((started-at (get-internal-real-time)))
    (handler-bind ((error (lambda (condition)
                            (update-check-as-failed check
                                                    (get-traceback condition)
                                                    (float (/ (- (get-internal-real-time)
                                                                 started-at)
                                                              internal-time-units-per-second)))
                            (if (in-repl)
                                (invoke-debugger condition)
                                (return-from perform)))))
      (let* ((tmp-dir "/tmp/checker")
             (project (get-project check))
             (downloaded (download project tmp-dir :latest t))
             (path (downloaded-project-path downloaded)))
       
        (unwind-protect
             (prog1 (when (or (check-if-project-was-changed project downloaded)
                              force)
                      (let* ((systems (collect-systems path)))
                        (save-project-systems project systems)
                        (make-release project systems)
                        (update-and-enable-project project
                                                   (downloaded-project-params downloaded)
                                                   :force force)
                        (values t)))
               (update-check-as-successful check
                                           (float (/ (- (get-internal-real-time)
                                                        started-at)
                                                     internal-time-units-per-second))))
          ;; Here we need to make a clean up to not clutter the file system
          (log:info "Deleting checked out" path)
          (delete-directory-tree path
                                 :validate t))))))


(defun test-locking (worker lock-name seconds timeout)
  (with-connection (:cached nil)
    (log4cl-json:with-fields (:worker worker :lock-name lock-name)
      (log:error "Trying to get lock")
      (ultralisp/db::get-lock2 "foo2" :timeout timeout)
      (log:error "Locked for" seconds)
      (sleep seconds)
      (log:error "Unlocking"))))
