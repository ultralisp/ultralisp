(defpackage #:ultralisp/pipeline/checking
  (:use #:cl)
  (:import-from #:ultralisp/models/action)
  (:import-from #:ultralisp/rpc/core
                #:submit-task)
  (:import-from #:ultralisp/rpc/command
                #:defcommand)
  (:import-from #:rutils
                #:fmt)
  (:import-from #:ultralisp/models/project
                #:project-name
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
                #:check->source
                #:check->project
                #:check2
                #:get-processed-in
                #:get-error
                #:pending-checks
                #:get-processed-at
                #:any-check
                #:get-project)
  (:import-from #:mito
                #:object-id
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
  (:import-from #:log4cl-extras/error
                #:with-log-unhandled)
  (:import-from #:log4cl-extras/context
                #:with-fields)
  (:import-from #:ultralisp/stats
                #:increment-counter)
  (:import-from #:ultralisp/models/versioned
                #:prev-version
                #:latest-p
                #:object-version)
  (:import-from #:ultralisp/models/source
                #:enable-this-source-version
                #:create-new-source-version)
  (:import-from #:ultralisp/models/dist-source
                #:create-pending-dists-for-new-source-version
                #:make-disable-reason)
  (:import-from #:ultralisp/utils/source
                #:make-file-ignorer)
  (:import-from #:ultralisp/utils/retries
                #:with-tries)
  (:export
   #:perform-pending-checks
   #:perform
   #:perform-remotely))
(in-package ultralisp/pipeline/checking)


(defun perform-remotely (check &key (force nil force-p))
  "This function can be called manually to debug source checking."
  (let* ((perform-params (when force-p
                           (list :force force)))
         (lisp-implementation (ultralisp/models/check:lisp-implementation check))
         (started-at (get-internal-real-time))
         (lock-name (fmt "prepare-pending-dists-~A"
                         (string-downcase lisp-implementation))))
    (with-fields (:check-id (mito:object-id check)
                  :lisp-implementation lisp-implementation)
      (with-log-unhandled ()
        ;; Here we need to establish a connection
        ;; to process each check in a separate transaction.
        ;; This way, errors during some checks will not affect
        ;; others:
        (with-connection (:cached nil)
          ;; This lock protects us from checking a version and binding
          ;; it to the pending dist which is currently preparing to build
          (with-lock (lock-name)
            (with-fields (:source (ultralisp/models/source:params-to-string
                                   (ultralisp/models/check:check->source check)))
              (log:info "Submitting check to remote ~S worker"
                        lisp-implementation)
              (restart-case
                  (handler-bind
                      ;; Usual errors will be processed inside the PERFORM2 function,
                      ;; however, sometimes worker can crash hardly, and after 3
                      ;; crashes it will raise error and propagate it to the Gearman
                      ;; client. And here we'll catch it and mark check as failed.
                      ;; Here we are using a separate DB connection because the current
                      ;; one might be already closed.
                      ((error (lambda (condition)
                                (log:error "Marking check as failed because worker crashed hard" condition)
                                (with-connection (:cached nil)
                                  (update-check-as-failed2 check
                                                           (get-traceback condition)
                                                           (float (/ (- (get-internal-real-time)
                                                                         started-at)
                                                                     internal-time-units-per-second))))
                                (unless (in-repl)
                                  (return-from perform-remotely)))))
                    (funcall 'submit-task
                             'perform2
                             :lisp-implementation lisp-implementation
                             :args (list* check
                                          perform-params)))
                (mark-check-as-failed ()
                  :report "Mark check as failed"
                  (log:error "Marking check as failed because worker crashed hard and user selected restart \"Mark check as failed\"")
                  (with-connection (:cached nil)
                    (update-check-as-failed2 check
                                             "Check marked as failed because user has selected the \"Mark check as failed\" restart."
                                             (float (/ (- (get-internal-real-time)
                                                           started-at)
                                                       internal-time-units-per-second))))))
              (log:info "Worker returned from perform2")))))
      (values))))


(defun perform-pending-checks (&key (force nil force-given-p)
                                    (lisp-implementation :sbcl))
  "Performs all pending checks and creates a new Ultralisp version
   if some projects were updated."
  (log:info "Trying to acquire a lock performing-pending-checks-or-version-build from perform-pending-checks to run checks")
  
  (let ((lock-name (fmt "performing-pending-checks-or-version-build-~A"
                        (string-downcase lisp-implementation))))
    (with-lock (lock-name)
      (log:info "Lock acquired")
      (let ((checks (pending-checks :lisp-implementation lisp-implementation)))
        (log:info "I have ~A checks to process"
                  (length checks))
        (flet ((perform (check)
                 (apply #'perform-remotely
                        check
                        (when force-given-p
                          (list :force force)))))
          (loop for check in checks
                do (if slynk-api:*emacs-connection*
                       (perform check)
                       ;; For production we want ignore error in a check
                       ;; to let other checks be processed:
                       (ignore-errors
                        (perform check)))))
        (log:info "I'm done with checks")
        (length checks)))))


;; TODO: remove
(defun check-if-project-was-changed (project downloaded)
  (check-type project project)
  (check-type downloaded downloaded-project)
  (let ((downloaded-params (downloaded-project-params downloaded))
        (project-params (get-params project)))
    (make-update-diff project-params
                      downloaded-params)))

(defun check-if-source-was-changed (source downloaded)
  (check-type source ultralisp/models/source:source)
  (check-type downloaded downloaded-project)
  (let ((downloaded-params (downloaded-project-params downloaded))
        (source-params (ultralisp/models/source:source-params source)))
    (make-update-diff source-params
                      downloaded-params)))


;; TODO: remove
(defcommand save-project-systems (project systems)
  (log:info "Saving systems for" project)
  (setf (get-systems-info project)
        systems)
  (save-dao project))


(defcommand save-release-info (project release-info)
  (log:info "Saving release info for" project)
  (setf (get-release-info project)
        release-info)
  (save-dao project))


(defun collect-systems (path &key (ignore-filename-p (constantly nil)))
  (quickdist:make-systems-info path
                               :ignore-filename-p ignore-filename-p))


;; TODO: remove
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

;; TODO: remove
(defcommand update-check-as-successful (check processed-in)
  (log:info "Updating check as successful" check)

  (increment-counter :checks-processed)

  (setf (get-error check) nil
        (get-processed-at check) (local-time:now)
        (get-processed-in check) processed-in)
  (save-dao check))


(defcommand update-check-as-successful2 (check processed-in)
  (check-type check check2)
  (log:info "Updating check as successful" check)

  (increment-counter :checks-processed)

  (setf (get-error check) nil
        (get-processed-at check) (local-time:now)
        (get-processed-in check) processed-in)
  
  (save-dao check))


;; TODO: remove
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


(defcommand update-check-as-failed2 (check traceback processed-in)
  (check-type check check2)
  (check-type traceback string)
  (check-type processed-in float)
  (with-fields (:check-id (object-id check))
    (log:info "Updating check as failed" check)

    (increment-counter :checks-failed)
  
    (with-transaction
      (let ((project (check->project check)))
        (with-fields (:project-name (project-name project))
          (log:error "Check failed, disabling project" project traceback)
          (setf (get-error check) traceback
                (get-processed-at check) (local-time:now)
                (get-processed-in check) processed-in)
          (save-dao check)
          ;; Now we have to disable this version of the source in a new versions of the dists.
          (let ((source (check->source check)))
            (create-pending-dists-for-new-source-version
             ;; old-source
             source
             ;; new-source is the same because we didn't change it.
             ;; It is OK to have the same source version, connected to
             ;; the different versions of the distribution, because
             ;; the link is carrying information about error received
             ;; during the check.
             source
             :enable nil
             :disable-reason (make-disable-reason :check-error
                                                  :traceback traceback))))))))


;; TODO: remove
(defun perform (check &key (force (eql (ultralisp/models/check:get-type check)
                                       :added-project)))
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
       
        (with-fields (:check-id (mito:object-id check)
                      :project (ultralisp/models/project:get-name project))
          (unwind-protect
               (prog1 (when (or (check-if-project-was-changed project downloaded)
                                force)
                        ;; We should run this perform function inside a worker
                        ;; process which will be killed after the finishing the task.
                        ;; That is why it is OK to change a *central-registry* here:
                        (pushnew path asdf:*central-registry*)
                        (let* ((systems (collect-systems path)))

                          (unless systems
                            (error "No asd files were found!"))

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
                                   :validate t)))))))


(defun perform2 (check2 &key (force (member (ultralisp/models/check:get-type check2)
                                            '(:added-project :manual))
                                    force-give-p))
  "Returns True if new changes were discovered during the check."
  (check-type check2 check2)
  
  (let ((check-id (object-id check2)))
    (with-tries (check-id)
      (let ((started-at (get-internal-real-time)))
        (handler-bind ((error (lambda (condition)
                                (update-check-as-failed2 check2
                                                         (get-traceback condition)
                                                         (float (/ (- (get-internal-real-time)
                                                                       started-at)
                                                                   internal-time-units-per-second)))
                                (if (in-repl)
                                    (invoke-debugger condition)
                                    (return-from perform2)))))
          (let* ((tmp-dir "/tmp/checker")
                 (source (ultralisp/models/check:check->source check2))
                 (project (ultralisp/models/project:source->project source))
                 (downloaded (download source tmp-dir :latest t))
                 (path (downloaded-project-path downloaded)))
           
            (with-fields (:check-id check-id
                          :source-id (object-id source)
                          :source-version (object-version source)
                          :project (ultralisp/models/project:project-name project))
              (log:info "Running perform2 check-type: ~S, force: ~S, force-given-p: ~S"
                        (ultralisp/models/check:get-type check2)
                        force
                        force-give-p)
              (unwind-protect
                   (prog1 (cond
                            ((not (latest-p source)) ;; we only want to process checks for latest sources
                             (log:warn "Ignoring the check because it is attached to an old version of the source")
                             (values nil))
                           
                            ((or (check-if-source-was-changed source downloaded)
                                 force)
                             ;; We should run this perform function inside a worker
                             ;; process which will be killed after the finishing the task.
                             ;; That is why it is OK to change a *central-registry* here:
                             (pushnew path asdf:*central-registry*)
                             (let* ((ignore-dirs (ultralisp/models/source:ignore-dirs source))
                                    (systems (progn
                                               (log:info "Collecting systems from ~A ignoring dirs ~A"
                                                         path
                                                         ignore-dirs)
                                               (collect-systems path
                                                                :ignore-filename-p
                                                                (make-file-ignorer ignore-dirs)))))
                              
                               (unless systems
                                 (error "No asd files were found!"))
                              
                               ;; Now we need to create another version of the source
                               ;; with release info and bind it to a pending version
                               (create-new-source-version source
                                                          systems
                                                          (downloaded-project-params downloaded)
                                                          :enable t)
                              
                               (values t)))
                            ;; When source wasn't changed, but probably
                            ;; was disabled in some distss:
                            (t
                             (enable-this-source-version source)
                             (values t)))
                     (update-check-as-successful2 check2
                                                  (float (/ (- (get-internal-real-time)
                                                                started-at)
                                                            internal-time-units-per-second))))
                ;; Here we need to make a clean up to not clutter the file system
                (log:info "Deleting checked out" path)
                (delete-directory-tree path
                                       :validate t)))))))))

