(defpackage #:ultralisp/pipeline/checking
  (:use #:cl)
  (:import-from #:ultralisp/models/action)
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
                #:get-traceback)
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


(defun perform-pending-checks ()
  "Performs all pending checks and creates a new Ultralisp version
   if some projects were updated."
  (with-transaction
    (with-lock ("performing-pending-checks-or-version-build")
      (let ((checks (get-pending-checks)))
        (loop for check in checks
              do (perform-check check))))))
