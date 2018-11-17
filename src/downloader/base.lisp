(defpackage #:ultralisp/downloader/base
  (:use #:cl)
  (:import-from #:ultralisp/models/check
                #:make-version-from
                #:get-all-not-checked-checks
                #:project-has-changes-p
                #:check
                #:get-project)
  (:import-from #:ultralisp/models/project
                #:get-source)
  (:import-from #:ultralisp/db
                #:with-transaction)
  (:export
   #:perform-project-check
   #:perform-check))
(in-package ultralisp/downloader/base)


(defgeneric perform-project-check (source project check)
  (:documentation "Performs actual check if project has changed in the remote source.

If it changed, then method should set `project-has-changes-p' of the `check'
to True and set it's description to a text describing changes. These changes will
be grouped into the changelog item for a new Ultralisp release.

Should return a check object."))


(defun perform-check (check)
  "Performs a project check. After this call, parameters
of the project linked to the check object can be changed
as well as check's attributes such like `project-has-changes'
and `description'."
  (check-type check check)
  
  (let* ((project (get-project check))
         (source (get-source project)))
    (perform-project-check source
                           project
                           check)))


(defun perform-pending-checks-and-trigger-version-build ()
  "Performs all pending checks and creates a new Ultralisp version
   if some projects were updated."
  (with-transaction
      (let* ((checks (get-all-not-checked-checks))
             (checks (mapcar 'perform-check checks))
             (checks (remove-if-not 'project-has-changes-p checks)))
        
        (when checks
          (let ((version (make-version-from checks)))
            (log:info "Version was created" version))))))
