(defpackage #:ultralisp/models/migration
  (:use #:cl)
  (:export
   #:create-initial-db-structure))
(in-package ultralisp/models/migration)


(defun create-initial-db-structure ()
  (let ((tables (list 'ultralisp/models/check:check
                      'ultralisp/models/check:check-trigger
                      'ultralisp/models/moderator:moderator
                      'ultralisp/models/project:project
                      'ultralisp/models/user:user
                      'ultralisp/models/version:version)))
    (ultralisp/db:with-connection ()
      (mapc #'mito:ensure-table-exists tables))))
