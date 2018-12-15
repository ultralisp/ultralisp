(defpackage #:ultralisp/models/migration
  (:use #:cl)
  (:import-from #:ultralisp/models/check)
  (:import-from #:ultralisp/models/moderator)
  (:import-from #:ultralisp/models/project)
  (:import-from #:ultralisp/models/user)
  (:import-from #:ultralisp/models/version)
  (:import-from #:mito-email-auth/models)
  (:export
   #:create-initial-db-structure))
(in-package ultralisp/models/migration)


(defun create-initial-db-structure ()
  (let ((tables (list 'ultralisp/models/check:check
                      'ultralisp/models/check:check-trigger
                      'ultralisp/models/moderator:moderator
                      'ultralisp/models/project:project
                      'ultralisp/models/user:user
                      'ultralisp/models/version:version
                      'mito-email-auth/models:registration-code)))
    (ultralisp/db:with-connection ()
      (mapc #'mito:ensure-table-exists tables))))
