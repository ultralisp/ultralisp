(defpackage #:ultralisp/models/migration
  (:use #:cl)
  (:import-from #:ultralisp/models/check)
  (:import-from #:ultralisp/models/moderator)
  (:import-from #:ultralisp/models/project)
  (:import-from #:ultralisp/models/user)
  (:import-from #:ultralisp/models/version)
  (:import-from #:mito-email-auth/models)
  (:export
   #:create-initial-db-structure
   #:migrate))
(in-package ultralisp/models/migration)


(defparameter *classes*
  (list 'ultralisp/models/check:check
        'ultralisp/models/check:check-trigger
        'ultralisp/models/moderator:moderator
        'ultralisp/models/project:project
        'ultralisp/models/user:user
        'ultralisp/models/version:version
        'mito-email-auth/models:registration-code))


(defun create-initial-db-structure ()
  (ultralisp/db:with-connection ()
    (mapc #'mito:ensure-table-exists *classes*)))


(defun migrate ()
  (ultralisp/db:with-connection ()
    (mapc #'mito:migrate-table *classes*)))
