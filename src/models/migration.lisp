(defpackage #:ultralisp/models/migration
  (:use #:cl)
  (:import-from #:ultralisp/models/check)
  (:import-from #:ultralisp/models/moderator)
  (:import-from #:ultralisp/models/project)
  (:import-from #:ultralisp/models/version)
  (:import-from #:reblocks-auth/models)
  (:import-from #:mito)
  (:import-from #:ultralisp/db
                #:with-connection)
  (:export
   #:migrate
   #:generate-migrations))
(in-package #:ultralisp/models/migration)


(defun get-directory ()
  (or (probe-file "./db/")
      (probe-file "/app/db/")))


(defun migrate ()
  (with-connection ()
    (mito:migrate (get-directory))))


(defun generate-migrations ()
  (ultralisp/db:with-connection ()
    (mito:migrate (get-directory))))
