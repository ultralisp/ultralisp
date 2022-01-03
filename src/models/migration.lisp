(defpackage #:ultralisp/models/migration
  (:use #:cl)
  (:import-from #:ultralisp/models/check)
  (:import-from #:ultralisp/models/moderator)
  (:import-from #:ultralisp/models/project)
  (:import-from #:ultralisp/models/version)
  (:import-from #:reblocks-auth/models)
  (:export
   #:migrate))
(in-package ultralisp/models/migration)


(defun migrate ()
  (ultralisp/db:with-connection ()
    (mito:migrate "./db/")))
