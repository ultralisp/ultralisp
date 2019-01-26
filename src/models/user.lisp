(defpackage #:ultralisp/models/user
  (:use #:cl)
  (:import-from #:mito)
  (:import-from #:mito-email-auth/weblocks
                #:user-with-email)
  (:export #:user
           #:get-all-users))
(in-package ultralisp/models/user)


(defclass user (user-with-email)
  ()
  (:metaclass mito:dao-table-class))


(defun get-all-users ()
  (mito:select-dao 'user))


