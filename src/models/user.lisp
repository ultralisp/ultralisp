;; Temporary solution to generate db migraions
(push )

(defpackage #:ultralisp/models/user
  (:use #:cl)
  (:import-from #:mito-email-auth/models
                #:user-with-email))
(in-package ultralisp/models/user)


(defclass user (user-with-email)
  ()
  (:metaclass mito:dao-table-class))
