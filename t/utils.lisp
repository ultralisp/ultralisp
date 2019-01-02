(defpackage #:ultralisp-test/utils
  (:use #:cl)
  (:import-from #:mito-email-auth/models)
  (:export #:with-login))
(in-package ultralisp-test/utils)


(defmacro with-login ((&key (email "bob@example.com"))
                      &body body)
  `(let ((user (or (mito-email-auth/models:get-user-by-email ,email)
                   (mito:create-dao 'ultralisp/models/user:user
                                    :email ,email))))
     (mito-email-auth/models:authenticate user)
     ,@body))
