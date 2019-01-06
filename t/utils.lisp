(defpackage #:ultralisp-test/utils
  (:use #:cl)
  (:import-from #:mito-email-auth/models)
  (:import-from #:ultralisp/db)
  (:import-from #:weblocks-test/utils)
  (:import-from #:cl-dbi)
  (:export #:with-login
           #:with-test-db))
(in-package ultralisp-test/utils)


(defmacro with-test-db (&body body)
  `(ultralisp/db:with-connection ()
     (with-output-to-string (*standard-output*)
       (with-output-to-string (*error-output*)
         (mito:execute-sql "DROP SCHEMA IF EXISTS unittest CASCADE;")
         (mito:execute-sql "CREATE SCHEMA unittest AUTHORIZATION CURRENT_USER;")
         (mito:execute-sql "SET search_path TO unittest;")
         (mito:migrate "./db/")))
     ,@body))


(defmacro with-login ((&key (email "bob@example.com"))
                      &body body)
  `(weblocks-test/utils:with-session
     (let ((user (or (mito-email-auth/models:get-user-by-email ,email)
                     (mito:create-dao 'ultralisp/models/user:user
                                      :email ,email))))
       (mito-email-auth/models:authenticate user)
       ,@body)))
