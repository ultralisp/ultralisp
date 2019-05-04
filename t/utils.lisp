(defpackage #:ultralisp-test/utils
  (:use #:cl)
  (:import-from #:ultralisp/db)
  (:import-from #:weblocks-test/utils)
  (:import-from #:cl-dbi)
  (:import-from #:ultralisp/metrics)
  (:export #:with-login
           #:with-test-db
           #:with-metrics))
(in-package ultralisp-test/utils)


(defmacro with-test-db (&body body)
  `(ultralisp/db:with-connection ()
     (with-output-to-string (*standard-output*)
       (with-output-to-string (*error-output*)
         (mito:execute-sql "DROP SCHEMA IF EXISTS unittest CASCADE;")
         (mito:execute-sql "CREATE SCHEMA unittest AUTHORIZATION CURRENT_USER;")
         (mito:execute-sql "SET search_path TO unittest;")
         (mito:migrate "./db/")))
     (unwind-protect (progn ,@body)
       ;; We need to return search path to a original state
       ;; to not disrupt accessing real database from the REPL
       (mito:execute-sql "SET search_path TO public;"))))


(defmacro with-login ((&key (email "bob@example.com"))
                      &body body)
  `(weblocks-test/utils:with-session
     (let* ((user (or (weblocks-auth/models:get-user-by-email ,email)
                      (mito:create-dao 'weblocks-auth/models:user
                                       :nickname ,email
                                       :email ,email))))
       (setf (weblocks-auth/models:get-current-user)
             user)
       ,@body)))


(defmacro with-metrics (&body body)
  `(progn (ultralisp/metrics:initialize)
          ,@body))
