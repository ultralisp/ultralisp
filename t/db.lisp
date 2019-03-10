(defpackage #:ultralisp-test/db
  (:use #:cl
        #:rove)
  (:import-from #:ultralisp/db
                #:with-connection)
  (:import-from #:alexandria
                #:assoc-value))
(in-package ultralisp-test/db)


(deftest test-with-connection
  (ultralisp-test/utils:with-test-db ()
    (with-connection ()
      (ok (= (first
              (assoc-value (mito:retrieve-by-sql "select 1 as value")
                           :value))
             1)))
    (ok (= (first
            (assoc-value (mito:retrieve-by-sql "select 2 as value")
                         :value))
           2))))
