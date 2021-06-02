(uiop:define-package #:ultralisp/models/check
  (:use #:cl)
  (:import-from #:rove
                #:deftest)
  (:import-from #:mito)
  (:import-from #:ultralisp-test/utils
                #:with-test-db)
  (:import-from #:ultralisp/models/dist-source
                #:add-source-to-dist)
  (:import-from #:ultralisp-test/utils
                #:make-project
                #:get-source)
  (:import-from #:rove
                #:ok)
  (:export
   #:lisp-implementation))
(in-package ultralisp/models/check)


(deftest test-check-lisp-implementation
  (with-test-db
    (let* ((project (make-project "40ants" "defmain"))
           (lispworks-dist (mito:create-dao 'ultralisp/models/dist:dist
                                            :name "LispWorks"
                                            :lisp-implementation :lispworks)))
      ;; And to switch dist into a READY state
      ;; (build-dists)

      (let* ((source (get-source project)))
        (add-source-to-dist lispworks-dist source)
        ;; At this point source will be bound to default "ultralisp" dist
        ;; and to the "lispworks" dist.
        
        (let ((check (make-check source :via-cron)))
          ;; check's implementation should match the dist ones
          (ok (eql (ultralisp/models/check::lisp-implementation check)
                   :lispworks)))))))
