(defpackage #:ultralisp-test/pipeline
  (:use #:cl
        #:rove)
  (:import-from #:hamcrest/rove
                #:assert-that
                #:contains
                #:has-type)
  (:import-from #:ultralisp-test/utils
                #:get-all-dist-project-names
                #:make-project
                #:get-dist
                #:build-dists
                #:with-login
                #:get-source
                #:with-metrics
                #:with-test-db)
  (:import-from #:ultralisp/models/dist
                #:dist-state
                #:dist-equal
                #:find-dist)
  (:import-from #:ultralisp/models/project
                #:add-or-turn-on-project)
  (:import-from #:ultralisp/models/source
                #:source-last-check-failed
                #:create-new-source-version)
  (:import-from #:ultralisp/pipeline/checking
                #:update-check-as-failed2
                #:update-check-as-successful2)
  (:import-from #:ultralisp/models/check
                #:make-check)
  (:import-from #:ultralisp/models/versioned
                #:refetch
                #:object-version))
(in-package #:ultralisp-test/pipeline)


(deftest test-source-update-on-successful-check
  ;; Here we simulate a situation when there is a
  ;; source which was marked as having an error on last check in the dist,
  ;; but when a new check will be successful,
  ;; a new dist version should be created.
  (block foo
    (with-test-db
      (with-login ()
        (let* ((project (make-project "40ants" "defmain"))
               (source-v0 (get-source project)))
          ;; First, we need to create a version which is enabled.
          (create-new-source-version source-v0 nil nil
                                     ;; This argument is t by default,
                                     ;; but here we use it to make a test explicits 
                                     :enable t)
          ;; And to switch dist into a READY state
          (build-dists)
        
          (let* ((dist-before (find-dist "ultralisp"))
                 (source-v1 (get-source project))
                 (check (make-check source-v1 :via-cron)))

            ;; Simulate that last check on source was failed:
            ;; (setd (source-last-check-failed source-v1)
            ;;       t)
            ;; (mito:save-dao source-v1)
          
            ;; check's implementation should match the dist ones
            (ok (eql (ultralisp/models/check:lisp-implementation check)
                     :sbcl))
            (ok (eql (ultralisp/models/check:lisp-implementation check)
                     (ultralisp/models/dist:lisp-implementation dist-before)))
            (ok (eql (dist-state dist-before)
                     :ready))
        
            ;; This is a check which will simulate an error
            (testing "Updating the check"
              (update-check-as-failed2 check
                                       ;; traceback
                                       "Some error"
                                       ;; processed-in
                                       0.1))
            ;; And to switch dist into a READY state again
            (build-dists)

            (ok (null (source-last-check-failed source-v1)))

            
            (let ((dist-after (find-dist "ultralisp")))
              (testing "New dist version should not be created because the check was failed"
                ;; No new dist was created because when check fails
                ;; we only mark source as having a failed check, but
                ;; not exclude it from the dist - thus not making a new version
                ;; of the dist.
                (ok (dist-equal dist-before
                                dist-after)))))

          (let* ((source-v1 (get-source project)))
            (testing "Now we simulate a situation when project's check was successful but didn't find any changes"
              (create-new-source-version source-v1 nil nil :enable t)

              (let ((source-v2 (get-source project)))
                ;; A new source version should be created:
                (ok (not (= (object-version source-v1)
                            (object-version source-v2))))

                ;; And neither this new version should be marked as failed:
                (ok (null (source-last-check-failed source-v2)))
                ;; nor an old source version (but we have to fetch updated obj from db):
                (ok (null (source-last-check-failed (refetch source-v1))))))))))))
