(defpackage #:ultralisp-test/pipeline
  (:use #:cl
        #:rove)
  (:import-from #:hamcrest/rove
                #:assert-that
                #:contains
                #:has-type)
  (:import-from #:ultralisp-test/utils
                #:make-project
                #:get-dist
                #:build-dists
                #:with-login
                #:get-source
                #:get-all-dist-projects
                #:with-metrics
                #:with-test-db)
  (:import-from #:ultralisp/models/dist
                #:dist-state
                #:dist-equal
                #:find-dist)
  (:import-from #:ultralisp/models/project
                #:add-or-turn-on-github-project)
  (:import-from #:ultralisp/models/source
                #:create-new-source-version)
  (:import-from #:ultralisp/pipeline/checking
                #:update-check-as-failed2
                #:update-check-as-successful2)
  (:import-from #:ultralisp/models/check
                #:make-check))
(in-package ultralisp-test/pipeline)


(deftest test-enable-source-if-check-was-successful
  ;; Here we simulate a situation when there is a
  ;; source which was disabled in the dist,
  ;; but now cron check was successful, and
  ;; a new dist version should be created where
  ;; this source will be enabled.
  (setf ultralisp/models/dist-source::*cnt* 0)
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

          ;; Just in case, we'll check that that a new dist was created
          ;; for this failed check and disabled source:
          (let ((dist-after (find-dist "ultralisp")))
            (testing "A new dist version should be created after the check was failed"
              (ok (not
                   (dist-equal dist-before
                               dist-after)))
              (ok (eql (dist-state dist-after)
                       :ready)))))

        (let ((dist-before (find-dist "ultralisp")))
          (testing "At this moment dist should be disabled"
            (ok (eql (dist-state dist-before)
                     :ready))
            (ok (equal (get-all-dist-projects dist-before
                                              :enabled t)
                       nil)))
        
          (let* ((source-v1 (get-source project)))
            (testing "Now we simulate a situation when project's check was successful but didn't find any changes"
              (ultralisp/models/source:enable-this-source-version source-v1)
            
              ;; (build-dists)

              (let ((dist-after (find-dist "ultralisp")))
                (testing "A new dist version should be created"
                  (ok (not
                       (dist-equal dist-before
                                   dist-after))))
              
                (testing "And now dist should include enabled project"
                  (ok (equal (get-all-dist-projects dist-after :enabled t)
                             '("40ants/defmain"))))))))))))
