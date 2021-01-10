(defpackage #:ultralisp-test/pipeline
  (:use #:cl
        #:rove)
  (:import-from #:hamcrest/rove
                #:assert-that
                #:contains
                #:has-type)
  (:import-from #:ultralisp-test/utils
                #:with-login
                #:get-source
                #:get-all-dist-projects
                #:with-metrics
                #:with-test-db)
  (:import-from #:ultralisp/models/dist
                #:find-dist)
  (:import-from #:ultralisp/models/project
                #:add-or-turn-on-github-project))
(in-package ultralisp-test/pipeline)


(deftest test-enable-source-if-check-was-successful
  ;; Here we simulate a situation when there is a
  ;; source which was disabled in the dist,
  ;; but now cron check was successful, and
  ;; a new dist version should be created where
  ;; this source will be enabled.
  (with-test-db
    (with-login ()
      (let* ((project (add-or-turn-on-github-project "40ants/defmain"))
             ;; (source (get-source project))
             (ultralisp (find-dist "ultralisp")))
        (declare (ignore project))

        (ultralisp/builder::prepare-pending-dists)
        (ultralisp/builder::build-prepared-dists)

        (testing "At this moment dist should include enabled"
          (ok (equal (get-all-dist-projects ultralisp :enabled t)
                     '("40ants/defmain")))))))
  )
