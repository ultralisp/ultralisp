(defpackage #:ultralisp-test/pipeline
  (:use #:cl
        #:rove)
  (:import-from #:hamcrest/rove
                #:assert-that
                #:contains
                #:has-type)
  (:import-from #:ultralisp-test/utils
                #:with-metrics
                #:with-test-db)
  (:import-from #:ultralisp/models/action
                #:get-project-actions
                #:project-added)
  (:import-from #:ultralisp/pipeline/checking
                #:perform)
  (:import-from #:ultralisp/models/check
                #:make-added-project-check)
  (:import-from #:ultralisp/models/project
                #:make-github-project))
(in-package ultralisp-test/pipeline)


(deftest test-create-project-added-action
  (with-test-db
    (with-metrics
      (testing "When project is disabled and check of type add-project-check we need to enable project and tocreate project-added action."
        (let* ((project (make-github-project "40ants" "defmain"))
               (check (make-added-project-check project)))

          (ok (null (get-project-actions project))
              "Before checking, there shouldn't be any actions bound to the project")

          (perform check)
         
          (let ((actions (get-project-actions project)))
            (testing "Created action should be of `project-added' type"
              (assert-that actions
                           (contains
                            (has-type 'project-added))))

            (testing "And project should be enabled"
              (ok (ultralisp/models/project:is-enabled-p project)))))))))
