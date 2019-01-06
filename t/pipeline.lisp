(defpackage #:ultralisp-test/pipeline
  (:use #:cl
        #:rove
        #:hamcrest/rove)
  (:import-from #:ultralisp-test/utils
                #:with-test-db))
(in-package ultralisp-test/pipeline)


(deftest test-create-project-added-action
  (with-test-db
    (testing "When project is disabled and check of type add-project-check we need to enable project and tocreate project-added action."
      (let* ((project (ultralisp/models/project::make-github-project "40ants" "defmain"))
             (check (ultralisp/models/check:make-added-project-check project)))

        (ok (null (ultralisp/models/action:get-project-actions project))
            "Before checking, there shouldn't be any actions bound to the project")

        (ultralisp/pipeline/checking:perform-check check)
        
        (let ((actions (ultralisp/models/action:get-project-actions project)))
          (assert-that actions
                       (contains
                        (has-type 'ultralisp/models/action:project-added))))))))
