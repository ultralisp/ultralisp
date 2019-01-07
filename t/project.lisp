(defpackage #:ultralisp-test/project
  (:use #:cl
        #:rove
        #:hamcrest/rove)
  (:import-from #:weblocks-test/utils)
  (:import-from #:ultralisp-test/utils
                #:with-test-db
                #:with-login)
  (:import-from #:ultralisp/models/action)
  (:import-from #:ultralisp/models/project
                #:add-or-turn-on-github-project))
(in-package ultralisp-test/project)


(deftest test-adding-github-project
  (with-test-db
    (with-login ()
      (testing "After the project was added it should have bound check and zero count of actions"
        (let* ((project (add-or-turn-on-github-project "40ants/defmain"))
               (actions (ultralisp/models/action:get-project-actions project))
               (checks (ultralisp/models/check:get-project-checks project)))
          
          (ok (= (length checks)
                 1)
              "A new check should be created")

          (ok (null actions)
              "There shouldn't be any action"))))))


(deftest test-update-and-enable
  (with-test-db
    (testing "When project already enabled we need to create project-updated action."
      (let ((project (ultralisp/models/project::make-github-project "40ants" "defmain")))
        (setf (ultralisp/models/project:is-enabled-p project)
              t
              (ultralisp/models/project:get-last-seen-commit project)
              "1234")
        ;; Now we emulate a situation when project's commit hash was changed
        (setf project
              (ultralisp/models/project:update-and-enable-project project
                                                                  :last-seen-commit "abcd"))
        (let ((actions (ultralisp/models/action:get-project-actions project)))
          (ok (= (length actions)
                 1)
              "This should create a one action")
          (ok (typep (first actions)
                     'ultralisp/models/action:project-updated)
              "And the action have to have type project-updated"))))))


(deftest test-project-disabling
  (with-test-db
    (testing "After the project was disabled, we should create a new action and version."
      (let ((project (ultralisp/models/project::make-github-project "40ants" "defmain")))
        (setf (ultralisp/models/project:is-enabled-p project)
              t)

        (ultralisp/models/project:disable-project project)
        
        (let ((actions (ultralisp/models/action:get-project-actions project)))
          (assert-that actions
                       (contains
                        (has-type 'ultralisp/models/action:project-removed)))))))
  )
;; TODO: action should be bound to a new pending version we need a separate test for it
