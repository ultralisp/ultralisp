(defpackage #:ultralisp-test/models/project
  (:use #:cl
        #:rove)
  (:import-from #:hamcrest/rove
                #:assert-that
                #:contains
                #:has-type)
  (:import-from #:weblocks-test/utils)
  (:import-from #:ultralisp-test/utils
                #:with-test-db
                #:with-login)
  (:import-from #:ultralisp/models/action
                #:project-added
                #:project-updated
                #:project-removed
                #:get-project-actions)
  (:import-from #:ultralisp/models/project
                #:extract-github-name
                #:disable-project
                #:update-and-enable-project
                #:make-github-project
                #:is-enabled-p
                #:get-last-seen-commit
                #:add-or-turn-on-github-project)
  (:import-from #:ultralisp/models/check
                #:get-project-checks))
(in-package ultralisp-test/models/project)


(deftest test-adding-github-project
  (with-test-db
    (with-login ()
      (testing "After the project was added it should have bound check and zero count of actions"
        (let* ((project (add-or-turn-on-github-project "40ants/defmain"))
               (actions (get-project-actions project))
               (checks (get-project-checks project)))
          
          (ok (= (length checks)
                 1)
              "A new check should be created")

          (ok (null actions)
              "There shouldn't be any action"))))))


(deftest test-update-and-enable-when-project-is-enabled-and-there-was-an-update
  (with-test-db
    (testing "When project already enabled we need to create project-updated action."
      (let ((project (make-github-project "40ants" "defmain")))
        (setf (is-enabled-p project)
              t
              (get-last-seen-commit project)
              "1234")
        ;; Now we emulate a situation when project's commit hash was changed
        (update-and-enable-project project
                                   (list :last-seen-commit "abcd"))
        (let ((actions (get-project-actions project)))
          (testing "Project-updated action should be created"
            (assert-that actions
                         (contains
                          (has-type 'project-updated)))))
        
        (testing "Project parameters should be updated as well"
          (ok (equal (get-last-seen-commit project)
                     "abcd")))))))


(deftest test-update-and-enable-when-project-is-disabled
  (with-test-db
    (testing "When project is idsabled we need to create project-added action."
      (let ((project (make-github-project "40ants" "defmain")))
        ;; Now we emulate a situation when we checked the project
        ;; and discovered it's commit hash 
        (update-and-enable-project project
                                   (list :last-seen-commit "abcd"))
        
        (testing "Project should be enabled now"
          (ok (is-enabled-p project)))
        
        (let ((actions (get-project-actions project)))
          (testing "Project-added action should be created"
            (assert-that actions
                         (contains
                          (has-type 'project-added)))))
        
        (testing "Project parameters should be updated as well"
          (ok (equal (get-last-seen-commit project)
                     "abcd")))))))


(deftest test-update-and-enable-when-project-is-enabled-and-there-is-no-difference
  (with-test-db
    (testing "When there is no difference, we shouldn't create any action"
      (let ((project (make-github-project "40ants" "defmain")))
        (setf (is-enabled-p project)
              t
              (get-last-seen-commit project)
              "1234")
        ;; Now we emulate a situation when project's commit hash was changed
        (update-and-enable-project project
                                   (list :last-seen-commit "1234"))
        (let ((actions (get-project-actions project)))
          (ok (null actions)
              "This should create a one action"))))))


(deftest test-project-disabling
  (with-test-db
    (testing "After the project was disabled, we should create a new action and version."
      (let ((project (make-github-project "40ants" "defmain")))
        (setf (is-enabled-p project)
              t)

        (disable-project project)
        
        (let ((actions (get-project-actions project)))
          (assert-that actions
                       (contains
                        (has-type 'project-removed))))))))

(deftest test-github-url-extraction
  (ok (equal (extract-github-name "https://github.com/Dimercel/listopia")
             "Dimercel/listopia"))
  (ok (equal (extract-github-name "http://github.com/Dimercel/listopia")
             "Dimercel/listopia"))
  (ok (equal (extract-github-name "https://github.com/Dimercel/listopia.git")
             "Dimercel/listopia")))

;; TODO: action should be bound to a new pending version we need a separate test for it


