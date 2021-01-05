(defpackage #:ultralisp-test/models/project
  (:use #:cl
        #:rove)
  (:import-from #:hamcrest/rove
                #:assert-that
                #:contains
                #:has-type)
  (:import-from #:weblocks-test/utils)
  (:import-from #:ultralisp-test/utils
                #:get-source
                #:make-project
                #:with-test-db
                #:with-metrics
                #:with-login)
  (:import-from #:ultralisp/models/action
                #:project-added
                #:project-updated
                #:project-removed
                #:get-project-actions)
  (:import-from #:ultralisp/utils/github
                #:extract-github-name)
  (:import-from #:ultralisp/models/project
                #:project-sources
                #:disable-project
                #:update-and-enable-project
                #:make-github-project
                #:is-enabled-p
                #:get-last-seen-commit
                #:add-or-turn-on-github-project)
  (:import-from #:ultralisp/models/check
                #:source-checks
                #:get-project-checks)
  (:import-from #:ultralisp/models/source
                #:source-params
                #:create-new-source-version)
  (:import-from #:ultralisp/models/versioned
                #:object-version)
  (:import-from #:ultralisp/models/dist-source
                #:enabled-p))
(in-package ultralisp-test/models/project)


(deftest test-adding-github-project
  (with-test-db
    (with-login ()
      (testing "After the project was added it should have bound check and zero count of actions"
        (let* ((project (add-or-turn-on-github-project "40ants/defmain"))
               (sources (project-sources project)))

          (loop for source in sources
                for checks = (source-checks source)
                do (ok (= (length checks)
                          1)
                       "A new check should be created")))))))


(deftest test-create-new-source-version
  (with-test-db
    (with-metrics
      (testing "When project check was successful and a new source version should be created and attached to the new pending dist"
        (let* ((project (make-project "40ants" "defmain"))
               (source (get-source project))
               (dists (ultralisp/models/dist-source:source->dists source))
               (dist (first dists))
               ;; Systems can be ignored for this test:
               (systems nil))
          (ok (typep dist 'ultralisp/models/dist:bound-dist))
          
          ;; (setf  ;; (enabled-p dist) t
          ;;       (get-last-seen-commit source)
          ;;       "1234")
          
          ;; Now we emulate a situation when project's commit hash was changed
          (create-new-source-version source
                                     systems
                                     ;; This value should be overriden in a new
                                     ;; source version:
                                     (list :last-seen-commit "abcd"))
          
          (let ((new-source (get-source project)))
            (testing "New version of the source should be created"
              (testing "New version is not the same instance"
                (ok (not (eq source new-source))))
              (testing "And has a large version id"
                (ok (> 
                     (object-version new-source)
                     (object-version source)))))
         
            (testing "Source parameters should be updated as well"
              (ok (equal (getf (source-params new-source)
                               :last-seen-commit)
                         "abcd")))))))))


(deftest test-update-and-enable-when-project-is-disabled
  (with-test-db
    (with-metrics
      (testing "When project is disabled we need to create project-added action."
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
                       "abcd"))))))))


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


