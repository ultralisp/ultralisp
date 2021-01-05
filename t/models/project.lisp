(defpackage #:ultralisp-test/models/project
  (:use #:cl
        #:rove)
  (:import-from #:hamcrest/rove
                #:assert-that
                #:contains
                #:has-type)
  (:import-from #:weblocks-test/utils)
  (:import-from #:ultralisp-test/utils
                #:get-dist
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
                #:make-check
                #:source-checks
                #:get-project-checks)
  (:import-from #:ultralisp/models/source
                #:source-params
                #:create-new-source-version)
  (:import-from #:ultralisp/models/versioned
                #:object-version)
  (:import-from #:ultralisp/protocols/enabled
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


(deftest test-create-new-source-version-when-source-is-bound-to-pending-dist
  (with-test-db
    (with-metrics
      (testing "When project check was successful and a new source version should be created and attached to the new pending dist"
        (let* ((project (make-project "40ants" "defmain"))
               (source (get-source project))
               (dist (get-dist source))
               ;; Systems can be ignored for this test:
               (systems nil))
          (ok (typep dist 'ultralisp/models/dist:bound-dist))

          (ok (not (enabled-p dist)))
          
          ;; Now we emulate a situation when project's commit hash was changed
          (create-new-source-version source
                                     systems
                                     ;; This value should be overriden in a new
                                     ;; source version:
                                     (list :last-seen-commit "abcd"))
          
          (let* ((new-source (get-source project))
                 (new-dist (get-dist new-source)))
            (testing "New version of the source should be created"
              (testing "New version is not the same instance"
                (ok (not (eq source new-source))))
              (testing "And has a large version id"
                (ok (> 
                     (object-version new-source)
                     (object-version source)))))

            (testing "The dist should not be changed, because originally source was bound to a PENDING dist"
              (testing "New dist hash the same version"
                (let ((old-dist (ultralisp/models/dist:dist dist))
                      (new-dist (ultralisp/models/dist:dist new-dist)))
                  (ok (= (object-version old-dist)
                         (object-version new-dist)))))
              (testing "And it is PENDING"
                (ok (eql (ultralisp/models/dist:dist-state dist)
                         :pending))))
            
            (testing "New source should be enabled"
              (ok (enabled-p new-dist)))
         
            (testing "Source parameters should be updated as well"
              (ok (equal (getf (source-params new-source)
                               :last-seen-commit)
                         "abcd")))))))))


(deftest test-source-disabling
  (with-test-db
    (with-metrics
      (testing "When project check was unsuccessful a new source version should be created and distabled"
        (let* ((project (make-project "40ants" "defmain"))
               (source (get-source project)))
          ;; First, we need to create a version which is enabled.
          ;; After this call source should be bound to a new PENDING version.
          (create-new-source-version source nil nil)

          ;; Retrieve new version of the source:
          (let* ((source (get-source project))
                 (dist (get-dist source))
                 (check (make-check source :via-cron)))

            (ok (enabled-p dist))
          
            ;; Now we emulate a situation when project's check was failed   
            (ultralisp/pipeline/checking::update-check-as-failed2 check
                                                                  ;; traceback
                                                                  "Some error"
                                                                  ;; processed-in
                                                                  0.1)
          
            (let* ((new-source (get-source project))
                   (new-dist (get-dist new-source)))
              (testing "New version of the source should not be, because it was bound to a :PENDING dist"
                (ok (=
                     (object-version new-source)
                     (object-version source))))

              (testing "New source should be disabled"
                (ok (not (enabled-p new-dist)))))))))))


(deftest test-github-url-extraction
  (ok (equal (extract-github-name "https://github.com/Dimercel/listopia")
             "Dimercel/listopia"))
  (ok (equal (extract-github-name "http://github.com/Dimercel/listopia")
             "Dimercel/listopia"))
  (ok (equal (extract-github-name "https://github.com/Dimercel/listopia.git")
             "Dimercel/listopia")))


