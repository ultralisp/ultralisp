(defpackage #:ultralisp-test/models/project
  (:use #:cl
        #:rove)
  (:import-from #:hamcrest/rove
                #:assert-that
                #:contains
                #:has-type)
  (:import-from #:reblocks-test/utils)
  (:import-from #:ultralisp-test/utils
                #:get-all-dist-project-names
                #:get-projects-linked-to-the
                #:get-all-dist-names
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
                #:enabled-p)
  (:import-from #:reblocks-auth/models
                #:get-current-user)
  (:import-from #:ultralisp/models/dist
                #:disable-reason
                #:dist-equal
                #:dist-state
                #:find-dist)
  (:import-from #:ultralisp/models/dist-moderator
                #:add-dist)
  (:import-from #:ultralisp/models/dist-source
                #:disable-reason-type
                #:create-pending-dists-for-new-source-version
                #:delete-source
                #:source-distributions
                #:update-source-dists
                #:dist-id))
(in-package ultralisp-test/models/project)


(deftest test-github-url-extraction
  (ok (equal (extract-github-name "https://github.com/Dimercel/listopia")
             "Dimercel/listopia"))
  (ok (equal (extract-github-name "http://github.com/Dimercel/listopia")
             "Dimercel/listopia"))
  (ok (equal (extract-github-name "https://github.com/Dimercel/listopia.git")
             "Dimercel/listopia")))


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
              (testing "New dist has the same version"
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


(defun run-test-source-disabling (&key pending-dists)
  (with-test-db
    (with-metrics
      (testing "When project check was unsuccessful a new source version should be created and disabled"
        (let* ((project (make-project "40ants" "defmain"))
               (source (get-source project)))
          ;; First, we need to create a version which is enabled.
          (create-new-source-version source nil nil
                                     ;; This argument is t by default,
                                     ;; but here we use it to make a test explicits 
                                     :enable t)

          (unless pending-dists
            (ultralisp/builder::prepare-pending-dists)
            (ultralisp/builder::build-prepared-dists))

          ;; Retrieve new version of the source:
          (let* ((source (get-source project))
                 (dist (get-dist source))
                 (check (make-check source :via-cron)))

            (testing "At this moment project should be enabled"
              (ok (enabled-p dist)))
            
            (testing "Now we emulate a situation when project's check was failed"
              (ultralisp/pipeline/checking::update-check-as-failed2 check
                                                                    ;; traceback
                                                                    "Some error"
                                                                    ;; processed-in
                                                                    0.1)
              (unless pending-dists
                (ultralisp/builder::prepare-pending-dists)
                (ultralisp/builder::build-prepared-dists))
            
              (let* ((new-source (get-source project))
                     (new-dist (get-dist new-source)))
                (testing "New version of the source should not be, because it was bound to a :PENDING dist"
                  (ok (=
                       (object-version new-source)
                       (object-version source))))

                (testing "New source should be disabled"
                  (ok (not (enabled-p new-dist))))



                (testing "When another check fails again, a new version of the dist should not be createdd, because source already disabled"
                  (let ((another-check (make-check new-source :via-cron)))
                  
                    ;; Now we emulate a situation when second check also failed   
                    (ultralisp/pipeline/checking::update-check-as-failed2 another-check
                                                                          ;; traceback
                                                                          "Another error"
                                                                          ;; processed-in
                                                                          0.1)

                    (unless pending-dists
                      (ultralisp/builder::prepare-pending-dists)
                      (ultralisp/builder::build-prepared-dists))
                  
                    (let* ((last-source-version (get-source project))
                           (last-dist-version (get-dist last-source-version)))
                      (testing "New version of the source should not be created because it wasn't changed"
                        (ok (=
                             (object-version last-source-version)
                             (object-version new-source))))

                      (testing "New dist version should not be created because source already disabled"
                        (ok (dist-equal last-dist-version
                                        new-dist))))))))))))))


(deftest test-source-disabling
  (run-test-source-disabling :pending-dists nil))


(deftest test-source-disabling-when-dist-is-pending
  (run-test-source-disabling :pending-dists t))


(deftest test-source-distribution-changes
  (with-test-db
    (with-metrics
      (with-login ()
        (let* ((user (get-current-user))
               (project (make-project "40ants" "defmain"))
               (source-v0 (get-source project)))
          ;; First, let's add a two distributions:
          (add-dist user "foo")
          (add-dist user "bar")

          ;; At this point, source should be bound only to
          ;; common Ultralisp distribution.
          
          ;; Let's enable the source, first.
          ;; After this call source should be bound to a new PENDING version.
          (create-new-source-version source-v0 nil nil)

          (let ((source-v1 (get-source project))
                (source-v2 nil)
                (source-v3 nil)
                (source-v4 nil))

            (testing "Initially project should be only in \"ultralisp\" dist"
              (ok (equal (get-all-dist-names source-v1 :enabled t)
                         '("ultralisp"))))
          
            (testing "Adding source to dist \"foo\""
              (setf source-v2
                    (update-source-dists source-v1
                                         :dists '("ultralisp" "foo")))
              (ok (equal (get-all-dist-names source-v2 :enabled t)
                         '("foo" "ultralisp"))))
            
            (testing "Adding source to dist \"bar\" and removing from \"foo\""
              (setf source-v3
                    (update-source-dists source-v2
                                         :dists '("ultralisp" "bar")))
              (ok (equal (get-all-dist-names source-v3)
                         '("bar" "ultralisp"))))
            
            (testing "Adding source to dist \"foo\" again and removing from \"bar\""
              (setf source-v4
                    (update-source-dists source-v3
                                         :dists '("ultralisp" "foo" "bar")))
              (ok (equal (get-all-dist-names source-v4)
                         '("bar" "foo" "ultralisp"))))))))))


(defun run-deletion-test (&key pending-dists)
  (with-test-db
    (with-metrics
      (with-login ()
        (let* ((user (get-current-user))
               (project (make-project "40ants" "defmain"))
               (source (get-source project)))
          (flet ((retrieve-latest-source ()
                   (setf source (get-source project))))
            ;; First, let's add a distribution:
            (add-dist user "foo")
          
            ;; At this point, source should be bound only to
            ;; common Ultralisp distribution.
          
            ;; Let's enable the source, first.
            ;; After this call source should be bound to a new PENDING version.
            (create-new-source-version source nil nil)
            (retrieve-latest-source)
            
            (update-source-dists source
                                 :dists '("ultralisp" "foo"))
            (retrieve-latest-source)

            (unless pending-dists
              (ultralisp/builder::prepare-pending-dists)
              (ultralisp/builder::build-prepared-dists))

            (let ((ultralisp (find-dist "ultralisp"))
                  (foo (find-dist "foo")))
              
              (if pending-dists
                  (testing "All dist versions should be pending now"
                    (ok (eql (dist-state ultralisp)
                             :pending))
                    (ok (eql (dist-state foo)
                             :pending)))
                  (testing "All dist versions should not be pending now"
                    (ok (eql (dist-state ultralisp)
                             :ready))
                    (ok (eql (dist-state foo)
                             :ready))))

              (testing "Both dists should include the project 40ants/defmain"
                (ok (equal (get-all-dist-project-names ultralisp)
                           '("40ants/defmain")))
                (ok (equal (get-all-dist-project-names foo)
                           '("40ants/defmain"))))

              (delete-source source)

              (let ((ultralisp-after (find-dist "ultralisp"))
                    (foo-after (find-dist "foo")))
                (if pending-dists
                    (testing "No new dist versions should be created, because dists were pending"
                      (ok (dist-equal ultralisp
                                      ultralisp-after))
                      (ok (dist-equal foo
                                      foo-after)))
                    (testing "New dist versions should be created"
                      (ok (not
                           (dist-equal ultralisp
                                       ultralisp-after)))
                      (ok (not
                           (dist-equal foo
                                       foo-after)))))

                (testing "Project 40ants/defmain should be deleted and disabled in new dists verssions"
                  (ok (equal (get-projects-linked-to-the ultralisp-after)
                             '((:name "40ants/defmain"
                                :enabled nil
                                :deleted t))))
                  (ok (equal (get-projects-linked-to-the foo-after)
                             '((:name "40ants/defmain"
                                :enabled nil
                                :deleted t)))))

                (testing "Project-sources should not return this source anymore"
                  (ok (null (project-sources project))))))))))))


(deftest test-delete-source-from-pending-dist
  (run-deletion-test :pending-dists t))


(deftest test-delete-source-from-non-pending-dist
  (run-deletion-test :pending-dists nil))



(deftest test-source-distributions-returns-latest-versions-of-the-dists
  ;; This is normal situation when the single version of the source is
  ;; connected to a few versions of the same dist.
  ;; But source-distributions function should return only
  ;; the latest versions of the dists.
  (with-test-db
    (with-login ()
      (testing "After the project was added it should have bound check and zero count of actions"
        (let* ((project (add-or-turn-on-github-project "40ants/defmain"))
               (source (get-source project)))

          (ultralisp/builder::prepare-pending-dists)
          (ultralisp/builder::build-prepared-dists)

          ;; Now we'll add this dist into a new dist version:
          (create-pending-dists-for-new-source-version source source)

          (let ((results (source-distributions source)))
            (testing "Function source-distribution should return only one dist"
              (ok (= (length results)
                     1)))
            (testing "And dist should have version 1, because it should be the latest (second) dist version."
              (ok (= (dist-id (first results))
                     1)))))))))
