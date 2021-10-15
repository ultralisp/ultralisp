(defpackage #:ultralisp-test/models/source
  (:use #:cl
        #:rove)
  (:import-from #:hamcrest/rove
                #:assert-that
                #:contains
                #:has-type)
  (:import-from #:weblocks-test/utils)
  (:import-from #:ultralisp-test/utils
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
                #:prev-version
                #:object-version)
  (:import-from #:ultralisp/protocols/enabled
                #:enabled-p)
  (:import-from #:weblocks-auth/models
                #:get-current-user)
  (:import-from #:ultralisp/models/dist
                #:dist-name
                #:dist-equal
                #:dist-state
                #:find-dist)
  (:import-from #:ultralisp/models/dist-moderator
                #:add-dist)
  (:import-from #:ultralisp/models/dist-source
                #:dist->sources
                #:create-pending-dists-for-new-source-version
                #:delete-source
                #:source-distributions
                #:update-source-dists
                #:dist-id)
  (:import-from #:mito
                #:object-id))
(in-package ultralisp-test/models/source)


(deftest test-prev-version-when-there-is-a-gap-in-dist-versions
  ;; Here we are modeling a situation when there are three
  ;; versions of the dist and source is bound to the first one
  ;; and to the third.

  (with-test-db
    (with-login ()
      (flet ((new-version (dist)
               (mito:create-dao 'ultralisp/models/dist::dist
                                :id (object-id dist)
                                :version (ultralisp/models/dist::get-next-dist-version dist)
                                :name (dist-name dist)
                                :state :ready)))
        (let* ((project (add-or-turn-on-github-project "40ants/defmain"))
               (source (get-source project))
               (ultralisp-v1 (find-dist "ultralisp"))
               (ultralisp-v2 (new-version ultralisp-v1))
               (ultralisp-v3 (new-version ultralisp-v2)))
         
          ;; Now we need to bind source to the latest dist version:
          (mito:create-dao 'ultralisp/models/dist-source::dist-source
                           :dist-id (object-id ultralisp-v3)
                           :dist-version (object-version ultralisp-v3)
                           :source-id (object-id source)
                           :source-version (object-version source)
                           :include-reason :direct)

          ;; Now we need to get a bound-source object, bound to the
          ;; latest dist version:
          (let* ((latest-source (first
                                 (dist->sources ultralisp-v3 :this-version t )))
                 ;; And to get the previous one:
                 (prev-source (prev-version latest-source)))
          
            (testing "Previous version should be bound to ultralisp-v1"
              (ok (not (null prev-source)))
              
              (ok (dist-equal (ultralisp/models/source:dist prev-source)
                              ultralisp-v1)))))))))


(deftest test-source-distributions-should-return-non-directly-bound-dist
  ;; Here we are modeling a situation when there are two
  ;; versions of the dist and source is bound to the first one.
  ;; Calling `source-distributions` method on this source
  ;; should return the first version of the dist.
  
  (with-test-db
    (with-login ()
      (flet ((new-version (dist)
               (mito:create-dao 'ultralisp/models/dist::dist
                                :id (object-id dist)
                                :version (ultralisp/models/dist::get-next-dist-version dist)
                                :name (dist-name dist)
                                :state :ready)))
        (let* ((project (add-or-turn-on-github-project "40ants/defmain"))
               (source (get-source project))
               (ultralisp-v1 (find-dist "ultralisp"))
               (ultralisp-v2 (new-version ultralisp-v1))
               (ultralisp-v3 (new-version ultralisp-v2)))
         
          ;; Now we need to bind source to the latest dist version:
          (mito:create-dao 'ultralisp/models/dist-source::dist-source
                           :dist-id (object-id ultralisp-v3)
                           :dist-version (object-version ultralisp-v3)
                           :source-id (object-id source)
                           :source-version (object-version source)
                           :include-reason :direct)

          ;; Now we need to get a bound-source object, bound to the
          ;; latest dist version:
          (let* ((latest-source (first
                                 (dist->sources ultralisp-v3 :this-version t )))
                 ;; And to get the previous one:
                 (prev-source (prev-version latest-source)))
          
            (testing "Previous version should be bound to ultralisp-v1"
              (ok (not (null prev-source)))
              
              (ok (dist-equal (ultralisp/models/source:dist prev-source)
                              ultralisp-v1)))))))))
