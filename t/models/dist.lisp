(defpackage #:ultralisp-test/models/dist
  (:use #:cl
        #:rove)
  (:import-from #:hamcrest/rove
                #:assert-that
                #:contains
                #:has-type)
  (:import-from #:ultralisp-test/utils
                #:with-test-db
                #:with-login)
  (:import-from #:ultralisp/models/dist
                #:dist-name)
  (:import-from #:ultralisp/models/dist-moderator
                #:add-dist
                #:moderated-dists)
  (:import-from #:weblocks-auth/models
                #:get-current-user))
(in-package ultralisp-test/models/dist)


(deftest test-adding-new-dist
  (with-test-db
    (with-login ()
      (let ((user (get-current-user)))
        (testing "Checking initial dists list available to user"
          (ok (null (moderated-dists user))))
      
        (testing "Adding a dist"
          (add-dist user "foo"))

        (testing "Checking user now can see a new dist"
          (ok (equal (mapcar #'dist-name
                             (moderated-dists user))
                     '("foo"))))))))
