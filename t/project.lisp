(defpackage #:ultralisp-test/project
  (:use #:cl
        #:rove
        #:hamcrest/rove)
  (:import-from #:weblocks-test/utils)
  (:import-from #:ultralisp/models/action)
  (:import-from #:ultralisp/models/project
                #:add-or-turn-on-github-project))
(in-package ultralisp-test/project)


(deftest test-adding-github-project
  (ultralisp/db:with-connection ()
    (unwind-protect
         (weblocks-test/utils:with-session
           (ultralisp-test/utils:with-login ()
            (testing "After the project was added it should have bound action and check"
              (let* ((project (add-or-turn-on-github-project "40ants/defmain"))
                     (actions (ultralisp/models/action::get-project-actions project))
                     ;; (checks (ultralisp/models/check::get-project-checks project))
                     )
                (ok (= (length actions)
                       1)
                    "There should be one action")
                (ok (typep (first actions)
                           'ultralisp/models/action::project-added))
                
                ;; (ok (= (length checks)
                ;;        1))
                ))))
      (cl-dbi:rollback mito:*connection*))))


;; TODO: action should be bound to a new pending version we need a separate test for it
