(defpackage #:ultralisp-test/github/webhook
  (:use #:cl
        #:rove)
  (:import-from #:hamcrest/rove
                #:assert-that
                #:contains
                #:has-type)
  (:import-from #:ultralisp-test/utils
                #:with-test-db)
  (:import-from #:ultralisp/models/project
                #:make-github-project)
  (:import-from #:ultralisp/github/webhook
                #:process-payload)
  (:import-from #:ultralisp/models/check
                #:get-project-checks))
(in-package ultralisp-test/github/webhook)


(defun load-payload (name)
  (let* ((relative-name (format nil "t/fixtures/~A.json" name))
         (path (asdf:system-relative-pathname :ultralisp-test relative-name))
         (data (alexandria:read-file-into-string path :external-format :utf-8)))
    (jonathan:parse data
                    :as :alist)))


(deftest test-case-when-webhook-receives-a-payload-for-known-project
  (with-test-db
    (testing "A new check should be created for the project"
      (let ((payload (load-payload "push-to-master"))
            (project (make-github-project "40ants" "log4cl-json")))
       
        (process-payload payload)

        (let ((checks (get-project-checks project)))
          (assert-that checks
                       (contains (has-type 'ultralisp/models/check:via-webhook-check))))))))
