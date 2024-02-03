(defpackage #:ultralisp-test/github/webhook
  (:use #:cl
        #:rove)
  (:import-from #:hamcrest/rove
                #:has-slots
                #:has-length
                #:assert-that
                #:contains
                #:has-type)
  (:import-from #:ultralisp-test/utils
                #:with-test-db)
  (:import-from #:ultralisp/models/project
                #:make-project-from-url
                #:project-sources
                #:make-github-project)
  (:import-from #:ultralisp/github/webhook
                #:process-payload)
  (:import-from #:ultralisp/models/check
                #:source-checks
                #:get-project-checks)
  (:import-from #:ultralisp/sources/setup
                #:setup-sources))
(in-package #:ultralisp-test/github/webhook)


(defun load-payload (name)
  (let* ((relative-name (format nil "t/fixtures/~A.json" name))
         (path (asdf:system-relative-pathname :ultralisp-test relative-name))
         (data (alexandria:read-file-into-string path :external-format :utf-8)))
    (jonathan:parse data
                    :as :alist)))


(deftest test-case-when-webhook-receives-a-payload-for-known-project
  (setup-sources)
  
  (with-test-db
    (testing "A new check should be created for the project"
      (let* ((payload (load-payload "push-to-master"))
             (project (make-project-from-url "https://github.com/40ants/log4cl-json"
                                             :moderator nil))
             (sources (project-sources project)))

        (assert-that sources
                     (has-length 1))
        
        ;; Delete :added check because while it is there a new check cannot be created.
        (mapcar #'mito:delete-dao
                (source-checks (first sources)))

        (let* ((source (first sources))
               (dists (ultralisp/models/dist-source:source->dists source)))
          (assert-that dists
                       (has-length 1)))

        (ok (process-payload payload))

        (loop for source in sources
              for checks = (source-checks source)
              do (assert-that checks
                              (contains (has-slots 'ultralisp/models/check::type
                                                   :via-webhook))))))))
