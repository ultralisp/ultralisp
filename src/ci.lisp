(defpackage #:ultralisp/ci
  (:use #:cl)
  (:import-from #:40ants-ci/jobs/linter)
  (:import-from #:40ants-ci/workflow
                #:defworkflow)
  (:import-from #:40ants-ci/jobs/run-tests))
(in-package ultralisp/ci)


(defclass ultralisp-job-mixin ()
  ())


(defmethod 40ants-ci/jobs/job:steps :around ((job ultralisp-job-mixin))
  (list*
   (40ants-ci/steps/sh:sh "Install libev"
                          "sudo apt-get install libev4")
   (call-next-method )))


(defclass linter (ultralisp-job-mixin 40ants-ci/jobs/linter:linter)
  ())


(defclass run-tests (ultralisp-job-mixin 40ants-ci/jobs/run-tests:run-tests)
  ())


(defmethod 40ants-ci/github:prepare-data :around ((job run-tests))
  (list*
   '("services"
     . (("postgres"
         . (("image" . "postgres:10")
            ("env" . (("POSTGRES_USER" . "ultralisp")
                      ("POSTGRES_PASSWORD" . "ultralisp")))
            ("options"
             . "--health-cmd pg_isready --health-interval 10s --health-timeout 5s --health-retries 5")
            ("ports" . ("5432:5432"))))))
   (call-next-method)))


(defworkflow ci
  :on-push-to "master"
  :by-cron "0 10 * * 1"
  :on-pull-request t
  :cache t
  :jobs ((linter
          :lisp "sbcl-bin/2.2.5")
         (run-tests
          :lisp "sbcl-bin/2.1.2"
          :coverage t)))
