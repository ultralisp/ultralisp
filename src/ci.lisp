(defpackage #:ultralisp/ci
  (:use #:cl)
  (:import-from #:40ants-ci/jobs/linter)
  (:import-from #:40ants-ci/workflow
                #:defworkflow)
  (:import-from #:40ants-ci/jobs/run-tests)
  (:import-from #:40ants-ci/jobs/docs))
(in-package #:ultralisp/ci)


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


(defclass build-docs (ultralisp-job-mixin 40ants-ci/jobs/docs:build-docs)
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
  :env (("CL_SOURCE_REGISTRY" . "${{ github.workspace }}/"))
  :jobs ((linter
          :lisp "sbcl-bin/2.2.5")
         (run-tests
          :lisp "sbcl-bin/2.1.2"
          ;; Does not work because cl-coverage complains
          ;; it can't quickload app-deps. Hmmm...
          ;; :coverage nil
          )))


(defworkflow docs
  :on-push-to "master"
  :by-cron "0 10 * * 1"
  :on-pull-request t
  ;; :cache t
  :jobs ((build-docs :asdf-system "ultralisp-docs"
                     ;; Because of unknown issue, builder on CI shows this message but not a single warning:
                     ;; WARNING: 92 warnings were caught
                     ;; Thus I've turned off them for a while
                     :error-on-warnings nil
                     )))
