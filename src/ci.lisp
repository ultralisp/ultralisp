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


(defworkflow ci
  :on-push-to "master"
  :by-cron "0 10 * * 1"
  :on-pull-request t
  :cache t
  :jobs ((linter)
         (run-tests
          :coverage t)))
