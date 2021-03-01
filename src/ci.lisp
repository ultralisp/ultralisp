(defpackage #:ultralisp/ci
  (:use #:cl)
  (:import-from #:40ants-ci/jobs/linter)
  (:import-from #:40ants-ci/workflow
                #:defworkflow)
  (:import-from #:40ants-ci/jobs/run-tests
                #:run-tests))
(in-package ultralisp/ci)


(defclass linter (40ants-ci/jobs/linter:linter)
  ())


(defmethod 40ants-ci/jobs/job:steps :around ((job linter))
  (list*
   (40ants-ci/steps/sh:sh "Install libev"
                          "sudo apt-get install libev4")
   (call-next-method )))


(defworkflow ci
  :on-push-to "master"
  :by-cron "0 10 * * 1"
  :on-pull-request t
  :cache t
  :jobs ((linter)
         (run-tests
          :coverage t)))
