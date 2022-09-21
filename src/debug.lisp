(defpackage #:ultralisp/debug
  (:use #:cl)
  (:import-from #:ultralisp/models/project
                #:get-github-project)
  (:import-from #:ultralisp/models/check
                #:make-via-cron-check)
  (:import-from #:ultralisp/db
                #:with-transaction)
  (:import-from #:ultralisp/pipeline/checking
                #:perform-pending-checks)
  (:export
   #:check-project))
(in-package #:ultralisp/debug)

;; To make development more predictable, stop the cronjobs before going further:
;; (ultralisp/cron:stop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Checking project
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun check-project (username project-name)
  "Creates a check and performs it like ultralisp usually do."
  (let* ((project (get-github-project username project-name)))
    (make-via-cron-check project)
    (with-transaction
      (perform-pending-checks :force t))
    ;; return an updated object from DB
    (ultralisp/models/project:get-github-project username project-name)))
