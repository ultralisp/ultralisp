(uiop:define-package #:ultralisp/dev
  (:use #:cl)
  (:export #:run-check))
(in-package #:ultralisp/dev)


(defun make-checks (project-name)
  "Creates one or more checks depending on how many dists this project is included to."
  
  (let* ((project (or (ultralisp/models/project:get-project2 project-name)
                      (error "Project \"~S\" was not found"
                             project-name)))
         (checks
           (or (ultralisp/models/check:make-checks project
                                                   :manual)
               (error "No checks were created for \"~S\" project"
                      project-name))))
    (values checks)))


(defun run-check (project-name)
  (let ((checks (make-checks project-name)))
    (ultralisp/rpc/core:submit-task 'ultralisp/pipeline/checking::perform2
                                    :args (list (first checks)
                                                :force t))))

(defparameter *broken-projects*
  (list
   "40ants/40ants-doc-theme-40ants"
   "40ants/reblocks")
  "A list of projects to recheck with CHECK-ALL function.")


(defparameter *checks-made-for* nil)


(defun check-all ()
  (ultralisp/db:with-connection ()
    (loop for name in *broken-projects*
          do (make-checks name))))
