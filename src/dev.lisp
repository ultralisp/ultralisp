(uiop:define-package #:ultralisp/dev
  (:use #:cl)
  (:export #:run-check))
(in-package #:ultralisp/dev)


(defun run-check (project-name)
  (let* ((project (or (ultralisp/models/project:get-project2 project-name)
                      (error "Project \"~S\" was not found"
                             project-name)))
         (checks
           (or (ultralisp/models/check:make-checks project
                                                      :manual)
               (error "No checks were created for \"~S\" project"
                      project-name))))
    (ultralisp/rpc/core:submit-task 'ultralisp/pipeline/checking::perform2
                                    :args (list (first checks)
                                                :force t))))



