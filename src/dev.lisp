(uiop:define-package #:ultralisp/dev
  (:use #:cl)
  (:export #:run-check
           #:check-broken-projects))
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
   "40ants/logging"
   "adlai/scalpl"
   "ak-coram/cl-duckdb"
   "bpanthi977/qoi"
   "byulparan/sc-extensions"
   "commonlispbr/starwar"
   "fosskers/cl-transducers"
   "fukamachi/mito"
   "HectareaGalbis/clith"
   "HectareaGalbis/expanders"
   "marijnh/Postmodern"
   "mark-watson/openai"
   "ndantam/sycamore"
   "qitab/cl-protobufs"
   "resttime/cl-liballegro"
   "ruricolist/cmd"
   "ryukinix/lisp-chat"
   "ryukinix/lisp-inference"
   "s-expressionists/Khazern"
   "shamazmazum/vp-trees"
   "Shinmera/clss"
   "Shirakumo/alloy"
   "Shirakumo/cl-mixed"
   "Shirakumo/glsl-toolkit"
   "Shirakumo/trial"
   "slburson/misc-extensions"
   "slime/slime"
   "Zulu-Inuoe/jzon")
  "A list of projects to recheck with CHECK-ALL function.")


(defparameter *checks-made-for* nil)


(defun check-broken-projects ()
  (ultralisp/db:with-connection ()
    (loop for name in *broken-projects*
          do (make-checks name)))
  (format t "~A projects will be rechecked."
          (length *broken-projects*))
  (values))
