(uiop:define-package #:ultralisp/dev
  (:use #:cl)
  (:import-from #:ultralisp/models/project
                #:project-name)
  (:import-from #:ultralisp/models/check)
  (:import-from #:ultralisp/rpc/core)
  (:import-from #:ultralisp/pipeline/checking)
  (:import-from #:ultralisp/utils
                #:program-exists-p)
  (:import-from #:yason)
  (:import-from #:serapeum
                #:@)
  (:export #:run-check
           #:check-broken-projects
           #:start-outside-docker))
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
  (ultralisp/db:with-connection ()
    (let ((checks (make-checks project-name)))
      (ultralisp/rpc/core:submit-task 'ultralisp/pipeline/checking::perform2
                                      :args (list (first checks)
                                                  :force t)))))

(defparameter *broken-projects*
  (list
   "fukamachi/cl-dbi")
  "A list of projects to recheck with CHECK-ALL function.")


(defparameter *checks-made-for* nil)


(defun check-broken-projects ()
  (ultralisp/db:with-connection ()
    (loop for name in *broken-projects*
          do (make-checks name)))
  (format t "~A projects will be rechecked."
          (length *broken-projects*))
  (values))


(defun get-all-disabled-projects (last-n-days)
  (ultralisp/db:with-connection ()
    (mito:select-by-sql
     'ultralisp/models/project:project2
     "select p.*
         from project2 as p
         join source as s on s.project_id = p.id and s.project_version = p.version
         join dist_source as ds on ds.source_id = s.id and ds.source_version = s.version
        where p.latest and not p.deleted and s.latest and not s.deleted and not ds.enabled
              and ds.created_at > now() - make_interval(days => ?)
       "
     :binds (list last-n-days))))


(defun check-all-disabled-projects (last-n-days)
  (let* ((projects (get-all-disabled-projects last-n-days))
         (*broken-projects* (mapcar #'project-name projects)))
    (check-broken-projects)))


(defun get-nix-library-path (library-name)
  (when (program-exists-p "nix")
    (let* ((json (uiop:run-program '("nix" "profile" "list" "--json")
                                   :output :string))
           (data (yason:parse json))
           (store-paths (@ data "elements" library-name "storePaths"))
           (path (first store-paths)))
      (when path
        (probe-file (merge-pathnames (make-pathname :directory '(:relative "lib"))
                                     (uiop:ensure-directory-pathname path)))))))


(defun extend-cffi-load-path ()
  (loop for library-name in '("libev")
        for path = (get-nix-library-path library-name)
        when (and (program-exists-p "nix")
                  (null path))
          do (error "When nix is available ~S should be installed using nix profile" library-name)
        when path
          do (pushnew path
                      cffi:*foreign-library-directories* :test 'equal))
  (values))


(defun start-outside-docker ()
  (extend-cffi-load-path)
  (ql:quickload :ultralisp/server)
  (uiop:symbol-call :ultralisp/server :start-outside-docker))
