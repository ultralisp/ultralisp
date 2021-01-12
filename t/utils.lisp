(defpackage #:ultralisp-test/utils
  (:use #:cl)
  (:import-from #:ultralisp/db)
  (:import-from #:weblocks-test/utils)
  (:import-from #:cl-dbi)
  (:import-from #:ultralisp/metrics)
  (:import-from #:ultralisp/models/project
                #:project-name
                #:source->project
                #:make-github-project
                #:project-sources)
  (:import-from #:ultralisp/models/dist
                #:dist-name)
  (:import-from #:ultralisp/models/dist-source
                #:dist->sources
                #:source->dists
                #:add-source-to-dist)
  (:import-from #:ultralisp/models/dist
                #:common-dist)
  (:import-from #:ultralisp/models/source
                #:deleted-p)
  (:import-from #:ultralisp/protocols/enabled
                #:enabled-p)
  (:export #:with-login
           #:with-test-db
           #:with-metrics
           #:get-source
           #:make-project
           #:get-dist
           #:get-all-dist-names
           #:get-all-dist-projects
           #:get-projects-linked-to-the
           #:build-dists))
(in-package ultralisp-test/utils)


(defmacro with-test-db (&body body)
  `(ultralisp/db:with-connection ()
     (with-output-to-string (*standard-output*)
       (with-output-to-string (*error-output*)
         (mito:execute-sql "DROP SCHEMA IF EXISTS unittest CASCADE;")
         (mito:execute-sql "CREATE SCHEMA unittest AUTHORIZATION CURRENT_USER;")
         (mito:execute-sql "SET search_path TO unittest;")
         (mito:migrate "./db/")))
     (unwind-protect (progn ,@body)
       ;; We need to return search path to a original state
       ;; to not disrupt accessing real database from the REPL
       (mito:execute-sql "SET search_path TO public;"))))


(defmacro with-login ((&key (email "bob@example.com"))
                      &body body)
  `(weblocks-test/utils:with-session
     (let* ((user (or (weblocks-auth/models:get-user-by-email ,email)
                      (mito:create-dao 'weblocks-auth/models:user
                                       :nickname ,email
                                       :email ,email))))
       (setf (weblocks-auth/models:get-current-user)
             user)
       ,@body)))


(defmacro with-metrics (&body body)
  `(progn (ultralisp/metrics:initialize)
          ,@body))


(defun get-source (project)
  (let ((sources (project-sources project)))
    (when (> (length sources) 1)
      (error "There are more than 1 source for this project"))
    (first sources)))


(defun get-dist (source)
  "Retruns a bound dist for the source."
  (let ((dists (source->dists source)))
    (when (> (length dists) 1)
      (error "There are more than 1 source for this project"))
    (first dists)))


(defun get-all-dist-names (source &key (enabled nil enabled-given-p))
  "Returns a sorted list of dist names for the source."
  (let ((dists (apply #'source->dists
                      source
                      (when enabled-given-p
                        (list :enabled enabled)))))
    (sort (mapcar #'dist-name
                  dists)
          #'string<)))


(defun get-all-dist-projects (dist &key (enabled nil enabled-given-p))
  "Returns sorted list of project names, included into the dist."
  (let* ((sources
           (apply #'dist->sources
                  dist
                  (when enabled-given-p
                    (list :enabled enabled))))
         (projects (mapcar #'source->project sources)))
    (sort (mapcar #'project-name
                  projects)
          #'string<)))


(defun get-projects-linked-to-the (dist &key (enabled nil enabled-given-p))
  "Returns a list of project, included into exactly given dist version.

   Returned list has following plists:

       '(:name \"foo/bar\" :enabled t :deleted nil)

   This function also is able to return dists which was deleted, whereas
   GET-ALL-DIST-PROJECTS don't.
"
  (let* ((sources
           (apply #'dist->sources
                  dist
                  :this-version t
                  (when enabled-given-p
                    (list :enabled enabled)))))
    (sort (loop for source in sources
                for project = (source->project source)
                for name = (project-name project)
                collect (list :name name
                              :enabled (enabled-p source)
                              :deleted (deleted-p source)))
          #'string<
          :key (lambda (item)
                 (getf item :name)))))


(defun make-project (user name)
  "Creates a project which is already added to the common dist."
  (let ((project (make-github-project user name)))
    (add-source-to-dist
     (common-dist)
     (get-source project))
    
    project))


(defun build-dists ()
  "Moves all dists from pending to ready state"
  (ultralisp/builder::prepare-pending-dists)
  (ultralisp/builder::build-prepared-dists))
