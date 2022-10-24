(uiop:define-package #:ultralisp/api/projects
  (:use #:cl)
  (:import-from #:ultralisp/db)
  (:import-from #:ultralisp/models/tag)
  (:import-from #:ultralisp/api/server
                #:define-rpc-method
                #:*default-page-size*)
  (:import-from #:alexandria
                #:plist-hash-table)
  (:import-from #:serapeum
                #:dict)
  (:import-from #:ultralisp/models/project
                #:project2)
  (:import-from #:mito
                #:object-id))
(in-package #:ultralisp/api/projects)


(defun project-to-dict (project)
  (check-type project project2)
  
  (dict "id" (object-id project)
        "name" (ultralisp/models/project::project-name project)
        "description" (ultralisp/models/project::project-description project)))


(define-rpc-method get-projects-by-tag (tag &key next-page-key (limit *default-page-size*))
  (log:error "Retrieving projects with" tag next-page-key)
  (ultralisp/db:with-connection ()
    (multiple-value-bind (items next-page-key)
        (ultralisp/models/tag::get-projects-by-tag-paginated tag
                                                             :next-page-key next-page-key
                                                             :limit limit)
      (dict
       "items" (mapcar #'project-to-dict
                       items)
       "next-page-key" next-page-key))))


(define-rpc-method get-project-tags (project-id &key next-page-key (limit *default-page-size*))
  ;; These arguments are not used yet, but here for API uniformity:
  (declare (ignore next-page-key limit))
  
  (ultralisp/db:with-connection ()
    (let ((tags (ultralisp/models/tag::get-project-tags project-id)))
      (dict
       "items" tags
       "next-page-key" nil))))

