(uiop:define-package #:ultralisp/api/projects
  (:use #:cl)
  (:import-from #:ultralisp/db)
  (:import-from #:ultralisp/models/tag)
  (:import-from #:ultralisp/api/api
                #:api
                #:*default-page-size*)
  (:import-from #:alexandria
                #:plist-hash-table)
  (:import-from #:serapeum
                #:dict)
  (:import-from #:ultralisp/models/project
                #:project2)
  (:import-from #:mito
                #:object-id)
  (:import-from #:openrpc-server
                #:define-rpc-method))
(in-package #:ultralisp/api/projects)


(define-rpc-method (api get-projects-by-tag) (tag &key page-key (limit *default-page-size*))
  (:summary "Returns project market by a given tag.")
  (:param tag string "Tag name.")
  (:param page-key integer "Next page key.")
  (:param limit integer "Maximum number of projects per page.")
  (:result (paginated-list-of project2))
  
  (log:error "Retrieving projects with" tag page-key)
  (ultralisp/db:with-connection ()
    (ultralisp/models/tag::get-projects-by-tag-paginated tag
                                                         :page-key page-key
                                                         :limit limit)))


(define-rpc-method (api get-project-tags) (project-id)
  (:summary "Retrieve all tags of a single project.")
  (:param project-id integer "ID of a project.")
  (:result (list-of string))
  
  (ultralisp/db:with-connection ()
    (ultralisp/models/tag::get-project-tags project-id)))


(define-rpc-method (api get-project-by-name) (name)
  (:summary "Returns a project details by it's name. Name should be in it's full form like \"40ants/doc\".")
  (:param name string)
  (:result (or null
               ultralisp/models/project:project2))
  (ultralisp/db:with-connection ()
    (ultralisp/models/project:get-project2 name)))
