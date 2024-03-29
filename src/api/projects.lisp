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
                #:fmt
                #:dict)
  (:import-from #:ultralisp/models/project
                #:get-project2-by-id
                #:project2)
  (:import-from #:mito
                #:object-id)
  (:import-from #:openrpc-server
                #:define-rpc-method)
  (:import-from #:ultralisp/models/system-info
                #:system-info))
(in-package #:ultralisp/api/projects)


(define-rpc-method (api get-projects-by-tag) (tag &key page-key (limit *default-page-size*))
  (:summary "Returns project market by a given tag.")
  (:param tag string "Tag name.")
  (:param page-key integer "Next page key.")
  (:param limit integer "Maximum number of projects per page.")
  (:result (paginated-list-of project2))
  
  (log:info "Retrieving projects with" tag page-key)
  (ultralisp/db:with-connection ()
    (ultralisp/models/tag::get-projects-by-tag-paginated tag
                                                         :page-key page-key
                                                         :limit limit)))


(define-rpc-method (api get-project-tags) (project-id)
  (:summary "Retrieve all tags of a single project.")
  (:param project-id integer "ID of a project.")
  (:result (list-of string))
  
  (ultralisp/db:with-connection ()
    (let ((project (ultralisp/models/project:get-project2-by-id project-id)))
      (unless project
        (openrpc-server:return-error (fmt "Project with id ~A not found."
                                          project-id)
                                     :code -1))
      (ultralisp/models/tag::get-project-tags project))))


(define-rpc-method (api get-project-by-name) (name)
  (:summary "Returns a project details by it's name. Name should be in it's full form like \"40ants/doc\".")
  (:param name string)
  (:result ultralisp/models/project:project2)
  (ultralisp/db:with-connection ()
    (let ((project (ultralisp/models/project:get-project2 name)))
      (unless project
        (openrpc-server:return-error (fmt "Project with name ~S not found."
                                          name)
                                     :code -2))
      (values project))))


(define-rpc-method (api get-project-systems) (project-id)
  (:summary "Retrieve all systems of a given project.")
  (:description "Systems are sorted alphabetically.

                 In case if project defines two or more sources, systems can be duplicated in the list.
                 Use get-project-sources in this case, to get separated list of systems for each source.")
  (:param project-id integer "ID of a project.")
  (:result (list-of system-info))
  
  (ultralisp/db:with-connection ()
    (let ((project (get-project2-by-id project-id)))
      (unless project
        (openrpc-server:return-error (fmt "Project with id ~A not found."
                                          project-id)
                                     :code -1))
      (sort (ultralisp/models/project::get-project-systems project)
            #'string<
            :key #'quickdist:get-name))))


(define-rpc-method (api get-project-sources) (project-id)
  (:summary "Retrieve all sources known for given project.")
  (:description "URL of the project can be retrieved from it's source params.

                 If source type is GITHUB, then it's params contain \"user-or-org\" and \"project\" keys.
                 URL can be contructed from these values.

                 For source type GIT, params hashmap will contain \"url\" key.

                 Systems in systems-info key of a source, are sorted by name.")
  (:param project-id integer "ID of a project.")
  (:result (list-of ultralisp/models/source::source))
  
  (ultralisp/db:with-connection ()
    (let ((project (get-project2-by-id project-id)))
      (unless project
        (openrpc-server:return-error (fmt "Project with id ~A not found."
                                          project-id)
                                     :code -1))

      (let ((sources (ultralisp/models/project::project-sources project)))
        (values sources)))))
