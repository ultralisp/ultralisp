(defpackage #:ultralisp/clpi
  (:use #:cl)
  (:import-from #:ultralisp/models/project)
  (:import-from #:ultralisp/models/source
                #:source-systems-info)
  (:import-from #:jsown)
  (:import-from #:alexandria
                #:assoc-value))
(in-package ultralisp/clpi)


(defun project-versions (project)
  ;; TODO: filter dist for which CLPI is built
  (mapcar #'second
          (mito:retrieve-by-sql 
           "select dist.quicklisp_version
              from project2
              join source on source.project_id = project2.id
              join dist_source on source.id = dist_source.source_id
               and source.version = dist_source.source_version
              join dist on dist.id = dist_source.dist_id
               and dist.version = dist_source.dist_version
             where project2.id = ?
               and dist.state = 'ready'
               and dist_source.enabled = true
          order by dist.created_at"
           :binds (list (mito:object-id project)))))


(defun write-project (project stream)
  (write (list* (ultralisp/models/project:project-name project)
                (project-versions project))
         :stream stream)
  (terpri stream)
  (values))


(defun get-project-systems-info (project)
  (loop with rows = (mito:retrieve-by-sql
                     "with rows as (
                           select project2.name as project,
                                  dist.quicklisp_version as version,
                                  jsonb_array_elements(source.systems_info) as info
                             from project2
                             join source
                               on source.project_id = project2.id
                             join dist_source
                               on source.id = dist_source.source_id
                              and source.version = dist_source.source_version
                             join dist
                               on dist.id = dist_source.dist_id
                              and dist.version = dist_source.dist_version
                            where project2.id = ?
                              and dist.state = 'ready'
                              and dist_source.enabled = true
                            order by dist.created_at
                     )
                     select info->>'NAME' as system,
                            jsonb_agg(jsonb_build_array(jsonb_build_array(project, version))) as project_versions
                       from rows
                      group by system"
                     :binds (list (mito:object-id project)))
        for row in rows
        for system = (getf row :system)
        for project-data = (getf row :project-versions)
        for project-versions = (jsown:parse project-data)
        collect (list* system
                       project-versions)))


(defun write-project-systems (project stream)
  (loop for system in (get-project-systems-info project)
        do (write system :stream stream)
           (terpri stream))
  (values))


(defun group-systems-by-file (systems-field)
  (loop with system-objs = (jsown:parse systems-field)
        with result = (make-hash-table :test 'equal)
        for system in system-objs
        for data = (cdr system)
        for filename = (assoc-value data "filename" :test 'string=)
        for name = (assoc-value data "name"  :test 'string=)
        for description = (assoc-value data "description" :test 'string=)
        for dependencies = (assoc-value data "dependencies" :test 'string=)
        do (push (list name
                       :description description
                       :dependencies dependencies)
                 (gethash filename
                          result))
        finally (return (loop for filename being the hash-key of result
                                using (hash-value systems)
                              collect (cons filename systems)))))


(defun get-project-releases (project)
  (loop with rows = (mito:retrieve-by-sql
                     "with rows as (
       select dist.quicklisp_version as version,
              jsonb_array_elements(source.systems_info) as info,
              source.release_info,
              project2.description as project_description
         from project2
         join source
           on source.project_id = project2.id
         join dist_source
           on source.id = dist_source.source_id
          and source.version = dist_source.source_version
         join dist
           on dist.id = dist_source.dist_id
          and dist.version = dist_source.dist_version
        where project2.id = ?
          and dist.state = 'ready'
          and dist_source.enabled = true
        order by dist.created_at),

    preprocessed as (
       select version,
              release_info->>'PROJECT-URL' as url,
              (release_info->>'FILE-SIZE')::bigint as size,
              release_info->>'MD5SUM' as md5,
              info->'FILENAME' as filename,
              info->'NAME' as system,
              project_description as system_description,
              info->'DEPENDENCIES' as deps
         from rows)

       select version,
              url,
              size,
              md5,
              jsonb_agg(jsonb_build_object('name', system,
                                           'description', system_description,
                                           'dependencies', deps,
                                           'filename', filename)) as systems
        from preprocessed
       group by version, url, size, md5"
                     :binds (list (mito:object-id project)))
        for row in rows
        collect (list (getf row :version)
                      :url (getf row :url)
                      :size (getf row :size)
                      :md5 (getf row :md5)
                      :systems (group-systems-by-file (getf row :systems)))))


(defun write-project-releases (project stream)
  (loop for release in (get-project-releases project)
        do (write release :stream stream)
           (terpri stream)))


(defun write-systems-info (systems-dir project)
  ;; TODO: filter dist for which CLPI is built and latest source
  (loop for source in (ultralisp/models/project:project-sources project)
        for systems = (ultralisp/models/source:source-systems-info
                       source)
        for project-name = (ultralisp/models/project:project-name project)
        do (loop for system in systems
                 for system-name = (quickdist:get-name system)
                 for primary-project-path = (merge-pathnames (format nil "~A/primary-project" system-name)
                                                             systems-dir)
                 when (string-equal system-name
                                    (asdf:primary-system-name system-name))
                   do (ensure-directories-exist primary-project-path)
                      (with-open-file (stream primary-project-path :direction :output
                                                                   :if-exists :supersede)
                        (write project-name
                               :stream stream)
                        (terpri stream))))
  (values))


(defun write-index (projects &key (base-dir #P"clpi/clpi/v0.4/"))
  (ultralisp/db:with-connection ()
    (let* ((clpi-version-file (merge-pathnames #P"clpi-version" base-dir))
           (projects-index-file (merge-pathnames #P"project-index" base-dir))
           (systems-index-file (merge-pathnames #P"system-index" base-dir))
           (projects-dir (merge-pathnames #P"projects/" base-dir))
           (systems-dir (merge-pathnames #P"systems/" base-dir)))
      (ensure-directories-exist projects-index-file)
      (ensure-directories-exist systems-index-file)

      (with-open-file (stream clpi-version-file :direction :output
                                                :if-exists :supersede)
        (write "0.4" :stream stream))

      (with-open-file (projects-stream projects-index-file :direction :output
                                                           :if-exists :supersede)
        (with-open-file (systems-stream systems-index-file :direction :output
                                                           :if-exists :supersede)
          (loop for project in projects
                for project-name = (ultralisp/models/project:project-name project)
                for releases-path = (merge-pathnames (format nil "~A/releases" project-name)
                                                     projects-dir)
                for version-scheme-path = (merge-pathnames (format nil "~A/version-scheme" project-name)
                                                           projects-dir)
                do (write-project project projects-stream)
                   (write-project-systems project systems-stream)
                  
                   (ensure-directories-exist releases-path)
                   (with-open-file (releases-stream releases-path :direction :output
                                                                  :if-exists :supersede)
                     (write-project-releases project releases-stream))
                   ;; TODO: probably, when we'll make version extraction out of ASDF,
                   ;;       we'll need to guess version type and write :DATE or :SEMANTIC
                   ;;       depending on actual version numbering of the project.
                   (with-open-file (stream version-scheme-path :direction :output
                                                               :if-exists :supersede)
                     (write :date :stream stream))
                   (write-systems-info systems-dir project))))))
  (values))


(defun write-index-for-dist (dist)
  (write-index (ultralisp/models/project:get-all-dist-projects dist)))
