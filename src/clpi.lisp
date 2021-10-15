 (defpackage #:ultralisp/clpi
  (:use #:cl)
  (:import-from #:ultralisp/models/project
                #:get-all-dist-projects)
  (:import-from #:ultralisp/models/source
                #:source-systems-info)
  (:import-from #:jsown)
  (:import-from #:alexandria
                #:assoc-value)
  (:import-from #:ultralisp/models/dist
                #:dist-name)
  (:import-from #:ultralisp/utils
                #:with-tmp-directory)
  (:import-from #:ultralisp/models/versioned
                #:latest-p)
  (:import-from #:ultralisp/protocols/enabled
                #:enabled-p)
  (:import-from #:local-time)
  (:import-from #:legit))
(in-package ultralisp/clpi)


(defun project-versions (dist project)
  ;; Here we get data for 'ready' and 'prepared' dists,
  ;; because when we are building CLPI for a new dist version,
  ;; it will be in 'prepared' state first and will be transferred
  ;; to 'ready' only after Quicklisp and CLPI dists are updated.
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
               and dist.id = ?
               and (dist.state = 'ready' or dist.state = 'prepared')
               and dist_source.enabled = true
          order by dist.created_at"
           :binds (list (mito:object-id project)
                        (mito:object-id dist)))))


(defun write-project (dist project stream)
  (write (list* (ultralisp/models/project:project-name project)
                (project-versions dist project))
         :stream stream)
  (terpri stream)
  (values))


(defun get-project-systems-info (dist project)
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
                              and dist.id = ?
                              and (dist.state = 'ready' or dist.state = 'prepared')
                              and dist_source.enabled = true
                            order by dist.created_at
                     )
                     select info->>'NAME' as system,
                            jsonb_agg(jsonb_build_array(jsonb_build_array(project, version))) as project_versions
                       from rows
                      group by system"
                     :binds (list (mito:object-id project)
                                  (mito:object-id dist)))
        for row in rows
        for system = (getf row :system)
        for project-data = (getf row :project-versions)
        for project-versions = (jsown:parse project-data)
        collect (list* system
                       project-versions)))


(defun write-project-systems (dist project stream)
  (loop for system in (get-project-systems-info dist project)
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
                       ;; It is important to sort dependencies because
                       ;; otherwise they will appear in diff and we'll
                       ;; have to upload system info even if this system
                       ;; wasn't released recently:
                       :dependencies (sort (copy-list dependencies)
                                           #'string<))
                 (gethash filename
                          result))
           ;; For stable output we have to sort content by filenames
           ;; and system names inside each file. Otherwise git diff
           ;; will be bigger than it have to.
        finally (return (sort (loop for filename being the hash-key of result
                                      using (hash-value systems)
                                    collect (cons filename
                                                  (sort systems
                                                        #'string<
                                                        :key #'car)))
                              #'string<
                              :key #'car))))


(defun get-project-releases (dist project)
  ;; TODO: In future we need to extract description of each ASDF system
  ;; and store them in the database. But for now we'll put projects'
  ;; description to the index.
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
          and dist.id = ?
          and (dist.state = 'ready' or dist.state = 'prepared')
          and dist_source.enabled = true),

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
       group by version, url, size, md5
       order by version"
                     :binds (list (mito:object-id project)
                                  (mito:object-id dist)))
        for row in rows
        collect (list (getf row :version)
                      :url (getf row :url)
                      :size (getf row :size)
                      :md5 (getf row :md5)
                      ;; TODO: also we need to extract license information
                      ;; :license ????
                      ;; TODO: extract and store system version if given in ASD
                      ;; :version ??? ;; Dotted string
                      :systems (group-systems-by-file (getf row :systems)))))


(defun write-project-releases (dist project stream)
  (loop for release in (get-project-releases dist project)
        do (write release :stream stream)
           (terpri stream)))


(defun write-systems-info (dist systems-dir project)
  "Writes systems/<system-name>/primary-project files."
  (loop for source in (ultralisp/models/project:project-sources project)
        for source-dist = (ultralisp/models/dist-source:get-link dist source)
        for source-enabled = (when source-dist
                               (enabled-p source-dist))
        ;; Were are only interested in the sources bound to the current dist
        ;; and enabled there.
        for systems = (when source-enabled
                        (ultralisp/models/source:source-systems-info source))
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


(defun write-repo-info (dist project repo-path)
  (let ((source (first
                 (remove-if-not
                  (lambda (source)
                    (member (mito:object-id dist)
                            (mapcar #'mito:object-id
                                    (ultralisp/models/dist-source:source->dists source))))
                  (ultralisp/models/project:project-sources project)))))
    (when source
      (let ((type (ultralisp/models/source:source-type source)))
        (when (eql type :github)
          (let* ((params (ultralisp/models/source:source-params source))
                 (user (getf params :user-or-org))
                 (project (getf params :project)))
            (when (and user project)
              (with-open-file (stream repo-path :direction :output
                                                :if-exists :supersede)
                (write (list :github
                             :path (format nil "~A/~A"
                                           user
                                           project))
                       :stream stream)
                (terpri stream)))))))))


(defun render-index-file (dist stream)
  (let* ((spinneret:*html* stream)
         (dist-name (dist-name dist))
         (url (format nil "https://ultralisp.org/dists/~A" dist-name)))
    (spinneret:with-html
      (:html
       (:body
        (:div :style "width: 80%; margin: 2em auto 0 auto"
              (:h1 ("~A" dist-name))
              (:p ("This is a CLPI index for \"~A\" distribution. You can use it with [Common Lisp Package Manager](https://www.clpm.dev/)."
                   dist-name))
              (:p "To learn more about soft, included into this distribution, go to it's page:")
              (:p (:a :href url
                      url))))))))


(defun write-index (dist projects &key (base-dir #P"clpi/"))
  (ultralisp/db:with-connection ()
    (let* ((index-file (merge-pathnames #P"index.html" base-dir))
           (inner-base-dir (merge-pathnames #P"clpi/v0.4/" base-dir)) ;; this part is required by CLPM
           (clpi-version-file (merge-pathnames #P"clpi-version" inner-base-dir))
           (projects-index-file (merge-pathnames #P"project-index" inner-base-dir))
           (systems-index-file (merge-pathnames #P"system-index" inner-base-dir))
           (projects-dir (merge-pathnames #P"projects/" inner-base-dir))
           (systems-dir (merge-pathnames #P"systems/" inner-base-dir)))
      (ensure-directories-exist projects-index-file)
      (ensure-directories-exist systems-index-file)

      (with-open-file (stream clpi-version-file :direction :output
                                                :if-exists :supersede)
        (write "0.4" :stream stream))
      
      (with-open-file (stream index-file :direction :output
                                         :if-exists :supersede)
        (render-index-file dist stream))

      (with-open-file (projects-stream projects-index-file :direction :output
                                                           :if-exists :supersede)
        (with-open-file (systems-stream systems-index-file :direction :output
                                                           :if-exists :supersede)
          (loop for project in (sort
                                ;; We need to sort projects by name to make project-index file
                                ;; stable and to minimize git diff
                                (copy-list projects)
                                #'string<
                                :key #'ultralisp/models/project:project-name)
                for project-name = (ultralisp/models/project:project-name project)
                for releases-path = (merge-pathnames (format nil "~A/releases" project-name)
                                                     projects-dir)
                for version-scheme-path = (merge-pathnames (format nil "~A/version-scheme" project-name)
                                                           projects-dir)
                for repo-path = (merge-pathnames (format nil "~A/repo" project-name)
                                                 projects-dir)
                do (write-project dist project projects-stream)
                   (write-project-systems dist project systems-stream)
                  
                   (ensure-directories-exist releases-path)
                   (with-open-file (releases-stream releases-path :direction :output
                                                                  :if-exists :supersede)
                     (write-project-releases dist project releases-stream))
                   ;; TODO: probably, when we'll make version extraction out of ASDF,
                   ;;       we'll need to guess version type and write :DATE or :SEMANTIC
                   ;;       depending on actual version numbering of the project.
                   (with-open-file (stream version-scheme-path :direction :output
                                                               :if-exists :supersede)
                     (write :date :stream stream))

                   (write-repo-info dist project repo-path)
                   (write-systems-info dist systems-dir project))))))
  (values))


(defun changed-files (repo)
  (loop with git-output = (with-output-to-string (legit:*git-output*)
                            (legit:with-chdir (repo)
                              (legit:git-status "." :porcelain t)))
        for line in (str:split #\Newline git-output :omit-nulls t)
        collect (subseq line 3)))


(defun ensure-user-in-git-config (repo)
  (let* ((filename (merge-pathnames ".git/config"
                                    (legit:location repo)))
         (content (alexandria:read-file-into-string filename)))
    (unless (str:containsp "[user]" content)
      (with-open-file (s filename
                         :direction :output
                         :if-exists :append)
        (format s "~2&[user]
    name = bot
    email = bot@ultralisp.org~%")))
    (values)))


(defun write-index-for-dist (dist)
  (let* ((base-dir (uiop:ensure-directory-pathname
                    (merge-pathnames (dist-name dist)
                                     #P"build/clpi/")))
         ;; We keep all CLPI for all dists in git repositories.
         ;; They are written to /app/clpi folder which is mounted
         ;; into the docker container to make files survive deployments.
         ;; This way we'll be able to learn what files were changed.
         (repo (legit:init base-dir
                           :if-does-not-exist :create)))

    (log:info "Writing CLPI index for dist with id = ~A and version = ~A"
              (mito:object-id dist)
              (ultralisp/models/versioned:object-version dist))
    
    (ensure-user-in-git-config repo)

    (when (changed-files repo)
      (log:debug "Commiting all files before update")
      (legit:add repo ".")
      (legit:commit repo (format nil "Update from ~A"
                                 (local-time:now))))
    
    (log:debug "Writing CLPI to the disk")
    (write-index dist
                 (get-all-dist-projects dist)
                 :base-dir base-dir)

    (let ((files (changed-files repo)))
      (cond
        (files
         (log:debug "Uploading CLPI to the hosting" files)
         (ultralisp/uploader/base:upload base-dir
                                         :clpi
                                         (format nil "/~A/" (dist-name dist))
                                         :only-files files)
         (log:debug "Done"))
        (t
         (log:warn "There is no new files in CLPI index for \"~A\" dist"
                   (dist-name dist)))))))
