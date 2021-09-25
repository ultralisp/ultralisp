(defpackage #:ultralisp/clpi
  (:use #:cl)
  (:import-from #:ultralisp/models/project)
  (:import-from #:ultralisp/models/source
                #:source-systems-info))
(in-package ultralisp/clpi)


(defun project-versions (project)
  (loop for source in (ultralisp/models/project:project-sources project)
        for dist-sources = (ultralisp/models/dist-source:source->dists source)
        ;; TODO: filter dist for which CLPI is built
        for dist = (first dist-sources)
        for version = (ultralisp/models/dist:dist-quicklisp-version dist)
        collect version))


(defun write-project (project stream)
  (write (list* (ultralisp/models/project:project-name project)
                (project-versions project))
         :stream stream)
  (terpri stream)
  (values))


(defun write-project-systems (project stream)
  (loop for source in (ultralisp/models/project:project-sources project)
        for dist-sources = (ultralisp/models/dist-source:source->dists source)
        ;; TODO: filter dist for which CLPI is built
        for dist = (first dist-sources)
        for version = (ultralisp/models/dist:dist-quicklisp-version dist)
        for systems = (ultralisp/models/source:source-systems-info
                       source)
        for project-name = (ultralisp/models/project:project-name project)
        do (loop for system in systems
                 for system-name = (quickdist:get-name system)
                 do (write (list system-name
                                 (loop for version in (project-versions project)
                                       collect (list project-name version)))
                           :stream stream)
                    (terpri stream))
        collect version)
  (values))


(defun write-project-releases (project stream)
  (loop for source in (ultralisp/models/project:project-sources project)
        for dist-sources = (ultralisp/models/dist-source:source->dists source)
        ;; TODO: filter dist for which CLPI is built
        for dist = (first dist-sources)
        for version = (ultralisp/models/dist:dist-quicklisp-version dist)
        for release = (ultralisp/models/source:source-release-info source)
        for project-name = (ultralisp/models/project:project-name project)
        ;; TODO: In future we need to extract description of each ASDF system
        ;; and store them in the database. But for now we'll put projects'
        ;; description to the index.
        for project-description = (ultralisp/models/project:project-description project)
        for systems = (loop with files = (make-hash-table :test 'equal)
                            for system in (source-systems-info source)
                            for file = (quickdist:get-filename system)
                            for name = (quickdist:get-name system)
                            for deps = (quickdist:get-dependencies system)
                            do (push (list name
                                           :description project-description
                                           ;; TODO: also we need to extract license information
                                           ;; :license ????
                                           ;; TODO: extract and store system version if given in ASD
                                           ;; :version ??? ;; Dotted string
                                           :dependencies deps)
                                     (gethash file files))
                            finally (return (alexandria:hash-table-alist files)))
        do (write (list version
                        :url (quickdist:get-project-url release)
                        :size (quickdist:get-file-size release)
                        :md5 (quickdist:get-md5sum release)
                        :systems systems)
                  :stream stream)
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

                 do (ensure-directories-exist primary-project-path)
                    (with-open-file (stream primary-project-path :direction :output
                                                                 :if-exists :supersede)
                      (write project-name
                             :stream stream)
                      (terpri stream))))
  (values))


(defun write-index (projects)
  (ultralisp/db:with-connection ()
    (let* ((base-dir #P"clpi/clpi/v0.4/")
           (clpi-version-file (merge-pathnames #P"clpi-version" base-dir))
           (project-index-file (merge-pathnames #P"project-index" base-dir))
           (system-index-file (merge-pathnames #P"system-index" base-dir))
           (projects-dir (merge-pathnames #P"projects/" base-dir))
           (systems-dir (merge-pathnames #P"systems/" base-dir)))
      (ensure-directories-exist project-index-file)
      (ensure-directories-exist system-index-file)

      (with-open-file (stream clpi-version-file :direction :output
                                                :if-exists :supersede)
        (write "0.4" :stream stream))

      (with-open-file (projects-stream project-index-file :direction :output
                                                          :if-exists :supersede)
        (with-open-file (systems-stream system-index-file :direction :output
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
                   (write-systems-info systems-dir project)))))))
