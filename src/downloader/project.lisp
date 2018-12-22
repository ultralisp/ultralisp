(defpackage #:ultralisp/downloader/project
  (:use #:cl)
  (:import-from #:log4cl)
  (:import-from #:ultralisp/models/project
                #:get-all-projects
                #:get-source
                #:get-params
                #:project)
  (:import-from #:ultralisp/downloader/base
                #:make-downloaded-project
                #:make-downloader
                #:download))
(in-package ultralisp/downloader/project)


(defmethod download ((projects (eql :all)) projects-dir)
  "Downloads sources defined in the database into the `projects-dir'."
  (declare (ignorable projects))
  (ensure-directories-exist projects-dir)
  
  (loop for project in (get-all-projects :only-enabled t)
        collect (download project projects-dir)))


(defmethod download ((project project) dir)
  (log:info "Downloading" project)
  (let* ((downloader (make-downloader (get-source project)))
         (path (apply downloader
                      dir
                      (get-params project))))
    (make-downloaded-project path
                             project)))

