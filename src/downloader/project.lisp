(defpackage #:ultralisp/downloader/project
  (:use #:cl)
  (:import-from #:log4cl)
  (:import-from #:ultralisp/models/project
                #:project-version
                #:get-all-projects
                #:get-source
                #:get-params
                #:project)
  (:import-from #:ultralisp/downloader/base
                #:make-downloaded-project
                #:make-downloader
                #:download))
(in-package #:ultralisp/downloader/project)


;; TODO: remove
(defmethod download ((projects (eql :all)) projects-dir &key latest)
  "Downloads sources defined in the database into the `projects-dir'."
  (declare (ignorable projects))
  (ensure-directories-exist projects-dir)
  
  (loop for project in (get-all-projects :only-enabled t)
        collect (download project projects-dir :latest latest)))


(defun %download (project dir &key latest)
  (log:info "Downloading" project)
  (let ((downloader (make-downloader (get-source project))))
    (multiple-value-bind (path params)
        (apply downloader
               dir
               :latest latest
               (get-params project))
      
      (make-downloaded-project path
                               project
                               params))))


(defmethod download ((project project) dir &key latest)
  (%download project dir :latest latest))


(defmethod download ((project project-version) dir &key latest)
  (%download project dir :latest latest))

