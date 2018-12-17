(defpackage #:ultralisp/downloader/project
  (:use #:cl)
  (:import-from #:log4cl)
  (:import-from #:ultralisp/models/project
                #:get-source
                #:get-params
                #:project)
  (:import-from #:ultralisp/downloader/base
                #:make-downloader
                #:download))
(in-package ultralisp/downloader/project)


(defmethod download ((projects (eql :all)) projects-dir)
  "Downloads sources defined in the database into the `projects-dir'."
  (declare (ignorable projects))
  (ensure-directories-exist projects-dir)
  
  (loop for project in (get-all-projects :only-enabled t)
        do (download project projects-dir)))


(defmethod download ((project project) dir)
  (log:info "Downloading" project)
  (apply (make-downloader (get-source project))
         dir
         (get-params project)))

