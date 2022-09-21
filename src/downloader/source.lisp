(defpackage #:ultralisp/downloader/source
  (:use #:cl)
  (:import-from #:log)
  ;; (:import-from #:ultralisp/models/project
  ;;               #:project-version
  ;;               #:get-all-projects
  ;;               #:get-source
  ;;               #:get-params
  ;;               #:project)
  (:import-from #:ultralisp/downloader/base
                #:make-downloaded-project
                #:make-downloader
                #:download)
  (:import-from #:ultralisp/models/source
                #:source-params
                #:source-type
                #:source))
(in-package #:ultralisp/downloader/source)


(defmethod download ((source source) dir &key latest)
  (check-type source source)
  
  (log:info "Downloading" source)
  
  (let ((dir (uiop:ensure-directory-pathname dir))
        (downloader (make-downloader (source-type source))))
    (multiple-value-bind (path params)
        (apply downloader
               dir
               :latest latest
               (source-params source))
      
      (make-downloaded-project path
                               ;; NOTE: this not correct,
                               ;; because previously we had
                               ;; project here:
                               source
                               params))))


