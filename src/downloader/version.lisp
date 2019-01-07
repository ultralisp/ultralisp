(defpackage #:ultralisp/downloader/version
  (:use #:cl)
  (:import-from #:ultralisp/models/version
                #:version)
  (:import-from #:ultralisp/downloader/base
                #:download)
  (:import-from #:ultralisp/models/project
                #:get-projects))
(in-package ultralisp/downloader/version)


(defmethod download ((version version) dir &key latest)
  (loop for project in (get-projects version)
        collect (download project dir :latest latest)))
