(defpackage #:ultralisp/downloader/version
  (:use #:cl)
  (:import-from #:ultralisp/models/version
                #:version)
  (:import-from #:ultralisp/downloader/base
                #:download)
  (:import-from #:ultralisp/models/check
                #:get-project
                #:get-checks))
(in-package ultralisp/downloader/version)


(defmethod download ((version version) dir &key latest)
  (loop for check in (get-checks version)
        for project = (get-project check)
        do (download project dir :latest latest)))
