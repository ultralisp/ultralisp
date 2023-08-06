(uiop:define-package #:ultralisp/downloader/github
  (:use #:cl)
  (:import-from #:legit)
  (:import-from #:ultralisp/models/project
                #:get-last-seen-commit
                #:project)
  (:import-from #:ultralisp/models/check
                #:get-processed-at
                #:get-description)
  (:import-from #:ultralisp/downloader/base
                #:downloaded-project-path
                #:download
                #:make-downloader)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:mito
                #:save-dao)
  (:import-from #:uiop
                #:ensure-directory-pathname)
  (:import-from #:ultralisp/utils
                #:ensure-absolute-dirname)
  (:import-from #:log4cl-extras/context
                #:with-fields)
  (:import-from #:log)
  (:import-from #:ultralisp/downloader/git
                #:git-clone-or-update))
(in-package #:ultralisp/downloader/github)
(in-readtable :interpol-syntax)


(defmethod make-downloader ((source (eql :github)))
  (lambda (dir &key user-or-org project last-seen-commit latest branch &allow-other-keys)
    (with-fields (:user user-or-org
                  :project project
                  :branch branch
                  :last-seen-commit last-seen-commit
                  :latest latest)
      (log:info "Downloading source from github")
      
      (let* ((url (format nil "https://github.com/~A/~A.git"
                          user-or-org
                          project))
             (repo (git-clone-or-update url
                                        dir
                                        :branch branch
                                        :commit (unless latest
                                                  last-seen-commit))))
      
        (let ((current-commit (legit:current-commit repo)))
          (values dir
                  (list :last-seen-commit
                        current-commit)))))))
