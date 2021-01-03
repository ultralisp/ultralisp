(defpackage #:ultralisp/downloader/github
  (:use #:cl)
  (:import-from #:legit)
  (:import-from #:ultralisp/models/project
                #:update-and-enable-project
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
                #:with-fields))
(in-package ultralisp/downloader/github)
(in-readtable :interpol-syntax)


(defun git-clone-or-update (url dir &key commit branch)
  (let* ((absolute-dir (ensure-absolute-dirname dir))
         (repo (legit:init absolute-dir
                           :remote url
                           :if-does-not-exist :clone)))
    ;; Here we are suppressing output from the git binary
    (with-output-to-string (*standard-output*)
      (let ((current-commit (legit:current-commit repo)))
        (when (or (not commit)
                  (not (string-equal commit
                                     current-commit)))
          (legit:pull repo)

          (cond
            ;; If commit given, it should have a preference
            ;; over everything else, because in this case
            ;; we want to checkout exact library version:
            (commit
             (legit:checkout repo commit))

            (branch
              (legit:checkout repo branch))))))
    
    ;; return repository so that other actions could be performed on it
    (values repo)))


(defmethod make-downloader ((source (eql :github)))
  (lambda (base-dir &key user-or-org project last-seen-commit latest branch &allow-other-keys)
    (with-fields (:user user-or-org
                  :project project
                  :branch branch
                  :last-seen-commit last-seen-commit
                  :latest latest)
      (log:info "Downloading source from github")
      
      (let* ((url (format nil "https://github.com/~A/~A.git"
                          user-or-org
                          project))
             (dir (ensure-directory-pathname
                   (merge-pathnames (format nil "~A-~A" user-or-org
                                            project)
                                    (ensure-directory-pathname base-dir))))
             (repo (git-clone-or-update url
                                        dir
                                        :branch branch
                                        :commit (unless latest
                                                  last-seen-commit))))
      
        (let ((current-commit (legit:current-commit repo)))
          (values dir
                  (list :last-seen-commit
                        current-commit)))))))
