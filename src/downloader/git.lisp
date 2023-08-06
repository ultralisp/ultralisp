(uiop:define-package #:ultralisp/downloader/git
  (:use #:cl)
  (:import-from #:legit)
  (:import-from #:log)
  (:import-from #:trivial-timeout
                #:with-timeout)
  (:import-from #:ultralisp/utils
                #:ensure-absolute-dirname
                #:directory-mtime)
  (:import-from #:ultralisp/downloader/base
                #:make-downloader)
  (:import-from #:ultralisp/sources/git
                #:extract-project-name)
  (:import-from #:log4cl-extras/context
                #:with-fields))
(in-package #:ultralisp/downloader/git)


(defun git-clone-or-update (url dir &key commit branch)
  ;; Sometimes legit:clone hangs for unknown reason :(
  (with-timeout (360)
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
      (values repo))))


;; (defun git-mtime (path)
;;   (when (probe-file (merge-pathnames ".git/" path))
;;     (let ((repo (legit:init path)))
;;       (legit:current-age repo))))


;; Here we patch quickdist to make it work better with git repositories
;; (defun quickdist::effective-mtime (path)
;;   (setf path
;;         (ensure-absolute-dirname path))
  
;;   (or (git-mtime path)
;;       (directory-mtime path)))


(defmethod make-downloader ((source (eql :git)))
  (lambda (dir &key url last-seen-commit latest branch &allow-other-keys)
    (let ((project-name (extract-project-name url)))
      (with-fields (:project project-name
                    :url url
                    :branch branch
                    :last-seen-commit last-seen-commit
                    :latest latest)
        (log:info "Downloading source from git")
       
        (let* ((repo (git-clone-or-update url
                                          dir
                                          :branch branch
                                          :commit (unless latest
                                                    last-seen-commit))))
         
          (let ((current-commit (legit:current-commit repo)))
            (values dir
                    (list :last-seen-commit
                          current-commit))))))))
