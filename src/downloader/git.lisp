(defpackage #:ultralisp/downloader/git
  (:use #:cl)
  (:import-from #:legit)
  (:import-from #:ultralisp/utils
                #:ensure-absolute-dirname
                #:directory-mtime))
(in-package ultralisp/downloader/git)


(defun git-mtime (path)
  (when (probe-file (merge-pathnames ".git/" path))
    (let ((repo (legit:init path)))
      (legit:current-age repo))))


;; Here we patch quickdist to make it work better with git repositories
(defun quickdist::effective-mtime (path)
  (setf path
        (ensure-absolute-dirname path))
  
  (or (git-mtime path)
      (directory-mtime path)))
