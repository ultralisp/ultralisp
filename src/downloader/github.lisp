(defpackage #:ultralisp/downloader/github
  (:use #:cl)
  (:import-from #:legit)
  (:import-from #:ultralisp/models/project
                #:get-last-seen-commit
                #:project)
  (:import-from #:ultralisp/models/check
                #:get-processed-at
                #:get-description
                #:project-has-changes-p
                #:check)
  (:import-from #:ultralisp/downloader/base
                #:download
                #:make-downloader
                #:perform-project-check)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:mito
                #:save-dao)
  (:import-from #:uiop
                #:ensure-directory-pathname)
  (:import-from #:ultralisp/utils
                #:ensure-absolute-dirname))
(in-package ultralisp/downloader/github)
(in-readtable :interpol-syntax)


(defmethod perform-project-check ((source (eql :github))
                                  (project project)
                                  (check check))
  (let* ((tmp-dir "/tmp/checker")
         (last-seen-commit (get-last-seen-commit project)))
    (multiple-value-bind (project-dir params-update)
        (download project tmp-dir)
      (declare (ignorable project-dir))
      
      (let ((latest-commit (getf params-update
                                 :last-seen-commit)))
        (unless (string-equal last-seen-commit
                              latest-commit)
          ;; Hey, project was changed at the github!
          ;; we should celebrate this fact!
          ;; Or just store it into the database
          (setf (get-last-seen-commit project)
                latest-commit)
          (save-dao project)
          
          (setf (project-has-changes-p check) t)
          
          (if last-seen-commit
              (setf (get-description check)
                    #?"Updated from ${last-seen-commit} to ${latest-commit} commit.")
              (setf (get-description check)
                    #?"Added on ${latest-commit} commit.")))
        
        (setf (get-processed-at check) (local-time:now))
        (save-dao check)
        
        ;; Should return a check object
        (values check)))))


(defun git-clone-or-update (url dir)
  (let* ((absolute-dir (ensure-absolute-dirname dir))
         (repo (legit:init absolute-dir
                           :remote url
                           :if-does-not-exist :clone)))
    ;; Here we are suppressing output from the git binary
    (with-output-to-string (*standard-output*)
      (legit:pull repo))
    
    ;; return repository so that other actions could be performed on it
    (values repo)))


(defmethod make-downloader ((source (eql :github)))
  (lambda (base-dir &key user-or-org project &allow-other-keys)
    (let* ((url (format nil "https://github.com/~A/~A.git"
                        user-or-org
                        project))
           (dir (ensure-directory-pathname
                 (merge-pathnames (format nil "~A-~A" user-or-org
                                          project)
                                  (ensure-directory-pathname base-dir))))
           (repo (git-clone-or-update url dir)))
      
      (values dir
              (list :last-seen-commit
                    (legit:current-commit repo))))))
