(defpackage #:ultralisp/downloader/github
  (:use #:cl)
  (:import-from #:ultralisp/models/project
                #:get-last-seen-commit
                #:project)
  (:import-from #:ultralisp/models/check
                #:get-processed-at
                #:get-description
                #:project-has-changes-p
                #:check)
  (:import-from #:ultralisp/downloader
                #:download)
  (:import-from #:ultralisp/downloader/base
                #:perform-project-check)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:mito
                #:save-dao))
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
          
          (setf (project-has-changes-p check) t
                (get-processed-at check) (local-time:now))
          
          (if last-seen-commit
              (setf (get-description check)
                    #?"Updated from ${last-seen-commit} to ${latest-commit} commit.")
              (setf (get-description check)
                    #?"Added on ${latest-commit} commit."))

          (save-dao check))
        ;; Should return a check object
        (values check)))))
