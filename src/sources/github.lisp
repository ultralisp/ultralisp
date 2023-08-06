(uiop:define-package #:ultralisp/sources/github
  (:use #:cl)
  (:import-from #:dex)
  (:import-from #:jonathan)
  (:import-from #:ultralisp/sources/base
                #:project-name
                #:create-project
                #:base-source)
  (:import-from #:ultralisp/utils/github
                #:get-topics
                #:extract-github-name)
  (:import-from #:ultralisp/db
                #:with-transaction)
  (:import-from #:ultralisp/models/tag
                #:add-tags)
  (:import-from #:str
                #:containsp
                #:split)
  (:import-from #:mito
                #:create-dao)
  (:import-from #:ultralisp/models/project
                #:add-source
                #:*github-oauth-token*
                #:project2)
  (:import-from #:arrows
                #:->)
  (:import-from #:function-cache
                #:defcached)
  (:import-from #:log4cl-extras/error
                #:with-log-unhandled))
(in-package #:ultralisp/sources/github)


(defclass github-source (base-source)
  ())


(defun guess-github-source (url)
  (let ((project-name (extract-github-name url)))
    (when project-name
      (make-instance 'github-source
                     :url url
                     :project-name project-name))))


(defcached %github-get-description (project-name)
  (check-type project-name string)
  (unless (containsp "/" project-name)
    (error "Project name should contain a \"/\" symbol, but ~S doesn't."
           project-name))
  
  (let ((headers (when *github-oauth-token*
                   (list (cons "Authorization"
                               (format nil "token ~A"
                                       *github-oauth-token*))))))
    (-> (format nil "https://api.github.com/repos/~A"
                project-name)
        (dex:get :headers headers)
        (jonathan:parse)
        (getf :|description|))))


(defmethod create-project ((source github-source))
  (with-transaction
    (let* ((full-project-name (project-name source))
           (user-or-org (first (split "/" full-project-name)))
           (project-name (second (split "/" full-project-name)))
           (description (or (ignore-errors
                             ;; We ignore errors here, because
                             ;; description is not very important
                             ;; and can be updated later,
                             ;; but we definitely want to log these
                             ;; errors, to not miss some system problems.
                             (with-log-unhandled ()
                               (%github-get-description full-project-name)))
                            "No description."))
           (project (create-dao 'project2
                                :name full-project-name
                                :description description)))
      (add-source project
                  :type :github
                  :params (list :user-or-org user-or-org
                                :project project-name))

      (let ((github-tags (get-topics full-project-name)))
        (add-tags project
                  github-tags))
      project)))
