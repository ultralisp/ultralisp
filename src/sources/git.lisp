(uiop:define-package #:ultralisp/sources/git
  (:use #:cl)
  (:import-from #:quri
                #:parse-uri)
  (:import-from #:alexandria
                #:last-elt)
  (:import-from #:serapeum
                #:~>>
                #:fmt)
  (:import-from #:ultralisp/sources/base
                #:base-source
                #:project-name
                #:source-url
                #:create-project)
  (:import-from #:ultralisp/db
                #:with-transaction)
  (:import-from #:mito
                #:create-dao)
  (:import-from #:ultralisp/models/project
                #:add-source
                #:project2)
  (:import-from #:ultralisp/utils/git
                #:probe-git-url)
  (:import-from #:str
                #:replace-all))
(in-package #:ultralisp/sources/git)


(defclass git-source (base-source)
  ())


(defun extract-project-name (url)
  (let* ((path (nth-value 4 (parse-uri url)))
         (parsed-path (uiop:parse-unix-namestring path))
         (project-name (pathname-name parsed-path))
         (dir (pathname-directory parsed-path))
         (username (when (> (length dir) 1)
                     (last-elt dir)))
         (full-name (if username
                        (fmt "~A/~A" username project-name)
                        project-name)))
    (error "Foo")
    (~>> full-name 
         (replace-all "@" "")
         (replace-all "~" ""))))


(defun guess-git-source (url)
  (when (probe-git-url url)
    (make-instance 'git-source
                   :url url
                   :project-name (extract-project-name url))))


(defmethod create-project ((source git-source))
  (with-transaction
    (let* ((full-project-name (project-name source))
           (description "No description.")
           (project (create-dao 'project2
                                :name full-project-name
                                :description description)))
      (add-source project
                  :type :git
                  :params (list :url (source-url source)))

      project)))
