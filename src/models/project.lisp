(defpackage #:ultralisp/models/project
  (:use #:cl)
  (:import-from #:mito
                #:delete-dao
                #:select-dao
                #:create-dao)
  (:import-from #:jonathan
                #:to-json)
  (:import-from #:dexador)
  (:import-from #:ultralisp/metadata
                #:read-metadata)
  (:import-from #:cl-dbi
                #:with-transaction)
  (:import-from #:function-cache
                #:defcached)
  (:import-from #:cl-arrows
                #:->)
  (:import-from #:cl-strings
                #:split)
  (:import-from #:alexandria
                #:make-keyword)
  (:import-from #:sxql
                #:limit
                #:where)
  (:export
   #:get-all-projects
   #:get-description
   #:get-url
   #:get-name
   #:project
   #:get-source
   #:get-params
   #:get-github-project))
(in-package ultralisp/models/project)


(defclass project ()
  ((source :col-type (:text)
           :initarg :source
           :reader get-source
           :inflate (lambda (text)
                      (make-keyword (string-upcase text)))
           :deflate #'symbol-name)
   (name :col-type (:text)
         :initarg :name
         :accessor get-name)
   (description :col-type :text
                :initarg :description
                :accessor get-description)
   (params :col-type (:jsonb)
           :initarg :params
           :accessor get-params
           :deflate #'jonathan:to-json
           :inflate (lambda (text)
                      (jonathan:parse
                       ;; Jonathan for some reason is unable to work with
                       ;; `base-string' type, returned by database
                       (coerce text 'simple-base-string)))))
  (:unique-keys name)
  (:metaclass mito:dao-table-class))


(defmethod print-object ((project project) stream)
  (print-unreadable-object (project stream :type t)
    (format stream
            "~A name=~A"
            (get-source project)
            (get-name project))))


(defun get-url (project)
  (let* ((params (get-params project))
         (user-name (getf params :user-or-org))
         (project-name (getf params :project)))
    (format nil "https://github.com/~A/~A"
            user-name
            project-name)))


(defcached %github-get-description (user-or-org project)
  (-> (format nil "https://api.github.com/repos/~A/~A"
              user-or-org
              project)
      (dex:get)
      (jonathan:parse)
      (getf :|description|)))


(defun make-github-project (user-or-org project)
  (let ((name (concatenate 'string
                           user-or-org
                           "/"
                           project))
        (description (or (ignore-errors (%github-get-description user-or-org
                                                                 project))
                         "")))
    (create-dao 'project
                :source "github"
                :name name
                :description description
                :params (list :user-or-org user-or-org
                              :project project))))


(defun get-all-projects ()
  (select-dao 'project))


(defun get-github-project (user-or-org project)
  (first
   (select-dao 'project
     (where (:and
             (:= :source
                 "GITHUB")
             (:a> :params
                  (to-json
                   (list :user-or-org user-or-org
                         :project project)))))
     (limit 1))))


(defun delete-project (project)
  (delete-dao project))


(defun convert-metadata (&optional (filename "projects/projects.txt"))
  "Loads old metadata from file into a database."
  (let ((metadata (read-metadata filename)))
    (with-transaction mito:*connection*
      (loop for item in metadata
            for urn = (ultralisp/metadata:get-urn item)
            for splitted = (split urn "/")
            for user = (first splitted)
            for project = (second splitted)
            do (make-github-project user project)))))
