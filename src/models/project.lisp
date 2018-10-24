(defpackage #:ultralisp/models/project
  (:use #:cl)
  (:import-from #:mito
                #:create-dao)
  (:import-from #:jonathan)
  (:import-from #:ultralisp/metadata
                #:read-metadata)
  (:import-from #:cl-dbi
                #:with-transaction))
(in-package ultralisp/models/project)


(defclass project ()
  ((source :col-type (:text)
           :initarg :source
           :reader get-source)
   (name :col-type (:text)
         :initarg :name
         :reader get-name)
   (params :col-type (:json)
           :initarg :params
           :reader get-params
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


(defun make-github-project (user-or-org project)
  (let ((name (concatenate 'string
                           user-or-org
                           "/"
                           project)))
    (create-dao 'project
                :source "github"
                :name name
                :params (list :user-or-org user-or-org
                              :project project))))


(defun get-all-projects ()
  (mito:select-dao 'project))


(defun delete-project (project)
  (mito:delete-dao project))


(defun convert-metadata (&optional (filename "projects/projects.txt"))
  "Loads old metadata from file into a database."
  (let ((metadata (read-metadata filename)))
    (with-transaction mito:*connection*
      (loop for item in metadata
            for urn = (ultralisp/metadata:get-urn item)
            for splitted = (cl-strings:split urn "/")
            for user = (first splitted)
            for project = (second splitted)
            do (make-github-project user project)))))
