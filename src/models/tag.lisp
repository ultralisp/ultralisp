(uiop:define-package #:ultralisp/models/tag
  (:use #:cl)
  (:import-from #:mito
                #:object-id)
  (:import-from #:ultralisp/models/project
                #:project-name
                #:get-projects-with-sources
                #:project2)
  (:import-from #:ultralisp/db
                #:make-list-placeholders
                #:with-transaction)
  (:import-from #:ultralisp/utils/github
                #:get-topics)
  (:import-from #:sxql
                #:order-by
                #:where
                #:join))
(in-package #:ultralisp/models/tag)


(defclass tag ()
  ((name :col-type (:text)
         :initarg :name
         :accessor tag-name))
  (:unique-keys name)
  (:metaclass mito:dao-table-class))

(defmethod print-object ((obj tag) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A"
            (tag-name obj))))


(defclass project-tag ()
  ;; Here we intentionally ignore project version
  ;; because we don't want to version links between project and tag as well
  ((project-id :col-type :bigint
               :initarg :project-id
               :reader project-id)
   (tag :col-type tag
        :reader tag))
  (:primary-key project-id tag)
  (:metaclass mito:dao-table-class))


(defun get-project-tags (project)
  "Returns a list of string with tag names."
  (check-type project project2)
  (loop for link in (mito:select-dao 'project-tag
                      (mito:includes 'tag)
                      (sxql:where (:= :project-id (object-id project))))
        collect (tag-name (tag link))))


(defun get-or-create (tag-name)
  (with-transaction
    (let ((tag (mito:find-dao 'tag :name tag-name)))
      (if tag
          tag
          (mito:create-dao 'tag :name tag-name)))))


(defun add-tags (project tags)
  "TAGS can contain values which already bound to the project.
   Returns tag names which were added."
  (check-type project project2)
  
  (loop for tag in tags
        do (check-type tag string))

  (with-transaction ()
    (loop with project-id = (object-id project)
          for tag-name in tags
          for tag = (get-or-create tag-name)
          for link = (mito:find-dao 'project-tag
                                    :project-id project-id
                                    :tag tag)
          unless link
            do (mito:create-dao 'project-tag
                                :project-id project-id
                                :tag tag)
            and collect tag-name)))

(defun remove-tags (project &rest tags)
  (check-type project project2)
  (loop for tag in tags
        do (check-type tag string))

  (with-transaction ()
    (mito:execute-sql
     (format nil "
DELETE FROM project_tag
 USING tag
 WHERE project_tag.tag_id = tag.id
   AND project_tag.project_id = ?
   AND tag.name IN ~A
" (make-list-placeholders tags))
     (list*
      (object-id project)
      tags)))
  (values))


(defun fill-tags-for-all-projects ()
  "This is a \"one time\" helper. It should be called after the DB migration, just to fill tags for all projects in the database."
  (loop for project in (get-projects-with-sources)
        for repository-name = (project-name project)
        for tags = (get-topics repository-name :timeout nil)
        do (add-tags project tags)))


(defun get-all-tags-with-counters ()
  "Returns a list of plists like (:name \"foo\" :count 100500)"
  (mito:retrieve-by-sql
   "SELECT name, COUNT(*) as count
      FROM tag
      JOIN project_tag ON tag.id = project_tag.tag_id
     GROUP BY name
     ORDER BY name"))


(defun get-projects-by-tag (tag-name)
  "Returns a list of PROJECT2 instances tagged by given tag name."
  (check-type tag-name string)
  (mito:select-dao 'project2
    (join :project_tag :on (:= :project2.id :project_tag.project_id))
    (join :tag :on (:= :project_tag.tag_id :tag.id))
    (where (:and (:= :tag.name tag-name)
            :project2.latest))
    (order-by :project2.name)))
