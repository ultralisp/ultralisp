(defpackage #:ultralisp/models/index
  (:use #:cl)
  (:import-from #:ultralisp/models/project
                #:project)
  (:import-from #:alexandria
                #:make-keyword)
  (:import-from #:sxql
                #:order-by
                #:left-join
                #:limit
                #:where)
  (:import-from #:local-time
                #:now)
  (:import-from #:ultralisp/utils
                #:time-in-future)
  (:export
   #:get-index-status
   #:set-index-status
   #:get-projects-to-index))
(in-package ultralisp/models/index)


(defun status-to-postgres (symbol)
  (check-type symbol (member :ok :failed))
  (string-downcase symbol))


(defun status-from-postgres (text)
  (when text
    (values
     (make-keyword (string-upcase text)))))


(defun get-index-status (project)
  (check-type project project)
  (status-from-postgres
   (second
    (assoc :status
           (mito:retrieve-by-sql "SELECT status FROM project_index WHERE project_id = ?"
                                 :binds (list (mito:object-id project)))))))


(defun set-index-status (project status
                         &key
                           (next-update-at (time-in-future :day 7)))
  (check-type project project)
  (check-type status (member :ok :failed))
  (if (get-index-status project)
      (mito:execute-sql "UPDATE project_index SET status = ?,
                                last_update_at = NOW(),
                                next_update_at = ?
                          WHERE project_id = ?"
                        (list (status-to-postgres status)
                              next-update-at
                              (mito:object-id project)))
      (mito:execute-sql "INSERT INTO project_index (project_id, last_update_at, next_update_at, status)
                                VALUES (?, NOW(), ?, ?)"
                        (list (mito:object-id project)
                              next-update-at
                              (status-to-postgres status)))))


(defun get-projects-to-index (&key (limit 10))
  "Returns a list of projects to update a search index."
  (mito:select-dao 'project
    (left-join :project_index :on (:= :project_index.project_id :project.id))
    (where (:and :project.enabled
                 (:or (:< :project_index.next_update_at
                          (now))
                      (:is-null :project_index.id))))
    (order-by :project_index.next_update_at)
    (limit limit)))
