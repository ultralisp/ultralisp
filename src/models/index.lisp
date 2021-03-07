(defpackage #:ultralisp/models/index
  (:use #:cl)
  (:import-from #:ultralisp/models/project
                #:project
                #:project2)
  (:import-from #:alexandria
                #:make-keyword)
  (:import-from #:sxql
                #:order-by
                #:left-join
                #:limit
                #:where)
  (:import-from #:local-time
                #:now
                #:format-rfc3339-timestring)
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
  (check-type project project2)
  (status-from-postgres
   (second
    (assoc :status
           (mito:retrieve-by-sql "SELECT status FROM project_index WHERE project_id = ?"
                                 :binds (list (mito:object-id project)))))))


(defun set-index-status (project status
                         &key
                           next-update-at
                           (total-time 0))
  (check-type project project2)
  (check-type status (member :ok :failed))
  (setf next-update-at
        (format-rfc3339-timestring
         nil
         (if next-update-at
             next-update-at
             (ecase status
               (:ok (time-in-future :day 7))
               ;; TODO: We'll need to use exponential
               ;;       here.
               (:failed (time-in-future :day 1))))))
  (if (get-index-status project)
      (mito:execute-sql "UPDATE project_index SET status = ?,
                                last_update_at = NOW(),
                                next_update_at = ?,
                                total_time = ?
                          WHERE project_id = ?"
                        (list (status-to-postgres status)
                              next-update-at
                              total-time
                              (mito:object-id project)))
      (mito:execute-sql "INSERT INTO project_index (
                                   project_id,
                                   last_update_at,
                                   next_update_at,
                                   total_time,
                                   status)
                                VALUES (?, NOW(), ?, ?, ?)"
                        (list (mito:object-id project)
                              next-update-at
                              total-time
                              (status-to-postgres status)))))


(defun get-projects-to-index (&key (limit 10))
  "Returns a list of projects to update a search index."
  (mito.dao:select-by-sql 'project2
                          "
WITH projects_with_sources AS (
  SELECT distinct source.project_id
    FROM source
   WHERE latest = true
     AND deleted = false
)
SELECT *
  FROM project2
  LEFT JOIN project_index ON (project_index.project_id = project2.id)
 WHERE project2.id IN (SELECT * FROM projects_with_sources)
   AND (project_index.next_update_at < NOW() OR
        project_index.id IS NULL)
 ORDER BY project_index.next_update_at
 LIMIT ?
"
                          :binds (list limit)))
