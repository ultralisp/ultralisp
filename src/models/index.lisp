(defpackage #:ultralisp/models/index
  (:use #:cl)
  (:import-from #:ultralisp/models/project
                #:ensure-project
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
  (:import-from #:rutils
                #:it
                #:awhen)
  (:export
   #:get-index-status
   #:set-index-status
   #:get-projects-to-index
   #:reschedule-indexing))
(in-package ultralisp/models/index)


(defun status-to-postgres (symbol)
  (check-type symbol (member :ok :failed :timeout))
  (string-downcase symbol))


(defun status-from-postgres (text)
  (when text
    (values
     (make-keyword (string-upcase text)))))


(defun get-index-status (project)
  (let* ((project (ensure-project project))
         (rows (mito:retrieve-by-sql
                "SELECT status, last_update_at, next_update_at, total_time, num_tries
                   FROM project_index WHERE project_id = ?"
                :binds (list (mito:object-id project))))
         (row (first rows)))
    (values (status-from-postgres
             (getf row :status))
            
            (awhen (getf row :last-update-at)
              (local-time:universal-to-timestamp
               it))
            
            (awhen (getf row :next-update-at)
              (local-time:universal-to-timestamp
               it))
            
            (getf row :total-time)
            
            (getf row :num-tries))))


(defun set-index-status (project status
                         &key
                           next-update-at
                           (total-time 0))
  (check-type status (member :ok :failed :timeout))
  (setf project
        (ensure-project project))
  (multiple-value-bind (prev-status
                        prev-last-update-at
                        prev-next-update-at
                        prev-total-time
                        prev-num-tries)
      (get-index-status project)
    (declare (ignore prev-last-update-at prev-next-update-at prev-total-time))

    (setf next-update-at
          (format-rfc3339-timestring
           nil
           (if next-update-at
               next-update-at
               (time-in-future :day
                               (min
                                ;; Exponencial delay
                                (* (1+ (or prev-num-tries
                                           0))
                                   1)
                                ;; Up to two weeks:
                                14)))))
    
    
    (if prev-status
        (mito:execute-sql "UPDATE project_index SET status = ?,
                                  last_update_at = NOW(),
                                  next_update_at = ?,
                                  total_time = ?,
                                  num_tries = ?
                          WHERE project_id = ?"
                          (list (status-to-postgres status)
                                next-update-at
                                total-time
                                (case status
                                  (:ok 0)
                                  (otherwise (1+ prev-num-tries)))
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
                                (status-to-postgres status))))))


(defun reschedule-indexing (project)
  (setf project
        (ensure-project project))
  (multiple-value-bind (prev-status
                        prev-last-update-at
                        prev-next-update-at
                        prev-total-time
                        prev-num-tries)
      (get-index-status project)
    (declare (ignore prev-last-update-at prev-next-update-at prev-total-time))

    (when prev-status
      (let ((next-update-at
              (format-rfc3339-timestring
               nil
               (time-in-future :day
                               (min
                                ;; Exponencial delay
                                (* (1+ prev-num-tries)
                                   1)
                                ;; Up to two weeks:
                                14)))))
        (mito:execute-sql "UPDATE project_index
                            SET next_update_at = ?,
                                num_tries = ?
                          WHERE project_id = ?"
                          (list next-update-at
                                (1+ prev-num-tries)
                                (mito:object-id project)))))))


(defun get-projects-to-index (&key (limit 10))
  "Returns a list of projects to update a search index."
  (mito.dao:select-by-sql (find-class 'project2)
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
