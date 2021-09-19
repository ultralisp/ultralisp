(defpackage #:ultralisp/metrics
  (:use #:cl)
  (:import-from #:ultralisp/stats
                #:add-counter
                #:add-gauge)
  (:import-from #:mito
                #:count-dao)
  (:import-from #:function-cache
                #:defcached)
  (:export #:initialize))
(in-package ultralisp/metrics)


(defvar *ttl* (* 5 60)
  "To not load a database, we'll not count these metrics every time when Prometheus checks the values.")


(defun count-by-sql (query)
  (let ((rows (mito:retrieve-by-sql query)))
    (getf (first rows)
          :count)))


(defcached (get-number-of-projects :timeout *ttl*) ()
  (count-by-sql "SELECT COUNT(*)
                   FROM project2
                  WHERE latest = 'true'
                    AND deleted = 'false'"))


(defcached (get-number-of-sources :timeout *ttl*) ()
  (count-by-sql "SELECT COUNT(*)
                   FROM source
                  WHERE latest = 'true'
                    AND deleted = 'false'"))

(defcached (get-number-of-disabled-sources :timeout *ttl*) ()
  (count-by-sql "SELECT COUNT(*)
                   FROM source
                   JOIN dist_source
                     ON source.id = dist_source.source_id
                    AND source.version = dist_source.source_version
                  WHERE source.latest = 'true'
                    AND source.deleted = 'false'
                    AND dist_source.enabled = 'false'"))


(defcached (get-number-of-dists :timeout *ttl*) ()
  (count-by-sql "SELECT COUNT(*)
                   FROM dist
                  WHERE latest = 'true'"))


(defcached (get-number-of-users :timeout *ttl*) ()
  (count-by-sql "SELECT COUNT(*)
                   FROM \"user\""))


(defcached (get-project-index-queue-count :timeout *ttl*) ()
  (count-by-sql
   "SELECT COUNT(*)
      FROM project_index
     WHERE next_update_at < NOW()"))


(defcached (get-project-index-ok-count :timeout *ttl*) ()
  (count-by-sql
   "SELECT COUNT(*)
      FROM project_index
     WHERE status = 'ok'"))


(defcached (get-project-index-failed-count :timeout *ttl*) ()
  (count-by-sql
   "SELECT COUNT(*)
      FROM project_index
     WHERE status = 'failed'"))


;; Here we use lesser timeout because this value will change more
;; frequently

(defcached (get-pending-checks-count :timeout 60) ()
  (count-by-sql
   "SELECT COUNT(*)
      FROM check2
     WHERE processed_at IS NULL"))


(defun initialize ()
  (ultralisp/stats:initialize)
  
  (add-counter :checks-processed "A number of processed checks")
  (add-counter :checks-failed "A number of failed checks")
  (add-counter :sources-updated "A number of sources, updated after the check")

  (add-gauge :projects-count "A number of projects"
             'get-number-of-projects)
  (add-gauge :sources-count "A number of sources"
             'get-number-of-sources)
  (add-gauge :disabled-sources-count "A number of disabled sources"
             'get-number-of-disabled-sources)
  (add-gauge :dists-count "A number of dists"
             'get-number-of-dists)
  (add-gauge :users-count "A number of users"
             'get-number-of-users)
  (add-gauge :pending-checks-count "A number of not processed checks"
             'get-pending-checks-count)
  
  (add-gauge :project-index-queue-count "A number of projects to index"
             'get-project-index-queue-count)
  (add-gauge :project-index-ok-count "A number of projects we were successfully indexed"
             'get-project-index-ok-count)
  (add-gauge :project-index-failed-count "A number of projects we were unable to index"
             'get-project-index-failed-count)
  
  (values))
