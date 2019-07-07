(defpackage #:ultralisp/metrics
  (:use #:cl)
  (:import-from #:ultralisp/stats
                #:add-counter
                #:add-gauge)
  (:import-from #:ultralisp/models/project
                #:project)
  (:import-from #:mito
                #:count-dao)
  (:import-from #:ultralisp/models/version
                #:version)
  (:import-from #:weblocks-auth/models
                #:user)
  (:import-from #:function-cache
                #:defcached)
  (:import-from #:ultralisp/models/check
                #:base-check)
  (:export
   #:get-number-of-disabled-projects
   #:get-number-of-projects
   #:get-number-of-users
   #:get-number-of-versions
   #:get-pending-checks-count
   #:initialize))
(in-package ultralisp/metrics)


(defvar *ttl* (* 5 60)
  "To not load a database, we'll not count these metrics every time when Prometheus checks the values.")


(defcached (get-number-of-projects :timeout *ttl*) ()
  (count-dao 'project :enabled 1))


(defcached (get-number-of-disabled-projects :timeout *ttl*) ()
  (count-dao 'project :enabled 0))


(defcached (get-number-of-versions :timeout *ttl*) ()
  (count-dao 'version))


(defcached (get-number-of-users :timeout *ttl*) ()
  (count-dao 'user))


;; Here we use lesser timeout because this value will change more
;; frequently
(defcached (get-pending-checks-count :timeout 60) ()
  (length (remove-if-not (lambda (check)
                           (null (ultralisp/models/check:get-processed-at check)))
                         (mito:retrieve-dao 'base-check)))
  ;; TODO: return when https://github.com/fukamachi/mito/issues/56 will be fixed
  ;; (count-dao 'base-check :processed-at :null)
  )



(defun initialize ()
  (ultralisp/stats:initialize)
  
  (add-counter :checks-processed "A number of processed checks")
  (add-counter :checks-failed "A number of failed checks")
  (add-counter :projects-updated "A number of projects, updated after the check")

  (add-gauge :projects-count "A number of projects"
             'ultralisp/metrics:get-number-of-projects)
  (add-gauge :disabled-projects-count "A number of disabled projects"
             'ultralisp/metrics:get-number-of-disabled-projects)
  (add-gauge :versions-count "A number of Ultralisp versions"
             'ultralisp/metrics:get-number-of-versions)
  (add-gauge :users-count "A number of users"
             'ultralisp/metrics:get-number-of-users)
  (add-gauge :pending-checks-count "A number of not processed checks"
             'ultralisp/metrics:get-pending-checks-count)
  
  (values))
