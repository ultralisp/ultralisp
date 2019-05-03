(defpackage #:ultralisp/metrics
  (:use #:cl)
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
   #:get-pending-checks-count))
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
  (count-dao 'base-check :processed-at :null))
