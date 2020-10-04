(defpackage #:ultralisp/models/dist-source
  (:use #:cl)
  (:import-from #:jonathan)
  (:import-from #:alexandria
                #:make-keyword)
  (:import-from #:ultralisp/models/dist
                #:get-or-create-pending-version
                #:dist-name
                #:ensure-dist)
  (:import-from #:ultralisp/models/source)
  (:import-from #:mito
                #:object-id)
  (:import-from #:ultralisp/models/versioned
                #:object-version)
  (:import-from #:ultralisp/db
                #:with-transaction)
  (:export
   #:dist-source
   #:dist-id
   #:dist-version
   #:source-id
   #:source-version
   #:include-reason
   #:enabled-p
   #:disable-reason
   #:deleted-p
   #:dist-source->dist
   #:source->dists
   #:source-distributions
   #:update-source))
(in-package ultralisp/models/dist-source)


(defclass dist-source ()
  ((dist-id :col-type :bigint
            :initarg :dist-id
            :reader dist-id)
   (dist-version :col-type :bigint
                 :initarg :dist-version
                 :reader dist-version)
   (source-id :col-type :bigint
              :initarg :source-id
              :reader source-id)
   (source-version :col-type :bigint
                   :initarg :source-version
                   :reader source-version)
   (include-reason :col-type :text
                   :initarg :include-reason
                   :reader include-reason
                   :deflate #'symbol-name
                   :inflate (lambda (text)
                              (make-keyword (string-upcase text))))
   (enabled :col-type :boolean
            :initarg :enabled
            :initform t
            :reader enabled-p)
   (disable-reason :col-type :jsonb
                   :initarg :disable-reason
                   :initform nil
                   :reader disable-reason
                   :deflate #'jonathan:to-json
                   :inflate (lambda (text)
                              (jonathan:parse
                               ;; Jonathan for some reason is unable to work with
                               ;; `base-string' type, returned by database
                               (coerce text 'simple-base-string))))
   (deleted :col-type :boolean
            :initarg :deleted
            :initform nil
            :reader deleted-p))
  (:primary-key
   dist-id dist-version
   source-id source-version)
  (:metaclass mito:dao-table-class))


(defmethod include-reason ((obj ultralisp/models/dist:bound-dist))
  (ultralisp/models/dist::include-reason obj))


(defun dist-source->dist (dist-source)
  (check-type dist-source dist-source)
  (first
   (mito:retrieve-dao
    'ultralisp/models/dist:dist
    :id (dist-id dist-source)
    :version (dist-version dist-source))))


(defun source-distributions (source)
  (check-type source ultralisp/models/source:source)
  (mito:retrieve-dao 'dist-source
                     :source-id (object-id source)
                     :source-version (object-version source)
                     :deleted "false"))


(defun source->dists (source)
  "Returns dist objects along with their enabled flag"
  (check-type source
              ultralisp/models/source:source)
  (loop for dist-source in (source-distributions source)
        for dist = (dist-source->dist dist-source)
        collect (make-instance 'ultralisp/models/dist:bound-dist
                               :dist dist
                               :enabled (enabled-p dist-source)
                               :disable-reason (disable-reason dist-source)
                               :include-reason (include-reason dist-source))))


;; (defun dist-source->source (dist-source)
;;   (check-type dist-source
;;               ultralisp/models/dist-source:dist-source)
;;   (first
;;    (mito:retrieve-dao
;;     'source
;;     :project-id (ultralisp/models/dist-source:project-id dist-source)
;;     :project-version (ultralisp/models/dist-source:project-version dist-source)
;;     :version (ultralisp/models/dist-source:source-version dist-source))))


(defun update-source (source &key (url nil url-p)
                                  (dists nil dists-p)
                                  (include-reason :direct))
  (declare (ignorable url))
  (check-type source ultralisp/models/source:source)
  (when url-p
    (error "Changing url is not supported yet"))
  (when dists-p
    (with-transaction
      (let* ((new-dists (mapcar #'ensure-dist dists))
             (current-dists (remove-if-not
                             #'ultralisp/models/dist:enabled-p
                             (source->dists source)))
             (dists-to-remove
               (set-difference current-dists
                               new-dists
                               :key #'dist-name
                               :test #'string=))
             (dists-to-add
               (set-difference new-dists
                               current-dists
                               :key #'dist-name
                               :test #'string=))
             ;; These dists nor added nor removed,
             ;; we have to keep links to them
             (keep-dists
               (intersection current-dists
                             new-dists
                             :key #'dist-name
                             :test #'string=)))
        (when (or dists-to-remove
                  dists-to-add)

          (log:error "KEPPP" keep-dists)
          ;; We need to create a source's copy
          ;; to increment it's version which will be bound to new
          ;; sources:
          (setf source
                (ultralisp/models/source:copy-source source))
          
          (loop for dist in keep-dists
                do (mito:create-dao 'dist-source
                                    :dist-id (object-id dist)
                                    :dist-version (object-version dist)
                                    :source-id (object-id source)
                                    :source-version (object-version source)
                                    :include-reason (include-reason dist)))
          
          ;; If source should be added to the dist,
          ;; we have to get/create a pending dist
          ;; and to link this source to it:
          (loop for dist in dists-to-add
                for new-version = (get-or-create-pending-version dist)
                do (mito:create-dao 'dist-source
                                    :dist-id (object-id new-version)
                                    :dist-version (object-version new-version)
                                    :source-id (object-id source)
                                    :source-version (object-version source)
                                    :include-reason include-reason))
          
          ;; if source should be removed from some dist,
          ;; then we have to get/create a pending dist
          ;; and to link this source with "deleted" mark:
          (loop for dist in dists-to-remove
                for new-version = (get-or-create-pending-version dist)
                do (mito:create-dao 'dist-source
                                    :dist-id (object-id new-version)
                                    :dist-version (object-version new-version)
                                    :source-id (object-id source)
                                    :source-version (object-version source)
                                    ;; Here we reuse reason from the removed
                                    ;; dist.
                                    :include-reason (include-reason dist)
                                    ;; Important to set this flag:
                                    :deleted t))))))
  ;; Now we'll return old or a new source: 
  source)
