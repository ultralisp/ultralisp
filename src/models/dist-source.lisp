(defpackage #:ultralisp/models/dist-source
  (:use #:cl)
  (:import-from #:jonathan)
  (:import-from #:alexandria
                #:make-keyword)
  (:import-from #:ultralisp/models/dist
                #:get-or-create-pending-version
                #:dist-name
                #:dist-equal
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
   #:update-source-dists
   #:create-pending-dists-for-new-source-version
   #:make-disable-reason
   #:disable-reason-type
   #:dist-source->source
   #:dist->sources
   #:add-source-to-dist))
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
                   ;; Can be:
                   ;; - :direct
                   :initarg :include-reason
                   :reader include-reason
                   :deflate #'symbol-name
                   :inflate (lambda (text)
                              (make-keyword (string-upcase text))))
   (enabled :col-type :boolean
            :initarg :enabled
            :initform t
            :documentation "This field can be changed only on dist-source linked to the :pending dist."
            :accessor enabled-p)
   (disable-reason :col-type :jsonb
                   :initarg :disable-reason
                   :initform nil
                   :accessor disable-reason
                   :deflate #'jonathan:to-json
                   :inflate (lambda (text)
                              (jonathan:parse
                               ;; Jonathan for some reason is unable to work with
                               ;; `base-string' type, returned by database
                               (coerce text 'simple-base-string)))
                   :documentation "A plist with like this: '(:type :manual :comment \"Renamed the project.\")
                                   This field can be changed only on dist-source linked to the :pending dist.")
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


(defun dist-source->source (dist-source)
  (check-type dist-source
              ultralisp/models/dist-source:dist-source)
  (mito:find-dao
   'ultralisp/models/source:source
   :id (source-id dist-source)
   :version (source-version dist-source)))


(defgeneric source-distributions (source)
  (:method ((source ultralisp/models/source:source))
    "Returns all source distributions given source belongs to
     except those where it was deleted."
    (mito:retrieve-dao 'dist-source
                       :source-id (object-id source)
                       :source-version (object-version source)
                       :deleted "false"))
  (:method ((dist ultralisp/models/dist:dist))
    "Returns all source distributions which are enabled and not
     deleted in the given dist."
    (mito:select-dao 'dist-source
      (sxql:left-join 'ultralisp/models/source:source
                      :on (:and (:= 'dist_source.source_id
                                    'source.id)
                                (:= 'dist_source.source_version
                                    'source.version)))
      (sxql:where (:and (:= 'dist_source.dist_id
                            (object-id dist))
                        (:<= 'dist_source.dist_version
                             (object-version dist))
                        (:= 'source.latest
                            "true")
                        (:= 'source.deleted
                            "false"))))))


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

(defun dist->sources (dist)
  "Returns all sources bound to the dist dist objects along with their enabled flag"
  (check-type dist
              ultralisp/models/dist:dist)
  (loop for dist-source in (source-distributions dist)
        for source = (dist-source->source dist-source)
        collect (make-instance 'ultralisp/models/source:bound-source
                               :source source
                               :enabled (enabled-p dist-source)
                               :disable-reason (disable-reason dist-source)
                               :include-reason (include-reason dist-source))))


(defun update-source-dists (source &key (url nil url-p)
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


(defun disable-reason-type (reason)
  (getf reason :type))


(defun make-disable-reason (type &key comment traceback)
  (check-type type (member :check-error
                           :manual))
  (append (list :type type)
          (when comment
            (list :comment comment))
          (when traceback
            (list :traceback traceback))))


(defun create-pending-dists-for-new-source-version (old-source new-source &key
                                                                          (enable nil enable-p)
                                                                          disable-reason)
  "Creates pending dist copies and links new source using dist-source copies.

   If enable == t then new-source is linked as enabled unless previous link has been disabled manually.

   Links marked as \"deleted\" aren't copied.

   If source is already linked to the pending-dist,
   then it's dist-source's enabled, disable-reason are updated.
   "
  (with-transaction
    (loop for old-dist-source in (source-distributions old-source)
          for old-dist = (dist-source->dist old-dist-source)
          for pending-dist = (get-or-create-pending-version old-dist)
          for old-disable-reason = (disable-reason old-dist-source)
          for old-disable-reason-type = (disable-reason-type old-disable-reason)
          for old-enabled = (enabled-p old-dist-source)
          for new-enabled = (cond 
                              ((eql :manual old-disable-reason-type) old-enabled)
                              (enable-p enable)
                              (t old-enabled))
          for new-disable-reason = (unless new-enabled
                                     (or disable-reason
                                         old-disable-reason))
          do (let ((already-linked-dist-source
                     (first
                      (mito:retrieve-dao 'dist-source
                                         :dist-id (object-id pending-dist)
                                         :dist-version (object-version pending-dist)
                                         :source-id (object-id new-source)
                                         :source-version (object-version new-source)))))
               (cond
                 ;; This case can be when we are setting enable == t and disable reason
                 ;; for the checked source.
                 ;; In this case we just update existing dist-source:
                 (already-linked-dist-source
                  (setf (enabled-p already-linked-dist-source) new-enabled)
                  (setf (disable-reason already-linked-dist-source) new-disable-reason)
                  (mito:save-dao already-linked-dist-source))
                 (t
                  (unless (deleted-p old-dist-source)
                    (mito:create-dao 'dist-source
                                     :dist-id (object-id pending-dist)
                                     :dist-version (object-version pending-dist)
                                     :source-id (object-id new-source)
                                     :source-version (object-version new-source)
                                     :include-reason (include-reason old-dist-source)
                                     :enabled new-enabled
                                     :disable-reason new-disable-reason
                                     :deleted nil))))))))


(defun add-source-to-dist (dist source &key (include-reason :direct))
  "Creates pending dist and links the source using dist-source.

   Source is linked in \"disabled\" state.
   "
  (let* ((pending-dist (get-or-create-pending-version dist))
         (already-linked-dist-source
           (first
            (mito:retrieve-dao 'dist-source
                               :dist-id (object-id pending-dist)
                               :dist-version (object-version pending-dist)
                               :source-id (object-id source)
                               :source-version (object-version source)))))
    (unless already-linked-dist-source
      (mito:create-dao 'dist-source
                       :dist-id (object-id pending-dist)
                       :dist-version (object-version pending-dist)
                       :source-id (object-id source)
                       :source-version (object-version source)
                       :include-reason include-reason
                       :enabled nil
                       :disable-reason '(:type :just-added
                                         :comment "This source waits for the check.")
                       :deleted nil))))
