(defpackage #:ultralisp/models/dist-source
  (:use #:cl)
  (:import-from #:jonathan)
  (:import-from #:alexandria
                #:make-keyword)
  (:import-from #:ultralisp/models/dist
                #:dist-state
                #:get-or-create-pending-version
                #:dist-name
                #:dist-equal
                #:ensure-dist)
  (:import-from #:ultralisp/models/source
                #:copy-source)
  (:import-from #:mito
                #:object-id)
  (:import-from #:ultralisp/models/versioned
                #:prev-version
                #:object-version)
  (:import-from #:ultralisp/db
                #:with-transaction)
  (:import-from #:ultralisp/utils
                #:update-plist)
  (:import-from #:ultralisp/protocols/enabled
                #:enabled-p)
  (:export
   #:dist-source
   #:dist-id
   #:dist-version
   #:source-id
   #:source-version
   #:include-reason
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
                   :documentation "This field can be changed only on dist-source linked to the :pending dist."
                   :accessor source-version)
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
            :documentation "This field can be changed only on dist-source linked to the :pending dist."
            :accessor deleted-p))
  (:primary-key
   dist-id dist-version
   source-id)
  (:metaclass mito:dao-table-class))


(defmethod include-reason ((obj ultralisp/models/dist:bound-dist))
  (ultralisp/models/dist::include-reason obj))


(defun dist-source->dist (dist-source)
  (check-type dist-source dist-source)
  (mito:find-dao
   'ultralisp/models/dist:dist
   :id (dist-id dist-source)
   :version (dist-version dist-source)))


(defun dist-source->source (dist-source)
  (check-type dist-source
              ultralisp/models/dist-source:dist-source)
  (mito:find-dao
   'ultralisp/models/source:source
   :id (source-id dist-source)
   :version (source-version dist-source)))


(defgeneric source-distributions (source &key)
  (:method ((source ultralisp/models/source:source) &key (enabled nil enabled-given-p))
    "Returns all source distributions given source belongs to
     except those where it was deleted."
    (apply #'mito:retrieve-dao
           'dist-source
           :source-id (object-id source)
           :source-version (object-version source)
           :deleted "false"
           (when enabled-given-p
             (list :enabled
                   (if enabled
                       "true"
                       "false")))))
  (:method ((dist ultralisp/models/dist:dist) &key (enabled nil enabled-given-p)
                                                   limit)
    "Returns all source distributions which are enabled and not
     deleted in the given dist.

     Note: Results contain all sources linked to the previous
     dist versions."
    (let ((clauses
            `(:and (:= dist_source.dist_id
                       ,(object-id dist))
                   ,@(when enabled-given-p
                       `((:= dist_source.enabled
                             ,(if enabled
                                  "true"
                                  "false"))))
                   (:<= dist_source.dist_version
                        ,(object-version dist))
                   (:= source.latest
                       "true")
                   (:= source.deleted
                       "false"))))
      (mito:select-dao 'dist-source
        (sxql:left-join 'ultralisp/models/source:source
                        :on (:and (:= 'dist_source.source_id
                                      'source.id)
                                  (:= 'dist_source.source_version
                                      'source.version)))
        (sxql:where clauses)
        (sxql:limit limit)))))


(defun %this-version-source-distributions (dist &key (enabled nil enabled-given-p)
                                                     limit)
  "Returns only source distributions which are enabled 
   deleted in the given dist."
  (check-type dist ultralisp/models/dist:dist)
  
  (let ((clauses
          `(:and
            (:= dist_source.dist_id
                ,(object-id dist))
            (:= dist_source.dist_version
                ,(object-version dist))
            ,@(when enabled-given-p
                `((:= dist-source.enabled
                      ,(if enabled
                           "true"
                           "false")))))))
    (mito:select-dao 'dist-source
      (sxql:where clauses)
      (sxql:limit limit))))


(defun source->dists (source &key (enabled nil enabled-given-p))
  "Returns dist objects along with their enabled flag"
  (check-type source
              ultralisp/models/source:source)
  (loop for dist-source in (apply #'source-distributions
                                  source
                                  (when enabled-given-p
                                    (list :enabled enabled)))
        for dist = (dist-source->dist dist-source)
        collect (make-instance 'ultralisp/models/dist:bound-dist
                               :dist dist
                               :enabled (enabled-p dist-source)
                               :disable-reason (disable-reason dist-source)
                               :include-reason (include-reason dist-source))))


(defun dist->sources (dist &key this-version
                                (enabled nil enabled-given-p)
                                limit)
  "Returns all sources bound to the dist dist objects along with their enabled flag"
  (check-type dist
              ultralisp/models/dist:dist)
  (loop for dist-source in (cond
                             (this-version
                              (apply #'%this-version-source-distributions
                                     dist
                                     :limit limit
                                     (when enabled-given-p
                                       (list :enabled enabled))))
                             (t
                              (apply #'source-distributions
                                     dist
                                     (when enabled-given-p
                                       (list :enabled enabled)))))
        for source = (dist-source->source dist-source)
        collect (make-instance 'ultralisp/models/source:bound-source
                               :source source
                               :dist dist
                               :enabled (enabled-p dist-source)
                               :disable-reason (disable-reason dist-source)
                               :include-reason (include-reason dist-source))))


(defmethod prev-version ((obj ultralisp/models/source:bound-source))
  "
   To get the previous version of bound source, we have to find
   a source which was bound to a previos version of the dist.

   Otherwise we can choose wrong source version which was unbound
   during the distributions list change.
  "
  (let* ((source (ultralisp/models/source:source obj))
         (dist (ultralisp/models/source:dist obj))
         (prev-dist (prev-version dist)))
    (when prev-dist
      (let ((prev-dist-source (mito:find-dao 'dist-source
                                             ;; Here we use only dist-id
                                             ;; because prev-source can be bound
                                             ;; to another version of the same dist
                                             :dist-id (object-id prev-dist)
                                             :dist-version (object-version prev-dist)
                                             :source-id (object-id source))))
        (when prev-dist-source
          (let ((prev-source (ultralisp/models/source:find-source-version
                              (source-id prev-dist-source)
                              (source-version prev-dist-source))))
            (unless prev-source
              (error "Unable to find source with id = ~A and version = ~A"
                     (source-id prev-dist-source)
                     (source-version prev-dist-source)))
            (make-instance 'ultralisp/models/source:bound-source
                           :source prev-source
                           :dist prev-dist
                           :enabled (enabled-p prev-dist-source)
                           :disable-reason (disable-reason prev-dist-source)
                           :include-reason (include-reason prev-dist-source))))))))


(defun update-source-dists (source &key (params nil)
                                        (dists nil dists-p)
                                        (include-reason :direct)
                                        (disable-reason :manual)
                                        ;; If new-source version is not given,
                                        ;; it will be created automatically:
                                        (new-source nil))
  "Creates a new version of the source by changing the dists and optionally updating source params.

   New version is created only if there were changes in params or dists.

   Returns:
     Two values - a source object and boolean.
     If boolean is `t` then source was cloned and updated.
"
  (check-type source ultralisp/models/source:source)

  (with-transaction
    (multiple-value-bind (new-params params-changed-p)
        (update-plist (ultralisp/models/source:source-params source)
                      params)
      (flet ((ensure-there-is-a-clone ()
               (unless new-source
                 (setf new-source
                       (ultralisp/models/source:copy-source source
                                                            :params new-params)))))
        (when params-changed-p
          (ensure-there-is-a-clone))

        (let* ((current-dists (remove-if-not
                               #'enabled-p
                               (source->dists source)))
               (new-dists (if dists-p
                              (mapcar #'ensure-dist dists)
                              ;; If dists list was not given, then
                              ;; we'll just keep all dists as is by
                              ;; reataching them to a new source version.
                              current-dists))
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
          
          (when dists-to-add
            (ensure-there-is-a-clone)
            ;; If source should be added to the dist,
            ;; we have to get/create a pending dist
            ;; and to link this source to it:
            (loop for dist in dists-to-add
                  for new-version = (get-or-create-pending-version dist)
                  for old-dist-source = (mito:find-dao 'dist-source
                                                       :dist-id (object-id dist)
                                                       :dist-version (object-version dist)
                                                       :source-id (object-id source))
                  do (cond
                       ;; We only need to create a new link if a new pending
                       ;; version was created and the source previously
                       ;; was linked to it (and probably removed).
                       ((and (dist-equal dist new-version)
                             old-dist-source)
                        ;; When dist version is the same and it is still pending,
                        ;; we can just mark existing dist-source as enabled.
                        (unless (eql (dist-state new-version)
                                     :pending)
                          (error "We can remove a new source version only to a pending dist version."))
                        ;; First, we need to change source version in the link
                        (setf (source-version old-dist-source)
                              (object-version new-source))
                        ;; Second, to set flags, showing that the source is enabled and not deleted
                        ;; from the dist:
                        (setf (deleted-p old-dist-source) nil
                              (enabled-p old-dist-source) t
                              (disable-reason old-dist-source) nil)
                        (mito:update-dao old-dist-source))
                       (t
                        (mito:create-dao 'dist-source
                                         :dist-id (object-id new-version)
                                         :dist-version (object-version new-version)
                                         :source-id (object-id new-source)
                                         :source-version (object-version new-source)
                                         :include-reason include-reason)))))

          (when dists-to-remove
            (ensure-there-is-a-clone)
            ;; if source should be removed from some dist,
            ;; then we have to get/create a pending dist
            ;; and to link this source with "deleted" mark:
            (loop with disable-reason = (make-disable-reason
                                         disable-reason)
                  for dist in dists-to-remove
                  for new-version = (get-or-create-pending-version dist)
                  for old-dist-source = (mito:find-dao 'dist-source
                                                       :dist-id (object-id dist)
                                                       :dist-version (object-version dist)
                                                       :source-id (object-id source))
                  do (cond
                       ;; We only need to create a new link if a new pending
                       ;; version was created.
                       ((dist-equal dist new-version)
                        ;; When dist version is the same and it is still pending,
                        ;; we can just mark existing dist-source as disabled.
                        (unless (eql (dist-state new-version)
                                     :pending)
                          (error "We can remove a new source version only to a pending dist version."))
                        ;; First, we need to change source version in the link
                        (setf (source-version old-dist-source)
                              (object-version new-source))
                        ;; Second, to set flags, showing that the source was disabled and removed
                        ;; from the dist:
                        (setf (deleted-p old-dist-source) t
                              (enabled-p old-dist-source) nil
                              (disable-reason old-dist-source) disable-reason)
                        (mito:update-dao old-dist-source))
                       (t
                        (mito:create-dao 'dist-source
                                         :dist-id (object-id new-version)
                                         :dist-version (object-version new-version)
                                         :source-id (object-id new-source)
                                         :source-version (object-version new-source)
                                         ;; Here we reuse reason from the removed
                                         ;; dist.
                                         :include-reason (include-reason dist)
                                         ;; Important to set this flag:
                                         :deleted t
                                         :enabled nil
                                         :disable-reason disable-reason)))))
          
          (when new-source
            ;; We need to execute this section in any case if params were updated
            ;; or if some dists were changed. In both cases, source will be
            ;; cloned and new-version will not be nil.
            (loop for dist in keep-dists
                  for new-version = (get-or-create-pending-version dist)
                  ;; Here we need to attach to the pending dist a new source version
                  for old-dist-source = (mito:find-dao 'dist-source
                                                       :dist-id (object-id dist)
                                                       :dist-version (object-version dist)
                                                       :source-id (object-id source))
                  do (cond
                       ;; We only need to create a new link if a new pending
                       ;; version was created.
                       ((dist-equal dist new-version)
                        ;; When dist version is the same, but we need to bind
                        ;; a new source version to it, then we'll modify
                        ;; existing dist-source.
                        ;;
                        ;; However, we can do this only on pending dist
                        (unless (eql (dist-state new-version)
                                     :pending)
                          (error "We can rebind a new source version only to a pending dist version."))
                        (setf (source-version old-dist-source)
                              (object-version new-source))
                        (mito:update-dao old-dist-source))
                       (t
                        (mito:create-dao 'dist-source
                                         :dist-id (object-id new-version)
                                         :dist-version (object-version new-version)
                                         :source-id (object-id new-source)
                                         :source-version (object-version new-source)
                                         ;; Keep previous inclusion reason
                                         :include-reason (include-reason dist))))))))

      ;; Also, we'll need to recheck this source
      ;; before it will be included into the new dist versions.
      ;; 
      ;; We don't do this when only dists list changed, because
      ;; dists don't affect the source's code.
      (when params-changed-p
        ;; We have a circular dependency:
        ;; project -> dist-source -> check -> project :(
        (uiop:symbol-call "ULTRALISP/MODELS/CHECK"
                          "MAKE-CHECK"
                          new-source :changed-project)))
    
    ;; Now we'll return old or a new source and a True as a second
    ;; value, if something was changed and source was cloned.
    (values (or new-source
                source)
            (when new-source
              t))))


(defun disable-reason-type (reason)
  (getf reason :type))


(defun make-disable-reason (type &key comment traceback)
  (check-type type (member :check-error
                           ;; When source gets added to the distribution,
                           ;; it has this disable reason.
                           ;; However, if it has some release-info,
                           ;; it is added as "enabled".
                           :just-added
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
  (log:info "Checking if we need to create a pending dist for a" new-source "copied from" old-source)
  (log:debug "Other params are" enable enable-p disable-reason)

  (unless (= (object-id old-source)
             (object-id new-source))
    (error "Old source and new source are versions of different sources."))

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
          do (let ((old-dist-source-linked-to-the-pending-dist
                     (mito:find-dao 'dist-source
                                    :dist-id (object-id pending-dist)
                                    :dist-version (object-version pending-dist)
                                    :source-id (object-id old-source)
                                    :source-version (object-version old-source))))
               (log:debug "Found or created pending dist"
                          pending-dist
                          old-enabled
                          old-disable-reason)
               (cond
                 ;; This case can be when we are setting enable == t and disable reason
                 ;; for the checked source which is bound to the pending dist.
                 ;; In this case we just update existing dist-source, detaching the old
                 ;; source version and attaching the new version.
                 (old-dist-source-linked-to-the-pending-dist
                  (log:debug "Found existing dist-source bound to a pending dist." 
                             old-dist-source-linked-to-the-pending-dist
                             new-enabled
                             new-disable-reason)
                  (setf (enabled-p old-dist-source-linked-to-the-pending-dist)
                        new-enabled)
                  (setf (disable-reason old-dist-source-linked-to-the-pending-dist)
                        new-disable-reason)
                  (setf (source-version old-dist-source-linked-to-the-pending-dist)
                        (object-version new-source))

                  (mito:save-dao old-dist-source-linked-to-the-pending-dist))
                 
                 ((deleted-p old-dist-source)
                  (log:debug "Source was deleted from the dist, we'll ignore it and don't create a link to a pending dist."))
                 (t
                  (let ((include-reason (include-reason old-dist-source)))
                    (log:debug "Creating a link from source to the dist"
                               new-source
                               pending-dist
                               new-enabled
                               include-reason
                               new-disable-reason)
                    (mito:create-dao 'dist-source
                                     :dist-id (object-id pending-dist)
                                     :dist-version (object-version pending-dist)
                                     :source-id (object-id new-source)
                                     :source-version (object-version new-source)
                                     :include-reason include-reason
                                     :enabled new-enabled
                                     :disable-reason new-disable-reason
                                     :deleted nil))))))))


(defun add-source-to-dist (dist source &key (include-reason :direct))
  "Creates pending dist and links the source using dist-source.

   Source is linked in \"disabled\" state.
   "
  (let* ((pending-dist (get-or-create-pending-version dist))
         (already-linked-dist-source
           (mito:find-dao 'dist-source
                          :dist-id (object-id pending-dist)
                          :dist-version (object-version pending-dist)
                          :source-id (object-id source)
                          :source-version (object-version source))))
    (unless already-linked-dist-source
      
      (let* ((has-release-info (ultralisp/models/source:source-release-info source))
             (enabled (when has-release-info
                        t))
             (disable-reason (unless has-release-info
                               ;; When source gets added to the distribution,
                               ;; it has this disable reason.
                               ;; However, if it has some release-info,
                               ;; it is added as "enabled", because we don't
                               ;; need to check it to build the distribution.
                               (make-disable-reason :just-added
                                                    :comment "This source waits for the check."))))
          (mito:create-dao 'dist-source
                           :dist-id (object-id pending-dist)
                           :dist-version (object-version pending-dist)
                           :source-id (object-id source)
                           :source-version (object-version source)
                           :include-reason include-reason
                           :enabled enabled
                           :disable-reason disable-reason
                           :deleted nil)))))


(defun delete-source (source)
  "Removes source from all dists and marks it as deleted.

   Actually, this all happens with a new source version."
  (with-transaction
    (let ((new-source (copy-source source :deleted t)))
      (update-source-dists source
                           :new-source new-source
                           :dists nil))))