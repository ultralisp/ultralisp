(defpackage #:ultralisp/models/check
  (:use #:cl)
  (:import-from #:ultralisp/models/project
                #:project-sources
                #:get-name
                #:project
                #:project2)
  (:import-from #:ultralisp/models/source)
  (:import-from #:mito
                #:object-id
                #:retrieve-by-sql
                #:includes
                #:count-dao
                #:save-dao
                #:select-dao
                #:dao-table-class)
  (:import-from #:sxql
                #:from
                #:make-sql-symbol
                #:select
                #:left-join
                #:where
                #:order-by
                #:limit)
  (:import-from #:ultralisp/models/version
                #:make-version
                #:version)
  (:import-from #:alexandria
                #:make-keyword)
  (:import-from #:anaphora
                #:acond
                #:it)
  (:import-from #:ultralisp/models/versioned
                #:object-version)
  (:import-from #:global-vars
                #:define-global-var)
  (:import-from #:ultralisp/utils/db
                #:inflate-keyword
                #:deflate-keyword)
  (:export
   #:get-project-checks
   #:make-added-project-check
   #:make-via-webhook-check
   #:make-via-cron-check
   #:base-check
   #:get-project
   #:project-has-changes-p
   #:get-description
   #:get-type
   #:get-check
   #:get-pending-checks
   #:get-processed-at
   #:get-checks
   #:get-error
   #:get-all-checks
   #:get-check-by-id
   #:any-check
   #:get-processed-in
   #:get-last-project-check
   #:get-pending-checks-count
   #:get-pending-checks-for-disabled-projects
   #:get-pending-checks-for-disabled-projects-count
   #:source-checks
   #:make-check
   #:pending-checks
   #:make-checks
   #:check->source
   #:check->project
   #:get-last-project-checks
   #:get-last-source-check
   #:get-check2-by-id
   #:check2
   #:position-in-the-queue))
(in-package ultralisp/models/check)


(define-global-var +allowed-check-types+
  '(:added-project
    :changed-project
    :via-cron
    :via-webhook
    :manual))


(defclass any-check ()
  ())


(defmacro defcheck (name)
  (let ((class-name (intern
                     (string-upcase
                      (format nil "~A-check"
                              name))))
        (make-func-name (intern
                         (string-upcase
                          (format nil "make-~A-check"
                                  name))))
        (check-type (make-keyword name)))
    `(progn
       (export ',class-name)
       (defclass ,class-name (any-check)
         ((type :col-type (or :text :null)
                :initarg :type
                :initform nil
                :reader get-type
                :documentation "Should be one of :added-project :via-webhook :via-cron"
                :inflate #'inflate-keyword
                :deflate #'deflate-keyword)
          (project :col-type project
                   :initarg :project
                   :documentation "A link to a project to be checked."
                   :reader get-project)
          (processed-at :col-type (or :timestamptz :null)
                        :initarg :processed-at
                        :initform nil
                        :accessor get-processed-at
                        :documentation "Date and time a check was finished at.")
          (processed-in :col-type (or :float :null)
                        :initarg :processed-in
                        :initform nil
                        :accessor get-processed-in
                        :documentation "Number of seconds required to process this check.")
          (error :col-type (or :text :null)
                 :initarg :error
                 :initform nil
                 :accessor get-error
                 :documentation "A error description. If processed-at is not null and error is nil,
                                 then check is considered as successful."))
         (:table-name "check")
         (:metaclass dao-table-class))

       
       (defmethod print-object ((check ,class-name) stream)
         (print-unreadable-object (check stream :type t)
           (format stream "~A~@[ ~A~]"
                   (get-name (get-project check))
                   (when (slot-boundp check 'processed-at)
                     (get-processed-at check)))))


       (defun ,make-func-name (project)
         "Creates or gets a check for the project.

          As a second value, returns `t' if check was created, or nil
          if it already existed in the database."
         (check-type project project)
         (let ((type ,check-type))
           (log:info "Triggering a check for" project type)
           
           (unless (member type +allowed-check-types+)
             (let ((*print-case* :downcase))
               (error "Unable to create check of type ~S"
                      type)))

           (acond
             ((get-project-checks project :pending t)
              (log:warn "Check already exists")
              (values (first it) nil))
             (t (values (mito:create-dao ',class-name
                                         :project project
                                         :type type)
                        t))))))))


(defcheck base)
(defcheck added-project)
(defcheck via-webhook)
(defcheck via-cron)

(defclass check2 ()
  ((type :col-type (or :text :null)
         :initarg :type
         :initform nil
         :reader get-type
         :documentation "Should be one of :added-project :via-webhook :via-cron"
         :inflate #'inflate-keyword
         :deflate #'deflate-keyword)
   (source-id :col-type :bigint
              :initarg :source-id
              :reader source-id)
   (source-version :col-type :bigint
                   :initarg :source-version
                   :reader source-version)
   (processed-in :col-type (or :float :null)
                 :initarg :processed-in
                 :initform nil
                 :accessor get-processed-in
                 :documentation "Number of seconds required to process this check.")
   (processed-at :col-type (or :timestamptz :null)
                 :initarg :processed-at
                 :initform nil
                 :accessor get-processed-at
                 :documentation "Date and time a check was finished at.")
   (error :col-type (or :text :null)
          :initarg :error
          :initform nil
          :accessor get-error
          :documentation "A error description. If processed-at is not null and error is nil,

                                 then check is considered as successful."))
  (:metaclass dao-table-class))




;; #:make-added-project-check
;; #:make-via-webhook-check
;; #:make-via-cron-check

(defun make-check (source check-type)
  "Creates or gets a check for the project.

   As a second value, returns `t' if check was created, or nil
   if it already existed in the database."
  (check-type source ultralisp/models/source:source)
  
  (unless (member check-type +allowed-check-types+)
    (let ((*print-case* :downcase))
      (error "Unable to create check of type ~S. Use one of these: ~{~A~^, ~}."
             check-type
             +allowed-check-types+)))
  
  (log:info "Triggering a check for" source check-type)

  (acond
    ((source-checks source :pending t)
     (log:warn "Check already exists")
     (values (first it) nil))
    (t (values (mito:create-dao 'check2
                                :source-id (object-id source)
                                :source-version (object-version source)
                                :type check-type)
               t))))


(defun make-checks (project check-type)
  "Creates checks of given type for all project sources."
  (check-type project ultralisp/models/project:project2)
  
  (loop for source in (project-sources project)
        collect (make-check source check-type)))


(defmethod get-error :around (check)
  (if (get-processed-at check)
      (call-next-method)
      ;; Unless check was processed we can't know the result:
      (error "This check wasn't processed yet.")))


(defun upgrade-type (check)
  (when check
    (let* ((type (get-type check))
           (real-type (case type
                        (:added-project 'added-project-check)
                        (:via-webhook 'via-webhook-check)
                        (:via-cron 'via-cron-check))))
      (if real-type
          (change-class check real-type)
          check))))


(defun upgrade-types (checks)
  (mapcar #'upgrade-type checks))


;; TODO: remove, replaced withh pending-checks
(defun get-pending-checks (&key limit)
  (declare (ignore limit))
  (error "Shold be removed")
  ;; (upgrade-types
  ;;  (select-dao 'base-check
  ;;    (includes 'project)
  ;;    (left-join 'project
  ;;               :on (:= 'check.project_id
  ;;                       'project.id))
  ;;    (where (:is-null 'processed-at))
  ;;    (when limit
  ;;      (sxql:limit limit))))
  )

(defun cancel-pending-cron-checks ()
  (loop for check in (select-dao 'base-check
                       (where (:and (:is-null 'processed-at)
                                    (:= 'type "VIA-CRON"))))
        do (setf (get-processed-at check) (local-time:now)
                 (get-processed-in check) 0)
           (save-dao check)
        counting 1))

(defun get-pending-checks-count ()
  (let ((sql (select ((:as (:count :*)
                       :count))
               (from (make-sql-symbol (mito.dao::table-name (mito.util:ensure-class 'base-check))))
               (left-join 'project
                          :on (:= 'check.project_id
                                  'project.id))
               (where (:and (:is-null 'processed_at)
                       :project.enabled)))))
    (getf (first (retrieve-by-sql sql))
          :count)))


(defun get-pending-checks-for-disabled-projects (&key limit)
  (upgrade-types
   (select-dao 'base-check
     (includes 'project)
     (left-join 'project
                :on (:= 'check.project_id
                        'project.id))
     (where (:and (:is-null 'processed-at)
                  (:not :project.enabled)))
     (when limit
       (sxql:limit limit)))))

(defun get-pending-checks-for-disabled-projects-count ()
  (let ((sql (select ((:as (:count :*)
                       :count))
               (from (make-sql-symbol (mito.dao::table-name (mito.util:ensure-class 'base-check))))
               (left-join 'project
                          :on (:= 'check.project_id
                                  'project.id))
               (where (:and (:is-null 'processed_at)
                            (:not :project.enabled))))))
    (getf (first (retrieve-by-sql sql))
          :count)))

(defun get-all-checks ()
  (upgrade-types
   (select-dao 'base-check)))


(defun get-check-by-id (id)
  (upgrade-type
   (mito:find-dao 'base-check
                  :id id)))


(defun get-checks (version)
  (check-type version version)
  (upgrade-types
   (mito:retrieve-dao 'base-check
                      :version version)))


;; TODO: remove
(defun get-project-checks (project &key pending)
  (check-type project project)
  (upgrade-types
   (if pending
       (select-dao 'base-check
         (where (:and (:is-null 'processed-at)
                      (:= 'project-id (mito:object-id project)))))
       (mito:retrieve-dao 'base-check
                          :project project))))


(defun source-checks (source &key pending)
  (check-type source ultralisp/models/source:source)
  (if pending
      (select-dao 'check2
        (where (:and (:is-null 'processed-at)
                     (:= 'source-id (object-id source))
                     (:= 'source-version (object-version source)))))
      (mito:retrieve-dao 'check2
                         :source-id (object-id source)
                         :source-version (object-version source))))


(defun check->source (check)
  (check-type check check2)
  (values
   (ultralisp/models/source:get-source (source-id check)
                                       (source-version check))))


(defun check->project (check)
  (check-type check check2)
  (ultralisp/models/project:source->project
   (check->source check)))


(defun pending-checks ()
  "Returns all pending checks ordered from oldest to newest.

   Ordering is important, because we'll use it to calculate
   check's posiition in the queue."
  (select-dao 'check2
    (where (:is-null 'processed-at))
    (sxql:order-by 'created-at
                   ;; Additional ordering by id to make sort
                   ;; stable, because multiple checks can be
                   ;; created during single transaction and
                   ;; they will have the same created-at.
                   'id)))


(defun position-in-the-queue (check)
  (check-type check check2)
  (let* ((query (cl-dbi:prepare mito:*connection*
                                "
SELECT position
  FROM (SELECT id,
              (ROW_NUMBER() OVER (order by created_at, id desc) - 1) as position
          FROM check2
         WHERE processed_at IS NULL) AS t
 WHERE id = ?
"))
         (params (list (object-id check)))
         (results (cl-dbi:execute query params))
         (rows (cl-dbi:fetch-all results)))
    (getf (first rows)
          :|position|)))


(defmethod print-object ((check check2) stream)
  (print-unreadable-object (check stream :type t)
    (let ((source (or (ignore-errors
                       (check->source check))
                      "[unable to retrieve source]")))
      (format stream "~A ~A for ~A"
              (if (get-processed-at check)
                  "DONE"
                  "PENDING")
              (get-type check)
              source))))


(defun get-last-source-check (source)
  (check-type source ultralisp/models/source:source)
  (first
   (select-dao 'check2
     (where (:= 'source-id (mito:object-id source)))
     (order-by (:desc 'processed-at)
               (:desc 'created-at))
     (limit 1))))


(defun get-last-project-checks (project)
  "Returns a last performed checks for each of the project's sources.
   Checks are sorted from most recent to older."
  (check-type project project2)
  (flet ((check-timestamp (check)
           (or (get-processed-at check)
               (mito:object-updated-at check))))
    (loop for source in (project-sources project)
          for check = (get-last-source-check source)
          when check
          collect check into results
          finally (return (sort results #'local-time:timestamp>
                                :key #'check-timestamp)))))


(defun get-check2-by-id (id)
  (mito:find-dao 'check2
                 :id id))
