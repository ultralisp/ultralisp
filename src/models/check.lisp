(defpackage #:ultralisp/models/check
  (:use #:cl)
  (:import-from #:ultralisp/models/project
                #:get-name
                #:project)
  (:import-from #:mito
                #:save-dao
                #:select-dao
                #:dao-table-class)
  (:import-from #:sxql
                #:where)
  (:import-from #:ultralisp/models/version
                #:make-version
                #:version)
  (:import-from #:alexandria
                #:make-keyword)
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
   #:get-check-by-id))
(in-package ultralisp/models/check)


(defparameter *allowed-check-types*
  '(:added-project :via-cron :via-webhook))


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
       (defclass ,class-name ()
         ((type :col-type (or :text :null)
                :initarg :type
                :initform nil
                :reader get-type
                :documentation "Should be one of :added-project :via-webhook :via-cron"
                :inflate (lambda (text)
                           (make-keyword (string-upcase text)))
                :deflate #'symbol-name)
          (project :col-type project
                   :initarg :project
                   :documentation "A link to a project to be checked."
                   :reader get-project)
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
         (:table-name "check")
         (:metaclass dao-table-class))

       
       (defmethod print-object ((check ,class-name) stream)
         (print-unreadable-object (check stream :type t)
           (format stream "~A~@[ ~A~]"
                   (get-name (get-project check))
                   (when (slot-boundp check 'processed-at)
                     (get-processed-at check)))))


       (defun ,make-func-name (project)
         (check-type project project)
         (let ((type ,check-type))
           (log:info "Triggering a check for" project type)
           
           (unless (member type *allowed-check-types*)
             (let ((*print-case* :downcase))
               (error "Unable to create check of type ~S"
                      type)))

           (cond
             ((get-project-checks project :pending t)
              (log:warn "Check already exists"))
             (t (mito:create-dao ',class-name
                                 :project project
                                 :type type)))))))))


(defcheck base)
(defcheck added-project)
(defcheck via-webhook)
(defcheck via-cron)


(defmethod get-error :around (check)
  (unless (get-processed-at check)
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


(defun get-pending-checks ()
  (upgrade-types
   (select-dao 'base-check
     (where (:is-null 'processed-at)))))


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


(defun get-project-checks (project &key pending)
  (check-type project project)
  (upgrade-types
   (if pending
       (select-dao 'base-check
         (where (:and (:is-null 'processed-at)
                      (:= 'project-id (mito:object-id project)))))
       (mito:retrieve-dao 'base-check
                          :project project))))
