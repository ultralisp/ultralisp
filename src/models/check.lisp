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
   #:make-check
   #:check
   #:get-project
   #:project-has-changes-p
   #:get-description
   #:check-trigger
   #:get-type
   #:get-check
   #:get-pending-checks
   #:get-processed-at
   #:make-version-from
   #:get-checks
   #:get-changelog))
(in-package ultralisp/models/check)


(defparameter *allowed-trigger-types*
  '(:manual :cron :webhook))


(defclass check ()
  ((project :col-type project
            :initarg :project
            :reader get-project)
   (processed-at :col-type (or :timestamptz :null)
                 :initarg :processed-at
                 :accessor get-processed-at)
   (project-has-changes :col-type :boolean
                        :documentation "If True, then there are some changes in a project since last version of Ultralisp dist."
                        :initform nil
                        :accessor project-has-changes-p)
   (description :col-type (or :text
                              :null)
                :initform nil
                :documentation "Stores a description of the change for the changelog."
                :accessor get-description)
   (version :col-type (or version
                          :null)
            :initform nil
            :documentation "Stores a link to a Ultralisp version triggered by this check."
            :accessor get-version))
  (:metaclass dao-table-class))


(defclass check-trigger ()
  ((type :col-type :text
         :initarg :type
         :reader get-type
         :inflate (lambda (text)
                    (make-keyword (string-upcase text)))
         :deflate #'symbol-name)
   (check :col-type check
          :initarg :check
          :reader get-check))
  (:metaclass dao-table-class))


(defmethod print-object ((check check) stream)
  (print-unreadable-object (check stream :type t)
    (format stream "~A~@[ ~A~]"
            (get-name (get-project check))
            (get-description check))))


(defun make-check (project &key (type :manual))
  (check-type project project)
  (check-type type keyword)

  (log:info "Triggering a check for" project type)
  
  (unless (member type *allowed-trigger-types*)
    (let ((*print-case* :downcase))
      (error "Parameter :type should be one of ~{~S~^, ~}."
             *allowed-trigger-types*)))

  (let* ((check (mito:find-dao 'check
                               :project project))
         (check (cond
                  (check (log:debug "Pending check found.")
                         check)
                  (t (mito:create-dao 'check
                                      :project project)))))
    (mito:create-dao 'check-trigger
                     :type type
                     :check check)
    check))


(defun make-version-from (checks)
  (let ((version (make-version)))
    (loop for check in checks
          do (setf (get-version check)
                   version)
             (save-dao check))))


(defun get-pending-checks ()
  (select-dao 'check
    (where (:is-null 'processed-at))))


(defun get-checks (version)
  (check-type version version)
  (select-dao 'check
    (where (:= :version version))))


(defun get-changelog (version)
  "Returns a string with a ChangeLog items collected from all checks, linked to the version."
  (check-type version version)
  (let ((checks (get-checks version)))
    (if checks
        (with-output-to-string (s)
          (loop for check in checks
                for project = (get-project check)
                for project-name = (get-name project)
                for description = (get-description check)
                do (format s "~&~A – ~A"
                           project-name
                           description)))
        "No changes")))

