(defpackage #:ultralisp/models/dist
  (:use #:cl)
  (:import-from #:ultralisp/models/versioned
                #:versioned
                #:object-version)
  (:import-from #:ultralisp/models/source)
  (:import-from #:alexandria
                #:make-keyword)
  (:import-from #:ultralisp/db
                #:with-transaction)
  (:import-from #:mito
                #:object-id)
  (:export
   #:dist
   #:dist-name
   #:find-dist
   #:dist-state
   #:common-dist
   #:bound-dist
   #:enabled-p
   #:disable-reason
   #:ensure-dist
   #:get-or-create-pending-version
   #:dist-equal
   #:get-pending-dists
   #:dist-built-at
   #:get-prepared-dists))
(in-package ultralisp/models/dist)


(defclass dist (versioned)
  ((name :col-type (:text)
         :initarg :name
         :reader dist-name)
   (quicklisp-version :col-type (:text)
                      :initform ""
                      :documentation "This field is updated by :function:`ultralisp/builder::build-pending-dist`.
                                      It contains a datetime of the moment when the distribution was built."
                      :accessor dist-quicklisp-version)
   (built-at :col-type (or :timestamptz
                           :null)
             :initform nil
             :accessor dist-built-at)
   (state :col-type (:text)
          :initarg :state
          :initform :pending
          :type (member
                 ;; Initial state, in this state new versions of
                 ;; project sources can be associated with a dist:
                 :pending
                 ;; In this state no new sources can be attached
                 ;; to the dist and it's quicklisp-version
                 ;; is already generated. Now it just waits for
                 ;; metadata generation:
                 :prepared
                 ;; Metadata was generated and uploaded to the storage,
                 ;; at the moment when state becomes :ready, built-at
                 ;; slot gets filled: 
                 :ready)
          :accessor dist-state
          :inflate (lambda (text)
                     (make-keyword (string-upcase text)))
          :deflate (lambda (symbol)
                     (string-downcase (symbol-name symbol)))))
  (:unique-keys name)
  ;; It is important to use symbols from versioned package
  ;; because otherwise mito is not able to find slots in the object
  ;; when doing update-dao
  (:primary-key ultralisp/models/versioned::id
                ultralisp/models/versioned::version)
  (:metaclass mito:dao-table-class))


(defmethod print-object ((obj dist) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream
            "~A (v~A)"
            (dist-name obj)
            (object-version obj))))


(defun find-dist (name &key (raise-error t))
  (let ((result (mito.dao:find-dao 'dist
                                   :name name)))
    (when (and (null result)
               raise-error)
      (error "Unable to find dist with name \"~A\"" name))
    result))


(defun common-dist ()
  (find-dist "ultralisp"))


(defclass bound-dist ()
  ((dist :initarg :dist
         :reader dist)
   (enabled :initarg :enabled
            :reader enabled-p)
   (disable-reason :initarg :disable-reason
                   :reader disable-reason)
   (include-reason :initarg :include-reason
                   :reader include-reason)))


;; We'll define a few readers to make bound-dist work the same like usual dist does:
(defmethod dist-name ((obj bound-dist))
  (dist-name (dist obj)))

(defmethod dist-state ((obj bound-dist))
  (dist-state (dist obj)))

(defmethod object-id ((obj bound-dist))
  (object-id (dist obj)))

(defmethod object-version ((obj bound-dist))
  (object-version (dist obj)))


(defmethod print-object ((obj bound-dist) stream)
  (let ((dist (dist obj)))
    (print-unreadable-object (obj stream :type t)
      (format stream
              "~A (v~A)"
              (dist-name dist)
              (object-version dist))
      (unless (enabled-p obj)
        (format stream " disabled")))))


(defgeneric ensure-dist (value)
  (:method ((dist dist))
    dist)
  (:method ((bound bound-dist))
    (dist bound))
  (:method ((name string))
    (find-dist name)))


(defun get-next-dist-version (dist)
  "Returns a number to be the next version for the dist."
  (getf (first
         (mito:retrieve-by-sql "
     SELECT COALESCE(MAX(version) + 1, 0) as version
       FROM dist
      WHERE id = ?" :binds (list (object-id dist))))
        :version))


(defun get-or-create-pending-version (dist)
  (with-transaction
    (let* ((id (object-id dist))
           (existing (mito.dao:find-dao 'dist
                                        :id id
                                        :state :pending)))
      (or existing
          (mito:create-dao 'dist
                           :id id
                           :version (get-next-dist-version dist)
                           :name (dist-name dist)
                           :state :pending)))))


(defun get-pending-dists ()
  (mito.dao:retrieve-dao 'dist
                         :state :pending))

(defun get-prepared-dists ()
  (mito.dao:retrieve-dao 'dist
                         :state :prepared))


(defun dist-equal (left-dist right-dist)
  (and (equal (object-id left-dist)
              (object-id right-dist))
       (equal (object-version left-dist)
              (object-version right-dist))))
