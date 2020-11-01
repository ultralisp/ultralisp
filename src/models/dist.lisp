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
   #:dist-equal))
(in-package ultralisp/models/dist)


(defclass dist (versioned)
  ((name :col-type (:text)
         :initarg :name
         :reader dist-name)
   (state :col-type (:text)
          :initarg :state
          :initform :pending
          :reader dist-state
          :inflate (lambda (text)
                     (make-keyword (string-upcase text)))
          :deflate (lambda (symbol)
                     (string-downcase (symbol-name symbol)))))
  (:unique-keys name)
  (:primary-key id version)
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
  (find-dist "common"))


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


(defun get-or-create-pending-version (dist)
  (with-transaction
    (let* ((id (object-id dist))
           (existing (mito.dao:find-dao 'dist
                                        :id id
                                        :state :pending)))
      (or existing
          (mito:create-dao 'dist
                           :id id
                           :version (1+ (object-version dist))
                           :name (dist-name dist)
                           :state :pending)))))


(defun dist-equal (left-dist right-dist)
  (and (equal (object-id left-dist)
              (object-id right-dist))
       (equal (object-version left-dist)
              (object-version right-dist))))
