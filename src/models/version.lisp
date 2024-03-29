(defpackage #:ultralisp/models/version
  (:use #:cl)
  (:import-from #:mito
                #:find-dao
                #:select-dao
                #:create-dao)
  (:import-from #:alexandria
                #:make-keyword)
  (:import-from #:ultralisp/utils/db
                #:deflate-keyword
                #:inflate-keyword)
  (:import-from #:sxql
                #:limit
                #:order-by
                #:where)
  (:export
   #:get-or-create-pending-version
   #:make-version
   #:version
   #:get-type
   #:get-number
   #:get-built-at
   #:get-latest-versions
   #:get-pending-version
   #:get-version-by-number
   #:make-version-number
   #:get-prepared-versions))
(in-package #:ultralisp/models/version)


(defclass version ()
  ((number :col-type (or :text :null)
           :initarg :number
           :initform nil
           :accessor get-number)
   ;; Remove null during migration
   (type :col-type (or :text :null)
         :initarg :type
         :accessor get-type
         :documentation "Should be one of :pending :prepared :ready"
         :inflate #'inflate-keyword
         :deflate #'deflate-keyword)
   (built-at :col-type (or :timestamptz
                           :null)
             :initform nil
             :accessor get-built-at))
  (:unique-keys number)
  (:metaclass mito:dao-table-class))


(defmethod print-object ((version version) stream)
  (print-unreadable-object (version stream :type t)
    (format stream "~A~@[ version=~A~]~@[ built-at=~A~]"
            (get-type version)
            (when (slot-boundp version 'number)
              (get-number version))
            (get-built-at version))))


(defun format-date (universal-time)
  (let* ((time (multiple-value-list (decode-universal-time universal-time)))
         (timestamp (reverse (subseq time 0 6))))
    (format nil "~{~2,'0d~}" timestamp)))


(defun make-version-number ()
  (format-date (get-universal-time)))


(defun get-pending-version ()
  (first
   (select-dao 'version
     (where (:= 'type "PENDING")))))


(defun get-prepared-versions ()
  (mito:retrieve-dao 'version
                     :type :prepared))


;; (defun make-version (&key (type :pending))
;;   "Creates a new version object or returns a pending version which is waiting to be built."
;;   (or (get-pending-version)
;;       (create-dao 'version
;;                   :number (make-version-number))))


(defun get-or-create-pending-version ()
  (or (get-pending-version)
      (mito:save-dao (make-instance 'version
                                    :type :pending))))


(defun get-version-by-number (number)
  (find-dao 'version
            :number number))


(defun get-latest-versions (&key (limit 10))
  (select-dao 'version
    (order-by (:desc :created-at))
    (limit limit)))

