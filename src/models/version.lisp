(defpackage #:ultralisp/models/version
  (:use #:cl)
  (:import-from #:mito
                #:select-dao
                #:create-dao)
  (:export
   #:make-version
   #:version
   #:get-number
   #:get-built-at
   #:get-latest-versions))
(in-package ultralisp/models/version)


(defclass version ()
  ((number :col-type (:text)
           :initarg :number
           :reader get-number)
   (built-at :col-type (or :timestamptz
                           :null)
             :initform nil
             :accessor get-built-at))
  (:unique-keys number)
  (:metaclass mito:dao-table-class))


(defmethod print-object ((version version) stream)
  (print-unreadable-object (version stream :type t)
    (format stream "~A~@[ built-at=~A~]"
            (get-number version)
            (get-built-at version))))


(defun format-date (universal-time)
  (let* ((time (multiple-value-list (decode-universal-time universal-time)))
         (timestamp (reverse (subseq time 0 6))))
    (format nil "~{~2,'0d~}" timestamp)))


(defun make-version-number ()
  (format-date (get-universal-time)))


(defun make-version ()
  "Creates a new version object and links all given checks to it."
  (create-dao 'version
              :number (make-version-number)))


(defun get-latest-versions (&key (limit 10))
  (select-dao 'version
    (sxql:order-by (:desc :created-at))
    (sxql:limit limit)))

