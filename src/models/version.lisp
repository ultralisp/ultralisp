(defpackage #:ultralisp/models/version
  (:use #:cl)
  (:import-from #:mito
                #:create-dao)
  (:export
   #:make-version
   #:version
   #:get-number
   #:get-built-at))
(in-package ultralisp/models/version)


(defclass version ()
  ((number :col-type (:text)
           :initarg :number
           :reader get-number)
   (built-at :col-type (:timestamptz)
             :initform nil
             :reader get-built-at))
  (:unique-keys number)
  (:metaclass mito:dao-table-class))


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
