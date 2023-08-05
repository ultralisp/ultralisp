(uiop:define-package #:ultralisp/sources/base
  (:use #:cl))
(in-package #:ultralisp/sources/base)


(defclass base-source ()
  ((url :initarg :url
        :type string
        :reader source-url)
   (project-name :initarg :project-name
                 :type string
                 :reader project-name
                 :documentation "A name of the project corresponding to the source.")))


(defgeneric create-project (source)
  (:documentation "Creates records in the database to save information about the source."))


(defmethod print-object ((obj base-source) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "name=~S url=~S"
            (project-name obj)
            (source-url obj))))
