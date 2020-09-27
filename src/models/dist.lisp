(defpackage #:ultralisp/models/dist
  (:use #:cl)
  (:import-from #:ultralisp/models/versioned
                #:versioned
                #:object-version)
  (:import-from #:ultralisp/models/dist-source)
  (:export
   #:dist
   #:dist-name
   #:find-dist
   #:dist-source->dist))
(in-package ultralisp/models/dist)


(defclass dist (versioned)
  ((name :col-type (:text)
         :initarg :name
         :reader dist-name))
  (:unique-keys name)
  (:primary-key id version)
  (:metaclass mito:dao-table-class))


(defmethod print-object ((obj dist) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream
            "~A (v~A)"
            (dist-name obj)
            (object-version obj))))


(defun find-dist (name)
  (mito.dao:find-dao 'dist
                     :name name))


(defun dist-source->dist (dist-source)
  (check-type dist-source
              ultralisp/models/dist-source:dist-source)
  (first
   (mito:retrieve-dao
    'dist
    :id (ultralisp/models/dist-source:dist-id dist-source)
    :version (ultralisp/models/dist-source:dist-version dist-source))))
