(defpackage #:ultralisp/models/versioned
  (:use #:cl)
  (:import-from #:mito
                #:object-id)
  (:import-from #:rutils
                #:fmt)
  (:import-from #:mito.class
                #:table-name)
  (:import-from #:mito.db
                #:last-insert-id)
  (:import-from #:sxql
                #:limit
                #:order-by
                #:where)
  (:export
   #:id
   #:version
   #:object-id
   #:object-version
   #:latest-p
   #:versioned
   #:deleted-p
   #:versioned-table-class
   #:prev-version
   #:get-latest-version-of))
(in-package #:ultralisp/models/versioned)


;; (defclass versioned-table-class (mito:dao-table-class)
;;   ())


(defclass versioned ()
  ((id :col-type :bigserial
       :initarg :id
       :reader object-id)
   (version :col-type :bigint
            :initarg :version
            :reader object-version
            :initform 0)
   (latest :col-type :boolean
           :initarg :latest
           :initform nil
           :reader latest-p)
   (deleted :col-type :boolean
            :initarg :deleted
            :initform nil
            :reader deleted-p))
  (:auto-pk nil)
  ;; This primary key is not inherited and should
  ;; be defined on each class:
  ;; (:primary-key id version)
  ;; (:metaclass versioned-table-class)
  (:metaclass mito:dao-table-class))


;; We need this method to make Mito fill id column after a new
;; versioned record was created:
;; (defmethod mito.class:table-serial-key ((class versioned-table-class))
;;   'id)


(defmethod mito:insert-dao :after ((obj versioned))
  (let ((table-name (table-name
                     (class-of obj)))
        (column-name "id"))
    (unless (slot-boundp obj 'id)
      (setf (slot-value obj 'id)
            (last-insert-id
             mito:*connection*
             table-name
             column-name)))
    (mito:execute-sql (fmt "UPDATE \"~A\" SET latest = False WHERE id = ?" table-name)
                      (list (object-id obj)))
    (mito:execute-sql (fmt "UPDATE \"~A\" SET latest = True WHERE id = ? AND version = ?" table-name)
                      (list (object-id obj)
                            (object-version obj)))))



(defgeneric prev-version (obj)
  (:method ((obj versioned))
    (first
     (mito:select-dao (class-of obj)
       (where
        (:and (:= 'id (object-id obj))
              ;; There could be gaps in version numbers
              ;; that is why we need to skip to the biggest
              ;; version which less than current version.
              (:< 'version (object-version obj))))
       (order-by (:desc 'version))
       (limit 1)))))


(defun get-latest-version-of (obj)
  (first
   (mito:select-dao (class-of obj)
     (where
      (:and (:= 'id (object-id obj))
            (:= 'latest 1)))
     (limit 1))))
