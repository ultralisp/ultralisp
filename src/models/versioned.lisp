(defpackage #:ultralisp/models/versioned
  (:use #:cl)
  (:import-from #:mito
                #:object-id)
  (:export
   #:object-id
   #:object-version
   #:latest-p
   #:versioned
   #:deleted-p
   #:versioned-table-class))
(in-package ultralisp/models/versioned)


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
  (let ((table-name (mito.class:table-name
                     (class-of obj)))
        (column-name "id"))
    (setf (slot-value obj 'id)
          (mito.db:last-insert-id
           mito:*connection*
           table-name
           column-name))))
