(defpackage #:ultralisp/models/dist-moderator
  (:use #:cl)
  (:import-from #:mito)
  (:import-from #:weblocks-auth/models
                #:user)
  (:import-from #:ultralisp/models/dist
                #:dist)
  (:export
   #:dist-moderator
   #:dist-id
   #:user-id))
(in-package ultralisp/models/dist-moderator)


(defclass dist-moderator ()
  ((dist-id :col-type :bigint
            :initarg :dist-id
            :reader dist-id
            :references (dist id))
   (user-id :col-type :bigint
            :initarg :user-id
            :reader user-id
            :references user))
  (:primary-key dist-id user-id)
  (:metaclass mito:dao-table-class))
