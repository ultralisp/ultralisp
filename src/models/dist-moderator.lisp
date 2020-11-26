(defpackage #:ultralisp/models/dist-moderator
  (:use #:cl)
  (:import-from #:mito)
  (:import-from #:weblocks-auth/models
                #:user)
  (:import-from #:ultralisp/models/dist
                #:dist)
  (:import-from #:ultralisp/db
                #:with-transaction)
  (:export
   #:dist-moderator
   #:dist-id
   #:user-id
   #:moderated-dists
   #:add-dist))
(in-package ultralisp/models/dist-moderator)


(defclass dist-moderator ()
  ((dist-id :col-type :bigint
            :initarg :dist-id
            :reader dist-id
            :references (dist id))
   (user-id :col-type :bigint
            :initarg :user-id
            :reader user-id
            :references (user id)))
  (:primary-key dist-id user-id)
  (:metaclass mito:dao-table-class))


(defun moderated-dists (user)
  (check-type user weblocks-auth/models:user)
  (mito.dao:select-by-sql
   'dist
   "SELECT dist.* FROM dist
      JOIN dist_moderator ON dist.id = dist_moderator.dist_id
     WHERE dist_moderator.user_id = ?
      AND dist.latest = 'true'"
   :binds (list (mito:object-id user))))


(defun add-dist (user name)
  (check-type user weblocks-auth/models:user)
  (check-type name string)
  (with-transaction
    ;; We aren't using unique db index, because there will be many
    ;; versions of the same dist with the same name.
    ;; That is why we only check for duplicate name only
    ;; when adding a new dist.
    (when (ultralisp/models/dist:find-dist name :raise-error nil)
      (error "Dist with name \"~A\" already exists"
             name))
    (let ((dist (mito:create-dao 'dist
                                 :name name
                                 ;; There is no any projects
                                 ;; bound to this dist yet.
                                 ;; So we may consider it is
                                 ;; ready:
                                 ;; :state :ready
                                 )))
      (mito:create-dao 'dist-moderator
                       :dist-id (mito:object-id dist)
                       :user-id (mito:object-id user))
      dist)))
