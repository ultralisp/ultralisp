(defpackage #:ultralisp/models/moderator
  (:use #:cl)
  (:import-from #:ultralisp/models/user
                #:user)
  (:import-from #:ultralisp/models/project
                #:project)
  (:import-from #:mito
                #:dao-table-class)
  (:export #:moderator))
(in-package ultralisp/models/moderator)


(defclass moderator ()
  ((user :col-type user
         :initarg :user)
   (project :col-type project
            :initarg :project))
  (:metaclass dao-table-class)
  (:auto-pk nil))


(defun make-moderator (project user)
  (check-type project project)
  (check-type user user)
  (log:info "Making user a moderator" project user)
  (mito:create-dao 'moderator
                   :project project
                   :user user))
