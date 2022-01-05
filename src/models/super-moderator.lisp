(defpackage #:ultralisp/models/super-moderator
  (:use #:cl)
  (:import-from #:reblocks-auth/models
                #:user)
  (:import-from #:function-cache
                #:defcached)
  (:export
   #:is-super-moderator-p))
(in-package ultralisp/models/super-moderator)


(defcached (%is-super-moderator-p :timeout 600) (user-id)
  (check-type user-id integer)
  (mito:retrieve-by-sql
   "SELECT 1
      FROM super_moderator
     WHERE user_id = ?"
   :binds (list user-id)))


(defun is-super-moderator-p (user)
  (check-type user user)
  (%is-super-moderator-p (mito:object-id user)))
