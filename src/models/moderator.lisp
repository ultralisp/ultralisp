(defpackage #:ultralisp/models/moderator
  (:use #:cl)
  (:import-from #:reblocks-auth/models
                #:user
                #:get-email
                #:get-nickname)
  (:import-from #:ultralisp/models/project
                #:project)
  (:import-from #:mito
                #:dao-table-class)
  (:import-from #:log)
  (:export #:moderator
           #:is-moderator-p
           #:get-moderators
           #:get-projects
           #:make-moderator))
(in-package #:ultralisp/models/moderator)


(defclass moderator ()
  ((user :col-type user
         :initarg :user
         :reader get-user)
   (project :col-type project
            :initarg :project
            :reader get-project))
  (:metaclass dao-table-class)
  (:auto-pk nil))


(defmethod print-object ((obj moderator) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "user=~A project=~A"
            (get-email
             (get-user obj))
            (ultralisp/models/project:get-name
             (get-project obj)))))


(defun make-moderator (project user)
  (check-type project project)
  (check-type user user)
  (log:info "Making user a moderator" project user)
  (mito:create-dao 'moderator
                   :project project
                   :user user))


(defun is-moderator-p (project user)
  "Returns `t' if user is project's moderator."
  (check-type project project)
  (check-type user (or null user))

  (when (and user
             (or (mito:find-dao 'moderator
                                :project project
                                :user user)
                 (string-equal (get-nickname user)
                               "svetlyak.40wt@gmail.com")))
    t))


(defun get-moderators (project)
  "Returns a list of project's moderator."
  (check-type project project)
  
  (mapcar #'get-user
          (mito:retrieve-dao 'moderator
                             :project project)))

(defun get-projects (user)
  "Returns a list of project's moderator."
  (check-type user user)
  
  (mapcar #'get-project
          (mito:retrieve-dao 'moderator
                             :user user)))
