(defpackage #:ultralisp/protocols/moderation
  (:use #:cl)
  (:export
   #:is-moderator
   #:make-moderator
   #:moderators))
(in-package ultralisp/protocols/moderation)


(defgeneric is-moderator (user obj)
  (:documentation "Returns t if user can edit the object.")
  (:method ((user (eql nil)) (obj t))
    "Anonymous user can't be a moderator"
    nil))


(defgeneric make-moderator (user obj)
  (:documentation "Adds a user to moderators of an object.")
  (:method ((user (eql nil)) (obj t))
    (error "Unable to make anonymous user a moderator")))


(defgeneric moderators (obj)
  (:documentation "Returns all users who are moderators of an object.")
  (:method ((obj t))
    nil))
