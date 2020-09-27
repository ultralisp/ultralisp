(defpackage #:ultralisp/protocols/moderation
  (:use #:cl)
  (:export
   #:is-moderator))
(in-package ultralisp/protocols/moderation)


(defgeneric is-moderator (user obj)
  (:documentation "Returns t if user can edit the object.")
  (:method ((user t) (obj (eql nil)))
    "Anonymous user can be a moderator"
    nil))
