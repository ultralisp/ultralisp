(defpackage #:ultralisp/protocols/external-url
  (:use #:cl)
  (:export
   #:external-url))
(in-package #:ultralisp/protocols/external-url)


(defgeneric external-url (obj)
  (:documentation "Return HTTP URL to the origin of the project, source or other entity."))
