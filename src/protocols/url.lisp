(defpackage #:ultralisp/protocols/url
  (:use #:cl)
  (:export #:url))
(in-package #:ultralisp/protocols/url)


(defgeneric url (obj)
  (:documentation "Returns internal URL to the project, source or other entity."))
