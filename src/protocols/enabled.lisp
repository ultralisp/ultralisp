(defpackage #:ultralisp/protocols/enabled
  (:use #:cl)
  (:export
   #:enabled-p))
(in-package ultralisp/protocols/enabled)


(defgeneric enabled-p (obj)
  (:documentation "This method should return `t` or `nil` for objects which can be enabled or disabled.

                   Usually it can be applied to wrappers which bind source and dist or dist and source. "))
