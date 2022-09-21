(defpackage #:ultralisp/protocols/render-changes
  (:use #:cl)
  (:import-from #:ultralisp/models/source)
  (:export
   #:render
   #:render-changes))
(in-package #:ultralisp/protocols/render-changes)


(defgeneric render (source-type prev-source new-source)
  (:documentation "
      This method should be implemented for every source type.
      The source-type is a keyworkd, like :github.
"))


(defun render-changes (prev-source new-source)
  (let ((type (ultralisp/models/source:source-type new-source)))
    (render type prev-source new-source)))
