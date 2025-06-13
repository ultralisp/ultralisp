(defpackage #:ultralisp/widgets/spinner
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:get-html-tag
                #:render
                #:defwidget)
  (:import-from #:reblocks/dependencies
                #:make-dependency
                #:get-dependencies)
  (:import-from #:reblocks/html
                #:with-html)
  (:export #:make-spinner
           #:spinner))
(in-package #:ultralisp/widgets/spinner)


(defwidget spinner ()
  ())


(defparameter *dependencies*
  nil)


(defmethod get-dependencies ((widget spinner))
  (append (call-next-method)
          *dependencies*))


(defmethod render ((widget spinner))
  (with-html ()
    (:img :src "/static/gear.gif")))


(defmethod get-html-tag ((widget spinner))
  :span)

(defun make-spinner ()
  (make-instance 'spinner))
