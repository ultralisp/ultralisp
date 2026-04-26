(defpackage #:ultralisp/widgets/spinner
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:get-html-tag
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks-ui2/widget
                #:render
                #:ui-widget
                #:get-html-tag)
  (:import-from #:reblocks-ui2/themes/tailwind
                #:tailwind-theme)
  (:export #:make-spinner
           #:spinner))
(in-package #:ultralisp/widgets/spinner)


(defwidget spinner (ui-widget)
  ())


(defmethod render ((widget spinner) (theme tailwind-theme))
  (with-html ()
    (:img :src "/static/gear.gif")))


(defmethod get-html-tag ((widget spinner) (theme tailwind-theme))
  :span)

(defun make-spinner ()
  (make-instance 'spinner))
