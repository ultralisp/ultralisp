(defpackage #:ultralisp/widgets/project-badge
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:defwidget
                #:render)
  (:import-from #:reblocks/request
                #:get-path)
  (:import-from #:reblocks/response
                #:immediate-response)
  (:import-from #:ultralisp/badges
                #:badge-svg)
  (:export #:make-project-badge-widget))

(in-package #:ultralisp/widgets/project-badge)


(defwidget project-badge ()
  ())


(defun make-project-badge-widget ()
  (make-instance 'project-badge))


(defmethod render ((widget project-badge))
  (declare (ignore widget))

  (let* ((path (get-path))
         (project (subseq path (length "/projects/")
                          (- (length path)
                              (length ".svg")))))
    (immediate-response
     (badge-svg project)
     :content-type "image/svg+xml")))
