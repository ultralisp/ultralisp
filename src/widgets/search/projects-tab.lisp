(uiop:define-package #:ultralisp/widgets/search/projects-tab
  (:use #:cl)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/widget
                #:defwidget
                #:update)
  (:import-from #:reblocks-ui2/widget
                #:render
                #:ui-widget)
  (:import-from #:reblocks-ui2/themes/tailwind
                #:tailwind-theme)
  (:import-from #:reblocks-ui/form
                #:render-link)
  (:import-from #:ultralisp/widgets/search/project-card
                #:make-project-card)
  (:import-from #:ultralisp/search2
                #:search-by-type)
  (:import-from #:log)
  (:export
   #:make-projects-tab))
(in-package #:ultralisp/widgets/search/projects-tab)


(defwidget projects-tab (ui-widget)
  ((query :initarg :query :reader tab-query)
   (results :initform nil :accessor tab-results)
   (total :initform 0 :accessor tab-total)
   (next-closure :initform nil :accessor tab-next-closure)))


(defun make-projects-tab (&key query)
  (let ((widget (make-instance 'projects-tab :query query)))
    (multiple-value-bind (results total next-closure)
        (search-by-type query :projects :limit 20)
      (setf (tab-results widget) results
            (tab-total widget) total
            (tab-next-closure widget) next-closure))
    widget))


(defun fetch-next (widget)
  (let ((closure (tab-next-closure widget)))
    (when closure
      (multiple-value-bind (results total next-closure)
          (funcall closure)
        (setf (tab-results widget)
              (append (tab-results widget) results)
              (tab-total widget) total
              (tab-next-closure widget) next-closure)))))


(defmethod render ((widget projects-tab) (theme tailwind-theme))
  (with-html ()
    (dolist (item (tab-results widget))
      (render (make-project-card
               :name (getf item :|name|)
               :description (getf item :|description|)
               :tags (getf item :|tags|))
              theme))
    (when (tab-next-closure widget)
      (render-link
       (lambda (&rest args)
         (declare (ignorable args))
         (fetch-next widget)
         (update widget))
       "Load more"))))
