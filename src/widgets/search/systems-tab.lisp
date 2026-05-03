(uiop:define-package #:ultralisp/widgets/search/systems-tab
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
  (:import-from #:ultralisp/widgets/search/system-card
                #:make-system-card)
  (:import-from #:ultralisp/search2
                #:search-by-type)
  (:import-from #:log)
  (:export
   #:make-systems-tab))
(in-package #:ultralisp/widgets/search/systems-tab)


(defwidget systems-tab (ui-widget)
  ((query :initarg :query :reader tab-query)
   (results :initform nil :accessor tab-results)
   (total :initform 0 :accessor tab-total)
   (next-closure :initform nil :accessor tab-next-closure)))


(defun make-systems-tab (&key query)
  (let ((widget (make-instance 'systems-tab :query query)))
    (multiple-value-bind (results total next-closure)
        (search-by-type query :systems :limit 20)
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


(defmethod render ((widget systems-tab) (theme tailwind-theme))
  (with-html ()
    (dolist (item (tab-results widget))
      (render (make-system-card
               :name (getf item :|name|)
               :description (getf item :|description|)
               :project-name (getf item :|project-name|)
               :dependencies (getf item :|dependencies|))
              theme))
    (when (tab-next-closure widget)
      (render-link
       (lambda (&rest args)
         (declare (ignorable args))
         (fetch-next widget)
         (update widget))
       "Load more"))))
