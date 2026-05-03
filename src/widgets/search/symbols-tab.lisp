(uiop:define-package #:ultralisp/widgets/search/symbols-tab
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
  (:import-from #:ultralisp/widgets/search/symbol-card
                #:make-symbol-card)
  (:import-from #:ultralisp/search2
                #:search-by-type)
  (:import-from #:log)
  (:export
   #:make-symbols-tab))
(in-package #:ultralisp/widgets/search/symbols-tab)


(defwidget symbols-tab (ui-widget)
  ((query :initarg :query :reader tab-query)
   (dist :initarg :dist :reader tab-dist)
   (results :initform nil :accessor tab-results)
   (total :initform 0 :accessor tab-total)
   (next-closure :initform nil :accessor tab-next-closure)))


(defun make-symbols-tab (&key query (dist "default"))
  (let ((widget (make-instance 'symbols-tab :query query :dist dist)))
    (multiple-value-bind (results total next-closure)
        (search-by-type query :symbols :limit 20 :dist dist)
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


(defmethod render ((widget symbols-tab) (theme tailwind-theme))
  (with-html ()
    (:ul :class "pl-0"
         (dolist (item (tab-results widget))
           (render (make-symbol-card
                    :type (getf item :|type|)
                    :symbol (getf item :|symbol|)
                    :doc (or (getf item :|documentation|) "")
                    :arguments (getf item :|arguments|)
                    :project (getf item :|project|)
                    :system (getf item :|system|)
                    :package (getf item :|package|)
                    :original-package (getf item :|original-package|))
                 theme)))
    (when (tab-next-closure widget)
      (render-link
       (lambda (&rest args)
         (declare (ignorable args))
         (fetch-next widget)
         (update widget))
       "Load more"))))
