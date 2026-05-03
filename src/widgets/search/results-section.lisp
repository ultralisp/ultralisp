(uiop:define-package #:ultralisp/widgets/search/results-section
  (:use #:cl)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/widget
                #:defwidget)
  (:import-from #:reblocks-ui2/widget
                #:render
                #:ui-widget)
  (:import-from #:reblocks-ui2/themes/tailwind
                #:tailwind-theme)
  (:import-from #:ultralisp/variables
                #:*link-color-classes*)
  (:export
   #:make-results-section))
(in-package #:ultralisp/widgets/search/results-section)


(defwidget results-section (ui-widget)
  ((title :initarg :title :reader section-title)
   (total :initarg :total :reader section-total)
   (cards :initarg :cards :reader section-cards)
   (tab-name :initarg :tab-name :reader section-tab-name)
   (query :initarg :query :reader section-query)))


(defun make-results-section (&key title total cards tab-name query)
  (make-instance 'results-section
                 :title title :total total
                 :cards cards :tab-name tab-name
                 :query query))


(defmethod render ((widget results-section) (theme tailwind-theme))
  (with-html ()
    (:div :class "mb-6"
          (:h3 :class "text-lg font-semibold mb-2"
               ("~A (~A)" (section-title widget) (section-total widget)))
          (dolist (card (section-cards widget))
            (render card theme))
          (when (> (section-total widget) (length (section-cards widget)))
            (:p :class "text-sm"
                (:a :href (format nil "/search/?query=~A&tab=~A"
                                  (section-query widget)
                                  (section-tab-name widget))
                    :class *link-color-classes*
                    ("All ~A results" (section-total widget))))))))
