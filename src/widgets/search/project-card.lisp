(uiop:define-package #:ultralisp/widgets/search/project-card
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
  (:import-from #:str)
  (:export
   #:make-project-card))
(in-package #:ultralisp/widgets/search/project-card)


(defwidget project-card (ui-widget)
  ((name :initarg :name :reader card-name)
   (description :initarg :description :initform "" :reader card-description)
   (tags :initarg :tags :initform nil :reader card-tags)))


(defun make-project-card (&key name description tags)
  (make-instance 'project-card :name name :description description :tags tags))


(defmethod render ((widget project-card) (theme tailwind-theme))
  (with-html ()
    (:div :class "mb-4 p-3 border rounded"
          (:div :class "flex justify-between items-start"
                (:a :href (format nil "/projects/~A" (card-name widget))
                    :class (concatenate 'string "font-bold " *link-color-classes*)
                    (card-name widget))
                (:span :class "text-xs text-gray-400" "project"))
          (when (and (card-description widget)
                     (not (str:emptyp (card-description widget))))
            (:p :class "text-sm text-gray-600 mt-1"
                (card-description widget)))
          (when (card-tags widget)
            (:div :class "mt-1"
                  (dolist (tag (card-tags widget))
                    (:span :class "text-xs bg-gray-100 rounded px-2 py-0.5 mr-1"
                           tag)))))))
