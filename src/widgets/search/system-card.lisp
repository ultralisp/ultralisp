(uiop:define-package #:ultralisp/widgets/search/system-card
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
   #:make-system-card))
(in-package #:ultralisp/widgets/search/system-card)


(defwidget system-card (ui-widget)
  ((name :initarg :name :reader card-name)
   (description :initarg :description :initform "" :reader card-description)
   (project-name :initarg :project-name :initform nil :reader card-project-name)
   (dependencies :initarg :dependencies :initform nil :reader card-dependencies)))


(defun make-system-card (&key name description project-name dependencies)
  (make-instance 'system-card
                 :name name :description description
                 :project-name project-name :dependencies dependencies))


(defmethod render ((widget system-card) (theme tailwind-theme))
  (with-html ()
    (:div :class "mb-4 p-3 border rounded"
          (:div :class "flex justify-between items-start"
                (:span :class "font-bold"
                       (card-name widget))
                (:span :class "text-xs text-gray-400" "system"))
          (when (and (card-description widget)
                     (not (str:emptyp (card-description widget))))
            (:p :class "text-sm text-gray-600 mt-1"
                (card-description widget)))
          (when (card-project-name widget)
            (:div :class "text-xs mt-1"
                  "from "
                  (:a :href (format nil "/projects/~A" (card-project-name widget))
                      :class *link-color-classes*
                      (card-project-name widget))))
          (when (card-dependencies widget)
            (:div :class "text-xs mt-1 text-gray-500"
                  "deps: "
                  (format nil "~{~A~^, ~}" (card-dependencies widget)))))))
