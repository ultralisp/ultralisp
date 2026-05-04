(uiop:define-package #:ultralisp/widgets/search/symbol-card
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
  (:import-from #:ultralisp/variables
                #:*link-color-classes*)
  (:import-from #:3bmd)
  (:import-from #:cl-ppcre
                #:regex-replace-all)
  (:import-from #:rutils
                #:fmt)
  (:export
   #:make-symbol-card))
(in-package #:ultralisp/widgets/search/symbol-card)


(defwidget symbol-card (ui-widget)
  ((type :initarg :type :reader card-type)
   (symbol :initarg :symbol :reader card-symbol)
   (doc :initarg :doc :reader card-doc)
   (arguments :initarg :arguments :initform nil :reader card-arguments)
   (project :initarg :project :initform nil :reader card-project)
   (system :initarg :system :initform nil :reader card-system)
   (package :initarg :package :initform nil :reader card-package)
   (original-package :initarg :original-package :initform nil :reader card-original-package)))


(defun make-symbol-card (&key type symbol doc arguments project system package original-package)
  (make-instance 'symbol-card
                 :type type :symbol symbol :doc doc
                 :arguments arguments :project project
                 :system system :package package
                 :original-package original-package))


(defun to-html (text)
  (let ((replaced (regex-replace-all "`([^`]+?)'" text "`\\1`")))
    (with-output-to-string (s)
      (3bmd:parse-string-and-print-to-stream replaced s))))


(defmethod render ((widget symbol-card) (theme tailwind-theme))
  (with-html ()
    (:div :class "mb-4 p-3 border rounded"
          (:div :class "flex justify-between items-start"
                (:span :class "font-bold"
                       ("~:@(~A:~A~)" (card-package widget) (card-symbol widget)))
                (:span :class "text-xs text-gray-400" (card-type widget)))
          (when (card-arguments widget)
            (:span :class "text-gray-500" (card-arguments widget)))
          (:div :class "mt-1"
                (:raw (to-html (card-doc widget))))
          (when (card-project widget)
            (:div :class "text-xs inline-block"
                  (:label "project:")
                  (:a :href (fmt "/projects/~A" (card-project widget))
                      :class *link-color-classes*
                      (card-project widget))))
          (when (card-system widget)
            (:div :class "text-xs inline-block ml-2"
                  (:label "system:")
                  (:span (card-system widget))))
          (when (card-original-package widget)
            (:div :class "text-xs inline-block ml-2"
                  (:label "original-package:")
                  (:span (card-original-package widget)))))))
