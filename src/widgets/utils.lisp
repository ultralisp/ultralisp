(defpackage #:ultralisp/widgets/utils
  (:use #:cl)
  (:import-from #:reblocks/actions)
  (:import-from #:reblocks/html
                #:with-html)
  (:export
   #:render-switch))
(in-package #:ultralisp/widgets/utils)


(defun render-switch (state action &key disabled labels title)
  (let* ((on-click (reblocks/actions:make-js-action action))
         (attrs (if disabled
                    (list :style "cursor: not-allowed; opacity: 0.5;")
                    (list :onclick on-click))))
    (when (and labels
               (not (= (length labels)
                       2)))
      (error "Labels argument should be either nil or a list of two strings."))

    (with-html ()
      (:label :title title
              :attrs attrs
              :class "relative inline-flex items-center cursor-pointer"
              (:input :type "checkbox"
                      :class "sr-only peer"
                      :checked state
                      :disabled disabled)
              (:div :class "w-9 h-5 bg-gray-200 peer-focus:outline-none rounded-full peer peer-checked:after:translate-x-full peer-checked:after:border-white after:content-[''] after:absolute after:top-[2px] after:left-[2px] after:bg-white after:border after:rounded-full after:h-4 after:w-4 after:transition-all peer-checked:bg-sky-600")
              (when labels
                (:span :class "ml-2 text-sm text-gray-500"
                       (if state
                           (first labels)
                           (second labels))))))))
