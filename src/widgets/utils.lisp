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
         (label-attrs (if disabled
                          ;; For some reason, Foundation does not
                          ;; render cursor propertly on a disabled switch.
                          ;; That is why we need to set it manually here.
                          (list :style "cursor: not-allowed")
                          (list :onclick on-click))))
    (when (and labels
               (not (= (length labels)
                       2)))
      (error "Labels argument should be either nil or a list of two strings."))
    
    (with-html ()
      (:span :class "switch tiny" :title title
             (:input :class "switch-input"
                     :type "checkbox"
                     :checked state
                     :disabled disabled)
             (:label :class "switch-paddle"
                     :attrs label-attrs
                     (when labels
                       (:span :class "switch-active"
                              :aria-hidden t
                              (first labels))
                       (:span :class "switch-inactive"
                              :aria-hidden t
                              (second labels))))))))
