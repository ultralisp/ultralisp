(defpackage #:ultralisp/widgets/utils
  (:use #:cl)
  (:import-from #:weblocks/actions)
  (:import-from #:weblocks/html
                #:with-html)
  (:export
   #:render-switch))
(in-package ultralisp/widgets/utils)


(defun render-switch (state action &key disabled)
  (let* ((action-code (weblocks/actions::function-or-action->action action))
         (on-click (format nil "initiateAction('~A'); return false;"
                           action-code))
         (label-attrs (if disabled
                          ;; For some reason, Foundation does not
                          ;; render cursor propertly on a disabled switch.
                          ;; That is why we need to set it manually here.
                          (list :style "cursor: not-allowed")
                          (list :onclick on-click))))
    (with-html
      (:span :class "switch tiny"
             (:input :class "switch-input"
                     :type "checkbox"
                     :checked state
                     :disabled disabled)
             (:label :class "switch-paddle"
                     :attrs label-attrs)))))
