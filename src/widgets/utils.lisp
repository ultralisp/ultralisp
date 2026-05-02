(defpackage #:ultralisp/widgets/utils
  (:use #:cl)
  (:import-from #:reblocks/actions)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:str
                #:concat)
  (:export
   #:render-switch
   #:large-header
   #:small-header))
(in-package #:ultralisp/widgets/utils)


(defun large-header (text &key extra-classes)
  (with-html ()
    (:h3 :class (str:concat "text-2xl text-slate-700 font-bold mt-6 mb-4"
                            (when extra-classes
                              (str:concat " " extra-classes)))
         text)))


(defun small-header (text &key extra-classes)
  (with-html ()
    (:h3 :class (str:concat "text-lg text-slate-700 font-semibold mt-6 mb-4"
                            (when extra-classes
                              (str:concat " " extra-classes)))
         text)))


(defun render-switch (state action &key disabled labels title)
  (let* ((on-click (reblocks/actions:make-js-action action))
         (attrs (cond
                  (disabled
                   (list :style "cursor: not-allowed; opacity: 0.5;"))
                  (t
                   (list :onclick on-click)))))
    (when (and labels
               (not (= (length labels)
                       2)))
      (error "Labels argument should be either nil or a list of two strings."))

    (with-html ()
      (:div :class "relative top-0.5" ;; to shift checkbox a little bit down and make it inline with text
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
                          (second labels)))))))))
