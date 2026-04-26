(uiop:define-package #:ultralisp/widgets/all-tags
  (:use #:cl)
  (:import-from #:function-cache)
  (:import-from #:reblocks/widget
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:ultralisp/models/tag
                #:get-all-tags-with-counters)
  (:import-from #:rutils
                #:fmt)
  (:import-from #:reblocks-ui2/widget
                #:render
                #:ui-widget)
  (:import-from #:reblocks-ui2/themes/tailwind
                #:tailwind-theme))
(in-package #:ultralisp/widgets/all-tags)


(defwidget all-tags (ui-widget)
  ())


(defun make-all-tags-widget ()
  (make-instance 'all-tags))


(function-cache:defcached (%cached-get-all-tags-with-counters :timeout (* 15 60)) ()
  (get-all-tags-with-counters))


(defmethod render ((widget all-tags) (theme tailwind-theme))
  (let ((all-tags (%cached-get-all-tags-with-counters)))
    (with-html ()
      (:h1 :class "text-2xl font-bold" "All tags")
      (:ul :class "list-disc pl-6"
           (loop for item in all-tags
                 for tag-name = (getf item :name)
                 for count = (getf item :count)
                 for url = (fmt "/tags/~A/" tag-name)
                 do (:li (:p (:a :href url :class "text-sky-600 hover:text-sky-700"
                                 tag-name)
                             (:sup :class "text-xs ml-1" count))))))))
