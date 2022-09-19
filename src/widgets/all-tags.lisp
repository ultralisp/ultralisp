(uiop:define-package #:ultralisp/widgets/all-tags
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:ultralisp/models/tag
                #:get-all-tags-with-counters)
  (:import-from #:rutils
                #:fmt))
(in-package #:ultralisp/widgets/all-tags)


(defwidget all-tags ()
  ())


(defun make-all-tags-widget ()
  (make-instance 'all-tags))


(defmethod render ((widget all-tags))
  (let ((all-tags (get-all-tags-with-counters)))
    (with-html
      (:h1 "All tags")
      (:ul
       (loop for item in all-tags
             for tag-name = (getf item :name)
             for count = (getf item :count)
             for url = (fmt "/tags/~A/" tag-name)
             do (:li (:p (:a :href url
                             tag-name)
                         (:sup count))))))))
