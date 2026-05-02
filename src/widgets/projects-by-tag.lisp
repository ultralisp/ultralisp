(uiop:define-package #:ultralisp/widgets/projects-by-tag
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/page)
  (:import-from #:ultralisp/models/tag
                #:get-projects-by-tag
                #:get-all-tags-with-counters)
  (:import-from #:rutils
                #:fmt)
  (:import-from #:ultralisp/widgets/projects
                #:render-projects-list)
  (:import-from #:40ants-routes/route-url
                #:route-url)
  (:import-from #:reblocks-ui2/widget
                #:render
                #:ui-widget)
  (:import-from #:reblocks-ui2/themes/tailwind
                 #:tailwind-theme)
  (:import-from #:ultralisp/variables
                #:*link-color-classes*))
(in-package #:ultralisp/widgets/projects-by-tag)


(defwidget projects-by-tag (ui-widget)
  ((tag-name :initarg :tag-name
             :reader get-tag-name)
   (projects :initform nil
             :reader get-projects)))


(defun make-projects-by-tag-widget (tag)
  (let ((widget (make-instance 'projects-by-tag :tag-name tag)))
    (setf (slot-value widget 'projects)
          (get-projects-by-tag tag))
    widget))


(defmethod render ((widget projects-by-tag) (theme tailwind-theme))
  (let* ((tag-name (get-tag-name widget))
         (title (fmt "Projects with \"~A\" tag" tag-name))
         (projects (get-projects widget)))
    (setf (reblocks/page:get-title)
          title)

    (with-html ()
      (:h1 :class "text-2xl font-bold"
           (:a :href (route-url "tags") :class *link-color-classes*
               "All tags")
           (:span :class "text-gray-500" (fmt " > ~A" tag-name)))
      (cond
        (projects (render-projects-list projects))
        (t (:p ("No projects are tagged with \"~A\" yet." tag-name))
           (:p ("[Go to this page](/github) to add your first project!")))))))
