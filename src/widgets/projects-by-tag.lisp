(uiop:define-package #:ultralisp/widgets/projects-by-tag
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/page)
  (:import-from #:ultralisp/models/tag
                #:get-projects-by-tag
                #:get-all-tags-with-counters)
  (:import-from #:rutils
                #:fmt)
  (:import-from #:reblocks/request
                #:get-path)
  (:import-from #:cl-ppcre
                #:register-groups-bind)
  (:import-from #:ultralisp/widgets/projects
                #:render-projects-list))
(in-package #:ultralisp/widgets/projects-by-tag)


(defwidget projects-by-tag ()
  ((tag-name :initform :nil
             :accessor tag-name)
   (projects :initform :nil
             :accessor projects)))


(defun make-projects-by-tag-widget ()
  (make-instance 'projects-by-tag))


(defmethod render ((widget projects-by-tag))
  (register-groups-bind (tag-name)
      ("^/tags/(.*)/$" (get-path))

    (unless (string-equal tag-name
                          (tag-name widget))

      (setf (tag-name widget)
            tag-name
            (projects widget)
            (get-projects-by-tag tag-name)))


    (let ((title (fmt "Projects with \"~A\" tag" tag-name))
          (projects (projects widget)))
      (setf (reblocks/page:get-title)
            title)

      (with-html
        (:h1 (:a :href "/tags/"
                 "All tags")
             (:span (fmt " > ~A" tag-name)))
        (cond
          (projects (render-projects-list projects))
          (t (:p ("No projects are tagged with \"~A\" yet." tag-name))
             (:p ("[Go to this page](/github) to add your first project!"))))))))

