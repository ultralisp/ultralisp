(defpackage #:ultralisp/widgets/projects
  (:use #:cl)
  (:import-from #:weblocks/html
                #:with-html)
  (:import-from #:ultralisp/models/project
                #:get-name
                #:get-url
                #:get-description)
  (:export
   #:render))
(in-package ultralisp/widgets/projects)


(defun render (projects)
  (with-html
    (:table :class "projects-list"
            (loop for project in projects
                  for description = (get-description project)
                  for url = (get-url project)
                  for name = (get-name project)
                  do (:tr
                      (:td :style "white-space: nowrap"
                           (:a :href url
                               name))
                      (:td description))))))

