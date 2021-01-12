(defpackage #:ultralisp/widgets/projects
  (:use #:cl)
  (:import-from #:weblocks/request)
  (:import-from #:weblocks/page)
  (:import-from #:ultralisp/widgets/not-found
                #:page-not-found)
  (:import-from #:weblocks/html
                #:with-html)
  (:import-from #:ultralisp/models/project
                #:get-url
                #:get-name
                #:get-description)
  (:import-from #:weblocks/widget
                #:render
                #:defwidget)
  (:import-from #:cl-ppcre
                #:register-groups-bind)
  (:import-from #:weblocks-auth/models
                #:get-current-user)
  (:import-from #:ultralisp/models/moderator)
  (:import-from #:ultralisp/models/project-moderator
                #:user->projects)
  (:export
   #:render
   #:render-projects-list
   #:make-author-projects-widget
   #:make-my-projects-widget))
(in-package ultralisp/widgets/projects)


(defwidget author-projects ()
  ())


(defwidget my-projects ()
  ())


(defun make-author-projects-widget ()
  (make-instance 'author-projects))

(defun make-my-projects-widget ()
  (make-instance 'my-projects))


(defun render-projects-list (projects)
  (with-html
    (:table :class "projects-list"
            (loop for project in projects
                  for description = (ultralisp/models/project:project-description project)
                  for url = (ultralisp/protocols/url:url project)
                  for name = (ultralisp/models/project:project-name project)
                  do (:tr
                      (:td :style "white-space: nowrap"
                           (:a :href url
                               name))
                      (:td description))))))


(defmethod render ((widget author-projects))
  (register-groups-bind (user-or-org)
      ("^/projects/(.*)$" (weblocks/request:get-path))
    ;; This is not an idiomatic Weblocks code because we should
    ;; make a database query only when widget gets created, not
    ;; during the render.
    (let ((projects (ultralisp/models/project:get-projects2-by-username user-or-org))
          (title (format nil "All projects of ~A" user-or-org)))
      (cond
        (projects
         (with-html
           (:h1 :class "author-name"
                title)
           (setf (weblocks/page:get-title)
                 title)
           (render-projects-list projects)))
        (t (page-not-found))))))


(defmethod render ((widget my-projects))
  (let* ((user (get-current-user))
         (projects (sort (user->projects user)
                         #'string<
                         :key #'ultralisp/models/project:project-name))
         (title "Moderated projects"))

    (setf (weblocks/page:get-title)
          title)

    (with-html
      (:h1 :class "author-name"
           title)
      (cond
        (projects (render-projects-list projects))
        (t (:p "You don't have any projects yet.")
           (:p ("[Go to this page](/github) to add your first project!")))))))
