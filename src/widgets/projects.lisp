(defpackage #:ultralisp/widgets/projects
  (:use #:cl)
  (:import-from #:reblocks/page)
  (:import-from #:ultralisp/widgets/not-found
                #:page-not-found)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:ultralisp/models/project
                #:get-url
                #:get-name
                #:get-description)
  (:import-from #:reblocks/widget
                #:defwidget)
  (:import-from #:reblocks-auth/models
                #:get-current-user)
  (:import-from #:ultralisp/models/moderator)
  (:import-from #:ultralisp/models/project-moderator
                #:user->projects)
  (:import-from #:reblocks-ui2/widget
                #:render
                #:ui-widget)
  (:import-from #:reblocks-ui2/themes/tailwind
                 #:tailwind-theme)
  (:import-from #:ultralisp/variables
                #:*link-color-classes*)
  (:export
   #:render
   #:render-projects-list
   #:make-author-projects-widget
   #:make-my-projects-widget))
(in-package #:ultralisp/widgets/projects)


(defwidget author-projects (ui-widget)
  ((user-or-org :initarg :user-or-org
                :reader get-user-or-org)))


(defwidget my-projects (ui-widget)
  ())


(defun make-author-projects-widget (author)
  (make-instance 'author-projects :user-or-org author))

(defun make-my-projects-widget ()
  (make-instance 'my-projects))


(defun render-projects-list (projects)
  (with-html ()
    (:table :class "w-full"
            (:tbody
             (loop for project in projects
                   for description = (ultralisp/models/project:project-description project)
                   for url = (ultralisp/protocols/url:url project)
                   for name = (ultralisp/models/project:project-name project)
                   do (:tr
                       (:td :class "whitespace-nowrap pr-4"
                             (:a :href url :class *link-color-classes*
                                 name))
                       (:td description)))))))


(defmethod render ((widget author-projects) (theme tailwind-theme))
  (let ((user-or-org (get-user-or-org widget)))
    (let ((projects (ultralisp/models/project:get-projects2-by-username user-or-org))
          (title (format nil "All projects of ~A" user-or-org)))
      (cond
        (projects
         (with-html ()
           (:h1 :class "text-2xl font-bold"
                title)
           (setf (reblocks/page:get-title)
                 title)
           (render-projects-list projects)))
        (t (page-not-found))))))


(defmethod render ((widget my-projects) (theme tailwind-theme))
  (let* ((user (get-current-user))
         (projects (sort (user->projects user)
                         #'string<
                         :key #'ultralisp/models/project:project-name))
         (title "Moderated projects"))

    (setf (reblocks/page:get-title)
          title)

    (with-html ()
      (:h1 :class "text-2xl font-bold"
           title)
      (cond
        (projects (render-projects-list projects))
        (t (:p "You don't have any projects yet.")
           (:p ("[Go to this page](/github) to add your first project!")))))))
