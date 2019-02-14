(defpackage #:ultralisp/widgets/project
  (:use #:cl)
  (:import-from #:ultralisp/models/project
                #:get-versions
                #:enable-project
                #:disable-project
                #:get-github-project
                #:is-enabled-p)
  (:import-from #:cl-ppcre
                #:register-groups-bind)
  (:import-from #:weblocks/widget
                #:defwidget
                #:render)
  (:import-from #:weblocks/html
                #:with-html)
  (:import-from #:weblocks/page
                #:get-title)
  (:import-from #:ultralisp/widgets/not-found
                #:page-not-found)
  (:import-from #:ultralisp/widgets/utils
                #:render-switch)
  (:import-from #:mito-email-auth/weblocks
                #:get-current-user)
  (:import-from #:ultralisp/models/moderator
                #:is-moderator-p)
  (:import-from #:ultralisp/models/action
                #:get-project-actions)
  (:export
   #:make-project-widget))
(in-package ultralisp/widgets/project)


(defwidget project ()
  ())


(defun make-project-widget ()
  (make-instance 'project))


(defun render-action (action)
  (with-html
    (:li "FOO")))


(defun toggle (widget project)
  (check-type widget project)
  (check-type project ultralisp/models/project:project)
  (if (is-enabled-p project)
      (disable-project project)
      (enable-project project))
  (weblocks/widget:update widget))


(defun render-project (widget project project-name)
  (check-type project ultralisp/models/project:project)
  
  (setf (get-title)
        project-name)
  (let* ((actions (get-project-actions project))
         (versions (get-versions project))
         (current-user-is-moderator
           (is-moderator-p project
                           (get-current-user)))
         (not-moderator
           (not current-user-is-moderator))
         (changelog (sort (append actions versions)
                          #'local-time:timestamp>
                          ;; We want last actions came first
                          :key #'mito:object-updated-at)))
    (with-html
      ;; Show a list of versions where it was included
      (:h1 project-name
           (render-switch (is-enabled-p project)
                          (lambda (&rest args)
                            (declare (ignorable args))
                            (toggle widget project))
                          :disabled not-moderator
                          :title (when not-moderator
                                   "You are not a moderator of this project")))
      
      (ultralisp/widgets/changelog:render changelog
                                          :timestamps t))))


(defmethod render ((widget project))
  (register-groups-bind (user-or-org project-name)
      ("^/projects/(.*)/(.*)$" (weblocks/request:get-path))
    ;; This is not an idiomatic Weblocks code because we should
    ;; make a database query only when widget gets created, not
    ;; during the render.
    (let ((project (get-github-project user-or-org
                                       project-name)))
      (cond
        (project (render-project widget project project-name))
        (t (page-not-found))))))
