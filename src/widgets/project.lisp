(defpackage #:ultralisp/widgets/project
  (:use #:cl)
  (:import-from #:ultralisp/widgets/changelog)
  (:import-from #:ultralisp/models/project
                #:get-description
                #:get-versions
                #:enable-project
                #:disable-project
                #:get-github-project
                #:is-enabled-p
                #:get-project2)
  (:import-from #:cl-ppcre
                #:register-groups-bind)
  (:import-from #:reblocks/widget
                #:defwidget
                #:render)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/page
                #:get-title)
  (:import-from #:ultralisp/widgets/not-found
                #:page-not-found)
  (:import-from #:ultralisp/widgets/utils
                #:render-switch)
  (:import-from #:reblocks-auth/models
                #:get-current-user)
  (:import-from #:ultralisp/protocols/moderation
                #:is-moderator)
  (:import-from #:ultralisp/models/action
                #:get-project-actions)
  (:import-from #:ultralisp/models/check
                #:get-project-checks)
  (:import-from #:ultralisp/cron
                #:get-time-of-the-next-check)
  (:import-from #:ultralisp/protocols/external-url
                #:external-url)
  (:import-from #:ultralisp/widgets/source
                #:make-add-source-widget
                #:make-source-widget)
  (:import-from #:fare-utils
                #:push-last-item!)
  (:import-from #:rutils
                #:fmt)
  (:import-from #:ultralisp/widgets/tags
                #:make-tags-widget)
  (:import-from #:reblocks-lass)
  (:import-from #:ultralisp/models/tag
                #:get-project-tags)
  (:import-from #:reblocks/request
                #:get-path)
  (:import-from #:str
                #:split)
  (:import-from #:ultralisp/utils
                #:format-timestamp)
  (:export
   #:make-project-widget))
(in-package #:ultralisp/widgets/project)


(defwidget project ()
  ((name :initform nil
         :reader project-name)
   (project :initform nil
            :reader project)
   (source-widgets :initform nil
                   :reader source-widgets)
   (add-form :initform nil
             :reader add-form)
   (tags-widget :initform nil
                :reader tags-widget))
  (:documentation "This widget will be updated with (setf (project-name widget) \"another/name\")
                   During update, sources list is changing."))


(defun make-project-widget ()
  (make-instance 'project))


(defmethod (setf project-name) (new-name (widget project))
  ;; Changes a project bound to the widget
  (unless (equal (slot-value widget 'name)
                 new-name)
    (let ((new-project (ultralisp/models/project:get-project2 new-name)))
      (unless new-project
        (page-not-found))
      
      (flet ((on-delete (source-widget)
               (with-slots (source-widgets) widget
                 (setf source-widgets
                       (remove source-widget
                               source-widgets)))
               (reblocks/widget:update widget)))
        (setf (slot-value widget 'name)
              new-name
              (slot-value widget 'project)
              new-project
              (slot-value widget 'source-widgets)
              (loop for source in (ultralisp/models/project:project-sources
                                   new-project)
                    collect (make-source-widget source
                                                :on-delete #'on-delete))
              (slot-value widget 'add-form)
              (make-add-source-widget
               new-project
               :on-new-source
               (lambda (source)
                 (uiop:appendf
                  (slot-value widget 'source-widgets)
                  (list (make-source-widget source
                                            :on-delete #'on-delete)))
                 (reblocks/widget:update widget)))

              (slot-value widget 'tags-widget)
              (make-tags-widget new-project
                                (get-project-tags new-project)
                                :editable t))))))


(defun toggle (widget project)
  (check-type widget project)
  (check-type project ultralisp/models/project:project)
  (if (is-enabled-p project)
      (disable-project project :reason :manual)
      (ultralisp/models/check:make-added-project-check project))
  (reblocks/widget:update widget))


(defclass next-check ()
  ((at :initarg :at
       :reader get-time)))


(defmethod ultralisp/widgets/changelog:render-object ((obj next-check) &key timestamp)
  (with-html
    (:li ("~@[~A - ~]Planned check"
          (when timestamp
            (format-timestamp (get-time obj)))))))


(defun render-project (widget)
  (let* ((project (project widget))
         (project-name (project-name widget))
         (description (ultralisp/models/project:project-description project))
         (tags (tags-widget widget)))
    (setf (get-title)
          (fmt "~A – ~A"
               project-name
               description))
    
    (with-html
      ;; Show a list of versions where it was included
      (:h1 :class "full-name"
           (destructuring-bind (user-name project-name)
               (split #\/ project-name :limit 2)
             (:a :class "user-name"
                 :href (fmt "/projects/~A"
                            user-name)
                 user-name)
             (:span :class "separator"
                    "/")
             (:span :class "project-name"
                    project-name))
           (when tags
             (render tags)))
      (:h2 :class "project-description"
           description)

      (loop for item in (source-widgets widget)
            do (render item))

      (render (add-form widget)))))


(defmethod render ((widget project))
  (register-groups-bind (project-name)
      ("^/projects/(.*/.*)$" (get-path))

    (setf (project-name widget)
          project-name)
    
    ;; This is not an idiomatic Reblocks code because we should
    ;; make a database query only when widget gets created, not
    ;; during the render.
    (let ((project (project widget)))
      (cond
        (project (render-project widget))
        (t (page-not-found))))))


(defmethod reblocks/dependencies:get-dependencies ((widget project))
  (append
   (list
    (reblocks-lass:make-dependency
      `((:and .widget .project)
        (.full-name
         :margin-bottom 0
         ;; (.author-name
         ;;  :color "black")
         (.project-name :margin-right 0.5em))
        (.project-description
         :font-size 1.5em)
        (.disable-reason
         :position relative
         :font-size 0.3em
         :top -0.4em))))
   (call-next-method)))
