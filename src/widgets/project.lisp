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
  (:import-from #:reblocks/widget
                #:defwidget
                #:update)
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
  (:import-from #:ultralisp/models/tag
                #:get-project-tags)
  (:import-from #:str
                #:split)
  (:import-from #:40ants-routes/route-url
                #:route-url)
  (:import-from #:ultralisp/utils
                #:format-timestamp)
  (:import-from #:reblocks-ui2/widget
                #:render
                #:ui-widget)
  (:import-from #:reblocks-ui2/themes/tailwind
                #:tailwind-theme)
  (:export
   #:make-project-widget))
(in-package #:ultralisp/widgets/project)


(defwidget project (ui-widget)
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


(defun make-project-widget (author name)
  (let ((widget (make-instance 'project))
        (project-name (format nil "~A/~A" author name)))
    (setf (project-name widget) project-name)
    widget))


(defmethod (setf project-name) (new-name (widget project))
  (unless (equal (slot-value widget 'name)
                 new-name)
    (let* ((new-project (ultralisp/models/project:get-project2 new-name)))
      (unless new-project
        (page-not-found))

      (flet ((on-delete (source-widget)
               (with-slots (source-widgets) widget
                 (setf source-widgets
                       (remove source-widget
                               source-widgets)))
               (update widget)))
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
                 (update widget)))

              (slot-value widget 'tags-widget)
              (make-tags-widget new-project
                                (get-project-tags new-project)))))))


(defclass next-check ()
  ((at :initarg :at
       :reader get-time)))


(defmethod ultralisp/widgets/changelog:render-object ((obj next-check) &key timestamp)
  (with-html ()
    (:li ("~@[~A - ~]Planned check"
          (when timestamp
            (format-timestamp (get-time obj)))))))


(defun render-project (widget theme)
  (let* ((project (project widget))
         (project-name (project-name widget))
         (description (ultralisp/models/project:project-description project))
         (tags (tags-widget widget)))
    (setf (get-title)
          (fmt "~A – ~A"
               project-name
               description))

    (with-html ()
      (:h1 :class "text-2xl font-bold mb-0"
           (destructuring-bind (user-name project-name)
               (split #\/ project-name :limit 2)
             (:a :class "text-gray-500 hover:text-gray-700"
                 :href (route-url "author" :author user-name)
                 user-name)
             (:span :class "mx-1" "/")
             (:span project-name))
           (when tags
             (render tags theme)))
      (:h2 :class "text-xl text-gray-600 mb-4"
           description)

      (loop for item in (source-widgets widget)
            do (render item theme))

      (render (add-form widget) theme))))


(defmethod render ((widget project) (theme tailwind-theme))
  (let ((project (project widget)))
    (cond
      (project (render-project widget theme))
      (t (page-not-found)))))
