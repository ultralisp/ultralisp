(uiop:define-package #:ultralisp/widgets/tags
  (:use #:cl)
  (:import-from #:log)
  (:import-from #:ultralisp/protocols/moderation
                #:is-moderator)
  (:import-from #:reblocks-auth/models
                #:get-current-user)
  (:import-from #:reblocks/widget
                #:defwidget
                #:update)
  (:import-from #:reblocks-ui/form
                #:with-html-form)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:rutils
                #:fmt
                #:removef)
  (:import-from #:ultralisp/models/project
                #:project2)
  (:import-from #:ultralisp/models/tag
                #:remove-tags
                #:add-tags)
  (:import-from #:str
                #:trim
                #:replace-all)
  (:import-from #:reblocks/request
                #:ajax-request-p)
  (:import-from #:reblocks/actions
                #:make-js-action)
  (:import-from #:40ants-routes/route-url
                #:route-url)
  (:import-from #:reblocks-ui2/widget
                #:render
                #:ui-widget)
  (:import-from #:reblocks-ui2/themes/tailwind
                 #:tailwind-theme)
  (:import-from #:ultralisp/variables
                #:*link-color-classes*))
(in-package #:ultralisp/widgets/tags)


(defwidget tags (ui-widget)
  ((project :initarg :project
            :type project2
            :reader tags-project)
   (tags :initarg :tags
         :type list
         :reader tags-list)
   (adding-p :initform nil
             :accessor adding-p)))


(defun make-tags-widget (project tags)
  (check-type tags list)
  (check-type project project2)
  (loop for tag in tags
        do (check-type tag string))

  (make-instance 'tags
                 :project project
                 :tags tags))

(defgeneric normalize-tag (widget value)
  (:method ((widget tags) (value string))
    (string-downcase
     (replace-all "_" "-"
                  (replace-all " " "-"
                               (trim value))))))


(defun split-and-normalize (widget tags)
  (check-type tags string)
  (loop for item in (str:split "," tags)
        for tag = (normalize-tag widget item)
        unless (string= tag "")
        collect tag))


(defun toggle-adding (widget)
  (setf (adding-p widget) (not (adding-p widget)))
  (update widget))


(defmethod render ((widget tags) (theme tailwind-theme))
  (let ((reblocks/html:*pretty-html* nil)
        (editablep (is-moderator (get-current-user)
                                 (tags-project widget))))
    (labels
        ((remove-tag (&key tag &allow-other-keys)
           (cond
             (editablep
              (log:info "Removing tag" tag)
              (remove-tags (tags-project widget)
                           tag)
              (removef (slot-value widget 'tags)
                       tag
                       :test #'string=)
              (update widget))
             (t
              (log:error "Attempt to call REMOVE-TAG when tags widget is not editable"))))
         (add-new-tags (&key tags &allow-other-keys)
           (cond
             (editablep
              (let ((normalized-tags (split-and-normalize widget tags)))
                (when normalized-tags
                  (log:info "Adding tags" normalized-tags)
                  (let ((added-tags (add-tags (tags-project widget)
                                              normalized-tags)))
                    (setf (slot-value widget 'tags)
                          (sort (append (slot-value widget 'tags)
                                        added-tags)
                                #'string<))))))
             (t
              (log:error "Attempt to call ADD-NEW-TAGS when tags widget is not editable")))
           (setf (adding-p widget) nil)
           (update widget)))
      (with-html ()
        (:div :class "inline-flex items-center gap-1 text-gray-400 text-xs ml-2 relative -top-0.5 flex-wrap"
              (loop for tag in (tags-list widget)
                    do (:span :class "inline-flex items-center gap-1 mr-1 rounded-full px-2 py-0.5 bg-gray-100 hover:bg-gray-200"
                               (:a :href (route-url "tag" :tag tag)
                                   :class *link-color-classes*
                                   (fmt "#~A" tag))
                              (when editablep
                                (with-html-form (:post #'remove-tag :class "inline ml-0.5")
                                  (:input :type "hidden" :name "tag" :value tag)
                                  (:button :type "submit"
                                           :class "text-xs text-red-400 hover:text-red-600 border-none bg-transparent cursor-pointer p-0 leading-none"
                                           "✕")))))
              (when editablep
                (if (adding-p widget)
                    (let ((cancel-action (make-js-action
                                          (lambda (&rest args)
                                            (declare (ignore args))
                                            (toggle-adding widget)))))
                      (:div :class "relative inline-block"
                            (:span :class "cursor-pointer text-sky-600 hover:text-sky-700"
                                   :onclick cancel-action
                                   "−")
                            (:div :class "absolute top-full left-0 mt-1 bg-white border rounded shadow-lg p-2 z-50 whitespace-nowrap"
                                  (with-html-form (:post #'add-new-tags :class "inline-flex items-center gap-1")
                                    (:input :type "text"
                                            :name "tags"
                                            :class "border rounded px-1.5 py-0.5 text-xs w-48"
                                            :placeholder "Enter comma separated tag names"
                                            :autofocus t)
                                    (:button :type "submit"
                                             :class "text-xs text-sky-600 hover:text-sky-700 border-none bg-transparent cursor-pointer"
                                             "Add")))))
                    (let ((open-action (make-js-action
                                        (lambda (&rest args)
                                          (declare (ignore args))
                                          (toggle-adding widget)))))
                      (:span :class "cursor-pointer text-sky-600 hover:text-sky-700"
                             :onclick open-action
                             "+")))))))))


(defmethod reblocks-ui2/themes/styling:css-classes ((widget tags) (theme tailwind-theme) &key)
  (list "inline-block"))
