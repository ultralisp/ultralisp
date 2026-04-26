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
                #:form-error
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
  (:import-from #:40ants-routes/route-url
                #:route-url)
  (:import-from #:reblocks-ui2/widget
                #:render
                #:ui-widget)
  (:import-from #:reblocks-ui2/themes/tailwind
                #:tailwind-theme))
(in-package #:ultralisp/widgets/tags)


(defwidget tags (ui-widget)
  ((project :initarg :project
            :type project2
            :reader tags-project)
   (tags :initarg :tags
         :type list
         :reader tags-list)))


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

           (update widget)))
      (with-html ()
        (:div :class "inline-block text-gray-400 text-xs relative -top-0.5"
              (loop for tag in (tags-list widget)
                    do (:span :class "inline-block mr-2 rounded-full px-2 py-0.5 bg-gray-100 hover:bg-gray-200"
                              (:a :href (route-url "tag" :tag tag)
                                  :class "text-sky-600 hover:text-sky-700"
                                  (fmt "#~A" tag))
                              (when editablep
                                (:span :class "ml-1 cursor-pointer text-red-400 hover:text-red-600"
                                       (with-html-form (:post #'remove-tag)
                                         (:input :type "hidden" :name "tag" :value tag)
                                         (:button :type "submit"
                                                  :class "text-xs text-red-400 hover:text-red-600 border-none bg-transparent cursor-pointer p-0"
                                                  "✕"))))))
              (when editablep
                (with-html-form (:post #'add-new-tags)
                  (:span :class "inline-block"
                         (:input :type "text"
                                 :name "tags"
                                 :class "border rounded px-1 py-0.5 text-xs w-32"
                                 :placeholder "new tag")
                         (:button :type "submit"
                                  :class "ml-1 text-xs text-sky-600 hover:text-sky-700 border-none bg-transparent cursor-pointer"
                                  "+")))))))))
