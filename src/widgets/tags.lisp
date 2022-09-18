(uiop:define-package #:ultralisp/widgets/tags
  (:use #:cl)
  (:import-from #:reblocks-lass)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
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
                #:replace-all))
(in-package #:ultralisp/widgets/tags)


(defwidget tags ()
  ((project :initarg :project
            :type project2
            :reader tags-project)
   (tags :initarg :tags
         :type list
         :reader tags-list)
   (editable :initarg :editable
             :initform nil
             :reader editablep)))


(defun make-tags-widget (project tags &key editable)
  (check-type tags list)
  (check-type project project2)
  (loop for tag in tags
        do (check-type tag string))
  
  (make-instance 'tags
                 :project project
                 :tags tags
                 :editable editable))

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


(defmethod render ((widget tags))
  (let ((reblocks/html:*pretty-html* nil))
    (labels
        ((remove-tag (&key tag &allow-other-keys)
           (log:info "Removing tag" tag)
           (remove-tags (tags-project widget)
                        tag)
           (removef (slot-value widget 'tags)
                    tag
                    :test #'string=)
           (reblocks/widget:update widget)
           (reinitialize))
         (add-new-tags (&key tags &allow-other-keys)
           (let ((normalized-tags (split-and-normalize widget tags)))
             (when normalized-tags
               (log:info "Adding tags" normalized-tags)
               (let ((added-tags (add-tags (tags-project widget)
                                           normalized-tags)))
                 (setf (slot-value widget 'tags)
                       (sort (append (slot-value widget 'tags)
                                     added-tags)
                             #'string<)))))
             
           (reblocks/widget:update widget)
           (reinitialize))
         (reinitialize ()
           ;; And we need reinitialize Zurb because
           ;; otherwise the second click on +
           ;; will not open popup.
           (when (reblocks/request:ajax-request-p)
             (reblocks/response:send-script
              '(parenscript:chain
                (j-query document)
                (foundation))))))
      (with-html
        (loop for tag in (tags-list widget)
              do (:span :class "tag"
                        (:a :href (fmt "/tags/~A/" tag)
                            (fmt "#~A" tag))
                        (when (editablep widget)
                          (:span :class "delete"
                                 (with-html-form (:post #'remove-tag)
                                   (:input :type "hidden" :name "tag" :value tag)
                                   (reblocks-ui/form:render-button "✕" :class "button alert"))))))
        (when (editablep widget)
          (let* ((popup-id (symbol-name (gensym "NEW-TAG-POPUP"))))
            (:div :id popup-id
                  :class "reveal small"
                  :data-reveal "true"
                  (with-html-form (:post #'add-new-tags)
                    (:h3 "Enter comma separated tag names:")
                    (:input :type "text"
                            :name "tags")
                    (:input :type "submit"
                            :class "button"
                            :data-close "")))
           
            (:a :data-open popup-id
                "+")))))))


(defmethod get-dependencies ((widget tags))
  (append
   (list
    (reblocks-lass:make-dependency
      `((:and .widget .tags)
        :display "inline-block"
        :color "#808080"
        :font-size 0.5em
        :position "relative"
        :top -0.2em
        (.tag
         :margin-right 10px
         :border-radius 10px
         (.delete :display none))
        ((:and .tag :hover)
         (.delete
          :display inline-block
          :position absolute
          :top -0.4em
          (.button :border-radius 2em
                   :font-size 0.3em))))))
   (call-next-method)))
