(defpackage #:ultralisp/widgets/source
  (:use #:cl)
  (:import-from #:parenscript
                #:@)
  (:import-from #:ultralisp/protocols/render-changes)
  (:import-from #:weblocks/widget
                #:defwidget
                #:render)
  (:import-from #:weblocks-lass)
  (:import-from #:weblocks/dependencies)
  (:import-from #:ultralisp/models/dist-source
                #:update-source-dists
                #:source-distributions)
  (:import-from #:ultralisp/models/dist-moderator)
  (:import-from #:ultralisp/protocols/external-url
                #:external-url)
  (:import-from #:ultralisp/protocols/url)
  (:import-from #:rutils
                #:fmt)
  (:import-from #:weblocks/html
                #:with-html)
  (:import-from #:ultralisp/models/dist
                #:dist-name)
  (:import-from #:ultralisp/models/dist-source
                #:source->dists
                #:dist-source->dist)
  (:import-from #:ultralisp/models/check
                #:get-last-source-check
                #:source-checks)
  (:import-from #:ultralisp/utils
                #:make-keyword)
  (:import-from #:ultralisp/models/source
                #:params-from-github)
  (:import-from #:ultralisp/utils/github
                #:get-branches)
  (:import-from #:ultralisp/protocols/enabled
                #:enabled-p)
  (:export
   #:make-source-widget
   #:make-add-source-widget))
(in-package ultralisp/widgets/source)


(defwidget source-widget ()
  ((source :initarg :source
           :accessor source)
   (subwidget :initform nil
              :accessor subwidget)))


(defwidget readonly-source-widget ()
  ((parent :initarg :parent
           :type source-widget
           :reader parent)
   (last-check :initarg :check
               :initform nil
               :type (or ultralisp/models/check:check2
                         null)
               :reader last-check)))


(defwidget branch-select-widget ()
  ((branches :initarg :branches
             :reader branches)
   (current :initarg :current
            :reader current-branch)))


(defun make-branch-select-widget (github-url &optional (current "main"))
  (multiple-value-bind (branches default-branch)
      (get-branches github-url)
    (make-instance 'branch-select-widget
                   :branches branches
                   :current (or current
                                default-branch))))


(defun update-url (widget url)
  (check-type widget branch-select-widget)
  (check-type url string)
  
  (multiple-value-bind (branches default-branch)
      (get-branches url)
    (setf (slot-value widget 'branches)
          branches
          (slot-value widget 'current)
          default-branch)
    (weblocks/widget:update widget)))


(defwidget edit-source-widget ()
  ((parent :initarg :parent
           :type source-widget
           :reader parent)
   (branches :initarg :branches
             :reader branches)))


(defwidget add-source-widget ()
  ((project :initarg :project
            :reader project)
   (on-new-source :initform nil
                  :initarg :on-new-source
                  :reader on-new-source
                  :documentation "
                      A function of one argument.
                      Will be called with a new source.
                  ")))


(defun edit (widget)
  (check-type widget readonly-source-widget)
  (log:debug "Switching to the edit widget")
  (let* ((main (parent widget))
         (source (source main))
         ;; Где-то здесь надо воткнуть получение списка полей
         (subwidget (make-instance 'edit-source-widget
                                   :parent main
                                   :branches (make-branch-select-widget
                                              (external-url source)
                                              (ultralisp/models/source:get-current-branch source)))))
    (setf (slot-value main 'subwidget)
          subwidget)
    (weblocks/widget:update main)))


(defun switch-to-readonly (widget)
  (check-type widget edit-source-widget)

  (log:debug "Switching to the view widget")
  
  (let* ((parent (parent widget))
         (source (source parent))
         (subwidget (make-instance 'readonly-source-widget
                                   :parent parent
                                   :check (get-last-source-check source))))

    (setf (slot-value parent 'subwidget)
          subwidget)
    (weblocks/widget:update parent)))


;; Here is the flow of working with a source.
;; Each source type has to define it's own form for read-only
;; rendering, editing and a method for saving results.
;; When results are saved, there is a common part of
;; distribution update and a custom part of fields update.

(defun save (widget args)
  (check-type widget edit-source-widget)

  (let* ((parent (parent widget)))
    (loop with url = (getf args :url)
          with branch = (getf args :branch)
          for (name value) on args by #'cddr
          when (member name '(:distributions :distributions[]))
            collect value into new-dist-names
          finally
             (multiple-value-bind (user-name project-name)
                 (params-from-github url)

               (log:info "Saving" new-dist-names url branch)
               
               ;; TODO: we need to refactor this part because for other
               ;; types of sources we'll need to process params differently:
               (multiple-value-bind (new-source was-cloned-p)
                   (update-source-dists (source parent)
                                        :dists new-dist-names
                                        :params (list :user-or-org user-name
                                                      :project project-name
                                                      :branch branch))
                 (when was-cloned-p
                   (log:info "A new source version was created: " new-source)
                   (setf (source parent)
                         new-source)))))
    (switch-to-readonly widget)))


(defun make-add-source-widget (project &key on-new-source)
  (make-instance 'add-source-widget
                 :project project
                 :on-new-source on-new-source))


(defun make-source-widget (source)
  (let* ((main (make-instance 'source-widget
                              :source source))
         (subwidget (make-instance 'readonly-source-widget
                                   :parent main
                                   :check (get-last-source-check source))))
    (setf (slot-value main 'subwidget)
          subwidget)
    main))


(defun render-distribution (dist-source)
  (check-type dist-source ultralisp/models/dist-source:dist-source)
  (let* ((dist (dist-source->dist dist-source))
         (name (dist-name dist))
         (url (ultralisp/protocols/url:url dist))
         (enabled (enabled-p dist-source))
         (class (if enabled
                    "dist enabled"
                    "dist disabled"))
         (reason (unless enabled
                   ;; TODO: create a special pretty-printer for disable reason.
                   ;; and probably make a popup with the traceback.
                   (fmt "Disabled: ~A"
                        (ultralisp/models/dist-source:disable-reason dist-source)))))
    (with-html
      (:a :class class
          :title reason
          :href url
          name))))


(defgeneric render-source (widget type source))


(defun github-url (source-params)
  (let ((user-or-org (getf source-params :user-or-org))
        (project-name (getf source-params :project)))
    (if (and user-or-org project-name)
        (format nil "https://github.com/~A/~A"
                user-or-org
                project-name)
        "")))


(defmethod render-source ((widget readonly-source-widget)
                          (type (eql :github))
                          source)
  (let* ((params (ultralisp/models/source:source-params source))
         (deleted (ultralisp/models/source:deleted-p source))
         (url (github-url params))
         (last-seen-commit (getf params :last-seen-commit))
         (release-info (ultralisp/models/source:source-release-info source))
         (distributions (source-distributions source)))
    ;; Deleted sources should not be in the list
    ;; for rendering.
    (assert (not deleted))
    
    (with-html
      (:table
       (:tr (:td :class "label-column"
                 "Type")
            (:td :class "field-column"
                 type
                 ;; Controls for editing and deleting source
                 (when (ultralisp/protocols/moderation:is-moderator
                        (weblocks-auth/models:get-current-user)
                        (ultralisp/models/project:source->project source))
                   (:div :class "source-controls float-right"
                         (weblocks-ui/form:with-html-form
                             (:post
                              (lambda (&rest args)
                                (declare (ignorable args))
                                (log:error "REMOVING"))
                              :requires-confirmation-p t
                              :confirm-question (:div (:h1 "Warning!")
                                                      (:p "If you'll remove this source, it will be excluded from the future versions of these distributions:")
                                                      (:ul
                                                       (loop for name in (mapcar #'dist-name
                                                                                 (remove-if-not
                                                                                  #'enabled-p
                                                                                  (source->dists source)))
                                                             do (:li name)))))
                           (:input :type "submit"
                                   :class "alert button tiny"
                                   :name "button"
                                   :value "Remove"))
                         
                         (weblocks-ui/form:with-html-form
                             (:post (lambda (&rest args)
                                      (declare (ignorable args))
                                      (edit widget)))
                           (:input :type "submit"
                                   :class "button tiny"
                                   :name "button"
                                   :value "Edit"))))))
       (:tr (:td :class "label-column"
                 "Source")
            (:td :class "field-column"
                 (:a :href url
                     url)))
       (:tr (:td :class "label-column"
                 "Branch or tag")
            (:td :class "field-column"
                 (ultralisp/models/source:get-current-branch source)))
       (when last-seen-commit
         (:tr (:td :class "label-column"
                   "Last seen commit")
              (:td :class "field-column"
                   (:a :href (fmt "~A/commit/~A" url last-seen-commit)
                       last-seen-commit))))
       (when release-info
         (:tr (:td :class "label-column"
                   "Release")
              (:td :class "field-column"
                   (:a :href (quickdist:get-project-url release-info)
                       (quickdist:get-project-url release-info)))))
       (:tr (:td :class "label-column"
                 "Distributions")
            (:td :class "field-column"
                 (mapc #'render-distribution
                       distributions)))
       
       (when (last-check widget)
         (:tr (:td :class "label-column"
                   "Last check")
              (let* ((check (last-check widget))
                     (processed-at (ultralisp/models/check:get-processed-at check)))
                (:td :class "field-column"
                     ("~A~A"
                      (if processed-at
                          (format nil "Finished at ~A." processed-at)
                          "Waiting in the queue.")
                      (if (and (ultralisp/models/check:get-processed-at check)
                               (ultralisp/models/check:get-error check))
                          (format nil " There was an error.")
                          ""))))))))))


(defmethod render-source ((widget edit-source-widget)
                          (type (eql :github))
                          source)
  (let* ((params (ultralisp/models/source:source-params source))
         (deleted (ultralisp/models/source:deleted-p source))
         (url (github-url params))
         (last-seen-commit (getf params :last-seen-commit))
         ;; (distributions (source-distributions source))
         (user (weblocks-auth/models:get-current-user))
         (user-dists (ultralisp/models/dist-moderator:moderated-dists user))
         (all-dists (cons (ultralisp/models/dist:common-dist)
                          user-dists))
         (current-dists (remove-if-not
                         #'enabled-p
                         (source->dists source)))
         (release-info (ultralisp/models/source:source-release-info source)))
    ;; Deleted sources should not be in the list
    ;; for rendering.
    (assert (not deleted))

    (flet ((is-enabled (dist)
             (member (ultralisp/models/dist:dist-name dist)
                     current-dists
                     :key #'ultralisp/models/dist:dist-name
                     :test #'string-equal)))
      (with-html
        (weblocks-ui/form:with-html-form
            (:post (lambda (&rest args)
                     (declare (ignorable args))
                     (save widget args)))
          (:table
           (:tr (:td :class "label-column"
                     "Type")
                (:td :class "field-column"
                     type
                     (:div :class "source-controls float-right"
                           (weblocks-ui/form:with-html-form
                               (:post (lambda (&rest args)
                                        (declare (ignore args))
                                        (switch-to-readonly widget)))
                               (:input :type "submit"
                                       :class "secondary button tiny"
                                       :name "button"
                                       :value "Cancel"))
                           (:input :type "submit"
                                   :class "success button tiny"
                                   :name "button"
                                   :value "Save"))))
           (:tr (:td :class "label-column"
                     "Source")
                (:td :class "field-column"
                     (:input :value url
                             :name "url"
                             :type "text"
                             :onchange
                             (weblocks-parenscript:make-js-handler
                              :lisp-code ((&key url)
                                          (update-url (branches widget)
                                                      url))
                              :js-code ((event)
                                        ;; This will pass new URL value
                                        ;; to the backend:
                                        (parenscript:create
                                         :url (@ event target value)))))))
           (:tr (:td :class "label-column"
                     "Branch or tag")
                (:td :class "field-column"
                     (render (branches widget))))
           (when last-seen-commit
             (:tr (:td :class "label-column"
                       "Last seen commit")
                  (:td :class "field-column"
                       (:a :href (fmt "~A/commit/~A" url last-seen-commit)
                           last-seen-commit))))
           (when release-info
             (:tr (:td :class "label-column"
                       "Release")
                  (:td (:a :href (quickdist:get-project-url release-info)
                           (quickdist:get-project-url release-info)))))
           (:tr (:td :class "label-column"
                     "Distributions")
                (:td :class "field-column"
                 (loop for dist in all-dists
                       for name = (ultralisp/models/dist:dist-name dist)
                       do  (:input :type "checkbox"
                                   :name "distributions"
                                   :value name
                                   :checked (is-enabled dist)
                                   (:label name)))))))))))


(defmethod weblocks/widget:render ((widget branch-select-widget))
  (with-html
    (:select :name "branch"
      (:option :disabled "disabled"
               "Select a branch")
      (loop with current = (current-branch widget)
            for branch in (branches widget)
            do (:option :selected (when (string-equal branch
                                                      current)
                                    "selected")
                        branch)))))


(defmethod weblocks/widget:render ((widget source-widget))
  (weblocks/widget:render (subwidget widget)))


(defmethod weblocks/widget:render ((widget readonly-source-widget))
  (let* ((source (source (parent widget)))
         (type (ultralisp/models/source:source-type source)))
    (render-source widget type source)))


(defmethod weblocks/widget:render ((widget edit-source-widget))
  (let* ((source (source (parent widget)))
         (type (ultralisp/models/source:source-type source)))
    (render-source widget type source)))


(defmethod weblocks/dependencies:get-dependencies ((widget source-widget))
  (append
   (list
    (weblocks-lass:make-dependency
      `(.source-widget
        (input :margin 0)
        (.dist :margin-right 1em)
        (.label-column :white-space "nowrap")
        (.field-column :width "100%")
        ((:and .dist .disabled) :color "gray")

        ((.source-controls > (:or form input))
         :display "inline-block"
         :margin-left 1em))))
   (call-next-method)))


;; Methods to render changes between source versions

(defmethod ultralisp/protocols/render-changes:render ((type (eql :github)) prev-source new-source)
  (weblocks/html:with-html
    (:ul
     (loop with old-params = (ultralisp/models/source:source-params prev-source)
           with new-params = (ultralisp/models/source:source-params new-source)
           with key-to-name = '(:last-seen-commit "commit")
           with key-to-length = '(:last-seen-commit 8)
           for (key new-value) on new-params by #'cddr
           for old-value = (getf old-params key)
           for name = (getf key-to-name key
                            (string-downcase
                             (symbol-name key)))
           for length = (getf key-to-length key 20)
           unless (equal old-value new-value)
           do (:li ("**~A** ~A ➞ ~A"
                    name
                    (str:shorten length old-value :ellipsis "…")
                    (str:shorten length new-value :ellipsis "…")))))))


(defmethod weblocks/widget:render ((widget add-source-widget))
  (let ((project (project widget))
        (user (weblocks-auth/models:get-current-user)))
    (weblocks/html:with-html
      ;; Controls for editing and deleting source
      (when (ultralisp/protocols/moderation:is-moderator user project)
        (weblocks-ui/form:with-html-form
            (:post (lambda (&rest args)
                     (declare (ignorable args))
                     (let* ((on-new-source (on-new-source widget))
                            (source-type (make-keyword (getf args :type)))
                            (source (ultralisp/models/source:create-source project
                                                                           source-type)))
                       (weblocks/utils/misc:safe-funcall on-new-source source))))
          (:table
           (:tr
            (:td
             (:label :style "min-width: 20%; white-space: nowrap"
                     "New source of type:"))
            (:td
             (:select :name "type"
               :style "min-width: 7em; margin: 0"
               (:option :selected t
                        "GITHUB")
               (:option "ARCHIVE")))
            (:td :style "width: 100%"
                 (:input :type "submit"
                         :class "button small"
                         :style "margin: 0"
                         :name "button"
                         :value "Add")))))))))
