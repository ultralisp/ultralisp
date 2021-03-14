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
                #:dist-version
                #:dist-id
                #:delete-source
                #:update-source-dists
                #:source-distributions)
  (:import-from #:ultralisp/models/dist-moderator)
  (:import-from #:ultralisp/protocols/external-url
                #:external-url)
  (:import-from #:ultralisp/protocols/url)
  (:import-from #:rutils
                #:awhen
                #:it
                #:fmt)
  (:import-from #:weblocks/html
                #:with-html)
  (:import-from #:ultralisp/models/dist
                #:dist-name)
  (:import-from #:ultralisp/models/dist-source
                #:source->dists
                #:dist-source->dist)
  (:import-from #:ultralisp/models/check
                #:make-check
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
  (:import-from #:mito
                #:object-id)
  (:import-from #:ultralisp/models/versioned
                #:object-version)
  (:import-from #:ultralisp/models/project
                #:source->project)
  (:import-from #:weblocks-auth/models
                #:get-current-user)
  (:import-from #:ultralisp/protocols/moderation
                #:is-moderator)
  (:import-from #:log4cl-extras/context
                #:with-fields)
  (:import-from #:ultralisp/utils/time
                #:humanize-timestamp
                #:humanize-duration)
  (:import-from #:ultralisp/utils/source
                #:format-ignore-list
                #:parse-ignore-list)
  (:import-from #:group-by
                #:group-by)
  (:import-from #:ultralisp/models/asdf-system
                #:asdf-systems-conflict)
  (:export
   #:make-source-widget
   #:make-add-source-widget))
(in-package ultralisp/widgets/source)


(defwidget source-widget ()
  ((source :initarg :source
           :accessor source)
   (on-delete :initform nil
              :initarg :on-delete
              :documentation "An optional callback.

                              It will be called if the source is deleted.

                              The SOURCE-WIDGET's instance is passed as a single argument to a callback.")
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
             :reader branches)
   (dist-conflicts :initform nil
                   :accessor dist-conflicts)))


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
          with ignore-dirs = (awhen (getf args :ignore-dirs)
                               (parse-ignore-list it))
          for (name value) on args by #'cddr
          when (member name '(:distributions :distributions[]))
            collect value into new-dist-names
          finally
             (multiple-value-bind (user-name project-name)
                 (params-from-github url)

               (log:info "Saving" new-dist-names url branch)
               
               ;; TODO: we need to refactor this part because for other
               ;; types of sources we'll need to process params differently:
               (let ((params-update
                       (list :user-or-org user-name
                             :project project-name
                             :branch branch
                             :ignore-dirs ignore-dirs)))
                 (with-fields (:params params-update)
                   (multiple-value-bind (new-source was-cloned-p)
                       (update-source-dists (source parent)
                                            :dists new-dist-names
                                            :params params-update)
                     (when was-cloned-p
                       (log:error "A new source version was created: " new-source)
                       (setf (source parent)
                             new-source)))))))
    (switch-to-readonly widget)))


(defun make-add-source-widget (project &key on-new-source)
  (make-instance 'add-source-widget
                 :project project
                 :on-new-source on-new-source))


(defun make-source-widget (source &key on-delete)
  (let* ((main (make-instance 'source-widget
                              :source source
                              :on-delete on-delete))
         (subwidget (make-instance 'readonly-source-widget
                                   :parent main)))
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


(defun on-delete (widget)
  (check-type widget source-widget)
  
  (let ((source (source widget)))
    (with-fields (:source-id (object-id source)
                  :source-version (object-version source))
      (log:error "Deleting source")
     
      (delete-source source)
     
      (weblocks/utils/misc:safe-funcall (slot-value widget 'on-delete)
                                        widget))))


(defmethod render-source ((widget readonly-source-widget)
                          (type (eql :github))
                          source)
  (let* ((params (ultralisp/models/source:source-params source))
         (deleted (ultralisp/models/source:deleted-p source))
         (url (github-url params))
         (last-seen-commit (getf params :last-seen-commit))
         (ignore-dirs (getf params :ignore-dirs))
         (release-info (ultralisp/models/source:source-release-info source))
         (distributions (source-distributions source))
         (systems (ultralisp/models/source:source-systems-info source))
         (user-is-moderator (is-moderator
                             (get-current-user)
                             (source->project source))))
    ;; Deleted sources should not be in the list
    ;; for rendering.
    (assert (not deleted))

    (flet ((deletion-handler (&rest args)
             (declare (ignorable args))
             (on-delete (parent widget))))
      (let ((last-check (get-last-source-check source)))
        (with-html
          (:table :class "unstriped"
                  (:thead
                   (:tr (:th :class "label-column"
                             "Type")
                        (:th :class "field-column"
                             type
                             ;; Controls for editing and deleting source
                             (when user-is-moderator
                               (:div :class "source-controls float-right"
                                     (weblocks-ui/form:with-html-form
                                         (:post #'deletion-handler
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
                                               :value "Edit")))))))
                  (:tbody
                   (:tr (:td :class "label-column"
                             "Source")
                        (:td :class "field-column"
                             (:a :href url
                                 url)))
                   (:tr (:td :class "label-column"
                             "Branch or tag")
                        (:td :class "field-column"
                             (ultralisp/models/source:get-current-branch source)))
                   (when ignore-dirs
                     (:tr (:td :class "label-column"
                               "Ignore systems in these dirs and ASD files")
                          (:td :class "field-column"
                               (:pre
                                (:code
                                 (format-ignore-list
                                  ignore-dirs))))))
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
                   (when systems
                     (:tr (:td :class "label-column"
                               "Systems")
                          (:td :class "field-column"
                               (:dl
                                (loop with grouped = (sort
                                                      (group-by systems
                                                                :key #'quickdist:get-filename
                                                                :value #'quickdist:get-name
                                                                :test #'string=)
                                                      #'string<
                                                      :key #'car)
                                      for (filename . systems) in grouped
                                      do (:dt filename)
                                         (:dd :style "padding-left: 2em"
                                              (str:join ", " (sort systems
                                                                   #'string<))))))))
                   (:tr (:td :class "label-column"
                             "Distributions")
                        (:td :class "field-column"
                             (mapc #'render-distribution
                                   distributions)))

                   (:tr (:td :class "label-column"
                             "Last check")
                        (:td :class "field-column"
                             (cond
                               (last-check
                                (let* ((processed-at (ultralisp/models/check:get-processed-at
                                                      last-check)))
                                  (cond (processed-at
                                         (let* ((now (local-time:now))
                                                (duration
                                                  (humanize-duration
                                                   (local-time-duration:timestamp-difference
                                                    now
                                                    processed-at)))
                                                (error (ultralisp/models/check:get-error last-check))
                                                (next-check-at (humanize-duration
                                                                (local-time-duration:timestamp-difference
                                                                 (ultralisp/cron:get-time-of-the-next-check
                                                                  source)
                                                                 now))))
                                           (:span (fmt "Finished ~A ago. " duration))
                                           
                                           (when error
                                             (:span "There was an")
                                             (let* ((popup-id (symbol-name (gensym "ERROR-POPUP"))))
                                               (:div :id popup-id
                                                     :class "reveal large"
                                                     :data-reveal "true"
                                                     (:h1 "Check Error")
                                                     (:pre error))
                                               (:a :data-open popup-id
                                                   "error"))
                                             (:span "."))
                                           (:span
                                            ("Next check will be made in ~A."
                                             next-check-at))))
                                        (t
                                         ("Waiting in the queue. Position: ~A."
                                          (ultralisp/models/check:position-in-the-queue last-check))))))
                               (t
                                ("No checks yet.")))

                             (when user-is-moderator
                               (weblocks-ui/form:with-html-form
                                   (:post (lambda (&rest args)
                                            (declare (ignore args))
                                            ;; This call will create a new check
                                            ;; only if it is not exist yet:
                                            (make-check source
                                                        :manual)
                                            (weblocks/widget:update widget))
                                    :class "float-right")
                                 (:input :type "submit"
                                         :class "button tiny secondary"
                                         :name "button"
                                         :value "Check"
                                         :title "Put the check into the queue."))))))))))))


(defmethod render-source ((widget readonly-source-widget)
                          (type (eql :archive))
                          source)
  (with-html
    (:p "Archive sources aren't supported yet")))


(defmethod render-source ((widget edit-source-widget)
                          (type (eql :github))
                          source)
  (let* ((params (ultralisp/models/source:source-params source))
         (deleted (ultralisp/models/source:deleted-p source))
         (url (github-url params))
         (last-seen-commit (getf params :last-seen-commit))
         (ignore-dirs (getf params :ignore-dirs))
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
                     (handler-case (save widget args)
                       (asdf-systems-conflict (c)
                         (let ((message (fmt "~A" c)))
                           (setf (dist-conflicts widget)
                                 message)
                           (weblocks/widget:update widget))))))
          (:table :class "unstriped"
           (:thead
            (:tr (:th :class "label-column"
                      "Type")
                 (:th :class "field-column"
                      type
                      (:div :class "source-controls float-right"
                            (let ((js-code-to-cancel
                                    (weblocks/actions:make-js-action
                                     (lambda (&rest args)
                                           (declare (ignore args))
                                       (switch-to-readonly widget)))))
                              (:input :type "button"
                                      :class "secondary button tiny"
                                      :name "button"
                                      :onclick js-code-to-cancel
                                      :value "Cancel"))
                            (:input :type "submit"
                                    :class "success button tiny"
                                    :name "button"
                                    :value "Save")))))
           (:tbody
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
            (:tr (:td :class "label-column"
                      "Ignore systems in these dirs and ASD files")
                 (:td :class "field-column"
                      (:textarea :name "ignore-dirs"
                                 :placeholder "vendor/, my-system-test.asd"
                                 (format-ignore-list
                                  ignore-dirs))))
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
                                        (:label name)))
                      (when (dist-conflicts widget)
                        (:pre :class "error"
                              (dist-conflicts widget))))))))))))


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
        :border-top "2px solid #cc4b37"
        (input :margin 0)
        (.dist :margin-right 1em)
        (.label-column :white-space "nowrap"
                       :vertical-align "top")
        (.field-column :width "100%")
        ((:and .dist .disabled) :color "gray")

        ((.source-controls > (:or form input))
         :display "inline-block"
         :margin-left 1em)
        (.error :color "red"))))
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
           for old-value = (let ((result (getf old-params key)))
                             (cond
                               ;; For new sources
                               ;; branch might be not given.
                               ;; In this case we show it as "main"
                               ;; TODO: We need to fill branch parameter
                               ;; when source gets added to the database.
                               ;; When it will be done, this hack can be removed.
                               ((and (eql key :branch)
                                     (null result))
                                "main")
                               (t result)))
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
                        "GitHub")
               (:option "Archive" :disabled t)
               (:option "Git" :disabled t)
               (:option "Mercurial" :disabled t)))
            (:td :style "width: 100%"
                 (:input :type "submit"
                         :class "button small"
                         :style "margin: 0"
                         :name "button"
                         :value "Add")))))))))
