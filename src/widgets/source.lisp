(defpackage #:ultralisp/widgets/source
  (:use #:cl)
  (:import-from #:parenscript
                #:@)
  (:import-from #:log)
  (:import-from #:quickdist)
  (:import-from #:ultralisp/protocols/render-changes)
  (:import-from #:reblocks/widget
                #:defwidget
                #:render)
  (:import-from #:reblocks/dependencies)
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
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:ultralisp/models/dist
                #:dist-name
                #:find-dist)
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
                #:project-version
                #:source-project-id
                #:source-type
                #:params-from-github
                #:source-last-check-failed)
  (:import-from #:ultralisp/utils/git
                #:get-git-branches)
  (:import-from #:ultralisp/utils/github
                #:get-github-branches)
  (:import-from #:ultralisp/protocols/enabled
                #:enabled-p)
  (:import-from #:mito
                #:object-id)
  (:import-from #:ultralisp/models/versioned
                #:get-latest-version-of
                #:object-version
                #:prev-version)
  (:import-from #:ultralisp/models/project
                #:source->project)
  (:import-from #:reblocks-auth/models
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
  (:import-from #:reblocks-ui/form
                #:form-error-placeholder
                #:field-error
                #:error-placeholder)
  (:import-from #:local-time
                #:now)
  (:import-from #:local-time-duration
                #:timestamp-difference)
  (:import-from #:reblocks/actions
                #:make-js-action)
  (:import-from #:reblocks/utils/misc
                #:safe-funcall)
  (:import-from #:str
                #:join)
  (:import-from #:ultralisp/cron
                #:get-time-of-the-next-check)
  (:import-from #:reblocks-parenscript
                 #:make-js-handler)
  (:import-from #:ultralisp/variables
                #:*link-color-classes*)
  (:import-from #:reblocks-ui2/themes/styling
                #:join-css-classes)
  (:import-from #:cl-ppcre
                #:regex-replace-all)
  (:export
   #:make-source-widget
   #:make-add-source-widget))
(in-package #:ultralisp/widgets/source)


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
   (retrieve-branches-func :initarg :retrieve-branches-func
                           :reader retrieve-branches-func)
   (current :initarg :current
            :reader current-branch)))


(defun make-branch-select-widget (url source-type &key (current "main"))
  (let ((callable (case source-type
                    (:github 'get-github-branches)
                    (:git 'get-git-branches))))
    (multiple-value-bind (branches default-branch)
        (funcall callable url)
      (make-instance 'branch-select-widget
                     :retrieve-branches-func callable
                     :branches branches
                     :current (or current
                                  default-branch)))))


(defun update-url (widget url)
  (check-type widget branch-select-widget)
  (check-type url string)
  
  (multiple-value-bind (branches default-branch)
      (funcall (retrieve-branches-func widget) url)
    (setf (slot-value widget 'branches)
          branches
          (slot-value widget 'current)
          default-branch)
    (reblocks/widget:update widget)))


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
                                              (source-type source)
                                              :current (ultralisp/models/source:get-current-branch source)))))
    (setf (slot-value main 'subwidget)
          subwidget)
    (reblocks/widget:update main)))


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
    (reblocks/widget:update parent)))


;; Here is the flow of working with a source.
;; Each source type has to define it's own form for read-only
;; rendering, editing and a method for saving results.
;; When results are saved, there is a common part of
;; distribution update and a custom part of fields update.


(defun ensure-all-dists-have-same-lisp-implementation (dist-names)
  (loop with implementations = nil
        for name in dist-names
        for dist = (find-dist name :raise-error nil)
        when dist
        do (pushnew (ultralisp/models/dist:lisp-implementation dist)
                    implementations
                    :test #'equal)
        finally (when (> (length implementations) 1)
                  (field-error "distributions"
                               "Unable to add source to distributions with different lisp implementations"))))


(defun get-updated-params (source url branch ignore-dirs)
  (let ((type (source-type source)))
    (case type
      (:github
       (multiple-value-bind (user-name project-name)
           (params-from-github url)
         (list :user-or-org user-name
               :project project-name
               :branch branch
               :ignore-dirs ignore-dirs))
       )
      (:git
       (list :url url
             :branch branch
             :ignore-dirs ignore-dirs)))))


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
             (log:info "Saving" new-dist-names url branch)
             (ensure-all-dists-have-same-lisp-implementation new-dist-names)
          
             (let ((params-update
                     (get-updated-params (source parent)
                                         url
                                         branch
                                         ignore-dirs)))
               (with-fields (:params params-update)
                 (multiple-value-bind (new-source was-cloned-p)
                     (update-source-dists (source parent)
                                          :dists new-dist-names
                                          :params params-update)
                   (when was-cloned-p
                     (log:info "A new source version was created: " new-source)
                     (setf (source parent)
                           new-source))))))
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
         (class (join-css-classes nil
                                   (if enabled
                                       "mr-2"
                                       "mr-2 line-through text-gray-400")
                                   *link-color-classes*))
         (reason (unless enabled
                   (fmt "Disabled: ~A"
                        (ultralisp/models/dist-source:disable-reason dist-source)))))
    (with-html ()
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
     
      (reblocks/utils/misc:safe-funcall (slot-value widget 'on-delete)
                                        widget))))


(defun render-check-description (source last-check)
  (with-html ()
    (cond
      (last-check
       (let* ((processed-at (ultralisp/models/check:get-processed-at
                             last-check)))
         (cond (processed-at
                (let* ((now (now))
                       (duration
                         (humanize-duration
                          (timestamp-difference
                           now
                           processed-at)))
                       (time-to-next-check
                         (local-time-duration:timestamp-difference
                          (get-time-of-the-next-check source)
                          now))
                       (next-check-at (if (> (local-time-duration:duration-as time-to-next-check :sec)
                                             0)
                                        (fmt " Next check will be made in ~A."
                                             (humanize-duration
                                              time-to-next-check))
                                        " Next check will be made very soon.")))
                  (:span (fmt "Finished ~A ago. " duration))
                  (:span next-check-at)))
               (t
                (fmt "Waiting in the queue. Position: ~A."
                     (ultralisp/models/check:position-in-the-queue last-check))))))
      (t
       "No checks yet."))))


(defun render-check-failed-callout (source last-check)
  (with-html ()
    (let* ((last-check-failed (source-last-check-failed source))
           (processed-at (ultralisp/models/check:get-processed-at last-check))
           (error (when processed-at
                    (ultralisp/models/check:get-error last-check))))
      (when (and processed-at
                 last-check-failed)
        (cond
          (error
           ;; overflow-x-auto + whitespace-pre: long error lines scroll
           ;; horizontally instead of wrapping. bg-white is on the outer
           ;; div so the background stays fixed while text scrolls inside.
           (:details :class "bg-red-50 border border-red-300 text-red-700 px-4 py-3 rounded mt-2"
                     (:summary :class "cursor-pointer"
                               "Check of latest code version was failed.")
                     (:div :class "overflow-x-auto mt-2 bg-white rounded"
                           (:pre :class "p-3 text-xs max-h-96 whitespace-pre"
                                 error))))
          (t
           (:span "Check of latest code version was failed.")))))))


(defun render-check-button (user-is-moderator widget source last-check)
  (let ((processed-at (ultralisp/models/check:get-processed-at
                       last-check)))
    (when (and user-is-moderator
               (not (null processed-at)))
      (reblocks-ui/form:with-html-form
          (:post (lambda (&rest args)
                   (declare (ignore args))
                   (make-check source
                               :manual)
                   (reblocks/widget:update widget))
            :class "inline ml-2")
        (:input :type "submit"
                :class "text-xs px-2 py-1 rounded bg-gray-500 text-white hover:bg-gray-600 cursor-pointer"
                :name "button"
                :value "Check"
                :title "Put the check into the queue.")))))


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
    (assert (not deleted))

    (flet ((deletion-handler (&rest args)
             (declare (ignorable args))
             (on-delete (parent widget))))
      (let ((last-check (get-last-source-check source))
            (confirm-msg (fmt "If you'll remove this source, it will be excluded from the future versions of these distributions: ~{~A~^, ~}"
                              (mapcar #'dist-name
                                      (remove-if-not #'enabled-p
                                                     (source->dists source))))))
        (with-html ()
          (:div :class "border rounded-lg shadow-sm mb-6 overflow-hidden"
                (:div :class "flex justify-between items-center px-4 py-2 bg-gray-50 border-b"
                      (:span :class "font-semibold text-gray-700"
                             (fmt "Type: ~A" type))
                      (when user-is-moderator
                        (:div :class "flex gap-2"
                              (reblocks-ui/form:with-html-form
                                  (:post (lambda (&rest args)
                                           (declare (ignorable args))
                                           (edit widget))
                                    :class "inline")
                                (:input :type "submit"
                                        :class "text-xs px-2 py-1 rounded bg-sky-600 text-white hover:bg-sky-700 cursor-pointer"
                                        :name "button"
                                        :value "Edit"))
                              (reblocks-ui/form:with-html-form
                                  (:post #'deletion-handler
                                    :class "inline")
                                (:input :type "submit"
                                        :class "text-xs px-2 py-1 rounded bg-red-600 text-white hover:bg-red-700 cursor-pointer"
                                        :name "button"
                                        :value "Remove"
                                        :onclick (format nil "return confirm('~A');"
                                                         (cl-ppcre:regex-replace-all "'" confirm-msg "\\\\'")))))))
                (:div :class "divide-y"
                      (:div :class "flex px-4 py-2"
                            (:div :class "w-1/4 text-gray-500 font-medium shrink-0" "Created at")
                            (:div :class "flex-1"
                                  (humanize-timestamp
                                   (mito:object-created-at source))))
                      (:div :class "flex px-4 py-2"
                            (:div :class "w-1/4 text-gray-500 font-medium shrink-0" "Source")
                            (:div :class "flex-1"
                                  (:a :href url
                                      :class *link-color-classes*
                                      url)))
                      (:div :class "flex px-4 py-2"
                            (:div :class "w-1/4 text-gray-500 font-medium shrink-0" "Branch or tag")
                            (:div :class "flex-1"
                                  (ultralisp/models/source:get-current-branch source)))
                      (when ignore-dirs
                        (:div :class "flex px-4 py-2"
                              (:div :class "w-1/4 text-gray-500 font-medium shrink-0"
                                    "Ignore systems in these dirs and ASD files")
                              (:div :class "flex-1"
                                    (:pre :class "text-xs bg-gray-50 p-2 rounded"
                                          (:code
                                           (format-ignore-list
                                            ignore-dirs))))))
                      (when last-seen-commit
                        (:div :class "flex px-4 py-2"
                              (:div :class "w-1/4 text-gray-500 font-medium shrink-0" "Last seen commit")
                              (:div :class "flex-1"
                                    (:a :href (fmt "~A/commit/~A" url last-seen-commit)
                                        :class *link-color-classes*
                                        last-seen-commit))))
                      (when release-info
                        (:div :class "flex px-4 py-2"
                              (:div :class "w-1/4 text-gray-500 font-medium shrink-0" "Release")
                              (:div :class "flex-1"
                                    (:a :href (quickdist:get-project-url release-info)
                                        :class *link-color-classes*
                                        (quickdist:get-project-url release-info)))))
                      (when systems
                        (:div :class "flex px-4 py-2"
                              (:div :class "w-1/4 text-gray-500 font-medium shrink-0" "Systems")
                              (:div :class "flex-1"
                                    (:dl
                                     (loop with grouped = (sort
                                                           (group-by systems
                                                                     :key #'quickdist:get-filename
                                                                     :value #'quickdist:get-name
                                                                     :test #'string=)
                                                           #'string<
                                                           :key #'car)
                                           for (filename . systems) in grouped
                                           do (:dt :class "font-medium"
                                                   filename)
                                              (:dd :class "ml-8 mb-1"
                                                   (join ", " (sort systems
                                                                    #'string<))))))))
                      (:div :class "flex px-4 py-2"
                            (:div :class "w-1/4 text-gray-500 font-medium shrink-0" "Distributions")
                            (:div :class "flex-1"
                                  (mapc #'render-distribution
                                        distributions)))
                      (:div :class "flex px-4 py-2"
                            (:div :class "w-1/4 text-gray-500 font-medium shrink-0" "Last check")
                            ;; min-w-0: without it the flex item's implicit
                            ;; min-width:auto prevents it from shrinking below
                            ;; the intrinsic width of the <details>/<pre>
                            ;; content, causing the whole card to widen.
                            (:div :class "flex-1 min-w-0"
                                  (:div :class "flex items-start gap-2"
                                        (:div :class "flex-1 min-w-0"
                                              (render-check-description source last-check))
                                        (render-check-button user-is-moderator widget source last-check))
                                  (render-check-failed-callout source last-check))))))))))


;; Probably I need to replace eql git with real class and reuse some code between
;; git and github source types?
(defmethod render-source ((widget readonly-source-widget)
                          (type (eql :git))
                          source)
  (let* ((params (ultralisp/models/source:source-params source))
         (deleted (ultralisp/models/source:deleted-p source))
         (url (getf params :url))
         (last-seen-commit (getf params :last-seen-commit))
         (ignore-dirs (getf params :ignore-dirs))
         (release-info (ultralisp/models/source:source-release-info source))
         (distributions (source-distributions source))
         (systems (ultralisp/models/source:source-systems-info source))
         (user-is-moderator (is-moderator
                             (get-current-user)
                             (source->project source))))
    (assert (not deleted))

    (flet ((deletion-handler (&rest args)
             (declare (ignorable args))
             (on-delete (parent widget))))
      (let ((last-check (get-last-source-check source))
            (confirm-msg (fmt "If you'll remove this source, it will be excluded from the future versions of these distributions: ~{~A~^, ~}"
                              (mapcar #'dist-name
                                      (remove-if-not #'enabled-p
                                                     (source->dists source))))))
        (with-html ()
          (:div :class "border rounded-lg shadow-sm mb-6 overflow-hidden"
                (:div :class "flex justify-between items-center px-4 py-2 bg-gray-50 border-b"
                      (:span :class "font-semibold text-gray-700"
                             (fmt "Type: ~A" type))
                      (when user-is-moderator
                        (:div :class "flex gap-2"
                              (reblocks-ui/form:with-html-form
                                  (:post (lambda (&rest args)
                                           (declare (ignorable args))
                                           (edit widget))
                                    :class "inline")
                                (:input :type "submit"
                                        :class "text-xs px-2 py-1 rounded bg-sky-600 text-white hover:bg-sky-700 cursor-pointer"
                                        :name "button"
                                        :value "Edit"))
                              (reblocks-ui/form:with-html-form
                                  (:post #'deletion-handler
                                    :class "inline")
                                (:input :type "submit"
                                        :class "text-xs px-2 py-1 rounded bg-red-600 text-white hover:bg-red-700 cursor-pointer"
                                        :name "button"
                                        :value "Remove"
                                        :onclick (format nil "return confirm('~A');"
                                                         (cl-ppcre:regex-replace-all "'" confirm-msg "\\\\'")))))))
                (:div :class "divide-y"
                      (:div :class "flex px-4 py-2"
                            (:div :class "w-1/4 text-gray-500 font-medium shrink-0" "Created at")
                            (:div :class "flex-1"
                                  (humanize-timestamp
                                   (mito:object-created-at source))))
                      (:div :class "flex px-4 py-2"
                            (:div :class "w-1/4 text-gray-500 font-medium shrink-0" "Source")
                            (:div :class "flex-1"
                                  (:a :href url
                                      :class *link-color-classes*
                                      url)))
                      (:div :class "flex px-4 py-2"
                            (:div :class "w-1/4 text-gray-500 font-medium shrink-0" "Branch or tag")
                            (:div :class "flex-1"
                                  (ultralisp/models/source:get-current-branch source)))
                      (when ignore-dirs
                        (:div :class "flex px-4 py-2"
                              (:div :class "w-1/4 text-gray-500 font-medium shrink-0"
                                    "Ignore systems in these dirs and ASD files")
                              (:div :class "flex-1"
                                    (:pre :class "text-xs bg-gray-50 p-2 rounded"
                                          (:code
                                           (format-ignore-list
                                            ignore-dirs))))))
                      (when last-seen-commit
                        (:div :class "flex px-4 py-2"
                              (:div :class "w-1/4 text-gray-500 font-medium shrink-0" "Last seen commit")
                              (:div :class "flex-1"
                                    (:a :href (fmt "~A/commit/~A" url last-seen-commit)
                                        :class *link-color-classes*
                                        last-seen-commit))))
                      (when release-info
                        (:div :class "flex px-4 py-2"
                              (:div :class "w-1/4 text-gray-500 font-medium shrink-0" "Release")
                              (:div :class "flex-1"
                                    (:a :href (quickdist:get-project-url release-info)
                                        :class *link-color-classes*
                                        (quickdist:get-project-url release-info)))))
                      (when systems
                        (:div :class "flex px-4 py-2"
                              (:div :class "w-1/4 text-gray-500 font-medium shrink-0" "Systems")
                              (:div :class "flex-1"
                                    (:dl
                                     (loop with grouped = (sort
                                                           (group-by systems
                                                                     :key #'quickdist:get-filename
                                                                     :value #'quickdist:get-name
                                                                     :test #'string=)
                                                           #'string<
                                                           :key #'car)
                                           for (filename . systems) in grouped
                                           do (:dt :class "font-medium"
                                                   filename)
                                              (:dd :class "ml-8 mb-1"
                                                   (join ", " (sort systems
                                                                    #'string<))))))))
                      (:div :class "flex px-4 py-2"
                            (:div :class "w-1/4 text-gray-500 font-medium shrink-0" "Distributions")
                            (:div :class "flex-1"
                                  (mapc #'render-distribution
                                        distributions)))
                      (:div :class "flex px-4 py-2"
                             (:div :class "w-1/4 text-gray-500 font-medium shrink-0" "Last check")
                             ;; min-w-0: without it the flex item's implicit
                             ;; min-width:auto prevents it from shrinking below
                             ;; the intrinsic width of the <details>/<pre>
                             ;; content, causing the whole card to widen.
                             (:div :class "flex-1 min-w-0"
                                  (cond
                                    (last-check
                                     (let* ((processed-at (ultralisp/models/check:get-processed-at
                                                           last-check)))
                                       (cond (processed-at
                                              (let* ((now (now))
                                                     (duration
                                                       (humanize-duration
                                                        (timestamp-difference
                                                         now
                                                         processed-at)))
                                                     (time-to-next-check
                                                       (local-time-duration:timestamp-difference
                                                        (get-time-of-the-next-check source)
                                                        now))
                                                     (next-check-at (if (> (local-time-duration:duration-as time-to-next-check :sec)
                                                                           0)
                                                                      (fmt " Next check will be made in ~A."
                                                                           (humanize-duration
                                                                            time-to-next-check))
                                                                      " Next check will be made very soon.")))
                                                (:span (fmt "Finished ~A ago. " duration))
                                                (:span next-check-at)))
                                             (t
                                              (fmt "Waiting in the queue. Position: ~A."
                                                   (ultralisp/models/check:position-in-the-queue last-check))))))
                                    (t
                                     "No checks yet."))

                                  (render-check-failed-callout source last-check)

                                  (when user-is-moderator
                                    (reblocks-ui/form:with-html-form
                                        (:post (lambda (&rest args)
                                                 (declare (ignore args))
                                                 (make-check source
                                                             :manual)
                                                 (reblocks/widget:update widget))
                                          :class "inline ml-2")
                                      (:input :type "submit"
                                              :class "text-xs px-2 py-1 rounded bg-gray-500 text-white hover:bg-gray-600 cursor-pointer"
                                              :name "button"
                                              :value "Check"
                                              :title "Put the check into the queue."))))))))))))


(defmethod render-source ((widget readonly-source-widget)
                          (type (eql :archive))
                          source)
  (with-html ()
    (:p "Archive sources aren't supported yet")))


(defmethod render-source ((widget edit-source-widget)
                          (type (eql :github))
                          source)
  (let* ((params (ultralisp/models/source:source-params source))
         (deleted (ultralisp/models/source:deleted-p source))
         (url (github-url params))
         (ignore-dirs (getf params :ignore-dirs))
         (user (reblocks-auth/models:get-current-user))
         (user-dists (ultralisp/models/dist-moderator:moderated-dists user))
         (all-dists (append (ultralisp/models/dist:public-dists)
                            user-dists))
         (current-dists (source->dists source)))
    (assert (not deleted))

    (flet ((is-enabled (dist)
             (member (ultralisp/models/dist:dist-name dist)
                     current-dists
                     :key #'ultralisp/models/dist:dist-name
                     :test #'string-equal)))
      (with-html ()
        (reblocks-ui/form:with-html-form
            (:post (lambda (&rest args)
                     (handler-case (save widget args)
                       (asdf-systems-conflict (c)
                         (let ((message (fmt "~A" c)))
                           (setf (dist-conflicts widget)
                                 message)
                           (reblocks/widget:update widget))))))
          (form-error-placeholder)
          (:div :class "border rounded-lg shadow-sm mb-6 overflow-hidden"
                (:div :class "flex justify-between items-center px-4 py-2 bg-gray-50 border-b"
                      (:span :class "font-semibold text-gray-700"
                             (fmt "Type: ~A" type))
                      (:div :class "flex gap-2"
                            (let ((js-code-to-cancel
                                    (make-js-action
                                     (lambda (&rest args)
                                       (declare (ignore args))
                                       (switch-to-readonly widget)))))
                              (:input :type "button"
                                      :class "text-xs px-2 py-1 rounded bg-gray-500 text-white hover:bg-gray-600 cursor-pointer"
                                      :name "button"
                                      :onclick js-code-to-cancel
                                      :value "Cancel"))
                            (:input :type "submit"
                                    :class "text-xs px-2 py-1 rounded bg-green-600 text-white hover:bg-green-700 cursor-pointer"
                                    :name "button"
                                    :value "Save")))
                (:div :class "space-y-3"
                      (:div :class "flex px-4 py-2 gap-3"
                            (:div :class "w-1/4 text-gray-500 font-medium shrink-0" "Source")
                            (:div :class "flex-1"
                                  (:input :value url
                                          :name "url"
                                          :type "text"
                                          :class "w-full border rounded px-2 py-1 text-sm"
                                          :onchange
                                          (make-js-handler
                                           :lisp-code ((&key url &allow-other-keys)
                                                       (update-url (branches widget)
                                                                   url))
                                           :js-code ((event)
                                                     (parenscript:create
                                                      :url (@ event target value)))))))
                      (:div :class "flex px-4 py-2 gap-3"
                            (:div :class "w-1/4 text-gray-500 font-medium shrink-0" "Branch or tag")
                            (:div :class "flex-1"
                                  (render (branches widget))))
                      (:div :class "flex px-4 py-2 gap-3"
                            (:div :class "w-1/4 text-gray-500 font-medium shrink-0"
                                  "Ignore systems in these dirs and ASD files")
                            (:div :class "flex-1"
                                  (:textarea :name "ignore-dirs"
                                             :class "w-full border rounded px-2 py-1 text-sm"
                                             :placeholder "vendor/, my-system-test.asd"
                                             (format-ignore-list
                                              ignore-dirs))))
                      (:div :class "flex px-4 py-2 gap-3"
                            (:div :class "w-1/4 text-gray-500 font-medium shrink-0" "Distributions")
                            (:div :class "flex-1"
                                  (loop for dist in all-dists
                                        for name = (ultralisp/models/dist:dist-name dist)
                                        do (:label :class "inline-flex items-center mr-3"
                                                   (:input :type "checkbox"
                                                           :name "distributions"
                                                           :value name
                                                           :class "mr-1"
                                                           :checked (is-enabled dist))
                                                   name))
                                  (when (dist-conflicts widget)
                                    (:pre :class "text-red-600 text-xs mt-1"
                                          (dist-conflicts widget)))
                                  (error-placeholder "distributions"))))))))))

;; TODO: deduplicate code between :git and :github
(defmethod render-source ((widget edit-source-widget)
                          (type (eql :git))
                          source)
  (let* ((params (ultralisp/models/source:source-params source))
         (deleted (ultralisp/models/source:deleted-p source))
         (url (getf params :url))
         (ignore-dirs (getf params :ignore-dirs))
         (user (reblocks-auth/models:get-current-user))
         (user-dists (ultralisp/models/dist-moderator:moderated-dists user))
         (all-dists (append (ultralisp/models/dist:public-dists)
                            user-dists))
         (current-dists (source->dists source)))
    (assert (not deleted))

    (flet ((is-enabled (dist)
             (member (ultralisp/models/dist:dist-name dist)
                     current-dists
                     :key #'ultralisp/models/dist:dist-name
                     :test #'string-equal)))
      (with-html ()
        (reblocks-ui/form:with-html-form
            (:post (lambda (&rest args)
                     (handler-case (save widget args)
                       (asdf-systems-conflict (c)
                         (let ((message (fmt "~A" c)))
                           (setf (dist-conflicts widget)
                                 message)
                           (reblocks/widget:update widget))))))
          (form-error-placeholder)
          (:div :class "border rounded-lg shadow-sm mb-6 overflow-hidden"
                (:div :class "flex justify-between items-center px-4 py-2 bg-gray-50 border-b"
                      (:span :class "font-semibold text-gray-700"
                             (fmt "Type: ~A" type))
                      (:div :class "flex gap-2"
                            (let ((js-code-to-cancel
                                    (make-js-action
                                     (lambda (&rest args)
                                       (declare (ignore args))
                                       (switch-to-readonly widget)))))
                              (:input :type "button"
                                      :class "text-xs px-2 py-1 rounded bg-gray-500 text-white hover:bg-gray-600 cursor-pointer"
                                      :name "button"
                                      :onclick js-code-to-cancel
                                      :value "Cancel"))
                            (:input :type "submit"
                                    :class "text-xs px-2 py-1 rounded bg-green-600 text-white hover:bg-green-700 cursor-pointer"
                                    :name "button"
                                    :value "Save")))
                (:div :class "space-y-3"
                      (:div :class "flex px-4 py-2 gap-3"
                            (:div :class "w-1/4 text-gray-500 font-medium shrink-0" "Source")
                            (:div :class "flex-1"
                                  (:input :value url
                                          :name "url"
                                          :type "text"
                                          :class "w-full border rounded px-2 py-1 text-sm"
                                          :onchange
                                          (make-js-handler
                                           :lisp-code ((&key url &allow-other-keys)
                                                       (update-url (branches widget)
                                                                   url))
                                           :js-code ((event)
                                                     (parenscript:create
                                                      :url (@ event target value)))))))
                      (:div :class "flex px-4 py-2 gap-3"
                            (:div :class "w-1/4 text-gray-500 font-medium shrink-0" "Branch or tag")
                            (:div :class "flex-1"
                                  (render (branches widget))))
                      (:div :class "flex px-4 py-2 gap-3"
                            (:div :class "w-1/4 text-gray-500 font-medium shrink-0"
                                  "Ignore systems in these dirs and ASD files")
                            (:div :class "flex-1"
                                  (:textarea :name "ignore-dirs"
                                             :class "w-full border rounded px-2 py-1 text-sm"
                                             :placeholder "vendor/, my-system-test.asd"
                                             (format-ignore-list
                                               ignore-dirs))))
                      (:div :class "flex px-4 py-2 gap-3"
                            (:div :class "w-1/4 text-gray-500 font-medium shrink-0" "Distributions")
                            (:div :class "flex-1"
                                  (loop for dist in all-dists
                                        for name = (ultralisp/models/dist:dist-name dist)
                                        do (:label :class "inline-flex items-center mr-3"
                                                   (:input :type "checkbox"
                                                           :name "distributions"
                                                           :value name
                                                           :class "mr-1"
                                                           :checked (is-enabled dist))
                                                   name))
                                  (when (dist-conflicts widget)
                                    (:pre :class "text-red-600 text-xs mt-1"
                                          (dist-conflicts widget)))
                                  (error-placeholder "distributions"))))))))))



(defmethod reblocks/widget:render ((widget branch-select-widget))
  (with-html ()
    (:select :name "branch"
             :class "w-full border rounded px-2 py-1 text-sm"
      (:option :disabled "disabled"
               "Select a branch")
      (loop with current = (current-branch widget)
            for branch in (branches widget)
            do (:option :selected (when (string-equal branch
                                                      current)
                                    "selected")
                        branch)))))


(defmethod reblocks/widget:render ((widget source-widget))
  ;; When user hits the refresh button we need to update
  ;; source information because it might be changed
  ;; while page was opened in the browser.
  (when (reblocks/request:refresh-request-p)
    (setf (source widget)
          (get-latest-version-of (source widget))))
  
  (reblocks/widget:render (subwidget widget)))


(defmethod reblocks/widget:render ((widget readonly-source-widget))
  (let* ((source (source (parent widget)))
         (type (ultralisp/models/source:source-type source)))
    (with-html ()
      ;; This piece here to make debugging easier:
      (:p :style "display: none"
          (fmt "source-id=~S version=~S project-id=~S project-version=~S"
               (object-id source)
               (object-version source)
               (source-project-id source)
               (project-version source))))
    (render-source widget type source)))


(defmethod reblocks/widget:render ((widget edit-source-widget))
  (let* ((source (source (parent widget)))
         (type (ultralisp/models/source:source-type source)))
    (render-source widget type source)))


(defmethod reblocks/dependencies:get-dependencies ((widget source-widget))
  (call-next-method))


;; Methods to render changes between source versions

(defmethod ultralisp/protocols/render-changes:render ((type (eql :git)) prev-source new-source)
  (reblocks/html:with-html ()
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
                      (str:shorten length (princ-to-string old-value) :ellipsis "…")
                      (str:shorten length (princ-to-string new-value) :ellipsis "…")))))))


(defmethod ultralisp/protocols/render-changes:render ((type (eql :github)) prev-source new-source)
  ;; Renders for :git and :github are the same. Probably we don't need a generic function here?
  (ultralisp/protocols/render-changes:render :git prev-source new-source))


(defmethod reblocks/widget:render ((widget add-source-widget))
  (let ((project (project widget))
        (user (reblocks-auth/models:get-current-user)))
    (reblocks/html:with-html ()
      (when (ultralisp/protocols/moderation:is-moderator user project)
        (reblocks-ui/form:with-html-form
            (:post (lambda (&rest args)
                     (declare (ignorable args))
                     (let* ((on-new-source (on-new-source widget))
                            (source-type (make-keyword (getf args :type)))
                            (source (ultralisp/models/source:create-source project
                                                                           source-type)))
                       (safe-funcall on-new-source source))))
          (:div :class "flex items-center gap-3"
                (:label :class "text-gray-500 font-medium text-sm whitespace-nowrap"
                        "New source of type:")
                (:select :name "type"
                         :class "border rounded px-2 py-1 text-sm"
                         (:option :selected t
                                  :value "github"
                                  "GitHub")
                         (:option :value "git"
                                  "Git")
                         (:option :value "archive"
                                  :disabled t "Tar Archive (not supported yet)")
                         (:option :value "mercurial"
                                  :disabled t "Mercurial (not supported yet)"))
                (:input :type "submit"
                        :class "text-sm px-3 py-1 rounded bg-sky-600 text-white hover:bg-sky-700 cursor-pointer"
                        :name "button"
                        :value "Add")))))))
