(defpackage #:ultralisp/widgets/source
  (:use #:cl)
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
  (:export
   #:make-source-widget))
(in-package ultralisp/widgets/source)


(defwidget readonly-source-widget ()
  ((parent :initarg :parent
           :reader parent)
   (last-check :initarg :check
               :initform nil
               :type (or ultralisp/models/check:check2
                         null)
               :reader last-check)))


(defwidget edit-source-widget ()
  ((parent :initarg :parent
           :reader parent)))


(defwidget source-widget ()
  ((source :initarg :source
           :accessor source)
   (subwidget :initform nil
              :accessor subwidget)))


(defun edit (widget)
  (check-type widget readonly-source-widget)
  (log:debug "Switching to the edit widget")
  (let* ((main (parent widget))
         (subwidget (make-instance 'edit-source-widget
                                   :parent main)))
    (setf (slot-value main 'subwidget)
          subwidget)
    (weblocks/widget:update main)))


(defun save (widget args)
  (check-type widget edit-source-widget)

  (log:debug "Switching to the view widget")
  
  (let* ((parent (parent widget))
         (source (source parent))
         (subwidget (make-instance 'readonly-source-widget
                                   :parent parent
                                   :check (get-last-source-check source))))

    (loop with url = (getf args :url)
          for (name value) on args by #'cddr
          when (member name '(:distributions :distributions[]))
          collect value into new-dist-names
          finally
             (log:info "Saving" new-dist-names url)
             (let ((new-source (update-source-dists source :dists new-dist-names)))
               (setf (source parent)
                     new-source)))
    
    (setf (slot-value parent 'subwidget)
          subwidget)
    (weblocks/widget:update parent)))


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
         (enabled (ultralisp/models/dist-source:enabled-p dist-source))
         (class (if enabled
                    "dist enabled"
                    "dist disabled"))
         (reason (unless enabled
                   ;; TODO: create a special pretty-printer for disable reason.
                   ;; and probably make a popup with the traceback.
                   (fmt "Disabled: ~A"
                        (ultralisp/models/dist-source:disable-reason dist-source)))))
    (with-html
      (:span :class class
             :title reason
             name))))


(defgeneric render-source (widget type source))


(defmethod render-source ((widget readonly-source-widget)
                          (type (eql :github))
                          source)
  (let* ((params (ultralisp/models/source:source-params source))
         (deleted (ultralisp/models/source:deleted-p source))
         (url (format nil "https://github.com/~A/~A"
                      (getf params :user-or-org)
                      (getf params :project)))
         (last-seen-commit (getf params :last-seen-commit))
         (release-info (ultralisp/models/source:source-release-info source))
         (distributions (source-distributions source)))
    ;; Deleted sources should not be in the list
    ;; for rendering.
    (assert (not deleted))
    
    (with-html
      (:table
       (:tr (:td "Type")
            (:td type))
       (:tr (:td "Source")
            (:td (:a :href url
                     url)))
       (:tr (:td "Branch or tag")
            ;; TODO: make this editable
            (:td "master (not editable yet)"))
       (when last-seen-commit
         (:tr (:td "Last seen commit")
              (:td (:a :href (fmt "~A/commit/~A" url last-seen-commit)
                       last-seen-commit))))
       (when release-info
         (:tr (:td "Release")
              (:td (:a :href (quickdist:get-project-url release-info)
                       (quickdist:get-project-url release-info)))))
       (:tr (:td "Distributions")
            (:td (mapc #'render-distribution
                       distributions)))
       
       (when (last-check widget)
         (:tr (:td "Last check")
              (let* ((check (last-check widget))
                     (processed-at (ultralisp/models/check:get-processed-at check)))
                (:td ("~A~A"
                      (if processed-at
                          (format nil "Finished at ~A." processed-at)
                          "Waiting in the queue.")
                      (if (and (ultralisp/models/check:get-processed-at check)
                               (ultralisp/models/check:get-error check))
                          (format nil " There was an error.")
                          ""))))))
       
       (when (ultralisp/protocols/moderation:is-moderator
              (weblocks-auth/models:get-current-user)
              (ultralisp/models/project:source->project source))
         (:tr (:td :colspan 2
                   (weblocks-ui/form:with-html-form
                       (:post (lambda (&rest args)
                                (declare (ignorable args))
                                (edit widget)))
                     (:input :type "submit"
                             :class "button float-right"
                             :name "button"
                             :value "Edit")))))))))


(defmethod render-source ((widget edit-source-widget)
                          (type (eql :github))
                          source)
  (let* ((params (ultralisp/models/source:source-params source))
         (deleted (ultralisp/models/source:deleted-p source))
         (url (format nil "https://github.com/~A/~A"
                      (getf params :user-or-org)
                      (getf params :project)))
         (last-seen-commit (getf params :last-seen-commit))
         ;; (distributions (source-distributions source))
         (user (weblocks-auth/models:get-current-user))
         (user-dists (ultralisp/models/dist-moderator:moderated-dists user))
         (all-dists (cons (ultralisp/models/dist:common-dist)
                          user-dists))
         (current-dists (remove-if-not
                         #'ultralisp/models/dist:enabled-p
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
           (:tr (:td "Type")
                (:td type))
           (:tr (:td "Source")
                (:td (:a :href url
                         url))
                ;; TODO: maybe add url editing some day:
                ;; (:td (:input :value url
                ;;              :name "url"
                ;;              :type "text"))
                )
           (:tr (:td "Branch or tag")
                ;; TODO: make this editable
                (:td "master (not editable yet)"))
           (when last-seen-commit
             (:tr (:td "Last seen commit")
                  (:td (:a :href (fmt "~A/commit/~A" url last-seen-commit)
                           last-seen-commit))))
           (when release-info
             (:tr (:td "Release")
                  (:td (:a :href (quickdist:get-project-url release-info)
                           (quickdist:get-project-url release-info)))))
           (:tr (:td "Distributions")
                (:td
                 (loop for dist in all-dists
                       for name = (ultralisp/models/dist:dist-name dist)
                       do  (:input :type "checkbox"
                                   :name "distributions"
                                   :value name
                                   :checked (is-enabled dist)
                                   (:label name)))))
           (:tr (:td :colspan 2
                     (:input :type "submit"
                             :class "button float-right"
                             :name "button"
                             :value "Save")))))))))


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
        ((:and .dist .disabled) :color "gray"))))
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
           for name = (getf key-to-name key key)
           for length = (getf key-to-length key 20)
           unless (equal old-value new-value)
           do (:li ("**~A** ~A ➞ ~A"
                    name
                    (str:shorten length old-value :ellipsis "…")
                    (str:shorten length new-value :ellipsis "…")))))))
