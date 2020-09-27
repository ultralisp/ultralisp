(defpackage #:ultralisp/widgets/source
  (:use #:cl)
  (:import-from #:weblocks/widget
                #:defwidget
                #:render)
  (:import-from #:ultralisp/models/source
                #:source-distributions)
  (:import-from #:ultralisp/protocols/external-url
                #:external-url)
  (:import-from #:rutils
                #:fmt)
  (:import-from #:weblocks/html
                #:with-html)
  (:import-from #:ultralisp/models/dist
                #:dist-name
                #:dist-source->dist)
  (:export
   #:make-source-widget))
(in-package ultralisp/widgets/source)


(defwidget source-widget ()
  ((source :initarg :source
           :reader source)))


(defun make-source-widget (source)
  (make-instance 'source-widget
                 :source source))


(defun render-distribution (dist-source)
  (check-type dist-source ultralisp/models/dist-source:dist-source)
  (let* ((dist (dist-source->dist dist-source))
         (name (dist-name dist))
         (enabled (ultralisp/models/dist-source:enabled-p dist-source))
         (class (if enabled
                    "enabled"
                    "disabled")))
    (with-html
      (:span :class class
             name))))


(defgeneric render-source (type source))


(defmethod render-source ((type (eql :github)) source)
  (let* ((params (ultralisp/models/source:source-params source))
         (deleted (ultralisp/models/source:deleted-p source))
         (url (format nil "https://github.com/~A/~A"
                      (getf params :user-or-org)
                      (getf params :project)))
         (last-seen-commit (getf params :last-seen-commit))
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
       (:tr (:td "Distributions")
            (:td (mapc #'render-distribution
                       distributions)))))))


(defmethod weblocks/widget:render ((widget source-widget))
  (let* ((source (source widget))
         (type (ultralisp/models/source:source-type source)))
    (render-source type source)))
