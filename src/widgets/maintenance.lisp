(defpackage #:ultralisp/widgets/maintenance
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:defwidget)
  (:import-from #:global-vars
                #:define-global-var)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/page
                #:get-title)
  (:export
   #:render-maintenance-page
   #:make-maintenance-widget))
(in-package #:ultralisp/widgets/maintenance)


(define-global-var *default-lock-filename*
  "/tmp/maintenance.lock")


(defwidget maintenance ()
  ((child :initarg :child
          :type reblocks/widget:widget)
   (lock-filename :initarg :lock-filename
                  :type pathname))
  (:documentation "This widget should be used as a toplevel widget.

                   It will show \"Temporarily out of service\" if there is a file /tmp/maintenance.lock"))


(defun make-maintenance-widget (child-widget &key (lock-filename *default-lock-filename*))
  (check-type child-widget reblocks/widget:widget)
  (make-instance 'maintenance
                 :child child-widget
                 :lock-filename (merge-pathnames lock-filename)))


(defgeneric render-maintenance-page (widget)
  (:method ((widget maintenance))
    (let ((title "Temporarily out of service"))
      (setf (get-title)
            title)
      (with-html
        (:h1 title)
        (:p "Site is under maintenance.")
        (:p "We'll bring it back as soon as possible.")))))


(defmethod reblocks/widget:render ((widget maintenance))
  (with-slots (lock-filename child) widget
    (let ((lock-exists (probe-file lock-filename)))
      (if lock-exists
          (render-maintenance-page widget)
          (reblocks/widget:render child)))))
