(defpackage #:ultralisp/packages-extractor
  (:use #:cl)
  (:export
   #:get-packages))
(in-package ultralisp/packages-extractor)


(defvar *component-packages*)
(defvar *current-system*)

(defvar *orig-standard-output* *standard-output*)
(defvar *orig-error-output* *error-output*)


(defun get-system-packages (system-designator)
  (let* ((target-system (asdf:find-system system-designator))
         (target-system-name (asdf:component-name target-system)))
    (loop for system being the hash-keys in *component-packages*
            using (hash-value packages)
          when (and system
                    (string-equal target-system-name
                                  (asdf:component-name system)))
            appending packages)))


(defmethod asdf:perform-with-restarts :before ((operation asdf:prepare-op)
                                               (component asdf:system))
  (when (boundp '*component-packages*)
    (push component *current-system*)))


(defun make-maxroexpand-hook (prev-hook)
  (lambda (expander form env)
    (when (and (boundp '*component-packages*)
               (consp form)
               (or (eq (first form)
                       'cl:defpackage)
                   (eq (first form)
                       'uiop:define-package))
               (ignore-errors (string (second form))))
      (push (second form)
            (gethash (car *current-system*)
                     *component-packages*)))
    (funcall prev-hook expander form env)))


(defmethod asdf:perform-with-restarts :after ((operation asdf:load-op)
                                              (component asdf:system))
  (when (boundp '*component-packages*)
    (pop *current-system*)))


(defmacro with-packages-collected (&body body)
  "During execution of the body, all created packages will be collected
   into the map.

   Use function load-system-and-return-packages to know which packages
   was created by the system."
  `(let ((*component-packages* (make-hash-table))
         (asdf:*user-cache* (merge-pathnames "temp-asdf-cache/"
                                             (uiop:temporary-directory)))
         (*macroexpand-hook* (make-maxroexpand-hook *macroexpand-hook*))
         *current-system*)
     (unwind-protect
          (progn ,@body)
       (uiop:delete-directory-tree asdf:*user-cache*
                                   :validate t))))


(defmacro with-fake-packages-collected ((fake) &body body)
  "For debugging. "
  `(let ((*component-packages* ,fake)
         *current-system*)
     ,@body))


(defun load-system-and-return-packages (system-name)
  (unless (boundp '*component-packages*)
    (error "Use this function inside the with-packages-collected macro"))

  ;; Probably, this is a package inferred system
  ;; then we need to load a parent system because
  ;; it can have some required dependencies, like ningle do.
  (let* ((*standard-output* (make-broadcast-stream))
         (*error-output* (make-broadcast-stream))
         (*trace-output* *error-output*)
         (*debug-io* (make-two-way-stream (make-string-input-stream "")
                                          *error-output*))
         )
    (when (find #\/ system-name)
      (ignore-errors
       (ql:quickload (asdf:primary-system-name system-name)
                     :silent t)))

    (ql:quickload system-name :silent t))

  (values (get-system-packages system-name)
          *component-packages*))


(defun hash-to-alist (table)
  "Returns an association list containing the keys and values of hash table
TABLE."
  (let ((alist nil))
    (maphash (lambda (k v)
               (push (cons k v) alist))
             table)
    alist))

(defun main ()
  "Extracts documentation from all given systems and pushes this documentation
   to the elastic search."
  (with-packages-collected
    (handler-bind ((asdf:load-system-definition-error
                     (lambda (c)
                       (format *orig-error-output* "ERROR: ~A~%" c)
                       (uiop:quit 1)))
                   (error
                     (lambda (c)
                       (declare (ignorable c))
                       (format *orig-error-output*
                               "Condition was caught: ~A~2%" c)
                       (sb-debug:print-backtrace :stream *orig-error-output*)
                       (uiop:quit 2))))
      (loop for system-name in (uiop:command-line-arguments)
            for packages = (load-system-and-return-packages system-name)
            do (format t "~A~{ ~A~}~%"
                       system-name
                       (mapcar #'package-name packages))
            finally (when (uiop:getenv "DEBUG_SYSTEMS")
                      (format *orig-error-output*
                              "~2&ALL: ~A~%"
                              (hash-to-alist *component-packages*)))))))

