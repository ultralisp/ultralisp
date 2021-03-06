(defpackage #:ultralisp/packages-extractor
  (:use #:cl)
  (:export
   #:get-packages))
(in-package ultralisp/packages-extractor)


(defvar *component-packages*)
(defvar *current-system*)

(defvar *orig-standard-output* *standard-output*)
(defvar *orig-error-output* *error-output*)

(defparameter *timeout* 60
  "Package extractor will interrupt the loading and consider that system has no any packages
   if it is unable to load the system in this amount of seconds.")


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
      (pushnew (second form)
               (gethash (car *current-system*)
                        *component-packages*)
               :test #'string-equal))
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
       (when (probe-file asdf:*user-cache*)
         (uiop:delete-directory-tree asdf:*user-cache*
                                     :validate t)))))


(defmacro with-fake-packages-collected ((fake) &body body)
  "For debugging. "
  `(let ((*component-packages* ,fake)
         *current-system*)
     ,@body))


(defun load-forced (name)
  "Try to load the system named by NAME, automatically loading any
Quicklisp-provided systems first, and catching ASDF missing
dependencies too if possible."
  (setf name (string-downcase name))
  (let ((*standard-output* (make-broadcast-stream))
        (*trace-output* (make-broadcast-stream)))
    (tagbody
     retry
       (handler-case
           (asdf:load-system name
                             :force t
                             :verbose nil)
         (asdf:missing-dependency (c)
           (let (;; (parent (asdf::missing-required-by c))
                 (missing (asdf::missing-requires c)))
             (ql:quickload missing :silent t)
             (go retry)))))))


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
                                          *error-output*)))
    (when (find #\/ system-name)
      (ignore-errors
       (load-forced (asdf:primary-system-name system-name))))

    ;; Надо заюзать внутренности quickload, но загружать только зависимости
    ;; А потом делать:
    (load-forced system-name))

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


(defun inner-main (&rest systems)
  "Extracts documentation from all given systems and pushes this documentation
   to the elastic search."
  (with-packages-collected
      (handler-bind ((asdf:load-system-definition-error
                       (lambda (c)
                         (format *orig-error-output* "ERROR: ~A~%" c)
                         (uiop:quit 1)))
                     (sb-ext:timeout
                       (lambda (c)
                         (declare (ignorable c))
                         (format *orig-error-output*
                                 "Timeout: unable to load system in ~A seconds"
                                 *timeout*)
                         (uiop:quit 3)))
                     (error
                       (lambda (c)
                         (declare (ignorable c))
                         (format *orig-error-output*
                                 "Condition was caught: ~A~2%" c)
                         (sb-debug:print-backtrace :stream *orig-error-output*)
                         (uiop:quit 2))))
        (sb-ext:with-timeout *timeout*
          (loop for system-name in systems
                for packages = (load-system-and-return-packages system-name)
                do (format t "~A~{ ~A~}~%"
                           system-name
                           (mapcar #'package-name packages))
                finally (when (uiop:getenv "DEBUG_SYSTEMS")
                          (format *orig-error-output*
                                  "~2&ALL: ~A~%"
                                  (hash-to-alist *component-packages*))))))))

(defun main ()
  "Extracts documentation from all given systems and pushes this documentation
   to the elastic search."
  (apply #'inner-main
         (uiop:command-line-arguments)))



