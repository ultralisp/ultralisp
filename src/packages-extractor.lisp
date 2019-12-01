(defpackage #:ultralisp/packages-extractor
  (:use #:cl)
  (:export
   #:get-packages))
(in-package ultralisp/packages-extractor)


(defvar *component-packages*)
(defvar *current-system*)
(defvar *packages-before*)


(defun get-system-packages (system-designator)
  (let* ((target-system (asdf:find-system system-designator))
         (target-system-name (asdf:component-name target-system)))
    (loop for system being the hash-keys in *component-packages*
            using (hash-value packages)
          for primary-system = (asdf:primary-system-name system)
          when (string-equal target-system-name primary-system)
            appending packages)))


(defmethod asdf:perform-with-restarts :before ((operation asdf:prepare-op)
                                               (component asdf:system))
  (when (boundp '*component-packages*)
    (setf *current-system* component)
    (setf *packages-before*
          (list-all-packages))))


(defmethod asdf:perform-with-restarts :after ((operation asdf:load-op)
                                              (component asdf:system))
  (when (boundp '*component-packages*)
    (unless (eql component *current-system*)
      (error "System \"~A\" is not previosly processed \"~A\""
             component
             *current-system*))
  
    (let ((component-packages (set-difference (list-all-packages)
                                              *packages-before*)))
      (when component-packages
        (setf (gethash component *component-packages*)
              component-packages)
        (format t "~A packages registered~%"
                (length component-packages))))
    (setf *packages-before*
          nil)
    (setf *current-system* nil)))


(defmacro with-packages-collected (&body body)
  "During execution of the body, all created packages will be collected
   into the map.

   Use function load-system-and-return-packages to know which packages
   was created by the system."
  `(let ((*component-packages* (make-hash-table))
         *current-system*
         *packages-before*)
     ,@body))


(defmacro with-fake-packages-collected ((fake) &body body)
  "For debugging. "
  `(let ((*component-packages* ,fake)
         *current-system*
         *packages-before*)
     ,@body))


(defun load-system-and-return-packages (system-name)
  (unless (boundp '*component-packages*)
    (error "Use this function inside the with-packages-collected macro"))
  
  ;; Probably, this is a package inferred system
  ;; then we need to load a parent system because
  ;; it can have some required dependencies, like ningle do.
  (when (find #\/ system-name)
    (ignore-errors
     (ql:quickload (asdf:primary-system-name system-name)
                   :silent t)))
  
  (ql:quickload system-name :silent t)
  
  (values (get-system-packages system-name)
          *component-packages*))


(defun main ()
  "Extracts documentation from all given systems and pushes this documentation
   to the elastic search."
  (with-packages-collected
    (handler-bind ((asdf:load-system-definition-error
                     (lambda (c)
                       (format *error-output* "ERROR: ~A~%" c)
                       (uiop:quit 1))))
      (loop for system-name in (uiop:command-line-arguments)
            for packages = (load-system-and-return-packages system-name)
            do (format t "~A~{ ~A~}~%"
                       system-name
                       (mapcar #'package-name packages))))))

