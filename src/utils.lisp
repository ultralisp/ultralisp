(uiop:define-package #:ultralisp/utils
  (:use #:cl)
  (:import-from #:cl-fad
                #:generate-random-pathname
                #:generate-random-string)
  (:import-from #:alexandria)
  (:import-from #:uiop
                #:ensure-absolute-pathname
                #:ensure-directory-pathname
                #:ensure-pathname)
  (:import-from #:uuid
                #:make-v4-uuid)
  (:import-from #:str)
  (:import-from #:local-time
                #:timestamp-to-universal)
  (:import-from #:metatilities
                #:format-date)
  (:import-from #:anaphora
                #:aif
                #:it)
  (:import-from #:local-time-duration
                #:timestamp-duration+
                #:duration
                #:timestamp-duration-)
  (:import-from #:log4cl-extras/error
                #:print-backtrace)
  (:import-from #:rutils
                #:fmt)
  (:import-from #:reblocks/response
                #:immediate-response)
  (:import-from #:reblocks/utils/misc
                #:relative-path)
  (:import-from #:log)
  (:export #:time-in-past
           #:getenv
           #:directory-mtime
           #:ensure-absolute-dirname
           #:ensure-existing-file
           #:path-to-string
           #:make-request-id
           #:make-update-diff
           #:update-plist
           #:format-timestamp
           #:get-traceback
           #:walk-dir
           #:starts-with-slash-p
           #:first-letter-of
           #:remove-last-slash
           #:with-tmp-directory
           #:delete-file-if-exists
           #:in-repl
           #:with-trace
           #:time-in-future
           #:make-keyword
           #:ends-with-slash-p
           #:run-program))
(in-package #:ultralisp/utils)


(defun getenv (name &optional (default nil))
  "Возвращает значение из переменной окружения или дефолт, если переменная не задана"
  (let ((value (uiop:getenv name)))
    (if value
        (cond 
          ((or (integerp default)
               (floatp default))
           (read-from-string value))
          ((stringp default)
           value)
          (t value))
        default)))


(defun ensure-existing-file (path)
  (ensure-pathname path
                   :want-file t
                   :want-existing t))


(defun ensure-absolute-dirname (path)
  (ensure-directory-pathname
   (ensure-absolute-pathname
    path
    (probe-file "."))))


(defun ensure-directory-or-file (path)
  "If path points to existing directory, add a / to the end if it missing and making it an absolute.
   Otherwise, check if file exists, return an absolute path to this file.
   If neither directory nor path exists, returns nil."
  (aif (cl-fad:directory-exists-p path)
       it
       (probe-file path)))


(defun directory-mtime (path)
  (if (not (fad:directory-pathname-p path))
      (file-write-date path)
      (apply #'max 0 (mapcar #'directory-mtime (fad:list-directory path)))))


(defun path-to-string (pathname)
  (with-output-to-string (s)
    (princ pathname s)))


(defun %walk-dir (path thunk)
  (let* ((path (ensure-directory-or-file path))
        (is-directory (cl-fad:directory-exists-p path))
        (parent-dir (if is-directory
                        path
                        (cl-fad:pathname-directory-pathname path))))

    (flet ((process-file (absolute)
             (let ((relative (relative-path absolute parent-dir)))
               (funcall thunk absolute relative))))
      (if is-directory
          (fad:walk-directory path #'process-file)
          (process-file path)))))


(defmacro walk-dir ((path absolute relative) &body body)
  "Walks a directory calling the body with absolute and relative
   symbols bound to absolute and relative paths of each file.

   Path could point to a file. In this case, body will be
   called once with absolute bound to the full path and relative
   to the file's name."
  `(%walk-dir ,path
              (lambda (,absolute ,relative)
                (declare (ignorable ,absolute ,relative))
                ,@body)))


(defun make-request-id ()
  "Makes unique request-id for usage in logs."
  (uuid:print-bytes nil (make-v4-uuid)))


(defun make-update-diff (data update)
  "Returns a new plist with items which differ in `update' and original `data' plist.

   Each item in a result is a `list' of two values - first value - the data
   plist, second - from the update.

   If some item is absent in the update plist, it is considered missing and replaced with nil."
  (loop for (key update-value) on update by #'cddr
        for data-value = (getf data key)
        unless (equal data-value update-value)
          append (list key
                       (list data-value
                             update-value))))


(defun update-plist (data update)
  "Updates `data` plist with items from `update` plist.

   Returns:
       A new plist. And the second value will be `t` if original plist was changed.
"
  (let ((result (copy-list data))
        (changed nil))
    (loop for (key value) on update by #'cddr
          unless (equal value
                        (getf result key))
          do (setf (getf result key) value
                   changed t))
    (values result
            changed)))


(defun format-timestamp (timestamp)
  (check-type timestamp local-time:timestamp)
  (format-date "%Y-%m-%d %H:%M:%S UTC"
               (timestamp-to-universal timestamp)))


(defun time-in-past (&key (week 0) (day 0) (hour 0) (minute 0) (sec 0) (nsec 0))
  (timestamp-duration-
   (local-time:now)
   (duration :week week
             :day day
             :hour hour
             :minute minute
             :sec sec
             :nsec nsec)))

(defun time-in-future (&key (week 0) (day 0) (hour 0) (minute 0) (sec 0) (nsec 0))
  (timestamp-duration+
   (local-time:now)
   (duration :week week
             :day day
             :hour hour
             :minute minute
             :sec sec
             :nsec nsec)))


(defun starts-with-slash-p (s)
  (check-type s string)
  (str:starts-with-p "/" s))

(defun ends-with-slash-p (s)
  (check-type s string)
  (str:ends-with-p "/" s))


(defun get-traceback (condition)
  "Returns a traceback as a string, supressing conditions during printing backtrace itself."
  (print-backtrace :condition condition
                   :stream nil))


(defun first-letter-of (s)
  (check-type s string)
  (when (> (length s) 0)
    (subseq s 0 1)))


(defun remove-last-slash (s)
  (check-type s string)
  (string-right-trim "/" s))


(defun get-tmp-directory-name ()
  (uiop:ensure-absolute-pathname
   (fmt "temp-~A/"
        (generate-random-string))
   (uiop:temporary-directory)))


(defmacro with-tmp-directory ((path) &body body)
  `(let* ((,path (get-tmp-directory-name)))
     (unwind-protect
          (progn ,@body)
       (when (cl-fad:directory-exists-p ,path)
         (uiop:delete-directory-tree ,path
                                     :validate t)))))


(defun delete-file-if-exists (path)
  "Deletes file if it is exists.

   Returns the same path to allow chaining."
  (when (cl-fad:file-exists-p path)
    (delete-file path))
  path)


(defun in-repl ()
  (and (find-package :slynk-gray)
       (typep *standard-output*
              (uiop:intern* :sly-output-stream
                            :slynk-gray))))


(defmacro with-trace ((code-name) &body body)
    (alexandria:with-gensyms (result)
      `(progn
         (log:debug "TRACE: Calling" ,code-name)
         (let ((,result (handler-bind (((or error
                                            immediate-response)
                                         (lambda (condition)
                                           (log:debug "TRACE: Call to" ,code-name
                                                      "raised" condition))))
                          (multiple-value-list (progn ,@body)))))
           (log:debug "TRACE: Call to" ,code-name
                      "returned this values list:" ,result)
           (values-list ,result)))))


(defun make-keyword (text)
  (alexandria:make-keyword
   (string-upcase text)))


;; TODO: move to a separate ASDF system
(defun reset-random-state ()
  (let ((new-random-state (make-random-state t)))
    (setf *random-state*
          new-random-state)

    (let ((symbol (uiop:find-symbol* "*NAME-RANDOM-STATE*" "CL-FAD" nil)))
      (when symbol
        (setf (symbol-value symbol)
              new-random-state)))))


(define-condition program-failed (error)
  ((args :initarg :args
         :reader program-args)
   (exit-code :initarg :exit-code
              :reader program-exit-code)
   (output :initarg :output
           :reader program-output))
  (:report (lambda (c stream)
             (format stream "Program ~S failed with code ~A. Here is it's output:~%~A"
                     (program-args c)
                     (program-exit-code c)
                     (program-output c)))))


(declaim (ftype (function ((or string list)
                           &key
                           (:directory (or string
                                           pathname))
                           (:ignore-errors-p boolean))
                          (values (or null string)))
                run-program))

(defun run-program (args &key directory ignore-errors-p)
  "Returns a full output of the outer programm or signals an error if exit code is not 0.

   If argument is :ignore-error, then in case of not 0 exit code, NIL will be returned.
   This helpful, if you interested in output only if an utility found something like:

   where qlot

   should return a path or NIL if qlot not found."
  (log:info "Running program" args directory)
  
  (uiop:with-current-directory (directory)
    (multiple-value-bind (full-output stderr code)
        (uiop:run-program args
                          :output '(:string :stripped t)
                          :error-output :output
                          :ignore-error-status t)
      ;; We have both stdout and stderr mixed in stdout var
      (declare (ignore stderr))
      (cond
        ((zerop code)
         (values full-output))
        (t
         (cond
           (ignore-errors-p
            (values nil))
           (t
            (error 'program-failed
                   :exit-code code
                   :output full-output
                   :args args))))))))
