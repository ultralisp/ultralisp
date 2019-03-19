(defpackage #:ultralisp/utils
  (:use #:cl)
  (:import-from #:cl-fad
                #:generate-random-pathname
                #:generate-random-string)
  (:import-from #:trivial-backtrace
                #:print-backtrace)
  (:import-from #:uiop
                #:ensure-absolute-pathname
                #:ensure-directory-pathname
                #:ensure-pathname)
  (:import-from #:uuid
                #:make-v4-uuid)
  (:import-from #:cl-strings
                #:starts-with
                #:split)
  (:import-from #:local-time
                #:timestamp-to-universal)
  (:import-from #:metatilities
                #:format-date)
  (:import-from #:anaphora
                #:aif
                #:it)
  (:import-from #:local-time-duration
                #:duration
                #:timestamp-duration-)
  (:export
   #:time-in-past
   #:getenv
   #:directory-mtime
   #:ensure-absolute-dirname
   #:ensure-existing-file
   #:path-to-string
   #:make-request-id
   #:parse-workers-hosts
   #:make-update-diff
   #:update-plist
   #:format-timestamp
   #:get-traceback
   #:walk-dir
   #:starts-with-slash-p
   #:first-letter-of
   #:remove-last-slash
   #:with-tmp-directory
   #:delete-file-if-exists))
(in-package ultralisp/utils)


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
             (let ((relative (weblocks/utils/misc:relative-path absolute parent-dir)))
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


(defun parse-workers-hosts (string)
  "Parses comma-separated string like that:

   localhost:10100,localhost:10101

   And returns a list:

   '((\"localhost\" 10100)
     (\"localhost\" 10101))
"
  (loop for item in (split string ",")
        for (host port) = (split item ":")
        collect (list host (parse-integer port))))


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
  "Updates `data' plist with items from `update' plist.

   Returns a new plist."
  (let ((result (copy-list data)))
    (loop for (key value) on update by #'cddr
          do (setf (getf result key)
                   value))
    result))


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


(defun get-traceback (condition)
  "Returns a traceback as a string, supressing conditions during printing backtrace itself."
  (handler-bind
      ((error (lambda (condition)
                (declare (ignorable condition))
                #+sbcl
                (let ((skip (find-restart 'sb-debug::skip-printing-frame)))
                  (when skip
                    (invoke-restart skip))))))
    (print-backtrace condition
                     :output nil)))


(defun starts-with-slash-p (s)
  (check-type s string)
  (starts-with s "/"))


(defun first-letter-of (s)
  (check-type s string)
  (when (> (length s) 0)
    (subseq s 0 1)))


(defun remove-last-slash (s)
  (check-type s string)
  (string-right-trim "/" s))


(defun get-tmp-directory-name ()
  (cl-fad:pathname-as-directory
   (translate-logical-pathname
    (generate-random-pathname cl-fad::*default-template*
                              'generate-random-string))))


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
