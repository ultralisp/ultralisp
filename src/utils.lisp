(defpackage #:ultralisp/utils
  (:use #:cl)
  (:import-from #:cl-fad)
  (:import-from #:uiop
                #:ensure-absolute-pathname
                #:ensure-directory-pathname
                #:ensure-pathname)
  (:import-from #:uuid
                #:make-v4-uuid)
  (:import-from #:cl-strings
                #:split)
  (:import-from #:local-time
                #:timestamp-to-universal)
  (:import-from #:metatilities
                #:format-date)
  (:export
   #:getenv
   #:directory-mtime
   #:ensure-absolute-dirname
   #:ensure-existing-file
   #:path-to-string
   #:make-request-id
   #:parse-workers-hosts
   #:make-update-diff
   #:update-plist
   #:format-timestamp))
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


(defun directory-mtime (path)
  (if (not (fad:directory-pathname-p path))
      (file-write-date path)
      (apply #'max 0 (mapcar #'directory-mtime (fad:list-directory path)))))


(defun path-to-string (pathname)
  (with-output-to-string (s)
    (princ pathname s)))


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
