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
  (:export
   #:getenv
   #:directory-mtime
   #:ensure-absolute-dirname
   #:ensure-existing-file
   #:path-to-string
   #:make-request-id
   #:parse-workers-hosts))
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
        collect (list host port)))
