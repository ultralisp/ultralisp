(defpackage #:ultralisp/import
  (:use #:cl)
  (:import-from #:defmain
                #:defmain))
(in-package ultralisp/import)


(defun empty-string-p (s)
  (string= s ""))


(defun parse-line (s)
  (let ((splitted (cl-strings:split s #\Space)))
    (list :source (alexandria:make-keyword (string-upcase (first splitted)))
          :url (second splitted))))


(defun get-source (obj)
  (let ((source (getf obj :source))
        (url (getf obj :url)))
    (cond
      ((search "github.com" url)
       :just-github)
      ((search "gitlab.com" url)
        :the-gitlab)
      (t source))))

(defun get-url (obj)
  (getf obj :url))


(defun parse (filename)
  (let* ((content (alexandria:read-file-into-string filename))
         (lines (cl-strings:split content #\Newline))
         (lines (remove-if #'empty-string-p lines)))
    ;; (when (> (length lines)
    ;;          1)
    ;;   (with-simple-restart (continue "Ignore all lines except the first one")
    ;;     (error "Num lines in the file is ~A and this is unexpected:~2%~A"
    ;;            (length lines)
    ;;            content)))
    (parse-line (first lines))))


(defun is-source-file-p (filename)
  (check-type filename pathname)
  (and (string-equal (pathname-name filename)
                     "source")
       (string-equal (pathname-type filename)
                     "txt")))


(defun import-dir (path)
  (let (data)
    (flet ((import-dir (filename)
             (let* ((parsed (parse filename))
                    (source (get-source parsed)))
               (incf (getf data source 0)))))
      (cl-fad:walk-directory path
                             #'import-dir
                             :test #'is-source-file-p)
      data)))


(defmain main ()
  (let ((path #P"quicklisp-projects/projects/"))
    (import-dir path)))
