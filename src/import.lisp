(defpackage #:ultralisp/import
  (:use #:cl)
  (:import-from #:defmain
                #:defmain)
  (:import-from #:legit)
  (:import-from #:cl-fad
                #:walk-directory)
  (:import-from #:ultralisp/models/project
                #:make-project-from-url)
  (:import-from #:reblocks-auth/models
                #:get-user-by-email)
  (:import-from #:str)
  (:export
   #:main
   #:import-quicklisp))
(in-package #:ultralisp/import)

;; This is not finished tool for fetching projects metadata from main Quicklisp distribution
;; I've intended to make it a periodical task, but really used only once from the REPL.

(defun empty-string-p (s)
  (string= s ""))


(defun parse-line (s)
  (let ((splitted (str:split " " s)))
    (list :source (alexandria:make-keyword (string-upcase (first splitted)))
          :url (second splitted))))


(defun get-source (obj)
  (let ((source (getf obj :source))
        (url (getf obj :url)))
    (cond
      ((search "github.com/" url)
       :the-github)
      ((search "gitlab.com/" url)
        :the-gitlab)
      (t source))))


(defun get-url (obj)
  (getf obj :url))


(defun parse (filename)
  (let* ((content (alexandria:read-file-into-string filename))
         (lines (str:split " " content))
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
             (let* ((parsed (parse filename)))
               (push parsed data))))
      (walk-directory path
                      #'import-dir
                      :test #'is-source-file-p)
      data)))


(defun show-stats (parsed-data)
  (loop with data
        for item in parsed-data
        for source = (get-source item)
        do (incf (getf data source 0))
        finally (return data)))


(defun show-items-of-type (parsed-data type)
  (check-type type keyword)
  (loop for item in parsed-data
        for source = (get-source item)
        when (eql source type)
          collect item))


(defun create-projects (data moderator-email limit)
  (let ((github-items (show-items-of-type data :the-github))
        (moderator (get-user-by-email moderator-email)))
    (unless moderator
      (error "Unable to find moderator with email \"~A\""
             moderator-email))
    
    (loop with 5-min-ago = (ultralisp/utils:time-in-past :minute 15)
          for item in github-items
          for url = (get-url item)
          for project = (make-project-from-url url :moderator moderator)
          for created-at = (mito:object-created-at project)
          when (local-time:timestamp> created-at
                                      5-min-ago)
            do (decf limit)
          when (= 0 limit)
            do (return))))


(defun import-quicklisp (moderator-email limit)
  (ultralisp/utils:with-tmp-directory (path)
    (legit:clone "https://github.com/quicklisp/quicklisp-projects" path)
    (let ((data (import-dir path)))
      (create-projects data moderator-email limit))))

