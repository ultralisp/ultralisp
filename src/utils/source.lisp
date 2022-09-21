(defpackage #:ultralisp/utils/source
  (:use #:cl)
  (:import-from #:ultralisp/utils/text
                #:multi-split)
  (:import-from #:str
                #:starts-with-p
                #:ends-with-p)
  (:export
   #:make-file-ignorer
   #:format-ignore-list
   #:parse-ignore-list))
(in-package #:ultralisp/utils/source)


(defun parse-ignore-list (input)
  "Returns a sorted list of directories and files.

   Each directory will be relative, leading
   backslash will be stripped and a new backslash
   will be appended to the end, it it is missing.
   But if entry ends with .asd extension, then
   backslash will not be added. This way it is
   possible to ignore separate asd files.

   Input should be a text which can contain directories
   either comma or newline separated.

   Empty items will be omitted."
  (check-type input string)

  (loop for path in (multi-split '(#\Newline #\,) input)
        for path1 = (string-left-trim '(#\/) path)
        for path2 = (cond
                      ((ends-with-p "/" path1)
                       path1)
                      ((ends-with-p ".asd" path1)
                       path1)
                      (t
                       (concatenate 'string path1 "/")))
        collect path2 into paths
        finally (return (sort paths
                              #'string<))))


(defun format-ignore-list (paths)
  (format nil "~{~A~^, ~}"
          paths))


(defun make-file-ignorer (input)
  "Returns a function which accepts a single string filename, relative
   to the data source dir, and returns T if this file should be ignored.

   If there is no directories in the input, then "
  (let ((dirs (etypecase input
                (string (multi-split '(#\Newline #\,) input))
                (list input))))
    (if dirs
        (lambda (filename)
          (loop for dir in dirs
                  thereis (starts-with-p dir filename)))
        ;; This way we'll not ignore nested asd files by default.
        ;; We need this because of issue:
        ;; https://github.com/ultralisp/ultralisp/issues/55
        (constantly nil))))
