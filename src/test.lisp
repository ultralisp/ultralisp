(defpackage #:ultralisp/test
  (:use #:cl)
  (:export
   #:set-project-github-commit))
(in-package #:ultralisp/test)

;; Different utils to simplify manual testing

(defmethod set-project-github-commit ((check ultralisp/models/check:base-check) new-commit)
  (set-project-github-commit (ultralisp/models/check:get-project check)
                             new-commit))

(defmethod set-project-github-commit ((project ultralisp/models/project:project) new-commit)
  (let ((params (ultralisp/models/project:get-params project)))
    (setf (getf params :last-seen-commit)
          new-commit)
    (mito:save-dao project)))
