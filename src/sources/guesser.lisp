(uiop:define-package #:ultralisp/sources/guesser
  (:use #:cl))
(in-package #:ultralisp/sources/guesser)


(defvar *hooks* nil)


(defun make-source (url)
  (loop for hook in *hooks*
        thereis (funcall hook url)))
