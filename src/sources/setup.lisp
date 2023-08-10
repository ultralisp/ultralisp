(uiop:define-package #:ultralisp/sources/setup
  (:use #:cl)
  (:import-from #:ultralisp/sources/git
                #:guess-git-source)
  (:import-from #:ultralisp/sources/github
                #:guess-github-source)
  (:import-from #:ultralisp/sources/guesser
                #:*hooks*))
(in-package #:ultralisp/sources/setup)


(defun setup-sources ()
  (setf *hooks*
        (list 'guess-github-source
              'guess-git-source)))
