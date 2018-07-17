(defpackage #:ultralisp/widgets/main
  (:use #:cl)
  (:import-from #:weblocks/widget
                #:defwidget
                #:render)
  (:import-from #:weblocks/html
                #:with-html)
  (:export
   #:make-main-widget))
(in-package ultralisp/widgets/main)


(defwidget main-widget ()
  ())


(defun make-main-widget ()
  (make-instance 'main-widget))


(defmethod render ((widget main-widget))
  (with-html
    (:p "Ultralisp is a quicklisp distribution, updated as fast as you can push commits to the GitHub.")
    (:p "Work in progress, but you already can install some packages, like quickdist or weblocks from this repository.")
    (:h3 "How to use it")
    (:p "To use it, open your Lisp REPL and eval:")
    (:pre "(ql-dist:install-dist \"http://dist.ultralisp.org/\"
                   :prompt nil)")))
