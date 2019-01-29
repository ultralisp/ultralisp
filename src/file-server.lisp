(defpackage #:ultralisp/file-server
  (:use #:cl)
  (:import-from #:weblocks-file-server)
  (:import-from #:ultralisp/variables
                #:get-dist-dir)
  (:export #:make-route))
(in-package ultralisp/file-server)


(defun make-route ()
  (let ((root (get-dist-dir)))
    (uiop:ensure-all-directories-exist (list root))
    (weblocks-file-server:make-route
     :uri "/dist/"
     :root root)))
