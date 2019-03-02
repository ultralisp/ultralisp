(defpackage #:ultralisp/file-server
  (:use #:cl)
  (:import-from #:weblocks-file-server)
  (:export #:make-route))
(in-package ultralisp/file-server)


(defun make-route (root uri)
  (uiop:ensure-all-directories-exist (list root))
  (weblocks-file-server:make-route
   :uri uri
   :root root))
