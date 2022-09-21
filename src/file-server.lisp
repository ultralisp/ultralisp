(defpackage #:ultralisp/file-server
  (:use #:cl)
  (:import-from #:reblocks-file-server)
  (:export #:make-route))
(in-package #:ultralisp/file-server)


(defun make-route (root uri)
  (uiop:ensure-all-directories-exist (list root))
  (reblocks-file-server:make-route
   :uri uri
   :root root))
