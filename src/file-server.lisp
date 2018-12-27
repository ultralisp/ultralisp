(defpackage #:ultralisp/file-server
  (:use #:cl)
  (:import-from #:weblocks-file-server)
  (:export #:make-route))
(in-package ultralisp/file-server)


                     ;; (uri "/dist/")
                     ;; (root "./build/dist/")

(defun make-route ()
  (weblocks-file-server:make-route))
