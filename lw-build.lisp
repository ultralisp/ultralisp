(in-package "CL-USER")

(load-all-patches)

(require 'asdf)

(load "/quicklisp.lisp")

(quicklisp-quickstart:install)

(push "./" asdf:*central-registry*)

(ql:quickload :qlot)

;; Without this, russian text is loaded incorrectly:

(defun force-utf-8-file-encoding (pathname ef-spec buffer length)
  (declare (ignore pathname buffer length))
  (system:merge-ef-specs ef-spec :utf-8))

;; Not sure why I've decided to add this line
(set-default-character-element-type 'character)

(setf stream::*default-external-format* '(:utf-8 :eol-style :lf))

(setf system:*file-encoding-detection-algorithm*
      '(force-utf-8-file-encoding))

(qlot:with-local-quicklisp ((probe-file #P"ultralisp.asd"))
  (ql:quickload :ultralisp/worker))
  
(let* ((app-path "/app/worker"))
  (save-image app-path
              :restart-function 'ultralisp/worker::main
              :multiprocessing t
              :console t))
