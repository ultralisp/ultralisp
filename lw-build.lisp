(in-package "CL-USER")

(load-all-patches)

(require 'asdf)

(defparameter *ql-setup-files*
  (list ".qlot/setup.lisp"
        "/root/quicklisp/setup.lisp"))

(defparameter *ql-install-files*
  (list "/quicklisp.lisp"))

(defun find-file (files)
  (loop for filename in files
        thereis (probe-file filename)))

(let ((setup-file (find-file *ql-setup-files*))
      (install-file (find-file *ql-install-files*)))
  (unless (or setup-file
              install-file)
    (error "QL setup file or quicklisp installation file should be present."))
  
  (cond
    (setup-file
     (format t "Loading ~A~%" setup-file)
     ;; Use installed Quicklisp client:
     (load setup-file))
    (t
     ;; Install Quicklisp client:
     (format t "Loading ~A~%" install-file)
     (load install-file)
     (uiop:symbol-call :quicklisp-quickstart :install))))


(push "./" asdf:*central-registry*)

;; Qlot will require lparallel,
;; but it is broken in Quicklisp 2024-10-12 distribution
;; and can't compile on LispWorks.
;; That is why we need to install Ultralisp dist
;; here. Later we'll change it to the dist
;; mentioned in qlfile.lock by calling qlot:with-local-quicklisp
(unless (find "ultralisp" (ql-dist:all-dists)
              :test #'string-equal
              :key #'ql-dist:name)
  (ql-dist:install-dist "http://dist.ultralisp.org/ultralisp/20250214233001/distinfo.txt"
                        :prompt nil))

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
  
(let* ((app-path (cond
                   ((probe-file "/app/")
                    "/app/worker")
                   (t
                    "lw-worker"))))
  (save-image app-path
              :restart-function 'ultralisp/worker::main
              :multiprocessing t
              :console t))
