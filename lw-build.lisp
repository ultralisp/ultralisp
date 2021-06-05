(in-package "CL-USER")

(load-all-patches)

(require 'asdf)

(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(hcl:change-directory "~/projects/lisp/ultralisp/")

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

(qlot:with-local-quicklisp ((probe-file #P"ultralisp.asd")
                            :central-registry
                            (list (probe-file "~/projects/lisp/cl-gearman/")
                                  ;; Тут патч для :unspecific
                                  (probe-file "~/projects/lisp/quickdist/")
                                  (probe-file "~/projects/lisp/defmain/") 
                                  ;; Без этого жалуется что компонент ASDF не найден
                                  ;; (возможно это повилось после того, как я проапгрейдил ASDF)
                                  (probe-file "~/projects/lisp/asdf/")))
  (ql:quickload :ultralisp/worker))

  
(let* ((app-path (asdf:system-relative-pathname :ultralisp "lw-worker")))
  (save-image app-path
              :restart-function 'ultralisp/worker::main
              :multiprocessing t
              :console t))
