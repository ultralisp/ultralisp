((lisp-mode . ((eval . (let ((local-file (locate-dominating-file default-directory "ultralisp-local.el")))
                         (when local-file
                            (load (expand-file-name "ultralisp-local.el" local-file) nil t)
                            (sly-ultralisp-enable)))))))
