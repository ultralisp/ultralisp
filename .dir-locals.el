((lisp-mode . ((eval . (let ((local-file (locate-dominating-file default-directory "ultralisp-local.el")))
                         (when (and local-file
                                    (not (bound-and-true-p sly-ultralisp--enabled)))
                           (load (expand-file-name "ultralisp-local.el" local-file) nil t)
                           (sly-ultralisp-enable)))))))
