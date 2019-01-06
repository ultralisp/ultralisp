(require 'sly)

;; (defun sly-ultralisp-init ()
;;   (defun sly-make-tramp-file-name (username remote-host lisp-filename)
;;     "Old (with multi-hops) tramp compatability function"
;;     (message "Making tramp2 filename for %s %s and %s" username remote-host lisp-filename)
;;     (tramp-make-tramp-file-name "docker"
;;                                 username
;;                                 nil
;;                                 remote-host
;;                                 nil
;;                                 lisp-filename))

;;   (defun sly-docker-on-connection ()
;;     (add-to-list 'sly-filename-translations
;;                  (sly-create-filename-translator
;;                   :machine-instance "ultralisp_app"
;;                   :username "root"))


;;     (add-to-list 'sly-filename-translations
;;                  (sly-create-filename-translator
;;                   :machine-instance "ultralisp_worker"
;;                   :username "root"))

;;     (let* ((host (sly-machine-instance))
;;            (docker-output (shell-command-to-string "docker ps --format '{{.ID}} {{.Names}}'"))
;;            (containers (split-string docker-output))
;;            (pos (position host containers :test 'string=)))
;;       (when pos
;;         ;; If we found a real container's name, then we'll
;;         ;; replace sly-machine-instance to it to have
;;         ;; nice hosts for tramp.
;;         (let ((container-name (nth (1+ pos) containers)))
;;           (setf (sly-machine-instance)
;;                 container-name)))))
  
;;   (add-hook 'sly-connected-hook 'sly-docker-on-connection))


;; (add-hook 'sly-mode-hook
;;           'sly-ultralisp-init)


(setq sly-contribs '(sly-fancy
                     sly-tramp-docker
                     ;; sly-named-readtables
                     ;; sly-package-inferred
                     ))
