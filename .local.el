

;; (add-to-list 'sly-filename-translations
;;              (sly-create-filename-translator
;;               :machine-instance "72796b623077"
;;               :remote-host "ultralisp_app_run_2"
;;               :username "root"))


;; (defun sly-make-tramp-file-name (username remote-host lisp-filename)
;;   "Old (with multi-hops) tramp compatability function"
;;   (message "Makining tramp filename for %s" lisp-filename)
;;   (tramp-make-tramp-file-name "docker"
;;                               username
;;                               nil
;;                               remote-host
;;                               nil
;;                               lisp-filename))
