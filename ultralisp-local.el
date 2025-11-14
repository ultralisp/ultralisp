(require 'sly)
(require 'cl-lib)

(let ((filename (expand-file-name ".qlot/log4sly-setup.el")))
  (when (file-exists-p filename)
    (load filename)
    (global-log4sly-mode 1)))

(defconst sly-ultralisp--project-root
  (let* ((path (or load-file-name
                   (and (boundp 'buffer-file-name) buffer-file-name)
                   default-directory))
         (dir (if (file-directory-p path)
                  (expand-file-name path)
                (file-name-directory (expand-file-name path)))))
    (file-name-as-directory
     (or (locate-dominating-file dir ".git")
         dir)))
  "Absolute path to the Ultralisp project root on the host.")

(defconst sly-ultralisp--container-root "/app/"
  "Path inside Docker containers where the Ultralisp project is mounted.")

(defvar sly-ultralisp--enabled nil
  "Non-nil when Ultralisp-specific filename translation is active.")

(defvar sly-ultralisp--previous-to-lisp nil
  "Previous value of `sly-to-lisp-filename-function' before enabling Ultralisp support.")

(defvar sly-ultralisp--previous-from-lisp nil
  "Previous value of `sly-from-lisp-filename-function' before enabling Ultralisp support.")

(defun sly-ultralisp--docker-name-for (machine-instance)
  "Lookup docker container name for MACHINE-INSTANCE ID via `docker ps` output.
Returns nil when MACHINE-INSTANCE does not correspond to a running container."
  (let* ((output (shell-command-to-string "docker ps --format '{{.ID}} {{.Names}}'"))
         (lines (split-string output "\n" t)))
    (cl-loop for line in lines
             for parts = (split-string line " " t)
             for id = (car parts)
             for name = (cadr parts)
             when (string= machine-instance id)
             return name)))

(defun sly-ultralisp--local-to-container (filename)
  "Translate host FILENAME into the path seen inside the Ultralisp container.
If FILENAME is outside `sly-ultralisp--project-root`, return it unchanged."
  (let* ((absolute (expand-file-name filename))
         (root sly-ultralisp--project-root))
    (if (file-in-directory-p absolute root)
        (concat sly-ultralisp--container-root
                (file-relative-name absolute root))
      absolute)))

(defun sly-ultralisp--container-to-local (filename)
  "Translate container FILENAME back into a host filesystem path."
  (if (string-prefix-p sly-ultralisp--container-root filename)
      (expand-file-name (substring filename (length sly-ultralisp--container-root))
                        sly-ultralisp--project-root)
    filename))

(defun sly-ultralisp--ensure-translation (host)
  "Ensure HOST has our Ultralisp filename translator in `sly-filename-translations`."
  (let* ((pattern (concat "^" (regexp-quote host) "$"))
         (existing (cl-remove-if (lambda (entry)
                                   (string= (car entry) pattern))
                                 sly-filename-translations)))
    (setq sly-filename-translations
          (cons (list pattern
                      #'sly-ultralisp--local-to-container
                      #'sly-ultralisp--container-to-local)
                existing))))

(defun sly-ultralisp--to-lisp-filename (filename)
  "Convert host FILENAME into the form expected by the connected Lisp."
  (sly-ultralisp--local-to-container (expand-file-name filename)))

(defun sly-ultralisp--from-lisp-filename (filename)
  "Convert container-side FILENAME paths back to host filesystem paths."
  (sly-ultralisp--container-to-local filename))

(defun sly-ultralisp--configure-docker-connection ()
  "Normalise SLY connection hostname and install filename translators.
Maps container IDs to their names and registers both with translation
functions so file operations work across host and container."
  (let* ((instance (sly-machine-instance))
         (container-name (sly-ultralisp--docker-name-for instance))
         (primary (or container-name instance)))
    (when container-name
      (setf (sly-machine-instance) container-name))
    (dolist (host (cl-remove-duplicates
                   (cl-remove-if-not #'identity
                                     (list primary container-name instance))))
      (when (or container-name
                (string-prefix-p "ultralisp_" host)
                (string-match-p "^[0-9a-f]+$" host))
        (sly-ultralisp--ensure-translation host)))))

(defun sly-ultralisp-enable ()
  "Activate Ultralisp-specific filename translation for the current Emacs session.
Stores previous SLY filename translator functions so they can be restored
with `sly-ultralisp-disable`."
  (interactive)
  (unless sly-ultralisp--enabled
    (setq sly-ultralisp--enabled t)
    (setq sly-ultralisp--previous-to-lisp sly-to-lisp-filename-function)
    (setq sly-ultralisp--previous-from-lisp sly-from-lisp-filename-function)
    (add-hook 'sly-connected-hook #'sly-ultralisp--configure-docker-connection)
    (setq sly-to-lisp-filename-function #'sly-ultralisp--to-lisp-filename)
    (setq sly-from-lisp-filename-function #'sly-ultralisp--from-lisp-filename)
    (when (sly-current-connection)
      (sly-ultralisp--configure-docker-connection))))

(defun sly-ultralisp-disable ()
  "Deactivate Ultralisp-specific filename translation and restore defaults."
  (interactive)
  (when sly-ultralisp--enabled
    (setq sly-ultralisp--enabled nil)
    (remove-hook 'sly-connected-hook #'sly-ultralisp--configure-docker-connection)
    (setq sly-to-lisp-filename-function (or sly-ultralisp--previous-to-lisp
                                            #'convert-standard-filename))
    (setq sly-from-lisp-filename-function (or sly-ultralisp--previous-from-lisp
                                              #'identity))
    (setq sly-ultralisp--previous-to-lisp nil)
    (setq sly-ultralisp--previous-from-lisp nil)))
