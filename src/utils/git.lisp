(uiop:define-package #:ultralisp/utils/git
  (:use #:cl)
  (:import-from #:legit
                #:*git-output*
                #:git-error)
  (:import-from #:str
                #:split))
(in-package #:ultralisp/utils/git)


(legit::define-git-wrapper git-ls-remote url &optional reference &key heads)


(defun probe-git-url (url)
  (handler-case
      (let ((*git-output* (make-broadcast-stream)))
        (git-ls-remote url :reference "HEAD"))
    (git-error ()
      NIL)))


(defun get-git-branches (url)
  "Returns two values: a list of branches and a name of the main branch."
  (when url
    (let ((main-branch-hash
            (first
             (split #\Tab
                    (with-output-to-string (*git-output*)
                      (git-ls-remote url :reference "HEAD")))))
          (output (with-output-to-string (*git-output*)
                    (git-ls-remote url :heads t))))
      (flet ((trim-ref (reference)
               (subseq ;; trimming refs/heads/ from reference name
                reference
                11)))
        (loop with main-branch = nil
              for line in (split #\Newline output)
              for (hash reference) = (split #\Tab line)
              when (string= hash main-branch-hash)
              do (setf main-branch
                       (trim-ref reference))
              unless (string= line "")
              collect (trim-ref
                       reference)
              into branches
              finally (return (values branches main-branch)))))))
