(defpackage #:ultralisp/widgets/changelog
  (:use #:cl)
  (:import-from #:weblocks/html
                #:with-html)
  (:import-from #:ultralisp/models/action
                #:get-params
                #:project-updated
                #:project-removed
                #:get-project
                #:project-added)
  (:import-from #:ultralisp/models/project
                #:get-name)
  (:export
   #:render
   #:get-key-name
   #:render-action))
(in-package ultralisp/widgets/changelog)


(defgeneric get-key-name (key)
  (:method ((key (eql :last-seen-commit)))
    "commit"))


(defgeneric prepare-value (key value)
  (:method ((key (eql :last-seen-commit)) value)
    (when value
      (str:prune 8 value :ellipsis "…"))))


(defgeneric render-action (action)
  (:method ((action t))
    (with-html
      (:li ("Unknown type of action ~A"
            (type-of action)))))
  
  (:method ((action project-added))
    (let* ((project (get-project action))
           (project-name (get-name project)))
      (with-html
        (:li ("Project ~A was added" project-name)))))
  
  (:method ((action project-removed))
    (let* ((project (get-project action))
           (project-name (get-name project)))
      (with-html
        (:li ("Project ~A was removed" project-name)))))
  
  (:method ((action project-updated))
    (let* ((project (get-project action))
           (project-name (get-name project))
           (params (get-params action))
           (diff (getf params :diff)))
      (with-html
        (:li (:p ("Project ~A was updated" project-name))
             (:dl :class "diff"
                  (loop for (key (before after)) on diff by #'cddr
                        do (:dt (get-key-name key))
                        when before
                          do (:dd ("~A -> ~A" (prepare-value key before)
                                              (prepare-value key after)))
                        else
                          do (:dd ("set to ~A" (prepare-value key after))))))))))


(defun render (actions)
  (check-type actions (or list null))
  (with-html
    (if actions
        (:ul :class "changelog"
             (mapc #'render-action actions))
        (:ul :class "changelog"
             (:li "No changes")))))
