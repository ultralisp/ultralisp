(defpackage #:ultralisp/widgets/changelog
  (:use #:cl)
  (:import-from #:str)
  (:import-from #:weblocks/html
                #:with-html)
  (:import-from #:ultralisp/models/action
                #:get-params
                #:project-updated
                #:project-removed
                #:get-project
                #:project-added)
  (:import-from #:ultralisp/models/project
                #:get-url
                #:get-name)
  (:import-from #:ultralisp/utils
                #:format-timestamp)
  (:import-from #:mito
                #:object-updated-at
                #:object-created-at)
  (:import-from #:ultralisp/models/version
                #:get-built-at
                #:get-number
                #:version)
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


(defgeneric render-object (action &key timestamp)
  (:method ((action t) &key timestamp)
    (with-html
      (:li ("~@[~A - ~]Unknown type of object ~A"
            (when timestamp
              (format-timestamp (object-updated-at action)))
            (type-of action)))))
  
  (:method ((version version) &key timestamp)
    (let* ((number (get-number version))
           (updated-at (object-updated-at version))
           ;; TODO: create a generic get-uri and define it for a version class
           (url (format nil "/versions/~A" number))
           (version-type (ultralisp/models/version:get-type version)))
      (with-html
        (if (eql version-type :ready)
            (:li ("~@[~A - ~]Version [~A](~A)"
                  (when timestamp
                    (format-timestamp updated-at))
                  url number))
            (:li ("~@[~A - ~]Pending version"
                  (when timestamp
                    (format-timestamp updated-at))))))))

  (:method ((action project-added) &key timestamp)
    (let* ((project (get-project action))
           (name (get-name project))
           (url (get-url project)))
      (with-html
        (:li ("~@[~A - ~]Project [~A](~A) was added"
              (when timestamp
                (format-timestamp (object-updated-at action)))
              url name)))))
  
  (:method ((action project-removed) &key timestamp)
    (let* ((project (get-project action))
           (name (get-name project))
           (url (get-url project))
           (params (get-params action))
           (reason (getf params :reason))
           (traceback (getf params :traceback)))
      (with-html
        (:li ("~@[~A - ~]Project [~A](~A) was removed."
              (when timestamp
                (format-timestamp (object-updated-at action)))
              url name
              reason)
             (when reason
               (:span "Reason is: ")
               (if traceback
                   (:a :title traceback reason)
                   (:span reason)))))))
  
  (:method ((action project-updated) &key timestamp)
    (let* ((project (get-project action))
           (name (get-name project))
           (url (get-url project))
           (params (get-params action))
           (diff (getf params :diff)))
      (with-html
        (:li (:p ("~@[~A - ~]Project [~A](~A) was updated"
                  (when timestamp
                    (format-timestamp (object-updated-at action)))
                  url name))
             (:dl :class "diff"
                  (loop for (key (before after)) on diff by #'cddr
                        do (:dt (get-key-name key))
                        when before
                          do (:dd ("~A -> ~A" (prepare-value key before)
                                              (prepare-value key after)))
                        else
                          do (:dd ("set to ~A" (prepare-value key after))))))))))


(defun render (objects &key timestamps)
  (check-type objects (or list null))
  (with-html
    (if objects
        (:ul :class "changelog"
             (loop for object in objects
                   do (render-object object
                                     :timestamp timestamps)))
        (:ul :class "changelog"
             (:li "No changes")))))
