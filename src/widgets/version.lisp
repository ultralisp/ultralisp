(defpackage #:ultralisp/widgets/version
  (:use #:cl)
  (:import-from #:ultralisp/widgets/projects)
  (:import-from #:reblocks/request)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:ultralisp/variables)
  (:import-from #:ultralisp/widgets/changelog)
  (:import-from #:ultralisp/models/version
                #:get-version-by-number)
  (:import-from #:cl-ppcre
                #:all-matches-as-strings)
  (:import-from #:ultralisp/widgets/not-found
                #:page-not-found)
  (:import-from #:ultralisp/models/action
                #:get-version-actions)
  (:import-from #:ultralisp/models/project
                #:get-projects)
  (:export
   #:make-version-widget))
(in-package #:ultralisp/widgets/version)


(defwidget version ()
  ())


(defwidget old-version ()
  ())


(defmethod render ((widget old-version))
  ;; Old version urlslike /versions/20210111123844 are not
  ;; available anymore
  (page-not-found))


(defmethod render ((widget version))
  (let* ((path (reblocks/request:get-path))
         (version-number (first (all-matches-as-strings "\\d+" path)))
         (version (when version-number
                    (get-version-by-number version-number))))
    (unless version
      (page-not-found))
    
    (let ((actions (get-version-actions version))
          (projects (get-projects version))
          (dist-url (format nil
                            "~A~A/~A/distinfo.txt"
                            (ultralisp/variables:get-base-url)
                            (ultralisp/variables:get-dist-name)
                            version-number)))
      (reblocks/html:with-html ()
        (:h4 ("Version ~A" version-number))
        
        (:h5 "To install:")
        
        (:pre (format nil "(ql-dist:install-dist \"~A\"
                      :prompt nil)" dist-url))
        (:p ("or if you are using the [Qlot](https://github.com/fukamachi/qlot), then add this line into the top of your <code>qlfile</code>:"))
        (:pre (format nil "dist ultralisp ~A"
                      dist-url))
        (:p ("and run <code>qlot update</code> in the shell."))
        
        (when actions
          (:h5 "Changes")
          (ultralisp/widgets/changelog:render actions))

        ;; TODO: probably this is not a good idea
        ;;       to show all 1500 projects here.
        (when projects
          (:h5 "Projects")
          (ultralisp/widgets/projects:render-projects-list projects))))))


(defun make-version-widget ()
  (make-instance 'old-version))
