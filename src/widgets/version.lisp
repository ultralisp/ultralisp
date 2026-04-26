(defpackage #:ultralisp/widgets/version
  (:use #:cl)
  (:import-from #:ultralisp/widgets/projects)
  (:import-from #:reblocks/widget
                #:defwidget)
  (:import-from #:ultralisp/variables)
  (:import-from #:ultralisp/widgets/changelog)
  (:import-from #:ultralisp/models/version
                #:get-version-by-number)
  (:import-from #:ultralisp/widgets/not-found
                #:page-not-found)
  (:import-from #:ultralisp/models/action
                #:get-version-actions)
  (:import-from #:ultralisp/models/project
                #:get-projects)
  (:import-from #:reblocks-ui2/widget
                #:render
                #:ui-widget)
  (:import-from #:reblocks-ui2/themes/tailwind
                #:tailwind-theme)
  (:export
   #:make-version-widget))
(in-package #:ultralisp/widgets/version)


(defwidget version (ui-widget)
  ((number :initarg :number
           :reader get-version-number)))


(defun make-version-widget (number)
  (make-instance 'version :number number))


(defmethod render ((widget version) (theme tailwind-theme))
  (let* ((version-number (get-version-number widget))
         (version (get-version-by-number version-number)))
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
        (:h4 :class "text-xl font-bold"
             ("Version ~A" version-number))

        (:h5 :class "text-lg font-semibold mt-4" "To install:")

        (:pre :class "bg-gray-100 p-2 rounded overflow-x-auto"
              (format nil "(ql-dist:install-dist \"~A\"
                      :prompt nil)" dist-url))
        (:p ("or if you are using the [Qlot](https://github.com/fukamachi/qlot), then add this line into the top of your <code>qlfile</code>:"))
        (:pre :class "bg-gray-100 p-2 rounded overflow-x-auto"
              (format nil "dist ultralisp ~A"
                      dist-url))
        (:p ("and run <code>qlot update</code> in the shell."))

        (when actions
          (:h5 :class "text-lg font-semibold mt-4" "Changes")
          (ultralisp/widgets/changelog:render actions))

        (when projects
          (:h5 :class "text-lg font-semibold mt-4" "Projects")
          (ultralisp/widgets/projects:render-projects-list projects))))))
