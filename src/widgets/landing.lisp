(defpackage #:ultralisp/widgets/landing
  (:use #:cl)
  (:import-from #:ultralisp/metadata)
  (:import-from #:weblocks/widget
                #:defwidget
                #:render)
  (:import-from #:ultralisp/models/action)
  (:import-from #:weblocks/html
                #:with-html)
  (:import-from #:ultralisp/models/project
                #:get-url
                #:get-name
                #:get-description
                #:get-all-projects)
  (:import-from #:weblocks/page
                #:get-title)
  (:import-from #:ultralisp/models/version
                #:version
                #:get-built-at
                #:get-number
                #:get-latest-versions)
  (:import-from #:metatilities
                #:format-date)
  (:import-from #:local-time
                #:timestamp-to-universal)
  (:import-from #:ultralisp/models/check
                #:get-changelog)
  (:export
   #:make-landing-widget))
(in-package ultralisp/widgets/landing)


(defwidget landing-widget ()
  ())


(defun make-landing-widget ()
  (make-instance 'landing-widget))


(defgeneric render-action (action)
  (:method ((action t))
    (with-html
      (:li ("Unknown type of action ~A"
            (type-of action)))))
  (:method ((action ultralisp/models/action:project-added))
    (let* ((project (ultralisp/models/action:get-project action))
           (project-name (ultralisp/models/project:get-name project)))
      (with-html
        (:li ("Project ~A was added" project-name))))))


(defun render-version (version)
  (check-type version version)
  
  (let* ((number (get-number version))
         (built-at (get-built-at version))
         (version-type (ultralisp/models/version:get-type version))
         (actions (ultralisp/models/action:get-version-actions version))
         (version-uri (format nil "/versions/~A" number)))
    (with-html
      (:tr
       (:td :style "white-space: nowrap"
            (case version-type
              (:pending
               (:span "No version yet"))
              (t
               (:a :href version-uri
                   number))))
       (:td :style "white-space: nowrap"
            (if built-at
                (format-date "%Y-%m-%d %H:%M:%S"
                             (timestamp-to-universal built-at))
                "Pending"))
       (:td
        (if actions
            (:ul :class "changelog"
                 (mapc #'render-action actions))
            (:ul :class "changelog"
                 (:li "No changes"))))))))


(defmethod render ((widget landing-widget))
  (setf (get-title)
        "Ultralisp - Fast Common Lisp Repository")
  
  (with-html
    (:p "Ultralisp is a quicklisp distribution, updated as fast as you can push commits to the GitHub.")
    (:p "Work in progress, but you already can install some packages, like quickdist or weblocks from this repository.")
    (:h3 "How to use it")
    (:p "To use it, open your Lisp REPL and eval:")
    (:pre "(ql-dist:install-dist \"http://dist.ultralisp.org/\"
                      :prompt nil)")

    (:h3 "How to add my own project")

    (:a :class "button"
        :href "/github"
        :title "Add your projects from Github to Ultralisp distribution!"
        "Select Github projects")

    (:h3 "Roadmap")

    (:ul
     (:li (:s "Plug in a real database to store projects' metadata and other information."))
     (:li (:s "Integration with the GitHub to add projects in one click."))
     (:li "Support for project sources other than GitHub.")
     (:li "Automatic distribution's ChangeLog generation.")
     (:li "Running tests for updated project and all dependent systems."))

    (let ((latest-versions (get-latest-versions))
          (all-projects (get-all-projects :only-enabled t)))
      (when latest-versions
        (with-html
          (:div :class "latest-builds"
                (:h3 "Latest builds")

                (:table :class "versions-list"
                        (:tr
                         (:th "Version")
                         (:th "Built-at")
                         (:th :style "width: 100%"
                              "Changelog"))
                        (mapc #'render-version
                              latest-versions)))))

      (when all-projects
        (with-html
          (:h3 "Projects in the dist")
       
          (:table :class "projects-list"
                  (loop for project in all-projects
                        for description = (get-description project)
                        for url = (get-url project)
                        for name = (get-name project)
                        do (with-html
                             (:tr
                              (:td :style "white-space: nowrap" (:a :href url
                                                                    name))
                              (:td description))))))))))


(defmethod weblocks/dependencies:get-dependencies ((widget landing-widget))
  (append
   (list
    (weblocks-lass:make-dependency
      `(.changelog
        :margin-bottom 0)))
   (call-next-method)))
