(defpackage #:ultralisp/widgets/main
  (:use #:cl)
  (:import-from #:ultralisp/metadata)
  (:import-from #:weblocks/widget
                #:defwidget
                #:render)
  (:import-from #:weblocks/html
                #:with-html)
  (:export
   #:make-main-widget))
(in-package ultralisp/widgets/main)


(defwidget main-widget ()
  ())


(defun make-main-widget ()
  (make-instance 'main-widget))


(defmethod render ((widget main-widget))
  (with-html
    (:p "Ultralisp is a quicklisp distribution, updated as fast as you can push commits to the GitHub.")
    (:p "Work in progress, but you already can install some packages, like quickdist or weblocks from this repository.")
    (:h3 "How to use it")
    (:p "To use it, open your Lisp REPL and eval:")
    (:pre "(ql-dist:install-dist \"http://dist.ultralisp.org/\"
                      :prompt nil)")

    (:h3 "How to add my own project")

    (:h4 "Add project's metadata to Ultralisp")
    
    (:p "Right now, adding a new project to the distribution is a manual process.
         To do this, create a new pull-request into the repository:"
        (:a :href "https://github.com/ultralisp/ultralisp-projects"
            "https://github.com/ultralisp/ultralisp-projects"))
    (:p "Your pull request should add another line to a file \"projects.txt\".")

    
    (:h4 "Setup a webhook for automatic distribution update")
    
    (:p "After that, add a webhook to your repository. Go to the url similar to:"
        (:a :href "https://github.com/40ants/defmain/settings/hooks"
            "https://github.com/40ants/defmain/settings/hooks"))
    (:p "and add a URL \"http://ultralisp.org/webhook/github\" as a webhook. This webhook should receive \"push\" payloads as application/json:")

    (:img :src "https://s3-eu-west-1.amazonaws.com/ultralisp-images/ultralisp-webhook.png")

    (:h3 "Roadmap")

    (:ul
     (:li "Plug in a real database to store projects' metadata and other information.")
     (:li "Integration with the GitHub to add projects in one click and authentication.")
     (:li "Support for project sources other than GitHub.")
     (:li "Automatic distribution's ChangeLog generation.")
     (:li "Running tests for updated project and all dependent systems."))

    (:h3 "Projects in the dist")
    
    (:table :class "projects-list"
            (loop for item in (ultralisp/metadata:read-metadata "./projects/projects.txt")
                  do (with-html
                       (:tr
                        (:td :style "white-space: nowrap" (:a :href (format nil "https://github.com/~A"
                                                                            (ultralisp/metadata:get-urn item))
                                                              (ultralisp/metadata:get-urn item)))
                        (:td (ultralisp/metadata:get-description item))))))))
