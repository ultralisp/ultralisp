(defpackage #:ultralisp/widgets/landing
  (:use #:cl)
  (:import-from #:str
                #:concat)
  (:import-from #:40ants-routes/route-url
                #:route-url)
  (:import-from #:ultralisp/widgets/projects)
  (:import-from #:ultralisp/widgets/changelog)
  (:import-from #:ultralisp/widgets/utils
                #:large-header)
  (:import-from #:ultralisp/metadata)
  (:import-from #:reblocks/widget
                #:defwidget)
  (:import-from #:reblocks-ui2/widget
                #:render
                #:ui-widget)
  (:import-from #:reblocks-ui2/themes/tailwind
                #:tailwind-theme)
  (:import-from #:ultralisp/models/action)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:ultralisp/models/project
                #:get-recent-projects
                #:is-enabled-p
                #:get-name
                #:get-description
                #:get-all-projects)
  (:import-from #:reblocks/page
                #:get-title)
  (:import-from #:ultralisp/models/version
                #:version
                #:get-built-at
                #:get-number
                #:get-latest-versions)
  (:import-from #:metatilities
                #:format-date)
  (:import-from #:local-time
                #:now
                #:timestamp-to-universal)
  (:import-from #:ultralisp/models/check
                #:get-pending-checks-for-disabled-projects-count
                #:get-pending-checks-for-disabled-projects
                #:get-pending-checks-count
                #:get-project
                #:get-pending-checks)
  (:import-from #:ultralisp/variables
                 #:get-dist-name
                 #:get-base-url
                 #:*link-color-classes*)
  (:import-from #:ultralisp/models/dist
                #:latest-dists
                #:dist)
  (:import-from #:ultralisp/models/dist-source
                #:dist->sources)
  (:import-from #:ultralisp/models/source
                #:bound-source)
  (:import-from #:ultralisp/models/versioned
                #:prev-version)
  (:import-from #:ultralisp/protocols/render-changes
                #:render-changes)
  (:import-from #:ultralisp/widgets/dist
                #:render-installation-instructions)
  (:import-from #:ultralisp/protocols/enabled
                #:enabled-p)
  (:import-from #:local-time-duration
                #:timestamp-difference)
  (:import-from #:ultralisp/utils/time
                #:humanize-duration
                #:humanize-timestamp)
  (:import-from #:reblocks-ui2/tables/cell
                #:cell)
  (:import-from #:reblocks-ui2/tables/table
                #:column
                #:make-table)
  (:import-from #:reblocks-ui2/html
                #:html)
  (:export
   #:make-landing-widget))
(in-package #:ultralisp/widgets/landing)


(defwidget landing-widget (ui-widget)
  ())


(defun make-landing-widget ()
  (make-instance 'landing-widget))



(defun render-bound-source (source)
  (check-type source bound-source)
  (let* ((project (ultralisp/models/project:source->project source))
         (prev-source (prev-version source))
         (url (ultralisp/protocols/url:url project))
         (name (ultralisp/models/project:project-name project))
         (suffix (cond
                   ((null prev-source)
                    "was added")
                   ((and
                     (enabled-p source)
                     (null (enabled-p prev-source)))
                    "was enabled")
                   ((and
                     (null (enabled-p source))
                     (enabled-p prev-source))
                    "was disabled")
                   ((and
                     (enabled-p source)
                     (enabled-p prev-source))
                    "was changed")
                   (t
                    (case
                        (alexandria:make-keyword
                         (getf (ultralisp/models/source:disable-reason source)
                               :type))
                      (:just-added
                         "was added and waits for a check")
                      (:check-error
                         (if (and
                              (null (enabled-p source))
                              (null (enabled-p prev-source)))
                           "still disabled because of error"
                           "was disabled because of error"))
                      (:manual
                         "was removed manually")
                      (t
                         ""))))))

    (with-html ()
      (:li (:span "Project")
           (:span " ")
           (:a :href url
               :class *link-color-classes*
               name)
           (:span " ")
           (:span suffix)
           
           (when (and
                  prev-source
                  (enabled-p source)
                  (enabled-p prev-source))
             (render-changes prev-source source))))))


(defclass dist-header ()
  ((dist :initarg :dist
         :reader wrapped-dist)))

(defun dist-header (dist)
  (make-instance 'dist-header
                 :dist dist))


(defclass dist-description ()
  ((dist :initarg :dist
         :reader wrapped-dist)))

(defun dist-description (dist)
  (make-instance 'dist-description
                 :dist dist))


(defun dist-name (obj)
  (etypecase obj
    (dist-header
       (let* ((dist (wrapped-dist obj))
              (dist-name (ultralisp/models/dist:dist-name dist))
              (dist-url (ultralisp/protocols/url:url dist)))
          (html
              ((:a :href dist-url
                   :class *link-color-classes*
                   dist-name)))))
    (dist-description
       (let* ((limit 5)
              (dist (wrapped-dist obj))
              (bound-sources (dist->sources dist :this-version t :limit (1+ limit)))
              (has-more (= (length bound-sources)
                           (1+ limit)))
              (bound-sources (subseq bound-sources
                                     0
                                     (min (length bound-sources)
                                          limit))))
         (cell (html
                   ((cond
                      (bound-sources
                       (:ul :class "list-disc pl-6 my-0"
                            (mapc #'render-bound-source
                                  bound-sources))
                       (when has-more
                         (:p "And more...")))
                      (t
                       (:span :class "pl-6" "No changes"))))
                   :css-classes '("text-left"))
               :colspan 3)))))


(defun dist-version (obj)
  (etypecase obj
    (dist-header
       (let* ((dist (wrapped-dist obj))
              (version (ultralisp/models/dist:dist-quicklisp-version dist)))
         (if version
           version
           "No version yet")))
    (dist-description
       nil)))


(defun dist-built-at (obj)
  (etypecase obj
    (dist-header
       (let ((state (ultralisp/models/dist:dist-state (wrapped-dist obj)))
             (built-at (ultralisp/models/dist:dist-built-at (wrapped-dist obj))))
         (if built-at
           (html
               ((:span :title (humanize-timestamp built-at)
                       ("~A ago"
                        (humanize-duration
                         (timestamp-difference (now)
                                               built-at))))))
           (symbol-name state))))
    (dist-description
       nil)))


(defwidget custom-table-row (reblocks-ui2/tables/table:table-row)
  ())


(defmethod reblocks-ui2/tables/table:row-css-classes ((row custom-table-row) (theme tailwind-theme))
  (typecase (reblocks-ui2/tables/table:row-object row)
    (dist-header
       (list
        "bg-stone-100"))
    (t
       nil)))


(defmethod render ((widget landing-widget) (theme tailwind-theme))
  (setf (get-title)
        "Ultralisp - Fast Common Lisp Repository")

  (let ((latest-dists (latest-dists))
        (recent-projects (get-recent-projects)))
    (with-html ()
      (:a :class "github-fork-ribbon left-top"
          :href "https://github.com/ultralisp/ultralisp"
          :data-ribbon "Fork me on GitHub"
          :title "Fork me on GitHub"
          "Fork me on GitHub")

      (:p "Ultralisp is a quicklisp distribution, which updates every 5 minutes.")

      (large-header "How to add my own project?")

      (:a :class "inline-block px-4 py-2 bg-sky-600 text-white rounded hover:bg-sky-700"
          :href (route-url "github")
          :title "Add your projects from Github to Ultralisp distribution!"
          "Add projects from GitHub or other forges")

      (render-installation-instructions (ultralisp/models/dist:common-dist))

      (let ((issues-url "https://github.com/ultralisp/ultralisp/issues"))
        (large-header "Roadmap")

        (:ul :class "list-disc pl-6"
             (:li (:s "Plug in a real database to store projects' metadata and other information."))
             (:li (:s "Integration with the GitHub to add projects in one click."))
             (:li (:s "Automatic distribution's ChangeLog generation."))
             (:li (:s "Support for project sources other than GitHub."))
             (:li "Running tests for updated project and all dependent systems.")
             (:li ("[Add your feature request](~A) at the Github." issues-url)))

        (large-header "How to help")
        (:p "Any help is appreciated. You can:")
        (:ul :class "list-disc pl-6"
             (:li ("[Select an issue](~A) on the GitHub, assign yourself and send a pull request. Issues are marked as \"good first issue\", \"medium\" and \"big story\" to help you to select which impact do you want to make."
                   issues-url))
             (:li "Suggest your own ideas.")
             (:li (:p ("Become a sponsor on [Patreon](https://www.patreon.com/ultralisp) and donate money to support further development:"))
                  (:div :class "my-2"
                        (:a :class "inline-block px-4 py-2 bg-green-600 text-white rounded hover:bg-green-700"
                            :href "https://www.patreon.com/join/ultralisp"
                            "Donate $$$ at Patreon"))
                  (:p ("Grand sponsors will be listed at the bottom of this page and also [this separate page](/sponsors) together with gold sponsors. You can send me a link an logo of your company for the sponsor page.")))))

      (when latest-dists
        (:div :class "mt-6"
               (large-header "Latest builds" :extra-classes "mt-0")

              (make-table (list (column "Name" :getter #'dist-name)
                                (column "Version" :getter #'dist-version)
                                (column "Built-at" :getter #'dist-built-at))
                          (loop for dist in latest-dists
                                collect (dist-header dist)
                                collect (dist-description dist))
                          :row-class 'custom-table-row)))

      (when recent-projects
        (large-header "Recently added projects")
        (ultralisp/widgets/projects:render-projects-list recent-projects)))))
