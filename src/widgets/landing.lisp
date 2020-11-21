(defpackage #:ultralisp/widgets/landing
  (:use #:cl)
  (:import-from #:str
                #:concat)
  (:import-from #:ultralisp/widgets/projects)
  (:import-from #:ultralisp/widgets/changelog)
  (:import-from #:ultralisp/metadata)
  (:import-from #:weblocks/widget
                #:defwidget
                #:render)
  (:import-from #:ultralisp/models/action)
  (:import-from #:weblocks/html
                #:with-html)
  (:import-from #:ultralisp/models/project
                #:get-recent-projects
                #:is-enabled-p
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
                #:get-pending-checks-for-disabled-projects-count
                #:get-pending-checks-for-disabled-projects
                #:get-pending-checks-count
                #:get-project
                #:get-pending-checks)
  (:import-from #:ultralisp/variables
                #:get-dist-name
                #:get-base-url)
  (:import-from #:ultralisp/models/dist
                #:latest-dists
                #:dist)
  (:import-from #:ultralisp/models/dist-source
                #:dist->sources)
  (:import-from #:ultralisp/models/source
                #:bound-source)
  (:export
   #:make-landing-widget))
(in-package ultralisp/widgets/landing)


(defwidget landing-widget ()
  ())


(defun make-landing-widget ()
  (make-instance 'landing-widget))



(defun render-version (version)
  (check-type version version)

  (let* ((number (get-number version))
         (built-at (get-built-at version))
         (version-type (ultralisp/models/version:get-type version))
         (actions (ultralisp/models/action:get-version-actions version :limit 3))
         ;; TODO: create a generic get-uri and define it for a version class
         (version-uri (format nil "/versions/~A" number)))
    (with-html
      (:tr
       (:td :class "version-cell"
            (case version-type
              (:ready
               (:a :href version-uri
                   number))
              (t (:span "No version yet"))))
       (:td :class "timestamp-cell"
            (if built-at
                (format-date "%Y-%m-%d %H:%M:%S UTC"
                             (timestamp-to-universal built-at))
                "Pending")))
      (:tr 
       (:td :class "changelog-cell"
            :colspan 2
            (ultralisp/widgets/changelog:render actions))))))


(defun render-bound-source (source)
  (check-type source bound-source)
  (let* ((project (ultralisp/models/project:source->project source)))
    (with-html
      (:li ("Project ~A was ~A"
            ;; TODO: Add a link to the project
            (ultralisp/models/project:project-name project)
            (cond
              ((ultralisp/models/source:enabled-p source) "enabled")
              (t
               (case
                   (alexandria:make-keyword
                    (getf (ultralisp/models/source:disable-reason source)
                          :type))
                 (:just-added
                  "added and waits for a check")
                 (:check-error
                  "disabled because of error")
                 (t
                  "")))))))))

;;* 
(defun render-dist (dist)
  (check-type dist dist)

  (let* ((dist-name (ultralisp/models/dist:dist-name dist))
         (number (ultralisp/models/dist:dist-quicklisp-version dist))
         (built-at (ultralisp/models/dist:dist-built-at dist))
         (state (ultralisp/models/dist:dist-state dist))
         (bound-sources (dist->sources dist :this-version t))
         ;; TODO: create a generic get-uri and define it for a version class
         (version-uri ""
                      ;; (format nil "/versions/~A" number)
                      )
         )
    (with-html
      (:tr
       (:td :class "name-cell"
            dist-name)
       (:td :class "version-cell"
            (case state
              (:ready
               (:a :href version-uri
                   number))
              (t (:span "No version yet"))))
       (:td :class "timestamp-cell"
            (if built-at
                (format-date "%Y-%m-%d %H:%M:%S UTC"
                             (timestamp-to-universal built-at))
                (symbol-name state)

                ;; "Pending"
                )))
      (:tr 
       (:td :class "changelog-cell"
            :colspan 3
            (:ul :class "changelog"
                 (mapc #'render-bound-source
                       bound-sources)))))))


(defmethod render ((widget landing-widget))
  (setf (get-title)
        "Ultralisp - Fast Common Lisp Repository")

  (let ((latest-versions (get-latest-versions))
        (latest-dists (latest-dists))
        (recent-projects (get-recent-projects)))
    (with-html
      ;; Taken from https://simonwhitaker.github.io/github-fork-ribbon-css/
      (:a :class "github-fork-ribbon left-top"
          :href "https://github.com/ultralisp/ultralisp"
          :data-ribbon "Fork me on GitHub"
          :title "Fork me on GitHub"
          "Fork me on GitHub")

      (:p "Ultralisp is a quicklisp distribution, which updates every 5 minutes.")

      (:h3 "How to add my own project?")

      (:a :class "button"
          :href "/github"
          :title "Add your projects from Github to Ultralisp distribution!"
          "Add projects from Github")
      
      (:h3 "How to use it?")
      
      (let* ((base-url (get-base-url))
             (url (if (search "localhost" base-url)
                      (concat base-url (get-dist-name) ".txt")
                      base-url)))
        (:p "Open your Lisp REPL and eval:")
        (:pre (format nil "(ql-dist:install-dist \"~A\"
                      :prompt nil)"
                      url))
        (:p ("Or if you are using [Qlot](https://github.com/fukamachi/qlot), put these lines in your **qlfile**:"))
        (:pre (format nil "dist ultralisp ~A
ql :all :latest
ultralisp :all :latest"
                      url)))

      (let ((issues-url "https://github.com/ultralisp/ultralisp/issues"))
        (:h3 "Roadmap")

        (:ul
         (:li (:s "Plug in a real database to store projects' metadata and other information."))
         (:li (:s "Integration with the GitHub to add projects in one click."))
         (:li (:s "Automatic distribution's ChangeLog generation."))
         (:li "Support for project sources other than GitHub.")
         (:li "Running tests for updated project and all dependent systems.")
         (:li ("[Add your feature request](~A) at the Github." issues-url)))

        (:h3 "How to help")
        (:p "Any help is appreciated. You can:")
        (:ul
         (:li ("[Select an issue](~A) on the GitHub, assign yourself and send a pull request. Issues are marked as \"good first issue\", \"medium\" and \"big story\" to help you to select which impact do you want to make."
               issues-url))
         (:li "Suggest your own ideas.")
         (:li (:p ("Become a sponsor on [Patreon](https://www.patreon.com/ultralisp) or [Liberapay](https://en.liberapay.com/Ultralisp.org) and donate money to support further development:"))
              (:div :class "donate"
                    (:a :class "button success"
                        :href "https://www.patreon.com/join/ultralisp"
                        "Donate $$$ at Patreon")
                    (:span :style "display: inline-block; width: 2em"
                           " ")
                    (:a :class "button success"
                        :href "https://en.liberapay.com/Ultralisp.org/donate"
                        "Donate $$$ at Liberapay"))
              (:p ("Gold sponsors will be listed at the bottom of this page, and grand sponsors on [this separate page](/sponsors).")))))

      (when latest-versions
        (:div :class "latest-builds"
              (:h3 "Latest builds")

              (:table :class "versions-list"
                      (:tr
                       (:th :class "version-cell"
                            "Version")
                       (:th :class "timestamp-cell"
                            "Built-at"))
                      (mapc #'render-version
                            latest-versions))))
      
      (when latest-dists
        (:div :class "latest-builds"
              (:h3 "Latest builds")

              (:table :class "dist-list"
                      (:tr
                       (:th :class "name-cell"
                            "Name")
                       (:th :class "version-cell"
                            "Version")
                       (:th :class "timestamp-cell"
                            "Built-at"))
                      (mapc #'render-dist
                            latest-dists))))

      (when recent-projects
        (:h3 "Recently added projects")
        (ultralisp/widgets/projects:render-projects-list recent-projects)))))


(defmethod weblocks/dependencies:get-dependencies ((widget landing-widget))
  (append
   (list
    (weblocks-lass:make-dependency
      `(.dist-list
        ((:or .name-cell .version-cell .timestamp-cell)
         :vertical-align top
         :white-space nowrap
         :text-align left)
        (.timestamp-cell
         :width 100%)
        (.changelog-cell
         :padding-left 1.7em
         (.changelog
          :margin 0
          (p :margin 0)
          (.diff
           :margin 0
           (dt :margin 0
               :margin-right 0.6em
               :display inline-block)
           (dd :margin 0
               :display inline-block)))
         (.and-more :margin 0))))
    ;; TODO: remove
    (weblocks-lass:make-dependency
      `(.versions-list
        ((:or .version-cell .timestamp-cell)
         :vertical-align top
         :white-space nowrap
         :text-align left)
        (.timestamp-cell
         :width 100%)
        (.changelog-cell
         :padding-left 1.7em
         (.changelog
          :margin 0
          (p :margin 0)
          (.diff
           :margin 0
           (dt :margin 0
               :margin-right 0.6em
               :display inline-block)
           (dd :margin 0
               :display inline-block)))
         (.and-more :margin 0)))))
   (call-next-method)))
