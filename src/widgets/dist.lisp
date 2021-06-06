(defpackage #:ultralisp/widgets/dist
  (:use #:cl)
  (:import-from #:weblocks/request)
  (:import-from #:ultralisp/protocols/url)
  (:import-from #:ultralisp/protocols/external-url)
  (:import-from #:ultralisp/models/dist)
  (:import-from #:weblocks/widget
                #:defwidget)
  (:import-from #:ultralisp/widgets/not-found
                #:page-not-found)
  (:import-from #:cl-ppcre
                #:register-groups-bind)
  (:import-from #:ultralisp/variables
                #:get-base-url)
  (:import-from #:ultralisp/protocols/moderation
                #:is-moderator)
  (:import-from #:weblocks/page
                #:get-title)
  (:export
   #:make-dist-widget
   #:render-installation-instructions))
(in-package ultralisp/widgets/dist)


(defwidget dist-widget ()
  ((name :initform nil
         :reader dist-name)
   (dist :initform nil
         :reader dist))
  (:documentation ""))


(defun make-dist-widget ()
  (make-instance 'dist-widget))


(defmethod (setf dist-name) (new-name (widget dist-widget))
  (unless (equal (slot-value widget 'name)
                 new-name)
    (let ((new-dist (ultralisp/models/dist:find-dist new-name
                                                        :raise-error nil)))
      (setf (slot-value widget 'name)
            new-name
            (slot-value widget 'dist)
            new-dist))))


(defun render-installation-instructions (dist)
  (let ((url (ultralisp/protocols/external-url:external-url dist)))
    (weblocks/html:with-html
      (when (string-equal (ultralisp/models/dist:dist-name dist)
                          "lispworks")
        (:p "This distribution contains extensions and libraries for LispWorks.")
        (:p "These libraries are checked in LispWorks and may not work in other LISP implementations.")
        (:p "WARNING! This distribution is experimental and may be updated with delays because of LW licensing issues."))
      
      (:h3 "How to use it?")
      (:p "Open your Lisp REPL and eval:")
      (:pre (format nil "(ql-dist:install-dist \"~A\"
                      :prompt nil)"
                    url))
      (:p ("Or if you are using [Qlot](https://github.com/fukamachi/qlot), put this line into the beginning of your **qlfile**:"))

      (:pre (format nil "dist ultralisp ~A"
                    url)))))


(defun render-dist (dist-widget)
  (let* ((dist (dist dist-widget))
         (name (dist-name dist-widget))
         (limit 50)
         (sources (ultralisp/models/dist-source:dist->sources dist
                                                              :limit (1+ limit)))
         (has-more (= (length sources)
                      (1+ limit)))
         (sources (subseq sources
                          0
                          (min (length sources)
                               limit))))

    (weblocks/html:with-html
      (:h1 name)

      (render-installation-instructions dist)

      (:h3 "Projects")

      (cond
        (sources
         (:ul :class "dist-projects"
              (loop for source in sources
                    for project = (ultralisp/models/project:source->project source)
                    for project-name = (ultralisp/models/project:project-name project)
                    for project-description = (ultralisp/models/project:project-description project)
                    for url = (ultralisp/protocols/url:url project)
                    collect (list project-name project-description url) into data
                    ;; Before output we'll sort projects by name
                    finally (loop for (name desc url) in (sort data #'string<
                                                               :key #'car)
                                  do (:li ("[~A](~A)~@[ — ~A~]"
                                           url name
                                           (unless (string-equal desc "")
                                             desc))))))
         (when has-more
           (:p (:b "This distribution has more projects, but for performance reason we can't show them all."))
           (:p ("Future version of Ultralisp may include a search posibility. If you need it, vote for
                [this issue](https://github.com/ultralisp/ultralisp/issues/92)."))))
        ;; TODO: Maybe add a button to add some projects?
        (t (:p "No projects yet.")
           (when (is-moderator (weblocks-auth/models:get-current-user)
                               dist)
             (:p "You are the moderator of this distribution. Here is how you can add some projects:")
             (:ol
              (:li ("Open the list of [~A](~A)."
                    "/my/projects"
                    "your projects"))
              (:li "Select some project.")
              (:li "Push \"Edit\" button.")
              (:li ("Click on a checkbox against the distribution name \"~A\"." name))
              (:li "Push \"Save\" button."))))))))


(defmethod weblocks/widget:render ((widget dist-widget))
  (register-groups-bind (dist-name)
      ("^/dists/(.*)$" (weblocks/request:get-path))

    (setf (dist-name widget)
          dist-name
          (get-title)
          (format nil "Ultralisp: ~A distribution" dist-name))

    ;; This is not an idiomatic Weblocks code because we should
    ;; make a database query only when widget gets created, not
    ;; during the render.
    (let ((dist (dist widget)))
      (cond
        (dist (render-dist widget))
        (t (page-not-found))))))


(defmethod ultralisp/protocols/url:url ((dist ultralisp/models/dist:dist))
  (format nil "/dists/~A"
          (ultralisp/models/dist:dist-name dist)))


(defmethod ultralisp/protocols/external-url:external-url ((dist ultralisp/models/dist:dist))
  (let* ((base-url (get-base-url))
         (dist-name (ultralisp/models/dist:dist-name dist))
         (full-url (concatenate 'string
                                base-url
                                dist-name
                                ".txt")))
    (cond ((search "localhost" base-url)
           full-url)
          ((string-equal dist-name "ultralisp")
           base-url)
          (t
           full-url))))
