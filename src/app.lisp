(defpackage #:ultralisp/app
  (:use #:cl)
  (:shadowing-import-from #:40ants-routes/defroutes
                           #:get)
  (:import-from #:reblocks/app
                #:get-current
                #:defapp)
  (:import-from #:reblocks/routes
                #:static-file
                #:page)
  (:import-from #:reblocks/request-handler)
  (:import-from #:40ants-routes/defroutes
                #:post)
  (:import-from #:ultralisp/routes
                #:process-webhook-route)
  (:import-from #:reblocks-prometheus
                #:metrics)
  (:import-from #:ultralisp/variables)
  (:import-from #:ultralisp/stats
                #:make-collector)
  (:import-from #:ultralisp/db
                #:with-connection)
  (:import-from #:ultralisp/badges
                #:badge-svg)
  (:import-from #:ultralisp/widgets/frame
                #:wrap-with-page-frame)
  (:import-from #:ultralisp/widgets/landing
                #:make-landing-widget)
  (:import-from #:ultralisp/widgets/search
                #:make-search-page)
  (:import-from #:ultralisp/widgets/all-tags
                #:make-all-tags-widget)
  (:import-from #:ultralisp/widgets/projects-by-tag
                #:make-projects-by-tag-widget)
  (:import-from #:ultralisp/widgets/project
                #:make-project-widget)
  (:import-from #:ultralisp/widgets/projects
                #:make-author-projects-widget
                #:make-my-projects-widget)
  (:import-from #:ultralisp/widgets/dist
                #:make-dist-widget)
  (:import-from #:ultralisp/widgets/dists
                #:make-my-dists-widget)
  (:import-from #:ultralisp/widgets/version
                #:make-version-widget)
  (:import-from #:ultralisp/widgets/sponsors
                #:make-sponsors-widget)
  (:import-from #:ultralisp/github/widgets/repositories
                #:make-repositories-widget)
  (:import-from #:reblocks-auth/core
                #:make-login-processor
                #:make-logout-processor)
  (:import-from #:ultralisp/widgets/not-found
                #:page-not-found)
  (:import-from #:serapeum
                #:eval-always))
(in-package #:ultralisp/app)


(eval-always
  (defun get-project-title (&key author name)
    (format nil "~A/~A" author name))


  (defun get-author-title (&key author)
    (format nil "Projects of ~A" author))


  (defun get-tag-title (&key tag)
    (format nil "Projects with \"~A\" tag" tag))


  (defun get-dist-title (&key name)
    (format nil "Ultralisp: ~A distribution" name))


  (defun get-version-title (&key number)
    (format nil "Ultralisp: Version ~A" number)))


(defapp app
  :prefix "/"
  :description "The UltraLisp.org server."
  :autostart nil
  :debug t
  :page-constructor #'wrap-with-page-frame
  :routes ((static-file "/robots.txt"
                        (asdf:system-relative-pathname "ultralisp"
                                                       "static/robots.txt"))
           (static-file "/static/gear.gif"
                        (asdf:system-relative-pathname :ultralisp #P"src/widgets/gear.gif")
                        :content-type "image/gif")
           (get ("/dist/<.*:path>")
             (let* ((path (cond
                            ((str:emptyp path)
                             "ultralisp.txt")
                            (t path)))
                    (result (merge-pathnames
                             (uiop:parse-unix-namestring
                              path)
                             (merge-pathnames
                              (uiop:parse-unix-namestring
                               (ultralisp/variables:get-dist-dir))))))
               (log:info "Serving static from ~A" result)
               (list 200
                     nil
                     result)))
           (get ("/clpi/<.*:path>")
             (let* ((result (merge-pathnames
                             (uiop:parse-unix-namestring path)
                             (merge-pathnames
                              (make-pathname :directory '(:relative "clpi"))))))
               (log:info "Serving static from ~A" result)
               (list 200
                     nil
                     result)))
           (get ("/images/<.*:path>")
             (let* ((result (merge-pathnames (uiop:parse-unix-namestring path)
                                             (asdf:system-relative-pathname "ultralisp"
                                                                            (make-pathname :directory '(:relative "images"))))))
               (log:info "Serving static from ~A" result)
               (list 200
                     nil
                     result)))
           (get ("/projects/<.*:project>.svg")
             (with-connection ()
               (list 200
                     (list :content-type "image/svg+xml")
                     (list (badge-svg project)))))
           (metrics ("/metrics"
                     :user-metrics (list (make-collector))))
           (post ("/webhook/github")
             (process-webhook-route (get-current)))

           (page ("/login" :name "login")
             (make-login-processor))
           (page ("/logout" :name "logout")
             (make-logout-processor))
           (page ("/github" :name "github")
             (make-repositories-widget))
           (page ("/search/" :name "search")
             (make-search-page))
           (page ("/tags/" :name "tags")
             (make-all-tags-widget))
           (page ("/tags/<string:tag>/" :name "tag"
                                        :title #'get-tag-title)
             (make-projects-by-tag-widget tag))
           (page ("/sponsors" :name "sponsors")
             (make-sponsors-widget))
           (page ("/versions/<int:number>" :name "version"
                                           :title #'get-version-title)
             (make-version-widget number))
           (page ("/projects/<string:author>/<string:name>" :name "project"
                                                            :title #'get-project-title)
             (make-project-widget author name))
           (page ("/projects/<string:author>" :name "author"
                                               :title #'get-author-title)
             (make-author-projects-widget author))
            (page ("/dists/<.*:name>" :name "dist"
                                       :title #'get-dist-title)
              (make-dist-widget name))
           (page ("/my/projects" :name "my-projects")
             (make-my-projects-widget))
           (page ("/my/dists" :name "my-dists")
             (make-my-dists-widget))
           (page ("/" :name "index")
             (make-landing-widget))))
