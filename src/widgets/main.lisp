(defpackage #:ultralisp/widgets/main
  (:use #:cl)
  (:import-from #:reblocks-navigation-widget
                #:defroutes)
  (:import-from #:reblocks/html
                #:with-html-string)

  ;; Just depdendencies
  (:import-from #:log)
  (:import-from #:reblocks/app)
  (:import-from #:reblocks/widget)
  (:import-from #:reblocks/page)
  (:import-from #:ultralisp/widgets/version
                #:make-version-widget)
  (:import-from #:ultralisp/widgets/landing
                #:make-landing-widget)
  (:import-from #:reblocks-auth/core
                #:make-logout-processor
                #:make-login-processor)
  (:import-from #:ultralisp/widgets/login-menu
                #:make-login-menu)
  (:import-from #:ultralisp/github/widgets/repositories
                #:make-repositories-widget)
  (:import-from #:ultralisp/widgets/not-found
                #:page-not-found)
  (:import-from #:ultralisp/widgets/project
                #:make-project-widget)
  (:import-from #:ultralisp/widgets/dist
                #:make-dist-widget)
  (:import-from #:ultralisp/widgets/projects
                #:make-author-projects-widget
                #:make-my-projects-widget)
  (:import-from #:ultralisp/widgets/sponsors
                #:make-sponsors-widget)
  (:import-from #:ultralisp/widgets/search
                #:make-search-page)
  (:import-from #:ultralisp/widgets/dists
                #:make-my-dists-widget)
  (:export #:make-main-routes))
(in-package ultralisp/widgets/main)


(defroutes main-routes
    ("/"
     (make-landing-widget))
  ("/login"
   (make-login-processor))
  ("/logout"
   (make-logout-processor))
  ("/github"
   (make-repositories-widget))
  ("/search/"
   (make-search-page))
  ("/sponsors"
   (make-sponsors-widget))
  ("/versions/\\d+"
   (make-version-widget))
  ("/projects/.*/.*"
   (make-project-widget))
  ("/dists/.*"
   (make-dist-widget))
  ("/projects/.*"
   (make-author-projects-widget))
  ("/my/projects"
   (make-my-projects-widget))
  ("/my/dists"
   (make-my-dists-widget))
  (t
   (page-not-found)))


(defmethod reblocks/widget:render ((widget main-routes))
  (reblocks/widget:render
   (make-login-menu))

  (call-next-method))
