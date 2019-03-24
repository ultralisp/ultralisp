(defpackage #:ultralisp/widgets/main
  (:use #:cl)
  (:import-from #:weblocks-navigation-widget
                #:defwidget)
  (:import-from #:weblocks/html
                #:with-html-string)
  
  ;; Just depdendencies
  (:import-from #:log)
  (:import-from #:weblocks/app)
  (:import-from #:weblocks/widget)
  (:import-from #:weblocks/page)
  (:import-from #:ultralisp/widgets/version
                #:make-version-widget)
  (:import-from #:ultralisp/widgets/landing
                #:make-landing-widget)
  (:import-from #:mito-email-auth/weblocks
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
  (:import-from #:ultralisp/widgets/projects
                #:make-author-projects-widget)
  (:import-from #:ultralisp/widgets/sponsors
                #:make-sponsors-widget)
  (:export #:make-main-widget))
(in-package ultralisp/widgets/main)


(defwidget main-widget
    ("/"
     (make-landing-widget))
  ("/login"
   (make-login-processor))
  ("/logout"
   (make-logout-processor))
  ("/github"
   (make-repositories-widget))
  ("/sponsors"
   (make-sponsors-widget))
  ("/versions/\\d+"
   (make-version-widget))
  ("/projects/.*/.*"
   (make-project-widget))
  ("/projects/.*"
   (make-author-projects-widget))
  (t
   (page-not-found)))


(defmethod weblocks/widget:render ((widget main-widget))
  (weblocks/widget:render
   (make-login-menu))

  (call-next-method))
