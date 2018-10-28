(defpackage #:ultralisp/widgets/main
  (:use #:cl)
  (:import-from #:weblocks-navigation-widget
                #:defwidget)
  (:import-from #:weblocks/response
                #:abort-processing)
  (:import-from #:weblocks/html
                #:with-html-string)
  
  ;; Just depdendencies
  (:import-from #:log)
  (:import-from #:weblocks/app)
  (:import-from #:weblocks/widget)
  (:import-from #:weblocks/page)
  (:import-from #:ultralisp/widgets/landing
                #:make-landing-widget)
  (:import-from #:mito-email-auth/weblocks
                #:make-logout-processor
                #:make-login-processor)
  (:import-from #:ultralisp/widgets/login-menu
                #:make-login-menu)
  (:export #:make-main-widget))
(in-package ultralisp/widgets/main)



(defun page-not-found ()
  (setf (weblocks/page:get-title)
        "Outer space")

  (abort-processing
   ;; TODO: replace with weblocks/response:return-page
   (with-html-string
     (weblocks/page:render (weblocks/app:get-current)
                           (with-html-string
                             (:h1 "404")
                             (:h2 "Page not found"))))
   :content-type "text/html"
   :code 404))


(defwidget main-widget
    ("/"
     (make-landing-widget))
  ("/login"
   (make-login-processor))
  ("/logout"
   (make-logout-processor))
  (t
   (page-not-found)))


(defmethod weblocks/widget:render ((widget main-widget))
  (weblocks/widget:render
   (make-login-menu))

  (call-next-method))
