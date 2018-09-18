(defpackage #:ultralisp/widgets/main
  (:use #:cl)
  (:import-from #:weblocks-navigation-widget
                #:defwidget)
  (:import-from #:weblocks/response
                #:abort-processing)
  (:import-from #:weblocks/html
                #:with-html-string)
  
  (:import-from #:ultralisp/widgets/login
                #:make-login-processor
                #:make-logout-processor)
  
  ;; Just depdendencies
  (:import-from #:log)
  (:import-from #:weblocks/app)
  (:import-from #:weblocks/page)
  (:import-from #:ultralisp/widgets/landing
                #:make-landing-widget)
  (:export #:make-main-widget))
(in-package ultralisp/widgets/main)


(defwidget main-widget
    ("/"
     (make-landing-widget))
  ("/login"
   (make-login-processor))
  ("/logout"
   (make-logout-processor))
  (t
   (page-not-found)))



(defun page-not-found ()
  (setf (weblocks/page:get-title)
        "Outer space")

  (abort-processing
   ;; TODO: replace with weblocks/response:return-page
   (with-html-string
     (weblocks/page:render (weblocks/app:get-current)
                           (with-html-string
                             (:h1 "Сказка не найдена")
                             (:p "Перед вами камень, на котором написано:")
                             (:ul (:li ("[На главную страницу](/) пойдёшь - сказку найдёшь."))
                                  (:li ("[Налево](/about) пойдёшь - про этот проект прочтёшь."))
                                  (:li ("[Направо](http://lisp-lang.org) пойдёшь - про Lisp прочтёшь."))))))
   :content-type "text/html"
   :code 404))


;; (defun get-login-menu ()
;;   (make-login-menu))


;; (defmethod weblocks/widget:render ((widget main-widget))
;;   (weblocks/widget:render
;;    (get-login-menu))

;;   (call-next-method))
