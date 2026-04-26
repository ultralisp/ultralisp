(defpackage #:ultralisp/widgets/login-menu
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks-ui2/widget
                #:render
                #:ui-widget)
  (:import-from #:reblocks-ui2/themes/tailwind
                #:tailwind-theme)
  (:import-from #:reblocks-auth/models
                #:get-current-user
                #:get-nickname
                #:anonymous-p)
  (:import-from #:reblocks/response
                 #:add-retpath-to)
  (:import-from #:ultralisp/variables
                #:*link-color-classes*)
  (:export
   #:make-login-menu))
(in-package #:ultralisp/widgets/login-menu)


(defwidget login-menu (ui-widget)
  ())


(defun make-login-menu ()
  (make-instance 'login-menu))


(defmethod render ((widget login-menu) (theme tailwind-theme))
  (let ((user (get-current-user))
        (my-projects-url "/my/projects")
        (my-dists-url "/my/dists")
        (feedback-url "https://github.com/ultralisp/ultralisp/issues"))
    (if (anonymous-p user)
        (with-html ()
          (:div :class "absolute top-0 right-0 text-sm"
                (:a :href feedback-url
                    :class "mr-2 text-gray-600 hover:text-gray-800"
                    "Leave feedback")
                (:a :href (add-retpath-to "/login")
                     :class *link-color-classes*
                     "Log In")))

        (with-html ()
          (:div :class "absolute top-0 right-0 text-sm"
                (:a :href "#"
                    :class "text-gray-700 font-semibold"
                    (get-nickname user))
                (:div :class "dropdown-menu hidden group-hover:block"
                      (:a :href feedback-url :class "block py-1 text-gray-600 hover:text-gray-800"
                          "Leave feedback")
                      (:a :href my-projects-url :class "block py-1 text-gray-600 hover:text-gray-800"
                          "My projects")
                      (:a :href my-dists-url :class "block py-1 text-gray-600 hover:text-gray-800"
                          "My dists")
                      (:a :href (add-retpath-to "/logout") :class "block py-1 text-gray-600 hover:text-gray-800"
                          "Logout")))))))
