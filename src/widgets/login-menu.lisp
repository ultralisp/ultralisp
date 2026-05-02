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
  (:import-from #:40ants-routes/route-url
                #:route-url)
  (:import-from #:ultralisp/variables
                #:*link-color-classes*)
  (:import-from #:reblocks-ui2/themes/styling
                #:join-css-classes)
  (:export
   #:make-login-menu))
(in-package #:ultralisp/widgets/login-menu)


(defwidget login-menu (ui-widget)
  ())


(defun make-login-menu ()
  (make-instance 'login-menu))


(defmethod render ((widget login-menu) (theme tailwind-theme))
  (let ((user (get-current-user))
        (my-projects-url (route-url "my-projects"))
        (my-dists-url (route-url "my-dists"))
        (feedback-url "https://github.com/ultralisp/ultralisp/issues"))
    (cond
      ((anonymous-p user)
       (with-html ()
         (:div :class "absolute top-0 right-0 text-sm mt-4 mr-4"
               (:a :href feedback-url
                   :class (join-css-classes theme "mr-2"
                                            *link-color-classes*)
                   "Leave feedback")
               (:a :href (add-retpath-to "/login")
                   :class *link-color-classes*
                   "Log In"))))
      (t
       (with-html ()
         (:div :class "absolute top-0 right-0 text-sm group mt-4 mr-4"
                (:a :href "#"
                    :class "text-gray-700 font-semibold inline-flex items-center gap-1"
                    (:span :class "inline-block transition-transform duration-200 group-hover:rotate-90"
                           :style "width:0;height:0;border-top:5px solid transparent;border-bottom:5px solid transparent;border-left:8px solid currentColor")
                    (get-nickname user))
               (:div :class "hidden group-hover:block absolute right-0 top-full bg-white border rounded shadow-md py-2 px-3 z-50 whitespace-nowrap"
                     (:a :href feedback-url :class (join-css-classes theme "block py-1" *link-color-classes*)
                         "Leave feedback")
                     (:a :href my-projects-url :class (join-css-classes theme "block py-1" *link-color-classes*)
                         "My projects")
                     (:a :href my-dists-url :class (join-css-classes theme "block py-1" *link-color-classes*)
                         "My dists")
                     (:a :href (add-retpath-to "/logout") :class (join-css-classes "block py-1" *link-color-classes*)
                         "Logout"))))))))
