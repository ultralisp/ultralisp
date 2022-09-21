(defpackage #:ultralisp/widgets/login-menu
  (:use #:cl)
  (:import-from #:reblocks-lass)
  (:import-from #:reblocks/widget
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks-auth/models
                #:get-current-user
                #:get-nickname
                #:anonymous-p)
  (:import-from #:reblocks/response
                #:add-retpath-to)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)

  (:export
   #:make-login-menu))
(in-package #:ultralisp/widgets/login-menu)


(defwidget login-menu ()
  ())


(defmethod reblocks/widget:render ((widget login-menu))
  (let ((user (get-current-user))
        (my-projects-url "/my/projects")
        (my-dists-url "/my/dists")
        (feedback-url "https://github.com/ultralisp/ultralisp/issues"))
    (if (anonymous-p user)
        (reblocks/html:with-html
          (:div :class "login-link"
                (:a :href feedback-url
                    "Leave feedback")
                (:a :href (add-retpath-to "/login") "Log In")))
        
        (with-html
          (:ul :class "dropdown menu"
               :data-dropdown-menu t
               (:li (:a :href "#"
                        (get-nickname user))
                    (:ul :class "menu"
                         (:li (:a :href feedback-url
                                  "Leave feedback"))
                         (:li (:a :href my-projects-url
                                  "My projects"))
                         (:li (:a :href my-dists-url
                                  "My dists"))
                         (:li (:a :href (add-retpath-to "/logout")
                                  "Logout")))))))))


(defun make-login-menu ()
  (make-instance 'login-menu))


(defparameter *dependencies*
  (list (reblocks-lass:make-dependency
          '(.login-menu
            :color red
            :position absolute
            :top 0
            :right 0
            (.login-link
             (a :margin-right 0.5rem))))))


(defmethod get-dependencies ((widget login-menu))
  (append *dependencies*
          (call-next-method)))
