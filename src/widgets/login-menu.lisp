(defpackage #:ultralisp/widgets/login-menu
  (:use #:cl)
  (:import-from #:weblocks-lass)
  (:import-from #:weblocks/widget
                #:defwidget)
  ;; (:import-from #:app/models/user
  ;;               #:anonymous-p
  ;;               #:get-email
  ;;               #:is-user-admin
  ;;               #:get-current-user)
  (:import-from #:weblocks/html
                #:with-html)
  (:import-from #:mito-email-auth/models
                #:get-email
                #:anonymous-p)
  (:import-from #:mito-email-auth/weblocks
                #:get-current-user)

  (:export
   #:make-login-menu))
(in-package ultralisp/widgets/login-menu)


(defwidget login-menu ()
  ())


(defmethod weblocks/widget:render ((widget login-menu))
  (let ((user (get-current-user)))
    (if (anonymous-p user)
        (weblocks/html:with-html
          (:div :class "login-link"
                (:a :href "https://github.com/ultralisp/ultralisp/issues"
                                  "Leave feedback")
                (:a  :href "/login" "Log In")))
        
        (with-html
          (:ul :class "dropdown menu"
               :data-dropdown-menu t
               (:li (:a :href "#"
                        (get-email user))
                    (:ul :class "menu"
                         (:li (:a :href "https://github.com/ultralisp/ultralisp/issues"
                                  "Leave feedback"))
                         (:li (:a :href "/logout"
                                  "Logout")))))))))


(defun make-login-menu ()
  (make-instance 'login-menu))


(defparameter *dependencies*
  (list (weblocks-lass:make-dependency
          '(.login-menu
            :color red
            :position absolute
            :top 1rem
            :right 0
            (.login-link
             (a :margin-right 0.5rem))))))


(defmethod weblocks/dependencies:get-dependencies ((widget login-menu))
  (append *dependencies*
          (call-next-method)))
