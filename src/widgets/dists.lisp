(defpackage #:ultralisp/widgets/dists
  (:use #:cl)
  (:import-from #:str)
  (:import-from #:weblocks/page)
  (:import-from #:weblocks/widget
                #:defwidget)
  (:import-from #:weblocks-auth/models
                #:get-nickname
                #:get-current-user)
  (:import-from #:weblocks/html
                #:with-html)
  (:import-from #:ultralisp/models/dist
                #:dist-name)
  (:import-from #:ultralisp/protocols/url)
  (:import-from #:ultralisp/models/dist-moderator
                #:add-dist
                #:moderated-dists)
  (:import-from #:rutils
                #:fmt)
  (:export
   #:make-my-dists-widget))
(in-package ultralisp/widgets/dists)


(defwidget dists-widget ()
  ())


(defun make-my-dists-widget ()
  (make-instance 'dists-widget))


(defun render-dist (dist)
  (check-type dist ultralisp/models/dist:dist)
  (let ((name (dist-name dist))
        (url (ultralisp/protocols/url:url dist)))
    (with-html
      (:li (:a :href url
               name)))))


(defmethod weblocks/widget:render ((widget dists-widget))
  (let* ((title "Moderated dists")
         (user (get-current-user))
         (dists (when user
                  (moderated-dists user))))
    (setf (weblocks/page:get-title)
          title)
    
    (flet ((render-form ()
             (weblocks-ui/form:with-html-form
                 (:post (lambda (&key name &allow-other-keys)
                          (when (str:containsp "/" name)
                            (error "Dist name should not contain / char."))
                
                          (let* ((nickname (get-nickname user))
                                 (full-dist-name (fmt "~A/~A"
                                                      nickname
                                                      name)))
                            ;; For some reason, a dozen nicknames for users
                            ;; created from 2019-01-26 to 2019-03-30 have emails
                            ;; instead of nicknames.
                            ;; I've fixed them with datamigration, but
                            ;; to protect ourselves from the problems in future,
                            ;; we'll check that nickname is not an email:
                            (assert (not (str:containsp "@" nickname)))
                            
                            (log:info "Adding dist with name" full-dist-name)
                            (add-dist user full-dist-name)
                            (weblocks/widget:update widget))))
               (:table
                (:tbody :style "border: 0px solid white"
                        (:tr
                         (:td :style "padding: 0"
                              (:input :type "text"
                                      :name "name"
                                      :placeholder "Dist name"))
                         (:td :style "padding: 0; padding-left: 1em"
                              (:input :type "submit"
                                      :class "button"
                                      :value "Add"))))))))
      (with-html
        (:h1 title)
        (cond
          ((null user)
           (:p "Please log in to add custom distributions."))
          (dists
           (:ul :class "dists-list"
                (mapc #'render-dist dists))
           (render-form))
          (t (:p "You have no custom Quicklisp distributions yet.")
             (:p "Create one and you'll be able to group Common Lisp libraries as you want.")
             (render-form)))))))
