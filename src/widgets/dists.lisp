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
  (let ((name (dist-name dist)))
    (with-html
      (:li name))))


(defmethod weblocks/widget:render ((widget dists-widget))
  (let* ((title "Moderated dists")
         (user (get-current-user))
         (dists (moderated-dists user)))
    (setf (weblocks/page:get-title)
          title)
    
    (flet ((render-form ()
             (weblocks-ui/form:with-html-form
                 (:post (lambda (&key name &allow-other-keys)
                          (when (str:containsp "/" name)
                            (error "Project name should not contain / char."))
                
                          (let* ((nickname (get-nickname user))
                                 (full-dist-name (fmt "~A/~A"
                                                      nickname
                                                      name)))
                            (log:info "Adding dist with name" full-dist-name)
                            (add-dist user full-dist-name)
                            (weblocks/widget:update widget))))
               (:table
                (:tbody :style "border: 0px solid white"
                        (:tr
                         (:td :style "padding: 0"
                              (:input :type "text"
                                      :name "name"
                                      :placeholder "Project name"))
                         (:td :style "padding: 0; padding-left: 1em"
                              (:input :type "submit"
                                      :class "button"
                                      :value "Add"))))))))
      (with-html
        (:h1 title)
        (cond
          (dists
           (:ul :class "dists-list"
                (mapc #'render-dist dists))
           (render-form))
          (t (:p "You have no custom Quicklisp distributions yet.")
             (:p "Create one and you'll be able to group Common Lisp libraries as you want.")
             (render-form)))))))


(let* (
       (projects (sort (ultralisp/models/moderator:get-projects user)
                       #'string<
                       :key #'ultralisp/models/project:get-name))
       (title "Moderated projects"))
    (cond
      (projects (with-html
                  (:h1 :class "author-name"
                       title)
                  (setf (weblocks/page:get-title)
                        title)
                  (render-projects-list projects)))
      (t (page-not-found))))
