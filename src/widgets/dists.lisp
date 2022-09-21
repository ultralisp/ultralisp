(defpackage #:ultralisp/widgets/dists
  (:use #:cl)
  (:import-from #:str)
  (:import-from #:log)
  (:import-from #:reblocks/page)
  (:import-from #:reblocks/widget
                #:defwidget)
  (:import-from #:reblocks-auth/models
                #:get-nickname
                #:get-current-user)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:ultralisp/models/dist
                #:find-dist
                #:dist-name)
  (:import-from #:ultralisp/protocols/url)
  (:import-from #:ultralisp/models/dist-moderator
                #:add-dist
                #:moderated-dists)
  (:import-from #:rutils
                #:fmt)
  (:import-from #:reblocks-ui/form
                #:with-html-form)
  (:import-from #:spinneret
                #:with-html-string)
  (:export
   #:make-my-dists-widget))
(in-package #:ultralisp/widgets/dists)


(defwidget dists-widget ()
  ((value :initform nil)
   (error :initform nil)))


(defun make-my-dists-widget ()
  (make-instance 'dists-widget))


(defun render-dist (dist)
  (check-type dist ultralisp/models/dist:dist)
  (let ((name (dist-name dist))
        (url (ultralisp/protocols/url:url dist)))
    (with-html
      (:li (:a :href url
               name)))))


(defmethod reblocks/widget:render ((widget dists-widget))
  (let* ((title "Moderated dists")
         (user (get-current-user))
         (nickname (get-nickname user))
         (dists (when user
                  (moderated-dists user))))
    (setf (reblocks/page:get-title)
          title)
    
    (with-slots (value error) widget
      (flet ((render-form ()
               (with-html-form
                   (:post (lambda (&key name &allow-other-keys)
                            ;; We keep the name in a slot to render it again
                            ;; if there is some error.
                            (setf value
                                  name)
                            (let* ((full-dist-name (fmt "~A/~A"
                                                        nickname
                                                        name))
                                   (existing-dist (find-dist full-dist-name :raise-error nil)))
                              ;; For some reason, a dozen nicknames for users
                              ;; created from 2019-01-26 to 2019-03-30 have emails
                              ;; instead of nicknames.
                              ;; I've fixed them with datamigration, but
                              ;; to protect ourselves from the problems in future,
                              ;; we'll check that nickname is not an email:
                              (assert (not (str:containsp "@" nickname)))
                             
                              (log:info "Adding dist with name" full-dist-name)
                              (cond
                                ((str:containsp "/" name)
                                 (setf error
                                       (with-html-string
                                         (:span "Dist name should not contain / char."))))
                                ((string= name "")
                                 (setf error
                                       (with-html-string
                                         (:span "Dist name consist of your nickname and a second part, which can't be empty."))))
                                (existing-dist
                                 (setf error
                                       (with-html-string
                                         (:span ("Unable to create dist with name \"~A\" because such dist [~A](already exists)."
                                                 full-dist-name
                                                 (ultralisp/protocols/url:url existing-dist))))))
                                (t
                                 (setf value nil
                                       error nil)
                                 (add-dist user full-dist-name)))
                              
                              (reblocks/widget:update widget))))
                 (:table
                  (:tbody :style "border: 0px solid white; vertical-align: top;"
                          (:tr
                           (:td :style "padding: 0.45em 0.4em 0 0; white-space: nowrap;"
                                (:p ("~A / " nickname)))
                           (:td :style "padding: 0; width: 100%"
                                (:input :type "text"
                                        :name "name"
                                        :value value
                                        :placeholder "Dist name")
                                (when error
                                  (:p :style "color: #cc4b37"
                                      (:raw error))))
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
               (render-form))))))))
