(defpackage #:ultralisp/widgets/dists
  (:use #:cl)
  (:import-from #:str
                #:containsp)
  (:import-from #:log)
  (:import-from #:reblocks/page)
  (:import-from #:reblocks/widget
                #:update
                #:widget
                #:defwidget)
  (:import-from #:reblocks-auth/models
                #:change-nickname
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
  (:import-from #:event-emitter
                #:emit
                #:on
                #:event-emitter)
  (:import-from #:reblocks-auth/errors
                #:nickname-is-not-available)
  (:export
   #:make-my-dists-widget))
(in-package #:ultralisp/widgets/dists)


(defwidget change-username-widget (event-emitter widget)
  ()
  (:documentation "This widget is shown when current username contains @ character and can't be used as part of the distribution name.
                   After nickname will be changed it emits :NICKNAME-CHANGED event on itself."))


(defwidget dists-widget ()
  ((value :initform nil)
   (error :initform nil)
   (change-username-widget :initarg :change-username-widget
                           :reader change-username-widget)))


(defun make-change-username-widget ()
  (make-instance 'change-username-widget))


(defun make-my-dists-widget ()
  (let* ((subwidget (make-change-username-widget))
         (main (make-instance 'dists-widget
                              :change-username-widget subwidget)))
    (on :nickname-changed subwidget
        (lambda ()
          (update main)))
    (values main)))


(defun render-dist (dist)
  (check-type dist ultralisp/models/dist:dist)
  (let ((name (dist-name dist))
        (url (ultralisp/protocols/url:url dist)))
    (with-html
      (:li (:a :href url
               name)))))


(defparameter *bad-chars* "!@#$%^&*,.")


(defun bad-nickname-p (nickname)
  (loop for char across *bad-chars*
        thereis (position char nickname
                          :test #'char=)))


(defmethod reblocks/widget:render ((widget change-username-widget))
  (let* ((user (get-current-user))
         (nickname (when user
                     (get-nickname user))))
    (when (and nickname
               (bad-nickname-p nickname))
      (reblocks/html:with-html
        (:p ("You current nickname ~S contains special characters such like ~S. Please, change nickname to add custom distributions:"
             nickname
             *bad-chars*))

        (with-html-form
            (:post (lambda (&key new-nickname &allow-other-keys)
                     (when (string= new-nickname "")
                       (reblocks-ui/form:field-error "new-nickname"
                                                     "New nickname should not be empty."))
                     (when (bad-nickname-p new-nickname)
                       (reblocks-ui/form:field-error "new-nickname"
                                                     "New nickname still contains wrong characters."))

                     (when (zerop (reblocks-ui/form:get-field-errors-count))
                       ;; Changing username only if it is correct.
                       (handler-case (progn (change-nickname new-nickname)
                                            (emit :nickname-changed widget))
                         (nickname-is-not-available ()
                           (reblocks-ui/form:field-error "new-nickname"
                                                         "This nickname is used by another account."))))))

          (:div :style "display: flex; gap: 1rem;"
                (:div :style "display: flex; flex-direction: column; gap: 1rem; flex-grow: 1;"
                      (:input :type "text"
                              :name "new-nickname"
                              :value nickname
                              :style "margin: 0;"
                              :placeholder "New nickname")

                      (reblocks-ui/form:error-placeholder "new-nickname")
                      (reblocks-ui/form:form-error-placeholder))

                (:input :type "submit"
                        :class "button small"
                        :style "height: 2.5rem"
                        :value "Change")))))))


(defmethod reblocks/widget:render ((widget dists-widget))
  (let* ((title "Moderated dists")
         (user (get-current-user))
         (nickname (when user
                     (get-nickname user)))
         (dists (when user
                  (moderated-dists user))))
    (setf (reblocks/page:get-title)
          title)

    (cond
      ((null user)
       (with-html
         (:p "This page requires authentication.")))
      ((bad-nickname-p nickname)
       (reblocks/widget:render
        (change-username-widget widget)))
      ;; When nickname is ok, we might show the new dists form:
      (t
    
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
                  (render-form))))))))))
