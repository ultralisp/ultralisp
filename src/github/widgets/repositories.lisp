(defpackage #:ultralisp/github/widgets/repositories
  (:use #:cl)
  (:import-from #:parenscript
                #:@
                #:chain)
  (:import-from #:link-header)
  (:import-from #:lparallel)
  (:import-from #:weblocks/widget
                #:get-html-tag
                #:render
                #:defwidget)
  (:import-from #:ultralisp/github/core
                #:make-authentication-url
                #:get-oauth-token-by)
  (:import-from #:weblocks/html
                #:with-html)
  (:import-from #:f-underscore
                #:f_)
  (:import-from #:weblocks/page
                #:get-title)
  (:import-from #:ultralisp/models/project
                #:is-enabled-p
                #:turn-off-github-project
                #:add-or-turn-on-github-project
                #:get-params
                #:get-github-projects)
  (:import-from #:ultralisp/webhook)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:cl-arrows
                #:->)
  (:import-from #:mito-email-auth/weblocks
                #:get-current-user)
  (:import-from #:mito-email-auth/models
                #:anonymous-p)
  (:import-from #:quri
                #:url-encode)
  (:import-from #:ultralisp/db
                #:with-connection)
  (:import-from #:log4cl-json
                #:with-log-unhandled)
  (:export
   #:make-repositories-widget
   #:repositories))
(in-package ultralisp/github/widgets/repositories)
(in-readtable :interpol-syntax)


(defparameter *token* nil
  "FOR DEBUG")


(defun get-url (url &key (token *token*))
  (unless token
    (error "I need a token to access github API. Seems somewhere is a logical error."))
  
  (let* ((url (if (cl-strings:starts-with url "https://")
                  url
                  (format nil "https://api.github.com~A" url)))
         (headers `(("Authorization" . ,#?"token ${token}")))
         (response (multiple-value-list (dex:get url
                                                 :headers headers
                                                 :timeout 3)))
         (data (jonathan:parse (first response)))
         (headers (third response))
         (links (link-header:parse (gethash "link" headers)))
         (next-page-url (getf links :next)))
    (append data
            (when next-page-url
              (get-url next-page-url :token token)))))


(defun get-all-repositories (&key (token *token*))
  (let* ((data (get-url "/user/repos" :token token))
         (names (mapcar (lambda (item)
                          (getf item :|full_name|))
                        data)))
    (sort names
          #'string<)))


(defun check-if-lisp-repository (repository token)
  (check-type repository list)
  (let* ((languages-url (getf repository :|languages_url|))
         (data (handler-case (get-url languages-url :token token)
                 ;; Some repositories can be "disabled" and
                 ;; server will return 404 for their languages page
                 (dexador.error:http-request-not-found ()
                   nil))))
    (getf data :|Common Lisp|)))


(function-cache:defcached get-lisp-repositories (&key (token *token*))
  (let* ((data (get-url "/user/repos" :token token))
         (data (lparallel:premove-if-not
                (lambda (repository)
                  (check-if-lisp-repository repository token))
                data))
         (names (mapcar (lambda (item)
                          (getf item :|full_name|))
                        data)))
    (sort names
          #'string<)))


(defun get-ultralisp-repositories (names)
  "Receives a list of repository names which are strings like \"40ants/defmain\"
   and returns a list of projects from the database where each project
   is belongs to the same users as repositories listed in the names argument."
  
  (let (usernames)
    (loop for name in names
          for username = (first (cl-strings:split name "/"))
          do (pushnew username usernames :test #'string-equal)
          finally (return (get-github-projects usernames)))))


(defwidget repositories ()
  ((oauth-token :initform nil
                :reader get-oauth-token)
   (state :initform :waiting-for-authentication
          :reader get-state)
   (thread :initform nil)
   (repositories :initform nil
                 :reader get-repositories)
   (repository-widgets :initform nil
                       :reader get-repository-widgets)))

(defwidget repository ()
  ((name :initarg :name
         :reader get-name)
   (in-ultralips-p :initarg :in-ultralisp-p
                   :accessor in-ultralisp-p)
   (token :initarg :token
          :reader get-token)
   ;; We store this url in a widget to make easier to test
   ;; widget in a repl, because full url can be constructed
   ;; only during request processing.
   (webhook-url :initform nil
                :initarg :webhook-url
                :reader get-webhook-url)))

(defmethod print-object ((obj repository) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream
            "~A ~:[❌~;✓~] "
            (get-name obj)
            (in-ultralisp-p obj))))


(defun make-repositories-widget ()
  (make-instance 'repositories))


(defun create-repository-widgets (widget webhook-url)
  "Fetches all Common Lisp repositories from the github
   and creates a `repository' widget for each of them."
  (check-type widget repositories)
  (log:debug "Starting to fetch repositories.")

  (let* ((token (get-oauth-token widget))
         (repositories (get-lisp-repositories :token token))
         (ultralisp-projects (get-ultralisp-repositories repositories))
         ;; A list of repository names which are in Ultralisp already.
         ;; We need it to draw a switcher in a correct state.
         (ultralisp-names (loop for project in ultralisp-projects
                                for params = (get-params project)
                                when (is-enabled-p project)
                                  collect (format nil "~A/~A"
                                                  (getf params :user-or-org)
                                                  (getf params :project)))))
    (setf (slot-value widget 'repository-widgets)
          (mapcar (lambda (name)
                    (make-instance 'repository
                                   :name name
                                   :in-ultralisp-p (member name
                                                           ultralisp-names
                                                           :test #'string-equal)
                                   :token token
                                   :webhook-url webhook-url))
                  repositories)
          (slot-value widget 'repositories) repositories
          (slot-value widget 'state) :data-fetched)
    (log:debug "Repositories were fetched.")
    ultralisp-names)

  )


(defun set-oauth-token (widget token)
  (log:debug "Setting oauth token")
  (setf (slot-value widget 'oauth-token)
        token
        (slot-value widget 'state)
        :fetching-data)

  (log:debug "Starting thread to retrieve repositories")
  (let ((webhook-url (ultralisp/webhook:get-webhook-url)))
    (setf (slot-value widget 'thread)
          (bt:make-thread
           (lambda ()
             (with-log-unhandled ()
               (with-connection ()
                 (create-repository-widgets widget webhook-url))))
           :name "searching-for-repositories"))))


(defgeneric render-with-state (widget state)
  (:method ((state (eql :waiting-for-authentication)) (widget repositories))
    (let* ((code (weblocks/request:get-parameter "code"))
           (token (when code
                    (get-oauth-token-by code))))
      (cond
        (token
         (set-oauth-token widget token)
         (render widget))
        ((or (null ultralisp/github/core:*client-id*)
             (null ultralisp/github/core:*secret*))
         (weblocks/html:with-html
           (:p "Please set ultralisp/github/core:*client-id* and ultralisp/github/core:*secret*:")
           (:pre "\(setf ultralisp/github/core:*client-id* \"0bc*************\"
      ultralisp/github/core:*secret* \"3f46**************************\"\)
")))
        ((anonymous-p (get-current-user))
         (let ((url (url-encode (weblocks/request:get-uri))))
           (weblocks/html:with-html
             (:p "Please login to add GitHub repositories to Ultralisp.")
             (:p (:a :href #?"/login?retpath=${url}"
                     :class "button"
                     "Log in")))))
        (t (weblocks/html:with-html
             (:p "Please authenticate in GitHub to plug your repositories into Ultralisp.")
             (:p (:a :href (make-authentication-url)
                     :class "button"
                     "Authenticate with Github")))))))
  
  (:method ((state (eql :fetching-data)) (widget repositories))
    (weblocks/html:with-html
      (:p "Searching for Common Lisp repositories...")
      (weblocks-ui/form:render-form-and-button
       "Refresh" 
       (lambda (&rest args)
         (declare (ignorable args))
         (log:debug "User clicked")
         (weblocks/widget:update widget ))
       :button-class "button refresh")))
  
  (:method ((state (eql :data-fetched)) (widget repositories))
    (with-html
      (:table 
       (loop for repository-widget in (get-repository-widgets widget)
             do (render repository-widget)))))
  
  
  (:method ((state t) (widget repositories))
    (weblocks/html:with-html
      (:p "This state is not supported yet."))))


(defmethod render ((widget repositories))
  (setf (get-title)
        "Github Repositories")

  (let ((state (get-state widget)))
    (log:debug "Rendering in" state)
  
    (render-with-state state
                       widget)))


(defun render-switch (state action)
  (let* ((action-code (weblocks/actions::function-or-action->action action))
         (on-click (format nil "initiateAction(\"~A\"); return false;"
                           action-code)))
    (with-html
      (:div :class "switch tiny"
            (:input :class "switch-input"
                    :type "checkbox"
                    :checked state)
            (:label :class "switch-paddle"
                    :onclick on-click)))))


(defun get-hook-id (repository)
  "Checks if Ultralisp is turned on for the repository and if it does, then returns hook id"
  (check-type repository repository)
  (let* ((name (get-name repository))
         (hook (get-webhook-url repository)))
    (loop for item in (get-url #?"/repos/${name}/hooks"
                               :token (get-token repository))
          for item-url = (-> item
                             (getf :|config|)
                             (getf :|url|))
          when (string-equal item-url
                             hook)
            do (return (getf item :|id|)))))


(defun turn-on (repository)
  (check-type repository repository)
  (log:info "Turning on" repository)
  
  (let* ((name (get-name repository))
         (hook (get-webhook-url repository))
         (token (get-token repository))
         (url #?"https://api.github.com/repos/${name}/hooks"))
    
    (cond ((get-hook-id repository)
           (log:warn "Hmm, hook is already configured for this repository"))
          (t
           (dex:post url
                     :headers `(("Authorization" . ,#?"token ${token}")
                                ("Content-Type" . "application/json"))
                     :content (jonathan:to-json
                               `(:|config| (:|url| ,hook
                                             :|content_type| "json"))))))
    
    (add-or-turn-on-github-project name)
    (values)))


(defun turn-off (repository)
  (check-type repository repository)
  (log:info "Turning off" repository)

       
  (let* ((name (get-name repository))
         (hook-id (get-hook-id repository))
         (token (get-token repository))
         (url #?"https://api.github.com/repos/${name}/hooks/${hook-id}"))

    (cond ((get-hook-id repository)
           (dex:delete url
                       :headers `(("Authorization" . ,#?"token ${token}")
                                  ("Content-Type" . "application/json"))))
          (t
           (log:warn "Hmm, hook is already removed for" repository)))

    (turn-off-github-project name)
    (values)))


(defun toggle (repository)
  (check-type repository repository)
  (cond
    ((in-ultralisp-p repository)
     (log:info "Turning Ultralisp off for" repository)
     (turn-off repository)
     
     (setf (in-ultralisp-p repository)
           nil))
    (t
     (log:info "Turning Ultralisp on for" repository)
     (turn-on repository)
     (setf (in-ultralisp-p repository)
           t)))
  (when (weblocks/request:ajax-request-p)
   (weblocks/widget:update repository)))


(defmethod get-html-tag ((widget repository))
  :tr)

(defmethod render ((widget repository))
  (with-html
    (:td (get-name widget))
    (:td (render-switch (in-ultralisp-p widget)
                        (lambda (&rest args)
                          (declare (ignorable args))
                          (toggle widget))))))


(defmethod weblocks/dependencies:get-dependencies ((widget repositories))
  (append
   (list
    (weblocks-parenscript:make-dependency
      (let ((timer (@ window repositories-timer)))
        (unless timer
          (chain console (log "Installing timer to check when repositories will be discovered"))
          (setf (@ window repositories-timer)
                (set-interval
                 (lambda ()
                   (let ((refresh-button (chain (j-query ".repositories .refresh.button")))
                         (data-fetched (chain (j-query ".repositories.data-fetched") length )))
                     (when refresh-button
                       (chain console (log "Still fetching data"))
                       (chain refresh-button (click)))
                     (when data-fetched
                       (chain console (log "Data is ready"))
                       (clear-interval (@ window repositories-timer)))))
                 5000))))))
   (call-next-method)))


(defmethod weblocks/dependencies:get-dependencies ((widget repository))
  (append
   (list
    (weblocks-lass:make-dependency
      `(.repository
        (.switch :margin 0))))
   (call-next-method)))


(defmethod weblocks/widget:get-css-classes ((widget repositories))
  (cons (get-state widget)
        (call-next-method)))

;; /user/orgs
;; /user/repos
