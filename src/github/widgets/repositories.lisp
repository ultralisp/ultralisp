(defpackage #:ultralisp/github/widgets/repositories
  (:use #:cl)
  (:import-from #:parenscript
                #:@
                #:chain)
  (:import-from #:link-header)
  (:import-from #:rutils)
  (:import-from #:lparallel)
  (:import-from #:weblocks/widget
                #:update
                #:get-html-tag
                #:render
                #:defwidget)
  (:import-from #:weblocks/html
                #:with-html)
  (:import-from #:f-underscore
                #:f_)
  (:import-from #:weblocks/page
                #:get-title)
  (:import-from #:ultralisp/models/project
                #:get-reason
                #:unable-to-create-project
                #:make-github-project-from-url
                #:is-enabled-p
                #:add-or-turn-on-github-project
                #:get-params
                #:get-github-projects)
  (:import-from #:ultralisp/github/webhook)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:cl-arrows
                #:->)
  (:import-from #:weblocks-auth/github
                #:get-scopes)
  (:import-from #:quri
                #:url-encode)
  (:import-from #:ultralisp/db
                #:with-connection)
  (:import-from #:log4cl-extras/error
                #:with-log-unhandled)
  (:import-from #:ultralisp/widgets/spinner
                #:make-spinner)
  (:import-from #:weblocks-ui/form
                #:render-form-and-button)
  (:import-from #:ultralisp/widgets/utils
                #:render-switch)
  (:import-from #:weblocks/response
                #:add-retpath-to)
  (:import-from #:ultralisp/models/source
                #:get-github-sources)
  (:export
   #:make-repositories-widget
   #:repositories))
(in-package ultralisp/github/widgets/repositories)
(in-readtable :interpol-syntax)


(defvar *token* nil
  "FOR DEBUG")

(defparameter *required-scopes*
  (list
   ;; We need this scope to be able to show your public repositories from your organizations
   "read:org"
   ;; And this scope allows us to add a webhook which will process all new commits
   ;; and rebuild the Ultralisp distribution including all new great changes in your projects!
   "admin:repo_hook"))


(defun get-url (url &key (token *token*))
  (unless token
    (error "I need a token to access github API. Seems somewhere is a logical error."))
  
  (let* ((url (if (cl-strings:starts-with url "https://")
                  url
                  (format nil "https://api.github.com~A" url)))
         (headers `(("Authorization" . ,#?"token ${token}")))
         (response (multiple-value-list (dex:get url
                                                 :headers headers
                                                 :connect-timeout 3
                                                 :read-timeout 3)))
         (data (jonathan:parse (first response)))
         (headers (third response))
         (links (link-header:parse (gethash "link" headers)))
         (next-page-url (getf links :next)))
    (append data
            (when next-page-url
              (get-url next-page-url :token token)))))


(defun check-if-lisp-repository (repository token)
  (check-type repository list)
  (let* ((languages-url (getf repository :|languages_url|))
         (data (handler-case (get-url languages-url :token token)
                 ;; Some repositories can be "disabled" and
                 ;; server will return 404 for their languages page
                 (dexador.error:http-request-not-found ()
                   nil))))
    (getf data :|Common Lisp|)))


(function-cache:defcached (get-lisp-repositories :timeout 600) (&key (token *token*))
  (let* ((data (get-url "/user/repos" :token token))
         (data (lparallel:premove-if-not
                (lambda (repository)
                  (check-if-lisp-repository repository token))
                data))
         (reps (loop for item in data
                     for name = (getf item :|full_name|)
                     for fork = (getf item :|fork|)
                     collect (list :name name
                                   :fork fork))))
    (sort reps
          #'string<
          :key (lambda (item)
                 (getf item :name)))))


(defun get-sources-for-github-repositories (repositories)
  "Receives a list of repository names which are strings like \"40ants/defmain\"
   and returns a list of sources from the database where each source
   is belongs to the same users as repositories listed in the names argument."
  
  (let (usernames)
    (loop for repo in repositories
          for name = (getf repo :name)
          for username = (first (cl-strings:split name "/"))
          do (pushnew username usernames :test #'string-equal)
          finally (return (get-github-sources usernames)))))


(defwidget repositories ()
  ((oauth-token :initform nil
                :reader get-oauth-token)
   (state :initform :checking-scopes
          :reader get-state)
   (thread :initform nil)
   (repositories :initform nil
                 :reader get-repositories)
   (spinner :initform (make-spinner)
            :reader get-spinner)
   (repository-widgets :initform nil
                       :reader get-repository-widgets)
   (show-forks :initform nil
               :accessor show-forks-p)
   (url-form-error :initform nil
                   :accessor get-url-form-error)))


(defwidget repository ()
  ((name :initarg :name
         :reader get-name)
   (in-ultralips-p :initarg :in-ultralisp-p
                   :accessor in-ultralisp-p)
   (fork :initarg :fork
         :reader is-fork-p)
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
         (ultralisp-sources (get-sources-for-github-repositories repositories))
         ;; A list of repository names which are in Ultralisp already.
         ;; We need it to draw a switcher in a correct state.
         (ultralisp-names (loop for source in ultralisp-sources
                                for params = (ultralisp/models/source:source-params source)
                                for dists = (ultralisp/models/dist-source:source->dists source :enabled t)
                                when dists
                                collect (format nil "~A/~A"
                                                (getf params :user-or-org)
                                                (getf params :project)))))
    (setf (slot-value widget 'repository-widgets)
          (loop for repo in repositories
                for name = (getf repo :name)
                for fork = (getf repo :fork)
                collect (make-instance 'repository
                                       :name name
                                       :in-ultralisp-p (member name
                                                               ultralisp-names
                                                               :test #'string-equal)
                                       :token token
                                       :fork fork
                                       :webhook-url webhook-url))
          (slot-value widget 'repositories) repositories
          (slot-value widget 'state) :data-fetched)
    (log:debug "Repositories were fetched.")
    ultralisp-names))


(defun set-oauth-token (widget token)
  (log:debug "Setting oauth token")
  (setf (slot-value widget 'oauth-token)
        token
        (slot-value widget 'state)
        :fetching-data)

  (log:debug "Starting thread to retrieve repositories")
  (let ((webhook-url (ultralisp/github/webhook:get-webhook-url)))
    (setf (slot-value widget 'thread)
          (bt:make-thread
           (lambda ()
             (with-log-unhandled ()
               (with-connection ()
                 (create-repository-widgets widget webhook-url))))
           :name "searching-for-repositories"))))


(defun render-url-input (widget)
  (with-html
    (:p "or insert a project's URL:")

    (weblocks-ui/form:with-html-form
        (:post
         (lambda (&key url &allow-other-keys)
           (log:info "CATCHED ARGS" url)
           (handler-case
               (let* ((project (make-github-project-from-url url))
                      (project-url (ultralisp/models/project:project-url project)))
                 (weblocks/response:redirect project-url))
             (unable-to-create-project (condition)
               (let ((reason (get-reason condition)))
                 (log:error "Setting the reason"
                            (get-reason condition))
                 (setf (get-url-form-error widget)
                       reason)
                 (update widget))))))
      (:table :class "url-frame"
              (:tr :style "vertical-align: top"
               (:td
                (:input :type "text"
                        :name "url")
                (if (get-url-form-error widget)
                    (:p :class "label alert"
                        (get-url-form-error widget))
                    ;; Otherwise, just show a note about webhooks
                    (:p :class "label secondary"
                        "But this way a project we'll not be able to setup a webhook and project will be updated only by cron.")))
               (:td
                (weblocks-ui/form:render-button
                 "Add"
                 :class "button")))))))


(defgeneric render-with-state (widget state)
  (:method ((state (eql :checking-scopes)) (widget repositories))
    (let* ((token (weblocks-auth/github:get-token))
           (user (weblocks-auth/models:get-current-user))
           (scopes (get-scopes))
           (has-required-scopes
             (loop for scope in *required-scopes*
                   unless (member scope scopes
                                  :test 'string-equal)
                     do (return nil)
                   finally (return t))))
      (cond
        ((and token has-required-scopes)
         (set-oauth-token widget token)
         (render widget))
        
        (t (weblocks/html:with-html
             (cond (user
                    (:p "To show all your public repositories, we need additional permissions from GitHub."
                        (:span :style "position: relative; margin-left: 0.5em; top: 0.4em"
                               (weblocks-auth/github:render-button
                                :scopes (append *required-scopes*
                                                weblocks-auth/github:*default-scopes*))))
                    (render-url-input widget))
                   (t
                    (:p "To be able to add your public repositories, you need to login using GitHub."
                        (:span :style "position: relative; margin-left: 0.5em; top: 0.4em"
                               (weblocks-auth/github:render-button
                                :text "Login"
                                :scopes (append *required-scopes*
                                                weblocks-auth/github:*default-scopes*)))))))))))
  
  (:method ((state (eql :fetching-data)) (widget repositories))
    (weblocks/html:with-html
      (:p "Searching for Common Lisp repositories..."
          (render (get-spinner widget)))
      (render-form-and-button
       "Refresh" 
       (lambda (&rest args)
         (declare (ignorable args))
         (log:debug "User clicked")
         (update widget ))
       :button-class "button refresh")))
  
  (:method ((state (eql :data-fetched)) (widget repositories))
    (with-html
      (:p "Show forks?"
          (:span :style "position: relative; top: 0.4em"
                 (render-switch (show-forks-p widget)
                                (lambda (&rest args)
                                  (declare (ignorable args))
                                  (setf (show-forks-p widget)
                                        (not (show-forks-p widget)))
                                  (update widget))
                                :labels '("Yes" "No"))))
      
      (:table 
       (loop for repository-widget in (get-repository-widgets widget)
             for fork = (is-fork-p repository-widget)
             do (when (or (show-forks-p widget)
                          (not fork))
                  (render repository-widget))))
      ;; Alternative method of adding project
      (render-url-input widget)))
  
  
  (:method ((state t) (widget repositories))
    (weblocks/html:with-html
      (:p ("State \"~A\" is not supported yet." state)))))


(defmethod render ((widget repositories))
  (setf (get-title)
        "Github Repositories")

  (let ((state (get-state widget)))
    (log:debug "Rendering in" state)

    (render-with-state state
                       widget)))


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
         (url (rutils:fmt "https://api.github.com/repos/~A/hooks" name)))
    
    (cond ((str:containsp "localhost" hook)
           (log:warn "Don't adding hook in development mode"))
          ((get-hook-id repository)
           (log:warn "Hmm, hook is already configured for this repository"))
          (t
           (dex:post url
                     :headers `(("Authorization" . ,(rutils:fmt "token ~A" token))
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

    (ultralisp/models/project:turn-off-project2 name)
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
   (update repository)))


(defmethod get-html-tag ((widget repository))
  :tr)

(defmethod render ((widget repository))
  (with-html
    (:td (get-name widget)
         (when (is-fork-p widget)
           (:span :class "label secondary"
                  :style "margin-left: 0.7em"
                  "fork")))
    (:td (:span :style "position: relative; top: 0.3em"
                (render-switch (in-ultralisp-p widget)
                               (lambda (&rest args)
                                 (declare (ignorable args))
                                 (toggle widget))
                               :labels '("On" "Off"))))))


(defmethod weblocks/dependencies:get-dependencies ((widget repositories))
  (append
   (list
    (weblocks-lass:make-dependency
      `(.url-frame
        (tbody :border 0
               (td :padding 0)
               ((:and td (:nth-child 1))
                :padding-right 1em
                :padding-top 1px))))
        
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
