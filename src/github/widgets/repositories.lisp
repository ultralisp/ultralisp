(uiop:define-package #:ultralisp/github/widgets/repositories
  (:use #:cl)
  (:import-from #:reblocks-parenscript)
  (:import-from #:dexador)
  (:import-from #:reblocks/response
                #:make-uri)
  (:import-from #:ultralisp/sources/github
                #:guess-github-source)
  (:import-from #:jonathan)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:log)
  (:import-from #:parenscript
                #:@
                #:chain)
  (:import-from #:link-header)
  (:import-from #:rutils)
  (:import-from #:lparallel)
  (:import-from #:reblocks/widget
                #:update
                #:get-html-tag
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:f-underscore
                #:f_)
  (:import-from #:reblocks/page
                #:get-title)
  (:import-from #:ultralisp/models/project
                #:get-reason
                #:unable-to-create-project
                #:make-project-from-url
                #:is-enabled-p
                #:add-or-turn-on-project
                #:get-params)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:arrows
                #:->)
  (:import-from #:reblocks-auth/github
                #:get-scopes)
  (:import-from #:quri
                #:url-encode)
  (:import-from #:ultralisp/db
                #:with-connection)
  (:import-from #:log4cl-extras/error
                #:with-log-unhandled)
  (:import-from #:ultralisp/widgets/spinner
                #:make-spinner)
  (:import-from #:reblocks-ui2/widget
                #:render
                #:ui-widget)
  (:import-from #:reblocks-ui2/themes/tailwind
                #:tailwind-theme)
  (:import-from #:ultralisp/widgets/utils
                #:render-switch)
  (:import-from #:reblocks/response
                #:add-retpath-to)
  (:import-from #:ultralisp/models/source
                #:source-params
                #:get-github-sources)
  (:import-from #:str)
  (:import-from #:bordeaux-threads
                #:make-thread)
  (:import-from #:ultralisp/models/dist-source
                #:source->dists)
  (:import-from #:ultralisp/variables
                #:*github-webhook-path*)
  (:import-from #:reblocks/widgets/dom
                #:dom-id)
  (:export #:make-repositories-widget
           #:repositories))
(in-package #:ultralisp/github/widgets/repositories)
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
  
  (let* ((url (if (str:starts-with-p "https://" url)
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
          for username = (first (str:split "/" name))
          do (pushnew username usernames :test #'string-equal)
          finally (return (get-github-sources usernames)))))


(defwidget repositories (ui-widget)
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


(defwidget repository (ui-widget)
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


(defun get-ultralisp-names (repositories)
  (loop for source in (get-sources-for-github-repositories repositories)
        for params = (source-params source)
        for dists = (source->dists source :enabled t)
        when dists
        collect (format nil "~A/~A"
                        (getf params :user-or-org)
                        (getf params :project))))


(defun create-repository-widgets (widget webhook-url)
  "Fetches all Common Lisp repositories from the github
   and creates a `repository' widget for each of them."
  (check-type widget repositories)
  (log:debug "Starting to fetch repositories.")

  (let* ((token (get-oauth-token widget))
         (repositories (get-lisp-repositories :token token))
         ;; A list of repository names which are in Ultralisp already.
         ;; We need it to draw a switcher in a correct state.
         (ultralisp-names (get-ultralisp-names repositories)))

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


(defun get-full-webhook-url ()
  "Returns a full path to a webhook, which can be used in GitHub's settings."
  (make-uri *github-webhook-path*))


(defun set-oauth-token (widget token)
  (log:debug "Setting oauth token")
  (setf (slot-value widget 'oauth-token)
        token
        (slot-value widget 'state)
        :fetching-data)

  (log:debug "Starting thread to retrieve repositories")
  (let ((webhook-url (get-full-webhook-url)))
    (setf (slot-value widget 'thread)
          (make-thread
           (lambda ()
             (with-log-unhandled ()
               (with-connection ()
                 (create-repository-widgets widget webhook-url))))
           :name "searching-for-repositories"))))


(defun render-url-input (widget)
  (with-html ()
    (:p :class "mt-4" "or insert a project's URL:")

    (reblocks-ui/form:with-html-form
        (:post
         (lambda (&key url &allow-other-keys)
           (log:info "CATCHED ARGS" url)
           (handler-case
               (let* ((project (make-project-from-url url))
                      (project-url (ultralisp/models/project:project-url project)))
                 (reblocks/response:redirect project-url))
             (unable-to-create-project (condition)
               (let ((reason (get-reason condition)))
                 (log:error "Setting the reason"
                            (get-reason condition))
                 (setf (get-url-form-error widget)
                       reason)
                 (update widget))))))
      (:div :class "flex flex-col gap-2"
            (:div :class "flex gap-2 items-start"
                  (:input :type "text"
                          :name "url"
                          :class "border rounded px-2 py-1 flex-grow"
                          :placeholder "https://github.com/takagi/lake or https://git.sr.ht/~fosskers/cl-transducers")
                  (:input :type "submit"
                          :class "px-4 py-1 bg-sky-600 text-white rounded hover:bg-sky-700 cursor-pointer"
                          :value "Add"))
            (if (get-url-form-error widget)
                (:p :class "text-red-600 text-sm"
                    (get-url-form-error widget))
                (:p :class "text-gray-500 text-sm"
                    "But this way a project we'll not be able to setup a webhook and project will be updated only by cron."))))))


(defgeneric render-with-state (widget state theme)
  (:method ((state (eql :checking-scopes)) (widget repositories) (theme tailwind-theme))
    (let* ((token (reblocks-auth/github:get-token))
           (user (reblocks-auth/models:get-current-user))
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
         (render widget theme))
        
        (t (reblocks/html:with-html ()
             (cond
               (user
                (:p "To show all your public repositories, we need additional permissions from GitHub.")
                (:p :class "mt-2"
                    (reblocks-auth/github:render-button
                     :class "inline-block px-3 py-1.5 bg-sky-600 text-white rounded hover:bg-sky-700"
                     :scopes (append *required-scopes*
                                     reblocks-auth/github:*default-scopes*)))
                (render-url-input widget))
               (t
                (:p "To be able to add your public repositories, you need to login using GitHub.")
                (:p :class "mt-2"
                    (reblocks-auth/github:render-button
                        :text "Login"
                        :class "inline-block px-3 py-1.5 bg-sky-600 text-white rounded hover:bg-sky-700"
                        :scopes (append *required-scopes*
                                        reblocks-auth/github:*default-scopes*))))))))))
  
  (:method ((state (eql :fetching-data)) (widget repositories) (theme tailwind-theme))
    (reblocks/html:with-html ()
      (:p "Searching for Common Lisp repositories..."
          (render (get-spinner widget) theme))
      (reblocks-ui/form:with-html-form
          (:post (lambda (&rest args)
                   (declare (ignorable args))
                   (log:debug "User clicked")
                   (update widget)))
        (:input :type "submit"
                :class "px-4 py-2 bg-sky-600 text-white rounded hover:bg-sky-700 cursor-pointer js-refresh-btn"
                :value "Refresh"))))
  
  (:method ((state (eql :data-fetched)) (widget repositories) (theme tailwind-theme))
    (with-html ()
      (:div :class "flex gap-2"
            (:div "Show forks?")
            (render-switch (show-forks-p widget)
                           (lambda (&rest args)
                             (declare (ignorable args))
                             (setf (show-forks-p widget)
                                   (not (show-forks-p widget)))
                             (update widget))
                           :labels '("Yes" "No")))
      
      (:table :class "mt-2 data-fetched"
              (loop for repository-widget in (get-repository-widgets widget)
                    for fork = (is-fork-p repository-widget)
                    do (when (or (show-forks-p widget)
                                 (not fork))
                         (render repository-widget theme))))
      (render-url-input widget)))
  
  
  (:method ((state t) (widget repositories) (theme tailwind-theme))
    (reblocks/html:with-html ()
      (:p ("State \"~A\" is not supported yet." state)))))


(defmethod render ((widget repositories) (theme tailwind-theme))
  (setf (get-title)
        "Github Repositories")

  (let ((state (get-state widget)))
    (log:debug "Rendering in" state)

    (render-with-state state
                       widget
                       theme)))


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
    
    (let ((repository-url (fmt "https://github.com/~A" name)))
      (add-or-turn-on-project (guess-github-source repository-url)))
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
  (when (reblocks/request:ajax-request-p)
   (update repository)))


(defmethod get-html-tag ((widget repository))
  :tr)

(defmethod render ((widget repository) (theme tailwind-theme))
  (with-html ()
    (:td (get-name widget)
         (when (is-fork-p widget)
           (:span :class "ml-2 text-xs bg-gray-200 text-gray-600 px-2 py-0.5 rounded"
                  "fork")))
    (:td (render-switch (in-ultralisp-p widget)
                        (lambda (&rest args)
                          (declare (ignorable args))
                          (toggle widget))
                        :labels '("On" "Off")))))


(defmethod reblocks-ui2/widget:get-dependencies ((widget repositories) (theme t))
  (let ((refresh-button-selector (fmt "#~A .js-refresh-btn"
                                      (dom-id widget)))
        (data-fetched-selector (fmt "#~A .data-fetched"
                                    (dom-id widget))))
    (append
     (list
      (reblocks-parenscript:make-dependency*
       `(let ((timer (@ window repositories-timer)))
          (unless timer
            (chain console (log "Installing timer to check when repositories will be discovered"))
            (setf (@ window repositories-timer)
                  (set-interval
                   (lambda ()
                     (let ((refresh-button (chain (j-query ,refresh-button-selector)))
                           (data-fetched (chain (j-query ,data-fetched-selector) length )))
                       (when (@ refresh-button length)
                         (chain console (log "Still fetching data"))
                         (chain refresh-button (click)))
                       (when data-fetched
                         (chain console (log "Data is ready"))
                         (clear-interval (@ window repositories-timer)))))
                   5000))))))
     (call-next-method))))


(defmethod reblocks/widget:get-css-classes ((widget repositories))
  (cons (get-state widget)
        (call-next-method)))
