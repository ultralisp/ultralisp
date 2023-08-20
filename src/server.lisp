(defpackage #:ultralisp/server
  (:use #:cl)
  (:import-from #:reblocks-auth)
  (:import-from #:ultralisp/mail)
  (:import-from #:ultralisp/metrics)
  (:import-from #:woo)
  (:import-from #:lack.middleware.mount)
  (:import-from #:github)
  (:import-from #:cl-fad)
  (:import-from #:dbi)
  (:import-from #:dbi.cache.thread)
  (:import-from #:dexador.connection-cache)
  (:import-from #:log)
  (:import-from #:reblocks-auth/github)
  (:import-from #:spinneret)
  (:import-from #:spinneret/cl-markdown)
  (:import-from #:ultralisp/logging)
  (:import-from #:ultralisp/cron)
  (:import-from #:reblocks/app)
  (:import-from #:mailgun)
  (:import-from #:slynk)
  (:import-from #:mito)
  (:import-from #:reblocks/debug)
  (:import-from #:reblocks/server
                #:insert-middleware)
  (:import-from #:reblocks-lass)
  (:import-from #:reblocks/session)
  (:import-from #:reblocks-ui
                #:*foundation-dependencies*)
  (:import-from #:reblocks/page
                #:init-page
                #:render-headers
                #:get-language)
  (:import-from #:reblocks/dependencies
                #:get-dependencies
                #:*cache-remote-dependencies-in*)
  (:import-from #:reblocks/html
                #:with-html
                #:with-html-string)
  (:import-from #:reblocks/response
                #:immediate-response)
  (:import-from #:ultralisp/widgets/main
                #:make-main-routes)
  (:import-from #:ultralisp/utils
                #:make-request-id
                #:getenv)
  (:import-from #:ultralisp/file-server)
  (:import-from #:ultralisp/app
                #:app)
  (:import-from #:ultralisp/models/migration
                #:migrate)
  (:import-from #:ultralisp/github/webhook)
  (:import-from #:ultralisp/metrics)
  (:import-from #:ultralisp/analytics
                #:render-google-counter
                #:render-yandex-counter)
  (:import-from #:ultralisp/models/moderator)
  (:import-from #:ultralisp/mail)
  (:import-from #:lparallel
                #:make-kernel)
  (:import-from #:alexandria
                #:random-elt
                #:remove-from-plistf
                #:make-keyword)
  (:import-from #:ultralisp/uploader/base
                #:make-uploader
                #:*uploader-type*)
  (:import-from #:ultralisp/uploader/fake)
  (:import-from #:ultralisp/uploader/s3)
  (:import-from #:ultralisp/downloader/github)
  (:import-from #:ultralisp/downloader/version)
  (:import-from #:ultralisp/downloader/source)
  (:import-from #:ultralisp/sources/github)
  (:import-from #:reblocks/request)
  (:import-from #:reblocks/request-handler
                #:handle-request)
  (:import-from #:ultralisp/db
                #:with-connection)
  (:import-from #:defmain
                #:defmain)
  (:import-from #:ultralisp/variables
                #:get-github-robot-token
                #:get-dist-dir
                #:get-user-agent
                #:get-mailgun-domain
                #:get-mailgun-api-key
                #:get-github-client-id
                #:get-github-secret
                #:get-uploader-type)
  (:import-from #:function-cache
                #:defcached)
  (:import-from #:ultralisp/models/project
                #:get-all-projects)
  (:import-from #:ultralisp/models/asdf-system)
  (:import-from #:reblocks/error-handler
                #:on-error)

  ;; Extra dependencies with implementation of important protocols:
  (:import-from #:ultralisp/models/moderator)
  ;; Other models, just to be sure
  ;; that datamigration will be generated:
  (:import-from #:ultralisp/models/dist)
  (:import-from #:ultralisp/models/dist-moderator)
  (:import-from #:ultralisp/models/project-moderator)
  ;; (:import-from #:ultralisp/models/source)
  ;; (:import-from #:ultralisp/models/dist-source)
  (:import-from #:ultralisp/widgets/landing)
  (:import-from #:log4cl-extras/context
                #:with-fields)
  (:import-from #:reblocks-auth/models
                #:get-current-user)
  (:import-from #:ultralisp/widgets/maintenance
                #:make-maintenance-widget)
  (:import-from #:ultralisp/utils/lisp
                #:get-compiler-policies)
  (:import-from #:rutils
                #:fmt)
  (:import-from #:str
                #:join)
  (:import-from #:global-vars
                #:define-global-var)
  (:import-from #:cl-info
                #:get-cl-info)
  (:import-from #:log4cl-extras/error
                #:print-backtrace
                #:make-placeholder
                #:make-args-filter
                #:with-log-unhandled)
  (:import-from #:log4cl-extras/secrets
                #:make-secrets-replacer)
  (:import-from #:ultralisp/rpc/core
                #:serialize
                #:deserialize)
  (:import-from #:ultralisp/utils/time
                #:humanize-duration)
  (:import-from #:local-time
                #:now)
  (:import-from #:local-time-duration
                #:timestamp-difference)
  (:import-from #:ultralisp/sources/setup
                #:setup-sources)
  (:import-from #:ultralisp/api/api
                #:api)
  (:import-from #:ultralisp/api/server)
  
  (:shadow #:restart)
  (:export
   #:main
   #:start
   #:restart
   #:stop
   #:start-outside-docker))
(in-package #:ultralisp/server)



(define-global-var +cl-info+
  (join #\Newline
        (list
         (rtl:fmt "~A" (get-cl-info))
         (get-compiler-policies))))

(define-global-var +ultralisp-version+
    (asdf:component-version
     (asdf:find-system :ultralisp)))

(define-global-var *started-at* nil)


(defparameter +search-help+
  (list "signal - this will search in symbol's name and documentation."
        "project:\"40ants/reblocks\" AND symbol:\"request\""
        "package:\"reblocks/actions\" to search all symbols exported from a package."))


(defvar *request-id* nil
  "Here we'll keep current request id when rendering a response to HTTP request or action.")


(defclass ultralisp-server (reblocks/server:server)
  ())


(defmethod init-page ((app app)
                      (url-path string)
                      expire-at)
  (check-type expire-at (or null local-time::timestamp))

  (make-maintenance-widget
   (make-main-routes)))


(defmethod reblocks/server:make-middlewares ((server ultralisp-server) &rest rest)
  (declare (ignore rest))
  
  ;; (reblocks/server:insert-middleware
  ;;  (call-next-method)
  ;;  (cons
  ;;   :websocket
  ;;   (lambda (app)
  ;;     (funcall (lack.util:find-middleware :mount)
  ;;              app
  ;;              "/api/"
  ;;              (make-api-app))))
  ;;  :before :app)
  (let ((api-middleware
          (cons
           :api
           (lambda (app)
             (funcall (lack.util:find-middleware :mount)
                      app
                      "/api"
                      (openrpc-server:make-clack-app api))))))
    (insert-middleware (call-next-method)
                       api-middleware
                       :after :session)))


(defparameter *app-dependencies*
  (list (reblocks-lass:make-dependency
          '(body
            :position absolute
            :height 100%
            :min-height 100%
            :width 100%
            :margin 0
            :padding 0

            (.motto
             :font-size 1.5em)

            (.search-help
             :margin 0
             :font-size 0.75rem
             :position relative
             :top -0.5rem
             :color gray)

            (.num-projects
             :font-size 0.3em
             :top -1.75em)

            (*
             :box-sizing "border-box")
            (a
             ;; special color for links
             :color "#0071d8")

            (.page-header :border-bottom 1px solid "#add8e6"
                          :padding-bottom 0.5rem
                          :margin-bottom 1rem
             ((:and a :hover)
              ;; Don't want a site name change it's color because
              ;; SVG logo doesn't change it.
              :color "#0071d8"))
            
            (.page-footer :color "#AAA"
                          :margin-top 3em)))

        ;; (reblocks-lass:make-dependency
        ;;   '(.page-header
        ;;     :border-bottom 5px solid "#555"
        ;;     :padding-right 0.5rem
        ;;     :padding-left 0.5rem
        ;;     :padding-top 1rem
        ;;     :margin-bottom 2rem))
        
        (reblocks-lass:make-dependency
          '(:media "screen and (max-width: 40em)"
            (.latest-builds
             :display none)
            ((:or .motto .num-projects)
             :display none)
            ((:or .page-content .page-header .page-footer)
             :padding-left 1rem
             :padding-right 1rem)))
        
        ;; (reblocks-parenscript:make-dependency
        ;;   (defun reach-goal (name)
        ;;     "Регистрирует в Яндекс.Метрике достижение цели."

        ;;     (when (@ window ya-counter)
        ;;       (chain window ya-counter (reach-goal name)))

        ;;     (chain console (log (+ "Target " name " was reached")))))
        ))


(defmethod get-dependencies ((app app))
  (append (call-next-method)
          *foundation-dependencies*
          *app-dependencies*))


(defmethod render-headers ((app app))
  "Additional tags for head block."
  (call-next-method)
  
  (with-html
    (:link :rel "icon"
           :type "image/png"
           :href "/images/favicon.png")
    (:link :rel "apple-touch-icon"
           :type "image/png"
           :href "/images/apple-touch-favicon.png")
    ;; Taken from https://simonwhitaker.github.io/github-fork-ribbon-css/
    (:link :rel "stylesheet"
           :href "https://cdnjs.cloudflare.com/ajax/libs/github-fork-ribbon-css/0.2.2/gh-fork-ribbon.min.css")))


(defcached (get-num-projects :timeout (* 60 5)) ()
  (ultralisp/models/project::get-projects-count))


(defun make-version-info ()
  (format nil "~A~@[~2%Uptime: ~A~]"
          +cl-info+
          (when *started-at*
            (humanize-duration
             (timestamp-difference (now)
                                   *started-at*)))))


(defmethod reblocks/page:render-body ((app app) body-string)
  "Default page-body rendering method"
  (let ((spinneret::*pre* t)
        (num-projects (or
                       ;; Here we ignore errors because if there is a problem with DB
                       ;; connection, then we'll not be able to render an error page otherwise.
                       (ignore-errors
                        (with-log-unhandled ()
                            (get-num-projects)))
                       0)))
    (render-yandex-counter)
    (render-google-counter)
  
    (with-html
      (:div :class "grid-x"
            (:div :class "cell small-12 medium-10 medium-offset-1 large-8 large-offset-2"
                  (:header :class "page-header"
                           (:h1 :class "site-name"
                                (:a :href "/" "Ultralisp.org")
                                (unless (zerop num-projects)
                                  (:sup :class "num-projects"
                                        (format nil "includes ~R project~P"
                                                num-projects
                                                num-projects))))
                           (:h2 :class "motto"
                                "A fast-moving Common Lisp software distribution.")
                           (let ((query (reblocks/request:get-parameter "query"))
                                 (show-search (null (uiop:getenv "HIDE_SEARCH"))))
                             (when show-search
                               (:form :method "GET"
                                      :class "search-form"
                                      :action "/search/"
                                      (:input :type "text"
                                              :name "query"
                                              :value query
                                              :placeholder "search a symbol"))
                               (:p :class "search-help"
                                   ("Try: ~A"
                                    (random-elt +search-help+))))))
                  (:div :class "page-content"
                        (let ((spinneret::*pre* t))
                          (with-html (:raw body-string))))


                  (:footer :class "page-footer"
                           (:p "Ultralisp v"
                               (:span :title (make-version-info)
                                      +ultralisp-version+)
                               ("proudly served by [Common Lisp](https://common-lisp.net) and [Reblocks](http://40ants.com/reblocks/)!"))))))))

(defmethod initialize-instance ((app app) &rest args)
  (declare (ignorable args))

  (ultralisp/metrics:initialize)
  
  (ultralisp/file-server:make-route (get-dist-dir)
                                    "/dist/")
  (ultralisp/file-server:make-route #"clpi/"
                                    "/clpi/")
  (ultralisp/file-server:make-route (asdf:system-relative-pathname "ultralisp"
                                                                   "images/")
                                    "/images/")

  ;; (serve-static-file
  ;;  "/favicon.png"
  ;;  (asdf:system-relative-pathname :app "second-favicon.png"))
  
  (call-next-method))


(defmethod on-error ((app app) condition &key backtrace)
  (setf (reblocks/page:get-title)
        "Some shit happened with ultralisp.org")

  (when backtrace
    (with-fields (:uri (reblocks/request:get-path)
                  :user (let ((user (get-current-user)))
                          (cond
                            ((null user)
                             "unknown")
                            ((reblocks-auth/models:anonymous-p user)
                             "anonymous")
                            (t (reblocks-auth/models:get-nickname user)))))
      (log:error "Returning 500 error to user" backtrace)))

  (let ((content
          (cond
            ((reblocks/debug:status)
             (with-html-string
               (:h3 "Some shit happened.")
               (:h4 ("Don't panic. [Fill issue at GitHub](github.com/ultralisp/ultralisp/issues) and ask to fix it!"))
               (:h4 ("Mention ~S request id in the issue." *request-id*))
               (when condition
                 (:h5 ("~A" condition)))
               (when backtrace
                 (:pre backtrace))))
            (t
             (with-html-string
               (:h3 "Some shit happened.")
               (:h4 ("Don't panic. [Fill issue at GitHub](github.com/ultralisp/ultralisp/issues) and ask to fix it!"))
               (:h4 ("Mention ~S request id in the issue." *request-id*)))))))
    
    (immediate-response
     (with-html-string
       (reblocks/page:render
        (reblocks/app:get-current)
        content))
     :code 500
     :content-type "text/html")))


(defmethod handle-request ((app app))
  "Here we create a new connection and start new transaction on each request."
  (with-connection ()
    (let ((*request-id* (make-request-id)))
      (reblocks/response:add-header :x-request-id
                                    *request-id*)
      (with-fields (:request-id *request-id*)
        (call-next-method)))))


;; Top level start & stop scripts

(defvar *previous-args* nil
  "Arguments of the previos `start' call. Used to restart
   server with same arguments.")


(defun lack-env-p (arg)
  (and (listp arg)
       (member :request-method arg)
       (member :request-uri arg)))


(defun start (&key (debug t)
                (port 8080)
                (interface "localhost")
                ;; (server-type :woo)
                (server-type :hunchentoot)
                )
  "Starts the application by calling 'reblocks/server:start' with appropriate arguments."
  ;; Here I'm not using &REST because this way my own defaults
  ;; will not be passed to Reblocks and it will use Hunchentoot as the webserver.
  ;; But hunchentoot was broken recently:
  ;; https://github.com/fukamachi/clack/issues/186
  (let ((args (list :debug debug
                    :port port
                    :interface interface
                    :server-type server-type)))
  
    (log:info "Starting ultralisp" args)

    ;; This way we'll make backtrace nice and safe
    (setf log4cl-extras/error:*args-filters*
          (list (make-args-filter 'lack-env-p
                                  (make-placeholder "lack env"))
                (make-secrets-replacer)))
  
    (setf *previous-args* args)
  
    (setf lparallel:*kernel* (make-kernel 8
                                          :name "parallel worker"))

    (setf mailgun:*domain* (get-mailgun-domain))
    (unless mailgun:*domain*
      (log:error "Set MAILGUN_DOMAIN environment variable, otherwise login will not work"))
  
    (setf mailgun:*api-key* (get-mailgun-api-key))
    (unless mailgun:*api-key*
      (log:error "Set MAILGUN_API_KEY environment variable, otherwise login will not work"))

    (let ((uploader-type (get-uploader-type)))
      (when uploader-type
        (log:info "Setting uploader type to" uploader-type)
      
        (let ((uploader-type (make-keyword (string-upcase uploader-type))))
          (unless (compute-applicable-methods #'make-uploader (list uploader-type :quicklisp))
            (error "Uploader of type ~S is not supported."
                   uploader-type))
          (setf *uploader-type*
                uploader-type))))
  
    (setf reblocks-auth/github:*client-id* (get-github-client-id))
    (unless reblocks-auth/github:*client-id*
      (log:error "Set GITHUB_CLIENT_ID environment variable, otherwise github integration will not work"))
  
    (setf reblocks-auth/github:*secret* (get-github-secret))
    (unless reblocks-auth/github:*secret*
      (log:error "Set GITHUB_SECRET environment variable, otherwise github integration will not work"))

    (setf github:*token* (get-github-robot-token))
    (unless github:*token*
      (log:warn "Set GITHUB_ROBOT_TOKEN environment variable, otherwise github will apply hard rate limit"))

    (setf mailgun:*user-agent* (get-user-agent))
  
    (setf *cache-remote-dependencies-in*
          ;; TODO: make configurable
          #P"/tmp/reblocks-cache/ultralisp/")
    (setf (get-language)
          "en")

    (when (probe-file ".local.lisp")
      (load ".local.lisp"))


    (setf reblocks-auth:*enabled-services*
          (list :github :email))

    (setup-sources)
    
    (log:info "Starting cron jobs")
    (ultralisp/cron:setup)
    (ultralisp/cron:start)

    (log:info "Starting server" args)
    (apply #'reblocks/server:start
           :apps '(app)
           :server-class 'ultralisp-server
           args)
    
    (log:info "DONE")
    (setf *started-at*
          (local-time:now))
    (values)))


(defun start-outside-docker (&key (port 8081))
  "This helper to start Ultralisp in the REPL started outside of Docker.

   First, start Postgres, Gearman and Worker in the Docker Compose:

       lake

   Then open Emacs:

       qlot exec ros emacs


   Open SLY, load `:ultralisp/server` and call `start-outside-docker`.

   This way you'll have two HTTP servers:

   - One started on 8080 port and running inside Docker.
   - Second on 8081 port and running outside."

  #+darwin
  (uiop:run-program "brew install gnu-tar"
                    :ignore-error-status t)

  ;; These vars are the same as in the 
  (loop with vars = '(("GITHUB_CLIENT_ID" "0bc769474b14267aac28")
                      ("GITHUB_SECRET" "3f46156c6bd57f4c233db9449ed556b6e545315a")
                      ("BASE_URL" "http://localhost:8081/dist/")
                      ("CRON_DISABLED" "yes")
                      ("ELASTIC_SEARCH_HOST" "localhost"))
        for (name value) in vars
        do (setf (uiop/os:getenv name)
                 value))

  (function-cache:clear-cache-all-function-caches)

  ;; For some reason default "TEMPORARY-FILES:TEMP-%" does not work on OSX:
  (setf cl-fad::*default-template*
        "/tmp/ultralisp/temp-%")

  (start :port port)

  ;; TODO: think how to redesing Ultralisp logging
  ;; collection to use 40ANTS-LOGGING (https://40ants.com/logging/).
  (ultralisp/logging:setup-for-repl :level "error"
                                    :app "app"))


(defun stop ()
  "Stops the application by calling 'stop-reblocks'."
  (reblocks/server:stop))


(defun restart ()
  (stop)
  (sleep 5)
  (apply #'start *previous-args*))
