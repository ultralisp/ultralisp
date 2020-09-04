(defpackage #:ultralisp/server
  (:use #:cl)
  (:import-from #:ultralisp/metrics)
  (:import-from #:woo)
  (:import-from #:weblocks-auth/github)
  (:import-from #:spinneret/cl-markdown)
  (:import-from #:log4cl-extras/config)
  (:import-from #:ultralisp/cron)
  (:import-from #:ultralisp/slynk)
  (:import-from #:mailgun)
  (:import-from #:slynk)
  (:import-from #:mito)
  (:import-from #:weblocks/debug)
  (:import-from #:weblocks/server)
  (:import-from #:weblocks-lass)
  (:import-from #:weblocks/session)
  (:import-from #:weblocks-ui
                #:*foundation-dependencies*)
  (:import-from #:weblocks/page
                #:render-headers
                #:get-language)
  (:import-from #:weblocks/dependencies
                #:get-dependencies
                #:*cache-remote-dependencies-in*)
  (:import-from #:weblocks/html
                #:with-html
                #:with-html-string)
  (:import-from #:weblocks/response
                #:immediate-response)
  (:import-from #:trivial-backtrace
                #:print-backtrace)
  (:import-from #:ultralisp/widgets/main
                #:make-main-routes)
  (:import-from #:ultralisp/utils
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
  (:import-from #:ultralisp/mail
                #:send-login-code)
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
  (:import-from #:ultralisp/downloader/project)
  (:import-from #:weblocks/request)
  (:import-from #:weblocks/request-handler
                #:handle-request)
  (:import-from #:ultralisp/db
                #:with-connection)
  (:import-from #:defmain
                #:defmain)
  (:import-from #:ultralisp/variables
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
  (:import-from #:weblocks/error-handler
                #:on-error)
  (:shadow #:restart)
  (:export
   #:main
   #:start
   #:restart
   #:stop))
(in-package ultralisp/server)


(defparameter +search-help+
  (list "signal - this will search in symbol's name and documentation."
        "project:\"40ants/weblocks\" AND symbol:\"request\""
        "package:\"weblocks/actions\" to search all symbols exported from a package."))

(defmethod weblocks/session:init ((app app))
  (make-main-routes))


(defparameter *app-dependencies*
  (list (weblocks-lass:make-dependency
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

        ;; (weblocks-lass:make-dependency
        ;;   '(.page-header
        ;;     :border-bottom 5px solid "#555"
        ;;     :padding-right 0.5rem
        ;;     :padding-left 0.5rem
        ;;     :padding-top 1rem
        ;;     :margin-bottom 2rem))
        
        (weblocks-lass:make-dependency
          '(:media "screen and (max-width: 40em)"
            (.latest-builds
             :display none)
            ((:or .motto .num-projects)
             :display none)
            ((:or .page-content .page-header .page-footer)
             :padding-left 1rem
             :padding-right 1rem)))
        
        ;; (weblocks-parenscript:make-dependency
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
  (length (get-all-projects :only-enabled t)))


(defmethod weblocks/page:render-body ((app app) body-string)
  "Default page-body rendering method"
  (let ((spinneret::*pre* t)
        (num-projects (or (get-num-projects)
                          0)))
    (render-yandex-counter)
    (render-google-counter)
  
    (with-html
      (:div :class "grid-x"
            (:div :class "cell small-12 medium-10 medium-offset-1 large-8 large-offset-2"
                  (:header :class "page-header"
                           (:h1 :class "site-name"
                                (:a :href "/" "Ultralisp.org")
                                (:sup :class "num-projects"
                                      (format nil "includes ~R project~P"
                                              num-projects
                                              num-projects)))
                           (:h2 :class "motto"
                                "A fast-moving Common Lisp software distribution.")
                           (let ((query (weblocks/request:get-parameter "query"))
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
                           (:p ("Ultralisp ~A. Proudly served by [Common Lisp](https://common-lisp.net) and [Weblocks](http://40ants.com/weblocks/)!"
                                (asdf:component-version
                                 (asdf:find-system :ultralisp))))))))))


(defmethod initialize-instance ((app app) &rest args)
  (declare (ignorable args))

  (ultralisp/metrics:initialize)
  
  (ultralisp/file-server:make-route (get-dist-dir)
                                    "/dist/")
  (ultralisp/file-server:make-route (asdf:system-relative-pathname "ultralisp"
                                                                   "images/")
                                    "/images/")

  ;; (serve-static-file
  ;;  "/favicon.png"
  ;;  (asdf:system-relative-pathname :app "second-favicon.png"))
  
  (call-next-method))


(defmethod on-error ((app app) condition)
  (setf (weblocks/page:get-title)
        "Some shit happened with ultralisp.org")

  (let ((traceback (when condition
                     (print-backtrace condition
                                      :output nil))))
    (when traceback
      (log:error "Returning 500 error to user" traceback))

    (let ((content
            (cond
              ((weblocks/debug:status)
               (with-html-string
                 (:h3 "Some shit happened.")
                 (:h4 ("Don't panic. [Fill issue at GitHub](github.com/ultralisp/ultralisp/issues) and ask to fix it!"))
                 (when condition
                   (:h5 ("~A" condition)))
                 (when traceback
                   (:pre traceback))))
              (t
               (with-html-string
                 (:h3 "Some shit happened.")
                 (:h4 ("Don't panic. [Fill issue at GitHub](github.com/ultralisp/ultralisp/issues) and ask to fix it!")))))))
      
      (immediate-response
       ;; TODO: replace with weblocks/response:return-page
       (with-html-string
         (weblocks/page:render
          (weblocks/app:get-current)
          content))
       :code 500
       :content-type "text/html"))))


(defmethod handle-request ((app app))
  "Here we create a new connection and start new transaction on each request."
  (with-connection ()
    (call-next-method)))


;; Top level start & stop scripts

(defvar *app* nil
  "App's instance.")


(defvar *previous-args* nil
  "Arguments of the previos `start' call. Used to restart
   server with same arguments.")


(defun start (&rest args)
  "Starts the application by calling 'weblocks/server:start' with appropriate
arguments."
  (log:info "Starting ultralisp" args)

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
        (unless (compute-applicable-methods #'make-uploader (list uploader-type))
          (error "Uploader of type ~S is not supported."
                 uploader-type))
        (setf *uploader-type*
              uploader-type))))
  
  (setf weblocks-auth/github:*client-id* (get-github-client-id))
  (unless weblocks-auth/github:*client-id*
    (log:error "Set GITHUB_CLIENT_ID environment variable, otherwise github integration will not work"))
  
  (setf weblocks-auth/github:*secret* (get-github-secret))
  (unless weblocks-auth/github:*secret*
    (log:error "Set GITHUB_SECRET environment variable, otherwise github integration will not work"))

  (setf mailgun:*user-agent* (get-user-agent))
  
  (setf *cache-remote-dependencies-in*
        ;; TODO: make configurable
        #P"/tmp/weblocks-cache/ultralisp/")
  (setf (get-language)
        "en")

  (log:info "Starting cron jobs")
  (ultralisp/cron:setup)
  (ultralisp/cron:start)

  (log:info "Starting server" args)
  (apply #'weblocks/server:start :server-type :woo args)

  (log:info "DONE")
  (setf *app*
        (weblocks/app:start 'app)))


(defun stop ()
  "Stops the application by calling 'stop-weblocks'."
  (weblocks/server:stop))


(defun restart ()
  (stop)
  (sleep 5)
  (apply #'start *previous-args*))


(defvar slynk:*use-dedicated-output-stream*)


(defmain main ((dont-start-server "Don't start HTTP server."
                                  :flag t)
               (log-dir "A directory to store app.log."
                        :default "/app/logs")
               (debug "If true, then log will be include DEBUG and INFO nessages"
                      :flag t
                      :short nil
                      :env-var "DEBUG"))
  
  (log4cl-extras/config:setup
   `(:level ,(if debug
                 :info
                 :error)
     :appenders ((daily :layout :json
                        :name-format ,(format nil "~A/app.log"
                                              log-dir)
                        :backup-name-format "app-%Y%m%d.log"))))
  
  (let ((slynk-port 4005)
        (slynk-interface (getenv "SLYNK_INTERFACE" "0.0.0.0"))
        (interface (getenv "INTERFACE" "0.0.0.0"))
        (port (getenv "PORT" 80))
        (hostname (machine-instance))
        (debug (when (getenv "DEBUG")
                 t)))

    ;; To make it possible to connect to a remote SLYNK server where ports are closed
    ;; with firewall.
    (setf slynk:*use-dedicated-output-stream* nil)
    
    (format t "Starting slynk server on ~A:~A (dedicated-output: ~A)~%"
            slynk-interface
            slynk-port
            slynk:*use-dedicated-output-stream*)

    (ultralisp/slynk:setup)
    (slynk:create-server :dont-close t
                         :port slynk-port
                         :interface slynk-interface)

    ;; Now we'll ensure that tables are exists in the database
    ;; (migrate)

    (unless dont-start-server
      (format t "Starting HTTP server on ~A:~A~%"
              interface
              port)
      (start :port port
             :interface interface
             :debug debug))

    (format t "To start HTTP server:~%")
    (format t "Run ssh -6 -L ~A:localhost:4005 ~A~%"
            slynk-port
            hostname)
    (format t "Then open local Emacs and connect to the slynk on 4005 port~%")
    (format t "Evaluate:~%(server:stopserver)~%(server:runserver)~%~%in LISP repl and start hacking.~%"))

  ;; Now we'll wait forever for connections from SLY.
  (loop
    do (sleep 60)))


(defun serialize (object)
  (base64:usb8-array-to-base64-string
   (flex:with-output-to-sequence (stream)
     (cl-store:store object stream))))

(defun deserialize (base64-string)
  (flex:with-input-from-sequence
      (stream (base64:base64-string-to-usb8-array base64-string))
    (cl-store:restore stream)))


(defun test-gearman (arg)
  (let ((returned-value nil))
    (cl-gearman:with-client (client "gearman:4730")
      (log:info "Submitting job")
      (let* ((raw-result (cl-gearman:submit-job client "upcase" :arg (serialize arg)))
             (result (deserialize raw-result)))
        (log:info "Result received" result)
        (setf returned-value result)))
    returned-value))
