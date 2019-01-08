(defpackage #:ultralisp/server
  (:use #:cl)
  (:import-from #:woo)
  (:import-from #:spinneret/cl-markdown)
  (:import-from #:ultralisp/lfarm)
  (:import-from #:log4cl-json)
  (:import-from #:ultralisp/github/core)
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
  (:import-from #:weblocks/app
                #:defapp)
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
                #:abort-processing)
  (:import-from #:uiop
                #:print-backtrace)
  (:import-from #:ultralisp/widgets/main
                #:make-main-widget)
  (:import-from #:ultralisp/utils
                #:parse-workers-hosts
                #:getenv)
  (:import-from #:ultralisp/file-server)
  (:import-from #:ultralisp/models/migration
                #:migrate)
  (:import-from #:ultralisp/github/webhook
                #:make-webhook-route)
  (:import-from #:ultralisp/analytics
                #:render-google-counter
                #:render-yandex-counter)
  (:import-from #:ultralisp/models/user
                #:user)
  (:import-from #:ultralisp/models/moderator)
  (:import-from #:ultralisp/mail
                #:send-login-code)
  (:import-from #:lparallel
                #:make-kernel)
  (:import-from #:alexandria
                #:remove-from-plistf
                #:make-keyword)
  (:import-from #:ultralisp/uploader/base
                #:make-uploader
                #:*uploader-type*)
  (:import-from #:weblocks/request-handler
                #:handle-client-request)
  (:import-from #:ultralisp/db
                #:with-connection)
  (:import-from #:defmain
                #:defmain)
  (:import-from #:ultralisp/variables
                #:get-github-client-id
                #:get-github-secret
                #:get-lfarm-workers)
  (:shadow #:restart)
  (:export
   #:main
   #:start
   #:restart
   #:stop))
(in-package ultralisp/server)


(defapp app
  :prefix "/"
  :description "The UltraLisp.org server."
  :autostart nil
  :debug t)


(defmethod weblocks/session:init ((app app))
  (make-main-widget))


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

            (*
             :box-sizing "border-box")
            (a
             ;; special color for links
             :color "#0071d8")

            (.page-header :border-bottom 1px solid "#add8e6"
                          :padding-bottom 0.5rem
                          :margin-bottom 1rem)
            
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
            (.motto
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
  
  ;; (with-html
  ;;   (:link :rel "icon"
  ;;          :type "image/png"
  ;;          :href "/favicon.png")
  ;;
  ;;   (render-google-counter))
  )


(defmethod weblocks/page:render-body ((app app) body-string)
  "Default page-body rendering method"
  (let ((spinneret::*pre* t))
    (render-yandex-counter)
    (render-google-counter))
  
  (with-html
    (:div :class "grid-x"
          (:div :class "cell small-12 medium-10 medium-offset-1 large-8 large-offset-2"
                (:header :class "page-header"
                         (:h1 :class "site-name"
                              (:a :href "/" "Ultralisp.org"))
                         (:h2 :class "motto"
                              "A fast-moving Common Lisp software distribution."))
                (:div :class "page-content"
                      (let ((spinneret::*pre* t))
                        (with-html (:raw body-string))))


                (:footer :class "page-footer"
                         (:p ("Proudly served by [Common Lisp](http://lisp-lang.org) and [Weblocks](http://40ants.com/weblocks/)!")))))))


(defmethod initialize-instance ((app app) &rest args)
  (declare (ignorable args))
  
  (make-webhook-route)
  (ultralisp/file-server:make-route)

  ;; (serve-static-file
  ;;  "/favicon.png"
  ;;  (asdf:system-relative-pathname :app "second-favicon.png"))
  
  (call-next-method))


(defmethod on-error ((app app) condition)
  "Default implementation returns a plain text page and 500 status code."
  (declare (ignorable app))
  
  (when (weblocks/debug:status)
    (invoke-debugger condition))
  
  (setf (weblocks/page:get-title)
        "Some shit happened with ultralisp.org")

  (when condition
    (let ((traceback (print-backtrace :condition condition
                                      :stream nil)))
      (log:error "Returning 500 error to user" traceback)))

  (abort-processing
   ;; TODO: replace with weblocks/response:return-page
   (with-html-string
     (weblocks/page:render
      (weblocks/app:get-current)
      (with-html-string
        (:h3 "Some shit happened.")
        (:h4 "Don't panic."))))
   :code 500
   :content-type "text/html"))


(defmethod handle-client-request ((app app))
  "Here we create a new connection and start new transaction on each request."
  (with-connection ()
    (call-next-method)))


;; Top level start & stop scripts

(defvar *app* nil
  "App's instance.")


(defvar *previous-args* nil
  "Arguments of the previos `start' call. Used to restart
   server with same arguments.")


(defun start (&rest args &key lfarm-workers &allow-other-keys)
  "Starts the application by calling 'weblocks/server:start' with appropriate
arguments."
  (log:info "Starting ultralisp" args)

  (setf *previous-args* args)
  
  (remove-from-plistf args :lfarm-workers)

  (setf lparallel:*kernel* (make-kernel 8
                                        :name "parallel worker"))

  (setf mito-email-auth/models:*user-class*
        'user)
  
  (setf mito-email-auth/models:*send-code-callback*
        'send-login-code)

  (setf mailgun:*domain* (uiop:getenv "MAILGUN_DOMAIN"))
  (unless mailgun:*domain*
    (log:error "Set MAILGUN_DOMAIN environment variable, otherwise login will not work"))
  
  (setf mailgun:*api-key* (uiop:getenv "MAILGUN_API_KEY"))
  (unless mailgun:*api-key*
    (log:error "Set MAILGUN_API_KEY environment variable, otherwise login will not work"))

  (let ((uploader-type (uiop:getenv "UPLOADER_TYPE")))
    (when uploader-type
      (log:info "Setting uploader type to" uploader-type)
      
      (let ((uploader-type (make-keyword (string-upcase uploader-type))))
        (unless (compute-applicable-methods #'make-uploader (list uploader-type))
          (error "Uploader of type ~S is not supported."
                 uploader-type))
        (setf *uploader-type*
              uploader-type))))
  
  (setf ultralisp/github/core:*client-id* (get-github-client-id))
  (unless ultralisp/github/core:*client-id*
    (log:error "Set GITHUB_CLIENT_ID environment variable, otherwise github integration will not work"))
  
  (setf ultralisp/github/core:*secret* (get-github-secret))
  (unless ultralisp/github/core:*secret*
    (log:error "Set GITHUB_SECRET environment variable, otherwise github integration will not work"))
  
  (setf mailgun:*user-agent* (or (uiop:getenv "USER_AGENT")
                                 "ultralisp (http://ultralisp.org)"))

  
  (setf *cache-remote-dependencies-in*
        ;; TODO: make configurable
        #P"/tmp/weblocks-cache/ultralisp/")
  (setf (get-language)
        "en")

  (let ((lfarm-servers (or lfarm-workers
                           (parse-workers-hosts
                            (get-lfarm-workers)))))
    (when lfarm-servers
      (bordeaux-threads:make-thread
       (lambda ()
         (log:info "Connecting lfarm workers" lfarm-servers)
         (ultralisp/lfarm:connect-to-servers :servers lfarm-servers)
         (log:info "Connected to lfarm workers"))
       :name "Connecting to lfarm workers")))

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


(defmain main ((workers "A comma-separated list of workers to connect to in form \"localhost:10100,localhost:10101\". If not given, then we'll not try to connect to any workers and version building will not be available.")
               (dont-start-server "Don't start HTTP server."
                                  :flag t))
  (log4cl-json:setup :level :debug)

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
             :debug debug
             :lfarm-workers (when workers
                              (parse-workers-hosts workers))))

    (format t "To start HTTP server:~%")
    (format t "Run ssh -6 -L ~A:localhost:4005 ~A~%"
            slynk-port
            hostname)
    (format t "Then open local Emacs and connect to the slynk on 4005 port~%")
    (format t "Evaluate:~%(server:stopserver)~%(server:runserver)~%~%in LISP repl and start hacking.~%"))

  ;; Now we'll wait forever for connections from SLY.
  (loop
    do (sleep 60))
  
  (format t "Exiting. Why? I don't know! This should never happen~%"))
