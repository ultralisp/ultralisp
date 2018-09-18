(defpackage #:ultralisp/server
  (:use #:cl)
  (:import-from #:log4cl-json)
  (:import-from #:slynk)
  ;; To make inplace links work in the HTML
  (:import-from #:spinneret/cl-markdown)
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
                #:getenv)
  (:import-from #:ultralisp/webhook
                #:make-webhook-route)
  (:import-from #:ultralisp/analytics
                #:render-google-counter
                #:render-yandex-counter)
  (:export
   #:main
   #:start
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

            (*
             :box-sizing "border-box")
            (a
             ;; special color for links
             :color "#0071d8")
            
            (.page-footer :color "#AAA"
                          :margin-top 3em)))

        ;; (weblocks-lass:make-dependency
        ;;   '(.page-header
        ;;     :border-bottom 5px solid "#555"
        ;;     :padding-right 0.5rem
        ;;     :padding-left 0.5rem
        ;;     :padding-top 1rem
        ;;     :margin-bottom 2rem))
        
        ;; (weblocks-lass:make-dependency
        ;;   '(:media "screen and (max-width: 39.9375em)"
        ;;     (.site-name
        ;;      :visibility hidden)
        ;;     (.motto
        ;;      :text-align right)))
        
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


;; Top level start & stop scripts

(defvar *app* nil
  "App's instance.")


(defun start (&rest args)
  "Starts the application by calling 'weblocks/server:start' with appropriate
arguments."

  (setf *cache-remote-dependencies-in*
        ;; TODO: make configurable
        #P"/tmp/weblocks-cache/ultralisp/")
  (setf (get-language)
        "en")

  (mito:connect-toplevel :postgres
                         :host (or (uiop:getenv "POSTGRES_HOST")
                                   "localhost")
                         :database-name (or (uiop:getenv "POSTGRES_DBNAME")
                                            "ultralisp")
                         :username (or (uiop:getenv "POSTGRES_USER")
                                       "ultralisp")
                         :password (or (uiop:getenv "POSTGRES_PASS")
                                       "ultralisp"))

  
  (apply #'weblocks/server:start args)

  (setf *app*
        (weblocks/app:start 'app)))


(defun stop ()
  "Stops the application by calling 'stop-weblocks'."
  (weblocks/server:stop))


(defun restart (&rest args)
  (stop)
  (sleep 3)
  (apply #'start args))


(defun main (&rest argv)
  (declare (ignorable argv))

  (log4cl-json:setup :level :debug)

  (let ((slynk-port 4005)
        (slynk-interface (getenv "SLYNK_INTERFACE" "0.0.0.0"))
        (interface (getenv "INTERFACE" "0.0.0.0"))
        (port (getenv "PORT" 80))
        (num-workers (getenv "NUM_PROCESSES" 4))
        (hostname (machine-instance))
        (debug (when (getenv "DEBUG")
                 t)))

    (format t "Starting slynk server~%")

    (slynk:create-server :dont-close t
                         :port slynk-port
                         :interface slynk-interface)

    (start :port port
           :interface interface
           :debug debug)

    (format t "To start HTTP server:~%")
    (format t "Run ssh -6 -L ~A:localhost:4005 ~A~%"
            slynk-port
            hostname)
    (format t "Then open local Emacs and connect to the slynk on 4005 port~%")
    (format t "Evaluate:~%(server:stopserver)~%(server:runserver)~%~%in LISP repl and start hacking.~%"))

  ;; теперь ждем вечно, пока кто-нибудь не присоединится
  (do ()
      (nil)
    (format t "Waiting for slynk connection.~%")
    (sleep 60))
  
  (format t "Exiting. Why? I don't know! This should never happen~%"))
