(uiop:define-package #:ultralisp/server-cli
  (:use #:cl)
  (:import-from #:log4cl)
  (:import-from #:ultralisp/utils
                #:getenv)
  (:import-from #:ultralisp/logging)
  (:import-from #:ultralisp/server
                #:start)
  (:import-from #:40ants-slynk)
  (:import-from #:log4cl-extras/error
                #:with-log-unhandled)
  (:import-from #:defmain
                #:defmain))
(in-package #:ultralisp/server-cli)


(defmain (main) ((dont-start-server "Don't start HTTP server."
                                    :flag t)
                 (log-dir "A directory to store app.log."
                          :default "/app/logs")
                 (debug "If true, then log will be include DEBUG and INFO nessages"
                        :flag t
                        :short nil
                        :env-var "DEBUG"))

  
  (ultralisp/logging:setup log-dir
                           :level (if debug
                                      :info
                                      :error))

  (with-log-unhandled ()
    (let ((interface (getenv "INTERFACE" "0.0.0.0"))
          (port (getenv "PORT" 80))
          (hostname (machine-instance))
          (debug (when (getenv "DEBUG")
                   t)))

      (log:info "Starting the server")

      (40ants-slynk:start-slynk-if-needed)
      
      ;; Now we'll ensure that tables are exists in the database
      ;; (migrate)
      
      (unless dont-start-server
        (format t "Starting HTTP server on ~A:~A~%"
                interface
                port)
        (start :port port
               :interface interface
               :debug debug))))

  (loop
    do (sleep 60)))
