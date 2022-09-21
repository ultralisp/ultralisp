(uiop:define-package #:ultralisp/server-cli
  (:use #:cl)
  (:import-from #:log4cl)
  (:import-from #:slynk)
  (:import-from #:ultralisp/utils
                #:getenv)
  (:import-from #:ultralisp/logging)
  (:import-from #:ultralisp/server
                #:start)
  (:import-from #:ultralisp/slynk)
  (:import-from #:log4cl-extras/error
                #:with-log-unhandled)
  (:import-from #:defmain
                #:defmain))
(in-package ultralisp/server-cli)


(defvar slynk:*use-dedicated-output-stream*)


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
    (let ((slynk-port 4005)
          (slynk-interface (getenv "SLYNK_INTERFACE" "0.0.0.0"))
          (interface (getenv "INTERFACE" "0.0.0.0"))
          (port (getenv "PORT" 80))
          (hostname (machine-instance))
          (debug (when (getenv "DEBUG")
                   t)))

      (log:info "Starting the server")

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
      (format t "Evaluate:~%(server:stopserver)~%(server:runserver)~%~%in LISP repl and start hacking.~%")))

  ;; Now we'll wait forever for connections from SLY.
  (loop
    do (sleep 60)))
