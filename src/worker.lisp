(uiop:define-package #:ultralisp/worker
  (:use #:cl)
  (:import-from #:defmain
                #:defmain)
  (:import-from #:log4cl-extras/config)
  (:import-from #:ultralisp/logging)
  (:import-from #:ultralisp/variables
                #:get-gearman-server)

  ;; These dependencies need for a worker to process version builds
  (:import-from #:ultralisp/downloader/github)
  (:import-from #:ultralisp/downloader/version)
  (:import-from #:ultralisp/downloader/source)
  (:import-from #:ultralisp/pipeline/checking)
  (:import-from #:ultralisp/uploader/fake)
  (:import-from #:ultralisp/uploader/s3)
  ;; And we need this to index packages
  (:import-from #:ultralisp/search)
  (:import-from #:ultralisp/rpc/command
                #:task-with-commands)
  (:import-from #:ultralisp/rpc/core
                #:serialize
                #:deserialize)
  (:import-from #:log4cl-extras/error
                #:make-placeholder
                #:make-args-filter)
  (:import-from #:log4cl-extras/secrets
                #:make-secrets-replacer)
  (:import-from #:rutils
                #:fmt)
  (:export
   #:process-jobs
   #:start-outside-docker
   #:start-lispworks-worker))
(in-package ultralisp/worker)


(defparameter +hidden-arg+
  (make-placeholder "hidden arg"))


(defun hide-job-args (func-name args)
  (flet ((replace-job-arg (arg)
           (if (typep arg 'cl-gearman::job)
               +hidden-arg+
               arg)))
    (cond
      ((and (consp func-name)
            (eql (first func-name)
                 'lambda)
            (eql (fourth func-name)
                 'process-jobs))
       (values func-name
               (mapcar (constantly +hidden-arg+)
                       args)))
      (t (values func-name
                 (mapcar #'replace-job-arg
                         args))))))


(defun process-jobs (&key one-task-only gearman-server)

  (setf log4cl-extras/error:*args-filters*
        (list 'hide-job-args
              (make-secrets-replacer)))
  
  ;; May be we need to process a CL-GEARMAN::GEARMAN-CONNECTION-ERROR
  ;; here. This condition will be thrown in case, if gearman server is
  ;; unavailable.
  (cl-gearman:with-worker (worker (or gearman-server
                                      (get-gearman-server))) 
    (cl-gearman:add-ability worker
                            (fmt "~A-task-with-commands"
                                 (string-downcase
                                  (lisp-implementation-type)))
                            (lambda (arg job)
                              (declare (ignore job))
                              (let ((args (deserialize arg)))
                                (log:info "Processing task" args)
                                (serialize
                                 (apply #'task-with-commands
                                        args)))))
    
    (cond
      (one-task-only (cl-gearman:work worker)
                     (log:info "One job was processed, exiting.")
                     (uiop:quit))
      (t (loop (cl-gearman:work worker))))))


(defmain (main) ((slynk-port "A port to listen for connection from SLY."
                             :default 10200
                             :short nil)
                 (slynk-interface "An interface to listen on for connection from SLY."
                                  :default "localhost"
                                  :short nil)
                 (one-task-only "If true, then worker will quit after the task processing."
                                :flag t)
                 (log-file "Path to a log file"
                           :default "/app/logs/worker.log")
                 (debug "If true, then output will be verbose"
                        :flag t
                        :env-var "DEBUG")
                 (help "Show help and exit"
                       :flag t))
  (when help
    (defmain:print-help)
    (uiop:quit 0))
  
  (log4cl-extras/config:setup
    `(:level ,(if debug
                  :info
                  :error)
      :appenders ((daily :layout :json
                         :name-format ,log-file
                         :backup-name-format "worker-%Y%m%d.log"))))

  ;; To make it possible to connect to a remote SLYNK server where ports are closed
  ;; with firewall.
  (setf slynk:*use-dedicated-output-stream* nil)

  (ultralisp/slynk:setup)
  (slynk:create-server :dont-close t
                       :port slynk-port
                       :interface slynk-interface)
  
  (log:info "Waiting for tasks" one-task-only)
  #+lispworks
  (let ((ultralisp/rpc/command::*db-host-override* "localhost")
        (ultralisp/rpc/command::*db-port-override* 25432))
    (process-jobs :gearman-server "localhost:24730"
                  :one-task-only one-task-only))
  #-lispworks
  (process-jobs :one-task-only one-task-only))


(defun start-outside-docker ()
  (ultralisp/logging:setup-for-repl :level :debug 
                                    :app "worker")
  
  (loop with vars = '(("ELASTIC_SEARCH_HOST" "localhost"))
        for (name value) in vars
        do (setf (uiop/os:getenv name)
                 value))

  (function-cache:clear-cache-all-function-caches)
  
  (process-jobs))


(defun start-lispworks-worker ()
  (ultralisp/logging:setup-for-repl :level :debug 
                                    :app "worker")
  
  (let ((ultralisp/rpc/command::*db-host-override* "localhost")
        (ultralisp/rpc/command::*db-port-override* 25432))
    (process-jobs :gearman-server "localhost:24730")))
