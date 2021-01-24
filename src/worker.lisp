(defpackage #:ultralisp/worker
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
  (:export
   #:process-jobs
   #:start-outside-docker))
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


(defun process-jobs (&key one-task-only)

  (setf log4cl-extras/error:*args-filters*
        (list 'hide-job-args
              (make-secrets-replacer)))
  
  ;; May be we need to process a CL-GEARMAN::GEARMAN-CONNECTION-ERROR
  ;; here. This condition will be thrown in case, if gearman server is
  ;; unavailable.
  (cl-gearman:with-worker (worker (get-gearman-server)) 
    (cl-gearman:add-ability worker "task-with-commands"
                            (lambda (arg job)
                              (declare (ignore job))
                              (let ((args (deserialize arg)))
                                (log:info "Processing task" args)
                                (serialize
                                 (apply #'task-with-commands
                                        args)))))
    
    (cond
      (one-task-only (cl-gearman:work worker)
                     (log:info "One job was processed, exiting."))
      (t (loop (cl-gearman:work worker))))))


(defmain main ((slynk-port "A port to listen for connection from SLY."
                           :default 10200
                           :short nil)
               (slynk-interface "An interface to listen on for connection from SLY."
                                :default "localhost"
                                :short nil)
               (one-task-only "If true, then worker will quit after the task processing."
                              :flag t)
               (debug "If true, then output will be verbose"
                      :flag t
                      :env-var "DEBUG"))
  
  (log4cl-extras/config:setup
   `(:level ,(if debug
                 :info
                 :error)
     :appenders ((daily :layout :json
                        :name-format "/app/logs/worker.log"
                        :backup-name-format "worker-%Y%m%d.log"))))

  ;; To make it possible to connect to a remote SLYNK server where ports are closed
  ;; with firewall.
  (setf slynk:*use-dedicated-output-stream* nil)

  (ultralisp/slynk:setup)
  (slynk:create-server :dont-close t
                       :port slynk-port
                       :interface slynk-interface)
  
  (log:info "Waiting for tasks")
  (process-jobs :one-task-only one-task-only))


(defun start-outside-docker ()
  (ultralisp/logging:setup-for-repl :level "error"
                                    :app "worker")
  (process-jobs))
