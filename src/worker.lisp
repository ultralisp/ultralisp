(defpackage #:ultralisp/worker
  (:use #:cl)
  (:import-from #:defmain
                #:defmain)
  (:import-from #:ultralisp/variables
                #:get-gearman-server)

  ;; These dependencies need for a worker to process version builds
  (:import-from #:ultralisp/downloader/github)
  (:import-from #:ultralisp/downloader/version)
  (:import-from #:ultralisp/downloader/project)
  (:import-from #:ultralisp/pipeline/checking)
  (:import-from #:ultralisp/uploader/fake)
  (:import-from #:ultralisp/uploader/s3))
(in-package ultralisp/worker)


(defun serialize (object)
  (base64:usb8-array-to-base64-string
   (flex:with-output-to-sequence (stream)
     (cl-store:store object stream))))

(defun deserialize (base64-string)
  (flex:with-input-from-sequence
      (stream (base64:base64-string-to-usb8-array base64-string))
    (cl-store:restore stream)))


(defun process-jobs (&key one-task-only)
  ;; May be we need to process a CL-GEARMAN::GEARMAN-CONNECTION-ERROR
  ;; here. This condition will be thrown in case, if gearman server is
  ;; unavailable.
  (cl-gearman:with-worker (worker (get-gearman-server)) 
    (cl-gearman:add-ability worker "task-with-commands"
                            (lambda (arg job)
                              (declare (ignorable job))
                              (let ((args (deserialize arg)))
                                (log:info "Processing task" args)
                                (serialize
                                 (apply #'ultralisp/rpc/command:task-with-commands
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
  ;; To add a new line before any JSON log items will go
  (format t "~2&")
  
  (log4cl-json:setup :level (if debug
                                :info
                                :error))

  ;; To make it possible to connect to a remote SLYNK server where ports are closed
  ;; with firewall.
  (setf slynk:*use-dedicated-output-stream* nil)

  (ultralisp/slynk:setup)
  (slynk:create-server :dont-close t
                       :port slynk-port
                       :interface slynk-interface)
  
  (log:info "Waiting for tasks")
  (process-jobs :one-task-only one-task-only))
