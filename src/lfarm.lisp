(defpackage #:ultralisp/lfarm
  (:use #:cl)
  (:use #:log4cl)
  (:import-from #:slynk)
  (:import-from #:defmain
                #:defmain)
  
  ;; These dependencies need for a worker to process version builds
  (:import-from #:ultralisp/downloader/github)
  (:import-from #:ultralisp/downloader/version)
  (:import-from #:ultralisp/downloader/project)
  (:import-from #:ultralisp/uploader/fake)
  (:import-from #:ultralisp/uploader/s3)
  
  (:import-from #:lfarm-client)
  (:import-from #:lfarm-server
                #:kill-tasks
                #:+corrupt-stream-flag+
                #:send-object
                #:current-thread
                #:with-tag
                #:socket-close*
                #:with-task-index
                #:read-and-process-task
                #:ignore-errors/log)
  (:import-from #:lfarm-common
                #:socket-stream
                #:unwind-protect/safe
                #:with-errors-logged
                #:receive-object)
  (:export
   #:submit-task
   #:connect-to-servers))
(in-package ultralisp/lfarm)

;; This package contains a patched functions from lfarm-server
;; to be able to quit worker correctly when it processed given
;; number of tasks.
;; We need it to build each version in a clean separate process.

(defvar *after-last-task* nil
  "A function to call when the last task was processed. It could stop or restart the worker, for example.")


(defun task-loop (stream socket)
  (log:info "start task loop" stream (current-thread))
  (let ((num-processed-tasks 0)
        (num-tasks-to-process 1))
    (with-tag :next-task
      (log:info "reading next task")
      (when (< num-processed-tasks
               num-tasks-to-process)
        (flet ((clean-return (err)
                 (declare (ignore err))
                 (log:info "end task loop" stream)
                 (return-from task-loop))
               (corrupt-handler (err)
                 (log:info "corrupted stream" err stream)
                 (ignore-errors/log (send-object +corrupt-stream-flag+ stream))
                 (go :next-task))
               (next-task ()
                 "This function will be called to proceed to a new task if current was processed or if some
                  error was raised during it's processing."
                 (incf num-processed-tasks)
                 (go :next-task)))
          
          (read-and-process-task stream
                                 #'clean-return
                                 #'corrupt-handler
                                 #'next-task)
          
          (next-task)))
      
      (log:info "exit from task-loop because of tasks limit" stream)

      (when *after-last-task*
        (funcall *after-last-task*
                 socket)))))


(defun respond (message stream socket)
  (ecase message
    (:ping (send-object :pong stream))
    (:task-loop (send-object :in-task-loop stream)
                (with-task-index
                  (task-loop stream socket)))
    (:kill-tasks (kill-tasks (receive-object stream)))))


(defun lfarm-server::call-respond (message socket)
  (with-errors-logged
    (unwind-protect/safe
     :main (respond message (socket-stream socket) socket)
     :cleanup (socket-close* socket))))


(defun on-last-task (socket)
  (lfarm-server::socket-close* socket)
  (uiop:quit))


(defun lfarm-common::write-log (level package message &rest args)
  (let ((*package* package))
    (if (string= level
                 "info")
        (log:info message args)
        (log:error message args))))


(defun submit-task (func &rest args)
  "Submits a task to one of remote workers and waits for the result."
  (check-type func symbol)
  
  (let ((channel (lfarm:make-channel)))
    (loop
      do (handler-case
             (return-from submit-task
               (progn
                 (apply #'lfarm:submit-task* channel func args)
                 (lfarm:try-receive-result channel :timeout (* 24 60 60))))
           #+ccl
           (ccl:socket-error ()
             (log:info "Socket error catched, retrying task"))))))


(defun connect-to-servers (&key (servers '(("127.0.0.1" 10100))))
  "This function is for use in the main part of the Ultralisp.
   It will connect to one or many workers which will build versions and run checks."
  (setf lfarm:*kernel* (lfarm:make-kernel servers)))


(defmain main ((port "A port to listen on."
                     :default 10100)
               (interface "A interface to listen on."
                          :default "localhost")
               (slynk-port "A port to listen for connection from SLY."
                           :default 10200
                           :short nil)
               (slynk-interface "An interface to listen on for connection from SLY."
                                :default "localhost"
                                :short nil)
               (one-task-only "If true, then worker will quit after the task processing."
                              :flag t)
               (debug "If true, then output will be verbose"
                      :flag t))

  (log:info "Starting lfarm server")
  (cond (debug
         (log:config :sane2 :debug)
         (setf lfarm-common:*log-level* :debug))
        (t
         (log:config :sane2 :info)
         (setf lfarm-common:*log-level* :info)))
  
  (when one-task-only
    (setf *after-last-task* 'on-last-task))

  (slynk:create-server :dont-close t
                       :port slynk-port
                       :interface slynk-interface)
  
  (lfarm-server:start-server interface
                             port))
