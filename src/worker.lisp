(defpackage #:ultralisp/worker
  (:use #:cl)
  (:import-from #:defmain
                #:defmain)

  (:import-from #:ultralisp/lfarm/core
                #:*after-last-task*)

  ;; These dependencies need for a worker to process version builds
  (:import-from #:ultralisp/downloader/github)
  (:import-from #:ultralisp/downloader/version)
  (:import-from #:ultralisp/downloader/project)
  (:import-from #:ultralisp/pipeline/checking)
  (:import-from #:ultralisp/uploader/fake)
  (:import-from #:ultralisp/uploader/s3))
(in-package ultralisp/worker)


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
                      :flag t
                      :env-var "DEBUG"))

  (cond (debug
         (log4cl-json:setup :level :debug)
         (setf lfarm-common:*log-level* :debug))
        (t
         (log4cl-json:setup :level :info)
         (setf lfarm-common:*log-level* :info)))

  (log:info "Starting lfarm server")

  (when one-task-only
    (setf *after-last-task* 'ultralisp/lfarm/core::on-last-task))

  ;; To make it possible to connect to a remote SLYNK server where ports are closed
  ;; with firewall.
  (setf slynk:*use-dedicated-output-stream* nil)

  (ultralisp/slynk:setup)
  (slynk:create-server :dont-close t
                       :port slynk-port
                       :interface slynk-interface)
  
  (lfarm-server:start-server interface
                             port))
