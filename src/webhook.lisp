(defpackage #:ultralisp/webhook
  (:use #:cl)
  
  (:import-from #:ultralisp/builder)
  (:import-from #:chanl)
  (:import-from #:ultralisp/uploader)
  (:import-from #:weblocks/routes
                #:serve
                #:add-route
                #:route)
  (:import-from #:routes
                #:parse-template)
  (:import-from #:alexandria
                #:assoc-value)
  (:import-from #:ultralisp/metadata
                #:get-urn
                #:read-metadata)
  (:import-from #:ultralisp/downloader
                #:update-metadata-repository)
  (:export
   #:make-webhook-route))
(in-package ultralisp/webhook)



(defclass webhook-route (route)
  ())


(defun make-webhook-route (&optional (uri "/webhook"))
  (log:info "Making a route for a webhook")
  (let ((route (make-instance 'webhook-route
                              :template (parse-template uri))))
    (add-route route)))


(defvar *payloads* nil
  "We'll put here all payloads for debugging purpose.")

(defvar *queue* (make-instance 'chanl:unbounded-channel)
  "Here we'll put payloads to process in a separate thread, to serialize processing.")


(defvar *processor-thread* nil
  "A link to a thread where all processing will be done.")


(defun find-project-related-to (payload)
  "Searches a projects metadata among all projects, known to Ultralisp.
   Returns a metadata object or nil."
  (update-metadata-repository "projects")
  (let* ((project-name (assoc-value payload "project" :test #'string-equal))
         (all-metadata (read-metadata "projects/projects.txt")))
    (loop for item in all-metadata
          when (string-equal (get-urn item)
                             project-name)
            do (return item))))


(defun update (metadata)
  (let ((projects-dir "build/sources/")
        (dist-dir "build/dist/")
        (projects-metadata-path "projects/projects.txt"))
    (ultralisp/builder:build
     :projects-metadata-path (if (probe-file projects-dir)
                                 ;; If directory with projects already exists
                                 ;; then we will update just a project
                                 ;; mentioned in the payload
                                 metadata
                                 ;; otherwize, will download all known projects
                                 projects-metadata-path)
     :projects-dir projects-dir
     :dist-dir dist-dir)

    ;; Now we'll upload a dist to the server
    (ultralisp/uploader:upload :dir dist-dir)))


(defun process-payload (payload)
  (log:debug "Processing payload" payload)
  
  (let* ((project (find-project-related-to payload)))
    (if project
        (update project)
        (log:error "Project now found" payload))))


(defun process-payloads-from-the-queue ()
  (log:debug "Starting a thread for payload processing")
  (unwind-protect
       (loop for payload = (chanl:recv *queue*)
             do (process-payload payload))
    (log:debug "Exiting from the process payloads thread")))


(defun ensure-processor-thread-is-running ()
  (cond
    ((or (null *processor-thread*)
         (not (bt:thread-alive-p *processor-thread*)))
     (log:debug "Creating a thread")
     (setf *processor-thread*
           (bt:make-thread 'process-payloads-from-the-queue
                           :name "payloads processor")))
    (t (log:debug "Thread for payload processing is already running"))))


(defmethod serve ((route webhook-route) env)
  "Returns a robots of the site."
  (let* ((body (getf env :body-parameters)))
    (log:debug "New payload received" body)

    (push body *payloads*)
    (log:debug "Sending to the queue")
    (chanl:send *queue* body :blockp nil)
    (log:debug "Payload is in the queue now")

    (log:debug "Ensuring thread is running")
    (ensure-processor-thread-is-running)

    (log:debug "DONE")
    
    (list 200
          '(:content-type "text/plain")
          (list "OK"))))


;; Probably this could be removed because make-webhook-route is called from the app initialization code
;; (weblocks/hooks:on-application-hook-start-weblocks
;;   enable-webhook ()
  
;;   (weblocks/hooks:call-next-hook)
;;   (make-webhook-route))


