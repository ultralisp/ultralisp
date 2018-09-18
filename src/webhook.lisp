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
                #:last-elt
                #:assoc-value)
  (:import-from #:cl-arrows
                #:->)
  (:import-from #:ultralisp/metadata
                #:get-urn
                #:read-metadata)
  (:import-from #:ultralisp/downloader
                #:update-metadata-repository)
  (:import-from #:cl-strings
                #:split)
  (:export
   #:make-webhook-route))
(in-package ultralisp/webhook)



(defclass webhook-route (route)
  ())


(defun make-webhook-route (&optional (uri "/webhook/github"))
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


(defun get-project-name-from (github-payload)
  (-> github-payload
      (assoc-value "repository" :test 'string-equal)
      (assoc-value "full_name" :test 'string-equal)))


(defun get-main-branch-from (github-payload)
  (-> github-payload
      (assoc-value "repository" :test 'string-equal)
      (assoc-value "master_branch" :test 'string-equal)))


(defun get-branch-from (github-payload)
  (let ((ref (-> github-payload
                 (assoc-value "ref" :test 'string-equal))))
    (when ref
      (last-elt (split ref "/")))))


(defun find-project-related-to (payload)
  "Searches a projects metadata among all projects, known to Ultralisp.
   Returns a metadata object or nil."
  (let ((current-branch (get-branch-from payload))
        (main-branch (get-main-branch-from payload)))
    (cond
      ((and current-branch
            (string-equal current-branch
                          main-branch))

       ;; Now we will search the project from the payload
       ;; among all known projects
       (update-metadata-repository "projects")
     
       (let* ((project-name (get-project-name-from payload))
              (all-metadata (read-metadata "projects/projects.txt")))
         (loop for item in all-metadata
               when (string-equal (get-urn item)
                                  project-name)
                 do (return item))))
      
      (t (if current-branch
             (log:warn "Current branch does not match to main"
                       current-branch
                       main-branch)
             (log:warn "Unable to figure out current branch from the payload"))
         (values)))))


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


(defun update-all ()
  "This function is useful to call after some pull-request was merged to add new projects."
  ;; (update-metadata-repository "projects")
  ;; (ultralisp/webhook::update "projects/projects.txt")
  (let ((projects-dir "build/sources/")
        (dist-dir "build/dist/")
        (projects-metadata-path "projects/projects.txt"))
    (ultralisp/builder:build
     :projects-metadata-path projects-metadata-path
     :projects-dir projects-dir
     :dist-dir dist-dir)))


(defun process-payload (payload)
  (log:debug "Processing payload" payload)
  
  (let* ((project (find-project-related-to payload)))
    (when project
      (update project))))


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

    ;; Github may send us a json payload or x-ww-form-urlencoded
    ;; in first case, body contains already parsed json. In second -
    ;; only alist with single "payload" item with a value of json
    ;; encoded to the string and we need to handle this case specially:
    (let ((payload-as-string (assoc-value body "payload" :test 'string-equal)))
      (when payload-as-string
        (setf body
              (jonathan:parse payload-as-string
                              :as :alist))))

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


