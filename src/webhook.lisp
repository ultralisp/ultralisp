(defpackage #:ultralisp/webhook
  (:use #:cl)
  (:import-from #:ultralisp/builder)
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


(defvar *payloads* nil)


(defun find-project-related-to (payload)
  "Searches a projects metadata among all projects, known to Ultralisp.
   Returns a metadata object or nil."
  (let* ((project-name (assoc-value payload "project" :test #'string-equal))
         (all-metadata (read-metadata "projects/projects.txt")))
    (loop for item in all-metadata
          when (string-equal (get-urn item)
                             project-name)
            do (return item))))


(defun update (metadata)
  (let ((projects-dir "build2/sources/")
        (dist-dir "build2/dist/")
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
  (let* ((project (find-project-related-to payload)))
    (if project
        (update project)
        (log:error "Project now found" payload))))


(defmethod serve ((route webhook-route) env)
  "Returns a robots of the site."
  (let* ((body (getf env :body-parameters)))
    (push body *payloads*)

    
    ;; In real life we need to put a payload into the queue
    ;; and processe them sequentially, but for now I'm hacking
    ;; this process and will not update repositories in parallel.
    (process-payload body)
  
    (list 200
          '(:content-type "text/plain")
          (list "OK"))))


;; Probably this could be removed because make-webhook-route is called from the app initialization code
;; (weblocks/hooks:on-application-hook-start-weblocks
;;   enable-webhook ()
  
;;   (weblocks/hooks:call-next-hook)
;;   (make-webhook-route))
