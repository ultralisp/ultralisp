(defpackage #:ultralisp/github/webhook
  (:use #:cl)

  (:import-from #:reblocks/session)
  (:import-from #:ultralisp/builder)
  (:import-from #:chanl)
  (:import-from #:jonathan)
  (:import-from #:log)
  (:import-from #:reblocks/request)
  (:import-from #:ultralisp/uploader/base)
  (:import-from #:routes
                #:parse-template)
  (:import-from #:alexandria
                #:last-elt
                #:assoc-value)
  (:import-from #:arrows
                #:->)
  (:import-from #:ultralisp/metadata
                #:get-urn
                #:read-metadata)
  (:import-from #:str)
  (:import-from #:ultralisp/models/project
                #:get-project2
                #:get-github-project
                #:get-all-projects)
  (:import-from #:reblocks/response
                #:make-uri)
  (:import-from #:ultralisp/models/check
                #:make-checks)
  (:import-from #:log4cl-extras/error
                #:with-log-unhandled)
  (:import-from #:ultralisp/db
                #:with-connection)
  (:import-from #:ultralisp/app
                #:app)
  (:import-from #:rutils
                #:fmt)
  (:import-from #:bordeaux-threads
                #:make-thread)
  (:import-from #:ultralisp/routes
                #:process-webhook-route)
  (:export
   #:get-webhook-url))
(in-package #:ultralisp/github/webhook)


(defvar *github-webhook-path* "/webhook/github")


(defvar *payloads* nil
  "We'll put here all payloads for debugging purpose.")


(defvar *queue* (make-instance 'chanl:unbounded-channel)
  "Here we'll put payloads to process in a separate thread, to serialize processing.")


(defvar *processor-thread* nil
  "A link to a thread where all processing will be done.")


(defun get-user-or-org-from (github-payload)
  (-> github-payload
      (assoc-value "repository" :test 'string-equal)
      (assoc-value "owner" :test 'string-equal)
      (assoc-value "name" :test 'string-equal)))


(defun get-project-name-from (github-payload)
  (-> github-payload
      (assoc-value "repository" :test 'string-equal)
      (assoc-value "name" :test 'string-equal)))


(defun get-main-branch-from (github-payload)
  (-> github-payload
      (assoc-value "repository" :test 'string-equal)
      (assoc-value "master_branch" :test 'string-equal)))


(defun get-branch-from (github-payload)
  (let ((ref (-> github-payload
                 (assoc-value "ref" :test 'string-equal))))
    (when ref
      (last-elt (str:split "/" ref)))))


(defun find-project-related-to (payload)
  "Searches a projects metadata among all projects, known to Ultralisp.
   Returns a `project' object or nil."
  ;; TODO: probably this code should be fixed to
  ;; use branch name from the project's source definition
  (let ((current-branch (get-branch-from payload))
        (main-branch (get-main-branch-from payload))
        (project-name (fmt "~A/~A"
                           (get-user-or-org-from payload)
                           (get-project-name-from payload))))
    (cond
      ((and current-branch
            (string-equal current-branch
                          main-branch))

       (get-project2 project-name))
      
      (t (if current-branch
             (log:warn "Current branch does not match to main"
                       project-name
                       current-branch
                       main-branch)
             (log:warn "Unable to figure out current branch from the payload"))
         (values)))))


(defun update (project &key (upload nil))
  (let ((projects-dir "build/sources/")
        (dist-dir "build/dist/"))
    (ultralisp/builder:build
     :projects (if (probe-file projects-dir)
                   ;; If directory with projects already exists
                   ;; then we will update just a project
                   ;; mentioned in the payload
                   project
                   ;; otherwize, will download all known projects
                   :all)
     :projects-dir projects-dir
     :dist-dir dist-dir)

    ;; Now we'll upload a dist to the server
    (when upload
      (error "This code was broken at some moment, upload requires two parameters. But everything seems work. Probably it is not executed.")
      ;; (ultralisp/uploader/base:upload dist-dir)
      )))


(defun update-all (&key (build t)
                     (upload nil))
  "This function is useful to call manually after some pull-request was merged to add new projects."
  ;; (ultralisp/webhook::update "projects/projects.txt")
  (let ((projects-dir "build/sources/")
        (dist-dir "build/dist/"))
    (when build
      (ultralisp/builder:build
       :projects-dir projects-dir
       :dist-dir dist-dir))
    
    (when upload
      (error "This code was broken at some moment, upload requires two parameters. But everything seems work. Probably it is not executed.")
      ;; (ultralisp/uploader/base:upload dist-dir)
      )))


(declaim (ftype (function (list) (values boolean &optional))
                process-payload))

(defun process-payload (payload)
  (with-log-unhandled ()
    (with-connection ()
      (log:debug "Processing payload" payload)

      (push payload *payloads*)
      (setf *payloads*
            (subseq *payloads*
                    0
                    (min 40
                         (length *payloads*))))
     
      (let* ((project (find-project-related-to payload)))
        (cond
          (project
           (make-checks project :via-webhook)
           (values t))
          (t
           (values nil)))))))


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
           (make-thread 'process-payloads-from-the-queue
                        :name "github payloads processor")))
    (t (log:debug "Thread for payload processing is already running"))))


(defmethod process-webhook-route ((app app))
  (let* ((body (reblocks/request:get-parameters)))
    (log:debug "New payload received" body)

    ;; This is API, we don't want to keep any sessions here
    (reblocks/session:expire)

    ;; Github may send us a json payload or x-ww-form-urlencoded
    ;; in first case, body contains already parsed json. In second -
    ;; only alist with single "payload" item with a value of json
    ;; encoded to the string and we need to handle this case specially:
    (let ((payload-as-string (assoc-value body "payload" :test 'string-equal)))
      (when payload-as-string
        (setf body
              (jonathan:parse payload-as-string
                              :as :alist))))

    (log:debug "Sending to the queue")
    (chanl:send *queue* body :blockp nil)
    (log:debug "Payload is in the queue now")

    (log:debug "Ensuring thread is running")
    (ensure-processor-thread-is-running)

    (log:debug "DONE")

    (values "OK")))


(defun get-webhook-url ()
  "Returns a full path to a webhook, which can be used in GitHub's settings."
  (make-uri *github-webhook-path*))
