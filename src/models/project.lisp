(defpackage #:ultralisp/models/project
  (:use #:cl)
  (:import-from #:log4cl-json)
  (:import-from #:mito
                #:save-dao
                #:delete-dao
                #:select-dao
                #:create-dao)
  (:import-from #:jonathan
                #:to-json)
  (:import-from #:dexador)
  (:import-from #:ultralisp/metadata
                #:read-metadata)
  (:import-from #:function-cache
                #:defcached)
  (:import-from #:cl-arrows
                #:->)
  (:import-from #:cl-strings
                #:split)
  (:import-from #:alexandria
                #:make-keyword)
  (:import-from #:sxql
                #:order-by
                #:limit
                #:where)
  (:import-from #:mito-email-auth/weblocks
                #:get-current-user)
  (:import-from #:ultralisp/db
                #:with-transaction)
  (:import-from #:ultralisp/utils
                #:update-plist
                #:make-update-diff)
  (:import-from #:ultralisp/models/user
                #:get-all-users)
  (:import-from #:ultralisp/models/version
                #:get-number
                #:version)
  (:export
   #:update-and-enable-project
   #:is-enabled-p
   #:get-all-projects
   #:get-description
   #:get-url
   #:get-name
   #:project
   #:get-source
   #:get-params
   #:get-github-project
   #:add-or-turn-on-github-project
   #:turn-off-github-project
   #:get-last-seen-commit
   #:disable-project
   #:enable-project
   #:project-version
   #:get-version
   #:create-projects-snapshots-for
   #:get-projects
   #:get-project
   #:get-recent-projects))
(in-package ultralisp/models/project)


(defclass project ()
  ((source :col-type (:text)
           :initarg :source
           :reader get-source
           :inflate (lambda (text)
                      (make-keyword (string-upcase text)))
           :deflate #'symbol-name)
   (name :col-type (:text)
         :initarg :name
         :accessor get-name)
   (description :col-type :text
                :initarg :description
                :accessor get-description)
   (params :col-type (:jsonb)
           :initarg :params
           :accessor get-params
           :deflate #'jonathan:to-json
           :inflate (lambda (text)
                      (jonathan:parse
                       ;; Jonathan for some reason is unable to work with
                       ;; `base-string' type, returned by database
                       (coerce text 'simple-base-string))))
   (enabled :col-type :boolean
            :documentation "If True, then this project will be included into the next distribution version.

                            This attribute should be turned on only after some check was passed."
            :initform nil
            :accessor is-enabled-p))
  (:unique-keys name)
  (:metaclass mito:dao-table-class))


(defclass project-version ()
  ((version :col-type version
            :initarg :version
            :reader get-version)
   (source :col-type (:text)
           :initarg :source
           :reader get-source
           :inflate (lambda (text)
                      (make-keyword (string-upcase text)))
           :deflate #'symbol-name)
   (name :col-type (:text)
         :initarg :name
         :accessor get-name)
   (description :col-type :text
                :initarg :description
                :accessor get-description)
   (params :col-type (:jsonb)
           :initarg :params
           :accessor get-params
           :deflate #'jonathan:to-json
           :inflate (lambda (text)
                      (jonathan:parse
                       ;; Jonathan for some reason is unable to work with
                       ;; `base-string' type, returned by database
                       (coerce text 'simple-base-string)))))
  (:documentation "Items of this class store a snapshot of the project's state
                   at the moment when a particular version was built.")
  (:metaclass mito:dao-table-class))


(defmethod print-object ((project project) stream)
  (print-unreadable-object (project stream :type t)
    (format stream
            "~A name=~A enabled=~A"
            (get-source project)
            (get-name project)
            (is-enabled-p project))))


(defmethod print-object ((project project-version) stream)
  (print-unreadable-object (project stream :type t)
    (format stream
            "~A~@[ version=~A~] name=~A"
            (get-source project)
            (get-number
             (get-version project))
            (get-name project))))


(defun get-url (project)
  (let* ((params (get-params project))
         (user-name (getf params :user-or-org))
         (project-name (getf params :project)))
    (format nil "https://github.com/~A/~A"
            user-name
            project-name)))


(defun get-last-seen-commit (project)
  (check-type project project)
  (getf (get-params project)
        :last-seen-commit))


(defun (setf get-last-seen-commit) (value project)
  (check-type project project)
  (setf (getf (get-params project)
              :last-seen-commit)
        value))


(defcached %github-get-description (user-or-org project)
  (-> (format nil "https://api.github.com/repos/~A/~A"
              user-or-org
              project)
      (dex:get)
      (jonathan:parse)
      (getf :|description|)))


(defun make-github-project (user-or-org project)
  (let ((name (concatenate 'string
                           user-or-org
                           "/"
                           project))
        (description (or (ignore-errors
                          ;; We ignore errors here, because
                          ;; description is not very important
                          ;; and can be updated later,
                          ;; but we definitely want to log these
                          ;; errors, to not miss some system problems.
                          (log4cl-json:with-log-unhandled ()
                            (%github-get-description user-or-org
                                                     project)))
                         "")))
    (create-dao 'project
                :source :github
                :name name
                :description description
                :params (list :user-or-org user-or-org
                              :project project))))


(defun get-all-projects (&key only-enabled)
  (if only-enabled
      (select-dao 'project
        (where :enabled))
      (select-dao 'project)))


(defun get-recent-projects (&key (limit 10))
  "Returns a list of recently added projects to show them on a landing page."
  (select-dao 'project
    (where :enabled)
    (order-by (:desc :created-at))
    (limit limit)))


(defun get-github-project (user-or-org project)
  (first
   (select-dao 'project
     (where (:and
             (:= :source
                 "GITHUB")
             (:a> :params
                  (to-json
                   (list :user-or-org user-or-org
                         :project project)))))
     (limit 1))))


(defun get-github-projects (usernames)
  "Receives a list of usernames or orgnames and returns a list
   of GitHub projects, known to Ultralisp."
  (select-dao 'project
    (where (:and
            (:= :source
                "GITHUB")
            (:in (:raw "params->>'USER-OR-ORG'")
                 usernames)))))


(defun add-or-turn-on-github-project (name &key (moderator (get-current-user)))
  "Creates or updates a record in database adding current user to moderators list."
  (destructuring-bind (user-or-org project-name . rest)
      (cl-strings:split name "/")
    (declare (ignorable rest))
    
    (let ((project (get-github-project user-or-org project-name)))
      
      (unless project
        (log:info "Adding github project to the database" project)
        (setf project
              (make-github-project user-or-org project-name)))

      (uiop:symbol-call :ultralisp/models/moderator
                        :make-moderator
                        project
                        moderator)
      
      ;; Also, we need to trigger a check of this project
      ;; and to enabled it and to include into the next build
      ;; of a new Ultralisp version.
      (uiop:symbol-call :ultralisp/models/check
                        :make-added-project-check
                        project)
      
      project)))


(defun turn-off-github-project (name)
  "Creates or updates a record in database adding current user to moderators list."
  (destructuring-bind (user-or-org project . rest)
      (cl-strings:split name "/")
    (declare (ignorable rest))
    
    (let ((project (get-github-project user-or-org project)))
      (when project
        (disable-project project))
      project)))


(defun delete-project (project)
  (delete-dao project))


(defun convert-metadata (&optional (filename "projects/projects.txt"))
  "Loads old metadata from file into a database."
  (let ((metadata (read-metadata filename)))
    (with-transaction
      (loop for item in metadata
            for urn = (ultralisp/metadata:get-urn item)
            for splitted = (split urn "/")
            for user = (first splitted)
            for project = (second splitted)
            do (make-github-project user project)))))


(defun enable-project (project)
  "Enables project right now."
  (check-type project project)

  (log:info "Enabling project" project)
  
  (setf (is-enabled-p project)
        t)
  (save-dao project)
  (values project))


(defun update-and-enable-project (project data)
  (let* ((params (get-params project))
         (diff (make-update-diff params
                                 data)))
    (when diff
      (cond
        ((is-enabled-p project)
         (uiop:symbol-call :ultralisp/models/action
                           :make-project-updated-action project
                           :diff diff))
        (t
         (uiop:symbol-call :ultralisp/models/action
                           :make-project-added-action project
                           :diff diff)
         (setf (is-enabled-p project)
               t)))
      
      (setf (get-params project)
            (update-plist params
                          data))
      (save-dao project)))
  project)


(defun disable-project (project)
  "Disables project."
  (check-type project project)
  
  (when (is-enabled-p project)
    (log:info "Disabling project" project)

    ;; Also, we need to create a new action, related to this project
    (uiop:symbol-call :ultralisp/models/action
                      :make-project-removed-action
                      project)
    (setf (is-enabled-p project)
          nil)

    (save-dao project))
  
  (values project))


(defun create-project-snapshort (project version)
  (mito:create-dao 'project-version
                   :version version
                   :source (get-source project)
                   :name (get-name project)
                   :description (get-description project)
                   :params (get-params project)
                   :created-at (mito:object-created-at project)
                   :updated-at (mito:object-updated-at project)))


(defun create-projects-snapshots-for (version)
  (check-type version version)
  (loop for project in (get-all-projects :only-enabled t)
        do (create-project-snapshort project version)))


(defun get-projects (version)
  (check-type version version)
  (mito:select-dao 'project-version
    (where (:= :version version))
    (order-by :name)))


(defun get-project (project-version)
  "Returns original project."
  (check-type project-version project-version)
  (mito:find-dao 'project
                 :name (get-name project-version)))


(defun add-github-projects (names)
  "Adds a number of github project into the database.

   Here `names' is a list of string like \"40ants/cl-info\"."
  (let ((moderator (first (get-all-users))))
    (loop for name in names
          for project = (add-or-turn-on-github-project name
                                                       :moderator moderator)
          collect project)))
