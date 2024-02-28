(uiop:define-package #:ultralisp/models/source
  (:use #:cl)
  (:import-from #:jonathan)
  (:import-from #:dexador)
  (:import-from #:log)
  (:import-from #:cl-ppcre)
  (:import-from #:ultralisp/protocols/external-url
                #:external-url)
  (:import-from #:ultralisp/models/versioned
                #:deleted-p
                #:latest-p
                #:object-version)
  (:import-from #:ultralisp/models/utils
                #:systems-info-to-json
                #:release-info-to-json
                #:systems-info-from-json
                #:release-info-from-json)
  (:import-from #:ultralisp/stats
                #:increment-counter)
  (:import-from #:alexandria
                #:make-keyword)
  (:import-from #:mito
                #:object-id)
  (:import-from #:ultralisp/rpc/command
                #:defcommand)
  (:import-from #:ultralisp/utils
                #:remove-last-slash
                #:update-plist
                #:get-traceback)
  (:import-from #:ultralisp/db
                #:with-connection
                #:with-transaction)
  (:import-from #:quickdist
                #:get-archive-path
                #:get-system-files)
  (:import-from #:ultralisp/downloader/base
                #:downloaded-project-path
                #:remove-vcs-files
                #:download)
  (:import-from #:ultralisp/uploader/base
                #:upload)
  (:import-from #:ultralisp/protocols/enabled
                #:enabled-p)
  (:import-from #:log4cl-extras/context
                #:with-fields)
  (:import-from #:ultralisp/utils/db
                #:deflate-json
                #:inflate-json
                #:deflate-keyword
                #:inflate-keyword)
  (:import-from #:rutils
                #:awhen)
  (:import-from #:str)
  (:import-from #:ultralisp/utils/github
                #:get-github-branches)
  (:import-from #:ultralisp/variables
                #:get-base-url)
  (:import-from #:serapeum
                #:soft-list-of
                #:fmt)
  (:import-from #:ultralisp/models/system-info
                #:system-info)
  (:export
   #:source-systems-info
   #:source-release-info
   #:source
   #:source-project-id
   #:project-version
   #:source-type
   #:source-params
   #:deleted-p
   #:copy-source
   #:get-source
   #:bound-source
   #:disable-reason
   #:include-reason
   #:get-all-sources
   #:get-github-sources
   #:dist
   #:find-source-version
   #:create-source
   #:params-from-github
   #:get-current-branch
   #:enable-this-source-version
   #:params-to-string
   #:ignore-dirs
   #:get-latest-version-by-id
   #:get-latest-source))
(in-package #:ultralisp/models/source)


(defclass source (ultralisp/models/versioned:versioned)
  ;; TODO: тут нужен какой-то id, который
  ;; позволит различать разные source
  ;; Надо посмотреть, как можно сделать
  ;; автоинкремент, но без unique
  (;; (id :col-type :bigint
   ;;     :initarg :id
   ;;     :reader object-id)
   (project-id :col-type :bigint
               :initarg :project-id
               :reader source-project-id)
   (project-version :col-type :bigint
                    :initarg :project-version
                    :reader project-version)
   ;; (version :col-type :bigint
   ;;          :initarg :version
   ;;          :reader object-version
   ;;          :initform 0)
   ;; (latest :col-type :boolean
   ;;         :initarg :latest
   ;;         :initform t
   ;;         :reader latest-p)
   ;; (deleted :col-type :boolean
   ;;          :initarg :deleted
   ;;          :initform nil
   ;;          :reader deleted-p)
   (type :col-type (:text)
         :initarg :type
         :reader source-type
         :inflate #'inflate-keyword
         :deflate #'deflate-keyword)
   (params :col-type (:jsonb)
           :initarg :params
           :initform nil
           :reader source-params
           :deflate #'deflate-json
           :inflate #'inflate-json)
   (systems-info :col-type (or :jsonb :null)
                 :type (soft-list-of system-info)
                 :documentation "Contains a list of lists describing systems same way as quickdist returns."
                 :initform nil
                 :reader source-systems-info
                 :deflate #'systems-info-to-json
                 :inflate #'systems-info-from-json)
   (release-info :col-type (or :jsonb :null)
                 :type (or null quickdist:release-info)
                 :documentation ""
                 :initform nil
                 :reader source-release-info
                 :deflate #'release-info-to-json
                 :inflate #'release-info-from-json))
  
  (:primary-key ultralisp/models/versioned:id
                ultralisp/models/versioned:version)
  (:metaclass mito:dao-table-class))


(defclass bound-source ()
  ((source :initarg :source
           :reader source)
   (dist :initarg :dist
         :initform nil
         :documentation "
             This slot will point to a distribution or can be nil in case
             if the source version is not bound to the dist. Usually it
             can be as result of prev-version call if previous source version was
             not bound to a dist of original version.
         "
         :reader dist)
   (enabled :initarg :enabled
            :initform nil
            :reader enabled-p)
   (disable-reason :initarg :disable-reason
                   :initform nil
                   :reader disable-reason)
   (include-reason :initarg :include-reason
                   :initform nil
                   :reader include-reason))
  (:documentation "
             Represents the source bound to some distribution.

             Objects of this type are returned by ultralisp/models/source-dist:dist->sources function.

             Slots `dist', `enabled', `disable-reason' and `include-reason' can be nil
             if the bound-source is result of `prev-version' call.
"))


;; We'll define a few readers to make bound-dist work the same like usual dist does:

(defmethod object-id ((obj bound-source))
  (object-id (source obj)))

(defmethod object-version ((obj bound-source))
  (object-version (source obj)))

(defmethod source-project-id ((obj bound-source))
  (source-project-id (source obj)))

(defmethod project-version ((obj bound-source))
  (project-version (source obj)))

(defmethod source-type ((obj bound-source))
  (source-type (source obj)))

(defmethod source-params ((obj bound-source))
  (source-params (source obj)))

(defmethod source-systems-info ((obj bound-source))
  (source-systems-info (source obj)))

(defmethod source-release-info ((obj bound-source))
  (source-release-info (source obj)))

(defmethod deleted-p ((obj bound-source))
  (deleted-p (source obj)))


(defun params-to-string (source &key (last-seen t))
  (let ((type (source-type source)))
    (case type
      (:github
       (let ((params (source-params source)))
         (fmt "~A://~A/~A@~A"
              (string-downcase type)
              (getf params :user-or-org)
              (getf params :project)
              (if last-seen
                  (getf params :last-seen-commit)
                  (or (getf params :branch)
                      "master")))))
      (:git
       (let ((params (source-params source)))
         (fmt "~A@~A"
              (getf params :url)
              (if last-seen
                  (getf params :last-seen-commit)
                  (or (getf params :branch)
                      "master")))))
      (t
       (fmt "~A://unsupported-source-type"
            (string-downcase type))))))


(defmethod print-object ((obj source) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream
            "~A (v~A)~A~A"
            (params-to-string obj)
            (object-version obj)
            (if (deleted-p obj)
                " deleted"
                "")
            (if (source-release-info obj)
                " has-release-info"
                ""))))


(defmethod print-object ((obj bound-source) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream
            "~A (v~A)~A~A"
            (params-to-string obj)
            (object-version obj)
            (if (deleted-p obj)
                " deleted"
                "")
            (if (source-release-info obj)
                " has-release-info"
                ""))))


(defun %project-sources (project-id project-version)
  (mito:retrieve-dao 'source
                     :project-id project-id
                     :project-version project-version
                     :latest "true"
                     :deleted "false"))


(defmethod external-url ((obj source))
  (let ((type (source-type obj)))
    (case type
      (:git
       (let* ((params (source-params obj)))
         (getf params :url)))
      (:github
       (let* ((params (source-params obj))
              (user (getf params :user-or-org))
              (project (getf params :project)))
         (when (and user project)
           (format nil "https://github.com/~A/~A"
                   user
                   project)))))))


(defun params-from-github (url)
  "It should extract \"cbaggers/livesupport\" from urls like:

   http://github.com/cbaggers/livesupport
   https://github.com/cbaggers/livesupport
   https://github.com/cbaggers/livesupport/
   https://github.com/cbaggers/livesupport.git
   https://github.com/cbaggers/livesupport/issues

   Returns username as first value and project name as the second.
"
  
  (cl-ppcre:register-groups-bind (user-name project-name)
      ("https?://github.com/(.*?)/(.*?)($|/|\\.git)" url)
    (values user-name
            project-name)))


(defun create-source (project type)
  "Creates empty source of given type."
  (check-type type keyword)
  (mito:create-dao 'source
                   :project-id (object-id project)
                   :project-version (object-version project)
                   :type type))


(defun copy-source (source &key (params nil params-p)
                                (systems-info nil systems-info-p)
                                (release-info nil release-info-p)
                                (deleted nil deleted-p))
  (mito:create-dao 'source
                   :id (object-id source)
                   :version (1+ (object-version source))
                   :project-id (source-project-id source)
                   :project-version (project-version source)
                   :type (source-type source)
                   :params (if params-p
                               params
                               (source-params source))
                   :systems-info (if systems-info-p
                                     systems-info
                                     (source-systems-info source))
                   :release-info (if release-info-p
                                     release-info
                                     (source-release-info source))
                   :deleted (if deleted-p
                                deleted
                                (deleted-p source))))


(defun get-source (id version)
  (first
   (mito:retrieve-dao 'source
                      :id id
                      :version version)))


(defun get-latest-source (id)
  (first
   (mito:retrieve-dao 'source
                      :id id
                      :latest 1)))


(defun make-release (source systems)
  "Downloads the project into the temporary directory, builts a tarball and uploads it to the storage."
  (let (release-info)
    (ultralisp/utils:with-tmp-directory (tmp-dir)
      (unwind-protect
           (let* ((system-files (get-system-files systems))
                  (project (uiop:symbol-call :ultralisp/models/project :source->project source))
                  (project-name (uiop:symbol-call :ultralisp/models/project :project-name project))
                  (full-project-name (str:replace-all "/" "-"
                                                      project-name))
                  (target-path (uiop:merge-pathnames* full-project-name tmp-dir))
                  (downloaded (progn
                                (log:info "Downloading project ~A to ~A"
                                          project-name
                                          target-path)
                                (download source target-path :latest t)))
                  (archive-dir (uiop:ensure-pathname (merge-pathnames ".archive/" tmp-dir)
                                                     :ensure-directories-exist t))
                  (_ (progn
                       (log:info "Removing VCS files of project ~A from ~A"
                                 project-name
                                 target-path)
                       (remove-vcs-files downloaded)))
                  (source-id (mito:object-id source))
                  (archive-url (format nil "~A/archive/~A"
                                       (remove-last-slash (get-base-url))
                                       source-id))
                  (archive-destination (format nil "/archive/~A/"
                                               source-id)))
             (declare (ignorable _))
             
             (log:info "Archiving project ~A from ~A to ~A"
                       project-name
                       target-path
                       archive-dir)
             (setf release-info
                   (quickdist:make-archive (downloaded-project-path downloaded)
                                           full-project-name
                                           system-files
                                           archive-dir
                                           archive-url))
             (let ((archive-path (get-archive-path release-info)))
               (log:info "Uploading ~A from ~A to ~A"
                         project-name
                         archive-path
                         archive-destination)
               (upload archive-path
                       :quicklisp
                       archive-destination)))
        (uiop:delete-directory-tree tmp-dir
                                    :validate t)))
    release-info))


(defcommand create-new-source-version (source systems params &key (enable t))
  "Creates a new source version and attached it to a new or existing pending version.
   Params are replaced but extendded"
  (log:info "Creating new source version" source systems params)

  (with-transaction
    (increment-counter :sources-updated)

    (let* ((release-info (make-release source systems))
           (new-source
             (copy-source source
                          :systems-info systems
                          :release-info release-info
                          :params (update-plist (source-params source)
                                                params))))
      (log:debug "Creating a new dist version")
      (uiop:symbol-call :ultralisp/models/dist-source
                        :create-pending-dists-for-new-source-version
                        source new-source :enable enable)
      (log:debug "New dist was created"))))


(defcommand enable-this-source-version (source)
  "Enables existing source version.lisp

   This command whil be called by worker when it didn't discover any changes in the source,
   but error dissappeared this time.

   It will help to recheck sources by cron in case of some temporary issues like networking
   problems etc."
  
  (with-transaction
    (increment-counter :sources-updated)

    (with-fields (:source-id (object-id source)
                  :source-version (object-version source))
      
      (log:debug "Enabling the source")
      (uiop:symbol-call :ultralisp/models/dist-source :create-pending-dists-for-new-source-version
                        source source
                        :enable t))))


(defun get-all-sources ()
  "Returns a list of all sources which aren't deleted.

   Only latest version is returned.

   We need this function to iterate over all sources and
   schedule checks.

   NOTE: Probably this should be rewritten to some sort of paginated
   iterator to not fetch all data from the database at once."
  (mito:retrieve-dao 'source
                     :deleted "false"
                     :latest "true"))


(defun get-github-sources (usernames)
  "Receives a list of usernames or orgnames and returns a list
   of GitHub sources, known to Ultralisp."
  (when usernames
    (mito:select-dao 'source
      (sxql:where (:and
                   (:= 'latest
                       "true")
                   (:= 'deleted
                       "false")
                   (:= 'type
                       "github")
                   (:in (:raw "params->>'USER-OR-ORG'")
                        usernames))))))


(defun find-source-version (id version)
  ;; TODO: replace with find-source
  (mito:find-dao 'source
                 :id id
                 :version version))


(defun get-current-branch (source)
  (getf (source-params source)
        :branch))


(defun ignore-dirs (source)
  (getf (source-params source)
        :ignore-dirs))


(defun set-default-branch (source)
  (multiple-value-bind (branches default-branch)
      (get-github-branches
       (external-url source))
    (declare (ignore branches))
    (when default-branch
      (let ((new-params (update-plist (source-params source)
                                      (list :branch default-branch))))
        (setf (slot-value source 'params)
              new-params)
        (mito:save-dao source)))))


(defun get-sources-without-branches ()
  (remove-if #'get-current-branch
             (get-all-sources)))


(defun fill-default-branches ()
  (let ((retry (dexador:retry-request 1000 :interval 10))
        (sources (get-sources-without-branches)))
    (format t "We have ~A sources to process...~%"
            (length sources))
    
    (flet ((log-n-retry (c)
             (log:error "Retrying ~A~%" c)
             (funcall retry c)))
      (handler-bind ((dexador:http-request-forbidden
                       #'log-n-retry))
        (loop for source in sources
              do (set-default-branch source))))

    (format t "Done.~%")))


(defun get-latest-version-by-id (source-id)
  (assert source-id)
  (mito:find-dao 'source
                 :id source-id
                 :latest "true"))
