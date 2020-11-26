(defpackage #:ultralisp/models/source
  (:use #:cl)
  (:import-from #:jonathan)
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
                #:update-plist)
  (:import-from #:ultralisp/db
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
  (:export
   #:source-systems-info
   #:source-release-info
   #:source
   #:source-project-id
   #:project-version
   #:source-type
   #:source-params
   #:deleted-p
   #:source-distributions
   #:copy-source
   #:get-source
   #:bound-source
   #:enabled-p
   #:disable-reason
   #:include-reason
   #:get-all-sources
   #:get-github-sources
   #:dist
   #:find-source-version))
(in-package ultralisp/models/source)


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
         :inflate (lambda (text)
                    (make-keyword (string-upcase text)))
         :deflate #'symbol-name)
   (params :col-type (:jsonb)
           :initarg :params
           :reader source-params
           :deflate #'jonathan:to-json
           :inflate (lambda (text)
                      (jonathan:parse
                       ;; Jonathan for some reason is unable to work with
                       ;; `base-string' type, returned by database
                       (coerce text 'simple-base-string))))
   (systems-info :col-type (or :jsonb :null)
                 :documentation "Contains a list of lists describing systems same way as quickdist returns."
                 :initform nil
                 :reader source-systems-info
                 :deflate #'systems-info-to-json
                 :inflate #'systems-info-from-json)
   (release-info :col-type (or :jsonb :null)
                 :documentation ""
                 :initform nil
                 :reader source-release-info
                 :deflate #'release-info-to-json
                 :inflate #'release-info-from-json))
  
  (:primary-key id version)
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


(defun params-to-string (source)
  (let ((type (source-type source)))
    (if (eql type :github)
        (let ((params (source-params source)))
          (format nil "~A://~A/~A@~A"
                  (string-downcase type)
                  (getf params :user-or-org)
                  (getf params :project)
                  (getf params :last-seen-commit)))
        (format nil "~A://unsupported-source-type"
                (string-downcase type)))))


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
                     :latest "true"))


(defmethod external-url ((obj source))
  (let ((type (source-type obj)))
    (when (eql type :github)
        (let ((params (source-params obj)))
          (format nil "https://github.com/~A/~A"
                  (getf params :user-or-org)
                  (getf params :project))))))



(defun copy-source (source &key (params nil params-p)
                                (systems-info nil systems-info-p)
                                (release-info nil release-info-p))
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
                                     (source-release-info source))))


(defun get-source (id version)
  (first
   (mito:retrieve-dao 'source
                      :id id
                      :version version)))


(defun make-release (source systems)
  "Downloads the project into the temporary directory, builts a tarball and uploads it to the storage."
  (let (release-info)
    (ultralisp/utils:with-tmp-directory (path)
      (unwind-protect
           (let* ((system-files (get-system-files systems))
                  (downloaded (download source path :latest t))
                  (archive-dir (uiop:ensure-pathname (merge-pathnames ".archive/" path)
                                                     :ensure-directories-exist t))
                  (_ (remove-vcs-files downloaded))
                  (project (uiop:symbol-call :ultralisp/models/project :source->project source))
                  (project-name (uiop:symbol-call :ultralisp/models/project :project-name project))
                  (source-id (mito:object-id source))
                  (archive-url (format nil "~A/archive/~A"
                                       (remove-last-slash (ultralisp/variables:get-base-url))
                                       source-id))
                  (archive-destination (format nil "/archive/~A/"
                                               source-id)))
             (declare (ignorable _))
             (setf release-info
                   (quickdist:make-archive (downloaded-project-path downloaded)
                                           (cl-strings:replace-all project-name
                                                                   "/"
                                                                   "-")
                                           system-files
                                           archive-dir
                                           archive-url))
             (let ((archive-path (get-archive-path release-info)))
               (upload archive-path
                       archive-destination)))
        (uiop:delete-directory-tree path
                                    :validate t)))
    release-info))


(defcommand create-new-source-version (source systems params)
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
      (uiop:symbol-call :ultralisp/models/dist-source :create-pending-dists-for-new-source-version
                        source new-source :enable t)
      (log:debug "New dist was created"))))


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
                       "GITHUB")
                   (:in (:raw "params->>'USER-OR-ORG'")
                        usernames))))))


(defun find-source-version (id version)
  (mito:find-dao 'source
                 :id id
                 :version version))
