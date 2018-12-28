(defpackage #:ultralisp/downloader/base
  (:use #:cl)
  (:import-from #:cl-fad)
  (:import-from #:ultralisp/models/check
                #:make-version-from
                #:get-pending-checks
                #:project-has-changes-p
                #:check
                #:get-project)
  (:import-from #:ultralisp/models/project
                #:get-source)
  (:import-from #:ultralisp/db
                #:with-lock
                #:with-transaction)
  (:export
   #:make-downloaded-project
   #:find-project-by-path
   #:perform-project-check
   #:perform-check
   #:download
   #:make-downloader
   #:perform-pending-checks-and-trigger-version-build
   #:downloaded-project
   #:downloaded-project-params
   #:downloaded-project-project
   #:downloaded-project-path))
(in-package ultralisp/downloader/base)


(defstruct (downloaded-project (:constructor %make-downloaded-project))
  (path "" :type pathname :read-only t)
  (project nil :read-only t)
  (params nil :read-only t))


(defun make-downloaded-project (path project params)
  (%make-downloaded-project :path (truename path)
                            :project project
                            :params params))


(defgeneric perform-project-check (source project check)
  (:documentation "Performs actual check if project has changed in the remote source.

If it changed, then method should set `project-has-changes-p' of the `check'
to True and set it's description to a text describing changes. These changes will
be grouped into the changelog item for a new Ultralisp release.

Should return a check object."))


(defgeneric download (obj dir &key latest)
  (:documentation "Downloads something into the given directory.
                   Usually, object will be a metadata holding description of a project.

                   Should return a list of `downloaded-project' objects or a single object
                   of that type."))


(defgeneric make-downloader (source)
  (:documentation "This generic should return a funcallable object which accepts
                   one required argument - a directory where the sources should
                   be downloaded to and any number of keyword arguments.

                   Keyword arguments are taken from the project's `params' attribute,
                   which is a plist. These arguments can contain such data like a git hash
                   specifying a version of the repository to download.

                   This funcallable should fetch project's sources and put them
                   into the source directory.

                   It should return a directory where fetches sources were stored as
                   a first value, and can return a plist as a second value. This
                   plist will be used to update project's `params' plist.

                   Input argument `source' is a keyword from `source' slot of the metadata object."))


(defun perform-check (check)
  "Performs a project check. After this call, parameters
of the project linked to the check object can be changed
as well as check's attributes such like `project-has-changes'
and `description'."
  (check-type check check)
  
  (let* ((project (get-project check))
         (source (get-source project)))
    (perform-project-check source
                           project
                           check)))


(defun perform-pending-checks-and-trigger-version-build ()
  "Performs all pending checks and creates a new Ultralisp version
   if some projects were updated."
  (with-transaction
    (with-lock ("performing-pending-checks-or-version-build"
                ;; We don't need to signal because this function
                ;; will be called again by "cron" after some
                ;; period of time.
                :signal-on-failure nil)
      (let* ((checks (get-pending-checks))
             (checks (mapcar 'perform-check checks))
             (checks (remove-if-not 'project-has-changes-p checks)))

        (cond (checks
               (log:info "Some changes were found. Creating a new version.")
               (let ((version (make-version-from checks)))
                 (log:info "Version was created" version)
                 version))
              (t
               (log:info "There wasn't any changes, not building a new version")))))))


(defun find-project-by-path (downloaded-projects path)
  (let ((obj (find (truename path)
                   downloaded-projects
                   :key (lambda (item)
                          (truename (downloaded-project-path item)))
                   :test #'equal)))
    (when obj
      (downloaded-project-project obj))))


(defun remove-disabled-projects (projects-dir downloaded-projects)
  "This function walk over all subdirectories of the `projects-dir'
   and removes all which are not among downloaded-projects.

   This function is used during cleanup stage before building a quicklisp
   distribution."
  (let ((downloaded-paths (mapcar #'downloaded-project-path
                                  downloaded-projects))
        (projects-dir (truename projects-dir)))
    (loop for directory in (cl-fad:list-directory projects-dir)
          unless (member directory downloaded-paths
                         :test #'equal)
            do (delete-directory-tree directory
                                      :validate t))))
