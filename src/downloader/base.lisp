(defpackage #:ultralisp/downloader/base
  (:use #:cl)
  (:import-from #:cl-fad)
  (:import-from #:uiop
                #:delete-directory-tree)
  (:import-from #:ultralisp/db
                #:with-lock
                #:with-transaction)
  (:export
   #:make-downloaded-project
   #:find-project-by-path
   #:download
   #:make-downloader
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
