(defpackage #:ultralisp/downloader
  (:use #:cl)
  
  (:import-from #:legit)
  (:import-from #:log4cl)
  (:import-from #:cl-fad)
  (:import-from #:ultralisp/metadata
                #:metadata
                #:read-metadata)
  (:import-from #:uiop
                #:ensure-absolute-pathname
                #:ensure-pathname
                #:ensure-directory-pathname)
  (:import-from #:ultralisp/models/project
                #:get-params
                #:get-source
                #:project
                #:get-all-projects)
  (:export
   #:download))
(in-package ultralisp/downloader)


(defun ensure-existing-file (path)
  (ensure-pathname path
                   :want-file t
                   :want-existing t))


(defun ensure-absolute-dirname (path)
  (ensure-directory-pathname
   (ensure-absolute-pathname
    path
    (probe-file "."))))


(defun directory-mtime (path)
  (if (not (fad:directory-pathname-p path))
      (file-write-date path)
      (apply #'max 0 (mapcar #'directory-mtime (fad:list-directory path)))))


(defun git-mtime (path)
  (when (probe-file (merge-pathnames ".git/" path))
    (let ((repo (legit:init path)))
      (legit:current-age repo))))


(defun quickdist::effective-mtime (path)
  (setf path
        (ensure-absolute-dirname path))
  
  (or (git-mtime path)
      (directory-mtime path)))


(defgeneric download (obj dir)
  (:documentation "Downloads something into the given directory.
                   Usually, object will be a metadata holding description of a project."))


(defgeneric make-downloader (source)
  (:documentation "This generic should return a funcallable object which accepts
                   two parameter: urn and directory. This fancallable should fetch
                   project's sources and put them into the source directory.

                   Input argument `source' is a keyword from `source' slot of the metadata object."))


(defmethod download ((projects (eql :all)) projects-dir)
  "Downloads sources defined in the database into the `projects-dir'."
  (declare (ignorable projects))
  (ensure-directories-exist projects-dir)
  
  (loop for project in (get-all-projects)
        do (download project projects-dir)))


(defmethod download ((project project) dir)
  (log:info "Downloading" project)
  (apply (make-downloader (get-source project))
         dir
         (get-params project)))


(defun git-clone-or-update (url dir)
  (let* ((absolute-dir (ensure-absolute-dirname dir))
         (repo (legit:init absolute-dir
                           :remote url
                           :if-does-not-exist :clone)))
    ;; Here we are suppressing output from the git binary
    (with-output-to-string (*standard-output*)
      (legit:pull repo))
    ;; return nothing
    (values)))


(defmethod make-downloader ((source (eql :github)))
  (lambda (base-dir &key user-or-org project &allow-other-keys)
    (let ((url (format nil "https://github.com/~A/~A.git"
                       user-or-org
                       project))
          (dir (ensure-directory-pathname
                (merge-pathnames (format nil "~A-~A" user-or-org
                                         project)
                                 (ensure-directory-pathname base-dir)))))
      (git-clone-or-update url dir))))


(defun update-metadata-repository (directory)
  (git-clone-or-update "https://github.com/ultralisp/ultralisp-projects.git"
                       directory))
