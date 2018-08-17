(defpackage #:ultralisp/metadata
  (:use #:cl)
  (:import-from #:alexandria
                #:make-keyword)
  (:import-from #:ultralisp/downloader)
  (:import-from #:cl-fad)
  (:export
   #:read-metadata
   #:metadata
   #:get-source
   #:get-urn))
(in-package ultralisp/metadata)


(defclass metadata ()
  ((source :type keyword
           :initarg :source
           :documentation "Project source type: :github, :git, :bitbucket, :http, etc."
           :reader get-source)
   (urn :type string
        :initarg :urn
        :documentation "A string containing information to download sources. Format depends on a source slot's content."
        :reader get-urn)))


(defun make-metadata (source urn)
  (make-instance 'metadata
                 :source (make-keyword source)
                 :urn urn))


(defmethod print-object ((obj metadata) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A ~S"
            (get-source obj)
            (get-urn obj))))


(defun update-metadata-repository (path)
  (let ((directory (cl-fad:pathname-directory-pathname path))
        (url "https://github.com/ultralisp/ultralisp-projects.git"))
    (ultralisp/downloader::git-clone-or-update url directory)))


(defun read-metadata (path)
  (update-metadata-repository path)
  
  (with-open-file (input path)
    (loop for form = (read input nil nil)
          for metadata = (when form
                           (when (not (= (length form)
                                         2))
                             (error "Form ~A should countain two items, like \"(:github \"40ants/defmain\")\"."
                                    form))
                           (apply #'make-metadata form))
          while metadata
          collect metadata)))
