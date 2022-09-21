(defpackage #:ultralisp/metadata
  (:use #:cl)
  (:import-from #:alexandria
                #:make-keyword)
  (:import-from #:arrows
                #:->)
  (:import-from #:dexador)
  (:import-from #:jonathan)
  (:import-from #:function-cache
                #:defcached)
  (:export
   #:read-metadata
   #:metadata
   #:get-source
   #:get-urn
   #:get-description))
(in-package #:ultralisp/metadata)


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


(defun read-metadata (path)
  (when (probe-file path)
    (with-open-file (input path)
      (loop for form = (read input nil nil)
            for metadata = (when form
                             (when (not (= (length form)
                                           2))
                               (error "Form ~A should countain two items, like \"(:github \"40ants/defmain\")\"."
                                      form))
                             (apply #'make-metadata form))
            while metadata
            collect metadata))))


(defcached %github-get-description (project-urn)
  (-> (format nil "https://api.github.com/repos/~A"
              project-urn)
      (dex:get)
      (jonathan:parse)
      (getf :|description|)))


(defun get-description (metadata)
  (check-type metadata metadata)
  (%github-get-description (get-urn metadata)))
