(defpackage #:ultralisp/uploader/base
  (:use #:cl)
  (:export
   #:make-uploader
   #:*uploader-type*
   #:upload))
(in-package ultralisp/uploader/base)


(defparameter *uploader-type* :fake
  "Set this to :s3 to upload data to Amazon S3 servers.")


(defgeneric make-uploader (type)
  (:documentation "Returns a function to upload a quicklisp distribution.

                   A function should accept one positional argument - a pathname
                   to the distribution on the local disk."))


(defun upload (dir)
  (funcall (make-uploader *uploader-type*)
           dir))
