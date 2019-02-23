(defpackage #:ultralisp/uploader/base
  (:use #:cl)
  (:import-from #:ultralisp/utils
                #:starts-with-slash-p)
  (:export
   #:make-uploader
   #:*uploader-type*
   #:upload))
(in-package ultralisp/uploader/base)


(defvar *uploader-type* :fake
  "Set this to :s3 to upload data to Amazon S3 servers.")


(defgeneric make-uploader (type)
  (:documentation "Returns a function to upload a quicklisp distribution.

                   A function should accept one positional argument - a pathname
                   to the distribution on the local disk."))


(defun upload (dir-or-file destination)
  "Uploads given archive file to the storage (S3 or a static directory on local computer).

   Destination should be a string starting from /. If you'll specify just \"/\",
   and dir-or-file will be a \"foo.txt\", then this file will be accessable
   as http://dist.ultralisp.org/foo.txt or from other host, depending on the
   $BASE_URL environment variable."
  (check-type destination (satisfies starts-with-slash-p))
  (check-type dir-or-file (or pathname string))
  (funcall (make-uploader *uploader-type*)
           dir-or-file
           destination))
