(defpackage #:ultralisp/uploader/base
  (:use #:cl)
  (:import-from #:ultralisp/utils
                #:starts-with-slash-p
                #:ends-with-slash-p)
  (:export
   #:make-uploader
   #:*uploader-type*
   #:upload))
(in-package ultralisp/uploader/base)


(defvar *uploader-type* :fake
  "Set this to :s3 to upload data to Amazon S3 servers.")


(defgeneric make-uploader (type repo-type)
  (:documentation "Returns a function to upload a quicklisp distribution.

                   TYPE argument is an uploader type keyword like :fake or :s3.

                   REPO-TYPE is a typo of repository to be uploaded. It could
                   be a :quicklisp or :ultralisp. Depending on this parameter,
                   different storages or buckets could be used.

                   A function should accept one positional argument - a pathname
                   to the distribution on the local disk."))


(defun upload (dir-or-file repo-type destination &key only-files)
  "Uploads given archive file to the storage (S3 or a static directory on local computer).

   Destination should be a string starting from /. If you'll specify just \"/\",
   and dir-or-file will be a \"foo.txt\", then this file will be accessable
   as http://dist.ultralisp.org/foo.txt or from other host, depending on the
   $BASE_URL environment variable.

   If ONLY-FILES is given, then it should contain a list of strings
   with relative filenames."
  (check-type destination (and (satisfies starts-with-slash-p)
                               (satisfies ends-with-slash-p)))
  (check-type dir-or-file (or pathname string))
  (check-type repo-type keyword)

  (unless (member repo-type '(:quicklisp :clpi))
    (error "REPO-TYPE argument should be one of :QUICKLISP or :CLPI"))
  (funcall (make-uploader *uploader-type* repo-type)
           dir-or-file
           destination
           :only-files only-files))
