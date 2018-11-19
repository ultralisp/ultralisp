(defpackage #:ultralisp/uploader/s3
  (:use #:cl)
  (:import-from #:log4cl)
  (:import-from #:cl-fad
                #:walk-directory)
  (:import-from #:zs3
                #:put-object)
  
  (:import-from #:ultralisp/utils
                #:path-to-string)
  (:import-from #:ultralisp/uploader/base
                #:make-uploader))
(in-package ultralisp/uploader/s3)


(defclass s3-credentials ()
  ((access-key :initarg :access-key
               :reader zs3:access-key)
   (secret-key :initarg :secret-key
               :reader zs3:secret-key)))


(defun make-credentials ()
  (let ((access-key (uiop:getenv "AWS_ACCESS_KEY_ID"))
        (secret-key (uiop:getenv "AWS_SECRET_ACCESS_KEY")))
    (unless access-key
      (error "Please, define AWS_ACCESS_KEY_ID environment variable."))
    (unless secret-key
      (error "Please, define AWS_SECRET_ACCESS_KEY environment variable."))

    (make-instance 's3-credentials
                   :access-key access-key
                   :secret-key secret-key)))


(defmethod make-uploader ((type (eql :s3)))
    (lambda (dir &key to)
      (let* ((absolute-dir (probe-file dir))
             (dir-as-string (path-to-string absolute-dir))
             (base-dir-length (length dir-as-string))
             (zs3:*credentials* (make-credentials)))
     
        (walk-directory absolute-dir
                        (lambda (item)
                          (let* ((path (path-to-string item))
                                 (relative-path (subseq path base-dir-length))
                                 (key (if to
                                          (concatenate 'string
                                                       (string-right-trim "/" to)
                                                       "/"
                                                       relative-path)
                                          relative-path)))
                            (log:info "Uploading" relative-path)
                            (put-object item
                                        "dist.ultralisp.org"
                                        key)))))))
