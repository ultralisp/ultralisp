(defpackage #:ultralisp/uploader/s3
  (:use #:cl)
  (:import-from #:log4cl)
  (:import-from #:cl-fad
                #:walk-directory)
  (:import-from #:zs3
                #:put-object)
  
  (:import-from #:ultralisp/utils
                #:walk-dir
                #:path-to-string)
  (:import-from #:ultralisp/uploader/base
                #:make-uploader)
  (:import-from #:ultralisp/variables
                #:get-aws-access-key-id
                #:get-aws-secret-access-key
                #:get-s3-bucket)
  (:export
   #:prepare-for-debug))
(in-package ultralisp/uploader/s3)


(defvar *bucket* nil
  "Set this to a bucket name for debugging the uploader.")


(defclass s3-credentials ()
  ((access-key :initarg :access-key
               :reader zs3:access-key)
   (secret-key :initarg :secret-key
               :reader zs3:secret-key)))


(defun make-credentials (&key (access-key (get-aws-access-key-id))
                           (secret-key (get-aws-secret-access-key)))
  (unless access-key
    (error "Please, define AWS_ACCESS_KEY_ID environment variable."))
  (unless secret-key
    (error "Please, define AWS_SECRET_ACCESS_KEY environment variable."))

  (make-instance 's3-credentials
                 :access-key access-key
                 :secret-key secret-key))


(defun prepare-for-debug (bucket access-key secret-key)
  "Use this in the REPL for debugging the uploader."
  (setf *bucket* bucket)
  (setf zs3:*credentials*
        (make-credentials :access-key access-key
                          :secret-key secret-key)))


(defmethod make-uploader ((type (eql :s3)))
  (lambda (dir-or-file destination-path)
    (let* ((zs3:*credentials* (or zs3:*credentials*
                                  (make-credentials)))
           (bucket (or *bucket*
                       (get-s3-bucket))))
     
      (walk-dir (dir-or-file absolute relative)
        (let* ((key (string-left-trim '(#\/)
                                      (concatenate 'string
                                                   destination-path
                                                   relative))))
          (log:info "Uploading" absolute "to" key)
          (put-object absolute
                      bucket
                      key))))))


;; (defmethod make-uploader ((type (eql :s3)))
;;   (lambda (dir &key to)
;;     (let* ((absolute-dir (probe-file dir))
;;            (dir-as-string (path-to-string absolute-dir))
;;            (base-dir-length (length dir-as-string))
;;            (zs3:*credentials* (or zs3:*credentials*
;;                                   (make-credentials)))
;;            (bucket (or *bucket*
;;                        (get-s3-bucket))))
     
;;       (walk-directory absolute-dir
;;                       (lambda (item)
;;                         (let* ((path (path-to-string item))
;;                                (relative-path (subseq path base-dir-length))
;;                                (key (if to
;;                                         (concatenate 'string
;;                                                      (string-right-trim "/" to)
;;                                                      "/"
;;                                                      relative-path)
;;                                         relative-path)))
;;                           (log:info "Uploading" relative-path)
;;                           (put-object item
;;                                       bucket
;;                                       key)))))))
