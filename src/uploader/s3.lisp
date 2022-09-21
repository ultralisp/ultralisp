(defpackage #:ultralisp/uploader/s3
  (:use #:cl)
  (:import-from #:log4cl)
  (:import-from #:cl-fad
                #:walk-directory)
  (:import-from #:zs3
                #:put-object
                #:get-object)
  
  (:import-from #:ultralisp/utils
                #:walk-dir
                #:path-to-string)
  (:import-from #:ultralisp/uploader/base
                #:make-uploader
                #:make-files-inclusion-checker)
  (:import-from #:ultralisp/variables
                #:get-aws-access-key-id
                #:get-aws-secret-access-key
                #:get-s3-clpi-bucket
                #:get-s3-bucket)
  (:import-from #:secret-values
                #:secret-value
                #:reveal-value)
  (:import-from #:str)
  (:import-from #:log)
  (:export
   #:prepare-for-debug))
(in-package #:ultralisp/uploader/s3)


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
                 :secret-key (etypecase secret-key
                               (secret-value (reveal-value secret-key))
                               (string secret-key))))


(defun prepare-for-debug (bucket access-key secret-key)
  "Use this in the REPL for debugging the uploader."
  (setf *bucket* bucket)
  (setf zs3:*credentials*
        (make-credentials :access-key access-key
                          :secret-key secret-key)))


(defmethod make-uploader ((type (eql :s3)) repo-type)
  (lambda (dir-or-file destination-path &key only-files)
    (let* ((zs3:*credentials* (or zs3:*credentials*
                                  (make-credentials)))
           (bucket (or *bucket*
                       (ecase repo-type
                         (:quicklisp (get-s3-bucket))
                         (:clpi (get-s3-clpi-bucket)))))
           (need-to-upload-p
             (make-files-inclusion-checker only-files)))

      (walk-dir (dir-or-file absolute relative)
        (when (funcall need-to-upload-p relative)
          (let* ((key (string-left-trim '(#\/)
                                        (concatenate 'string
                                                     destination-path
                                                     relative))))
            (log:debug "Uploading" absolute "to" key)
            (put-object absolute
                        bucket
                        key
                        :content-type (when (str:ends-with-p ".html" key)
                                        "text/html"))))))))


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
