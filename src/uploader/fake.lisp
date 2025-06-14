(defpackage #:ultralisp/uploader/fake
  (:use #:cl)
  (:import-from #:log4cl)
  (:import-from #:ultralisp/uploader/base
                #:make-uploader
                #:make-files-inclusion-checker)
  (:import-from #:ultralisp/variables
                #:get-dist-dir
                #:get-clpi-dist-dir)
  (:import-from #:log)
  (:import-from #:ultralisp/utils
                #:walk-dir
                #:relative-path))
(in-package #:ultralisp/uploader/fake)


(defmethod make-uploader ((type (eql :fake)) repo-type)
  (lambda (dir-or-file destination-path &key only-files)
    (let* ((destination-path (relative-path
                              (uiop:ensure-directory-pathname
                               (ecase repo-type
                                 (:quicklisp (get-dist-dir))
                                 (:clpi (get-clpi-dist-dir))))
                              destination-path))
           (need-to-upload-p
             (make-files-inclusion-checker only-files)))
      (walk-dir (dir-or-file absolute relative)
        (when (funcall need-to-upload-p relative)
          (let ((destination (merge-pathnames relative destination-path)))
            (log:info "Copying" absolute "to" destination)
            (ensure-directories-exist destination)
            (uiop:copy-file absolute destination)))))))
