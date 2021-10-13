(defpackage #:ultralisp/uploader/fake
  (:use #:cl)
  (:import-from #:log4cl)
  (:import-from #:ultralisp/uploader/base
                #:make-uploader)
  (:import-from #:ultralisp/variables
                #:get-dist-dir
                #:get-clpi-dist-dir)
  (:import-from #:metatilities
                #:relative-pathname))
(in-package ultralisp/uploader/fake)


(defmethod make-uploader ((type (eql :fake)) repo-type)
  (lambda (dir-or-file destination-path &key only-files)
    (let* ((destination-path (relative-pathname (ecase repo-type
                                                  (:quicklisp (get-dist-dir))
                                                  (:clpi (get-clpi-dist-dir)))
                                                destination-path)))
      (ultralisp/utils:walk-dir (dir-or-file absolute relative)
        (when (or (null only-files)
                  (member relative only-files :test #'string-equal))
          (let ((destination (merge-pathnames relative destination-path)))
            (log:info "Copying" absolute "to" destination)
            (ensure-directories-exist destination)
            (uiop:copy-file absolute destination)))))))
