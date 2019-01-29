(defpackage #:ultralisp/uploader/fake
  (:use #:cl)
  (:import-from #:log4cl)
  (:import-from #:ultralisp/uploader/base
                #:make-uploader))
(in-package ultralisp/uploader/fake)


(defmethod make-uploader ((type (eql :fake)))
  (lambda (dir)
    (log:info "Doing nothing to upload files. They are stored in the" dir)))
