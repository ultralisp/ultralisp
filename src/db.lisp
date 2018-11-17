(defpackage #:ultralisp/db
  (:use #:cl)
  (:import-from #:cl-dbi)
  (:import-from #:mito
                #:connect-toplevel)
  (:export
   #:with-transaction))
(in-package ultralisp/db)


(defun connect ()
  (connect-toplevel :postgres
                    :host "postgres"
                    :database-name "ultralisp"
                    :username "ultralisp"
                    :password "ultralisp"))


(defmacro with-transaction (&body body)
  `(cl-dbi:with-transaction mito:*connection*
     ,@body))
