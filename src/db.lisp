(defpackage #:ultralisp/db
  (:use #:cl)
  (:import-from #:mito
                #:connect-toplevel))
(in-package ultralisp/db)


(defun connect ()
  (connect-toplevel :postgres
                    :host "postgres"
                    :database-name "ultralisp"
                    :username "ultralisp"
                    :password "ultralisp"))
