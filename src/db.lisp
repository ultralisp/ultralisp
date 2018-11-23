(defpackage #:ultralisp/db
  (:use #:cl)
  (:import-from #:cl-dbi
                #:connect-cached)
  (:import-from #:mito
                #:connect-toplevel)
  (:export
   #:with-transaction
   #:with-connection
   #:connect-toplevel))
(in-package ultralisp/db)


(defun connect ()
  (connect-cached :postgres
                          :host (or (uiop:getenv "POSTGRES_HOST")
                                    "localhost")
                          :database-name (or (uiop:getenv "POSTGRES_DBNAME")
                                             "ultralisp")
                          :username (or (uiop:getenv "POSTGRES_USER")
                                        "ultralisp")
                          :password (or (uiop:getenv "POSTGRES_PASS")
                                        "ultralisp")))


(defun connect-toplevel ()
  (setf mito:*connection* (connect)))


(defmacro with-transaction (&body body)
  `(cl-dbi:with-transaction mito:*connection*
     ,@body))


(defmacro with-connection (&body body)
  "Establish a new connection and start transaction"
  `(let ((mito:*connection* (connect)))
     (with-transaction
       ,@body)))
