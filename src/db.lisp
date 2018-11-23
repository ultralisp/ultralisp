(defpackage #:ultralisp/db
  (:use #:cl)
  (:import-from #:cl-dbi
                #:connect-cached)
  (:import-from #:mito
                #:connect-toplevel)
  (:export
   #:with-transaction
   #:with-connection
   #:connect-toplevel
   #:with-lock
   #:unable-to-aquire-lock
   #:get-lock-name
   #:sql-fetch-all))
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


(defun make-hash-for-lock-name (name)
  (let* ((bytes (crypto:ascii-string-to-byte-array name))
         (hmac (crypto:make-hmac bytes :sha256))
         (digest (crypto:hmac-digest hmac))
         (num-bits-in-result 63)
         (result (ironclad:octets-to-integer digest
                                             :n-bits num-bits-in-result)))
    result))


(defun sql-fetch-all (sql &rest params)
  (cl-dbi:fetch-all (apply #'cl-dbi:execute (cl-dbi:prepare mito:*connection* sql) params)))


(define-condition unable-to-aquire-lock (error)
  ((lock-name :initarg :lock-name
              :reader get-lock-name))
  (:documentation "Signaled if some thread was unable to get a lock on a database.")
  (:report (lambda (condition stream)
             (format stream "Unable to aquire lock \"~A\" on Postgres database."
                     (get-lock-name condition)))))


(defun try-to-get-lock (lock-name &key (signal-on-failure t))
  (unless cl-dbi::*in-transaction*
    (error "To get a lock, you need to start a transaction."))
  (let* ((key (make-hash-for-lock-name lock-name))
         (rows (sql-fetch-all "SELECT pg_try_advisory_xact_lock(?) as locked" key))
         (locked? (getf (first rows)
                        :|locked|) ))
    (unless locked?
      (when signal-on-failure
        (error 'unable-to-aquire-lock :lock-name lock-name)))
    locked?))


(defmacro with-lock ((name &key (signal-on-failure t)) &body body)
  `(when (try-to-get-lock ,name :signal-on-failure ,signal-on-failure)
     (log:info "Lock aquired:" ,name mito:*connection*)
     ,@body))
