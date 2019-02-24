(defpackage #:ultralisp/db
  (:use #:cl)
  (:import-from #:cl-dbi)
  (:import-from #:mito)
  (:import-from #:ironclad
                #:octets-to-integer
                #:hmac-digest
                #:make-hmac
                #:ascii-string-to-byte-array)
  (:import-from #:ultralisp/variables
                #:get-postgres-pass
                #:get-postgres-user
                #:get-postgres-host
                #:get-postgres-dbname)
  (:import-from #:weblocks/response
                #:immediate-response)
  (:export
   #:with-transaction
   #:with-connection
   #:connect-toplevel
   #:with-lock
   #:unable-to-aquire-lock
   #:get-lock-name
   #:sql-fetch-all
   #:get-lock))
(in-package ultralisp/db)


(defun connect (&key host database-name username password
                  (cached t))
  (funcall (if cached
               'cl-dbi:connect-cached
               'cl-dbi:connect)
           :postgres
           :host (or host
                     (get-postgres-host))
           :database-name (or  database-name
                               (get-postgres-dbname))
           :username (or username
                         (get-postgres-user))
           :password (or password
                         (get-postgres-pass))))


(defun connect-toplevel ()
  (setf mito:*connection* (connect :cached nil)))


(defmacro with-transaction (&body body)
  `(cl-dbi:with-transaction mito:*connection*
     (handler-bind ((immediate-response
                      (lambda (condition)
                        (declare (ignorable condition))
                        ;; If transaction was interrupted because
                        ;; of immediate response, then
                        ;; we need to commit transaction.
                        ;; Otherwise, cl-dbi will rollback any
                        ;; changes made during request processing.
                        (cl-dbi:commit mito:*connection*))))
       ,@body)))


(defmacro with-connection ((&rest connect-options) &body body)
  "Establish a new connection and start transaction"
  `(let ((mito:*connection* (funcall #'connect
                                     ,@connect-options)))
     (with-transaction
       ,@body)))


(defun make-hash-for-lock-name (name)
  ;; TODO: store all names in some global
  ;;       map {hash -> name} so that we'll be enable
  ;;       to make reverse transformation and know
  ;;       which locks are held on database.
  ;;       Also, we need to add a function which
  ;;       fetches all advisory locks from pg_locks table
  ;;       and returns a list of names.
  ;;       (All of this should be made threasafe of cause.)
  (let* ((bytes (ascii-string-to-byte-array name))
         (hmac (make-hmac bytes :sha256))
         (digest (hmac-digest hmac))
         (num-bits-in-result 63)
         (result (octets-to-integer digest
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
      (log:warn "Unable to get lock" lock-name)
      (when signal-on-failure
        (error 'unable-to-aquire-lock :lock-name lock-name)))
    locked?))


(defun get-lock (lock-name)
  (unless cl-dbi::*in-transaction*
    (error "To get a lock, you need to start a transaction."))
  
  (let ((key (make-hash-for-lock-name lock-name)))
    (sql-fetch-all "SELECT pg_advisory_xact_lock(?)" key)))


(defmacro with-lock ((name &key (block t) (signal-on-failure t)) &body body)
  (if block
      `(progn
         (get-lock ,name)
         (log:debug "Lock aquired:" ,name mito:*connection*)
         ,@body)
      `(when (try-to-get-lock ,name :signal-on-failure ,signal-on-failure)
         (log:debug "Lock aquired:" ,name mito:*connection*)
         ,@body)))
