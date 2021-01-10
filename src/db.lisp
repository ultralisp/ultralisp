(defpackage #:ultralisp/db
  (:use #:cl)
  (:import-from #:cl-dbi)
  (:import-from #:cl-postgres)
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
   #:get-lock
   #:execute
   #:connect-toplevel-in-dev
   #:lock-timeout))
(in-package ultralisp/db)


(defun connect (&key host database-name username password
                     (cached t))
  (funcall (if cached
               'cl-dbi:connect-cached
               'cl-dbi:connect)
           :postgres
           :host (or host
                     (get-postgres-host))
           :database-name (or database-name
                              (get-postgres-dbname))
           :username (or username
                         (get-postgres-user))
           :password (or password
                         (get-postgres-pass))))


(defun connect-toplevel ()
  (setf mito:*connection* (connect :cached nil)))


(defun connect-toplevel-in-dev ()
  (setf mito:*connection*
        (cl-dbi:connect :postgres
                        :host (ultralisp/variables:get-postgres-host)
                        :port 5432
                        :database-name (ultralisp/variables:get-postgres-dbname)
                        :username "ultralisp"
                        :password "ultralisp")))


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
                        (log:info "Commiting transaction because of \"immediate-response\"")
                        (cl-dbi:commit mito:*connection*)
                        (log:info "AFTER COMMIT"))))
       ,@body)))


(defvar *was-cached* nil)


(defmacro with-connection ((&rest connect-options) &body body)
  "Establish a new connection and start transaction"
  (let ((is-cached (getf connect-options :cached t)))
    `(let* ((was-cached *was-cached*)
            (mito:*connection* (funcall #'connect
                                        ,@connect-options))
            (*was-cached* ,is-cached))
       (unwind-protect
            (with-transaction
              ,@body)
         (unless (and ,is-cached was-cached)
           ;; We don't want to close nested cached connections
           ;; because they should be closed only on upper level
           ;; Here is a state table showing in which cases connect
           ;; will be closed:
           ;; | top connect | nested connect | close top | close nested |
           ;; | cached?     | cached?        | connect?  | connect?     |
           ;; |-------------+----------------+-----------+--------------|
           ;; | nil         | nil            | t         | t            |
           ;; | nil         | t              | t         | t            |
           ;; | t           | t              | t         | nil          |
           (cl-dbi:disconnect mito:*connection*))))))


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


(defun execute (sql &rest params)
  (cl-dbi:execute (cl-dbi:prepare mito:*connection* sql) params))


(defun sql-fetch-all (sql &rest params)
  (cl-dbi:fetch-all (cl-dbi:execute (cl-dbi:prepare mito:*connection* sql) params)))


(define-condition unable-to-aquire-lock (simple-error)
  ((lock-name :initarg :lock-name
              :reader get-lock-name
              :documentation "Human readable lock name, passed to `with-lock' macro.")
   (key :initarg :key
        :reader get-key
        :documentation "An integer, representing lock in a Postgres database."))
  (:documentation "Signaled if some thread was unable to get a lock on a database.")
  (:report (lambda (condition stream)
             (format stream
                     "Unable to aquire lock: name=~A key=~A"
                     (ignore-errors
                      (get-lock-name condition))
                     (ignore-errors
                      (get-key condition))))))


(define-condition lock-timeout (unable-to-aquire-lock)
  ((timeout :initarg :timeout
            :reader get-timeout
            :documentation "An integer, a number a milliseconds."))
  (:report
   (lambda (condition stream)
     (format stream
             "Lock timeout: name=~A key=~A timeout=~A"
             (ignore-errors (get-lock-name condition))
             (ignore-errors (get-key condition))
             (ignore-errors (get-timeout condition)))))
  (:documentation "Raised when you are trying to get lock to was unable to do this during current lock_timeout."))


(defun try-to-get-lock (lock-name &key (signal-on-failure t))
  (unless (cl-dbi:in-transaction mito:*connection*)
    (error "To get a lock, you need to start a transaction."))
  
  (let* ((key (make-hash-for-lock-name lock-name))
         (rows (sql-fetch-all "SELECT pg_try_advisory_xact_lock(?) as locked" key))
         (locked? (getf (first rows)
                        :|locked|) ))
    (unless locked?
      (log:warn "Unable to get lock" lock-name)
      (when signal-on-failure
        (error 'unable-to-aquire-lock
               :lock-name lock-name
               :key key)))
    locked?))


(defun get-lock (lock-name &key (timeout 3000))
  ""
  (unless (cl-dbi:in-transaction mito:*connection*)
    (error "To get a lock, you need to start a transaction."))
  
  (let ((key (make-hash-for-lock-name lock-name)))
    (when timeout
      (check-type timeout integer)
      (execute (format nil "SET lock_timeout = ~A" timeout)))
    
    (handler-bind ((cl-dbi:<dbi-database-error>
                     ;; If we were unable to acquire lock because
                     ;; of timeout, we need to throw a special
                     ;; condition which can be catched by the caller
                     (lambda (condition)
                       (with-slots ((code dbi.error::error-code)) condition
                         ;; lock_not_available
                         (when (string-equal code "55P03")
                           (error 'lock-timeout
                                  :lock-name lock-name
                                  :key key
                                  :timeout timeout))))))
      (execute "SELECT pg_advisory_xact_lock(?)" key))))


(defmacro with-lock ((name &key (block t) (timeout 3000) (signal-on-failure t)) &body body)
  (if block
      `(block with-lock
         (handler-bind ((lock-timeout (lambda (c)
                                        (declare (ignorable c))
                                        (unless ,signal-on-failure
                                          (return-from with-lock))))))
         (get-lock ,name :timeout ,timeout)
         (log:debug "Lock aquired:" ,name mito:*connection*)
         ,@body)
      `(when (try-to-get-lock ,name :signal-on-failure ,signal-on-failure)
         (log:debug "Lock aquired:" ,name mito:*connection*)
         ,@body)))
