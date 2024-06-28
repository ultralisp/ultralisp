(defpackage #:ultralisp/db
  (:use #:cl)
  (:import-from #:log)
  (:import-from #:dbi)
  (:import-from #:dbi.error)
  (:import-from #:cl-postgres)
  ;; To prevent Mito from trying to load driver on first connect.
  ;; Sometimes this can cause errors if DBD get's updated by some
  ;; project's check
  (:import-from #:dbd.postgres)
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
  (:import-from #:reblocks/response
                #:immediate-response)
  (:import-from #:secret-values
                #:ensure-value-revealed)
  (:import-from #:alexandria
                #:with-gensyms)
  (:import-from #:bordeaux-threads-2
                #:make-lock)
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
(in-package #:ultralisp/db)


(defparameter *cached-default* t)

(defvar *debug* t
  "If T, then Postgres application name will include pid and a thread name.")

(defvar *transaction-state-queries-lock*
  (make-lock :name "*transaction-state-queries* lock"))

(defvar *transaction-state-queries*
  (make-hash-table)
  "This hash will be filled with queries when *debug* mode is on.")


(define-condition connection-error (error)
  ((message :initarg :message
            :reader error-message))
  (:report (lambda (condition stream)
             (format stream "~A"
                     (error-message condition)))))


(defun get-application-name ()
  (let ((result
          (cond
            (*debug*
             #+sbcl
             (format nil "ul:~A:~A"
                     (sb-posix:getpid)
                     (bordeaux-threads-2:thread-name
                      (bordeaux-threads-2:current-thread)))
             #+lispworks
             (format nil "ul:~A"
                     (bordeaux-threads-2:thread-name
                      (bordeaux-threads-2:current-thread))))
            (t
             "ultralisp"))))
    (str:shorten 63 result :ellipsis "")))

(declaim (notinline inner-connect))

(defun inner-connect (&key host database-name username password
                           (port 5432)
                           (application-name (get-application-name))
                           (cached *cached-default*))
  "This function is used to leave a trace in the backtrace and let
   logger know which arguments are secret."
  
  (funcall (if cached
               'cl-dbi:connect-cached
               'cl-dbi:connect)
           :postgres
           :host host
           :port port
           :database-name database-name
           :username username
           :application-name application-name
           :password (ensure-value-revealed password)))


(defun connect (&key host database-name username password
                     (port 5432)
                     (application-name (get-application-name))
                     (cached *cached-default*))
  (inner-connect :host (or host
                           (get-postgres-host))
                 :port port
                 :database-name (or database-name
                                    (get-postgres-dbname))
                 :username (or username
                               (get-postgres-user))
                 :password (or password
                               (get-postgres-pass))
                 :application-name application-name
                 :cached cached))


(defun connect-toplevel ()
  (setf mito:*connection* (connect :cached nil)))


(defun connect-toplevel-in-dev ()
  (setf mito:*connection*
        (cl-dbi:connect :postgres
                        :host (ultralisp/variables:get-postgres-host)
                        :port 5432
                        :database-name (ultralisp/variables:get-postgres-dbname)
                        :application-name "ultralisp-repl"
                        :username "ultralisp"
                        :password "ultralisp")))


(defun call-with-transaction (func)
  (cl-dbi:with-transaction mito:*connection*
    (handler-bind ((immediate-response
                     (lambda (condition)
                       (declare (ignorable condition))
                       ;; If transaction was interrupted because
                       ;; of immediate response, then
                       ;; we need to commit transaction.
                       ;; Otherwise, cl-dbi will rollback any
                       ;; changes made during request processing.
                       (log:info "Commiting transaction because of \"immediate-response\"")
                       (cl-dbi:commit mito:*connection*))))
      (funcall func))))


(defmacro with-transaction (&body body)
  (with-gensyms (transactional-func)
    `(flet ((,transactional-func ()
              ,@body))
       (declare (dynamic-extent #',transactional-func))
       (call-with-transaction #',transactional-func))))


(defvar *was-cached*)


(defun call-with-connection (func &rest connect-options &key (cached *cached-default*) &allow-other-keys)
  (when (and cached
             (boundp '*was-cached*)
             (not *was-cached*))
    (error 'connection-error
           :message "Unable to get cached connection inside a block with non-cached connection."))

  (let* ((*was-cached* cached)
         (mito:*connection*
           ;; In cached mode we will reuse current connect.
           ;; This way, nested WITH-CONNECTION calls will
           ;; reuse the same connection and postgres savepoints.
           (cond ((and *was-cached*
                       mito:*connection*)
                  mito:*connection*)
                 (t
                  (apply #'connect
                         connect-options)))))
    (unwind-protect
         (call-with-transaction func)
      (unless cached
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
        (cl-dbi:disconnect mito:*connection*)))))


(defmacro with-connection ((&rest connect-options) &body body)
  "Establish a new connection and start transaction"
  (with-gensyms (connected-func)
    `(flet ((,connected-func ()
              ,@body))
       (declare (dynamic-extent #',connected-func))
       (call-with-connection #',connected-func ,@connect-options))))


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


(defun make-list-placeholders (list)
  "Given a list of items, returns a string like \"(?,?,?)\"
   where number of questionmarks corresponds to number of list items."
  (format nil "(~{~A~^,~})"
          (loop repeat (length list)
                collect "?")))


(defmethod dbi.driver:execute-using-connection :around (conn query params)
  (let ((transaction-state (dbi.driver::get-transaction-state conn)))
    (when transaction-state
      (bt2:with-lock-held (*transaction-state-queries-lock*)
        (push (cons query params)
              (gethash transaction-state *transaction-state-queries*))))

    (call-next-method)))


(defun forget-connection-queries (conn)
  (let ((transaction-state (dbi.driver::get-transaction-state conn)))
    (when transaction-state
      (bt2:with-lock-held (*transaction-state-queries-lock*)
        (remhash transaction-state *transaction-state-queries*)))))


(defmethod dbi.driver:commit :around ((conn t))
  (unwind-protect
       (call-next-method)
    (forget-connection-queries conn)))

(defmethod dbi.driver:rollback :around ((conn t))
  (unwind-protect
       (call-next-method)
    (forget-connection-queries conn)))


(defun print-transactions-state (stream)
  "How to use this function for getting trace from a stucked thread:

(bt:interrupt-thread some-thread
     (funcall (lambda (stream) (lambda () (ultralisp/db::print-transactions-state stream)))
              *debug-io*))
"
  (loop with connections = (reverse
                            (remove-duplicates
                             (mapcar #'dbi.driver::get-conn
                                     dbi.driver::*transaction-state*)))
        with connection-to-name = (loop with result = (make-hash-table)
                                        for conn in connections
                                        for idx upfrom 0
                                        for name = (code-char (+ (char-code #\A)
                                                                  idx))
                                        do (setf (gethash conn result)
                                                 name)
                                        finally (return result))
        for state in (reverse dbi.driver::*transaction-state*)
        for state-type = (symbol-name (type-of state))
        for conn = (dbi.driver::get-conn state)
        for conn-name = (gethash conn connection-to-name)
        for queries = (reverse (bt2:with-lock-held (*transaction-state-queries-lock*)
                                 (gethash state *transaction-state-queries*)))
        do (format stream "Connection ~A (~A):~2%"
                   conn-name
                   state-type)
           (if queries
               (loop for (query . params) in queries
                     do (format stream "~A with ~A~2%"
                                query params))
               (format stream "No queries.~2%"))))
