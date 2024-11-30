(defpackage #:ultralisp/stats
  (:use #:cl)
  (:import-from #:log4cl)
  (:import-from #:log4cl-extras/error
                #:with-log-unhandled)
  (:import-from #:ultralisp/db
                #:with-connection)
  (:import-from #:prometheus
                #:collector
                #:make-registry)
  (:import-from #:prometheus.formats.text
                #:marshal)
  #+sbcl
  (:import-from #:prometheus.sbcl
                #:make-memory-collector
                #:make-threads-collector)
  (:import-from #:prometheus.process
                #:make-process-collector)
  (:import-from #:reblocks/session)
  (:import-from #:reblocks/routes
                #:defroute)
  (:import-from #:routes
                #:parse-template)
  (:import-from #:kebab
                #:to-snake-case)
  (:import-from #:ultralisp/app
                #:app)
  (:export
   #:increment-counter
   #:add-counter
   #:add-gauge
   #:initialize))
(in-package #:ultralisp/stats)


(defvar *registry* nil
  "The holder for prometheus metrics.")


(defclass reblocks-stats (collector)
  ((gauges :initform (make-hash-table)
           :documentation "Each value is a cons of the gauge object and
  funcallable to set value of the gauge. Funcallable will be called on each
  retrieve of /metrics route, so if there are expensive calculations, use a
  caching to not recalculate value each time. You can use FUNCTION-CACHE
  system to do caching."
           :reader get-gauges)
   (counters :initform (make-hash-table)
             :reader get-counters)))


(defun make-reblocks-stats (registry)
  (let ((collector (make-instance 'reblocks-stats
                                  :name "reblocks_collector")))
    (when registry
      (prometheus:register collector registry))
    collector))


(defmethod prometheus:collect ((collector reblocks-stats) cb)
  (loop for (gauge . func) being the hash-values in (get-gauges collector)
        for value = (funcall func)
        do (prometheus:gauge.set gauge value)
           (prometheus:collect gauge cb))
  
  (loop for counter being the hash-values in (get-counters collector)
        do (prometheus:collect counter cb)))


(defvar *collector* nil)


(defun add-counter (name help &key labels)
  (check-type name keyword)
  (check-type labels list)

  (unless *collector*
    (error "Please call ultralisp/stats:initialize before calling this function."))
  
  (setf (gethash name (get-counters *collector*))
        (prom:make-counter :name (to-snake-case (symbol-name name))
                           :help help
                           :labels labels
                           :registry nil)))


(defun add-gauge (name help funcallable &key labels)
  (check-type name keyword)
  (check-type labels list)

  (unless *collector*
    (error "Please call ultralisp/stats:initialize before calling this function."))
  
  (setf (gethash name (get-gauges *collector*))
        (cons (prom:make-gauge :name (to-snake-case (symbol-name name))
                               :help help
                               :labels labels
                               :registry nil)
              funcallable))
  (values))


(defun increment-counter (name &key (value 1)
                                    labels)
  (check-type name keyword)
  (check-type value integer)
  (check-type labels list)

  (unless *collector*
    (error "Please call ultralisp/stats:initialize before calling this function."))
  
  (let ((counter (gethash name (get-counters *collector*))))
    (unless counter
      (error "No counter with name ~A" name))
    
    (prometheus:counter.inc counter
                            :value value
                            :labels labels)))


(defun initialize ()
  (unless *registry*
    (setf *registry* (make-registry))
    (setf *collector* (make-reblocks-stats *registry*))
    
    (let ((prom:*default-registry* *registry*))
      ;; NOTE: Turned off because process hangs after some period of time
      ;;       See https://github.com/ultralisp/ultralisp/issues/288 for details
      ;; #+sbcl
      ;; (make-memory-collector)
      #+sbcl
      (make-threads-collector)
      (make-process-collector)))
  (values))


(defroute (app /metrics :content-type "text/plain")
  (with-log-unhandled ()
    (with-connection ()
      (let ((content (if *registry*
                         (marshal *registry*)
                         "")))
        ;; This is API, we don't want to keep any sessions here
        (reblocks/session:expire)
        (values content)))))
