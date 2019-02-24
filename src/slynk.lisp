(defpackage #:ultralisp/slynk
  (:use #:cl)
  (:import-from #:slynk)
  (:import-from #:log4cl)
  (:import-from #:global-vars
                #:define-global-var)
  (:export
   #:*connections*
   #:setup))
(in-package ultralisp/slynk)


(define-global-var *connections* nil
  "Here we'll store all Slynk connections.")


(defun on-connection-open (conn)
  (log:info "SLY connected")
  (push conn *connections*))


(defun on-connection-close (conn)
  (log:info "SLY disconnected")
  (setf *connections*
        (remove conn *connections*)))


(defun setup ()
  (slynk-api:add-hook slynk-api:*new-connection-hook*
                      'on-connection-open)
  (slynk-api:add-hook slynk-api:*connection-closed-hook*
                      'on-connection-close)
  (values))
