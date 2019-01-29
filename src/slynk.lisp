(defpackage #:ultralisp/slynk
  (:use #:cl)
  (:import-from #:slynk)
  (:import-from #:log4cl)
  (:export
   #:*connections*
   #:setup))
(in-package ultralisp/slynk)


(global-vars:define-global-var *connections* nil
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
