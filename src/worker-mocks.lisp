(uiop:define-package #:ultralisp/worker-mocks
  (:use #:cl))
(in-package #:ultralisp/worker-mocks)


(defun get-current-user ()
  (error "GET-CURRENT-USER is not available in ultralisp worker mode"))


(defun increment-counter (counter-name)
  (declare (ignore counter-name))
  (error "INCREMENT-COUNTER function is unavailable in Ultralisp Worker."))

