(uiop:define-package #:ultralisp/routes
  (:use #:cl))
(in-package #:ultralisp/routes)


(defgeneric process-webhook-route (app))
