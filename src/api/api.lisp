(uiop:define-package #:ultralisp/api/api
  (:use #:cl)
  (:import-from #:openrpc-server
                #:define-api))
(in-package #:ultralisp/api/api)


(define-api (api
             :title "Ultralisp API"
             :version "0.1.0"))


(defvar *default-page-size* 2)
