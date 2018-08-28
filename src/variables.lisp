(defpackage #:ultralisp/variables
  (:use #:cl)
  (:export
   #:get-google-counter-id
   #:get-yandex-counter-id))
(in-package ultralisp/variables)


(defun get-yandex-counter-id ()
  (uiop:getenv "YANDEX_COUNTER_ID"))


(defun get-google-counter-id ()
  (uiop:getenv "GOOGLE_COUNTER_ID"))
