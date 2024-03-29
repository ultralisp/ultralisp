(defpackage #:ultralisp/utils/http
  (:use #:cl)
  (:import-from #:jonathan)
  (:import-from #:dexador)
  (:export
   #:get-json))
(in-package #:ultralisp/utils/http)


(defun get-json (url &key headers)
  (jonathan:parse
   (dex:get url
            :connect-timeout 3
            :read-timeout 3
            :headers headers)))
