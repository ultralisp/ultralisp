(defpackage #:ultralisp/utils/db
  (:use #:cl)
  (:import-from #:alexandria
                #:make-keyword)
  (:import-from #:jonathan)
  (:export
   #:inflate-json
   #:deflate-json
   #:deflate-keyword
   #:inflate-keyword))
(in-package #:ultralisp/utils/db)


(defun deflate-keyword (symbol)
  "Prepares symbol to be stored in the TEXT postgres field.

   Value is stored in lowercase."
  (string-downcase (symbol-name symbol)))


(defun inflate-keyword (text)
  "Restores keyword symbol from the database."
  (make-keyword (string-upcase text)))


(defun deflate-json (obj)
  "Encodes object to json before it will be stored to JSONB Postgres field."
  (jonathan:to-json obj))


(defun inflate-json (text)
  "Decodes text received from the postgres JSONB field.

   Returns plist or list of plists."
  
  (jonathan:parse
   ;; Jonathan for some reason is unable to work with
   ;; `(VECTOR CHARACTER *)' type, returned by database.
   ;; here we'll convert them into `(SIMPLE-ARRAY CHARACTER (*))`
   ;; as https://sabracrolleton.github.io/json-review advices.
   ;; 
   ;; Previously we've used (COERCE 'simple-base-string) here
   ;; but it fails to convert strings containing #\HORIZONTAL_ELLIPSIS
   ;; and probably some other symbols.
   ;;
   ;; Also, previously I tried to use (format nil "~A" text) here,
   ;; but sometimes it produced a broken JSON :(
   ;; Probably, because format produces:  (SIMPLE-BASE-STRING 22524)
   ;; byt with-output-to-string produces: (SIMPLE-ARRAY CHARACTER (22524))
   ;; 
   ;; Probabably we should switch from JONATHAN to JSOWN. I've checked
   ;; and it works without coercion.
   (with-output-to-string (s)
     (write-string text s))))
