(defpackage #:ultralisp/utils/text
  (:use #:cl)
  (:import-from #:str)
  (:export
   #:multi-split))
(in-package #:ultralisp/utils/text)


(defun multi-split (separators text &key (trim-whitespace t) (remove-empty t))
  "Like str:split, but can work with a multiple separators"
  (labels ((recur-split (separators text)
             (if separators
                 (loop for part in (str:split (first separators)
                                              text)
                       appending (recur-split (rest separators)
                                              part))
                 (list text))))
    (loop for item in (recur-split (uiop:ensure-list separators)
                                   text)
          for resulting-item = (if trim-whitespace
                                   (string-trim '(#\Space) item)
                                   item)
          unless (and remove-empty
                      (string= resulting-item ""))
            collect resulting-item)))
