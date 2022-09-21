(defpackage #:ultralisp/utils/lisp
  (:use #:cl)
  (:import-from #:str
                #:split)
  (:export
   #:get-compiler-policies))
(in-package #:ultralisp/utils/lisp)


(defun get-compiler-policies ()
  #+sbcl
  (let* ((text (with-output-to-string (*standard-output*)
                 (sb-ext:describe-compiler-policy)))
         (lines (split #\Newline text))
         (core-policies (subseq lines 1 6)))
    (str:join #\Newline core-policies))
  #-sbcl
  (format nil "Unable to get policies for ~A implementation."
          (lisp-implementation-type)))
