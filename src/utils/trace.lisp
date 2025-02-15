(uiop:define-package #:ultralisp/utils/trace
  (:use #:cl)
  (:import-from #:trivial-backtrace
                #:print-backtrace-to-stream))
(in-package #:ultralisp/utils/trace)


(defun dump-traces (&key (stream *standard-output*))
  (flet ((dump-trace-and-continue ()
           (format stream "Thread: ~S~2%"
                   (bt2:thread-name (bt2:current-thread)))
           (print-backtrace-to-stream stream)
           (format stream "~3%")))
    (loop for thread in (reverse (bt2:all-threads))
          do (bt2:interrupt-thread thread #'dump-trace-and-continue))))
