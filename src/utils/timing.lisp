(uiop:define-package #:ultralisp/utils/timing
  (:use #:cl)
  (:import-from #:alexandria
                #:with-gensyms))
(in-package #:ultralisp/utils/timing)


(defvar *timings* (make-hash-table))


(defun report-timings ()
  (loop for key being the hash-key of *timings*
        using (hash-value values)
        for summ = (reduce #'+ values :initial-value 0)
        for metrics-count = (length values)
        for avg = (/ summ metrics-count)
        collect (list key avg summ metrics-count)))


(defmacro with-timings ((name) &body body)
  (with-gensyms (started-at stopped-at timing)
    `(let ((,started-at (get-internal-real-time)))
       (multiple-value-prog1 (progn ,@body)
         (let* ((,stopped-at (get-internal-real-time))
                (,timing (coerce (/ (- ,stopped-at
                                        ,started-at)
                                    internal-time-units-per-second)
                                 'float)))
           (push ,timing
                 (gethash ,name *timings*)))))))


