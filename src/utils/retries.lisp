(defpackage #:ultralisp/utils/retries
  (:use #:cl)
  (:import-from #:alexandria
                #:with-output-to-file)
  (:import-from #:ultralisp/utils
                #:delete-file-if-exists)
  (:documentation "This package contains a code to track complete
                   failures when worker process dies because of some reason.

                   The idea is to append timestamps into a files where
                   filename contains a check's id. And if there already was N attempts
                   in M minutes, then on it's next call WITH-TRIES macro will log message and
                   signal an error.")
  (:export
   #:with-tries))
(in-package #:ultralisp/utils/retries)


(defparameter *data-dir* #P"/tmp/retries/")


(defun read-timestamps (check-id)
  (let ((filename (merge-pathnames (princ-to-string check-id)
                                   *data-dir*)))
    (cond
      ((probe-file filename)
       (uiop:read-file-form filename))
      (t
       nil))))


(defun save-timestamps (check-id timestamps)
  (let ((filename (merge-pathnames (princ-to-string check-id)
                                   *data-dir*)))
    (ensure-directories-exist filename)

    (with-output-to-file (s filename
                            :if-exists :supersede)
      (write timestamps
             :stream s))
    (values)))


(defun clear-timestamps (check-id)
  (let ((filename (merge-pathnames (princ-to-string check-id)
                                   *data-dir*)))
    (delete-file-if-exists filename)
    (values)))


(defun filter-timestamps (timestamps window)
  (loop with time-in-past = (- (get-universal-time) window)
        for ts in timestamps
        when (> ts time-in-past)
        collect ts))


(defun call-with-tries (check-id num-attempts time-window func)
  (let* ((all-timestamps (read-timestamps check-id))
         (timestamps (filter-timestamps all-timestamps time-window)))

    (cond
      ((>= (length timestamps)
           num-attempts)
       (error "We've made ~A attempts in ~A seconds for check-id = ~A"
              (length timestamps)
              time-window
              check-id))
      (t
       (push (get-universal-time)
             timestamps)
       (save-timestamps check-id timestamps)

       (multiple-value-prog1
           (funcall func)
         (clear-timestamps check-id))))))


(defmacro with-tries ((check-id &key (num-attempts 3)
                                     (time-window (* 5 60)))
                      &body body)
  "Raises an error if body was executed NUM-ATTEMPTS times in TIME-WINDOW seconds,
   but Lisp crashed and didn't finished execution."
  `(call-with-tries ,check-id ,num-attempts ,time-window
                    (lambda ()
                      ,@body)))

