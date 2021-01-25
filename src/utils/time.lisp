(defpackage #:ultralisp/utils/time
  (:use #:cl)
  (:import-from #:local-time-duration)
  (:export
   #:humanize-duration))
(in-package ultralisp/utils/time)


(defun default-format-part (stream part-type part)
  "This is default function, define your own if you want to support other language."
  (ecase part-type
    (:weeks (format stream "~d week~:p" part))
    (:days (format stream "~d day~:p" part))
    (:hours (format stream "~d hour~:p" part))
    (:minutes (format stream "~d minute~:p" part))
    (:secs (format stream "~d second~:p" part))
    (:nsecs (format stream "~d nsec~:p" part))))


(defun humanize-duration (duration &optional stream (n-parts 2) (format-part #'default-format-part))
  "This is the better version of local-time-duration:human-readable-duration.

   By default it returns only 2 most significant duration parts.

   If duration is 2 hour, 43 seconds and 15 nanoseconsds, then
   function will return \"2 hours 43 seconds\":

   ```lisp
   CL-USER> (ultralisp/utils/time:humanize-duration
             (local-time-duration:duration :hour 2
                                           :sec 43
                                           :nsec 15))
   \"2 hours 43 seconds\"
   ```
"
  (check-type n-parts (integer 1 6))
  
  (multiple-value-bind (nsecs secs minutes hours days weeks)
      (local-time-duration::decode-duration duration :weeks t)
    (local-time-duration::with-designated-stream (stream stream)
      (let ((part-types (list :weeks :days :hours :minutes :secs :nsecs))
            (parts (list weeks days hours minutes secs nsecs)))
        (if (every #'zerop parts)
            (format stream "0 length")
            (loop with n-printed = 0
                  for part in parts
                  for part-type in part-types
                  unless (zerop part)
                  do (unless (zerop n-printed)
                       ;; Add a space between parts
                       (format stream " "))
                     (funcall format-part
                              stream
                              part-type
                              part)
                     (incf n-printed)
                  when (>= n-printed n-parts)
                  do (return)))))))
