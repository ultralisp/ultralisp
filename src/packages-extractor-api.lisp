(defpackage #:ultralisp/packages-extractor-api
  (:use #:cl)
  (:import-from #:cl-strings
                #:split)
  (:import-from #:log4cl-extras/error
                #:with-log-unhandled)
  (:import-from #:log4cl-extras/context
                #:with-fields)
  (:import-from #:function-cache
                #:defcached)
  (:export
   #:get-packages))
(in-package ultralisp/packages-extractor-api)


(defun parse-packages-output (text)
  "Parses text like this:

   dexador DEXADOR.ERROR DEXADOR.UTIL DEXADOR.DECODING-STREAM DEXADOR.CONNECTION-CACHE DEXADOR.BACKEND.USOCKET DEXADOR DEXADOR.BODY DEXADOR.KEEP-ALIVE-STREAM DEXADOR.ENCODING

   Into a list of plists:

   ((:system \"dexador\" :packages (list \"DEXADOR.ERROR\" \"DEXADOR.UTIL\" ...)))
"
  (with-input-from-string (s text)
    (loop for line = (read-line s nil nil)
          while line
          for parsed = (split line)
          for system-name = (first parsed)
          for packages = (rest parsed)
          unless (string= system-name "")
            collect (list :system system-name
                          :packages packages))))

(defun get-packages-orig (system-names &key (extractor-binary-path "/app/packages-extractor"))
  "External function to use in other Ultralisp code.
   Runs packages extractor in a separate process which
   does not have any dependencies and is able to
   load a system from scratch."
  (let* ((system-names (uiop:ensure-list system-names))
         (command (format nil "qlot exec ~A~{ ~A~}"
                          extractor-binary-path
                          system-names)))
    (uiop:with-current-directory ("/app")
      (handler-case
          (with-fields (:systems system-names)
            (with-log-unhandled ()
              (parse-packages-output
               (with-output-to-string (output)
                 (with-output-to-string (error-output)
                   (handler-bind ((uiop:subprocess-error
                                    (lambda (c)
                                      (let ((exit-code (uiop:subprocess-error-code c))
                                            (command (uiop:subprocess-error-command c))
                                            (output (get-output-stream-string
                                                     output))
                                            (error-output (get-output-stream-string
                                                           error-output)))
                                        (log:error "packages-extractor returned"
                                                   exit-code
                                                   command
                                                   output
                                                   error-output)))))
                     (uiop:run-program command
                                   :output output
                                   :error error-output)))))))
        ;; In any case we just log error and return nothing
        (uiop/run-program:subprocess-error ())))))

(defcached get-packages (system-names &key (extractor-binary-path "/app/packages-extractor")
                                      (work-dir "/app"))
  "External function to use in other Ultralisp code.
   Runs packages extractor in a separate process which
   does not have any dependencies and is able to
   load a system from scratch."
  (log:info "Getting packages for" system-names)
  (let* ((system-names (uiop:ensure-list system-names))
         (command (format nil "qlot exec ~A~{ ~A~}"
                          extractor-binary-path
                          system-names)))
    (uiop:with-current-directory (work-dir)
      (log:info "Calling" command work-dir)
      (handler-case
          (with-fields (:systems system-names)
            (with-log-unhandled ()
              (let (response)
                (with-output-to-string (output)
                  (with-output-to-string (error-output)
                    (handler-bind ((uiop:subprocess-error
                                     (lambda (c)
                                       (let ((exit-code (uiop:subprocess-error-code c))
                                             (command (uiop:subprocess-error-command c))
                                             (output (get-output-stream-string
                                                      output))
                                             (error-output (get-output-stream-string
                                                            error-output)))
                                         (log:error "packages-extractor returned"
                                                    exit-code
                                                    command
                                                    output
                                                    error-output)))))
                      (uiop:run-program command
                                        :output output
                                        :error error-output)
                      (setf response (get-output-stream-string output)))))
                (parse-packages-output response))))
        ;; In any case we just log error and return nothing
        (uiop/run-program:subprocess-error ())))))
