(defpackage #:ultralisp/packages-extractor-api
  (:use #:cl)
  (:import-from #:str)
  (:import-from #:log4cl-extras/error
                #:with-log-unhandled)
  (:import-from #:log4cl-extras/context
                #:with-fields)
  (:import-from #:function-cache
                #:defcached)
  (:import-from #:rutils
                #:fmt)
  (:export
   #:get-packages
   #:with-saved-ultralisp-root))
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
          for parsed = (str:split " " line)
          for system-name = (first parsed)
          for packages = (rest parsed)
          unless (string= system-name "")
            collect (list :system system-name
                          :packages packages))))


(defvar *ultralisp-root*)


(defmacro with-saved-ultralisp-root (&body body)
  "
  This macro needed to save path to Ultralisp root before
  we'll enter into a local Qlot environment.

  This path will be used to call Ultralisp functions in a
  separate process.
  "
  `(let ((*ultralisp-root* (uiop:merge-pathnames*
                            "../"
                            (asdf:component-pathname
                             (asdf:find-system :ultralisp)))))
     ,@body))


(defun make-command-to-run (func-as-string)
  (unless (boundp '*ultralisp-root*)
    (error "Variable *ULTRALISP-ROOT* is unbound. Wrap code using WITH-SAVED-ULTRALISP-ROOT."))
  
  (fmt "qlot exec ros run --eval '(push \"~A\" asdf:*central-registry*)' --system ultralisp/packages-extractor --eval '~A' --quit"
       *ultralisp-root*
       func-as-string))


(defcached (get-packages :timeout 30) (system-names &key
                                                    (work-dir "/app"))
  "
  External function to use in other Ultralisp code.
  Runs packages extractor in a separate process which
  does not have any dependencies and is able to
  load a system from scratch.

  Returns a list of lists where each sublist is a plist:

      ((:SYSTEM \"jonathan\" :PACKAGES
        (\"JONATHAN\" \"JONATHAN.HELPER\" \"JONATHAN.DECODE\" \"JONATHAN.ENCODE\"
         \"JONATHAN.UTIL\" \"JONATHAN.ERROR\")))

  "
  (log:info "Getting packages for" system-names)
  (let* ((system-names (uiop:ensure-list system-names))
         (command (make-command-to-run
                   (rutils:fmt "(ultralisp/packages-extractor::inner-main~{ ~A~})"
                               (loop for name in system-names
                                     collect (format nil
                                                     "\"~A\""
                                                     (string-downcase name))))))
         (result nil))
    (with-fields (:command command)
      (uiop:with-current-directory (work-dir)
        (with-output-to-string (output)
          (with-output-to-string (error-output)
            (handler-case
                (with-fields (:systems system-names)
                  (with-log-unhandled ()
                    (uiop:run-program command
                                      :output output
                                      :error-output error-output)
                    (setf result
                          (parse-packages-output
                           (get-output-stream-string output)))))
              ;; In any case we just log error and return nothing
              (uiop/run-program:subprocess-error (c)
                (let ((exit-code (uiop:subprocess-error-code c))
                      (output (get-output-stream-string
                               output))
                      (error-output (get-output-stream-string
                                     error-output)))
                  (with-fields (:exit-code exit-code
                                :output output
                                :error-output error-output)
                    (log:error "Packages extractor returned error")))))))))
    (values result)))
