(defpackage #:ultralisp/logging
  (:use #:cl)
  (:import-from #:log4cl-extras/config)
  (:import-from #:log4cl-extras/error)
  (:export
   #:setup-for-repl
   #:setup))
(in-package #:ultralisp/logging)


(defvar *log-dir* nil
  "Here we'll remember a directory to reuse it during setup-for-repl call.")


(defparameter *max-depth* 100)


(defun setup (log-dir &key
                        (level :error)
                        (app "app"))
  (setf *log-dir* log-dir)

  (setf log4cl-extras/error:*max-traceback-depth*
        *max-depth*)
  
  (log4cl-extras/config:setup
    `(:level ,level
      :appenders ((daily :layout :json
                         :name-format ,(format nil "~A/~A.log"
                                               log-dir
                                               app)
                         :backup-name-format "app-%Y%m%d.log")))))


(defun setup-for-repl (&key
                       (level :debug)
                       (app "app")
                       (log-dir "logs"))
  (setf log4cl-extras/error:*max-traceback-depth*
        *max-depth*)
  (setf *log-dir* log-dir)
  
  (let ((filename (format nil "~A/~A.log"
                          log-dir
                          app))
        (backup-filename (format nil "~A-%Y%m%d.log"
                                 app)))
    (log4cl-extras/config:setup
      `(:level ,level
        :appenders ((this-console :layout :plain)
                    (daily :layout :json
                           :name-format ,filename
                           :backup-name-format ,backup-filename))))))
