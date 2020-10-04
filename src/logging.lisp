(defpackage #:ultralisp/logging
  (:use #:cl)
  (:import-from #:log4cl-extras/config)
  (:export
   #:setup-for-repl
   #:setup))
(in-package ultralisp/logging)


(defvar *log-dir* nil
  "Here we'll remember a directory to reuse it during setup-for-repl call.")


(defun setup (log-dir &key (level :error))
  (setf *log-dir* log-dir)
  
  (log4cl-extras/config:setup
   `(:level ,level
     :appenders ((daily :layout :json
                        :name-format ,(format nil "~A/app.log"
                                              log-dir)
                        :backup-name-format "app-%Y%m%d.log")))))


(defun setup-for-repl (&key (level :debug))
  (log4cl-extras/config:setup
   `(:level ,level
     :appenders ((this-console :layout :plain)
                 (daily :layout :json
                        :name-format ,(format nil "~A/app.log"
                                              (or *log-dir*
                                                  "/tmp"))
                        :backup-name-format "app-%Y%m%d.log")))))
