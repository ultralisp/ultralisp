(defpackage #:ultralisp/builder
  (:use #:cl)
  (:import-from #:local-time
                #:now
                #:format-timestring)
  (:import-from #:quickdist
                #:quickdist)
  (:import-from #:ultralisp/downloader
                #:download)
  (:export
   #:build))
(in-package ultralisp/builder)


(defun get-new-version-number ()
  (format-timestring nil
                     (now)
                     :format '((:year 4) (:month 2) (:day 2) #\-
                               (:hour 2) (:min 2) (:sec 2))))


(defun build (&key (projects-metadata-path "projects.txt")
                (projects-dir "build/sources/")
                (name "ultralisp")
                (base-url "http://dist.ultralisp.org/")
                (dist-dir "build/dist/"))
  (download projects-metadata-path
            projects-dir)
  (quickdist :name name
             :base-url base-url
             :projects-dir projects-dir
             :dists-dir dist-dir
             :version (get-new-version-number)))

