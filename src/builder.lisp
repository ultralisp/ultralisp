(defpackage #:ultralisp/builder
  (:use #:cl)
  (:import-from #:local-time
                #:format-timestring
                #:+rfc3339-format/date-only+
                #:today)
  (:import-from #:quickdist
                #:quickdist)
  (:import-from #:ultralisp/downloader
                #:download)
  (:export
   #:build))
(in-package ultralisp/builder)


(defun get-new-version-number ()
  (format-timestring nil
                     (today)
                     :format +rfc3339-format/date-only+))


(defun build (&key (projects-metadata-path "projects.txt")
                (projects-dir "ultralisp-projects/")
                (name "ultralisp")
                (base-url "http://dist.ultralisp.org/")
                (dists-dir "ultralisp-dist/"))
  (download projects-metadata-path
            projects-dir)
  (quickdist :name name
             :base-url base-url
             :projects-dir projects-dir
             :dists-dir dists-dir
             :version (get-new-version-number)))

