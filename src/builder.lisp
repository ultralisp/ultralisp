(defpackage #:ultralisp/builder
  (:use #:cl)
  (:import-from #:local-time
                #:now
                #:format-timestring)
  (:import-from #:quickdist
                #:quickdist)
  (:import-from #:ultralisp/downloader/base
                #:download)
  (:import-from #:ultralisp/models/version
                #:get-built-at
                #:get-number
                #:version)
  (:import-from #:mito
                #:save-dao)
  (:export
   #:build
   #:build-version))
(in-package ultralisp/builder)


(defun get-new-version-number ()
  (format-timestring nil
                     (now)
                     :format '((:year 4) (:month 2) (:day 2) #\-
                               (:hour 2) (:min 2) (:sec 2))
                     :timezone local-time:+utc-zone+))


(defun build (&key
                (projects :all)
                (projects-dir "build/sources/")
                (name "ultralisp")
                (base-url "http://dist.ultralisp.org/")
                (dist-dir "build/dist/"))
  (download projects projects-dir)
  (quickdist :name name
             :base-url base-url
             :projects-dir projects-dir
             :dists-dir dist-dir
             :version (get-new-version-number)))


(defun build-version (version
                      &key
                        (projects-dir "build/sources/")
                        (name "ultralisp")
                        (base-url "http://dist.ultralisp.org/")
                        (dist-dir "build/dist/"))
  (check-type version version)
  (download version projects-dir)
  (quickdist :name name
             :base-url base-url
             :projects-dir projects-dir
             :dists-dir dist-dir
             :version (get-number version))
  (setf (get-built-at version)
        (local-time:now))
  (save-dao version))


(defun test-build (&key
                     (projects-dir "build/sources/")
                     (name "ultralisp")
                     (base-url "http://dist.ultralisp.org/")
                     (dist-dir "build/dist/"))
  (quickdist :name name
             :base-url base-url
             :projects-dir projects-dir
             :dists-dir dist-dir
             :version (get-new-version-number)))

