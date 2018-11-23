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
                #:get-pending-version
                #:get-built-at
                #:get-number
                #:version)
  (:import-from #:mito
                #:save-dao)
  (:import-from #:ultralisp/db
                #:with-lock
                #:with-transaction)
  (:import-from #:ultralisp/uploader/base
                #:upload)
  (:export
   #:build
   #:build-version
   #:build-pending-version))
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

  (with-transaction
    (download version projects-dir)
    (quickdist :name name
               :base-url base-url
               :projects-dir projects-dir
               :dists-dir dist-dir
               :version (get-number version))
    (setf (get-built-at version)
          (local-time:now))
    (save-dao version)

    ;; TODO: probably it is not the best idea to upload dist-dir
    ;;       every type, becase there can be previously built distributions
    ;;       May be we need to minimize network traffic here and upload
    ;;       only a part of it or make a selective upload which will not
    ;;       transfer files which already on the S3.
    (upload dist-dir)))


(defun build-pending-version ()
  "Searches and builds a pending version if any."
  (with-transaction
    (with-lock ("performing-pending-checks-or-verion-build"
                ;; We don't need to signal because this function
                ;; will be called again by "cron" after some
                ;; period of time.
                :signal-on-failure nil)
      (let ((version (get-pending-version)))
        (when version
          (build-version version))))))


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

