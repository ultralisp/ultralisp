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
                #:with-connection
                #:with-lock
                #:with-transaction)
  (:import-from #:ultralisp/uploader/base
                #:upload)
  (:import-from #:ultralisp/variables
                #:get-postgres-ro-pass
                #:get-postgres-ro-user
                #:get-dist-dir
                #:get-base-url
                #:get-dist-name
                #:get-projects-dir)
  (:import-from #:ultralisp/lfarm
                #:submit-task)
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
                (projects-dir (get-projects-dir))
                (name (get-dist-name))
                (base-url (get-base-url))
                (dist-dir (get-dist-dir)))
  (download projects projects-dir)
  (quickdist :name name
             :base-url base-url
             :projects-dir projects-dir
             :dists-dir dist-dir
             :version (get-new-version-number)))


(defun build-version-remotely (version
                               &key
                                 (projects-dir (get-projects-dir))
                                 (name (get-dist-name))
                                 (base-url (get-base-url))
                                 (dist-dir (get-dist-dir))
                                 db-user
                                 db-pass)
  (check-type version version)

  (with-connection (:username db-user
                    :password db-pass)
    (download version projects-dir)
    (quickdist :name name
               :base-url base-url
               :projects-dir projects-dir
               :dists-dir dist-dir
               :version (get-number version))
    (setf (get-built-at version)
          (local-time:now))
    
    ;; TODO: probably it is not the best idea to upload dist-dir
    ;;       every type, becase there can be previously built distributions
    ;;       May be we need to minimize network traffic here and upload
    ;;       only a part of it or make a selective upload which will not
    ;;       transfer files which already on the S3.
    (upload dist-dir)
    ;; Here we don't save version object because
    ;; this function will be called on a remote worker
    ;; without "write" access to the database.
    version))


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
          (let ((updated-version (submit-task
                                  'build-version-remotely
                                  version
                                  :db-user (get-postgres-ro-user)
                                  :db-pass (get-postgres-ro-pass))))
            (save-dao updated-version)))))))


(defun test-build (&key
                     (projects-dir (get-projects-dir))
                     (name (get-dist-name))
                     (base-url (get-base-url))
                     (dist-dir (get-dist-dir)))
  (quickdist :name name
             :base-url base-url
             :projects-dir projects-dir
             :dists-dir dist-dir
             :version (get-new-version-number)))

