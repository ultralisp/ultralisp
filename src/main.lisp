(defpackage #:ultralisp/main
  (:use #:cl
        #:defmain)
  (:import-from #:ultralisp/builder
                #:build)
  (:import-from #:ultralisp/uploader/base
                #:upload)
  (:export
   #:main))
(in-package ultralisp/main)


(defmain main ((upload "Add this option to upload data to S3."
                       :flag t)
               (projects-dir "A directory where sources will checked out."
                             :default "build/sources/"
                             :short nil)
               (dist-dir "A directory where sources will checked out."
                         :default "build/dist/")
               (name "A distribution's name."
                     :default "ultralisp")
               (base-url "A base url from which a distribution will be served."
                         :default "http://dist.ultralisp.org/"))
  (build :projects-dir projects-dir
         :dist-dir dist-dir
         :name name
         :base-url base-url)
  (when upload
    (error "This code was broken at some moment, upload requires two parameters. But everything seems work. Probably it is not used anymore.")
    ;; (upload dist-dir)
    ))
