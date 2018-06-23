(defpackage #:ultralisp/main
  (:use #:cl
        #:defmain)
  (:import-from #:ultralisp/builder
                #:build)
  (:import-from #:ultralisp/uploader
                #:upload)
  (:export
   #:main))
(in-package ultralisp/main)


(defmain main ()
  (build)
  (upload :to "test-dist2"))
