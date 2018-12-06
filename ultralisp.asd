(defsystem ultralisp
  :description "A fast-moving Common Lisp software distribution for those who want to publish his/her software today."
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :licence "BSD"
  :class :package-inferred-system
  :pathname "src"
  :depends-on (:cl-interpol
               ;; To make inplace links work in the HTML
               "ultralisp/main"
               "ultralisp/server"
               "ultralisp/widgets/landing"
               "ultralisp/uploader/s3"
               "ultralisp/uploader/fake"
               "ultralisp/downloader/github"
               "ultralisp/downloader/version"
               "ultralisp/downloader/project")
  :perform (compile-op :before (o c)
                       #+ros.installing
                       (roswell:roswell '("install" "40ants/defmain"))))
