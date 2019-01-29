(defsystem ultralisp
  :description "A fast-moving Common Lisp software distribution for those who want to publish his/her software today."
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :licence "BSD"
  :class :package-inferred-system
  :version (:read-file-form "version.lisp-expr")
  :pathname "src"
  :depends-on ("cl-interpol"
               ;; We need this while will not support package inferred systems:
               ;; https://github.com/ultralisp/ultralisp/issues/3
               "weblocks-ui"
               ;; To make inplace links work in the HTML
               "ultralisp/main"
               "ultralisp/server"
               ;; This package depends on ultralisp/models/project and vice versa
               ;; that is why ultralisp/models/project can't depend on moderator
               ;; explicitly.
               "ultralisp/models/moderator"
               "ultralisp/widgets/landing"
               "ultralisp/uploader/s3"
               "ultralisp/uploader/fake"
               "ultralisp/downloader/github"
               "ultralisp/downloader/version"
               "ultralisp/downloader/project")
  :in-order-to ((test-op (test-op ultralisp-test)))
  :perform (compile-op :before (o c)
                       #+ros.installing
                       (roswell:roswell '("install" "40ants/defmain"))))
