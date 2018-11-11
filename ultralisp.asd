(defsystem ultralisp
  :description "A fast-moving Common Lisp software distribution for those who want to publish his/her software today."
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :licence "BSD"
  :class :package-inferred-system
  :pathname "src"
  :depends-on (:cl-interpol
               ;; To make inplace links work in the HTML
               :spinneret/cl-markdown
               "ultralisp/main"
               "ultralisp/server"
               "ultralisp/widgets/landing")
  :perform (compile-op :before (o c)
                       #+ros.installing
                       (roswell:roswell '("install" "40ants/defmain"))))
