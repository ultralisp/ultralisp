(uiop:define-package #:ultralisp-docs/index
  (:use #:cl)
  (:import-from #:pythonic-string-reader
                #:pythonic-string-syntax)
  #+quicklisp
  (:import-from #:quicklisp)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:40ants-doc
                #:defsection
                #:defsection-copy)
  (:import-from #:ultralisp-docs/changelog
                #:@changelog)
  (:import-from #:docs-config
                #:docs-config)
  (:import-from #:40ants-doc/autodoc
                #:defautodoc)
  (:import-from #:ultralisp-docs/architecture
                #:@architecture)
  (:export #:@index
           #:@readme
           #:@changelog))
(in-package #:ultralisp-docs/index)

(in-readtable pythonic-string-syntax)


(defmethod docs-config ((system (eql (asdf:find-system "ultralisp-docs"))))
  ;; 40ANTS-DOC-THEME-40ANTS system will bring
  ;; as dependency a full 40ANTS-DOC but we don't want
  ;; unnecessary dependencies here:
  #+quicklisp
  (ql:quickload "40ants-doc-theme-40ants")
  #-quicklisp
  (asdf:load-system "40ants-doc-theme-40ants")
  
  (list :theme
        (find-symbol "40ANTS-THEME"
                     (find-package "40ANTS-DOC-THEME-40ANTS")))
  )


(defsection @index (:title "ultralisp - A Quicklisp compatible Common Lisp software distribution."
                    :ignore-words ("JSON"
                                   "HTTP"
                                   "TODO"
                                   "BSD"
                                   "REPL"
                                   "ASDF:PACKAGE-INFERRED-SYSTEM"
                                   "ASDF"
                                   "S3"
                                   "40A"
                                   "API"
                                   "URL"
                                   "URI"
                                   "RPC"
                                   "GIT"))
  (ultralisp system)
  "
[![](https://github-actions.40ants.com/ultralisp/ultralisp/matrix.svg?only=ci.run-tests)](https://github.com/ultralisp/ultralisp/actions)

![Quicklisp](http://quickdocs.org/badge/ultralisp.svg)
"
  ;; (@installation section)
  ;; (@usage section)
  (@architecture section))


(defsection-copy @readme @index)


;; (defsection @installation (:title "Installation")
;;   """
;; You can install this library from Quicklisp, but you want to receive updates quickly, then install it from Ultralisp.org:

;; ```
;; (ql-dist:install-dist "http://dist.ultralisp.org/"
;;                       :prompt nil)
;; (ql:quickload :ultralisp)
;; ```
;; """)


;; (defsection @usage (:title "Usage")
;;   "
;; TODO: Write a library description. Put some examples here.
;; ")


