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
  (:import-from #:ultralisp-docs/dev
                #:@dev)
  (:import-from #:ultralisp-docs/self-hosting
                #:@self-hosting)
  (:import-from #:ultralisp-docs/intro
                #:@intro)
  (:import-from #:ultralisp-docs/hacking
                #:@hacking)
  (:export #:@index
           #:@readme
           #:@changelog))
(in-package #:ultralisp-docs/index)

(in-readtable pythonic-string-syntax)


(defsection-copy @index @intro)

(defsection @readme (:title "Ultralisp - A Quicklisp compatible Common Lisp software distribution.")
  (@intro section)
  (@more-info section))


(defsection @more-info (:title "What is next?")
  "For more information, see [full documentation](https://ultralisp.github.io/ultralisp/).")


(defmethod docs-config ((system (eql (asdf:find-system "ultralisp-docs"))))
  (list :root-sections '(@index
                         @architecture
                         @dev
                         @self-hosting
                         @hacking
                         @readme)))


;; (defsection @index (:title "ultralisp - A Quicklisp compatible Common Lisp software distribution."
;;                     :ignore-words ("JSON"
;;                                    "HTTP"
;;                                    "TODO"
;;                                    "BSD"
;;                                    "TCP"
;;                                    "SLY"
;;                                    "REPL"
;;                                    "ASDF:PACKAGE-INFERRED-SYSTEM"
;;                                    "ASDF"
;;                                    "S3"
;;                                    "40A"
;;                                    "API"
;;                                    "URL"
;;                                    "URI"
;;                                    "RPC"
;;                                    "GIT"))
;;   (ultralisp system)
;;   "
;; [![](https://github-actions.40ants.com/ultralisp/ultralisp/matrix.svg?only=ci.run-tests)](https://github.com/ultralisp/ultralisp/actions)

;; ![Quicklisp](http://quickdocs.org/badge/ultralisp.svg)
;; "
;;   (@architecture section)
;;   (@dev section))


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


