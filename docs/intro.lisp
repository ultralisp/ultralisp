(uiop:define-package #:ultralisp-docs/intro
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
                #:defautodoc))
(in-package #:ultralisp-docs/intro)

(in-readtable pythonic-string-syntax)


(defsection @intro (:title "Introduction"
                    :ignore-words ("REPL"
                                   "HTTPS"))
  """

[![](http://github-actions.40ants.com/ultralisp/ultralisp/matrix.svg?only=ci,docs)](https://github.com/ultralisp/ultralisp)

[![](https://coveralls.io/repos/github/ultralisp/ultralisp/badge.svg?branch=coveralls)](https://coveralls.io/github/ultralisp/ultralisp?branch=coveralls)

# What is this?

This is a fast-moving Common Lisp software distribution for those who
want to publish his/her software today instead of waiting for the next
month.

# How to use it?

To use it, open your Lisp REPL and eval:

```lisp          
(ql-dist:install-dist "http://dist.ultralisp.org/" :prompt nil)
```

Pay attention, that Quicklisp's client does not support HTTPS yet.
Vote for [this issue](https://github.com/quicklisp/quicklisp-client/issues/167) on the
GitHub, to increase priority for this feature.
""")
