(uiop:define-package #:ultralisp-docs/dev
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
(in-package #:ultralisp-docs/dev)

(in-readtable pythonic-string-syntax)


(defsection @dev (:title "Development"
                  :ignore-words ("TCP"
                                 "SLY"))
  "
# Running in dev

## Easy way

To run all components in docker containers, just run:

```
docker compose up
```

It will start 5 docker containers with names started with `ultralisp_`. Web application will be running on 8080 TCP port.

You can connect to the lisp parts of the Ultralisp using SLY:

- MainApp is running on 14005
- Worker is running on 14006

Other services are also export some ports. See the full list in the common-services.yml file.

")
