(uiop:define-package #:ultralisp-docs/self-hosting
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
(in-package #:ultralisp-docs/self-hosting)

(in-readtable pythonic-string-syntax)


(defsection @self-hosting (:title "How to host Ultralisp on my own server"
                           :ignore-words ("4G"
                                          "RAM"))
  "
# Easy way

The easiest way to start a local Ultralisp server is to use
``docker compose``.

Checkout the repository::

```bash
git clone https://github.com/ultralisp/ultralisp
cd ultralisp
```

And run:

```bash
docker compose run --rm mito migrate
docker compose up app
```

**Note, you have at least 4G of RAM on your machine, to run all services, needed for Ultralisp!**


# Harder way

You may also build docker images out of Dockerfile and run them manually like this::

```bash
docker run --rm \
           --name ultralisp \
           -p 80:80 \
           -p 4005:4005 \
           -v `pwd`:/app \
           -e AWS_ACCESS_KEY_ID=$AWS_ACCESS_KEY_ID \
           -e AWS_SECRET_ACCESS_KEY=$AWS_SECRET_ACCESS_KEY \
           -e RESEND_API_KEY=key-xxxxxxxxxxxxx \
           -e USER_AGENT=xxxxxxxxxxxxx \
           40ants/ultralisp:latest
```

See the full list of env variables need by each component in the `common-services.yaml` file.

Also, you might choose to build each component manuall. See the instruction in the ULTRALISP-DOCS/DEV::@DEV section.
"

  )
