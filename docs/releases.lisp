(uiop:define-package #:ultralisp-docs/releases
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
(in-package #:ultralisp-docs/releases)

(in-readtable pythonic-string-syntax)


(defsection @releases (:title "How to release new Ultralisp version"
                       :ignore-words ("SBCL"))
  """
Ultralisp releases are performed automaticall using [build and deploy](https://github.com/ultralisp/ultralisp/blob/master/.github/workflows/build-and-deploy.yml) workflow:

It calls Ansible to deploy a new version.

Ansible configs are stored in the [.ansible](https://github.com/ultralisp/ultralisp/tree/master/.ansible) folder:

# Making a new release

1. To deploy a new release, you have to update a Changelog.rst and add a new section
to it.
2. After Changelog.rst was updated in the master branch, [autotag](https://github.com/ultralisp/ultralisp/blob/master/.github/workflows/autotag.yml) workflow will create a new version git tag on the last commit.
3. [Build and deploy](https://github.com/ultralisp/ultralisp/blob/master/.github/workflows/build-and-deploy.yml) workflow will be triggered by a new git tag and perform the release.

To check if the new version was deployed successfully, you can open <https://ultralisp.org> and scroll to the footer. The software version is stated in the footer's text. Also, if you will hold a mouse cursor over the version number, a popup will be opened with additional information about current uptime, SBCL version and some other information.

""")
