(uiop:define-package #:ultralisp-docs/architecture
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
(in-package #:ultralisp-docs/architecture)

(in-readtable pythonic-string-syntax)


(defsection @architecture (:title "Architecture"
                           :ignore-words ("ASDF"
                                          "S3"
                                          "RPC"))
  "
# Components of the system

Ultralisp consists of many moving parts:

![](docs/images/architecture.png)

All components running in the Digital Ocean are running as Docker containers.

## MainApp

MainApp implements most logic:

- It returns web pages of Ultralisp.org (the Reblocks framework is used).
- It runs a cron-like process for such regular tasks as:
  * Checking of new code for all registered projects.
  * Building new versions of the distribution.
  * Updating a search index in ElasticSearch.

New code checks requires to load asd files and to check if each asd system is loadable. To make this process
more secure, MainApp sends request for a check to the Gearman server and Worker receives this request, then
performs a check and returns result back to the MainApp. During the check Worker has only a read-only access to the database.
After the each check, container with Worker dies to start the next check from a scratch.

Periods between project checks by cron are calculated with respect to the distance between recent commits in the project's repository.
Also a project owner may setup a web hook and the project will be updated as soon as possible after the restart.

## Worker

Worker app just listens to the Gearman server and waits for a command to check the project. In production this container dies after the each check. For convenience of development it is possible to make many checks in the same process.

For each project's source Ultralisp remebers the git commit used to build the most recent release. Worker fetches new commits and see if the last one is different from the commit hash stored in the database. If it is differ, then Worker searches for all asd files, loads them and returns to the MainApp a new commit hash and a list of ASDF systems.

After the successful check, MainApp creates a new revision of the project's check and a binds it to a new version of the distribution. Then MainApp builds a new distrubution by downloading a new source of the project, archiving it and uploading to the Amazon S3 (for development a local storage is used instead).

## Gearman

This component is an opensource RPC server, it works like a proxy between MainApp and Worker. This way we can scale worker and run a multiple instances to speed up the checking some day.

To run Gearman we are using [this](https://github.com/artefactual-labs/docker-gearmand) docker image from Artefactual Labs.

## FileStorage

Amazon S3 is used as a storage for all project releases and dist metadata. CloudFlare proxies all requests to the https://dist.ultralisp.org/ into the Amazon S3 bucket.

During development all releases and metadata are saved into the local filesystem and can MainApp can serve them as static files.

")


;; DataModel
