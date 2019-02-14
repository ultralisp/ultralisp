===========
 ChangeLog
===========

0.4.0 (Unreleased)
==================

* Added a function ``ultralisp/import:main`` to import from Quicklisp
  all projects, hosted on the GitHub.
* Fixed the way how errors during project checks are processed. Now a
  project with error will be disabled and check will have a traceback in
  it's ``traceback`` slot.
* Fixed the issue with leaving checked out repositories in the
  ``/tmp/checking`` directory after the repository was checked for
  updates.
* If project was disabled because an error, now you can view a traceback.

0.3.1 (2019-02-08)
==================

* Now GitHub integration requires less permissive permissions.
  Previosly it required read/write access to all repositories, now site
  will have access for writing webhooks and reading your organizations
  list. It will be used to show you public repositories from these organizations.
* Removed loading of spin.js, because it is unnecessary.
* Fixed auto refreshing for GitHub repositories list.

0.3.0 (2019-02-07)
==================

* Added ability to add GitHub project just by URL.

0.2.0 (2019-01-29)
==================

* Now tracked projects are stored in a postgresql database.
* They can be added in few clicks from the GitHub.
* Users can login using their emails only.
* And many other changes.

0.1.7
=====

* Dependencies were updated to switch off Ultralisp dist temporarily and
  use fixed quickdist. Because current Ultralisp distribution is broken.

0.1.6
=====

* Switching to use dist.ultralisp.org for building docker image.
  Previously, some systems were fetched from the GitHub.

0.1.5
=====

* Move to a new ``quickdist`` which writes ``distinfo-template-url``
  into the metadata.

0.1.4
=====

* Fixed a rendering of yandex metrika code.

0.1.3
=====

* Added google analytics and yandex metrika counters support.

0.1.2
=====

* Fixed updating of the /projects/ subdirectory when it does not exists.
* Function `read-metadata` was fixed to ignore absence of the project.txt file and return nil.
