===========
 ChangeLog
===========

1.27.0 (2025-06-13)
===================

* Updated dependencies and code to use the latest Reblocks and other libs from the fresh Ultralisp dist.

1.26.11 (2025-06-12)
====================

* Fixed building of the docker image.

1.26.10 (2025-06-11)
====================

* Update S3 access key and LispWorks license.

1.26.9 (2025-04-13)
===================

* Fixed vulnerability when non-authenticated user was able to add and remove tags on projects.
  Reported by Charlie McMackin.

1.26.8 (2025-02-15)
===================

* Fixed the way how "processed" dist version are processed. Now each dist is processed in a separate transaction. Also, we collect garbage between dist builds.
* Fixed a login page broken after the last dependencies upgrade.

1.26.7 (2025-02-15)
===================

* Fixed build of worker under Lispworks.

1.26.6 (2025-02-14)
===================

* Normalize license info when collecting system info.
  Some authors may use symbol as license name like in:
  https://github.com/ajberkley/cl-binary-store/blob/3b0587eaaa74c79734477cb38132237411068ef8/cl-binary-store.asd#L66
  But we need to serialize it to JSON. Yason library fails to serialize symbols, so we cast license to string now.

1.26.5 (2024-11-30)
===================

* Fix startup scripts and their logging.

1.26.4 (2024-11-30)
===================

* Show more info about status of LW license application.

1.26.3 (2024-11-30)
===================

* Turned off memory metrics because they cause hanging of the server.
  Details are described at https://github.com/ultralisp/ultralisp/issues/288
* Updated LispWorks license once more.

1.26.2 (2024-11-19)
===================

Updated LispWorks license.

1.26.1 (2024-07-29)
============================

Now update of the dist will be made 10 times faster and hopefully will not trigger lock timeouts.

1.26.0 (2024-07-28)
============================

Switch to newer version of Quickdist where ``canonical-distinfo-url`` is added to the dist metadata file.

1.25.0 (2024-07-07)
===================

Update SBCL to 2.4.6.

1.24.9 (2024-06-28)
===================

Added a way to debug a deadlock in transactions.

1.24.8 (2024-04-16)
============================

* Update deps to solve problem introduced by `package rename <https://github.com/sionescu/bordeaux-threads/commit/666b5836f541c33e427eefd3ddf8809737e2ab33>`_ in the bordeaux-threads. This problem broke checks for some projects. Reported issue `#261 <https://github.com/ultralisp/ultralisp/issues/261>`_.
* Updated the list of sponsors and removed a link to the Liberapay, because it is useless.

1.24.7 (2024-03-01)
===================

* Handle SERIOUS-CONDITION instead of ERROR in DEFCRON macro. This should prevent server restarts in some cases.

1.24.6 (2024-03-01)
===================

* Reset random start when starting daemons.

1.24.5 (2024-02-28)
===================

* Added robots.txt file.
* More logging in the make-release function was added.

1.24.4 (2024-02-03)
===================

* Fixed issue #247, where process checking stuck trying to process `Fset <https://github.com/slburson/fset/tree/40859727fa6f93b72c2f2cfb70f314965cf0e06c>`_ which has two ``fset.asd`` files in different directories.
* Fixed manual adding of GitHub type source.

1.24.3 (2023-12-10)
===================

* Fixed a rare error when source check was created with "nil" string in lisp_implementation slot.

  Presumable, the problem occur when a check is created for the source not bound to any quicklisp distribution.

  Previously a check was created with "nil" in the lisp_implementation slot. Because of that, no worker was able to process such a check.

  Also, such check prevented creation of new correct checks because "check already exists" error.

  After the fix check will not be created in this case.

1.24.2 (2023-10-22)
===================

* Use deploy token to autotrigger builds.

1.24.1 (2023-10-22)
===================

* Fixed a typo in the footer.
* Added a note about using local ``clpmfile``.

1.24.0 (2023-10-22)
===================

* Use reCaptcha when user signs up using email to prevent spam bots.

1.23.2 (2023-10-01)
===================

* Using the latest Bordeaux Threads because the version pinned as `:ref 61e6e5645848e77367775610e4537043ea810f6d` does not work with latest Clack anymore - a web server just not starting.

1.23.1 (2023-10-01)
===================

* Use patched Legit, to make it work on LispWorks.

1.23.0 (2023-10-01)
===================

* Move from Mailgun to Resend for sending emails.

1.22.6 (2023-09-02)
===================

* Fix error which occur when author or maintainer in ASD files is quoted by a mistake.

1.22.5 (2023-08-20)
===================

* More fixes to json parsing.

1.22.4 (2023-08-20)
===================

* Fixed error on the frontpage caused by incompability of Jonthan to parse JSON
  including strings with accents. I've switched to Yason at this place.
* Fixed rendering of total projects count on the front page.

1.22.3 (2023-08-16)
===================

* Fixed type declaration in the yet another place.

1.22.2 (2023-08-16)
===================

* Fixed type of release-info slot and corresponding dist build issue.

1.22.1 (2023-08-15)
===================

* Slots systems-info and release-info of source have got their types.
  This should fix their representation in the API.

1.22.0 (2023-08-13)
===================

* Added such fields as author, maintainer, license, description and long description to source's asdf system objects.

1.21.0 (2023-08-13)
===================

* Added API method get-project-sources. This way you can learn information about sources, their systems and release_info.

1.20.5 (2023-08-11)
===================

* Fixed issue happened when you adding a new project from GitHub by selecting it from the list.

1.20.4 (2023-08-10)
===================

* Removed debug form.

1.20.3 (2023-08-10)
===================

* Fixed creationg of new Git projects from URL.
* Error page and logs now have "request-id" field to simplify debugging.

1.20.2 (2023-08-09)
===================

* Added more API methods: ``get-project-systems``, ``get-all-tags``.
* API method ``search-symbol`` was renamed to ``search-symbols``.

1.20.1 (2023-08-09)
===================

* Fixed pagination argument in the API methods.

1.20.0 (2023-08-08)
===================

* OpenRPC API was added with following methods:

  - ``get_projects_by_tag``
  - ``get_project_tags``
  - ``get_project_by_name``
  - ``search_symbol``

1.19.3 (2023-08-06)
===================

* Mailgun's API key was updated.

1.19.2 (2023-08-06)
===================

* Update certificate chain to make git work with SourceHut.

1.19.0 (2023-08-06)
===================

* An email authentication and generic git repositories support were added.

1.18.3 (2023-03-26)
===================

* Switch from broken Hunchentoot Clack handler to Woo.

1.18.2 (2023-03-26)
===================

* Fix starting Slynk inside the docker.

1.18.1 (2023-03-25)
===================

* Updated code to work with recent Reblocks version.

1.18.0 (2023-03-21)
===================

* Update Qlot dependencies in attempt to fix this nasty error occured when checking many projects:

  .. code::
     
     Condition: export cffi-sys::defcfun-helper-forms causes name-conflicts in
            #<package "CFFI"> between the following symbols:
              cffi-sys::defcfun-helper-forms, cffi::defcfun-helper-forms

  This become a problem because new CFFI, available in Ultralisp has been refactored and symbol
  was moved to another package.

  In old Ultralisp lisp image this symbol is in the CFFI package:

  .. code::

     CL-USER> (find-symbol "DEFCFUN-HELPER-FORMS" (find-package "CFFI"))
     CFFI::DEFCFUN-HELPER-FORMS
     :INTERNAL

     CL-USER> (find-symbol "DEFCFUN-HELPER-FORMS" (find-package "CFFI-SYS"))
     NIL
     NIL


1.17.7 (2022-12-24)
===================

* Yet another Qlot update, now with fixed Spinneret.

1.17.6 (2022-12-24)
===================

* Updated dependencies in Qlot.

1.17.5 (2022-12-24)
===================

* More logging on LW license application.

1.17.4 (2022-12-24)
===================

* Added new LispWorks lincese into the vault.

1.17.3 (2022-12-24)
===================

* Disabled "fresh quicklisp dist on checking" for Lispworks distribution,
  because qlot:install does not work under LispWorks as expected.

1.17.2 (2022-12-23)
===================

* Rebuild to update LispWorks licens in the worker.

1.17.1 (2022-11-12)
===================

* Fixed error caused every project disabling because some external-url method is missing.

1.17.0 (2022-11-11)
===================

* Now each project check will be made against latest versions of
  other libraries from it's dist.

1.16.14 (2022-10-16)
====================

* When chack is failed, we'll commit it in a separate DB connection,
  to prevent accidental transaction rollback.

1.16.13 (2022-10-16)
====================

* Added 15 minutes timeout on each RPC command.
* Extra logging of archive uploading errors.

  I suspect that sometimes S3 responds with:
  ``ZS3:SLOW-DOWN: SlowDown: Please reduce your request rate.``
  error and projects check hangs.

1.16.12 (2022-09-25)
====================

* Docker entry point now creates /app/logs directory
  if it does not exist. Also, now we are using the same
  s6 run files both in dev and in prod.

1.16.11 (2022-09-25)
====================

* Just rebuild for test purpose.

1.16.10 (2022-09-24)
====================

* Macro WITH-CONNECTION now signals error if we attempt to get cached connection when non-cached one is active.

  This should prevent errors where cl-dbi tries to commit or rollback connection which already closed:
  "DB Error: Connection to database server lost".

1.16.9 (2022-09-24)
===================

* Updated list of supporters at Patreon.

1.16.8 (2022-09-23)
===================

* Function fill-tags-for-all-projects now saves tags for each project in a separate transaction.
* Made a top level timeout for 16 minutes on a single project check. During this window,
  Ultralisp will make 3 attempts and each will be limited by 5 minutes timeout. If some
  project takes longer for compile or hangs, it will fail.
* Page of all tags now uses cached results.
* Fixed error on project page when project was not found in the database.

1.16.7 (2022-09-21)
===================

* Fixed dependencies in package inferred lisp files.

1.16.6 (2022-09-19)
===================

* Fixed dependencies installation. It is important to install them in order given at `app-deps`, but when they
  are installed using ASDF, seems their order is unpredictable.

1.16.0 (2022-09-18)
===================

* Now it is possible to tag projects. Also, tags are fetched from the GitHub automatically!

1.15.0 (2022-09-04)
===================

* First auto-deployed release!

1.14.3 (2022-09-04)
===================

* Now 5 minutes timeout is applied when we are uploading new package to S3.

1.14.2 (2022-08-31)
===================

* Added a timeout on git-clone-or-update because legit sometimes hangs forever.

1.14.1 (2022-03-31)
===================

* Just a new build to check building pipeline.

1.14.0 (2022-01-07)
===================

* Now LW worker runs under Docker.

1.13.1 (2022-01-04)
===================

* Fixed some errors of move to Reblocks.

1.13.0 (2022-01-04)
===================

* Move to Reblocks ASDF system.

1.12.0 (2022-01-04)
===================

* Rebuild with latest dependencies.

1.11.2 (2021-10-15)
===================

* Fixed forcing of the manual check.
* Fixed inclusion of the latest changed project into the CLPI.

1.11.1 (2021-10-14)
===================

Now Ultralisp uploads to S3 only changed part of the CLPI,
which makes updates much faster. Previously it took about 40 minutes
to upload the whole index.

1.11.0 (2021-10-10)
===================

Uploading a new version to S3 each time when
any dist got updated.

1.10.1 (2021-10-04)
===================

Preload ``DBD-POSTGRES`` to fix errors in worker.

1.10.0 (2021-09-29)
===================

Experimental CLPI index support.

1.9.12 (2021-09-21)
===================

Added a hack to ignore ASDF's compile errors caused bu
SBCL's package varience warnings.

1.9.11 (2021-09-19)
===================

Added more logging to worker.

1.9.10 (2021-09-19)
===================

Move to a newer cl-gearman where job retrying is available.

Also, ``number-or-disabled-sources`` metric was fixed.

1.9.9 (2021-09-14)
==================

Added cl-strings into app-deps.

1.9.8 (2021-09-12)
==================

Another release names fix.

1.9.7 (2021-09-12)
==================

Fixed temporary names in archive names. This bug was introduced in
the 1.9.6 release and release URLs were made indistinguishable:

.. code::
   
   Downloading http://dist.ultralisp.org/archive/1645/temp-CD1HFW64-20210908211649.tgz

1.9.6 (2021-09-03)
==================

Fixed a bug which lead to a broken dist version if
some project's source has moved from one GitHub user to another.

This closes issue https://github.com/ultralisp/ultralisp/issues/140

1.9.5 (2021-07-14)
==================

* Fixed the issue when we logged PG password in case of any error during connection.

1.9.4 (2021-07-12)
==================

* Fixed the way how worker's fatal errors are handled.
  Now if heap or memory was exhausted and worker crashed,
  it will retry the check 3 times and then mark it as failed.

* Switched to the newer dependencies and Quickdist 0.16.4
  where processing infinite reqursion and stack overflow
  were fixed for
  `lispbuilder-opengl-1-2 did <https://github.com/lispbuilder/lispbuilder/blob/b7df0f2f9bd46da5ff322427d4bc6e6eefbfa722/lispbuilder-opengl/lispbuilder-opengl-1-2.asd>`_ system.

* Fixed ``SELECT-BY-SQL`` calls, to work with latest (and broken Mito).
  Here is the `pull-request <https://github.com/fukamachi/mito/pull/101>`_ where Mito was fixed.
  After it will be merged, we can remove ``FIND-CLASS`` calls.

* Now ``WITH-CONNECTION`` macro reuses existing connection for nested calls in cached mode.
  This fixes tests broken after the Mito and other dependencies upgrade.

1.9.3 (2021-06-13)
==================

* Fixed lock between SBCL and LispWorks workers.

1.9.2 (2021-06-06)
==================

* Fixed showing of ``on/off`` switches on the ``/github`` page.

1.9.1 (2021-06-05)
==================

* A few fixes to run LispWorks worker on schedule.

1.9.0 (2021-06-03)
==================

* Added a way to create a Lispworks quicklisp distributions.
  However checks are runned manually for now.

1.8.4 (2021-03-21)
==================

* Fixed project check forcing.

1.8.3 (2021-03-21)
==================

* Now indexing task is rescheduled in case if there was
  no changes to any source.

1.8.2 (2021-03-20)
==================

* Fixed the case when worker wasn't able to finish project
  indexer because it was each time killed by OOM killer.
  
  Now the indexer will wait for 5 minutes and remove the job
  from the Gearman server.
* Also, Prometheus metrics were added to show number
  of indexed projects and a number of fails.

1.8.1 (2021-03-17)
==================

* Now "Check" button forces the project's check even if it's
  sources were not changed.

1.8.0 (2021-03-14)
=================

* Now distribution can include only one ASDF system with given name.

  Sources with conflicting ASDF systems will be disabled automatically
  and it will be impossible add duplicating systems again.

  However, you can include systems with the same name into different
  distributions.

1.7.0 (2021-03-13)
==================

* Added support for super-moderators. These people can edit
  any source or dist and tune ignore lists and other options.

1.6.0 (2021-03-13)
==================

* New feature! Now it is possible to specify
  a black list for the source. All ``*.asd`` files
  in the listed directories will be ignored.

  Also, this way you can ignore individual ``*.asd`` files.

  This feature allows to fix issues like:
  `this one <https://github.com/ultralisp/ultralisp/issues/117>`_.
  
* Now we show a list of systems, included into the latest
  source release.

1.5.2 (2021-03-12)
==================

* Removed code ``(setf dexador:*use-connection-pool* nil)``
  which caused descriptor leaks.
* Moved to newer Ultralisp dist where this warning was fixed::

      Please, switch to the ui-widget class, because widget was renamed to
      ui-widget and will be removed after 2020-06-01.

1.5.1 (2021-03-12)
==================

* Fixed building of Ultralisp. Ironclad from Ultralisp does
  not work with SBCL 2.1.2 :(

1.5.0 (2021-03-11)
==================

* Moved to a newer base docker image, based on Ubuntu Focal and SBCL 2.1.2.
* Fixed the problem which checking projects having something like
  ``(REQUIRE :sb-some-standard-module)`` in their ``:depends-on`` asdf definition.

1.4.2 (2021-03-09)
==================

* Fixed a way how project checking and dist building processes were synchronized.

  Previously, a single lock was used and it was aquired by process checking the
  sources during a long period of time. Sometimes this leads to a long periods
  of time when the server wasn't able to build a new dist version because of
  large amount of checks in the queue.

  Now, the lock is taken only during a single check and dist builder
  waits it for 4 minutes to have a chance to build the dist.
* Don't showing empty ``PENDING`` dist versions on the landing page.

1.4.1 (2021-03-07)
==================

* Indexer fixed. Now errors are processed correctly.
* Added a cron task to clear old documents from Elastic Search.
* Now date of the next source's check rendered relative
  to the current moment.

1.4.0 (2021-03-06)
==================

* Fixed Lisp symbol indexer.
* Fixed repeating of search results after a click to the "Load more" link.
  This closed issue https://github.com/ultralisp/ultralisp/issues/88

1.3.0 (2021-03-05)
==================

* Project's page now shows the date of the next check for each source.
* Also, date of the previous check is rendered more concise.

1.2.4 (2021-03-04)
==================

* Move to Quickdist 0.16.1 where fixed processing of systems with
  ``(:require :implementation-specific-module)``.

  This should fix build of systems like Serapeum:
  https://github.com/ultralisp/ultralisp/issues/101

1.2.3 (2021-03-03)
==================

* Moving to a fresher Ultralisp dist version, where
  ``log4cl-extras`` fixed for case when there is ``(setf some-func)``
  in a backtrace.

1.2.2 (2021-03-02)
==================

* Fixed the way how we are making cl-dbi's thread pool thread safe.
* Added a function ``to-prod-db``.
* Fixed an "Unhandled error" on a project's page when
  project was not found. Relates to:
  https://github.com/ultralisp/ultralisp/issues/105.
* Project search on ``/projects/user/name`` pages was made case insensitive.

1.2.1 (2021-03-01)
==================

* Updated dependencies, installed newer SLYNK
  and moved pull checks from CircleCI to GitHub Actions.

1.2.0 (2021-01-25)
==================

* Now build dates are humanized and show relatively to the current moment

  However, If you will point to the date and wait a few seconds, a tooltip with an absolute date will be rendered.

1.1.0 (2021-01-25)
==================

* Now sensitive values should not be logged in tracebacks.

1.0.4 (2021-01-13)
==================

* Ultralisp's version was updated to switch to a newer Weblocks where log4cl-extras is used.

1.0.3 (2021-01-13)
==================

* Fixed generation of the dist metadata.

  All dists, generated at 12 January
  are broken :(

1.0.2 (2021-01-12)
==================

* Return 404 for old style URLs like ``/versions/20210111123844``.

1.0.1 (2021-01-12)
==================

* Added a link to all user project from the page title.
* Fixed error on the page with user's projects.

1.0.0 (2021-01-10)
==================

* Major release, because massive backend changes vere introduced.

  Now user can create his own Quicklisp distributions and each
  project can have multiple sources.

  This way you can create an cutting edge distribution and stable distribution,
  which will include changes from the "release" branch.

  Also, database's schema was significantly changed. New Ultralisp
  tries to keep versions of most business objects such as a project,
  source and a distribution. This way we can track the history of each change
  and tell which projects were included in a particular version of a distribution.

0.17.4 (2020-10-08)
===================

* Added a version number into the footer.
* Fixed the sponsors page rendering with enabled ADBlock extension.
* Removed Black Brane Systems from sponsors page.

0.17.3 (2020-09-04)
===================

* Rolled back to the bordeaux-threads where wasn't defconstant.
  This should temproary fix the problem with worker.

0.17.2 (2020-09-03)
===================

* Fixed the problem with starting worker.

0.17.1 (2020-08-29)
===================

* Updated the sponsors list.

0.17.0 (2020-08-28)
===================

New features
------------

* Added the /my/projects page. It lists all projects where you are the moderator.
* Added ability to start cron jobs manually in dev mode.

Fixes
-----

* Cache github repositories list for 10 minutes instead of infinite amount of time.
* Fixed error when adding a repository in development mode.
* Fixed instructions how to use Ultralisp with Qlot.

0.16.0 (2020-07-18)
===================

* Switched to log4cl-extras and logging into the files.


0.15.1 (2020-06-14)
===================

* Fixed use of cl-dbi:execute, which now want parameters as a list.
* Don't log as unhandled errors lock timeouts from cron jobs.

  This makes log less cluttered with false error messages.

0.15.0 (2020-06-14)
===================

* Dependencies updated.
* Now more safe version of cl-dbi should be used and
  it should not corrupt a connection pool.
* Added a code to disable conflicting projects manually.

0.14.15 (2020-04-19)
====================

* Trying to fix checking of mmontone/ten project by adding
  a project path to ``asdf:*central-registry*``.
  

0.14.14 (2020-02-23)
====================

* Project's page performance was improved. Some unnecessary
  requests to the database were removed and now these pages
  show load in less than 300ms instead of 15 seconds.
* Moved to the ultralisp distribution 20200222213506 where
  cl-flamegraph was fixed.

0.14.13 (2020-02-22)
====================

Now Ultralisp requires lesser OAuth scopes.

Previosly, it requested a ``public_repo`` scope which gave a write permission
to all your public repositores.

Now Ultralisp will request two scopes:

* ``read:org`` - We need this scope to be able to list all your public repositories
  not only from your personal account but also from your organizations.
* ``admin:repo_hook`` - And this scope allows us to add a webhook which will process
  all new commits and rebuild the Ultralisp distribution including all new great
  changes in your projects!


Minor changes
-------------

* Front page loading was optimized and now it should load few times faster.
* Quicklisp was upgraded from 2019-12-27 to 2020-02-18.
* Ultralisp was upgraded from 20200121194004 to 20200222170508.
* Added ability to generate flamegraph data in special debug mode.


0.14.12 (2020-01-27)
====================

* Fixed an error with missing ``get-function-documentation`` method for ``call/cc`` function from ``cl-cont``.

0.14.11 (2020-01-26)
====================

* Now package extractor is limited in time. If it is unable to load system in 60 seconds,
  it returns no packages for this system. Such system will not be indexed and available
  for full text search.

  This protects Ultralisp from hanging on loading malformed systems.

0.14.10 (2020-01-25)
====================

Changes
-------

* Now stdout and stderr are written into the separate log files.
  And both of them aren't redirected to the Docker. If you are running
  Ultralisp in a Docker container, then mount a volume /app/logs to the host
  system, to get access to the logs.

Fixes
-----

* Fixed an error which interrupted the process of adding projects to the search index in case when
  some project's system raised an error during the quickload.
* Fixed an error during the fetching your github projects. Because of this error it was impossible
  to select a project from the github and to add it into the Ultralisp.

0.14.9 (2020-01-24)
===================

* Index projects every one minute instead of every five.

0.14.8 (2020-01-23)
===================

* Indexer was fixed to ignore empty lines from package extractor.

0.14.7 (2020-01-22)
===================

* Now we are indexing projects one by one each five minutes.
* Idexing each project in the nested transaction.
* Fixed reporting about a condition which is signaled when we were unable to aquire the DB lock.

0.14.6 (2020-01-21)
===================

* Fixed error on saving indexing status.

0.14.5 (2020-01-21)
===================

* Move to a newer ``cl-dbi``, with more checks after the manual commit or rollback.

0.14.4 (2020-01-20)
===================

* Fixed discovery of the ``ELASTIC_SEARCH_HOST`` variable. Previously, it was catched during
  the docker image building process.

0.14.3 (2020-01-20)
===================

* Upgrade to the latest Weblocks, where problem with ``x-forwarder-port`` parsing was solved.

0.14.2 (2020-01-19)
===================

* Creation of the main widget was fixed. Previosly broken by the refactoring.

0.14.1 (2020-01-19)
===================

* Elastic search host was made configurable through ``ELASTIC_SEARCH_HOST`` env variable.

0.14.0 (2020-01-16)
===================

* Added a full text search among all symbols from all systems known to Ultralisp.org

0.13.0 (2019-09-15)
===================

Fixes
-----

* Upgraded to a Quickdist version 0.16.0 where releases.txt metadata was fixed.

0.12.0 (2019-09-14)
===================

Changes
-------

* Quicklisp upgraded from 2019-05-21 to 2019-08-13.

Fixes
-----

* Upgraded to a new ``quickdist`` library, where issue
  `number 51 <https://github.com/ultralisp/ultralisp/issues/51>`_ was fixed.

  Because of this bug, all systems like ``slynk``, which have their asd files
  in nested subdirectories, were broken.


0.11.0 (2019-07-13)
===================

Changes
-------

* Added a cron task to remove checks processed more than a week ago.
* Now if no asd files were found for the project, it is disabled.
* Start only one worker process in development mode, and don't kill
  it after each task execution. This simplify debugging.
* Added a ``ultralisp/debug`` package and function ``check-project``
  which can be used for checking how project is processed by the pipeline.
  

Fixes
-----

* Fixed a title on a "user's projects" page. This closes issue #54.
* Now asd files are seached in the nested directories too. This closes issue #55,
  but probably some other projects will be broken because their nested asd files
  can't be loaded.


0.10.1 (2019-07-07)
===================

* Moving to a new ``quickdist`` to fix issue with distignoring all asd files.

0.10.0 (2019-07-07)
===================

* Fixed a way how error page is rendered.
* Fixed error in Prometheus handle, caused restarts.

0.9.1 (2019-06-30)
==================

* Function ``get-preparend-version`` was fixed to work with latest Mito.

0.9.0 (2019-06-29)
==================

* Ignoring some system files was supported, but they are hardcoded
  in the ``docker/.distignore`` file. This should allow to add a
  `numcl <https://github.com/numcl/numcl>`_ library.

  Thanks to Nikolai Matiushev for the pull request.

0.8.1 (2019-05-21)
==================

* Fixed error prevented enabling a project after it was disabled.

0.8.0 (2019-05-05)
==================

* Using gearman instead of lfarm, for remote task execution.

0.7.1 (2019-05-05)
==================

* Removed ``(break)`` causing server restarts from time to time.

0.7.0 (2019-05-03)
==================

* Added support for prometheus /metrics handle with basic metrics about SBCL's
  state and Ultralisp's entities.

0.6.0 (2019-04-19)
==================

* Authentication was refactored and now it use GitHub instead of
  codes, sent by email. But this is temprorary and more identity
  providers will be supported in future. To add them, we need to
  extend a ``weblocks-auth`` library.

0.5.4 (2019-03-30)
==================

* Don't require access to private GitHub repositories.

0.5.3 (2019-03-24)
==================

* Added a /sponsors page and a link to a company at Patreon.
  Thanks to Jean-Philippe Paradis for suggestion!

0.5.2 (2019-03-22)
==================

* Added a button for donations.

0.5.1 (2019-03-20)
==================

* Fixed error with renamed ``render-projects-list``.

0.5.0 (2019-03-20)
==================

Changes
-------

* Now disabled project will be checked by cron too. Except those project
  which were disabled manually by user.
* "Leave feedback" url now leads to the
  https://github.com/ultralisp/ultralisp/issues instead of
  https://github.com/ultralisp/ultralisp/issues/new to not require user
  to login into the GitHub. This partially closes issue #33.

Improvements
------------

* Added a helper function ``find-projects-with-conflicting-systems``
  which can be called manually after the import from the Quicklisp to
  detect conflicts in system's names.

  We'll need to automate this in regular checks and disable projects
  which introduce conflicts.
* Added a link to GitHub page on each project's page. This closes issue
  #7.
* Added a red ribbon "Fork me on GitHub" to the front page. This finally
  closes issue #33.
* Added a page with all project of a single author and links like:
  https://ultralisp.org/projects/Hexstream/ should work.
  Also, a title on the project name was modified and now includes a link
  to all projects by the author.

0.4.2 (2019-03-19)
==================

* Show at most 3 changed projects for each version on the landing.
* Fixed a bug with clicking on a newly added project or opening the
  newly added project.

0.4.1 (2019-03-17)
==================

* Fixed deletion of the .git directories.

0.4.0 (2019-03-17)
==================

* Added a function ``ultralisp/import:main`` to import from Quicklisp
  all projects, hosted on the GitHub.
* Fixed the way how errors during project checks are processed. Now a
  project with error will be disabled and check will have a traceback in
  it's ``traceback`` slot.
* Fixed the issue with leaving checked out repositories in the
  ``/tmp/checking`` directory after the repository was checked for
  updates.
* If project was disabled because an error, now you can view a
  traceback.
* Project's page now shows a description.
* Now package-inferred systems are supported.

0.3.2 (2019-02-26)
==================

* Fixed error in a cron which caused enormous number of checks created
  every minute.

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
