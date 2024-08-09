(uiop:define-package #:ultralisp-docs/hacking
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
(in-package #:ultralisp-docs/hacking)

(in-readtable pythonic-string-syntax)


(defsection @hacking (:title "Hacking around"
                      :ignore-words ("REPL"
                                     "SLY"))
  """
During development, it is better to start docker compose with the following
arguments:

```bash
docker-compose run --rm mito migrate
  
docker-compose up --build --abort-on-container-exit app
```

Most such commands are defined in the `Lakefile`. Use [lake](https://github.com/takagi/lake) to run
it like that:

```bash
lake devserver
```

Then you can connect to the web sever and worker using SLY. Just run in
the Emacs a command `sly-connect`, choose "127.0.0.1" as  a hostname
and `14005` as a port for webserver or `14006` as  a port to connect
to the worker.

To work in the REPL, you will need a connection to a database. Establish it
by running `(ultralisp/db:connect-toplevel)`.


# Running and creating database migrations

To generate a new database migration, run:

```bash
docker-compose rm --stop --force empty-postgres
docker-compose run --rm mito generate-migration
```

To rollup all migration to a dev database, run:

```bash
docker-compose run --rm mito migrate
```

If you want to experiment with database and then rollback the database's
state then create a dump with such command:

```bash
docker-compose run --rm db-ops dump
```

And when you want to restore the database's state, ensure that `app`
and `worker` containers are not running and run:

```bash
docker stop ultralisp_app ultralisp_worker
docker-compose run --rm db-ops restore
```

# Running tests


Install Postgres database, then create a test user and database:

```bash
sudo -u postgres psql -c "CREATE USER ultralisp_test WITH PASSWORD 'ultralisp_test'"
sudo -u postgres psql -c "CREATE DATABASE ultralisp_test OWNER = 'ultralisp_test'"
sudo -u postgres createdb --owner ultralisp_test ultralisp_test
```

Connect to the REPL and run:

```lisp
(ql:quickload :ultralisp-test)
(setf (uiop:getenv "POSTGRES_USER") "ultralisp_test")
(setf (uiop:getenv "POSTGRES_DBNAME") "ultralisp_test")
(setf (uiop:getenv "POSTGRES_PASS") "ultralisp_test")
(setf (uiop:getenv "POSTGRES_HOST") "localhost")
   
(setf rove:*enable-colors* nil)
(setf rove:*debug-on-error* t)
(asdf:test-system :ultralisp-test)
```

# Interesting environment variables

* `HIDE_SEARCH` - if you set it, then search bar will not render.
  But this does not disables projects indexing.
* `CRON_DISABLED` - turn off all cron jobs like project chechking,
  new version builds etc. Probably, we should create an admin page
  to perform these actions manually.

"""

  )
