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
                                 "URL"
                                 "SLY"))
  "
# Running in dev

## Easy way

To run all components in docker containers, just run:

```
dev:~$ docker compose up
```

It will start 5 docker containers with names started with `ultralisp_`. Web application will be running on 8080 TCP port.

You can connect to the lisp parts of the Ultralisp using SLY:

- MainApp is running on 14005
- Worker is running on 14006

Other services are also export some ports. See the full list in the common-services.yml file.

# Harder way

In hard mode we will build need componens manually. In this case you will have Roswell and Qlot installed on your machine.
I know, somebody might tell me they don't want to use these tools too, but I'm too lazy to manage my Lisp installations manually and Qlot allows to make builds almost hermetic (at least in terms of lisp dependencies).

## Database setup

by default, app tries to connect to the postgresql running on the same host to a database `ultralisp` as user `ultralisp` with password `ultralisp`. if there is no such database, it will quit.

let's install local postgres and create such database:

```
dev:~$ sudo apt-get install postgresql-14
dev:~$ sudo systemctl start postgresql

dev:~$ sudo su - postgres

postgres@dev:~$ createuser --interactive --pwprompt
enter name of role to add: ultralisp
enter password for new role:
enter it again:
shall the new role be a superuser? (y/n) n
shall the new role be allowed to create databases? (y/n) n
shall the new role be allowed to create more new roles? (y/n) n

postgres@dev:~$ createdb -o ultralisp ultralisp
```

**Note:** if you want, you can use another login, dbname and password. in this case you'll have to pass them to the webserver as `postgres_host`, `postgres_user`, `postgres_pass`, `postgres_dbname` environment variables.

now we need to fill the database with a schema:

```bash
dev:~$ qlot exec ros install mito

dev:~$ qlot exec .qlot/bin/mito \
            migrate --type postgres \
                    --host localhost \
                    --database ultralisp \
                    --username ultralisp \
                    --password ultralisp
```


## Building and running lisp components

To do this kind of installation, we need to build our main web server and a worker. Do it like this:

```
dev:~$ qlot install --no-deps
```

This command will create a `.qlot` directory with local environment.

Then run the build:

```
dev:~$ qlot exec ros build roswell/ultralisp-server.ros
dev:~$ qlot exec ros build roswell/worker.ros
```

After that, we can run our webserver:

```
dev:~$ mkdir dist

dev:~$ DEBUG=1 \
       PORT=8001 \
       INTERFACE=localhost \
       SLYNK_PORT=14005 \
       CRON_DISABLED=yes \
       DIST_DIR=`pwd`/dist \
       BASE_URL=http://localhost:8001/dist/ \
       GITHUB_CLIENT_ID=0bc769474b14267aac28 \
       GITHUB_SECRET=3f46156c6bd57f4c233db9449ed556b6e545315a \
       roswell/ultralisp-server --log-dir `pwd`/logs/
```

**Note:** pay attention to the client id and github secret. These are values I've created for a dev version Ultralisp server, hosted on localhost. If yor want to create a self-hosted installation, you'll have to create your own GitHub app and to keep it's secrets in some secure vault.

Now open URL http://localhost:8001/ in the browser! It will show the front page of your dev Ultralisp server!
")
