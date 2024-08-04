===========
 Ultralisp
===========



How to host Ultralisp on my own server
======================================

The easiest way to start a local Ultralisp server is to use
``docker-compose``.

Checkout the repository::

  git clone https://github.com/ultralisp/ultralisp
  cd ultralisp

And run::

  docker-compose run --rm mito migrate
  docker-compose up app

**Note, you have at least 4G of RAM on your machine, to run all services, needed for Ultralisp!**


Harder way
----------

You may also build docker images out of Dockerfile and run them manually like this::

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


Hacking around
==============

During development, it is better to start docker compose with the following
arguments::

  docker-compose run --rm mito migrate
  
  docker-compose up --build --abort-on-container-exit app

Most such commands are defined in the ``Lakefile``. Use `lake`_ to run
it like that::

  lake devserver

Then you can connect to the web sever and worker using SLY. Just run in
the Emacs a command ``sly-connect``, choose "127.0.0.1" as  a hostname
and ``14005`` as a port for webserver or ``14006`` as  a port to connect
to the worker.

To work in the REPL, you will need a connection to a database. Establish it
by running ``(ultralisp/db:connect-toplevel)``.


Running and creating database migrations
----------------------------------------

To generate a new database migration, run::

  docker-compose rm --stop --force empty-postgres
  docker-compose run --rm mito generate-migration

To rollup all migration to a dev database, run::

  docker-compose run --rm mito migrate

If you want to experiment with database and then rollback the database's
state then create a dump with such command::

  docker-compose run --rm db-ops dump

And when you want to restore the database's state, ensure that ``app``
and ``worker`` containers are not running and run::

  docker stop ultralisp_app ultralisp_worker
  docker-compose run --rm db-ops restore


Running tests
-------------

Install Postgres database, then create a test user and database:

.. code:: shell

   sudo -u postgres psql -c "CREATE USER ultralisp_test WITH PASSWORD 'ultralisp_test'"
   sudo -u postgres psql -c "CREATE DATABASE ultralisp_test OWNER = 'ultralisp_test'"
   sudo -u postgres createdb --owner ultralisp_test ultralisp_test

Connect to the REPL and run:

.. code:: common-lisp

   (ql:quickload :ultralisp-test)
   (setf (uiop:getenv "POSTGRES_USER") "ultralisp_test")
   (setf (uiop:getenv "POSTGRES_DBNAME") "ultralisp_test")
   (setf (uiop:getenv "POSTGRES_PASS") "ultralisp_test")
   (setf (uiop:getenv "POSTGRES_HOST") "localhost")
   
   (setf rove:*enable-colors* nil)
   (setf rove:*debug-on-error* t)
   (asdf:test-system :ultralisp-test)

Interesting environment variables
---------------------------------

* ``HIDE_SEARCH`` - if you set it, then search bar will not render.
  But this does not disables projects indexing.
* ``CRON_DISABLED`` - turn off all cron jobs like project chechking,
  new version builds etc. Probably, we should create an admin page
  to perform these actions manually.


Hosting Ultralisp on your own server
====================================

This should a big chapter of documentation but for now there is only a sketch.

Uploading distribution to S3
----------------------------

By default, Ultralisp stores data locally and serves it from the
``/dist/`` folder, like that:
``http://my-ultralisp.org/dist/``. Hovewer, you may want to upload the
data to Amazon S3 and to serve it through something like Cloudflare.

To do this, you need to set these environment variables for ultralisp
app:

* ``UPLOADER_TYPE=s3``
* ``S3_BUCKET=dist.my-ultralisp.org``
* ``AWS_ACCESS_KEY_ID=*****``
* ``AWS_SECRET_ACCESS_KEY=*****``
* ``BASE_URL=http://dist.my-ultralisp.org/`` - a URL of the server which will
  serve the files. In simplest case, you would just point to S3 server
  like that: https://s3.amazonaws.com/dist.my-ultralisp.org/ but right
  now this will not work because Quicklisp does not support HTTPS :(

And you need to create a bucket on the S3.

How to create a bucket
~~~~~~~~~~~~~~~~~~~~~~

* Go to the AWS console: https://s3.console.aws.amazon.com/s3/home
* Press https://s3.console.aws.amazon.com/s3/home?region=us-east-1#
* Set you bucket's name such as a domain, like ``dist.my-ultralisp.org``
* On a tab "Set permissions" remove ticks from these items:

  * Block new public ACLs and uploading public objects
  * Remove public access granted through public ACLs
  * Block new public bucket policies
  * Block public and cross-account access if bucket has public policies

* When the bucket is created, go to the buckets policy page which should have
  an url like that: https://s3.console.aws.amazon.com/s3/buckets/dist.my-ultralisp.org/?region=us-east-1&tab=permissions
  and insert such code into the "Bucket Policy" tab::

    {
      "Id": "Policy1547940357563",
      "Version": "2012-10-17",
      "Statement": [
        {
          "Sid": "Stmt1547940349039",
          "Action": [
            "s3:GetObject"
          ],
          "Effect": "Allow",
          "Resource": "arn:aws:s3:::dist.my-ultralisp.org/*",
          "Principal": "*"
        }
      ]
    }

  This will make this bucket readable to anybody.

Setup a proxy server
~~~~~~~~~~~~~~~~~~~~

You need a proxy or CDN which is able to serve data via plain HTTP,
because Quicklisp client does not support HTTPS yet. There is an
`issue <https://github.com/quicklisp/quicklisp-client/issues/167>`_ on
the GitHub, please, vote for it.

I use Cloudflare because it is free and easy to setup.

To serve files via Cloudflare, turn on "Static website hosting" of the
bucket at AWS. Set the "index document" as "ultralisp.txt".

After that, your quicklisp distribution will be available as http://dist.ultralisp.org.s3-website-eu-west-1.amazonaws.com


.. _lake: https://github.com/takagi/lake

# TODO: make ansible work
