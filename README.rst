===========
 Ultralisp
===========

What is this?
=============

This is a fast-moving Common Lisp software distribution for those who
want to publish his/her software today instead of waiting for the next
month.

How to use it?
==============

To use it, open your Lisp REPL and eval:

.. code:: common-lisp
          
   (ql-dist:install-dist "http://dist.ultralisp.org/" :prompt nil)

Pay attention, that Quicklisp's client does not support HTTPS yet.
Vote for `this issue
<https://github.com/quicklisp/quicklisp-client/issues/167>`_ on the
GitHub, to increase priority for this feature.


How to host Ultralisp on my own server
======================================

Easy way
--------

Easiest way to start a local Ultralisp server is to use
``docker-compose``.

Checkout the repository::

  git clone https://github.com/ultralisp/ultralisp
  cd ultralisp

And run::

  docker-compose up app


Harder way
----------

Use a docker image. To may build it youself by runing ``make all`` or
use an image from the Docker Hub.

Then start a container like this::

  docker run --rm \
             --name ultralisp \
             -p 80:80 \
             -p 4005:4005 \
             -v `pwd`:/app \
             -e AWS_ACCESS_KEY_ID=$AWS_ACCESS_KEY_ID \
             -e AWS_SECRET_ACCESS_KEY=$AWS_SECRET_ACCESS_KEY \
             -e MAILGUN_DOMAIN=mg.ultralisp.org \
             -e MAILGUN_API_KEY=key-xxxxxxxxxxxxx \
             -e USER_AGENT=xxxxxxxxxxxxx \
             40ants/ultralisp:latest

 
Hacking around
==============

During development, it is better to start docker compose with following
arguments::

  docker-compose up --build --abort-on-container-exit app

Then you can connect to the web sever and worker using SLY. Just run in
the Emacs a command ``sly-connect``, choose "127.0.0.1" as  a hostname
and ``14005`` as a port for webserver or ``14006`` as  a port to connect
to the worker.

To work in REPL, you will need a connection to a database. Establish it
by running ``(ultralisp/db:connect-toplevel)``.


Running and creating database migrations
----------------------------------------

To generate a new database migration, run::

  docker-compose run --rm mito generate-migration

To rollup all migration to a dev database, run::

  docker-compose run --rm mito migrate

