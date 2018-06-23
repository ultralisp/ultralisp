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
