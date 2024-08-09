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
                                          "AWS"
                                          "URL"
                                          "CDN"
                                          "S3"
                                          "HTTPS"
                                          "HTTP"
                                          "ACL"
                                          "RAM"))
  """
# Easy way

The easiest way to start a local Ultralisp server is to use
``docker compose``.

Checkout the repository:

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

You may also build docker images out of Dockerfile and run them manually like this:

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

# Uploading distribution to S3

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

## How to create a bucket

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
  and insert such code into the "Bucket Policy" tab:

    ```javascrip
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
    ```

  This will make this bucket readable to anybody.

## Setup a proxy server

You need a proxy or CDN which is able to serve data via plain HTTP,
because Quicklisp client does not support HTTPS yet. There is an
`issue <https://github.com/quicklisp/quicklisp-client/issues/167>`_ on
the GitHub, please, vote for it.

I use Cloudflare because it is free and easy to setup.

To serve files via Cloudflare, turn on "Static website hosting" of the
bucket at AWS. Set the "index document" as "ultralisp.txt".

After that, your quicklisp distribution will be available as http://dist.ultralisp.org.s3-website-eu-west-1.amazonaws.com

"""

  )
