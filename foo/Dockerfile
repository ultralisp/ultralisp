FROM 40ants/base-lisp-image:0.15.0-sbcl-bin

RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y bind9-host
RUN cat /etc/hosts
RUN cat /etc/resolv.conf
RUN host beta.quicklisp.org
