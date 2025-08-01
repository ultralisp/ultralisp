FROM 40ants/base-lisp-image:0.22.0-sbcl-bin as base

EXPOSE 80
EXPOSE 4005

# These a dev dependencies to simplify log reading and support
# file search from remote Emacs.
# Ca-certificates is required to make git work with SourceHut (and probably
# some other git forges too).
RUN apt-get update && \
    apt-get install -y \
            python3-pip \
            silversearcher-ag \
            lsof \
            ca-certificates=20230311ubuntu0.22.04.1 \
            postgresql-client \
            git

RUN mkdir -p /tmp/s6 && cd /tmp/s6 && \
    git clone https://github.com/skarnet/skalibs && cd skalibs && \
    git checkout v2.10.0.2 && \
    ./configure && make install && cd /tmp/s6 && \
    git clone https://github.com/skarnet/execline && cd execline && \
    git checkout v2.8.0.0 && \
    ./configure && make install && cd /tmp/s6 && \
    git clone https://github.com/skarnet/s6 && cd s6 && \
    git checkout v2.10.0.2 && \
    ./configure --with-lib=/usr/lib/execline && make install && \
    cd / && rm -fr /tmp/s6

ENV CC=gcc
COPY qlfile qlfile.lock ultralisp-deps.asd /app/
RUN install-dependencies

COPY . /app
COPY ./docker/.distignore /root/.config/quickdist/
# END OF THE base


FROM base as lw-worker

ENV DEBIAN_FRONTEND=noninteractive
ENV DISPLAY=host.docker.internal:0

RUN apt-get update && apt-get install -y libgtk2.0-0 curl

COPY dist-lw80 /lispworks

RUN cd /lispworks && \
    touch "/tmp/."`/bin/date '+%d%m%y'`"lispworks"`id -u` && \
    sh lwl-install.sh && \
    cd /usr/local/lib64/LispWorks && \
    ln -s /usr/local/lib64/LispWorks/lispworks-8-0-0-amd64-linux /usr/local/bin/lispworks

RUN curl https://beta.quicklisp.org/quicklisp.lisp > /quicklisp.lisp

RUN docker/lw-build.sh /app/lw-build.lisp /app/lw/license

COPY ./docker/s6-lw-worker /etc/s6
ENTRYPOINT ["s6-svscan", "/etc/s6"]

# END OF THE lw-worker


FROM base as sbcl-app

RUN rm -fr /app/lw

RUN qlot exec ros build \
    /app/roswell/ultralisp-server.ros && \
    mv /app/roswell/ultralisp-server /app/ultralisp-server

COPY ./docker/s6-app /etc/s6
ENTRYPOINT ["s6-svscan", "/etc/s6"]
# END OF THE sbcl-app


FROM base as sbcl-worker

RUN rm -fr /app/lw

RUN qlot exec ros build \
    /app/roswell/worker.ros && \
    mv /app/roswell/worker /app/worker
# If we don't do this, then Qlot will fail with this error:
#
# > The loader tried loading the symbol named RETRY-REQUEST
# > into the package named DEXADOR.RESTARTS, but the package
# > did not get defined, and does not exist.
RUN rm -fr ~/.cache/common-lisp/

COPY ./docker/s6-worker /etc/s6
ENTRYPOINT ["s6-svscan", "/etc/s6"]
# END OF THE sbcl-worker


# Next stage is for development only
FROM base as dev
RUN qlot exec ros install 40ants/gen-deps-system
ENTRYPOINT ["/app/docker/dev-entrypoint.sh"]


# To run Mito commands
FROM dev as mito
RUN qlot exec ros install fukamachi/mito

# https://medium.com/the-code-review/how-to-use-entrypoint-with-docker-and-docker-compose-1c2062aa17a2
ENTRYPOINT ["/app/docker/mito.sh"]


FROM postgres:10 as db-ops
COPY ./docker/dev-entrypoint.sh /entrypoint.sh
WORKDIR /
ENTRYPOINT ["/entrypoint.sh"]
