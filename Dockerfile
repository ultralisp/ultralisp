FROM 40ants/base-lisp-image:0.6.0-sbcl-bin as base

EXPOSE 80
EXPOSE 4005

ENV CC=gcc
COPY qlfile qlfile.lock app-deps.asd /app/
RUN install-dependencies

# These a dev dependencies to simplify log reading and support
# file search from remote Emacs.
RUN apt-get update && \
    apt-get install -y \
            python-pip \
            supervisor \
            silversearcher-ag \
            postgresql-client && \
    pip install jsail

COPY docker/worker-supervisord.conf /etc/supervisor/conf.d/worker.conf
COPY . /app

RUN ~/.roswell/bin/qlot exec ros build /app/roswell/worker.ros && mv /app/roswell/worker /app/worker
RUN ~/.roswell/bin/qlot exec ros build /app/roswell/ultralisp-server.ros && mv /app/roswell/ultralisp-server /app/ultralisp-server

ENTRYPOINT ["/app/docker/entrypoint.sh"]


# Next stage is for development only
FROM base as dev
RUN ros install 40ants/gen-deps-system
ENTRYPOINT ["/app/docker/dev-entrypoint.sh"]

# To run Mito commands
FROM dev as mito
RUN ros install svetlyak40wt/mito/add-host-argument

# https://medium.com/the-code-review/how-to-use-entrypoint-with-docker-and-docker-compose-1c2062aa17a2
ENTRYPOINT ["/app/docker/mito.sh"]


FROM postgres:10 as db-ops
COPY ./docker/dev-entrypoint.sh /entrypoint.sh
WORKDIR /
ENTRYPOINT ["/entrypoint.sh"]
