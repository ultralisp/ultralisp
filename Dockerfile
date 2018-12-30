FROM 40ants/base-lisp-image:0.6.0-ccl-bin as base

EXPOSE 80
EXPOSE 4005

COPY qlfile qlfile.lock app-deps.asd /app/
RUN install-dependencies

# These a dev dependencies to simplify log reading and support
# file search from remote Emacs.
RUN apt-get update && \
    apt-get install -y \
            python-pip \
            silversearcher-ag \
            postgresql-client && \
    pip install jsail

COPY . /app

ENTRYPOINT ["/app/docker/entrypoint.sh"]


# Next stage is for development only
FROM base as dev

RUN ros install 40ants/gen-deps-system

# To run Mito commands
FROM dev as mito
RUN ros install svetlyak40wt/mito/add-host-argument

# https://medium.com/the-code-review/how-to-use-entrypoint-with-docker-and-docker-compose-1c2062aa17a2
ENTRYPOINT ["/app/docker/mito.sh"]
