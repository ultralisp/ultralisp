FROM 40ants/base-lisp-image:0.6.0-ccl-bin

EXPOSE 80
EXPOSE 4005

COPY qlfile qlfile.lock app-deps.asd /app/
RUN install-dependencies

# These a dev dependencies to simplify log reading and support
# file search from remote Emacs.
RUN apt-get update && \
    apt-get install -y \
            python-pip \
            silversearcher-ag && \
    pip install jsail

COPY . /app

CMD /app/entrypoint.sh
