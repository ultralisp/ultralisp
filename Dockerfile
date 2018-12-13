FROM 40ants/base-lisp-image:0.6.0-ccl-bin

EXPOSE 80
EXPOSE 4005

COPY qlfile qlfile.lock app-deps.asd /app/
RUN install-dependencies
RUN apt-get update && apt-get install -y python-pip && pip install jsail

COPY . /app

CMD /app/entrypoint.sh
