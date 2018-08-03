FROM 40ants/base-lisp-image:0.4.0-ccl-bin

EXPOSE 80
EXPOSE 4005

COPY qlfile qlfile.lock app-deps.asd /app/
RUN install-dependencies

COPY . /app

CMD /app/entrypoint.sh
