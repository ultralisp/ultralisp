#!/bin/bash

set -e
set -x

if [[ "$MODE" == "worker" ]]; then
    s6-svscan /etc/s6
else
    /app/ultralisp-server \
        | tee -a /app/logs/app.log \
        | jsail
fi
