#!/bin/bash

set -e
set -x

if [[ "$MODE" == "worker" ]]; then
    s6-svscan /etc/s6
else
    /app/ultralisp-server \
        > /app/logs/app-stdout.log \
        2> /app/logs/app-stderr.log
fi
