#!/bin/bash

set -e
set -x

if [[ "$MODE" == "worker" ]]; then
    s6-svscan /etc/s6
    # /app/worker \
    #     --interface "0.0.0.0" \
    #     --port "10100" \
    #     --one-task-only \
    #     --slynk-interface "0.0.0.0" \
    #     --slynk-port "4005" \
    #     | tee -a /app/logs/worker.log \
    #     | jsail
else
    /app/ultralisp-server \
        | tee -a /app/logs/app.log \
        | jsail
fi
