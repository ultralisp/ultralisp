#!/bin/bash

set -e
set -x

if [[ "$MODE" == "worker" ]]; then
    ~/.roswell/bin/qlot exec /app/roswell/worker.ros \
                        --interface "0.0.0.0" \
                        --port 10100 \
                        --slynk-interface "0.0.0.0" \
                        --slynk-port 4005 | jsail
else
    ~/.roswell/bin/qlot exec /app/roswell/ultralisp-server.ros | jsail
fi
