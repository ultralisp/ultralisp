#!/bin/bash

set -e
set -x

if [[ "$MODE" == "worker" ]]; then
    ~/.roswell/bin/qlot exec /app/roswell/worker.ros | jsail
else
    ~/.roswell/bin/qlot exec /app/roswell/ultralisp-server.ros | jsail
fi
