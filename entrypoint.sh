#!/bin/bash

set -e
set -x

~/.roswell/bin/qlot exec /app/roswell/ultralisp-server.ros 2>&1 | tee /server.log
