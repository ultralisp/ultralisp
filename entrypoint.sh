#!/bin/bash

set -e
set -x

export CL_SOURCE_REGISTRY=/common-lisp

env | sort
ls -l /common-lisp/
#~/.roswell/bin/qlot exec /app/roswell/ultralisp-server.ros $@ 2>&1 | tee /server.log
~/.roswell/bin/qlot exec /app/roswell/ultralisp-server.ros $@
