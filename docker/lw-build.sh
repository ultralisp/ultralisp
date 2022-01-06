#!/bin/bash

DELIVERY_FILE=${1:-/app/deliver.lisp}
LICENSE_FILE=${2:-/lw-secrets}

set -xe

if [[ ! -e ${LICENSE_FILE} ]]; then
    echo "Please, COPY file ${LICENSE_FILE} with content like:"
    echo '--lwlicenseserial LWHDV-????-????-????-???? --lwlicensekey ????-????-????-??????'
    exit 1
fi

if [[ ! -e ${DELIVERY_FILE} ]]; then
    echo "Please, COPY file ${DELIVERY_FILE} into the docker image"
    exit 2
fi


if [[ -e /usr/local/lib64/LispWorks/lispworks-8-0-0-amd64-linux ]]; then
    LW_BINARY=/usr/local/lib64/LispWorks/lispworks-8-0-0-amd64-linux
elif [[ -e /usr/local/lib64/LispWorks/lispworks-7-1-0-amd64-linux ]]; then
    LW_BINARY=/usr/local/lib64/LispWorks/lispworks-7-1-0-amd64-linux
else
    echo 'Something went wrong - there is no LW binary.'
    exit 3
fi

${LW_BINARY} $(cat ${LICENSE_FILE})
${LW_BINARY} -build ${DELIVERY_FILE}
