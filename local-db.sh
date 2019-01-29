#!/bin/bash


if [[ "$1" == "ro" ]]; then
    psql 'host=localhost port=5432 user=ultralisp_ro password=ultralisp_ro dbname=ultralisp'
    exit 0
fi

if [[ "$1" == "empty" ]]; then
    psql 'host=localhost port=5433 user=ultralisp password=ultralisp dbname=ultralisp'
    exit 0
fi

# default
psql 'host=localhost port=5432 user=ultralisp password=ultralisp dbname=ultralisp'

