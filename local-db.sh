#!/bin/bash


if [[ "$1" == "ro" ]]; then
    psql 'host=localhost port=5432 user=ultralisp_ro password=ultralisp_ro dbname=ultralisp'
else
    psql 'host=localhost port=5432 user=ultralisp password=ultralisp dbname=ultralisp'
fi
