#!/bin/bash

set -e

COMMAND=$1
COMMON_OPTS="--type postgres --database ultralisp --username ultralisp --password ultralisp"


# Apply all migrations to an empty database and generate a new one
if [[ "$1" == "generate-migration" ]]; then
    qlot exec mito migrate \
         $COMMON_OPTS \
         --host empty-postgres
    
    qlot exec mito generate-migrations \
         $COMMON_OPTS \
         --host empty-postgres \
         --system ultralisp
    
    exit 0
fi

# Apply migrations to a dev database
if [[ "$1" == "migrate" ]]; then
    qlot exec mito migrate \
         $COMMON_OPTS \
         --host postgres
    exit 0
fi

echo "Available subcommands:"
echo ""
echo " - generate-migration - creates a new migration in db/migrations."
echo " - migrate - applies migrations to a development database."
