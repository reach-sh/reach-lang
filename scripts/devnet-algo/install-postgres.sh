#!/usr/bin/env bash
set -e

apk add --no-cache postgresql-jit

export PGDATA="$POSTGRES_DATA"
mkdir -p "$PGDATA" /run/postgresql
chown postgres:postgres "$PGDATA" /run/postgresql
su postgres -c "initdb --username='$POSTGRES_USER' --pwfile=<(echo '$POSTGRES_PASSWORD')"
su postgres -c postgres &
PID="$!"

bash /wait-for-postgres.sh

su postgres -c "psql -v ON_ERROR_STOP=1 --username '$POSTGRES_USER' --no-password --no-psqlrc --dbname postgres  --set db='$POSTGRES_DB' --tuples-only" <<- EOF
    CREATE DATABASE :"db" ;
EOF

kill "$PID"
wait
