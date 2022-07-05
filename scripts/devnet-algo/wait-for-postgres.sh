#!/usr/bin/env bash
while ! pg_isready --port="$POSTGRES_PORT" --username="$POSTGRES_USER" --dbname="$POSTGRES_DB"; do
  sleep 0.2
done
