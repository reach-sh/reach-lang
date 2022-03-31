#!/bin/sh

START_TIME_UNIX="$(date +%s)"
START_TIME_UTC="$(date --utc --iso-8601=seconds)Z"

ping_devnet() {
  TIME_NOW="$(date +%s)"
  ELAPSED="$(( "$TIME_NOW" - "$START_TIME_UNIX" )).0"
  REPORT="{
    \"userId\": \"ALGO-devnet\",
    \"startTime\": \"$START_TIME_UTC\",
    \"version\": \"0.1.9\",
    \"elapsed\": \"$ELAPSED\",
    \"result\": \"devnet 24h ping\",
    \"connectorMode\": \"ALGO-devnet\"
  }"

  curl \
    --request POST \
    --header "Content-Type: application/json; charset=utf-8" \
    --data "$REPORT" \
    "https://log.reach.sh/submit"
}

# 86400 sec = 24 hours
while sleep 86400; do
  ping_devnet &
done
