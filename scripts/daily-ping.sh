#!/bin/sh

START_TIME_UNIX="$(date +%s)"
START_TIME_UTC="$(date -u -Iseconds)Z"

ping_devnet() {
  # REACH_CONNECTOR_MODE, REACH_VERSION, and REACHC_ID are defined in hs/app/reach/embed/docker/service-devnet-*.yml
  TIME_NOW="$(date +%s)"
  ELAPSED="$(( TIME_NOW - START_TIME_UNIX )).0"
  REPORT="{
    \"userId\": \"$REACHC_ID\",
    \"startTime\": \"$START_TIME_UTC\",
    \"version\": \"$REACH_VERSION\",
    \"elapsed\": \"$ELAPSED\",
    \"result\": \"devnet 24h ping\",
    \"connectorMode\": \"$REACH_CONNECTOR_MODE\"
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
