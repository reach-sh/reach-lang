#!/bin/sh

if [ -n "$REACH_DISABLE_REPORTING" ]; then exit 0; fi

ping_devnet() {
  # REACH_CONNECTOR_MODE, REACH_VERSION, and REACHC_ID are defined in hs/app/reach/embed/docker/service-devnet-*.yml
  REPORT="{
    \"userId\": \"$REACHC_ID\",
    \"startTime\": \"$(date -u +%Y-%m-%d)\",
    \"version\": \"$REACH_VERSION\",
    \"result\": \"devnet 24h ping\",
    \"connectorMode\": \"$REACH_CONNECTOR_MODE\",
    \"initiator\": \"devnet_24h_ping\",
    \"usingVisualStudioExtension\": false
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
