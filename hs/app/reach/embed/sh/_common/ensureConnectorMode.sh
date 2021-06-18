# Makes sure to set REACH_CONNECTOR_MODE to one of:
# * ETH-test-dockerized-geth
# * ETH-live
# * ALGO-test-dockerized-algod
# $WHAT-$WHERE-$HOW-$HOW_WHERE
# (or error)
# This can be derived from REACH_TESTNET and possibly REACH_ETH_MODE

# Expand defaults
REACH_CONNECTOR_MODE=${REACH_CONNECTOR_MODE:-ETH}
case "$REACH_CONNECTOR_MODE" in
  "ETH")
    REACH_CONNECTOR_MODE="ETH-test-dockerized-geth"
    ;;
  "ETH-live")
    REACH_CONNECTOR_MODE="ETH-live"
    ;;
  "ETH-test")
    REACH_CONNECTOR_MODE="ETH-test-dockerized-geth"
    ;;
  "ETH-test-dockerized")
    REACH_CONNECTOR_MODE="ETH-test-dockerized-geth"
    ;;
  "ALGO")
    REACH_CONNECTOR_MODE="ALGO-test-dockerized-algod"
    ;;
  "ALGO-live")
    REACH_CONNECTOR_MODE="ALGO-live"
    ;;
  "ALGO-test")
    REACH_CONNECTOR_MODE="ALGO-test-dockerized-algod"
    ;;
  "ALGO-test-dockerized")
    REACH_CONNECTOR_MODE="ALGO-test-dockerized-algod"
    ;;
  "CFX")
    REACH_CONNECTOR_MODE="CFX-devnet"
    ;;
  "CFX-devnet")
    REACH_CONNECTOR_MODE="CFX-devnet"
    ;;
  "CFX-live")
    REACH_CONNECTOR_MODE="CFX-live"
    ;;
esac

# ensure it is one of the supported things
case "$REACH_CONNECTOR_MODE" in
  "ETH-live")
    ;;
  "ETH-test-dockerized-geth")
    ;;
  "ALGO-live")
    ;;
  "ALGO-test-dockerized-algod")
    ;;
  "CFX-devnet")
    ;;
  "CFX-live")
    ;;
  *)
    fatal_unrecognized_connector_mode
    ;;
esac

# make sure sub-commands receive this
export REACH_CONNECTOR_MODE
