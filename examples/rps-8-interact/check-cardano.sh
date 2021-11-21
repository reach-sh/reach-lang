#!/bin/sh
export REACH_CONNECTOR_MODE=ETH-live
export REACH_DEBUG=1
export ETH_NODE_URI=https://david.kevm.dev-mantis.iohkdev.io:8546

../../reach run
