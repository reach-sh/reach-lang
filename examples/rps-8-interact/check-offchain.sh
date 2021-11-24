#!/bin/sh
export REACH_CONNECTOR_MODE=ETH-live
export REACH_DEBUG=1
export ETH_NODE_URI=https://kovan2.arbitrum.io/rpc

../../reach run
