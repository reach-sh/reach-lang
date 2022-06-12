#!/bin/sh -e
export REACH_DEBUG=0
alias reach="../../reach"

[ -z "$CIRCLECI" ] && reach devnet --await-background
reach run index seller
reach run index buyer

# Does nothing yet.. keep going through the tutorial!
