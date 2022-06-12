#!/bin/sh -e
export REACH_DEBUG=0
alias reach="../../reach"

[ -z "$CIRCLECI" ] && reach devnet --await-background

# Role selection not yet implemented
reach run index

# Does nothing yet.. keep going through the tutorial! 
