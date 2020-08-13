#!/bin/sh

# Usage: sbin/prof2svg.sh PROF_FILE
# Creates PROF_FILE.svg next to PROF_FILE
# Requires ghc-prof-flamegraph to be stack built in your current snapshot

HERE=$(dirname $(realpath $0))
HS=${HERE}/..

export STACK_YAML=${HS}/stack.yaml

cat $1 | stack exec ghc-prof-flamegraph > $1.svg
