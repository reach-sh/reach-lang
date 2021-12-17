#! /bin/sh
set -ex
MODE="$1"

if [ "$MODE" = "build" ]; then
  alias go='make build'
elif [ "$MODE" = "push" ]; then
  alias go='make push'
elif [ "$MODE" = "build-push" ]; then
  alias go='make build push'
elif [ "$MODE" = "build-m1" ]; then
  alias go='make build-m1'
else
  echo "I don't know how to '$MODE'"
  exit 1
fi

(cd hs && go) &
HS=$!

# XXX js relies on hs, so this has to be done first
wait $HS

(cd js && go) &
JS=$!
(cd scripts/devnet-algo && go) &
ALGO=$!
(cd scripts/devnet-eth && go) &
ETH=$!
(cd scripts/devnet-cfx && go) &
CFX=$!

wait $HS $JS $ALGO $ETH $CFX

if [ "$MODE" = "build-push" ]; then
  set +x
  echo "============================"
  echo "Don't forget js-reach-stdlib"
  echo "============================"
fi
