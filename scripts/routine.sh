#! /bin/sh
set -ex

if [ "x$1" = "xbuild" ]; then
  alias go='make build'
elif [ "x$1" = "xbuild-push" ]; then
  alias go='make build push'
else
  echo "I don't know how to '$1'"
  exit 1
fi

(cd hs && go) &
HS=$!
(cd js && go) &
JS=$!
(cd scripts/devnet-algo && go) &
ALGO=$!
(cd scripts/devnet-eth && go) &
ETH=$!
(cd scripts/devnet-cfx && go) &
CFX=$!

wait $HS $JS $ALGO $ETH $CFX

if [ "x$1" = "xbuild-push" ]; then
  set +x
  echo "============================"
  echo "Don't forget js-reach-stdlib"
  echo "============================"
fi
