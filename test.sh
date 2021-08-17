#!/bin/sh -e
ROOT=$(pwd)
REACH=${ROOT}/reach

c () {
  echo "c $1"
  ${REACH} compile "$1"
}

fc () {
  if ! [ -f "$1" ] ; then
    echo "$1" does not exist
    false
  fi
  if (c "$1") ; then
    false
  else
    true
  fi
}

err () {
  fc "hs/t/n/$1.rsh"
}

jb () {
  # (cd "$ROOT"/js/js-deps && make build)
  (cd "$ROOT"/js/stdlib && make build)
  (cd "$ROOT"/js/runner && make build)
  # (cd "$ROOT"/js && make build)
}

r () {
  printf "\nRun %s\n" "$1"
  (cd "$1"

  ${REACH} clean
  rm -f build/*.teal*
  ${REACH} compile --install-pkgs
  ${REACH} compile

  # tealcount1 .

  make build
  # make down

  # jb

  export REACH_DEBUG=1
  # export REACH_ALGO_DEBUG=1
  # REACH_CONNECTOR_MODE=ETH ${REACH} run
  # REACH_CONNECTOR_MODE=CFX ${REACH} run
  REACH_CONNECTOR_MODE=ALGO ${REACH} run

  # Ganache
  # REACH_CONNECTOR_MODE=ETH-live ETH_NODE_URI=http://host.docker.internal:7545 REACH_ISOLATED_NETWORK=1 ${REACH} run

)
}

tealcount1 () {
  if [ -d "${1}/build" ] ; then
  for t in "${1}"/build/"${2}"*.teal ; do
    td=$(dirname "${t}")
    tn=$(basename "${t}")
    (cd "${td}" && ("${ROOT}/scripts/goal-devnet" clerk compile "${tn}" || true))
  done
  SIZE=$(wc -c "${1}"/build/"${2}"*tok | tail -1 | awk '{print $1}')
  echo "$1" "$2" - "$SIZE"
  wc -c "${1}"/build/"${2}"*tok
  fi
}

tealcount () {
  for e in examples/* ; do
    tealcount1 "$e"
  done
}

# tealcount

#######

# jb

# c hs/t/n/Err_IllegalEffPosition.rsh
# c hs/t/y/timeout_calc.rsh
# c examples/workshop-trust-fund/index.rsh
# err Err_Token_NotCreated

# c hs/t/y/big-d8cff.rsh
# tealcount1 hs/t/y big-d8cff

jb
#r examples/overview # XXX test debigger
#r examples/tut-7 # XXX test debigger

# r examples/log-attack1j
# r examples/remote
# r examples/map-rwrw
r examples/rent-seeking
exit 0
r examples/map-vary
exit 0
r examples/cache-events
r examples/workshop-fomo-generalized
r examples/workshop-trust-fund

# c examples/duoswap/index.rsh

# (cd examples/abstract-simul && make build)

# (cd hs && mk hs-test)
