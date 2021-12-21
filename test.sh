#!/bin/sh -e
ROOT=$(pwd)
REACH=${ROOT}/reach

export REACH_DOCKER=0
${REACH} -h

c () {
  echo c "$@"
  ${REACH} compile --intermediate-files "$@"
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
  #(cd "$ROOT"/js/js-deps && make build)
  (cd "$ROOT"/js/stdlib && make build)
  (cd "$ROOT"/js/runner && make build)
  #(cd "$ROOT"/js/rpc-server && make build)
  #(cd "$ROOT"/js/react-runner && make build)
  # (cd "$ROOT"/js && make build)
}

one () {
  MODE="$1"
  WHICH="$2"
  printf "\nONE %s %s\n" "$MODE" "$WHICH"
  (cd "examples"

  ./one.sh clean "${WHICH}"
  ./one.sh build "${WHICH}"
  REACH_DEBUG=1 REACH_CONNECTOR_MODE="${MODE}" ./one.sh run "${WHICH}"
)
}

ci () {
  MODE="$1"
  WHICH="$2"
  printf "\nCI %s %s\n" "$MODE" "$WHICH"
  (cd "examples/$WHICH"

  ${REACH} clean
  ${REACH} compile --intermediate-files
  make build
  REACH_DEBUG=1 REACH_CONNECTOR_MODE="$MODE" ${REACH} run
)
}

r () {
  printf "\nRun %s\n" "$1"
  (cd "$1"

  ${REACH} clean
  rm -f build/*.teal*
  ${REACH} compile --install-pkgs
  ${REACH} compile --intermediate-files

  # tealcount1 .

  make build
  # make down

  # jb

  export REACH_DEBUG=1
  # export REACH_ALGO_DEBUG=1
  REACH_CONNECTOR_MODE=ETH ${REACH} run
  #REACH_CONNECTOR_MODE=ALGO ${REACH} run
  #REACH_CONNECTOR_MODE=CFX ${REACH} run

  # Ganache
  #REACH_CONNECTOR_MODE=ETH-live ETH_NODE_URI=http://host.docker.internal:7545 REACH_ISOLATED_NETWORK=1 ${REACH} run

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

checkteal () {
  c "$1"/index.rsh
  ./scripts/goal-devnet clerk compile "$1"/build/index.main.appApproval.teal
}

# tealcount

#######

#exit 0

#jb
c hs/t/y/enthuGuy4.rsh
c hs/t/y/enthuGuy3.rsh
fc hs/t/n/API_NonCS.rsh
r users/hamza1
exit 0

# (cd hs && mk hs-test)
