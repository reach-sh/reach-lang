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

jbi() {
  WHICH="${1}"
  DIR="${ROOT}${WHICH}"
  (cd "${DIR}" && make build)
}

jb () {
  #jbi /js/js-deps
  jbi /js/stdlib
  jbi /js/runner
  #jbi /js/rpc-server
  jbi /js/react-runner
  #jbi /js
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
  ${REACH} compile --install-pkgs
  ${REACH} compile --intermediate-files
  make build
  REACH_DEBUG=N REACH_CONNECTOR_MODE="$MODE" ${REACH} run
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

  #export REACH_DEBUG=1
  #REACH_CONNECTOR_MODE=ETH ${REACH} run
  REACH_CONNECTOR_MODE=ALGO ${REACH} run
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

cdot () {
  export REACH_DEBUG=Y
  c "$1"
  dot -Tpng -O "$(dirname "$1")/build/$(basename "$1" .rsh).main.appApproval.dot"
  dot -Tpng -O "$(dirname "$1")/build/$(basename "$1" .rsh).main.state.dot"
}

# Disable this if you want to download a cache from CI (useful when js-deps
# changes)
export REACH_BUILD_NO_CACHE=Y

############################

c examples/rps-8-interact/index.rsh
c examples/rps-7-loops/index.rsh
exit 0

jb
export REACH_CONNECTOR_MODE=ALGO
export REACH_DEBUG=Y
#${REACH} devnet --await-background
cd users/xbacked-contracts/src
#${REACH} compile master_vault.rsh
${REACH} compile liquidate_and_stake_algo.rsh
rm -fr c2c/build
cp -fr build c2c/build
cd c2c
${REACH} run
exit 0

c users/algo-govt/index.rsh
exit 0

REACH_DEBUG=Y c users/xbacked-contracts/src/master_vault.rsh
exit 0

${REACH} compile --print-keyword-info
exit 0

REACH_DEBUG=Y cdot examples/algospensive/index.rsh
exit 0
jb
ci ALGO algospensive
exit 0

REACH_DEBUG=Y c users/xbacked-contracts/src/master_vault.rsh
XB=users/xbacked-contracts/src
diff -u "${XB}"/build/master_vault.main.appApproval.teal "${XB}"/build/master_vault_asa.main.appApproval.teal > xc.diff
exit 0
diff -u "${XB}"/master_vault.rsh "${XB}"/master_vault_asa.rsh
exit 0

# (cd hs && mk hs-test)
