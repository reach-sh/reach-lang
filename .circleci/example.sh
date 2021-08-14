#!/bin/bash
CONN="$1"
SIZE="$2"
RANK="$3"
KIDS=0

echo Running w/ "${CONN}:"

export REACH_CONNECTOR_MODE="${CONN}"
export REACH_DEBUG=1
case "${CONN}" in
  ALGO) TIMEOUT=$((10 * 60)) ;;
  CFX) TIMEOUT=$((10 * 60)) ;;
  ETH) TIMEOUT=$((10 * 60)) ;;
esac

D="devnet-$(echo "$CONN" | tr '[:upper:]' '[:lower:]')"
printf 'Bringing up %s...' "$D"
../reach devnet >/dev/null 2>&1 &
while true; do
  if [ "$(docker ps -qf "label=sh.reach.devnet-for=$D" | wc -l)" -gt 0 ]; then break; fi
  printf '.'
  sleep 1
done
printf ' Done.\n'

cd ../examples || exit 1
go() {
  WHICH="$1"
  echo "$2 ${WHICH}..."
  THIS="${CONN}.${WHICH}"
  THIS_ART="/tmp/artifacts/${THIS}"
  touch "${THIS_ART}"
  ./one.sh clean "${WHICH}" >>"${THIS_ART}"
  STATUS="fail"
  if ./one.sh build "${WHICH}" >>"${THIS_ART}" 2>&1 ; then
    # We are using foreground to get around the lack of TTY allocation that
    # inhibits docker-compose run. I am worried that this will be ineffective
    # at stopping the containers
    # ^ XXX it actually doesn't enforce things properly for tut-7-rpc
    timeout --foreground "${TIMEOUT}" ./one.sh run "${WHICH}" >>"${THIS_ART}" 2>&1
    EXIT=$?
    if [ $EXIT -eq 124 ] ; then
      echo "$WHICH timed out!"
      STATUS="fail-time"
    elif [ $EXIT -eq 0 ] ; then
      echo "$WHICH passed."
      STATUS="pass"
    else
      echo "$WHICH failed."
    fi
    gzip "${THIS_ART}"
    rm -f "${THIS_ART}"
  fi
  echo "[ \"${STATUS}\", \"${CONN}.${RANK}\" ]" >/tmp/workspace/record/"${THIS}"
}

EXS="$(find . -maxdepth 1 -type d | sed 'sX./XX' | sort | tail -n +2 | awk "NR % ${SIZE} == ${RANK}")"

case "${CONN}" in
  # XXX Conflux' transparent nonce selection currently isn't robust enough to
  # survive parallelized invocation; too many calls to `cfx_getNextNonce` at
  # once invariably clobber each other. See:
  # http://developer.confluxnetwork.org/conflux-doc/docs/send_transaction#send-transaction-by-javascript-program
  CFX) for WHICH in $EXS; do go "${WHICH}" 'Running'; done ;;

  *)
    for WHICH in $EXS; do
      KIDS=$((KIDS + 1))
      go "${WHICH}" 'Forking' &
    done

    while true; do
      wait -n || true
      KIDS=$((KIDS - 1))
      if [ $KIDS -eq 0 ]; then break; fi
    done
    ;;
esac

../reach down
wait
