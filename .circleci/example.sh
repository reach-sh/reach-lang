#!/bin/bash
CONN="$1"
SIZE="$2"
RANK="$3"

echo Running w/ "${CONN}"

export REACH_CONNECTOR_MODE="${CONN}"
export REACH_DEBUG=1
case "${CONN}" in
  ALGO) TIMEOUT=$((10 * 60)) ;;
  CFX) TIMEOUT=$((10 * 60)) ;;
  ETH) TIMEOUT=$((10 * 60)) ;;
esac

cd ../examples || exit 1
go() {
  WHICH="$1"
  echo "${WHICH}"
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
      echo Timeout
      STATUS="fail-time"
    elif [ $EXIT -eq 0 ] ; then
      STATUS="pass"
    fi
    gzip "${THIS_ART}"
    rm -f "${THIS_ART}"
  fi
  echo "[ \"${STATUS}\", \"${CONN}.${RANK}\" ]" >/tmp/workspace/record/"${THIS}"
}
for WHICH in $(find . -maxdepth 1 -type d | sed 'sX./XX' | sort | tail -n +2 | awk "NR % ${SIZE} == ${RANK}") ; do
  go "${WHICH}" &
done

wait
