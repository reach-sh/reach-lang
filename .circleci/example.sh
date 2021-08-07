#!/bin/bash
CONN="$1"
SIZE="$2"
RANK="$3"

banner () {
  echo
  echo "############"
  echo "$*"
  echo "############"
  echo
}

banner Running w/ "${CONN}"

export REACH_CONNECTOR_MODE="${CONN}"
export REACH_DEBUG=1
case "${CONN}" in
  ALGO) TIMEOUT=$((5 * 60)) ;;
  CFX) TIMEOUT=$((5 * 60)) ;;
  ETH) TIMEOUT=$((5 * 60)) ;;
esac

cd ../examples || exit 1
go() {
  WHICH="$1"
  banner "${WHICH}" - clean
  ./one.sh clean "${WHICH}"
  STATUS="fail"
  banner "${WHICH}" - build
  if ./one.sh build "${WHICH}" ; then
    # We are using foreground to get around the lack of TTY allocation that
    # inhibits docker-compose run. I am worried that this will be ineffective
    # at stopping the containers
    # ^ XXX it actually doesn't enforce things properly for tut-7-rpc
    timeout --foreground "${TIMEOUT}" ./one.sh run "${WHICH}"
    EXIT=$?
    if [ $EXIT -eq 124 ] ; then
      echo Timeout
      STATUS="fail-time"
    elif [ $EXIT -eq 0 ] ; then
      STATUS="pass"
    fi
    banner "${WHICH}" - down
    ./one.sh down "${WHICH}"
  fi
  # XXX output results in the JUnit test format so we can go to
  # ${EXAMPLE_URL}/tests/ETH on a failure?
  echo "[ \"${STATUS}\", \"${CIRCLE_BUILD_URL}\" ]" > /tmp/workspace/record/"${CONN}.${WHICH}"
}
for WHICH in $(find . -maxdepth 1 -type d | sed 'sX./XX' | sort | tail -n +2 | awk "NR % ${SIZE} == ${RANK}") ; do
  go "${WHICH}" &
done

wait
