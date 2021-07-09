#!/bin/bash
WHICH="$1"

banner () {
  echo
  echo "############"
  echo "$*"
  echo "############"
  echo
}

echo > /tmp/status
STATUS="pass"

BUILD_STATUS="fail"
banner Cleaning
./one.sh clean "${WHICH}"
banner Building
if ./one.sh build "${WHICH}" ; then
  BUILD_STATUS="pass"
fi
if [ "x${BUILD_STATUS}" = "xfail" ] ; then
  STATUS="fail"
fi

# It might be possible to run these all in parallel
for CONN in ETH ALGO CFX ; do
  THIS_STATUS="fail"
  if [ "x${BUILD_STATUS}" = "xpass" ] ; then
    export REACH_CONNECTOR_MODE="${CONN}"
    export REACH_DEBUG=1
    banner Running w/ "${CONN}"
    # We are using foreground to get around the lack of TTY allocation that
    # inhibits docker-compose run. I am worried that this will be ineffective
    # at stopping the containers
    # ^ XXX it actually doesn't enforce things properly for tut-7-rpc
    case "${CONN}" in
      ALGO) TIMEOUT=$((4 * 60)) ;;
      CFX) TIMEOUT=$((6 * 60)) ;;
      ETH) TIMEOUT=$((2 * 60)) ;;
    esac
    timeout --foreground "${TIMEOUT}" ./one.sh run "${WHICH}"
    EXIT=$?
    if [ $EXIT -eq 124 ] ; then
      echo Timeout
      THIS_STATUS="fail-time"
    elif [ $EXIT -eq 0 ] ; then
      THIS_STATUS="pass"
    fi
    banner Bringing down "${CONN}"
    ./one.sh down "${WHICH}"
  fi
  if ! [ "x${THIS_STATUS}" = "xpass" ] ; then
    STATUS="fail"
  fi
  echo "export ${CONN}_STATUS=\"${THIS_STATUS}\"" >> /tmp/status
done

echo "export STATUS=\"${STATUS}\"" >> /tmp/status
echo "export EXAMPLE_URL=\"${CIRCLE_BUILD_URL}\"" >> /tmp/status
# XXX output results in the JUnit test format so we can go to
# ${EXAMPLE_URL}/tests/ETH on a failure?
