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

for CONN in ETH ALGO CFX ; do
  THIS_STATUS="fail"
  if [ "x${BUILD_STATUS}" = "xpass" ] ; then
    export REACH_CONNECTOR_MODE="${CONN}"
    export REACH_DEBUG=1
    banner Running w/ "${CONN}"
    timeout $((2 * 60)) ./one.sh run "${WHICH}"
    EXIT=$?
    if [ $EXIT -eq 124 ] ; then
      echo Timeout
      THIS_STATUS="fail-time"
    elif [ $EXIT -eq 0 ] ; then
      THIS_STATUS="pass"
    fi
  fi
  if ! [ "x${THIS_STATUS}" = "xpass" ] ; then
    STATUS="fail"
  fi
  echo "export ${CONN}_STATUS=\"${THIS_STATUS}\"" >> /tmp/status
done

echo "export STATUS=\"${STATUS}\"" >> /tmp/status
