#!/bin/bash
cd /workspace/record || exit
FAIL=0
FAIL_ETH=0
FAIL_ALGO=0
FAIL_CFX=0
TOTAL=0
for i in * ; do
  ((TOTAL++))
  # shellcheck disable=SC1090
  source "$i"
  if [ "x${STATUS}" = "xfail" ] ; then
    ((FAIL++))
  fi
  if [ "x${ETH_STATUS}" = "xfail" ] ; then
    ((FAIL_ETH++))
  fi
  if [ "x${ALGO_STATUS}" = "xfail" ] ; then
    ((FAIL_ALGO++))
  fi
  if [ "x${CFX_STATUS}" = "xfail" ] ; then
    ((FAIL_CFX++))
  fi
done
echo "export RECORD_MESSAGE='${CIRCLE_USER}: ${CIRCLE_SHA1}: ${TOTAL} ${FAIL} ${FAIL_ETH} ${FAIL_ALGO} ${FAIL_CFX}'"
