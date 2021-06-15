#!/bin/bash
cd /tmp/workspace/record || exit
FAIL=0
FAIL_ETH=0
FAIL_ALGO=0
FAIL_CFX=0
TOTAL=0
FAILS=""
for i in * ; do
  ((TOTAL++))
  # shellcheck disable=SC1090
  source "$i"
  if [ "x${STATUS}" = "xfail" ] ; then
    echo FAILED: "$i"
    if (( FAIL < 10 )) ; then
      FAILS="${FAILS} ${i}"
    fi
    ((FAIL++))
  fi
  if [[ "x${ETH_STATUS}" == xfail* ]] ; then
    ((FAIL_ETH++))
  fi
  if [[ "x${ALGO_STATUS}" == xfail* ]] ; then
    ((FAIL_ALGO++))
  fi
  if [[ "x${CFX_STATUS}" == xfail* ]] ; then
    ((FAIL_CFX++))
  fi
done

if (( FAIL > 0 )) ; then
  SYM="FAIL"
  MSG="${FAIL} of ${TOTAL} failed!"
  _helper () {
    m="$1"
    c="$2"
    if (( c > 0 )) ; then
      MSG="${MSG} *$m* ${c}"
    fi
  }
  _helper "ETH" "${FAIL_ETH}"
  _helper "ALGO" "${FAIL_ALGO}"
  _helper "CFX" "${FAIL_CFX}"
else
  SYM="OKAY"
  MSG="${TOTAL} passed!"
fi
echo "export RECORD_MESSAGE='*${SYM}* ${CIRCLE_USERNAME} > Examples: ${MSG} <${CIRCLE_BUILD_URL}|more...>${FAILS}'"
