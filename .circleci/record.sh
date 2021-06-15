#!/bin/bash
# XXX Write this montrosity in something else with abstractions! Python3 is
# available or we could use JavaScript
# XXX Have the records in JSON?
cd /tmp/workspace/record || exit
FAIL=0
FAIL_ETH=0
FAILS_ETH=""
FAIL_ALGO=0
FAILS_ALGO=""
FAIL_CFX=0
FAILS_CFX=""
TOTAL=0
for i in * ; do
  ((TOTAL++))
  # shellcheck disable=SC1090
  source "$i"
  e=${i//examples./}
  el="<${EXAMPLE_URL}|${e}>"
  if [ "x${STATUS}" = "xfail" ] ; then
    (>&2 echo FAILED: "$e")
    ((FAIL++))
  fi
  # XXX with abstraction, if there are too many, add "..."
  if [[ "x${ETH_STATUS}" == xfail* ]] ; then
    if (( FAIL_ETH < 10 )) ; then
      FAILS_ETH="${FAILS_ETH} ${el}"
    fi
    ((FAIL_ETH++))
  fi
  if [[ "x${ALGO_STATUS}" == xfail* ]] ; then
    if (( FAIL_ALGO < 10 )) ; then
      FAILS_ALGO="${FAILS_ALGO} ${el}"
    fi
    ((FAIL_ALGO++))
  fi
  if [[ "x${CFX_STATUS}" == xfail* ]] ; then
    if (( FAIL_CFX < 10 )) ; then
      FAILS_CFX="${FAILS_CFX} ${el}"
    fi
    ((FAIL_CFX++))
  fi
done

if (( FAIL > 0 )) ; then
  SYM="FAIL"
  PRE="${FAIL} of ${TOTAL} failed!"
  POST=""
  _helper () {
    m="$1"
    c="$2"
    f="$3"
    if (( c > 0 )) ; then
      POST="${POST}\n*$m* ${c}: ${f}"
    fi
  }
  _helper "ETH" "${FAIL_ETH}" "${FAILS_ETH}"
  _helper "ALGO" "${FAIL_ALGO}" "${FAILS_ALGO}"
  _helper "CFX" "${FAIL_CFX}" "${FAILS_CFX}"
else
  SYM="OKAY"
  PRE="${TOTAL} passed!"
  POST=""
fi
# XXX make circle_branch a link
echo "export RECORD_MESSAGE='*${SYM}* ${CIRCLE_USERNAME}/${CIRCLE_BRANCH} > examples: ${PRE} <${CIRCLE_BUILD_URL}|more...>${POST}'"
