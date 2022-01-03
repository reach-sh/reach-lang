#!/bin/bash
CONN="$1"
# SIZE="$2"
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

../reach devnet --await-background

cd ../examples || exit 1
go() {
  WHICH="$1"
  echo "$2 ${WHICH}..."
  THIS="${CONN}.${WHICH}"
  THIS_ART="/tmp/artifacts/${THIS}"
  THIS_TR="/tmp/test_results/${WHICH}.xml"
  touch "${THIS_ART}" "${THIS_TR}"
  BEFORE=$(date +%s)
  ./one.sh clean "${WHICH}" >>"${THIS_ART}"
  STATUS="fail"
  MSG=""
  if ./one.sh build "${WHICH}" >>"${THIS_ART}" 2>&1 ; then
    # We are using foreground to get around the lack of TTY allocation that
    # inhibits docker-compose run. I am worried that this will be ineffective
    # at stopping the containers
    # ^ XXX it actually doesn't enforce things properly for tut-7-rpc
    timeout --foreground "${TIMEOUT}" ./one.sh run "${WHICH}" >>"${THIS_ART}" 2>&1
    EXIT=$?
    if [ $EXIT -eq 124 ] ; then
      MSG="$WHICH timed out!"
      STATUS="fail-time"
    elif [ $EXIT -eq 0 ] ; then
      MSG="$WHICH passed."
      STATUS="pass"
    else
      MSG="$WHICH failed."
    fi
  else
    MSG="$WHICH failed to build."
  fi
  echo "${MSG}"
  AFTER=$(date +%s)
  DURATION=$((AFTER - BEFORE))
  cat >>"${THIS_TR}" <<END
<?xml version="1.0" encoding="UTF-8"?>
<testsuite name="examples.${CONN}">
 <testcase name="${WHICH}" time="${DURATION}">
END
  if [ "${STATUS}" != "pass" ] ; then
    cat >>"${THIS_TR}" <<END
  <failure message="${STATUS}">${MSG}</failure>
END
  fi
  cat >>"${THIS_TR}" <<END
 </testcase>
</testsuite>
END
  gzip "${THIS_ART}"
  rm -f "${THIS_ART}"
  echo "[ \"${STATUS}\", \"${CONN}.${RANK}\" ]" >/tmp/workspace/record/"${THIS}"
}

EXS="$(find . -maxdepth 1 -type d | sed 'sX./XX' | sort | tail -n +2 | circleci tests split --split-by=timings --timings-type=testname)"

for WHICH in $EXS; do
  KIDS=$((KIDS + 1))
  go "${WHICH}" 'Forking' &
done

while true; do
  wait -n || true
  KIDS=$((KIDS - 1))
  if [ $KIDS -eq 0 ]; then break; fi
done

# ../reach down
wait
