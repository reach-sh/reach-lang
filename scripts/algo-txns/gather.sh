#!/bin/bash -xe
WHICH="$1"

month () {
  Y="$1"
  M="$2"
  Yp="${Y}"
  ((Mp = M + 1))
  if [ "$Mp" = "13" ] ; then
    ((Yp = Y + 1))
    Mp=1
  fi
  ./index.sh "${WHICH}" "${Y}-${M}-01" "${Yp}-${Mp}-01" >> "data-${WHICH}.csv"
}

year () {
  Y="$1"
  for ((i=1; i<=12; i++)); do
    month "$Y" "$i"
  done
}

month 2021 11
month 2021 12
month 2022 01
month 2022 02
month 2022 03
month 2022 04
