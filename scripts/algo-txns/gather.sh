#!/bin/bash
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

year 2020
year 2021
