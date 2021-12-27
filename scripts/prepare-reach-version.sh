#!/bin/bash

git checkout -- ../VERSION

. ../VERSION

export PATH=${PATH}:../.bin

rccount() {
  rc_exists=$(git tag | grep $VERSION | grep rc > /dev/null ; echo $?)
  if [ "${rc_exists}" -eq 0 ] ; then
    rc=$(($(git tag | grep $VERSION | grep rc | sort | tail -1 | awk -F '.' '{ print $NF }')+1)) 
  else
    rc=0
  fi
  echo $rc
}

rc=$(rccount)
export tag="-rc.${rc}"
export rc=".${rc}"

mo ../VERSION.mo > ../VERSION
