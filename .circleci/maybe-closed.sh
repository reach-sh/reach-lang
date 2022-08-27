#!/bin/sh -xe
REPO=reach-everest
REMOTE="git@github.com:reach-sh/${REPO}.git"

JOB="$1"
if ! (echo "$JOB" | grep closed > /dev/null) ; then
  echo "Not closed"
  exit 0
fi

RPZ=$(realpath "$0")
ROOT="$(dirname "$RPZ")/.."
cd "${ROOT}"

go () {
  BRANCH="$1"
  echo "Making shallow clone of '$BRANCH'"
  git clone --depth 1 --branch "$BRANCH" --single-branch "$REMOTE" "$REPO"
}

if (go "$CIRCLE_BRANCH") ; then
  echo Closed has same branch
else
  go "master"
fi

exec "./reach-everest/link.sh"
