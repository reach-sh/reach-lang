#!/bin/sh -e
REPO="reach-everest"

RPZ=$(realpath "$0")
ROOT="$(dirname "$RPZ")/.."

cd "${ROOT}"
if ! [ -d "$REPO" ] ; then
  git clone "git@github.com:reach-sh/${REPO}.git"
fi
OPEN=$(git rev-parse --abbrev-ref HEAD)

echo "Checking out ${OPEN}"
cd "$REPO"
if git checkout "$OPEN" ; then
  echo "Checked out ${OPEN}"
else
  echo "Keeping master"
fi
exec ./link.sh
