#!/bin/sh -e
MODE="$1"

IMAGE="reachsh/haskell-build-artifacts-${MODE}"

# Currently stan is broken
# docker run --rm \
#   -ti "${IMAGE}" \
#   make ROOT=.docker-root docker-check

mkdir -p hs/test-reports
if [ "${MODE}" = "closed" ] ; then
  REACH_HS_STACK_ARGS="--flag reach:everest"
fi

export REACH_HS_STACK_ARGS
docker run --rm \
  -v "${PWD}/examples:/examples/" \
  -v "${PWD}/docs/src:/docs/src/" \
  -v "${PWD}/hs/t:/build/t" \
  -v "${PWD}/hs/test-reports:/build/test-reports" \
  -v "${PWD}/vsce/data:/vsce/data" \
  -e REACH_HS_STACK_ARGS \
  -ti "${IMAGE}" \
  make ROOT=.docker-root docker-test-xml || true

OUT="/tmp/workspace/hs-test-${MODE}"
echo "${CIRCLE_BUILD_URL}" > "${OUT}-url"
cp hs/test-reports/junit.xml "${OUT}.xml"
