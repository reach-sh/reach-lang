#! /bin/sh
# usage: sbin/test.sh
# (call from js/dockerized-tests/ folder)

# Note: We're creating an `stdlib-test` directory from scratch here in order
#   to leverage the `reach` script and abstract over boilerplate such as
#   `docker-compose.yml` config.

set -x

mkdir stdlib-test
cp    index.* stdlib-test
cp -r lib     stdlib-test/lib
cd stdlib-test || exit 1

# js/dockerized-tests/stdlib-test/
REACH=../../../reach
$REACH compile
REACH_CONNECTOR_MODE=ETH $REACH run

# TODO re-enable these once `reach` script is ready
# REACH_CONNECTOR_MODE=ALGO $REACH run

RESULT=$?
$REACH down

exit $RESULT
