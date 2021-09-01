#! /bin/sh
# usage: sbin/test.sh
# (call from js/dockerized-tests/ folder)

# Note: We're creating an `stdlib-test` directory from scratch here in order
#   to leverage the `reach` script and abstract over boilerplate such as
#   `docker-compose.yml` config.

set -x

rm -fr stdlib-test
mkdir stdlib-test
cp    index.* stdlib-test
cp -r lib     stdlib-test/lib
cd stdlib-test || exit 1

# js/dockerized-tests/stdlib-test/
REACH=../../../reach
sh -x $REACH compile
for m in ETH ALGO ; do
  REACH_CONNECTOR_MODE=$m sh -x $REACH run || exit $?
done

RESULT=$?
$REACH down

exit $RESULT
