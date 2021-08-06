#!/bin/sh
. ./shared.sh

DEST=config.gen.yml

CONNS="ETH ALGO CFX"

cat config.pre.yml > "${DEST}"
for conn in ${CONNS} ; do
  for m in $(seq 0 $((HOW_MANY_MACHINES - 1))) ; do
    cat >>"${DEST}" <<END
    - "example":
        name: "examples.${conn}.${m}"
        connector: "${conn}"
        rank: "${m}"
        requires:
          - "build"
END
  done
done

cat >>"${DEST}" <<END
    - "example-sink":
        requires:
END
for conn in ${CONNS} ; do
  for m in $(seq 0 $((HOW_MANY_MACHINES - 1))) ; do
    cat >>"${DEST}" <<END
          - "examples.${conn}.${m}"
END
  done
done
