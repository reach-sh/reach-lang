#!/bin/sh
TOTALP1=$(find ../examples -maxdepth 1 -type d | wc -l)
TOTAL=$((TOTALP1 - 1))

# XXX Run everest linker

PRE=config.pre.yml
MID=config.mid.yml
END=config.end.yml
cat >"${MID}" </dev/null
cat >"${END}" </dev/null

deps () {
  DEPS="$*"
  cat >>"${MID}" <<END
        deps: "$DEPS"
END
  if [ "x${DEPS}" != "x" ] ; then
  cat >>"${MID}" <<END
        requires:
END
  for DEP in "$@"; do
    cat >>"${MID}" <<END
          - "build-${DEP}"
END
  done
  fi
}

image () {
  EXEC="$1"
  IMAGE="$2"
  shift 2
  NAME="build-${IMAGE}"
  cat >>"${MID}" <<END
    - "build-image":
        name: "${NAME}"
        image: "${IMAGE}"
        exec: "${EXEC}"
END
  deps "$@"
}

image "real" "haskell-build-artifacts" "devnet-algo"
image "fake" "reach" "haskell-build-artifacts"
image "fake" "reach-cli" "haskell-build-artifacts"
image "real" "js-deps"
image "real" "stdlib" "reach" "js-deps"
image "fake" "runner" "stdlib"
image "fake" "react-runner" "stdlib" "js-deps"
image "fake" "rpc-server" "runner"

cat >>"${END}" <<END
    - "build-sink":
        requires:
          - "hs-test"
END

conn () {
  EXEC="$1"
  CONN="$2"
  SIZE="$3"

  CONNlc=$(echo "${CONN}" | tr '[:upper:]' '[:lower:]')
  IMAGE="devnet-${CONNlc}"
  image "${EXEC}" "${IMAGE}"
  NAME="examples.${CONN}"
  cat >>"${MID}" <<END
    - "examples":
        name: "${NAME}"
        connector: "${CONN}"
        size: ${SIZE}
END
  deps "reach" "reach-cli" "runner" "react-runner" "rpc-server" "${IMAGE}"
  BT_NAME="browser-tests.${CONN}"
  cat >>"${MID}" <<END
    - "browser-tests":
        name: "${BT_NAME}"
        connector: "${CONN}"
END
  deps "reach" "reach-cli" "react-runner" "${IMAGE}"
  cat >>"${END}" <<END
          - "${NAME}"
          - "${BT_NAME}"
END
}

per () {
  PER="$1"
  echo $(((TOTAL + (PER - 1)) / PER))
}

# XXX Learn connectors from repo
conn fake ETH "$(per 16)"
conn fake ALGO "$(per 16)"
conn fake CFX "$(per 16)"

# XXX Allow closed to add stuff

cat "${PRE}" "${MID}" "${END}" > config.gen.yml
