#!/bin/sh
PRE=config.pre.yml
MID=config.mid.yml
END=config.end.yml
IEND=config.iend.yml
cat >"${MID}" </dev/null
cat >"${END}" </dev/null
cat >"${IEND}" </dev/null

cat >>"${IEND}" <<END
    - "build-sink":
        requires:
          - "hs-test"
END

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
  cat >>"${IEND}" <<END
          - "${NAME}"
END
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
    - "examples-sink":
        requires:
END

examples () {
  CONN="$1"
  EXEC="$2"
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
  deps "reach" "reach-cli" "runner" "rpc-server" "${IMAGE}"
  cat >>"${END}" <<END
          - "${NAME}"
END
}

examples ETH fake 1
examples ALGO fake 1
examples CFX real 1

cat "${PRE}" "${MID}" "${END}" "${IEND}" > config.gen.yml
