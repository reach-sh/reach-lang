#!/bin/sh
TOTALP1=$(find ../examples -maxdepth 1 -type d | wc -l)
TOTAL=$((TOTALP1 - 1))

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
END
#          - "hs-test"

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
  EXTRA_LAYER="$3"
  shift 3
  NAME="build-${IMAGE}"
  cat >>"${MID}" <<END
    - "build-image":
        name: "${NAME}"
        image: "${IMAGE}"
        exec: "${EXEC}"
        extra_layer: "${EXTRA_LAYER}"
END
  deps "$@"
  cat >>"${IEND}" <<END
          - "${NAME}"
END
}

image "real" "haskell-build-artifacts" "solc z3" ""
image "fake" "reach" "" "haskell-build-artifacts"
image "fake" "reach-cli" "" "haskell-build-artifacts"
image "real" "js-deps" "" "haskell-build-artifacts"
image "real" "stdlib" "build" "reach" "js-deps"
image "fake" "runner" "" "stdlib"
image "fake" "react-runner" "" "stdlib" "js-deps"
image "fake" "rpc-server" "" "runner"

cat >>"${END}" <<END
    - "examples-sink":
        requires:
END

for CONN in ETH ALGO CFX ; do
  CONNlc=$(echo "${CONN}" | tr '[:upper:]' '[:lower:]')
  IMAGE="devnet-${CONNlc}"
  case "${CONN}" in
    ALGO) PER=8 EXEC="fake" LAYERS="build prepare generate";;
    CFX) PER=8 EXEC="real" LAYERS="build";;
    ETH) PER=16 EXEC="fake" LAYERS="";;
  esac
  image "${EXEC}" "${IMAGE}" "${LAYERS}"
  SIZE=$(((TOTAL + (PER - 1)) / PER))
  for RANK in $(seq 0 $((SIZE - 1))) ; do
    NAME="examples.${CONN}.${RANK}"
    cat >>"${MID}" <<END
    - "examples":
        name: "${NAME}"
        connector: "${CONN}"
        size: "${SIZE}"
        rank: "${RANK}"
END
    deps "reach" "reach-cli" "runner" "rpc-server" "${IMAGE}"
    cat >>"${END}" <<END
          - "${NAME}"
END
  done
done

cat "${PRE}" "${MID}" "${END}" "${IEND}" > config.gen.yml
