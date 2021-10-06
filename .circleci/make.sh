#!/bin/sh
TOTALP1=$(find ../examples -maxdepth 1 -type d | wc -l)
TOTAL=$((TOTALP1 - 1))


PRE=config.pre.yml
MID=config.mid.yml
END=config.end.yml
IEND=config.iend.yml
RCEND=config.rcend.yml
cat >"${MID}" </dev/null
cat >"${END}" </dev/null
cat >"${IEND}" </dev/null
cat >"${RCEND}" </dev/null

cat >>"${RCEND}" <<END
    - "build-sink-rc":
        requires:
END

cat >>"${IEND}" <<END
    - "build-sink":
        requires:
END
#          - "hs-test"

deps () {
  IS_RC=$1
  shift 1
  DEPS="$@"
  cat >>"${MID}" <<END
        deps: "$DEPS"
END
  if [ "x${DEPS}" != "x" ] || [ "x${IS_RC}" != "x1" ]; then
  cat >>"${MID}" <<END
        requires:
END
    if [ "x${IS_RC}" != "x1" ]; then
    cat >>"${MID}" <<END
          - "RC-release"
END
    fi
    for DEP in "$@"; do
      if [ "x${IS_RC}" != "x1" ] ; then
    cat >>"${MID}" <<END
          - "build-${DEP}-rc"
END
       else
    cat >>"${MID}" <<END
          - "build-${DEP}"
END
      fi
    done
  fi
}

image () {
  EXEC="$1"
  IMAGE="$2"
  shift 2
  NAME="build-${IMAGE}"
  IS_RC="1"
  for i in $(seq 0 1); do
    if [ $i -eq 1 ]; then
      NAME=${NAME}-rc
      IS_RC="0"
  cat >>"${RCEND}" <<END
          - "${NAME}"
END
    else
  cat >>"${IEND}" <<END
          - "${NAME}"
END
    fi
  cat >>"${MID}" <<END
    - "build-image":
        name: "${NAME}"
        image: "${IMAGE}"
        exec: "${EXEC}"
END
    deps $IS_RC "$@"
  done
}

image "real" "haskell-build-artifacts"
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

for CONN in ETH ALGO CFX ; do
  CONNlc=$(echo "${CONN}" | tr '[:upper:]' '[:lower:]')
  IMAGE="devnet-${CONNlc}"
  case "${CONN}" in
    ALGO) PER=8 EXEC="fake";;
    CFX) PER=8 EXEC="real";;
    ETH) PER=16 EXEC="fake";;
  esac
  image "${EXEC}" "${IMAGE}"
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
    deps "1" "reach" "reach-cli" "runner" "rpc-server" "${IMAGE}"
    cat >>"${END}" <<END
          - "${NAME}"
END
  done
done

cat "${PRE}" "${MID}" "${END}" "${IEND}" "${RCEND}" > config.gen.yml
