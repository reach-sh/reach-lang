#!/bin/sh
TOTALP1=$(find ../examples -maxdepth 1 -type d | wc -l)
TOTAL=$((TOTALP1 - 1))

PRE=config.pre.yml
MID=config.mid.yml
END=config.end.yml

cat >>"${END}" <<END
    - "examples-sink":
        requires:
END

for CONN in ETH ALGO CFX ; do
  case "${CONN}" in
    ALGO) 
      PER=8
      DEP="build-devnet-algo"
      ;;
    CFX)
      PER=8
      DEP="build-devnet-cfx"
      ;;
    ETH)
      PER=16
      DEP="build-devnet-eth"
      ;;
  esac
  SIZE=$(((TOTAL + (PER - 1)) / PER))
  for RANK in $(seq 0 $((SIZE - 1))) ; do
    cat >>"${MID}" <<END
    - "examples":
        name: "examples.${CONN}.${RANK}"
        filters:
          tags:
            only: /[0-9]*\.[0-9]*\.[0-9]*/
        connector: "${CONN}"
        size: "${SIZE}"
        rank: "${RANK}"
        requires:
          - "build"
          - "${DEP}"
END
    cat >>"${END}" <<END
          - "examples.${CONN}.${RANK}"
END
  done
done

cat "${PRE}" "${MID}" "${END}" > config.gen.yml
