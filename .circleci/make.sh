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
    ALGO) PER=8 ;;
    CFX) PER=8 ;;
    ETH) PER=16 ;;
  esac
  SIZE=$(((TOTAL + (PER - 1)) / PER))
  for RANK in $(seq 0 $((SIZE - 1))) ; do
    cat >>"${MID}" <<END
    - "examples":
        name: "examples.${CONN}.${RANK}"
        connector: "${CONN}"
        size: "${SIZE}"
        rank: "${RANK}"
        requires:
          - "build"
END
    cat >>"${END}" <<END
          - "examples.${CONN}.${RANK}"
END
  done
done

cat "${PRE}" "${MID}" "${END}" > config.gen.yml
