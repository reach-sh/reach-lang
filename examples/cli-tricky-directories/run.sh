#!/usr/bin/env bash
# shellcheck disable=SC2000,SC2001
set -e

# TODO: portable replacement for `wc -L`
l () { echo -n "$1" | wc -L 2>/dev/null || echo -n "$1" | wc -m; }

while read -r d; do
  n="$(echo "$d" | sed 's/[[:space:]].*//')"
  p="$(echo "$d" | sed "s/^${n}[[:space:]]*://")"
  mkdir -p "$p"
  echo "$n" > "$p/NAME"
done < ./DS

TEMP="$(mkdir -p ./.tmp && cd ./.tmp && pwd)"
TOUT="$(mkdir -p "$TEMP"/build && cd "$TEMP/build" && pwd)"
DIRS="$(find . -maxdepth 1 -type d ! \( -name . -o -name .tmp \) -prune | sed 's/^\.\///' | sort -h)"
LDIR="$(echo "$DIRS" | while IFS=$'\n' read -r d; do l "$d"; done | sort -h | tail -n 1)"
ROOT="$(cd ../.. && pwd)"

echo 0 >"$TEMP/EXIT"

IFS=$'\n'
for d in $DIRS; do
  p="$((LDIR - $(l "$d") + 3))"
  printf '\e[34;1mRunning "%s"...\e[0m' "$d"

  if [ "$(find "$d" -type f -name SKIP | wc -l)" -gt 0 ]; then
    if [ -n "$CI" ]; then
      printf '\e[31;1m  %*s\e[0m Failed:\n' "$p" "✘"
      echo 'Skipping is only allowed during development.'
      echo 1 >"$TEMP/EXIT"
    else
      printf '\e[1;33m  %*s\e[0m Skipped\n' "$p" "•"
    fi
    continue
  fi

  t="$TEMP/$d.log"

  set +e
  ./go.sh "$ROOT" "$TOUT" "$d" >"$t" 2>&1
  r="$?"
  set -e

  if [ "$r" -eq 0 ]; then
    printf '\e[1;32m  %*s\e[0m Succeeded\n'            "$p" "✔"
  else
    printf '\e[31;1m  %*s\e[0m Failed with code %s:\n' "$p" "✘" "$r"
    printf '%s\n\n' "$(cat "$t")"
    echo 1 >"$TEMP/EXIT"
  fi

  rm -r "$d"
done

echo
exit "$(cat "$TEMP/EXIT")"
