#!/bin/sh

oldArts=/tmp/workspace/artifacts/
newArts=/tmp/artifacts/
result="$newArts/bytecode-size.json"

echo "{" > "$result"
first=1
for f in "$oldArts"/ETH.*.size.json; do
  if [ "$first" = "1" ]; then
    first=0
  else
    echo "," >> "$result"
  fi
  echo -n '"' >> "$result"
  echo -n "$(basename $f | sed 's/ETH\.\(.*\)\.size\.json/\1/')" >> "$result"
  echo -n '": ' >> "$result"
  cat "$f" >> "$result"
done;
echo "}" >> "$result"

