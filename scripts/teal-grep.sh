#!/bin/bash
while IFS= read -r -d $'\0' file; do
  # Do two extracts appear in a row?
  awk '/extract/ && last {print last; print} {last=""} /extract/{last=$0}' "$file"
done < <(find . -name '*.teal' -print0)
