#!/bin/sh

echo 'Fetching; this may take a few minutes...'
time aws dynamodb scan \
  --table-name CompileLog \
  --select SPECIFIC_ATTRIBUTES \
  --projection-expression userId,startTime,geoCountry,geoRegion \
  > data.json
echo
echo 'Done! Data dumped to'
echo 'data.json'
