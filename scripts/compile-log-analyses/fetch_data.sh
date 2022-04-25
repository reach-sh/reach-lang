#!/bin/sh

echo 'fetching... this may take a few mins...'
time aws dynamodb scan \
  --table-name CompileLog \
  --select SPECIFIC_ATTRIBUTES \
  --projection-expression userId,startTime,ip \
  > data.json
echo
echo 'Done! Data dumped to'
echo 'data.json'
