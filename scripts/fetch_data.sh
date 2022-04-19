#!/bin/sh

echo 'fetching... this may take a few mins...'
time aws dynamodb scan \
  --table-name CompileLog \
  --select SPECIFIC_ATTRIBUTES \
  --projection-expression userId,startTime,ip \
  > data.json
echo 'data dumped to: data.json'
