#!/bin/bash

# Requires curl, awscli, gnuplot, and jq
# usage: ./reach-usage-report-csv.sh

HERE=$(dirname "$(realpath "$0")")
REPORT_FILE='report.csv'
USERS_FILE='user-report.csv'
HISTOGRAM_FILE='users-histogram.png'

# https://docs.docker.com/registry/spec/api/

UsageReport=$("$HERE/reach-usage-report.sh")

echo 'report_date,pull_count,row_count' > $REPORT_FILE
echo "$UsageReport" | jq -r '[.report_date, .repository_stats.pull_count, .CompileLog.row_count ] | @csv' >> $REPORT_FILE
echo "Wrote report to $REPORT_FILE"

echo 'date,unique_users' > $USERS_FILE
echo "$UsageReport" | jq -r '.CompileLog.unique_users | to_entries | map([.key, (.value | map(.userId.S) | unique | length)]) | map(join(", ")) | join("\n")' >> $USERS_FILE
echo "Wrote user report to $USERS_FILE"

gnuplot -e "filename='$HISTOGRAM_FILE'" -e "datafile='$USERS_FILE'" "gnuplot.txt"
echo "Generated $HISTOGRAM_FILE"
