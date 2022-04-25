#!/bin/bash

# Requires curl, awscli, gnuplot and jq
# usage: ./reach-usage-report-csv.sh

HERE=$(dirname "$(realpath "$0")")
REPORT_FILE='report.csv'
USERS_FILE='user-report.csv'
LOCATION_FILE='location.csv'
HISTOGRAM_FILE='users-histogram.png'

# https://docs.docker.com/registry/spec/api/

UsageReport=$("$HERE/reach-usage-report.sh")

echo 'report_date,pull_count,row_count' > $REPORT_FILE
echo "$UsageReport" | jq -r '[.report_date, .repository_stats.pull_count, .CompileLog.row_count ] | @csv' >> $REPORT_FILE
echo "Wrote report to $REPORT_FILE"

echo 'date,unique_users' > $USERS_FILE
echo "$UsageReport" | jq -r '.CompileLog.unique_users
  | to_entries
  | map([.key, (.value | map(.userId.S) | unique | length) ])
  | map(join(",")) | join("\n")' >> $USERS_FILE
echo "Wrote user report to $USERS_FILE"

echo 'month,country,region,count' > $LOCATION_FILE
echo "$UsageReport" | jq -r '.CompileLog.unique_users
  | to_entries
  | map(.key as $month
    | .value
    | map(select(.geoCountry != null and .geoRegion != null))
    | group_by(.geoCountry.S, .geoRegion.S)
    | map(. as $ex
      | { month:   $month
        , country: .[0] | .geoCountry.S
        , region:  .[0] | .geoRegion.S
        , count:   ($ex | length)
        }))
  | flatten
  | map(join(",")) | join("\n")' >> $LOCATION_FILE

gnuplot -e "filename='$HISTOGRAM_FILE'" -e "datafile='$USERS_FILE'" "gnuplot.txt"
echo "Generated $HISTOGRAM_FILE"

# Print quick update on command line
echo "Quick Report"
echo -n "  Downloads: "
echo "$UsageReport" | jq -r '.repository_stats.pull_count'
echo -n "  Row count: "
echo "$UsageReport" | jq -r '.CompileLog.row_count'
echo -n "  Total Unique Users: "
echo "$UsageReport" | jq -r '.CompileLog.unique_users | flatten | map(.userId.S) | unique | length'
