#!/usr/bin/env python3
import sys
import csv

data={}

with open(sys.argv[1]) as csv_file:
    reader = csv.reader(csv_file)
    next(reader)
    for row in reader:
        dk = f"{row[0]}-{row[1]}"
        if not (dk in data):
            data[dk] = {
               "bn": int(row[0])*12+int(row[1]),
               "true-true": 0,
               "true-false": 0,
               "false-true": 0,
               "false-false": 0,
            }
        d = data[dk]
        k = f"{row[2]}-{row[3]}"
        count = int(row[4])
        d[k] = count

for k in data:
    d = data[k]
    bn = d["bn"]
    rshTxn = d["false-true"]
    allTxn = d["false-false"] + rshTxn
    rshApp = d["true-true"]
    allApp = d["true-false"] + rshApp
    print(f"{k},{bn},_,_,{rshTxn},{allTxn},{rshApp},{allApp}")
