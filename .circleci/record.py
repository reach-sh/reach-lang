#!/usr/bin/env python3
import os
import json
from pathlib import Path
from itertools import filterfalse

def env(k):
    return os.environ[k]

conns = [ 'ETH', 'ALGO', 'CFX' ]

bad = set()
fails = []
cfails = {}
for c in conns:
    cfails[c] = []
time = set()

DIR = "/tmp/workspace/record"
recs = sorted(Path(DIR).iterdir())
urls = {}
for rp in recs:
    with open(rp) as rf:
        o = rf.read()
        # Convert `export K=V\n...` into `{ "K":V, ... }
        m = o.replace("=", "\":").replace("export ", "\"").replace("\"\n", "\",")
        p = json.loads("{" + m + " \"okay\": true}")
        me = str(rp).replace(f"{DIR}/examples.", "")
        urls[me] = p['EXAMPLE_URL']
        def add_fail(k, l, cme=None):
            if p[k] == "fail-time":
                if cme: time.add(cme)
            if p[k].startswith("fail"):
                l.append(me)
                if cme: bad.add(cme)
        add_fail('STATUS', fails)
        for c in conns:
            cme = f"{c}.{me}"
            add_fail(f'{c}_STATUS', cfails[c], f"{c}.{me}")

# These are allowed to fail and we still release
whitelist = { "ALGO.ttt" }
blacklist = bad - whitelist

total = len(recs)
SYM = "OKAY"
PRE = f"{total} passed!"
POST = ""

failc = len(fails)
if failc > 0:
    SYM = "FAIL"
    PRE = f"{failc} of {total} failed!"

for c in conns:
    def fmte(e):
        x = f"<{urls[e]}|{e}>"
        ce = f"{c}.{e}"
        if ce in blacklist: x = f"*{x}*"
        if ce in time: x = f"{x}(t)"
        return x
    tfails = cfails[c]
    tfailc = len(tfails)
    # XXX elide when too long
    if tfailc > 0:
        msg = ' '.join(map(fmte, tfails))
        POST += f"\n*{c}* {tfailc}: {msg}"

EXIT = 0
POST += "\n"
if len(blacklist) > 0:
    POST += f"DO NOT "
    EXIT = 1
POST += "RELEASE"

# XXX make branch a link
print(f"export RECORD_MESSAGE='*{SYM}* {env('CIRCLE_USERNAME')}/{env('CIRCLE_BRANCH')} > examples: {PRE} <{env('CIRCLE_BUILD_URL')}|more...>{POST}'")

exit(EXIT)
