#!/usr/bin/env python3
import os
import json
from pathlib import Path
from itertools import filterfalse

def env(k):
    return os.environ[k]

conns = [ 'ETH', 'ALGO', 'CFX' ]

bad = set()
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
        for c in conns:
            cme = f"{c}.{me}"
            k = f'{c}_STATUS'
            if p[k] == "fail-time":
                time.add(cme)
            if p[k].startswith("fail"):
                cfails[c].append(me)
                bad.add(cme)

source_dir = Path(__file__).resolve().parent
wlp = source_dir / 'whitelist.txt'
wlf = open(wlp, "r")
def no_hash(x): return x.startswith("#")
whitelist = set( filterfalse(no_hash, wlf.read().splitlines()) )
blacklist = bad - whitelist

badc = len(bad)
blackc = len(blacklist)
nblackc = badc - blackc
total = len(recs) * len(conns)

SYM = "OKAY"
PRE = f"{total} passed!"
POST = ""

if badc > 0:
    SYM = "FAIL"
    PRE = f"{badc} of {total} failed!"
    if nblackc > 0:
        PRE += f" ({nblackc} expected failures)"

for c in conns:
    def fmte(e):
        x = f"<{urls[e]}|{e}>"
        ce = f"{c}.{e}"
        if ce in blacklist: x = f"*{x}*"
        if ce in time: x = f"{x}(t)"
        return x
    tfails = cfails[c]
    tfailc = len(tfails)
    if tfailc > 0:
        msg = ' '.join(map(fmte, tfails))
        POST += f"\n- *{c}* {tfailc}: {msg}"

EXIT = 0
POST += "\n*"
if blackc > 0:
    POST += f"DO NOT "
    EXIT = 1
POST += "RELEASE*"

# XXX make branch a link
print(f"export RECORD_MESSAGE='*{SYM}* {env('CIRCLE_USERNAME')}/{env('CIRCLE_BRANCH')} > examples: {PRE} <{env('CIRCLE_BUILD_URL')}|more...>{POST}'")

exit(EXIT)
