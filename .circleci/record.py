#!/usr/bin/env python3
import os
import json
from pathlib import Path
from itertools import filterfalse

def env(k):
    return os.environ[k]

# XXX Change this file so that it knows it is looking for conns x examples so
# it can "tell" if something is missing, rather than just looking at the
# records that happen to show up

conns = [ 'ETH', 'ALGO', 'CFX' ]

cfails = {}
for c in conns:
    cfails[c] = []  # the list of examplename that have failed for CONN c
time = set() # the set of CONN.examplename that timed out
fail = set() # the set of CONN.examplename that exited nonzero (and not via timeout)

DIR = "/tmp/workspace/record"
recs = sorted(Path(DIR).iterdir())
urls = {}
for rp in recs:
    with open(rp) as rf:
        o = json.load(rf)
        cme = str(rp).replace(f"{DIR}/", "")
        cmel = cme.split(".")
        c = cmel[0]
        me = cmel[1]
        urls[cme] = o[1]
        stat = o[0]
        if stat == "fail-time":
            time.add(cme)
        if stat == "fail":
            fail.add(cme)
        if stat.startswith("fail"):
            cfails[c].append(me)

source_dir = Path(__file__).resolve().parent
def no_blank(x): return not x or x.startswith("#") or x.isspace()
def read_to_set(fp):
    with open(fp, 'r') as f:
        return set(filterfalse(no_blank, f.read().splitlines()))

# x = expected
xfail = read_to_set(source_dir / 'xfail.txt')
xtime = read_to_set(source_dir / 'xtime.txt')

# nx = unexpected
nxfail = fail - xfail
nxtime = time - xtime
nxft = nxfail.union(nxtime)
nxftc = len(nxft)

ftc = len(fail.union(time))
xftc = ftc - nxftc
total = len(recs)

SYM = "OKAY"
PRE = f"{total} passed!"
POST = ""

if ftc > 0:
    SYM = "FAIL"
    PRE = f"{ftc} of {total} failed!"
    if xftc > 0:
        PRE += f" ({xftc} expected)"

for c in conns:
    def fmte(e):
        ce = f"{c}.{e}"
        x = f"<{urls[ce]}|{e}>"
        if ce in nxft: x = f"*{x}*"
        if ce in time: x = f"{x} (t)"
        return x
    tfails = cfails[c]
    tfailc = len(tfails)
    if tfailc > 0:
        upto = 20
        msg = ' '.join(map(fmte, tfails[:upto]))
        if tfailc > upto:
            msg += ' (+ ' + str(tfailc - upto) + ' more not shown)'
        POST += f"\\n- *{c}* {tfailc}: {msg}"

EXIT = 0
POST += "\\n*"
if nxftc > 0:
    POST += f":warning: DO NOT "
    EXIT = 1
else:
    POST += f":pizza: "
if (env('CIRCLE_BRANCH') == 'master'):
    POST += "RELEASE"
else:
    POST += "MERGE"
POST += f"*: `{env('CIRCLE_SHA1')[:8]}`"

# XXX make branch a link
print(f"export RECORD_MESSAGE='*{SYM}* {env('CIRCLE_USERNAME')}/{env('CIRCLE_BRANCH')} > examples: {PRE} <{env('CIRCLE_BUILD_URL')}|more...>{POST}'")

exit(EXIT)
