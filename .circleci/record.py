#!/usr/bin/env python3
import os
import json
from pathlib import Path
from itertools import filterfalse

def env(k):
    return os.environ[k]

conns = [ 'ETH', 'ALGO', 'CFX' ]
total = 0
cfails = {}
for c in conns:
    cfails[c] = []  # the list of examplename that have failed for CONN c
time = set() # the set of CONN.examplename that timed out
fail = set() # the set of CONN.examplename that exited nonzero (and not via timeout)
urls = {}

proj_dir = Path("/home/circleci/project")
ex_dir = proj_dir / 'examples'
rec_dir = Path("/tmp/workspace/record")
art_dir = Path("/tmp/workspace/artifacts")

arts = {}
for ap in sorted(art_dir.iterdir()):
    if not ap.is_file():
        continue
    o = { "items": [] }
    with open(ap) as af:
        o = json.load(af)
    me = str(ap).replace(f"{art_dir}/", "")
    m = {}
    for r in o["items"]:
        m[r["path"]] = r["url"]
    arts[me] = m

for ep in sorted(ex_dir.iterdir()):
    if not ep.is_dir():
        continue
    me = str(ep).replace(f"{ex_dir}/", "")
    for c in conns:
        total += 1
        cme = f"{c}.{me}"
        o = [ "fail", False ]
        rp = rec_dir / cme
        if rp.is_file():
            with open(rp) as rf:
                o = json.load(rf)
        u = o[1]
        if u: u = arts.get(u)
        if u: u = u.get(f"tmp/artifacts/{cme}.gz")
        urls[cme] = u
        stat = o[0]
        if stat == "fail-time":
            time.add(cme)
        if stat == "fail":
            fail.add(cme)
        if stat.startswith("fail"):
            cfails[c].append(me)

def no_blank(x): return not x or x.startswith("#") or x.isspace()
def read_to_set(fp):
    with open(fp, 'r') as f:
        return set(filterfalse(no_blank, f.read().splitlines()))

source_dir = proj_dir / '.circleci'
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

SYM = ":jayparfait: OKAY"
PRE = f"{total} passed!"
POST = ""

if ftc > 0:
    SYM = ":warning: FAIL"
    PRE = f"{ftc} of {total} failed!"
    if xftc > 0:
        PRE += f" ({xftc} expected)"

for c in conns:
    def fmte(e):
        ce = f"{c}.{e}"
        x = e
        u = urls[ce]
        if u: x = f"<{u}|{x}>"
        if ce in nxft: x = f"*{x}*"
        if ce in time: x = f"{x} (t)"
        return x
    tfails = cfails[c]
    tfailc = len(tfails)
    if tfailc > 0:
        upto = 10
        msg = ' '.join(map(fmte, tfails[:upto]))
        if tfailc > upto:
            msg += ' (+ ' + str(tfailc - upto) + ' more not shown)'
        POST += f"\\n- *{c}* {tfailc}: {msg}"

EXIT = 0
POST += "\\n*"
if nxftc > 0:
    POST += f":warning: DANGER "
    EXIT = 1
else:
    POST += f":pizza: "
if (env('CIRCLE_BRANCH') == 'master'):
    POST += "RELEASE"
else:
    POST += "MERGE"
POST += f"*: `{env('CIRCLE_SHA1')[:8]}`"

def truncate_message(msg):
    # 3000 character limit for text field in block api
    # https://api.slack.com/reference/block-kit/blocks#section_fields
    trunc_msg = "... message truncated"
    if len(msg) <= 3000:
        return msg
    else:
        new_len = 3000 - len(trunc_msg)
        return msg[:new_len] + trunc_msg

username = env('CIRCLE_USERNAME')
branch = env('CIRCLE_BRANCH')
branch_url = f"https://github.com/reach-sh/reach-lang/tree/{branch}"
pr = env('CIRCLE_PULL_REQUEST')
build = env('CIRCLE_BUILD_URL')

circle_message = f"*{SYM}* {username}/<{branch_url}|{branch}>/<{pr}|PR> > examples: {PRE} <{build}|more...>{POST}"
circle_message = truncate_message(circle_message)
print(f'export RECORD_MESSAGE="{circle_message}"')

exit(EXIT)
