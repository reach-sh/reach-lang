#!/usr/bin/env python3
import subprocess

jobs={}
def check(d):
    print(f"waiting for {d}...")
    dp = jobs[d]
    dp.wait()
    if dp.returncode != 0:
        print(f"{d} failed...")
        exit(1)

def image(which, deps):
    for d in deps:
        check(d)
    print(f"{which} starting...")
    jobs[which] = subprocess.Popen(["./image.sh", which])

image("devnet-algo", [])
image("devnet-eth", [])
image("devnet-cfx", [])
image("haskell-build-artifacts", [])
image("reach", ["haskell-build-artifacts"])
image("reach-cli", ["haskell-build-artifacts"])
image("js-deps", [])
image("stdlib", ["reach", "js-deps"])
image("runner", ["stdlib"])
image("react-runner", ["stdlib", "js-deps"])
image("rpc-server", ["runner"])

print(f"All scheduled...")
for d in jobs:
    check(d)
    subprocess.run(["./save-image.sh", d])
