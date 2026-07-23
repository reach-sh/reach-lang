# verified-solidity

An end-to-end example of the standalone verified-Solidity workflow:
write a contract as a Reach program, let the compiler formally verify it, and consume the emitted Solidity with ordinary Ethereum tooling.

## Compile

```sh
REACH_DOCKER=0 ../../reach sol index.rsh
# or, with reachc on PATH:
../../scripts/reach-sol index.rsh
```

On success `build/` contains exactly three artifacts:

- `index.main.sol` — self-contained Solidity source (solc 0.8.26, `evmVersion: paris`)
- `index.main.abi.json` — the contract ABI
- `index.main.verify.json` — machine-readable verification report (`vr_verified`, theorem counts, failures with counterexample witnesses)

If verification fails, the compile exits nonzero and no `.sol` is emitted.

## Use with Foundry

`foundry-test/run.sh` deploys the emitted contract under Foundry's `forge` (skipped when `forge` is not installed):

```sh
./foundry-test/run.sh
```

## CI consumption

```sh
python3 -c "import json; d = json.load(open('build/index.main.verify.json')); assert d['vr_verified'] and d['vr_theoremCount'] > 0"
```
