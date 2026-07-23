# {#guide-verified-solidity} Verified Solidity output

Reach can act as a standalone "verified Solidity" compiler for Ethereum developers.
In this workflow you author your contract as a Reach program, the compiler formally verifies it with Z3, and — only if verification passes — it emits a self-contained Solidity source file, its ABI, and a machine-readable verification report.
The output drops into standard Ethereum tooling (Foundry, Hardhat, ethers, viem) with no dependency on Reach's JavaScript runtime, Docker images, or the Algorand toolchain.

To be clear about the authoring model: you write Reach, not raw Solidity.
The verification runs on Reach's optimized intermediate representation; Solidity is the verified _output_.
Reach does not verify arbitrary hand-written Solidity.

## Compiling

Run the `sol` subcommand of the `reach` command-line tool:

```cmd
$ reach sol index.rsh
```

This is equivalent to invoking the compiler directly with the `--sol` flag:

```cmd
$ reachc --sol -o build index.rsh
```

The `scripts/reach-sol` wrapper does the same and additionally checks that `z3` and `solc` are on your `PATH` at the versions pinned in the repository's `DEPS` file.

In this mode the compiler forces the Ethereum connector regardless of the source's `connectors` option (it is an error if the source explicitly excludes `ETH`), skips the JavaScript backend, and never invokes the Algorand toolchain.
Because verification runs before code generation and a failure aborts compilation, the emitted Solidity is verified-or-absent by construction.
For the same reason, `--sol` refuses to run when verification has been disabled via the environment.

## The three artifacts

A successful compile writes exactly three files into the output directory:

+ `<src>.<app>.sol` — the self-contained Solidity source for your contract.
+ `<src>.<app>.abi.json` — the contract ABI, ready for ethers, viem, or `forge`.
+ `<src>.<app>.verify.json` — the verification report.

A failing compile exits nonzero, prints the usual counterexample ("Violation Witness") output, still writes `verify.json` (with `"vr_verified": false`) so CI can inspect the failure, and leaves no `.sol` behind.

## The verification report

`verify.json` is a stable, machine-readable summary of what the verifier checked:

+ `vr_verified` — whether every theorem was proven.
+ `vr_theoremCount`, `vr_succeeded`, `vr_failed`, `vr_timedOut`, `vr_omittedRepeats` — theorem counts.
+ `vr_modes` — one entry per honesty mode checked (e.g. "ALL participants are honest", "NO participants are honest"), each with its failure count.
+ `vr_failures` — one entry per failure, with the theorem kind, source location, optional message, a timeout flag, and the full counterexample witness text.

A minimal CI gate looks like:

```cmd
$ python3 -c "import json; d = json.load(open('build/index.main.verify.json')); assert d['vr_verified'] and d['vr_theoremCount'] > 0"
```

## Compatibility

The emitted Solidity is compiled and checked with the pinned `solc` (see `DEPS`) using `evmVersion: paris`.
The `paris` pin means the generated contract avoids the `PUSH0` opcode, so it deploys on pre-Shanghai chains and alt-EVM networks as well as current Ethereum mainnet.
When consuming the `.sol` with your own tooling, configure the same EVM target (for example `evm_version = "paris"` in `foundry.toml`).

See `examples/verified-solidity/` in the repository for an end-to-end example that compiles a Reach program and deploys the emitted contract under Foundry.

## Future work

A true Solidity frontend — parsing existing `.sol` files into Reach's verified intermediate representation — is a research-scale project and out of scope.
Other planned directions include SARIF output for the verification report and analogous single-connector output modes for other networks.
