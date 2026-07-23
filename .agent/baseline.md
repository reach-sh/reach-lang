# Validation Baseline

**Date**: 2026-07-21 (updated after full build + test runs)
**Maturity Level**: 1 (see .agent/maturity.md)

## Test Results
- `cd hs && make hs-build`: **PASS** — full stack build on lts-19.7/GHC 9.0.2, 186 actions, reachc/reach/reach-test installed.
- `cd hs && make hs-test` (run 1, no `goal` on PATH): **464/806 FAIL** — 452 failures were `goal: createProcess` (missing Algorand `goal` binary; reachc shells out to `goal clerk compile` to assemble TEAL, see hs/src/Reach/Connector/ALGO.hs:3466). Fixed via a Docker-backed `goal` shim (requires Docker daemon + `reachsh/devnet-algo` image; the stock `scripts/goal-devnet` mounts only `$PWD`, which breaks reachc's absolute paths — the shim mounts the repo and TMPDIR at identical container paths).
- Remaining ~12 failures: z3 model-value drift — `t/n` golden outputs embed verification counterexample witness values, and local z3 4.12.5 picks different (semantically equivalent) models than the pinned 4.8.17. Resolved by `hs-test-accept` as part of the z3 version bump.
- Run 2 (with goal shim): **14/806 FAIL (2137s)** — all benign version drift, no functional regressions:
  - 12 × `t/n` goldens: z3 4.12.5 picks different (semantically equivalent) counterexample witness values than 4.8.17.
  - 2 × `t/y` goldens (`gh-1183`, `tooBig`): goldens embed bytecode sizes in "exceeds the maximum limit" warnings; solc 0.8.26 produces *smaller* bytecode in every case (e.g. 45328→41952, 49068→48445).
- Phase 2+3 landed: `DEPS` pinned to SOLC_VERSION=0.8.26 / Z3_VERSION=4.12.5; `Version.hs` regenerated via `make expand`; the 14 drifted goldens accepted via `make hs-test-accept` (12 `t/n` + `t/y/gh-1183.txt` + `t/y/tooBig.txt`); confirmation run: **806/806 PASS (2051s)** — no `t/n` flapping observed, so z3 4.12.5 counterexample models appear stable across runs on this machine. Phase 2+3 complete, uncommitted.
- **Accept-run hazard (learned the hard way)**: if Docker (goal) is down, `make hs-test-accept` silently **corrupts `t/n` goldens** — those tests expect reachc to fail, so the "TEAL compiler failed / docker API" crash counts as failure and tasty-golden accepts the Docker error text as the new expected output (5 goldens corrupted, reverted via `git checkout`). Always `docker info` before any accept run, and `git diff hs/t | grep docker` after.
- Local env gotcha: `/usr/local/bin/mo` crashes under macOS bash 3.2 (`MO_FUNCTION_CACHE_HIT[@]: unbound variable` — empty-array expansion under `set -u`, fixed in bash 4.4). A failed `mo` run leaves the target file EMPTY (shell `>` truncates before mo crashes), and make then considers it up to date — `rm` the target before retrying. Workaround: patched mo copy in the session scratchpad `bin/` (guards the two unguarded `"${MO_FUNCTION_CACHE_*[@]}"` loops); durable fix is `brew install bash` or upgrading mo.
- Quick sanity check `cd hs && make expand`: **PASS** — generated `hs/package.yaml`, `hs/src/Reach/Version.hs`, and `hs/sol/stdlib.sol` (including OpenZeppelin expansion) with exit code 0.
- solc 0.8.26 compatibility probe: **PASS** — Reach's exact standard-json settings (viaIR, optimizer, revertStrings strip, bytecodeHash none) accepted by solc 0.8.26; the custom Yul optimizer step sequence in ETH_solc.hs is dead code (opSpecialSeq=False in all active policies).

## Verified-Solidity feature (specs/verified-solidity-compiler.md, 2026-07-21)
- Feature landed: `reachc --sol` / `reach sol` / `scripts/reach-sol` / `make reachc-dist`; suite grew 806 → 812 (t/y/sol_only, t/n/sol_only_vfail, examples/verified-solidity golden, 3 unit specs for verify.json shape + flag parsing).
- Full run 1: 452/812 fail — goal shim regression: the recreated shim lacked `-w "$PWD"`, so goal saw relative `./build/*.teal` paths against the image workdir. Shim fixed (mount repo + `/private/var/folders` (both spellings) + `/private/tmp`, and `-w "$PWD"`); a from-scratch shim needs BOTH the identical-path mounts AND the cwd passthrough.
- Full run 2: 810/812 (1939s) — only failures were the two paris-pin bytecode-size goldens predicted in Phase 4 (`t/y/tooBig`, `t/y/gh-1183`); re-accepted (diff audited: size integers only, no docker text), both green individually. Confirmation run: in progress at time of writing.
- `make hs-format` BROKEN under lts-20.26: the google/ormolu "gfork" git pin's extra-deps (ghc-lib-parser 8.10.7, optparse-applicative 0.16.1) predate the resolver — `stack build ormolu` fails before formatting. Pre-existing branch issue (stack.yaml fork pain point); new code styled by hand to match surroundings.
- `make sh-lint`: shellcheck now installed (brew); new scripts (`scripts/reach-sol`, `examples/verified-solidity/foundry-test/run.sh`) are shellcheck-clean; repo-wide sh-lint fails only on the untracked `AgenticEngineeringFramework/` scripts (pre-existing, not in git).
- Foundry (`forge` 0.8.33 toolchain) IS installed on this machine; `examples/verified-solidity/foundry-test/run.sh` deploy test passes against the emitted `.sol`.

## Phase 4: stack LTS upgrade
- Hop 1, lts-19.7 → lts-20.26 (GHC 9.0.2 → 9.2.8): **hs-build PASS** (184 actions), **hs-test 806/806 PASS (2285s)**. Zero source changes needed — only stack.yaml: resolver bump, dropped 5 stale hackage pins now in the snapshot (megaparsec, streaming-bytestring, selective, old tomland/validation-selective), re-pinned tomland-1.3.3.3 + validation-selective-0.2.0.0 (tomland left Stackage). All 5 git forks compile unchanged under GHC 9.2.8.
- Deploy smoke test (ETH-devnet, examples/overview via published runner:0.1.13 images + docker-compose shim): round 1 **FAIL — real finding**: solc >=0.8.20 defaults to the shanghai EVM target and emits PUSH0, which the bundled 2022-era devnet-eth geth (and any pre-shanghai/alt-EVM chain) rejects with `invalid opcode: PUSH0` at eth_estimateGas. Everything up to that point worked (local CLI, compose, runner stdlib, account funding, deploy tx construction). Fix: pin `evmVersion: "paris"` in the solc standard-json settings (hs/src/Reach/Connector/ETH_solc.hs — HIGH-RISK AREA, needs human review). Note: goldens embedding bytecode sizes (tooBig, gh-1183) drift under paris codegen; re-accepted.
- Round 2 (2026-07-22, paris fix in, reachc rebuilt on GHC 9.2.8): **PASS** — `REACH_DOCKER=0 REACH_CONNECTOR_MODE=ETH-devnet reach run` on examples/overview deployed and ran the full Alice/Bob interaction on devnet-eth:0.1.13; no PUSH0 rejection (round 1 died at eth_estimateGas before any interaction, so a successful deploy is the positive check). Foundry deploy test (`examples/verified-solidity/foundry-test/run.sh`, evm_version=paris) also PASS on freshly regenerated artifacts (8 theorems, no failures).

- Hop 1 + paris fix committed 2026-07-22: `920e5ad28` (lts-20.26 resolver), `0885e2396` (paris pin + re-accepted size goldens, flagged for human review).
- `make hs-format` FIXED (`9454fe180`): ormolu gfork now builds in isolated `hs/ormolu-tool/stack.yaml` at lts-18.28 (its native GHC 8.10.7 era) — decoupled from the compiler resolver ladder permanently. Root cause of the old break: gfork needs ghc-lib-parser 8.10.x; stack's suggested pins would have downgraded reach's own optparse-applicative. EverestUtil.hs excluded from the sweep (mid-expression #ifdef branches can never parse under ormolu's CPP masking). hs-format verified idempotent. Accumulated hand-style drift reformatted in `50fbabdf8` (58 files, mechanical only; files carrying uncommitted feature work left out).
- Hop 2, lts-20.26 → lts-21.25 (GHC 9.2.8 → 9.4.8), committed `64147a799`: **hs-build PASS** (188 actions, zero source changes, all forks + -Werror clean), **hs-test 812/812 PASS (2620s)**, zero golden drift. Dry-run needed no pin changes.
- Hop 3, lts-21.25 → lts-22.44 (GHC 9.6.7), committed `56bf33153` (+ pragma-order fixup `eb9981139`): **hs-build PASS**, **hs-test 812/812 PASS (2425s)**, zero golden drift. Mechanical API migrations required (12 rounds of iterative builds): mtl 2.3 Control.Monad re-export removal (12 files net), base16-1.0 renames (Eval/Core decodeBase16Untyped; Backend/JS extractBase16 — emitted JS byte-identical), scotty 0.20 (ActionT arity, MonadUnliftIO for WebM via new unliftio-core dep, documented -Wno-deprecations for param/raise fall-through), optparse-applicative 0.18 prettyprinter migration (monomorphic text shim in app/reach/Main.hs). Post-hop validation on the 9.6.7 binary: local compile PASS, Foundry deploy PASS, ETH-devnet deploy smoke PASS, hs-format idempotent vs HEAD.
- **Phase 4 COMPLETE (2026-07-22)**: lts-19.7 → lts-22.44, GHC 9.0.2 → 9.6.7, in three shippable resolver commits; all five fork pins survived (ormolu decoupled into hs/ormolu-tool/, others compile unchanged). README stack floor updated to v2.15+.

## Lint Results
- `make sh-lint`: **SKIPPED** — `shellcheck` not installed.
- `make docker-lint`: **SKIPPED** — `hadolint` not installed.
- `make check`: **SKIPPED** — `ag` (the_silver_searcher) not installed.

## Build Results
- `cd hs && make hs-build`: **SKIPPED** — long stack build; deferred.
- `cd js && make build`: **SKIPPED** — Docker daemon not running.
- `cd docs && make build`: **SKIPPED** — Docker daemon not running.

## Toolchain Audit (2026-07-21)
| Tool | Status | Pinned (DEPS) | Local |
|---|---|---|---|
| stack | present | v2.7.5 (README) | 3.3.1 |
| z3 | present | 4.8.17 | 4.12.5 |
| solc | present | 0.8.17 | 0.8.26 |
| mo | present | — | ok |
| node | present | 16.14 (images) | 24.11.1 |
| docker | installed, daemon NOT running | — | — |
| shellcheck | MISSING | — | — |
| hadolint | MISSING | — | — |
| ag | MISSING | — | — |
| goal | MISSING (symlink scripts/goal-devnet) | — | — |

## Action Items
- Install shellcheck, hadolint, ag: `brew install shellcheck hadolint the_silver_searcher`
- Symlink `scripts/goal-devnet` as `goal` on PATH (or install go-algorand)
- Reconcile solc/z3 versions with `DEPS` pins (relevant to the active `feat/solidity-update` branch)
- Start Docker daemon before js/docs/image builds
- Run `cd hs && make hs-build && make hs-test` once and update this baseline with pass/fail counts
