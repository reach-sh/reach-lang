# Repository Maturity Classification

**Date**: 2026-07-21
**Level**: 1 (Base)
**Justification**: Before this /prime run, AGENTS.md and manifest.yml were unfilled framework templates (placeholder FastAPI/Python values for a Haskell compiler monorepo) — effectively Level 0. AGENTS.md and manifest.yml now contain a real project overview, navigation, architecture, risk model, and discovered validation commands, which satisfies Level 1. Level 2 requires the documented test/lint/build commands to be verified working end-to-end; only the fast `make expand` check was executed (full `stack` builds exceed a reasonable bootstrap budget, and `shellcheck`/`hadolint`/`ag` are not installed locally, so `make sh-lint`, `make docker-lint`, and `make check` cannot run yet).

## What Exists
- Real AGENTS.md and manifest.yml (generated 2026-07-21 from repository scan)
- Extensive Makefile-driven build system (root, `hs/`, `js/`, `docs/`) with documented targets
- Haskell test suite with golden tests (`hs/test/`, `hs/t/`) and accept workflow (`hs-test-accept`)
- CI via CircleCI and GitHub Actions
- ~255 example DApps in `examples/` serving as integration tests
- `.claude/` skills and `.adws/` framework scaffolding
- Toolchain partially present locally: stack 3.3.1, z3 4.12.5, solc 0.8.26, mo, node 24; Docker installed but daemon not running

## What is Missing
- Verified-working validation baseline: `make hs-test` / `hs-build` not yet run locally (long stack build)
- Lint tooling not installed locally: shellcheck, hadolint, ag (the_silver_searcher)
- Local tool versions drift from `DEPS` pins (solc 0.8.17, z3 4.8.17 expected)
- Docker daemon not running — js/, docs/, and image builds unavailable

## Path to Next Level (Level 2)
- Install shellcheck, hadolint, and ag; run `make sh-lint` and record results
- Run `cd hs && make hs-build && make hs-test` once (accept the cold-build cost) and record pass/fail counts in `.agent/baseline.md`
- Align local solc/z3 with `DEPS` pins, or document the accepted drift
- Start the Docker daemon and verify `cd js && make b` and `cd docs && make build`
