All "local hs build" make targets are prefixed with "hs-".
These require that `stack` and `mo` is installed on your local machine.

# Pretend like you have goal installed locally

For whatever PROJ and BIN dirs you prefer:

```bash
(cd $PROJ/reach-lang/scripts/devnet-algo && make build)
ln -s $PROJ/reach-lang/scripts/goal-devnet $BIN/goal
```

# Build source

```bash
make hs-build
```

# Run the tests

```bash
make hs-test
```

# Run tests in "accept new golden values" mode

```bash
make hs-test-accept
```

# Run stan (Haskell STatic Analyzer)

```bash
make hs-check
```

# Clear out project-specific build artifacts

This is sometimes necessary to make stan happy, particularly because
the `.hie` folder is not automatically cleaned up.

```bash
make hs-clean
```

# Build haddocks

```bash
make hs-doc
```

# Run reachc and get a profiling flamegraph

```bash
# Note: rebuilds whole hs project for profiling
export REACHC_PROFILE=Y
../../reach compile src.rsh ident
../../hs/sbin/prof2svg.sh reachc.prof
```
