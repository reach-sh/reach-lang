All "local hs build" make targets are prefixed with "hs-".
These require that `stack` is installed on your local machine.

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
../../reach compile src.rsh ident -o build
../../hs/sbin/prof2svg.sh reachc.prof
```
