#!/bin/sh
# Deploy the emitted Solidity under Foundry.  CI-optional: exits 0 with a
# notice when forge is not installed.
set -eu

cd "$(dirname "$0")/.."

if ! command -v forge >/dev/null 2>&1; then
  echo "forge not installed; skipping Foundry test"
  exit 0
fi

if [ ! -f build/index.main.sol ]; then
  echo "build/index.main.sol missing; compile first (see README.md)" >&2
  exit 1
fi

rm -rf foundry-test/scratch
mkdir -p foundry-test/scratch/src foundry-test/scratch/test
cp build/index.main.sol foundry-test/scratch/src/
cp foundry-test/Deploy.t.sol foundry-test/scratch/test/

cat > foundry-test/scratch/foundry.toml <<EOF
[profile.default]
src = "src"
test = "test"
out = "out"
evm_version = "paris"
via_ir = true
optimizer = true
EOF

cd foundry-test/scratch
forge test -vv
