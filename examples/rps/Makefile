.PHONY: all build test lint start_geth stop_geth logs

export STACK_YAML=../../hs/stack.yaml

all: lint start_geth test demo stop_geth

build: build/rps.mjs build/rps_while.mjs

clean:
	rm -f build/rps.mjs build/rps_while.mjs

build/%.mjs: rps/%.rsh
	stack build
	mkdir -p build
	stack exec -- reachc -o build $^

lint:
	@`npm bin`/eslint -c .eslintrc.yaml "rps/**/*.mjs"

start_geth:
	@../../scripts/ethereum-devnet/run.sh

stop_geth:
	@killall geth

test: build
	@node --experimental-modules rps/spec/index.mjs

logs:
	@tail -f ../../.ethereum/geth-data/logs/testnet.log

demo: build
	@node --experimental-modules rps/demo-run-stdout.mjs
