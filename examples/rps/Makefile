.PHONY: all build test start_geth stop_geth logs

export STACK_YAML=../../hs/stack.yaml

all: start_geth test demo stop_geth

build: build/rps.mjs build/rps_while.mjs

clean:
	rm -f build/rps.mjs build/rps_while.mjs

build/%.mjs: rsh/%.rsh
	stack build
	mkdir -p build
	stack exec -- reachc -o build $^

start_geth:
	@../../scripts/ethereum-devnet/run.sh

stop_geth:
	@killall geth

test: build
	npm test

logs:
	@tail -f ../../.ethereum/geth-data/logs/testnet.log

demo: build
	npm run pretest
	npm run demo
