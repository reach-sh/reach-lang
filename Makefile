.PHONY: all
all: check test

.PHONY: check
check:
	@echo Consistent Solidity Versions
	@ag --ignore ./Makefile --ignore-dir docs --nogroup " solidity "
	@echo Find TODO/XXX
	@ag --ignore ./Makefile --ignore package-lock.json '(XXX|TODO)'

.PHONY: test
test: start_geth
	cd js && npm test
	cd examples/rps && make clean build test demo

.PHONY: start_geth
start_geth:
	@./scripts/ethereum-devnet/run.sh

.PHONY: stop_geth
stop_geth:
	@killall geth

.PHONY: logs
logs:
	@tail -f ./.ethereum/geth-data/logs/testnet.log
