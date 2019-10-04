.PHONY: all
all: check test

.PHONY: check
check:
	@echo Consistent Solidity Versions
	@ag --ignore ./Makefile --ignore-dir docs --nogroup " solidity "
	@echo Find TODO/XXX
	@ag --ignore ./Makefile '(XXX|TODO)'

.PHONY: run-all
run-all:
	cd js && $(MAKE) run
	cd examples/rps && $(MAKE) run
	cd examples/multisig && $(MAKE) run

.PHONY: build-all
build-all:
	cd hs && $(MAKE) build
	cd scripts/ethereum-devnet && $(MAKE) build
	cd js && $(MAKE) build
	cd examples/rps && $(MAKE) clean build
	cd examples/multisig && $(MAKE) clean build

.PHONY: push-all
push-all:
	cd hs && $(MAKE) push
	cd scripts/ethereum-devnet && $(MAKE) push
	cd js && $(MAKE) push
	cd examples/rps && $(MAKE) push
	cd examples/multisig && $(MAKE) push

.PHONY: publish-all
publish-all:
	cd hs && $(MAKE) publish
	cd js && $(MAKE) publish
	cd examples/rps && $(MAKE) publish
	cd examples/multisig && $(MAKE) publish
