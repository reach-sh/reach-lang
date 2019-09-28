.PHONY: all
all: check test

.PHONY: check
check:
	@echo Consistent Solidity Versions
	@ag --ignore ./Makefile --ignore-dir docs --nogroup " solidity "
	@echo Find TODO/XXX
	@ag --ignore ./Makefile '(XXX|TODO)'

.PHONY: test
test: build-all devnet
	cd js && $(MAKE) run
	cd examples/rps && $(MAKE) run

.PHONY: devnet
devnet:
	[ `docker inspect -f '{{.State.Running}}' devnet` = "true" ] || docker run --name devnet -d -p 8545:8545 reachsh/ethereum-devnet:v0.1.0

.PHONY: build-all
build-all:
	cd scripts/ethereum-devnet && $(MAKE) build
	cd js && $(MAKE) build
	cd examples/rps && $(MAKE) build

.PHONY: push-all
push-all:
	cd scripts/ethereum-devnet && $(MAKE) push
	cd js && $(MAKE) push
	cd examples/rps && $(MAKE) push

.PHONY: clean-all
clean-all:
	cd scripts/ethereum-devnet && $(MAKE) clean
	cd js && $(MAKE) clean
	cd examples/rps && $(MAKE) clean

.PHONY: clean-images
clean-images:
	docker rmi $(docker images --filter "dangling=true" -q --no-trunc)
