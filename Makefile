.PHONY: all
all: check test

.PHONY: check
check:
	@echo Consistent Solidity Versions
	@ag --ignore ./Makefile --ignore-dir docs --nogroup " solidity "
	@echo Find TODO/XXX
	@ag --ignore ./Makefile --ignore package-lock.json '(XXX|TODO)'

.PHONY: test
test: devnet
	cd js && npm test
	cd examples/rps && make clean build test demo

.PHONY: devnet
devnet:
	[ `docker inspect -f '{{.State.Running}}' devnet` = "true" ] || docker run --name devnet -d -p 8545:8545 reachsh/ethereum-devnet:v0.1.0

.PHONY: clean-images
clean-images:
	docker rmi $(docker images --filter "dangling=true" -q --no-trunc)
