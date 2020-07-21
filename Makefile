.PHONY: all
all: check run-all

.PHONY: check
check:
	@echo Consistent Solidity Versions
	@ag --ignore ./Makefile --ignore-dir docs --nogroup " solidity "
	@echo Find TODO/XXX
	@ag --ignore ./Makefile --ignore-dir docs '(xxx|todo|fixme)'

.PHONY: todo
todo:
	@ag --color --no-break --ignore todo.html --ignore ./Makefile --ignore-dir docs \
		'(xxx|todo|fixme)' | aha --black > todo.html

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

.PHONY: rebuild-and-run-all-examples
rebuild-and-run-all-examples:
	cd examples && time $(MAKE) clean-all build-all run-all

.PHONY: rbe
rbe: rebuild-and-run-all-examples

.PHONY: docs
docs:
	cd docs-src && $(MAKE)
