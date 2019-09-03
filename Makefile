.PHONY: all
all: check rps

.PHONY: check
check:
	@echo Consistent Solidity Versions
	@ag --ignore ./Makefile --ignore-dir docs --nogroup " solidity "

.PHONY: rps
rps:
	cd examples/rps && $(MAKE) all
