.PHONY: all
all: check rps

.PHONY: check
check:
	@echo Consistent Solidity Versions
	@ag --ignore ./Makefile --ignore-dir docs --nogroup " solidity "
	@ echo Find TODO/XXX
	@ag --ignore ./Makefile --ignore package-lock.json '(XXX|TODO)'

.PHONY: rps
rps:
	cd examples/rps && $(MAKE) all
