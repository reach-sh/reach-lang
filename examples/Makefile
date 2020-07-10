examples = rps nim multisig

.PHONY: build-all
build-all:
	(set -e; for example in $(examples) ; do (cd $$example && $(MAKE) build) ; done)

.PHONY: run-all
run-all:
	(set -e; for example in $(examples) ; do (cd $$example && $(MAKE) run) ; done)

.PHONY: clean-all
clean-all:
	(set -e; for example in $(examples) ; do (cd $$example && $(MAKE) clean) ; done)
