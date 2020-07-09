examples = rps nim multisig

.PHONY: build-all
build-all:
	for example in $(examples) ; do (cd $$example && $(MAKE) build) ; done

.PHONY: run-all
run-all:
	for example in $(examples) ; do (cd $$example && $(MAKE) run) ; done

.PHONY: clean-all
clean-all:
	for example in $(examples) ; do (cd $$example && $(MAKE) clean) ; done
