examples = rps nim multisig

.PHONY: build-all
build-all:
	(set -e; for example in $(examples) ; do (cd $$example && $(MAKE) build) ; done)

.PHONY: run-all
run-all:
	(set -e; \
	for example in $(examples); do \
		(cd $$example && $(MAKE) run down); \
	done)

.PHONY: clean-all
clean-all:
	(set -e; for example in $(examples) ; do (cd $$example && $(MAKE) clean) ; done)

.PHONY: push-all
push-all:
	(set -e; for exapmle in $(examples) ; do (cd $$example && $(MAKE) push) ; done)

.PHONY: publish-all
publish-all:
	(set -e; for exapmle in $(examples) ; do (cd $$example && $(MAKE) publish) ; done)
