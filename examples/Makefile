examples = rps nim multisig

.PHONY: build-all
build-all:
	(set -e; for example in $(examples) ; do (cd $$example && $(MAKE) build) ; done)

.PHONY: run-all
run-all:
	(set -e; \
	for example in $(examples); do \
		(cd $$example && \
		$(MAKE) run || \
		(echo "trying again..." && $(MAKE) run); \
		docker-compose down); \
	done)
# try examples twice because if they fail once, they seem to pass the 2nd time

.PHONY: clean-all
clean-all:
	(set -e; for example in $(examples) ; do (cd $$example && $(MAKE) clean) ; done)
