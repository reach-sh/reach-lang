dead = exp/ auction/
examples = $(filter-out $(dead), $(sort $(dir $(wildcard */.))))

.PHONY: build-all
build-all:
	(set -e; \
	for example in $(examples) ; do \
		(cd $$example && (if [ -f Makefile ] ; then $(MAKE) build ; else ../../reach compile ; fi));\
	done)

.PHONY: run-all
run-all:
	(set -e; \
	for example in $(examples); do \
		(cd $$example && (if [ -f Makefile ] ; then $(MAKE) run down ; else ../../reach run ; fi)); \
	done)

.PHONY: clean-all
clean-all:
	(set -e; \
	for example in $(examples) ; do \
		(cd $$example && ([ ! -f Makefile ] || $(MAKE) clean)); \
	done)
