.PHONY: all rps
all: rps

rps:
	cd examples/rps && $(MAKE) all
