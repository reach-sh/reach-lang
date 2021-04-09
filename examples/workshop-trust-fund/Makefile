REACH=../../reach

.PHONY: build
build:
	$(REACH) compile index.rsh

.PHONY: run
run:
	REACH_CONNECTOR_MODE=ETH  $(REACH) run
	REACH_CONNECTOR_MODE=ALGO REACH_DEBUG=1 $(REACH) run
