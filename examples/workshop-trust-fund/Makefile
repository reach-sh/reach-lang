REACH=../../reach

.PHONY: build
build:
	$(REACH) compile index.rsh

.PHONY: run
run:
#	REACH_CONNECTOR_MODE=FAKE-test-embedded   $(REACH) run index
	REACH_CONNECTOR_MODE=ETH-test-embedded    $(REACH) run index
#	REACH_CONNECTOR_MODE=ETH-test-dockerized  $(REACH) run index
