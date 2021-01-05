REACH=../../reach

.PHONY: build
build:
	$(REACH) compile index.rsh

.PHONY: run
run:
#	REACH_CONNECTOR_MODE=FAKE $(REACH) run
	REACH_CONNECTOR_MODE=ETH  $(REACH) run
	REACH_CONNECTOR_MODE=ALGO $(REACH) run
