REACH=../../reach

.PHONY: build
build:
	$(REACH) compile index.rsh

.PHONY: run
run:
	REACH_CONNECTOR_MODE=FAKE $(REACH) run index
#	REACH_CONNECTOR_MODE=ETH  $(REACH) run index  # why does this hang?

