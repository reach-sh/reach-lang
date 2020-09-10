REACH=../../reach

.PHONY: build
build:
	$(REACH) compile index.rsh

# TODO: FAKE always lets the receiver collect the payment
# TODO: FAKE doesn't simulate the part of the contract that pays out

.PHONY: run
run:
	REACH_CONNECTOR_MODE=FAKE-test-embedded   $(REACH) run index
	REACH_CONNECTOR_MODE=ETH-test-dockerized  $(REACH) run index
	REACH_CONNECTOR_MODE=ETH-test-embedded    $(REACH) run index
