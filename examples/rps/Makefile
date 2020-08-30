REACH = ../../reach

.PHONY: run
run:
	REACH_CONNECTOR_MODE=FAKE             $(REACH) run
	REACH_CONNECTOR_MODE=ETH-test-geth    $(REACH) run
	REACH_CONNECTOR_MODE=ETH-test-ganache $(REACH) run
#	REACH_CONNECTOR_MODE=ALGO             $(REACH) run
