REACH=../../reach

# XXX Willing to try ALGO twice because on CircleCI sometimes it fails the first time
.PHONY: run
run:
	REACH_CONNECTOR_MODE=ALGO-test-dockerized $(REACH) run || (sleep 5 && \
	REACH_CONNECTOR_MODE=ALGO-test-dockerized $(REACH) run)
	REACH_CONNECTOR_MODE=ETH-test-dockerized $(REACH) run
	REACH_CONNECTOR_MODE=FAKE-test-embedded $(REACH) run

