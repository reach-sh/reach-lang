REACH = ../../reach

# XXX Willing to try ALGO twice because on CircleCI sometimes it fails the first time
.PHONY: run
run:
	REACH_CONNECTOR_MODE=ETH $(REACH) run
	REACH_CONNECTOR_MODE=ALGO REACH_DEBUG=1 $(REACH) run || (sleep 5 && \
	REACH_CONNECTOR_MODE=ALGO REACH_DEBUG=1 $(REACH) run)

