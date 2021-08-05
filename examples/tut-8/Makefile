REACH = ../../reach

.PHONY: run
run:
	./check.sh

.PHONY: run-devnet
run-devnet:
	$(REACH) run

.PHONY: run-live
run-live:
	REACH_CONNECTOR_MODE=ETH-live \
	REACH_ISOLATED_NETWORK=1 \
	ETH_NODE_URI="$${ETH_NODE_URI:-http://reach-devnet-eth:8545}" \
	$(REACH) run
