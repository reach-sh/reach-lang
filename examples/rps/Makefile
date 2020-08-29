TAG="reachsh/reach-app-rps:latest"
REACH = ../../reach

build/index.once.mjs build/index.nodraw.mjs:
	$(REACH) compile index.rsh once nodraw

.PHONY: build
build: build/index.once.mjs build/index.nodraw.mjs
	docker build --tag=$(TAG) .

.PHONY: run
run:
	     REACH_CONNECTOR_MODE=FAKE             $(REACH) run index \
	  && REACH_CONNECTOR_MODE=ETH-test-geth    $(REACH) run index \
	  && REACH_CONNECTOR_MODE=ETH-test-ganache $(REACH) run index
#	  && REACH_CONNECTOR_MODE=ALGO             $(REACH) run index

.PHONY: run-target
run-target:
	docker-compose run --rm reach-app-rps-$${REACH_CONNECTOR_MODE} $(ARGS)

.PHONY: run-ganache
run-ganache:
	docker run \
	  -e REACH_CONNECTOR_MODE='ETH-test-ganache-embedded' \
	  $(TAG)

.PHONY: run-devnet
run-devnet:
	docker run -it -p 127.0.0.1:18545:8545 reachsh/ethereum-devnet:v0.1.1
# ctrl+C to kill

.PHONY: run-alice
run-alice:
	docker run -it \
	  -e ETH_NODE_URI=http://docker.for.mac.localhost:18545 \
	  $(TAG) \
	  interactive A

.PHONY: run-bob
run-bob:
	docker run -it \
	  -e ETH_NODE_URI=http://docker.for.mac.localhost:18545 \
	  $(TAG) \
	  interactive B

.PHONY: run-alice-local
run-alice-local:
	ETH_NODE_URI=http://127.0.0.1:18545 npm run -- interactive A

.PHONY: run-bob-local
run-bob-local:
	ETH_NODE_URI=http://127.0.0.1:18545 npm run -- interactive B

.PHONY: down
down:
	docker-compose down

.PHONY: check
check:
	go tool compile build/rps.go

.PHONY: clean
clean:
	rm -f build/index.once.mjs build/index.nodraw.mjs
