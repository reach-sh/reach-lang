TAG="reachsh/examples-rps:v0.1.1"

build/index.once.mjs build/index.nodraw.mjs:
	../../reach compile index.rsh once nodraw

.PHONY: build
build: build/index.once.mjs build/index.nodraw.mjs
	docker build --tag=$(TAG) .

.PHONY: run
run:
	docker-compose run reach-app

.PHONY: run-ganache
run-ganache:
	docker run \
	  -e ETH_NODE_TYPE='in_memory_ganache' \
	  $(TAG)

.PHONY: run-devnet
run-devnet:
	docker run -it -p 127.0.0.1:18545:8545 reachsh/ethereum-devnet:v0.1.1
# ctrl+C to kill

.PHONY: run-alice
run-alice:
	docker run -it \
	  -e ETH_NODE_URI=http://docker.for.mac.localhost:18545 \
	  --entrypoint=/bin/bash \
	  $(TAG) \
	  -c 'npm run interactive --- A'

.PHONY: run-bob
run-bob:
	docker run -it \
	  -e ETH_NODE_URI=http://docker.for.mac.localhost:18545 \
	  --entrypoint=/bin/bash \
	  $(TAG) \
	  -c 'npm run interactive --- B'

.PHONY: run-alice-local
run-alice-local:
	ETH_NODE_URI=http://127.0.0.1:18545 npm run interactive --- A

.PHONY: run-bob-local
run-bob-local:
	ETH_NODE_URI=http://127.0.0.1:18545 npm run interactive --- B

.PHONY: down
down:
	docker-compose down

.PHONY: check
check:
	go tool compile build/rps.go

.PHONY: clean
clean:
	rm -f build/index.once.mjs build/index.nodraw.mjs
