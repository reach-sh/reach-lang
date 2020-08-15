NAME=multisig
TAG="reachsh/examples-$(NAME):v0.1.1"

build/%.main.mjs: %.rsh
	../../reachc -o build $^ main

.PHONY: build
build: build/index.main.mjs
	docker build --tag=$(TAG) .

.PHONY: push
push:
	docker push $(TAG)

.PHONY: publish
publish:
	npm publish --access public

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
	docker run -it -p 127.0.0.1:18545:8545 reachsh/ethereum-devnet:v0.1.0
# ctrl+C to kill

.PHONY: run-parent
run-parent:
	docker run -it \
	  -e ETH_NODE_URI=http://docker.for.mac.localhost:18545 \
	  --entrypoint=/bin/bash \
	  $(TAG) \
	  -c 'npm run interactive --- Parent'

.PHONY: run-child
run-child:
	docker run -it \
	  -e ETH_NODE_URI=http://docker.for.mac.localhost:18545 \
	  --entrypoint=/bin/bash \
	  $(TAG) \
	  -c 'npm run interactive --- Child'

.PHONY: run-parent-local
run-parent-local:
	ETH_NODE_URI=http://127.0.0.1:18545 npm run interactive --- Parent

.PHONY: run-child-local
run-child-local:
	ETH_NODE_URI=http://127.0.0.1:18545 npm run interactive --- Child

.PHONY: down
down:
	docker-compose down

.PHONY: clean
clean:
	rm -f build/index.main.mjs
