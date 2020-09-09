IMAGE="reachsh/algorand-devnet"

.PHONY: build
build:
	if ! [ -d algorand_network ] ; then \
		echo 'Please run `make generate-network` first' ; \
		exit 1 ; \
	fi
	docker build --tag=$(IMAGE):latest .
	TAG_ONLY=1 ../../scripts/docker-push.sh $(IMAGE)

.PHONY: push
push:
	../../scripts/docker-push.sh $(IMAGE)

.PHONY: run
run: build
	docker run -t -p 4180:4180 $(IMAGE):latest

.PHONY: run-shell
run-shell: build
	docker run -it -p 4180:4180 --entrypoint /bin/bash $(IMAGE):latest

.PHONY: status
status:
	@curl -s \
		-H "X-Algo-API-Token: $$(cat algorand_data/algod.token)" \
		localhost:4180/v2/status

# Intentionally not specifying deps for this because we don't
# want to get into loops.
.PHONY: generate-network
generate-network:
	docker run \
		--volume $$(pwd):/cwd \
		--entrypoint /bin/bash \
		$(IMAGE):latest \
		/cwd/generate_algorand_network.sh

.PHONY: clean-network
clean-network:
	rm -rf algorand_network
