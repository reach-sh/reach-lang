IMAGE="reachsh/algorand-devnet"

.PHONY: build
build:
	docker build --tag=$(IMAGE):latest .
	TAG_ONLY=1 ../../scripts/docker-push.sh $(IMAGE)

.PHONY: run
run: build
	docker run -t -p 4180:4180 $(IMAGE):latest

.PHONY: status
status:
	@curl -s \
		-H "X-Algo-API-Token: $$(cat algorand_data/algod.token)" \
		localhost:4180/v2/status
