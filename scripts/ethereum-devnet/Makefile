TAG="reachsh/ethereum-devnet:v0.1.0"

.PHONY: build
build:
	docker build --tag=$(TAG) .

.PHONY: clean
clean:
	docker rmi $(docker images --filter "dangling=true" -q --no-trunc)

.PHONY: push
push: build
	docker push $(TAG)
