TAG="reachsh/ethereum-devnet:v0.1.0"
NAME="devnet"

.PHONY: build
build:
	docker build --tag=$(TAG) .

.PHONY: push
push: build
	docker push $(TAG)

.PHONY: clean
clean:
	docker rm $(NAME)
