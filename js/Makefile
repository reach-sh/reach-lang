TAG="reachsh/stdlib:v0.1.0"
NAME="stdlib"

.PHONY: build
build:
	docker build --tag=$(TAG) .

.PHONY: push
push: build
	docker push $(TAG)

.PHONY: run
run: build
	docker run --name $(NAME) --net=host $(TAG)

.PHONY: clean
clean:
	docker rm $(NAME)
