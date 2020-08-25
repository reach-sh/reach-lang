NAME=auction
TAG="reachsh/examples-$(NAME):v0.1.1"

build/%.mjs: %.rsh
	../../reach compile $^

.PHONY: build
build: build/$(NAME).mjs
	docker build --tag=$(TAG) .

.PHONY: run
run:
	docker-compose run reach-app

.PHONY: clean
clean:
	rm -f build/$(NAME).mjs
