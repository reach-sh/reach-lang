NAME=multisig
TAG="reachsh/examples-$(NAME):v0.1.1"

build/%.mjs: %.rsh
	../../reachc -o build $^

.PHONY: build
build: build/$(NAME).mjs
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

.PHONY: clean
clean:
	rm -f build/$(NAME).mjs
