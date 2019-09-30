TAG="reachsh/examples-auction:v0.1.0"

build/%.mjs: %.rsh
	../../reach -o build $^

.PHONY: build
build: # build/auction.mjs
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
	rm -f build/auction.mjs
