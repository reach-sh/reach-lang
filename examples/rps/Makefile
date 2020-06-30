TAG="reachsh/examples-rps:v0.1.1"

build/%.mjs: rsh/%.rsh
	../../reachc -o build $^

.PHONY: build
build: build/rps.mjs build/rps_while.mjs
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
	rm -f build/rps.mjs build/rps_while.mjs
