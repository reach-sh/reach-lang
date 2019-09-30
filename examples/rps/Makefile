TAG="reachsh/examples-rps:v0.1.0"

build/%.mjs: rsh/%.rsh
	../../reach -o build $^

.PHONY: build
build: build/rps.mjs build/rps_while.mjs
	docker build --tag=$(TAG) .

.PHONY: push
push: build
	npm publish --access public
	docker push $(TAG)

.PHONY: run
run: build
	docker-compose run reach-app

.PHONY: clean
clean:
	rm -f build/rps.mjs build/rps_while.mjs
