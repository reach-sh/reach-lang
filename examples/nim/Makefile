TAG="reachsh/examples-nim:v0.1.1"

build/%.mjs: %.rsh
	../../reachc -o build $^

.PHONY: build
build: build/nim.mjs
	docker build --tag=$(TAG) .

.PHONY: push
push:
	docker push $(TAG)

.PHONY: run
run:
	docker-compose run reach-app

.PHONY: clean
clean:
	rm -f build/nim.mjs
