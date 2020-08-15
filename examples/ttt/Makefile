TAG="reachsh/examples-ttt:v0.1.1"

build/%.main.mjs: %.rsh
	../../reachc -o build $^

.PHONY: build
build: build/index.main.mjs
	docker build --tag=$(TAG) .

.PHONY: push
push:
	docker push $(TAG)

.PHONY: run
run:
	docker-compose run reach-app

.PHONY: clean
clean:
	rm -f build/index.main.mjs
