TAG="reachsh/examples-ttt:v0.1.1"

build/%.main.mjs: %.rsh
	../../reach compile $^

.PHONY: build
build: build/index.main.mjs
	docker build --tag=$(TAG) .

.PHONY: run
run:
	docker-compose run reach-app

.PHONY: down
down:
	docker-compose down

.PHONY: clean
clean:
	rm -f build/index.main.mjs
