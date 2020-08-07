TAG="reachsh/examples-rps:v0.1.1"

build/%.mjs: rsh/%.rsh
	../../reachc -o build $^

.PHONY: exp-build
exp-build:
	REACHC_ENABLE_EXPERIMENTAL_COMPILER=Y ../../reachc -o build index.rsh once nodraw

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

.PHONY: down
down:
	docker-compose down

.PHONY: check
check:
	go tool compile build/rps.go

.PHONY: clean
clean:
	rm -f build/rps.mjs build/rps_while.mjs
