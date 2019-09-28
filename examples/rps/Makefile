TAG="reachsh/examples-rps:v0.1.0"

export STACK_YAML=../../hs/stack.yaml

.PHONY: all
all: test demo

.PHONY: build
build: build/rps.mjs build/rps_while.mjs
	docker build --tag=$(TAG) .

.PHONY: push
push: build
	docker push $(TAG)

.PHONY: clean
clean:
	rm -f build/rps.mjs build/rps_while.mjs

build/%.mjs: rsh/%.rsh
	stack build
	mkdir -p build
	stack exec -- reachc -o build $^

.PHONY: run
run: build
	docker run --name rps --net=host $(TAG)
