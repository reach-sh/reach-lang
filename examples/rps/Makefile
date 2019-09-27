export STACK_YAML=../../hs/stack.yaml

.PHONY: all
all: test demo

.PHONY: build
build: build/rps.mjs build/rps_while.mjs

.PHONY: clean
clean:
	rm -f build/rps.mjs build/rps_while.mjs

build/%.mjs: rsh/%.rsh
	stack build
	mkdir -p build
	stack exec -- reachc -o build $^

.PHONY: test
test: build
	npm test

.PHONY: demo
demo: build
	npm run pretest
	npm run demo
