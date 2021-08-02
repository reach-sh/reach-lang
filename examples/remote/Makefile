REACH = ../../reach

.PHONY: clean
clean:
	rm -rf build/*.main.mjs build/*.sol.json

build/%.main.mjs: %.rsh
	$(REACH) compile $^ main

.PHONY: build
build: build/index.main.mjs build/index.sol.json

build/index.sol.json: index.sol
	solc --optimize --combined-json abi,bin $^ > $@
