REACH = ../../reach

.PHONY: clean
clean:
	rm -rf build/*.main.mjs build/*.sol.json

build/%.main.mjs: %.rsh
	$(REACH) compile $^ main

.PHONY: build
build: build/index.main.mjs build/index.sol.json
	docker build -f Dockerfile --tag=reachsh/reach-app-remote:latest .

build/index.sol.json: index.sol
	solc --optimize --combined-json abi,bin $^ > $@

.PHONY: run
run:
	$(REACH) run index

.PHONY: down
down:
	$(REACH) down
