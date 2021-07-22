REACH = ../../reach

.PHONY: clean
clean:
	rm -rf build/*.main.mjs

build/%.main.mjs: %.rsh
	$(REACH) compile $^ main

.PHONY: build
build: build/index.main.mjs
	docker build -f Dockerfile --tag=reachsh/reach-app-over:latest .

.PHONY: run
run:
	$(REACH) run index

.PHONY: down
down:
	$(REACH) down
