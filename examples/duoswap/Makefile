REACH = ../../reach

.PHONY: clean
clean:
	rm -rf build/*.main.mjs

build/%.main.mjs: %.rsh
	$(REACH) compile $^

.PHONY: build
build: build/announcer.main.mjs build/index.main.mjs build/n2nn.main.mjs
	docker build -f Dockerfile --tag=reachsh/reach-app-duoswap:latest .

.PHONY: run
run:
	$(REACH) run index

.PHONY: run-target
run-target: build
	docker-compose -f "docker-compose.yml" run --rm reach-app-duoswap-$${REACH_CONNECTOR_MODE} $(ARGS)

.PHONY: down
down:
	docker-compose -f "docker-compose.yml" down --remove-orphans
