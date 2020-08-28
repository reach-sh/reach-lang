.PHONY: check
check: expected actual
	diff -u $^

actual: build/index.main.pl
	grep -B 2 between $^ > $@

build/index.main.pl: index.rsh
	../../reach compile $^

build/%.main.mjs: %.rsh
	reach compile $^ main

.PHONY: build
build: build/index.main.mjs check
	docker build -f Dockerfile --tag=reachsh/reach-app-timeoutception:latest .

.PHONY: run
run: run-index

.PHONY: run-index
run-index: build
	docker-compose -f "docker-compose.yaml" run --rm reach-app-timeoutception app $(ARGS)

.PHONY: down
down:
	docker-compose down

.PHONY: clean
clean:
	rm -f build/
