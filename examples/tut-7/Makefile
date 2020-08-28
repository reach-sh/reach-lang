build/%.main.mjs: %.rsh
	../../reach compile $^ main

.PHONY: build
build: build/index.main.mjs
	docker build --tag=reachsh/reach-app-tut-7:latest .

.PHONY: run
run: check

.PHONY: check
check:
	./check.sh

.PHONY: run-live
run-live:
	docker-compose run --rm live

.PHONY: run-alice
run-alice:
	docker-compose run --rm alice

.PHONY: run-bob
run-bob:
	docker-compose run --rm bob

.PHONY: down
down:
	docker-compose down

.PHONY: clean
clean:
	rm -f build/index.main.mjs
