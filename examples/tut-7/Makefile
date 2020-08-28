build/%.main.mjs: %.rsh
	reach compile $^ main

.PHONY: build
build: build/index.main.mjs
	docker build --tag=reachsh/reach-app-tut-7:latest .

.PHONY: run
run: check

.PHONY: check
check:
	./check.sh

.PHONY: run-index
run-index:
	docker-compose run --rm alice

.PHONY: down
down: down-index
	docker-compose down
