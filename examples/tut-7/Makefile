REACH = ../../reach

.PHONY: clean
clean:
	rm -rf build/*.main.mjs

build/%.main.mjs: %.rsh
	$(REACH) compile $^ main

.PHONY: build
build: build/index.main.mjs
	docker build -f Dockerfile --tag=reachsh/reach-app-tut-7:latest .

.PHONY: down
down:
	docker-compose -f "docker-compose.yml" down

.PHONY: run
run: check

.PHONY: check
check: docker-compose.algo.yml
	./check.sh docker-compose.yml
	./check.sh docker-compose.algo.yml

docker-compose.algo.yml: docker-compose.yml
	sed -e '32s/ &default-app//' -e '51s/:/: \&default-app/' $^ > $@

.PHONY: run-live
run-live:
	docker-compose run --rm reach-app-tut-7-ETH-live

.PHONY: run-alice
run-alice:
	docker-compose run --rm alice

.PHONY: run-bob
run-bob:
	docker-compose run --rm bob
