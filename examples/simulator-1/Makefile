REACH = ../../reach
NODE = node

.PHONY: build
build:
	cd .

.PHONY: run
run:
	REACH_DOCKER=0 $(REACH) compile --sim &
	$(NODE) client.mjs

.PHONY: clean
clean:
	$(REACH) down
	lsof -ti tcp:3001 | xargs kill
