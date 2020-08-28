REACH = reach

build/%.main.mjs: %.rsh
	$(REACH) compile $^ main

.PHONY: build
build: build/index.main.mjs

.PHONY: run
run:
	$(REACH) run index hello 'Mr. Postman'

.PHONY: down
down:
	@echo "this space intentionally left blank"

.PHONY: clean
clean:
	rm -rf build/
