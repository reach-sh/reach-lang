REACH = ../../reach

build/%.main.mjs: %.rsh
	$(REACH) compile $^ main

.PHONY: build
build: build/index.main.mjs

.PHONY: run
run:
	$(REACH) run index hello 'Mr. Postman'

.PHONY: clean
clean:
	rm -rf build/
