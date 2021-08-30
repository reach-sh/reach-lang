REACH = ../../reach

.PHONY: clean
clean:
	rm -rf build/*.main.mjs

build/%.main.mjs: %.rsh
	$(REACH) compile $^

.PHONY: build
build: build/announcer.main.mjs build/index.main.mjs build/n2nn.main.mjs

.PHONY: run
run:
	$(REACH) run index
