REACH = ../../reach

.PHONY: clean
clean:
	rm -f index.rsh index.mjs

.PHONY: build
build: clean
	$(REACH) init
	$(REACH) scaffold
	$(REACH) compile
