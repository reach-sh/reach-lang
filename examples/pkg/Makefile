REACH = ../../reach

.PHONY: clean
clean:
	rm -rf build/*.main.mjs .reach

.PHONY: build
build: clean
	$(REACH) compile index.rsh; [ $$? -eq 1 ]
	$(REACH) compile --install-pkgs index.rsh
	[ ! -f build/index.main.mjs ]
	$(REACH) compile index.rsh
	$(REACH) compile --install-pkgs index-master.rsh; [ $$? -eq 1 ]

.PHONY: run
run: build
	$(REACH) run index
	$(REACH) down
