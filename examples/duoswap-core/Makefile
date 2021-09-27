all: build

REACH = ../../reach

.PHONY: clean
clean:
	rm -f index.rsh index.mjs
	rm -rf duoswap-core

checkout:
	if test -d duoswap-core; then echo "Repo already cloned"; else git clone https://github.com/reach-sh/duoswap-core duoswap-core; fi

.PHONY: build
build: checkout
	cd duoswap-core && make build

.PHONY: run
run:
	echo "external code will not be ran"
