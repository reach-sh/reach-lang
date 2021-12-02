all: build

.PHONY: clean
clean:
	rm -rf ../ext-duoswap-core

checkout:
	if test -d ../ext-duoswap-core; then echo "Repo already cloned"; else git clone https://github.com/reach-sh/duoswap-core ../ext-duoswap-core; fi

.PHONY: build
build: checkout
	cd ../ext-duoswap-core && ../one.sh build

.PHONY: run
run:
	echo "External code will not be run."
