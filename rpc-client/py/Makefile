.DEFAULT_GOAL := build

.PHONY: ./VERSION
./VERSION:
	. ../../VERSION && echo "$$VERSION" > VERSION

.PHONY: clean
clean:
	[ -f ./VERSION ] && rm    ./VERSION || true
	[ -d ./dist    ] && rm -r ./dist    || true

.PHONY: build
build: clean ./VERSION
	docker build --tag=reachsh/rpc-client-py:latest .
	@echo 'Bundling...' \
	  && CID="$$(docker run -itd --rm reachsh/rpc-client-py:latest)" \
	  && docker cp "$$CID:/lib/dist" ./dist \
	   ; docker container stop "$$CID"
