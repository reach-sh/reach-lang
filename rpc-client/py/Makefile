.DEFAULT_GOAL := build

.PHONY: ./VERSION
./VERSION:
	. ../../VERSION && echo "$$VERSION.$$(date -u '+%Y%m%d%H%M')" > VERSION

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

# https://packaging.python.org/tutorials/packaging-projects/#uploading-the-distribution-archives
.PHONY: pypi
pypi: build
	command -v twine >/dev/null \
	  || (echo 'You must install `twine` to perform an upload!' && exit 1)
	PYTHON_KEYRING_BACKEND=keyring.backends.null.Keyring twine upload dist/*
