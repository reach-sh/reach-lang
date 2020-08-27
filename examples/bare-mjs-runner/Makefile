TAG="reachsh/stdlib:v0.1.1"

.PHONY: build
build:
	@echo 'this space intentionally left blank'

.PHONY: clean
clean:
	@echo 'this space intentionally left blank'

.PHONY: run
run:
	docker run \
	  --volume "$$(pwd):/app" \
	  "$(TAG)" \
	  FAKE index.mjs argz
