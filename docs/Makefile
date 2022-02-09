ROOT=..
include $(ROOT)/DEPS

.PHONY: build
build:
	DOCKER_BUILDKIT=1 docker build \
	  --build-arg REACHC_HASH="$$(../scripts/git-hash.sh)" \
	  --build-arg DOCS_IMAGE=${DOCS_IMAGE} \
	  -o $$(pwd) \
	  -f Dockerfile ..

.PHONY: update-search
update-search:
	cd search && $(MAKE) run

NAME=reach-docs

.PHONY: serve-up
serve-up: serve-down
	docker run --name $(NAME) -v $$(pwd)/build:/usr/share/nginx/html:ro -d -p 8080:80 nginx:alpine
	@echo The docs are available at: http://localhost:8080/

.PHONY: serve-down
serve-down:
	docker rm --force $(NAME)

.PHONY: watch
watch:
	@command -v inotifywait >/dev/null \
	  || (echo 'Unable to find `inotifywait`' && exit 1)
	@while inotifywait -e modify -r ./dev; \
	  do clear \
	   ; date \
	   ; echo 'Re-rendering docs...' \
	   ; $(MAKE) -s build \
	   ; done
