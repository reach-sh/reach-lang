ROOT=..
include $(ROOT)/DEPS

.PHONY: build
build: check
	DOCKER_BUILDKIT=1 docker build \
	  --build-arg REACHC_HASH="$$(../scripts/git-hash.sh)" \
	  --build-arg DOCS_IMAGE=${DOCS_IMAGE} \
	  --pull=false \
	  -o $$(pwd) \
	  -f Dockerfile ..

kToD.ts:
	DOCKER_BUILDKIT=1 docker build \
	  --build-arg REACHC_HASH="$$(../scripts/git-hash.sh)" \
	  --build-arg NODE_IMAGE=${NODE_IMAGE} \
	  --pull=false \
	  -o $$(pwd) \
	  -f update_ext_hover_doc/Dockerfile ..

KDP=../vsce/server/src/keywordToDocumentation.ts
.PHONY: check
check: kToD.ts
	@if ! cmp $^ "${KDP}"; then \
		echo Fix with \`make check-accept\` && exit 1; \
	fi

.PHONY: check-accept
check-accept: kToD.ts
	mv $^ "${KDP}"

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

WATCHES = ./dev ./src Dockerfile Makefile
define WATCH
  do clear \
   ; date \
   ; echo 'Re-rendering docs...' \
   ; $(MAKE) -s build \
   ; echo '...Done.' \
   ; done
endef

.PHONY: watch-inotifywait
watch-inotifywait:
	@while inotifywait -e modify -r $(WATCHES); $(WATCH)

.PHONY: watch-fswatch
watch-fswatch:
	@echo 'Watching for changes...'
	@fswatch -ro --event Updated $(WATCHES) | while read x; $(WATCH)

.PHONY: watch
watch:
	@if   command -v inotifywait >/dev/null; then \
	   $(MAKE) -s watch-inotifywait; \
	 elif command -v fswatch     >/dev/null; then \
	   $(MAKE) -s watch-fswatch; \
	 else \
	   echo 'Unable to find `inotifywait` or `fswatch`.' && exit 1; \
	 fi
