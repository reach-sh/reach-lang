ROOT=../..
SHELL=/bin/bash

.PHONY: build
build: installed
	set -o pipefail ; (. $(ROOT)/VERSION && export VERSION MAJOR MINOR PATCH DRAFT && NODE_ENV=production node generator.js 2>&1) | tee log

.PHONY: profile
profile: installed
	NODE_ENV=production node --prof node --max-old-space-size=4096 generator.js
	NODE_ENV=production node --prof-process isolate*.log > proj.txt
	rm isolate*log

installed:
	npm install
	patch -u -p0 < mdast-util-toc.patch
	touch $@

NAME=reach-rdp

.PHONY: serve-up
serve-up: serve-down
	docker run --name $(NAME) -v $$(pwd)/build:/usr/share/nginx/html:ro -d -p 8081:80 nginx:alpine

.PHONY: serve-down
serve-down:
	docker rm --force $(NAME)
