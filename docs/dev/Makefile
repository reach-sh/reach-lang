.PHONY: build
build:
	cd tools && $(MAKE) build

NAME=reach-rdp

.PHONY: serve-up
serve-up: serve-down
	docker run --name $(NAME) -v $$(pwd):/usr/share/nginx/html:ro -d -p 8081:80 nginx:alpine

.PHONY: serve-down
serve-down:
	docker rm --force $(NAME)
