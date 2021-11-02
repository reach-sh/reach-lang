.PHONY: build
build:
	DOCKER_BUILDKIT=1 docker build \
	  --build-arg REACHC_HASH="$$(../scripts/git-hash.sh)" \
	  -o $$(pwd) \
	  -f Dockerfile ..

NAME=reach-docs

.PHONY: serve-up
serve-up: serve-down
	docker run --name $(NAME) -v $$(pwd)/build:/usr/share/nginx/html:ro -d -p 8080:80 nginx:alpine

.PHONY: serve-down
serve-down:
	docker rm --force $(NAME)
