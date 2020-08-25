NAME=secured-loan
TAG="reachsh/examples-$(NAME):v0.1.1"

build/$(NAME).main.mjs:
	../../reach compile -o build $(NAME).rsh

.PHONY: build
build: build/$(NAME).main.mjs
	echo "TODO: write Dockerfile"
#	docker build --tag=$(TAG) .

.PHONY: run
run:
	echo "TODO: write docker-compose.yaml"
#	docker-compose run reach-app

.PHONY: down
down:
	echo "TODO: write docker-compose.yaml"

.PHONY: clean
clean:
	rm -f build/$(NAME).main.mjs
