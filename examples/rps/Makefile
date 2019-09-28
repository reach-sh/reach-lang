TAG="reachsh/examples-rps:v0.1.0"
NAME="rps"

export STACK_YAML=../../hs/stack.yaml
build/%.mjs: rsh/%.rsh
	stack build
	mkdir -p build
	stack exec -- reachc -o build $^

.PHONY: build
build: build/rps.mjs build/rps_while.mjs
	docker build --tag=$(TAG) .

.PHONY: push
push: build
	docker push $(TAG)

.PHONY: run
run: build
	docker run --name $(NAME) --net=host $(TAG)

.PHONY: clean
clean:
	docker rm $(NAME)
	rm -f build/rps.mjs build/rps_while.mjs
