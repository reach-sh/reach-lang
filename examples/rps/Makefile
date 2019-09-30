TAG="reachsh/examples-rps:v0.1.0"
NAME="rps"

build/%.mjs: rsh/%.rsh
	../../hs/reach -o build $^

.PHONY: build
build: build/rps.mjs build/rps_while.mjs
	docker build --tag=$(TAG) .

.PHONY: push
push: build
	docker push $(TAG)

.PHONY: run
run: build
	docker-compose up $(NAME)

.PHONY: clean
clean:
	rm -f build/rps.mjs build/rps_while.mjs
