REACH = reach

.PHONY: clean
clean:
	rm -rf build/*.main.mjs

build/%.main.mjs: %.rsh
	\$(REACH) compile $^

.PHONY: build
build: build/${APP}.main.mjs
	docker build -f ${DOCKERFILE} --tag=${IMAGE_TAG} .

.PHONY: run
run:
	\$(REACH) run ${APP}

.PHONY: run-target
run-target: build
	docker-compose -f "${DOCKER_COMPOSE_YML}" run --rm ${SERVICE}-\$\${REACH_CONNECTOR_MODE} \$(ARGS)

.PHONY: down
down:
	docker-compose -f "${DOCKER_COMPOSE_YML}" down --remove-orphans
