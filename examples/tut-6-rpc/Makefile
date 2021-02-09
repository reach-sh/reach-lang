REACH = ../../reach

.PHONY: clean
clean:
	rm -rf server/build/*.main.mjs

server/build/%.main.mjs: server/%.rsh
	$(REACH) compile $^ main

.PHONY: build
build: server/build/index.main.mjs
	cd client-py && docker build -f Dockerfile --tag=reachsh/reach-app-tut-6-rpc-client-py:latest .
	cd client-go && docker build -f Dockerfile --tag=reachsh/reach-app-tut-6-rpc-client-go:latest .

.PHONY: run
run: build
	docker-compose -f "docker-compose.yml" up --remove-orphans reach-app-tut-6-rpc-server reach-app-tut-6-rpc
