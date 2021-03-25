REACH = ../../reach
include ../../VERSION


.PHONY: clean-client-lib-js
clean-client-lib-js:
	[ -d ./client-js/lib ] && rm -r ./client-js/lib || true

.PHONY: clean-client-lib-py
clean-client-lib-py:
	[ -d ./client-py/lib ] && rm -r ./client-py/lib || true

.PHONY: clean
clean: clean-client-lib-js clean-client-lib-py
	rm -rf server/build/*.main.mjs


server/build/%.main.mjs: server/%.rsh
	$(REACH) compile $^ main


.PHONY: client-lib-js
client-lib-js: clean-client-lib-js
	cp -r ../../rpc-client/js ./client-js/lib

.PHONY: client-lib-py
client-lib-py: clean-client-lib-py
	$(MAKE) -C ../../rpc-client/py build
	cp -r ../../rpc-client/py/dist ./client-py/lib


.PHONY: build
build: server/build/index.main.mjs client-lib-js client-lib-py
	cd client-js && docker build -f Dockerfile --tag=reachsh/reach-app-tut-7-rpc-client-js:latest .
	cd client-go && docker build -f Dockerfile --tag=reachsh/reach-app-tut-7-rpc-client-go:latest .
	cd client-py && docker build \
	  -f Dockerfile \
	  --build-arg VERSION=$(VERSION) \
	  --tag=reachsh/reach-app-tut-7-rpc-client-py:latest .

.PHONY: run
run: test

.PHONY: test
test: build
	echo 'FIXME'
#	REACH_DEBUG=1 sbin/test-on.sh ETH  js py go
#	REACH_DEBUG=1 sbin/test-on.sh ALGO js py go
#	sbin/test-on.sh FAKE js py go

.PHONY: run_old
run_old: build
	REACH_DEBUG=1 docker-compose -f "docker-compose.yml" up --remove-orphans reach-app-tut-7-rpc-server reach-app-tut-7-rpc

.PHONY: build_js
build_js:
#	cd ../../js/js-deps && make build
	cd ../../js/stdlib && make build
	cd ../../js/runner && make build
	cd ../../js/rpc-server && make build
