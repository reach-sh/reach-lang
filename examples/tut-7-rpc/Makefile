ROOT=../..
REACH=$(ROOT)/reach
include $(ROOT)/VERSION
include $(ROOT)/DEPS

.PHONY: clean-client-lib-js
clean-client-lib-js:
	[ -d ./client-js/lib ] && rm -r ./client-js/lib || true

.PHONY: clean-client-lib-py
clean-client-lib-py:
	[ -d ./client-py/lib ] && rm -r ./client-py/lib || true

.PHONY: clean-client-lib-go
clean-client-lib-go:
	[ -d ./client-go/lib ] && rm -r ./client-go/lib || true

.PHONY: clean
clean: clean-client-lib-js \
       clean-client-lib-py \
       clean-client-lib-go
	rm -rf server/build/*.main.mjs

server/build/%.main.mjs: server/%.rsh
	$(REACH) compile $^ main

.PHONY: client-lib-js
client-lib-js: clean-client-lib-js
	cp -r $(ROOT)/rpc-client/js ./client-js/lib

.PHONY: client-lib-py
client-lib-py: clean-client-lib-py
	$(MAKE) -C $(ROOT)/rpc-client/py build
	cp -r $(ROOT)/rpc-client/py/dist ./client-py/lib

.PHONY: client-lib-go
client-lib-go: clean-client-lib-go
	cp -r $(ROOT)/rpc-client/go ./client-go/lib

.PHONY: build-js
build-js: server/build/index.main.mjs client-lib-js
	cd client-js && docker build --tag=reachsh/reach-app-tut-7-rpc-client-js:latest \
	  --build-arg NODE_VERSION=$(NODE_VERSION) \
	  .

.PHONY: build-py
build-py: server/build/index.main.mjs client-lib-py
	cd client-py && docker build \
	  --build-arg VERSION=$(shell cat $(ROOT)/rpc-client/py/VERSION) \
	  --tag=reachsh/reach-app-tut-7-rpc-client-py:latest .

.PHONY: build-go
build-go: server/build/index.main.mjs client-lib-go
	cd client-go && docker build --tag=reachsh/reach-app-tut-7-rpc-client-go:latest .

.PHONY: build
build: server/build/index.main.mjs \
       build-js \
       build-py \
       build-go

.PHONY: run
run:
	REACH_DEBUG=1 sbin/test-on.sh "${REACH_CONNECTOR_MODE}" js py go

.PHONY: run261
run261: build-js
	REACH_DEBUG=0 sbin/test-on.sh "${REACH_CONNECTOR_MODE}" js
