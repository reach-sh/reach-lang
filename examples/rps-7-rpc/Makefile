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

.PHONY: clean-client-lib-cs
clean-client-lib-cs:
	[ -d ./client-cs/lib ] && rm -r ./client-cs/lib || true

.PHONY: clean
clean: clean-client-lib-js \
       clean-client-lib-py \
       clean-client-lib-go \
       clean-client-lib-cs
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

.PHONY: client-lib-cs
client-lib-cs: clean-client-lib-cs
	cp -r ../../rpc-client/cs ./client-cs/lib

.PHONY: build-js
build-js: server/build/index.main.mjs client-lib-js
	cd client-js && docker build --tag=reachsh/reach-app-tut-7-rpc-client-js:latest \
	  --build-arg NODE_IMAGE=$(NODE_IMAGE) \
	  .

.PHONY: build-py
build-py: server/build/index.main.mjs client-lib-py
	cd client-py && docker build \
	  --build-arg VERSION=$(shell cat $(ROOT)/rpc-client/py/VERSION) \
	  --build-arg PYTHON_IMAGE=$(PYTHON_IMAGE) \
	  --tag=reachsh/reach-app-tut-7-rpc-client-py:latest .

.PHONY: build-go
build-go: server/build/index.main.mjs client-lib-go
	cd client-go && docker build \
	 --build-arg GOLANG_IMAGE=$(GOLANG_IMAGE) \
	 --tag=reachsh/reach-app-tut-7-rpc-client-go:latest .

.PHONY: build-cs
build-cs: server/build/index.main.mjs client-lib-cs
	cd client-cs && docker build \
	 --build-arg DOTNET_RUNTIME_IMAGE=$(DOTNET_RUNTIME_IMAGE) \
	 --build-arg DOTNET_SDK_IMAGE=$(DOTNET_SDK_IMAGE) \
	 --tag=reachsh/reach-app-tut-7-rpc-client-cs:latest .

.PHONY: build
build: server/build/index.main.mjs \
       build-js \
       build-py \
       build-go \
       build-cs

.PHONY: run
run:
	REACH_DEBUG=1 sbin/test-on.sh "${REACH_CONNECTOR_MODE}" js py go cs

.PHONY: runtest
runtest: build-cs
	# $(REACH) down
	REACH_DEBUG=1 sbin/test-on.sh "${REACH_CONNECTOR_MODE}" cs
