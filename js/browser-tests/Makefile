# Note: this image is not meant to be published
IMG=reachsh/browser-tests
CYPRESS_BASE_URL=http://localhost:3000
ROOT=../..

.PHONY: build
build:
	  $(ROOT)/scripts/build-image.sh $(IMG) Dockerfile \
	  --build-arg CYPRESS_BASE_URL="$(CYPRESS_BASE_URL)"

# Note: this mounts ./cypress so that the recorded video appears in ./cypress/videos/
# You must have rps-9-web running already, w/ cypress.patch
.PHONY: run
run:
	docker run --network host -v "$(PWD)/cypress:/test/cypress" "$(IMG)"

.PHONY: js-install
js-install:
	yarn install

# You must have rps-9-web running already, w/ cypress.patch
.PHONY: js-run
js-run:
	CYPRESS_BASE_URL="$(CYPRESS_BASE_URL)" yarn run cypress run

# You must have rps-9-web running already, w/ cypress.patch
.PHONY: js-ui
js-ui:
	CYPRESS_BASE_URL="$(CYPRESS_BASE_URL)" yarn run cypress open
