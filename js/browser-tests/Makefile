# Note: this image is not meant to be published
IMG=reachsh/browser-tests
CYPRESS_BASE_URL=http://localhost:3000

.PHONY: build
build:
	docker build . \
	  --build-arg CYPRESS_BASE_URL="$(CYPRESS_BASE_URL)" \
	  --tag "$(IMG)"

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
