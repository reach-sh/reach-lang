# Note: this image is not meant to be published
IMG=reachsh/browser-tests

.PHONY: build
build:
	docker build . --tag "$(IMG)"

# Note: doesn't work yet due to network stuff
# Note: this mounts ./cypress so that the recorded video appears in ./cypress/videos/
# You must have rps-9-web running already, w/ cypress.patch
.PHONY: run
run:
	docker run -v "$(PWD)/cypress:/test/cypress" "$(IMG)"

.PHONY: js-install
js-install:
	yarn install

# You must have rps-9-web running already, w/ cypress.patch
.PHONY: js-run
js-run:
	CYPRESS_BASE_URL=http://localhost:3000 yarn run cypress run

# You must have rps-9-web running already, w/ cypress.patch
.PHONY: js-ui
js-ui:
	CYPRESS_BASE_URL=http://localhost:3000 yarn run cypress open
