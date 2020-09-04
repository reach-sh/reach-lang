REACH=../../reach

.PHONY: build
build:
	$(REACH) compile index.rsh

# TODO: why is FAKE getting an undefined balance?
# https://app.circleci.com/pipelines/github/reach-sh/reach-lang/1011/workflows/fafd3c1d-962e-4b06-ae88-1e78c73cdfa5/jobs/1808

.PHONY: run
run:
#	REACH_CONNECTOR_MODE=FAKE-test-embedded   $(REACH) run index
	REACH_CONNECTOR_MODE=ETH-test-dockerized  $(REACH) run index
	REACH_CONNECTOR_MODE=ETH-test-embedded    $(REACH) run index
