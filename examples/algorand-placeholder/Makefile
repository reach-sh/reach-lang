REACH=../../reach

.PHONY: run
run:
	REACH_CONNECTOR_MODE=ALGO-test-dockerized $(REACH) run index \
		|| echo 'sorry, broken on circle'
