IMAGE=reachsh/circleci-record-test:latest

.PHONY: check
check: output
	cat output

output: .built
	docker run -it --rm $(IMAGE) | tee $@

.built: Dockerfile record.py xfail.txt xtime.txt record examples artifacts
	docker build --tag=$(IMAGE) .
	echo > $@

record:
	cp -fr example/record record
	rm record/ETH.argz

artifacts:
	cp -fr example/artifacts artifacts

examples:
	find ../../examples -maxdepth 1 -type d | sed 'sX../../XX' | sort | tail -n +2 | xargs mkdir -p
	echo > examples/one.sh

record.py: ../record.py
	cp $^ $@

xfail.txt: ../xfail.txt
	cp $^ $@

xtime.txt: ../xtime.txt
	cp $^ $@
