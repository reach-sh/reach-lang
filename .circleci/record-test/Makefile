IMAGE=reachsh/circleci-record-test:latest

.PHONY: check
check: output
	cat output

output: .built
	docker run -it --rm $(IMAGE) | tee $@

.built: Dockerfile record.py xfail.txt xtime.txt example
	docker build --tag=$(IMAGE) .
	echo > $@

record.py: ../record.py
	cp $^ $@

xfail.txt: ../xfail.txt
	cp $^ $@

xtime.txt: ../xtime.txt
	cp $^ $@
