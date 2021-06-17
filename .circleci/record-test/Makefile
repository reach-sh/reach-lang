IMAGE=reachsh/circleci-record-test:latest

check: output
	cat output

output: .built
	docker run -it --rm $(IMAGE) | tee $@

.built: Dockerfile record.sh example
	docker build --tag=$(IMAGE) .
	echo > $@

record.sh: ../record.sh
	cp $^ $@
