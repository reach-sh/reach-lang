.PHONY: check
check: config.gen.yml
	circleci config validate $<

clean:
	rm -rf config.gen.yml __pycache__

config.gen.yml: generate-ci.py config.pre.yml
	./$<
