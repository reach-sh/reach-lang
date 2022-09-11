.PHONY: check
check: config.gen.yml
	circleci config validate $<

clean:
	rm -f config.gen.yml config.end.yml config.mid.yml

config.gen.yml: make.sh config.pre.yml
	./$<
