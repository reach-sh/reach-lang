pre-build: child.sol.json child.sol.bin child.approve.teal.tok child.clear.teal.tok

%.sol.json: %.sol
	solc --combined-json abi,bin $< > $@

%.sol.bin: %.sol.json
	jq -r '.contracts["$(patsubst %.sol.json,%.sol,$<):Contract"].bin' $< > $@

%.teal: %.teal.sh
	sh $< | tee $@

%.teal.tok: %.teal
	goal clerk compile -o $@ $<
