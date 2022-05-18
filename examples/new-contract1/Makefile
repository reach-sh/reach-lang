pre-build: child.sol.json child.sol.bin child.teal.tok

%.sol.json: %.sol
	solc --combined-json abi,bin $< > $@

%.sol.bin: %.sol.json
	jq ".bin" $< > $@

%.teal.tok: %.teal
	goal clerk compile -o $@ $<
