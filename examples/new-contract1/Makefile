REACH = ../../reach

pre-build: child.sol.json child.sol.bin child.approve.teal.tok child.clear.teal.tok build/child.main.mjs child.approve.teal.big.tok

build/%.main.mjs: %.rsh
	$(REACH) compile $<

%.sol.json: %.sol
	solc --optimize --combined-json abi,bin $< > $@

%.sol.yul: %.sol
	solc --optimize --ir $< > $@

%.sol.bin: %.sol.json
	jq -r '.contracts["$(patsubst %.sol.json,%.sol,$<):Contract"].bin' $< | head -c -1 > $@

%.teal: %.teal.sh
	sh $< | tee $@

%.teal.tok: %.teal
	goal clerk compile -o $@ $<

%.teal.big.tok: %.teal.tok
	cat $< > $@
	head -c 4096 /dev/null >> $@
