export function A(stdlib, ctc, txn, interact, v0, v1, kTop) {
  const v2 = v0;
  const v3 = v1;
  interact("params", (v4) => {
    const v5 = stdlib.add(v2, v3);
    ctc.sendrecv("A", "m0", [v2, v3], v5, "e0", (txn) => {
      const v6 = txn.value;
      const v7 = stdlib.add(v2, v3);
      const v8 = stdlib.eq(v6, v7);
      stdlib.assert(v8);
      ctc.recv("A", "e1", (txn) => {
        const v10 = txn.value;
        const v11 = stdlib.eq(v10, v2);
        stdlib.assert(v11);
        interact("getHand", (v12) => {
          const v13 = stdlib.bytes_eq(v12, "ROCK");
          const v14 = stdlib.bytes_eq(v12, "PAPER");
          const v15 = stdlib.bytes_eq(v12, "SCISSORS");
          const v16 = v14 ? true : v15;
          const v17 = v13 ? true : v16;
          stdlib.assert(v17);
          const v18 = v14 ? 1 : 2;
          const v19 = v13 ? 0 : v18;
          const v23 = stdlib.random_uint256();
          const v24 = stdlib.uint256_to_bytes(v23);
          const v25 = stdlib.uint256_to_bytes(v19);
          const v26 = stdlib.bytes_cat(v24, v25);
          const v27 = stdlib.keccak256(v26);
          const v28 = v27;
          interact("commits", (v29) => {
            ctc.sendrecv("A", "m2", [v2, v3, v28], 0, "e2", (txn) => {
              const v30 = txn.value;
              const v31 = stdlib.eq(v30, 0);
              stdlib.assert(v31);
              ctc.recv("A", "e3", (v43, txn) => {
                const v45 = txn.value;
                const v46 = stdlib.eq(v45, 0);
                stdlib.assert(v46);
                const v47 = stdlib.le(0, v43);
                const v48 = stdlib.lt(v43, 3);
                const v49 = v47 ? v48 : false;
                stdlib.assert(v49);
                const v50 = v23;
                const v51 = v19;
                interact("reveals", (v52) => {
                  ctc.sendrecv("A", "m4", [v2, v3, v28, v43, v50, v51], 0, "e4", (txn) => {
                    const v53 = txn.value;
                    const v54 = stdlib.eq(v53, 0);
                    stdlib.assert(v54);
                    const v55 = stdlib.uint256_to_bytes(v50);
                    const v56 = stdlib.uint256_to_bytes(v51);
                    const v57 = stdlib.bytes_cat(v55, v56);
                    const v58 = stdlib.keccak256(v57);
                    const v59 = stdlib.eq(v28, v58);
                    stdlib.assert(v59);
                    const v60 = stdlib.le(0, v51);
                    const v61 = stdlib.lt(v51, 3);
                    const v62 = v60 ? v61 : false;
                    stdlib.assert(v62);
                    const v63 = stdlib.le(0, v51);
                    const v64 = stdlib.lt(v51, 3);
                    const v65 = v63 ? v64 : false;
                    const v66 = stdlib.le(0, v43);
                    const v67 = stdlib.lt(v43, 3);
                    const v68 = v66 ? v67 : false;
                    const v69 = v65 ? v68 : false;
                    const v70 = stdlib.sub(4, v43);
                    const v71 = stdlib.add(v51, v70);
                    const v72 = stdlib.mod(v71, 3);
                    const v73 = v68 ? 0 : 1;
                    const v74 = v65 ? 2 : v73;
                    const v75 = v69 ? v72 : v74;
                    interact("outcome", (v114) => {
                      const v115 = stdlib.le(0, v75);
                      const v116 = stdlib.lt(v75, 3);
                      const v117 = v115 ? v116 : false;
                      stdlib.assert(v117);
                      const v118 = stdlib.eq(v75, 0);
                      const v119 = stdlib.eq(v75, 1);
                      const v120 = v119 ? "Draw" : "Alice wins";
                      const v121 = v118 ? "Bob wins" : v120;
                      kTop(v121); }); }); }); }); }); }); }); }); }); }); }

export function B(stdlib, ctc, txn, interact, kTop) {
  ctc.recv("B", "e0", (v2, v3, txn) => {
    const v6 = txn.value;
    const v7 = stdlib.add(v2, v3);
    const v8 = stdlib.eq(v6, v7);
    stdlib.assert(v8);
    interact("accepts", (v9) => {
      ctc.sendrecv("B", "m1", [v2, v3], v2, "e1", (txn) => {
        const v10 = txn.value;
        const v11 = stdlib.eq(v10, v2);
        stdlib.assert(v11);
        ctc.recv("B", "e2", (v28, txn) => {
          const v30 = txn.value;
          const v31 = stdlib.eq(v30, 0);
          stdlib.assert(v31);
          interact("getHand", (v32) => {
            const v33 = stdlib.bytes_eq(v32, "ROCK");
            const v34 = stdlib.bytes_eq(v32, "PAPER");
            const v35 = stdlib.bytes_eq(v32, "SCISSORS");
            const v36 = v34 ? true : v35;
            const v37 = v33 ? true : v36;
            stdlib.assert(v37);
            const v38 = v34 ? 1 : 2;
            const v39 = v33 ? 0 : v38;
            const v43 = v39;
            interact("shows", (v44) => {
              ctc.sendrecv("B", "m3", [v2, v3, v28, v43], 0, "e3", (txn) => {
                const v45 = txn.value;
                const v46 = stdlib.eq(v45, 0);
                stdlib.assert(v46);
                const v47 = stdlib.le(0, v43);
                const v48 = stdlib.lt(v43, 3);
                const v49 = v47 ? v48 : false;
                stdlib.assert(v49);
                ctc.recv("B", "e4", (v50, v51, txn) => {
                  const v53 = txn.value;
                  const v54 = stdlib.eq(v53, 0);
                  stdlib.assert(v54);
                  const v55 = stdlib.uint256_to_bytes(v50);
                  const v56 = stdlib.uint256_to_bytes(v51);
                  const v57 = stdlib.bytes_cat(v55, v56);
                  const v58 = stdlib.keccak256(v57);
                  const v59 = stdlib.eq(v28, v58);
                  stdlib.assert(v59);
                  const v60 = stdlib.le(0, v51);
                  const v61 = stdlib.lt(v51, 3);
                  const v62 = v60 ? v61 : false;
                  stdlib.assert(v62);
                  const v63 = stdlib.le(0, v51);
                  const v64 = stdlib.lt(v51, 3);
                  const v65 = v63 ? v64 : false;
                  const v66 = stdlib.le(0, v43);
                  const v67 = stdlib.lt(v43, 3);
                  const v68 = v66 ? v67 : false;
                  const v69 = v65 ? v68 : false;
                  const v70 = stdlib.sub(4, v43);
                  const v71 = stdlib.add(v51, v70);
                  const v72 = stdlib.mod(v71, 3);
                  const v73 = v68 ? 0 : 1;
                  const v74 = v65 ? 2 : v73;
                  const v75 = v69 ? v72 : v74;
                  interact("outcome", (v114) => {
                    const v115 = stdlib.le(0, v75);
                    const v116 = stdlib.lt(v75, 3);
                    const v117 = v115 ? v116 : false;
                    stdlib.assert(v117);
                    const v118 = stdlib.eq(v75, 0);
                    const v119 = stdlib.eq(v75, 1);
                    const v120 = v119 ? "Draw" : "Alice wins";
                    const v121 = v118 ? "Bob wins" : v120;
                    kTop(v121); }); }); }); }); }); }); }); }); }); }

export const ABI = [{"constant":false,"inputs":[{"internalType":"address payable","name":"pA","type":"address"},{"internalType":"address payable","name":"pB","type":"address"},{"internalType":"uint256","name":"v2","type":"uint256"},{"internalType":"uint256","name":"v3","type":"uint256"}],"name":"m1","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"address payable","name":"pA","type":"address"},{"internalType":"address payable","name":"pB","type":"address"},{"internalType":"uint256","name":"v2","type":"uint256"},{"internalType":"uint256","name":"v3","type":"uint256"},{"internalType":"uint256","name":"v28","type":"uint256"},{"internalType":"uint256","name":"v43","type":"uint256"}],"name":"m3","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"address payable","name":"pA","type":"address"},{"internalType":"address payable","name":"pB","type":"address"},{"internalType":"uint256","name":"v2","type":"uint256"},{"internalType":"uint256","name":"v3","type":"uint256"}],"name":"m0","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"address payable","name":"pA","type":"address"},{"internalType":"address payable","name":"pB","type":"address"},{"internalType":"uint256","name":"v2","type":"uint256"},{"internalType":"uint256","name":"v3","type":"uint256"},{"internalType":"uint256","name":"v28","type":"uint256"}],"name":"m2","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"address payable","name":"pA","type":"address"},{"internalType":"address payable","name":"pB","type":"address"},{"internalType":"uint256","name":"v2","type":"uint256"},{"internalType":"uint256","name":"v3","type":"uint256"},{"internalType":"uint256","name":"v28","type":"uint256"},{"internalType":"uint256","name":"v43","type":"uint256"},{"internalType":"uint256","name":"v50","type":"uint256"},{"internalType":"uint256","name":"v51","type":"uint256"}],"name":"m4","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"inputs":[{"internalType":"address payable","name":"pA","type":"address"},{"internalType":"address payable","name":"pB","type":"address"}],"payable":true,"stateMutability":"payable","type":"constructor"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"v2","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v3","type":"uint256"}],"name":"e0","type":"event"},{"anonymous":false,"inputs":[],"name":"e1","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"v28","type":"uint256"}],"name":"e2","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"v43","type":"uint256"}],"name":"e3","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"v50","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v51","type":"uint256"}],"name":"e4","type":"event"}];

export const Bytecode = "0x6080604052604051610a2e380380610a2e8339818101604052604081101561002657600080fd5b5080516020918201516040805160008186018190526001600160601b0319606095861b8116838501529390941b90921660548301528051604881840301815260689092019052805192019190912090556109a9806100856000396000f3fe60806040526004361061004a5760003560e01c8063371ff7ad1461004f578063555806701461008d578063bc5d9a1f146100d5578063de25135014610111578063ef7de38514610153575b600080fd5b61008b6004803603608081101561006557600080fd5b506001600160a01b038135811691602081013590911690604081013590606001356101a8565b005b61008b600480360360c08110156100a357600080fd5b506001600160a01b03813581169160208101359091169060408101359060608101359060808101359060a001356102ac565b61008b600480360360808110156100eb57600080fd5b506001600160a01b038135811691602081013590911690604081013590606001356103da565b61008b600480360360a081101561012757600080fd5b506001600160a01b038135811691602081013590911690604081013590606081013590608001356104e3565b61008b600480360361010081101561016a57600080fd5b506001600160a01b03813581169160208101359091169060408101359060608101359060808101359060a08101359060c08101359060e001356105f8565b6040805160016020808301919091526001600160601b0319606088811b82168486015287901b1660548301526068820185905260888083018590528351808403909101815260a890920190925280519101206000541461020757600080fd5b336001600160a01b0384161461021c57600080fd5b81341461022857600080fd5b6040517fa2c2d6664a47f199eff9c374cfc1c62a769ddc60f66cb047bf673f586c017b2390600090a16040805160026020808301919091526001600160601b0319606097881b8116838501529590961b909416605485015260688401929092526088808401919091528151808403909101815260a890920190528051910120600055565b6040805160036020808301919091526001600160601b031960608a811b82168486015289901b166054830152606882018790526088820186905260a88083018690528351808403909101815260c890920190925280519101206000541461031257600080fd5b336001600160a01b0386161461032757600080fd5b341561033257600080fd5b6003811061033f57600080fd5b6040805182815290517f6fbec89a9bad4c7daaf5b053ac2c5ad4e0ff33c287295fe9a98cf7f3a3043f9c9181900360200190a16040805160046020808301919091526001600160601b03196060998a1b8116838501529790981b90961660548701526068860194909452608885019290925260a884015260c8808401919091528151808403909101815260e890920190528051910120600055565b60408051600060208083018290526001600160601b0319606089811b82168587015288901b1660548401528351604881850301815260689093019093528151919092012090541461042a57600080fd5b336001600160a01b0385161461043f57600080fd5b808201341461044d57600080fd5b604080518381526020810183905281517f8117357c96451e4526279f58f4c30adbb462018bb6b7400f1393e45f7db30b7b929181900390910190a16040805160016020808301919091526001600160601b0319606097881b8116838501529590961b909416605485015260688401929092526088808401919091528151808403909101815260a890920190528051910120600055565b6040805160026020808301919091526001600160601b0319606089811b82168486015288901b1660548301526068820186905260888083018690528351808403909101815260a890920190925280519101206000541461054257600080fd5b336001600160a01b0386161461055757600080fd5b341561056257600080fd5b6040805182815290517ff04f5fc87a72102f7c0b228f8bbaf9b9aa7a2b5dc295c86538fdde91e95866e99181900360200190a16040805160036020808301919091526001600160601b0319606098891b8116838501529690971b90951660548601526068850193909352608884019190915260a8808401919091528151808403909101815260c890920190528051910120600055565b6040805160046020808301919091526001600160601b031960608c811b8216848601528b901b166054830152606882018990526088820188905260a8820187905260c88083018790528351808403909101815260e890920190925280519101206000541461066557600080fd5b336001600160a01b0389161461067a57600080fd5b341561068557600080fd5b60408051602080820185905282518083039091018152818301835260608083018590528351808403909101815260809092019092526106c491906108a5565b6040516020018082805190602001908083835b602083106106f65780518252601f1990920191602091820191016106d7565b6001836020036101000a0380198251168184511680821785525050505050509050019150506040516020818303038152906040528051906020012060001c841461073f57600080fd5b6003811061074c57600080fd5b6003808210908410600082610762576000610764565b815b6107895782610781578161077957600161077c565b60005b610784565b60025b61079a565b60038660040385018161079857fe5b065b905060028114811581816001600160a01b038f166108fc836107c957826107c1578e6107c4565b60005b6107ce565b8e6002025b8e019081150290604051600060405180830381858888f193505050501580156107fb573d6000803e3d6000fd5b508d6001600160a01b03166108fc836108235782610819578e61081e565b8e6002025b610826565b60005b6040518115909202916000818181858888f1935050505015801561084e573d6000803e3d6000fd5b50604080518a8152602081018a905281517fb71d350b59ceca5c6544e5367d61ca8cae3e36b25f8d900743d063dff3d6508b929181900390910190a160008055739527b6b278abf20ff7d69b5fe0696eb937e633e6ff5b606082518383604051602001808461ffff1661ffff1660f01b815260020183805190602001908083835b602083106108ee5780518252601f1990920191602091820191016108cf565b51815160209384036101000a600019018019909216911617905285519190930192850191508083835b602083106109365780518252601f199092019160209182019101610917565b6001836020036101000a038019825116818451168082178552505050505050905001935050505060405160208183030381529060405290509291505056fea265627a7a72315820603c289f53dec583c52c1a07f638ddf0b69d7e169e91122e4f33f0e55662dc5264736f6c634300050b0032";