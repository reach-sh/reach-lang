export async function A(stdlib, ctc, interact, v0, v1) {
  const txn0 = { balance: 0, value: 0 };
  const v2 = v0;
  const v3 = v1;
  const v4 = stdlib.isType("bool", await interact.params());
  const v5 = stdlib.add(v2, v3);
  const txn1 = await ctc.sendrecv("A", "m1", [v2, v3], v5, "e1", 10, "e2");
  if (txn1.didTimeout) {
    stdlib.assert(true);
    return ["Alice quits"]; }
  else {
    const v6 = txn1.value;
    const v7 = stdlib.add(v2, v3);
    const v8 = stdlib.eq(v6, v7);
    stdlib.assert(v8);
    const txn2 = await ctc.recv("A", "e3", 10, true, [v2, v3], "m4", "e4");
    if (txn2.didTimeout) {
      stdlib.assert(true);
      return ["Bob quits"]; }
    else {
      const [] = txn2.data;
      const v10 = txn2.value;
      const v11 = stdlib.eq(v10, v2);
      stdlib.assert(v11);
      const v12 = stdlib.isType("bytes", await interact.getHand());
      const v13 = stdlib.bytes_eq(v12, "ROCK");
      const v14 = stdlib.bytes_eq(v12, "PAPER");
      const v15 = stdlib.bytes_eq(v12, "SCISSORS");
      const v16 = v13 ? true : v14;
      const v17 = v16 ? true : v15;
      stdlib.assert(v17);
      const v18 = v14 ? 1 : 2;
      const v19 = v13 ? 0 : v18;
      const v23 = stdlib.random_uint256();
      const v24 = stdlib.uint256_to_bytes(v23);
      const v25 = stdlib.uint256_to_bytes(v19);
      const v26 = stdlib.bytes_cat(v24, v25);
      const v27 = stdlib.keccak256(v26);
      const v28 = v27;
      const v29 = stdlib.isType("bool", await interact.commits());
      const txn3 = await ctc.sendrecv("A", "m5", [v2, v3, v28], 0, "e5", 10, "e6");
      if (txn3.didTimeout) {
        stdlib.assert(true);
        return ["Alice quits"]; }
      else {
        const v30 = txn3.value;
        const v31 = stdlib.eq(v30, 0);
        stdlib.assert(v31);
        const txn4 = await ctc.recv("A", "e7", 10, true, [v2, v3, v28], "m8", "e8");
        if (txn4.didTimeout) {
          stdlib.assert(true);
          return ["Bob quits"]; }
        else {
          const [v43] = txn4.data;
          const v45 = txn4.value;
          const v46 = stdlib.eq(v45, 0);
          stdlib.assert(v46);
          const v47 = stdlib.le(0, v43);
          const v48 = stdlib.lt(v43, 3);
          const v49 = v47 ? v48 : false;
          stdlib.assert(v49);
          const v50 = v23;
          const v51 = v19;
          const v52 = stdlib.le(0, v43);
          const v53 = stdlib.lt(v43, 3);
          const v54 = v52 ? v53 : false;
          stdlib.assert(v54);
          const v55 = stdlib.eq(v43, 0);
          const v56 = stdlib.eq(v43, 1);
          const v57 = v56 ? "PAPER" : "SCISSORS";
          const v58 = v55 ? "ROCK" : v57;
          const v59 = stdlib.isType("bool", await interact.reveals(v58));
          const txn5 = await ctc.sendrecv("A", "m9", [v2, v3, v28, v43, v50, v51], 0, "e9", 10, "e10");
          if (txn5.didTimeout) {
            stdlib.assert(true);
            return ["Alice quits"]; }
          else {
            const v60 = txn5.value;
            const v61 = stdlib.eq(v60, 0);
            stdlib.assert(v61);
            const v62 = stdlib.uint256_to_bytes(v50);
            const v63 = stdlib.uint256_to_bytes(v51);
            const v64 = stdlib.bytes_cat(v62, v63);
            const v65 = stdlib.keccak256(v64);
            const v66 = stdlib.eq(v28, v65);
            stdlib.assert(v66);
            const v67 = stdlib.le(0, v51);
            const v68 = stdlib.lt(v51, 3);
            const v69 = v67 ? v68 : false;
            stdlib.assert(v69);
            const v70 = stdlib.le(0, v51);
            const v71 = stdlib.lt(v51, 3);
            const v72 = v70 ? v71 : false;
            const v73 = stdlib.le(0, v43);
            const v74 = stdlib.lt(v43, 3);
            const v75 = v73 ? v74 : false;
            const v76 = v72 ? v75 : false;
            const v77 = stdlib.sub(4, v43);
            const v78 = stdlib.add(v51, v77);
            const v79 = stdlib.mod(v78, 3);
            const v80 = v75 ? 0 : 1;
            const v81 = v72 ? 2 : v80;
            const v82 = v76 ? v79 : v81;
            const v121 = stdlib.isType("bool", await interact.outcome());
            const v122 = stdlib.le(0, v82);
            const v123 = stdlib.lt(v82, 5);
            const v124 = v122 ? v123 : false;
            stdlib.assert(v124);
            const v125 = stdlib.eq(v82, 0);
            const v126 = stdlib.eq(v82, 1);
            const v127 = stdlib.eq(v82, 2);
            const v128 = stdlib.eq(v82, 3);
            const v129 = v128 ? "Alice quits" : "Bob quits";
            const v130 = v127 ? "Alice wins" : v129;
            const v131 = v126 ? "Draw" : v130;
            const v132 = v125 ? "Bob wins" : v131;
            return [v132]; } } } } } }

export async function B(stdlib, ctc, interact) {
  const txn0 = { balance: 0, value: 0 };
  const txn1 = await ctc.recv("B", "e1", 10, true, [], "m2", "e2");
  if (txn1.didTimeout) {
    stdlib.assert(true);
    return ["Alice quits"]; }
  else {
    const [v2, v3] = txn1.data;
    const v6 = txn1.value;
    const v7 = stdlib.add(v2, v3);
    const v8 = stdlib.eq(v6, v7);
    stdlib.assert(v8);
    const v9 = stdlib.isType("bool", await interact.accepts(v2, v3));
    const txn2 = await ctc.sendrecv("B", "m3", [v2, v3], v2, "e3", 10, "e4");
    if (txn2.didTimeout) {
      stdlib.assert(true);
      return ["Bob quits"]; }
    else {
      const v10 = txn2.value;
      const v11 = stdlib.eq(v10, v2);
      stdlib.assert(v11);
      const txn3 = await ctc.recv("B", "e5", 10, true, [v2, v3], "m6", "e6");
      if (txn3.didTimeout) {
        stdlib.assert(true);
        return ["Alice quits"]; }
      else {
        const [v28] = txn3.data;
        const v30 = txn3.value;
        const v31 = stdlib.eq(v30, 0);
        stdlib.assert(v31);
        const v32 = stdlib.isType("bytes", await interact.getHand());
        const v33 = stdlib.bytes_eq(v32, "ROCK");
        const v34 = stdlib.bytes_eq(v32, "PAPER");
        const v35 = stdlib.bytes_eq(v32, "SCISSORS");
        const v36 = v33 ? true : v34;
        const v37 = v36 ? true : v35;
        stdlib.assert(v37);
        const v38 = v34 ? 1 : 2;
        const v39 = v33 ? 0 : v38;
        const v43 = v39;
        const v44 = stdlib.isType("bool", await interact.shows());
        const txn4 = await ctc.sendrecv("B", "m7", [v2, v3, v28, v43], 0, "e7", 10, "e8");
        if (txn4.didTimeout) {
          stdlib.assert(true);
          return ["Bob quits"]; }
        else {
          const v45 = txn4.value;
          const v46 = stdlib.eq(v45, 0);
          stdlib.assert(v46);
          const v47 = stdlib.le(0, v43);
          const v48 = stdlib.lt(v43, 3);
          const v49 = v47 ? v48 : false;
          stdlib.assert(v49);
          const txn5 = await ctc.recv("B", "e9", 10, true, [v2, v3, v28, v43], "m10", "e10");
          if (txn5.didTimeout) {
            stdlib.assert(true);
            return ["Alice quits"]; }
          else {
            const [v50, v51] = txn5.data;
            const v60 = txn5.value;
            const v61 = stdlib.eq(v60, 0);
            stdlib.assert(v61);
            const v62 = stdlib.uint256_to_bytes(v50);
            const v63 = stdlib.uint256_to_bytes(v51);
            const v64 = stdlib.bytes_cat(v62, v63);
            const v65 = stdlib.keccak256(v64);
            const v66 = stdlib.eq(v28, v65);
            stdlib.assert(v66);
            const v67 = stdlib.le(0, v51);
            const v68 = stdlib.lt(v51, 3);
            const v69 = v67 ? v68 : false;
            stdlib.assert(v69);
            const v70 = stdlib.le(0, v51);
            const v71 = stdlib.lt(v51, 3);
            const v72 = v70 ? v71 : false;
            const v73 = stdlib.le(0, v43);
            const v74 = stdlib.lt(v43, 3);
            const v75 = v73 ? v74 : false;
            const v76 = v72 ? v75 : false;
            const v77 = stdlib.sub(4, v43);
            const v78 = stdlib.add(v51, v77);
            const v79 = stdlib.mod(v78, 3);
            const v80 = v75 ? 0 : 1;
            const v81 = v72 ? 2 : v80;
            const v82 = v76 ? v79 : v81;
            const v121 = stdlib.isType("bool", await interact.outcome());
            const v122 = stdlib.le(0, v82);
            const v123 = stdlib.lt(v82, 5);
            const v124 = v122 ? v123 : false;
            stdlib.assert(v124);
            const v125 = stdlib.eq(v82, 0);
            const v126 = stdlib.eq(v82, 1);
            const v127 = stdlib.eq(v82, 2);
            const v128 = stdlib.eq(v82, 3);
            const v129 = v128 ? "Alice quits" : "Bob quits";
            const v130 = v127 ? "Alice wins" : v129;
            const v131 = v126 ? "Draw" : v130;
            const v132 = v125 ? "Bob wins" : v131;
            return [v132]; } } } } } }

export const ABI = [{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"pA","type":"address"},{"internalType":"address payable","name":"pB","type":"address"},{"internalType":"uint256","name":"v2","type":"uint256"},{"internalType":"uint256","name":"v3","type":"uint256"}],"name":"m3","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"pA","type":"address"},{"internalType":"address payable","name":"pB","type":"address"}],"name":"m2","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"pA","type":"address"},{"internalType":"address payable","name":"pB","type":"address"},{"internalType":"uint256","name":"v2","type":"uint256"},{"internalType":"uint256","name":"v3","type":"uint256"}],"name":"m4","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"pA","type":"address"},{"internalType":"address payable","name":"pB","type":"address"},{"internalType":"uint256","name":"v2","type":"uint256"},{"internalType":"uint256","name":"v3","type":"uint256"},{"internalType":"uint256","name":"v28","type":"uint256"},{"internalType":"uint256","name":"v43","type":"uint256"},{"internalType":"uint256","name":"v50","type":"uint256"},{"internalType":"uint256","name":"v51","type":"uint256"}],"name":"m9","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"pA","type":"address"},{"internalType":"address payable","name":"pB","type":"address"},{"internalType":"uint256","name":"v2","type":"uint256"},{"internalType":"uint256","name":"v3","type":"uint256"}],"name":"m1","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"pA","type":"address"},{"internalType":"address payable","name":"pB","type":"address"},{"internalType":"uint256","name":"v2","type":"uint256"},{"internalType":"uint256","name":"v3","type":"uint256"},{"internalType":"uint256","name":"v28","type":"uint256"},{"internalType":"uint256","name":"v43","type":"uint256"}],"name":"m7","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"pA","type":"address"},{"internalType":"address payable","name":"pB","type":"address"},{"internalType":"uint256","name":"v2","type":"uint256"},{"internalType":"uint256","name":"v3","type":"uint256"},{"internalType":"uint256","name":"v28","type":"uint256"},{"internalType":"uint256","name":"v43","type":"uint256"}],"name":"m10","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"pA","type":"address"},{"internalType":"address payable","name":"pB","type":"address"},{"internalType":"uint256","name":"v2","type":"uint256"},{"internalType":"uint256","name":"v3","type":"uint256"}],"name":"m6","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"pA","type":"address"},{"internalType":"address payable","name":"pB","type":"address"},{"internalType":"uint256","name":"v2","type":"uint256"},{"internalType":"uint256","name":"v3","type":"uint256"},{"internalType":"uint256","name":"v28","type":"uint256"}],"name":"m8","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"pA","type":"address"},{"internalType":"address payable","name":"pB","type":"address"},{"internalType":"uint256","name":"v2","type":"uint256"},{"internalType":"uint256","name":"v3","type":"uint256"},{"internalType":"uint256","name":"v28","type":"uint256"}],"name":"m5","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"inputs":[{"internalType":"address payable","name":"pA","type":"address"},{"internalType":"address payable","name":"pB","type":"address"}],"payable":true,"stateMutability":"payable","type":"constructor"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"v2","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v3","type":"uint256"}],"name":"e1","type":"event"},{"anonymous":false,"inputs":[],"name":"e2","type":"event"},{"anonymous":false,"inputs":[],"name":"e3","type":"event"},{"anonymous":false,"inputs":[],"name":"e4","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"v28","type":"uint256"}],"name":"e5","type":"event"},{"anonymous":false,"inputs":[],"name":"e6","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"v43","type":"uint256"}],"name":"e7","type":"event"},{"anonymous":false,"inputs":[],"name":"e8","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"v50","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v51","type":"uint256"}],"name":"e9","type":"event"},{"anonymous":false,"inputs":[],"name":"e10","type":"event"}];

export const Bytecode = "0x60806040526040516111113803806111118339818101604052604081101561002657600080fd5b50805160209182015160408051600081860181905243828401526001600160601b0319606095861b8116868401529390941b90921660748301528051606881840301815260889092019052805192019190912090556110878061008a6000396000f3fe6080604052600436106100915760003560e01c80634441778b116100595780634441778b146101ee57806380c418dc1461023c578063d21299951461028a578063d62511ba146102cc578063e93986271461031457610091565b806301ffa6c71461009657806310dee984146100da57806326b411a01461010e5780632fa0a8f8146101505780633995bf9d146101ac575b600080fd5b6100d8600480360360a08110156100ac57600080fd5b508035906001600160a01b0360208201358116916040810135909116906060810135906080013561035c565b005b6100d8600480360360608110156100f057600080fd5b508035906001600160a01b036020820135811691604001351661047b565b6100d8600480360360a081101561012457600080fd5b508035906001600160a01b0360208201358116916040810135909116906060810135906080013561052f565b6100d8600480360361012081101561016757600080fd5b508035906001600160a01b03602082013581169160408101359091169060608101359060808101359060a08101359060c08101359060e0810135906101000135610629565b6100d8600480360360a08110156101c257600080fd5b508035906001600160a01b036020820135811691604081013590911690606081013590608001356108e1565b6100d8600480360360e081101561020457600080fd5b508035906001600160a01b03602082013581169160408101359091169060608101359060808101359060a08101359060c00135610a05565b6100d8600480360360e081101561025257600080fd5b508035906001600160a01b03602082013581169160408101359091169060608101359060808101359060a08101359060c00135610b4f565b6100d8600480360360a08110156102a057600080fd5b508035906001600160a01b03602082013581169160408101359091169060608101359060800135610c58565b6100d8600480360360c08110156102e257600080fd5b508035906001600160a01b03602082013581169160408101359091169060608101359060808101359060a00135610d52565b6100d8600480360360c081101561032a57600080fd5b508035906001600160a01b03602082013581169160408101359091169060608101359060808101359060a00135610e53565b6040805160016020808301919091528183018890526001600160601b0319606088811b82168185015287901b1660748301526088820185905260a88083018590528351808403909101815260c89092019092528051910120600054146103c157600080fd5b336001600160a01b038416146103d657600080fd5b600a850143106103e557600080fd5b8134146103f157600080fd5b6040517f56800b5260512456b844fc9371dbdb10a629694349bc1829c0518bc84861b0c190600090a160408051600360208083019190915243828401526001600160601b0319606097881b8116888401529590961b9094166074850152608884019290925260a8808401919091528151808403909101815260c89092019052805191012060005550565b60408051600060208083018290528284018790526001600160601b0319606087811b82168186015286901b166074840152835160688185030181526088909301909352815191909201209054146104d157600080fd5b336001600160a01b038216146104e657600080fd5b600a83014310156104f657600080fd5b6040517f9b31f9e88fd11f71bfbf93b0237bc9a0900b8479a307f60435e40543e383403590600090a1600080556001600160a01b038216ff5b6040805160016020808301919091528183018890526001600160601b0319606088811b82168185015287901b1660748301526088820185905260a88083018590528351808403909101815260c890920190925280519101206000541461059457600080fd5b336001600160a01b038516146105a957600080fd5b600a85014310156105b957600080fd5b6040516001600160a01b03851690303180156108fc02916000818181858888f193505050501580156105ef573d6000803e3d6000fd5b506040517f79e5f05486520ee227978e44df5013b958eb8b7c4045fd17db1c8f340f2c361190600090a1600080556001600160a01b038416ff5b6040805160076020808301919091528183018c90526001600160601b031960608c811b8216818501528b901b1660748301526088820189905260a8820188905260c8820187905260e88083018790528351808403909101815261010890920190925280519101206000541461069d57600080fd5b336001600160a01b038916146106b257600080fd5b600a890143106106c157600080fd5b34156106cc57600080fd5b604080516020808201859052825180830390910181528183018352606080830185905283518084039091018152608090920190925261070b9190610f83565b6040516020018082805190602001908083835b6020831061073d5780518252601f19909201916020918201910161071e565b6001836020036101000a0380198251168184511680821785525050505050509050019150506040516020818303038152906040528051906020012060001c841461078657600080fd5b6003811061079357600080fd5b60038082109084106000826107a95760006107ab565b815b6107d057826107c857816107c05760016107c3565b60005b6107cb565b60025b6107e1565b6003866004038501816107df57fe5b065b905060028114811581816001600160a01b038f166108fc836108105782610808578e61080b565b60005b610815565b8e6002025b8e019081150290604051600060405180830381858888f19350505050158015610842573d6000803e3d6000fd5b508d6001600160a01b03166108fc8361086a5782610860578e610865565b8e6002025b61086d565b60005b6040518115909202916000818181858888f19350505050158015610895573d6000803e3d6000fd5b50604080518a8152602081018a905281517f6d32a2ce93084268e9ee8e741d4257b2c396b94a1ffc25aeb6fc4ba8fc0594d2929181900390910190a1600080556001600160a01b038f16ff5b60408051600060208083018290528284018990526001600160601b0319606089811b82168186015288901b1660748401528351606881850301815260889093019093528151919092012090541461093757600080fd5b336001600160a01b0385161461094c57600080fd5b600a8501431061095b57600080fd5b808201341461096957600080fd5b604080518381526020810183905281517fc9d006980a027d32b08195d9eab835f93f490ac52038c85ed3e8aca6e0b9b2e1929181900390910190a160408051600160208083019190915243828401526001600160601b0319606097881b8116888401529590961b9094166074850152608884019290925260a8808401919091528151808403909101815260c89092019052805191012060005550565b6040805160056020808301919091528183018a90526001600160601b031960608a811b82168185015289901b1660748301526088820187905260a8820186905260c88083018690528351808403909101815260e8909201909252805191012060005414610a7157600080fd5b336001600160a01b03861614610a8657600080fd5b600a87014310610a9557600080fd5b3415610aa057600080fd5b60038110610aad57600080fd5b6040805182815290517ffc55d683ac816a7149ebdfa999ae1bcfeeae27c37c9dab64a23f617beed2a0079181900360200190a160408051600760208083019190915243828401526001600160601b03196060998a1b81168a8401529790981b9096166074870152608886019490945260a885019290925260c884015260e880840191909152815180840390910181526101089092019052805191012060005550565b6040805160076020808301919091528183018a90526001600160601b031960608a811b82168185015289901b1660748301526088820187905260a8820186905260c8820185905260e880830185905283518084039091018152610108909201909252805191012060005414610bc357600080fd5b336001600160a01b03861614610bd857600080fd5b600a8701431015610be857600080fd5b6040516001600160a01b03861690303180156108fc02916000818181858888f19350505050158015610c1e573d6000803e3d6000fd5b506040517f5ef1d939728ae307281ba62215efdccdf0b99fa9cc412f247b7ab97b4729b74f90600090a1600080556001600160a01b038616ff5b6040805160036020808301919091528183018890526001600160601b0319606088811b82168185015287901b1660748301526088820185905260a88083018590528351808403909101815260c8909201909252805191012060005414610cbd57600080fd5b336001600160a01b03841614610cd257600080fd5b600a8501431015610ce257600080fd5b6040516001600160a01b03841690303180156108fc02916000818181858888f19350505050158015610d18573d6000803e3d6000fd5b506040517fd6694479677b4939b334fce2b8096952c15688edcb9eb18da0a00fbde0d22fb690600090a1600080556001600160a01b038416ff5b6040805160056020808301919091528183018990526001600160601b0319606089811b82168185015288901b1660748301526088820186905260a8820185905260c88083018590528351808403909101815260e8909201909252805191012060005414610dbe57600080fd5b336001600160a01b03861614610dd357600080fd5b600a8601431015610de357600080fd5b6040516001600160a01b03861690303180156108fc02916000818181858888f19350505050158015610e19573d6000803e3d6000fd5b506040517fffa3d43ab9b6e5b34273fd049718a85553427384eed63754abc672936df0584e90600090a1600080556001600160a01b038516ff5b6040805160036020808301919091528183018990526001600160601b0319606089811b82168185015288901b1660748301526088820186905260a88083018690528351808403909101815260c8909201909252805191012060005414610eb857600080fd5b336001600160a01b03861614610ecd57600080fd5b600a86014310610edc57600080fd5b3415610ee757600080fd5b6040805182815290517f26bdc6b0a0806ec5cb3992c1b74dd2db228f99da5f09181980c9114c97ebf4079181900360200190a160408051600560208083019190915243828401526001600160601b0319606098891b8116898401529690971b9095166074860152608885019390935260a884019190915260c8808401919091528151808403909101815260e89092019052805191012060005550565b606082518383604051602001808461ffff1661ffff1660f01b815260020183805190602001908083835b60208310610fcc5780518252601f199092019160209182019101610fad565b51815160209384036101000a600019018019909216911617905285519190930192850191508083835b602083106110145780518252601f199092019160209182019101610ff5565b6001836020036101000a038019825116818451168082178552505050505050905001935050505060405160208183030381529060405290509291505056fea265627a7a723158205bcebd1c0127a01b0e4d751b9788fbb903507849d6a9924e0f0b6c7384d6bd0064736f6c634300050b0032";