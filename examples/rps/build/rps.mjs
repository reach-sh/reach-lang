// Automatically generated with Reach 0.1.0

import * as stdlib from '@reach-sh/stdlib';

export async function A(ctc, interact, v0, v1) {
  const txn0 = { balance: 0, value: 0 };
  const v2 = v0;
  const v3 = v1;
  const v4 = stdlib.isType('bool', await interact.params());
  const v5 = stdlib.add(v2, v3);
  const txn1 = await ctc.sendrecv('A', 'm1', [v2, v3], v5, 'e1', 10, 'e2');
  if (txn1.didTimeout) {
    const v7 = txn1.from;
    stdlib.assert(true);
    return ['Alice quits']; }
  else {
    const v6 = txn1.from;
    const v8 = txn1.value;
    const v9 = stdlib.add(v2, v3);
    const v10 = stdlib.eq(v8, v9);
    stdlib.assert(v10);
    const txn2 = await ctc.recv('A', 'e3', 10, true, [v2, v3, v6], 'm4', 'e4');
    if (txn2.didTimeout) {
      stdlib.assert(true);
      return ['Bob quits']; }
    else {
      const v12 = txn2.from;
      const [] = txn2.data;
      const v13 = txn2.value;
      const v14 = stdlib.eq(v13, v2);
      stdlib.assert(v14);
      const v15 = stdlib.isType('bytes', await interact.getHand());
      const v16 = stdlib.bytes_eq(v15, 'ROCK');
      const v17 = stdlib.bytes_eq(v15, 'PAPER');
      const v18 = stdlib.bytes_eq(v15, 'SCISSORS');
      const v19 = v16 ? true : v17;
      const v20 = v19 ? true : v18;
      stdlib.assert(v20);
      const v21 = v17 ? 1 : 2;
      const v22 = v16 ? 0 : v21;
      const v26 = stdlib.random_uint256();
      const v27 = stdlib.uint256_to_bytes(v26);
      const v28 = stdlib.uint256_to_bytes(v22);
      const v29 = stdlib.bytes_cat(v27, v28);
      const v30 = stdlib.keccak256(v29);
      const v31 = v30;
      const v32 = stdlib.isType('bool', await interact.commits());
      const txn3 = await ctc.sendrecv('A', 'm5', [v2, v3, v6, v12, v31], 0, 'e5', 10, 'e6');
      if (txn3.didTimeout) {
        stdlib.assert(true);
        return ['Alice quits']; }
      else {
        const v33 = txn3.value;
        const v34 = stdlib.eq(v33, 0);
        stdlib.assert(v34);
        const txn4 = await ctc.recv('A', 'e7', 10, true, [v2, v3, v6, v12, v31], 'm8', 'e8');
        if (txn4.didTimeout) {
          stdlib.assert(true);
          return ['Bob quits']; }
        else {
          const [v46] = txn4.data;
          const v48 = txn4.value;
          const v49 = stdlib.eq(v48, 0);
          stdlib.assert(v49);
          const v50 = stdlib.le(0, v46);
          const v51 = stdlib.lt(v46, 3);
          const v52 = v50 ? v51 : false;
          stdlib.assert(v52);
          const v53 = v26;
          const v54 = v22;
          const v55 = stdlib.le(0, v46);
          const v56 = stdlib.lt(v46, 3);
          const v57 = v55 ? v56 : false;
          stdlib.assert(v57);
          const v58 = stdlib.eq(v46, 0);
          const v59 = stdlib.eq(v46, 1);
          const v60 = v59 ? 'PAPER' : 'SCISSORS';
          const v61 = v58 ? 'ROCK' : v60;
          const v62 = stdlib.isType('bool', await interact.reveals(v61));
          const txn5 = await ctc.sendrecv('A', 'm9', [v2, v3, v6, v12, v31, v46, v53, v54], 0, 'e9', 10, 'e10');
          if (txn5.didTimeout) {
            stdlib.assert(true);
            return ['Alice quits']; }
          else {
            const v63 = txn5.value;
            const v64 = stdlib.eq(v63, 0);
            stdlib.assert(v64);
            const v65 = stdlib.uint256_to_bytes(v53);
            const v66 = stdlib.uint256_to_bytes(v54);
            const v67 = stdlib.bytes_cat(v65, v66);
            const v68 = stdlib.keccak256(v67);
            const v69 = stdlib.eq(v31, v68);
            stdlib.assert(v69);
            const v70 = stdlib.le(0, v54);
            const v71 = stdlib.lt(v54, 3);
            const v72 = v70 ? v71 : false;
            stdlib.assert(v72);
            const v73 = stdlib.le(0, v54);
            const v74 = stdlib.lt(v54, 3);
            const v75 = v73 ? v74 : false;
            const v76 = stdlib.le(0, v46);
            const v77 = stdlib.lt(v46, 3);
            const v78 = v76 ? v77 : false;
            const v79 = v75 ? v78 : false;
            const v80 = stdlib.sub(4, v46);
            const v81 = stdlib.add(v54, v80);
            const v82 = stdlib.mod(v81, 3);
            const v83 = v78 ? 0 : 1;
            const v84 = v75 ? 2 : v83;
            const v85 = v79 ? v82 : v84;
            const v124 = stdlib.isType('bool', await interact.outcome());
            const v125 = stdlib.le(0, v85);
            const v126 = stdlib.lt(v85, 5);
            const v127 = v125 ? v126 : false;
            stdlib.assert(v127);
            const v128 = stdlib.eq(v85, 0);
            const v129 = stdlib.eq(v85, 1);
            const v130 = stdlib.eq(v85, 2);
            const v131 = stdlib.eq(v85, 3);
            const v132 = v131 ? 'Alice quits' : 'Bob quits';
            const v133 = v130 ? 'Alice wins' : v132;
            const v134 = v129 ? 'Draw' : v133;
            const v135 = v128 ? 'Bob wins' : v134;
            return [v135]; } } } } } }

export async function B(ctc, interact) {
  const txn0 = { balance: 0, value: 0 };
  const txn1 = await ctc.recv('B', 'e1', 10, true, [], 'm2', 'e2');
  if (txn1.didTimeout) {
    const v7 = txn1.from;
    stdlib.assert(true);
    return ['Alice quits']; }
  else {
    const v6 = txn1.from;
    const [v2, v3] = txn1.data;
    const v8 = txn1.value;
    const v9 = stdlib.add(v2, v3);
    const v10 = stdlib.eq(v8, v9);
    stdlib.assert(v10);
    const v11 = stdlib.isType('bool', await interact.accepts(v2, v3));
    const txn2 = await ctc.sendrecv('B', 'm3', [v2, v3, v6], v2, 'e3', 10, 'e4');
    if (txn2.didTimeout) {
      stdlib.assert(true);
      return ['Bob quits']; }
    else {
      const v12 = txn2.from;
      const v13 = txn2.value;
      const v14 = stdlib.eq(v13, v2);
      stdlib.assert(v14);
      const txn3 = await ctc.recv('B', 'e5', 10, true, [v2, v3, v6, v12], 'm6', 'e6');
      if (txn3.didTimeout) {
        stdlib.assert(true);
        return ['Alice quits']; }
      else {
        const [v31] = txn3.data;
        const v33 = txn3.value;
        const v34 = stdlib.eq(v33, 0);
        stdlib.assert(v34);
        const v35 = stdlib.isType('bytes', await interact.getHand());
        const v36 = stdlib.bytes_eq(v35, 'ROCK');
        const v37 = stdlib.bytes_eq(v35, 'PAPER');
        const v38 = stdlib.bytes_eq(v35, 'SCISSORS');
        const v39 = v36 ? true : v37;
        const v40 = v39 ? true : v38;
        stdlib.assert(v40);
        const v41 = v37 ? 1 : 2;
        const v42 = v36 ? 0 : v41;
        const v46 = v42;
        const v47 = stdlib.isType('bool', await interact.shows());
        const txn4 = await ctc.sendrecv('B', 'm7', [v2, v3, v6, v12, v31, v46], 0, 'e7', 10, 'e8');
        if (txn4.didTimeout) {
          stdlib.assert(true);
          return ['Bob quits']; }
        else {
          const v48 = txn4.value;
          const v49 = stdlib.eq(v48, 0);
          stdlib.assert(v49);
          const v50 = stdlib.le(0, v46);
          const v51 = stdlib.lt(v46, 3);
          const v52 = v50 ? v51 : false;
          stdlib.assert(v52);
          const txn5 = await ctc.recv('B', 'e9', 10, true, [v2, v3, v6, v12, v31, v46], 'm10', 'e10');
          if (txn5.didTimeout) {
            stdlib.assert(true);
            return ['Alice quits']; }
          else {
            const [v53, v54] = txn5.data;
            const v63 = txn5.value;
            const v64 = stdlib.eq(v63, 0);
            stdlib.assert(v64);
            const v65 = stdlib.uint256_to_bytes(v53);
            const v66 = stdlib.uint256_to_bytes(v54);
            const v67 = stdlib.bytes_cat(v65, v66);
            const v68 = stdlib.keccak256(v67);
            const v69 = stdlib.eq(v31, v68);
            stdlib.assert(v69);
            const v70 = stdlib.le(0, v54);
            const v71 = stdlib.lt(v54, 3);
            const v72 = v70 ? v71 : false;
            stdlib.assert(v72);
            const v73 = stdlib.le(0, v54);
            const v74 = stdlib.lt(v54, 3);
            const v75 = v73 ? v74 : false;
            const v76 = stdlib.le(0, v46);
            const v77 = stdlib.lt(v46, 3);
            const v78 = v76 ? v77 : false;
            const v79 = v75 ? v78 : false;
            const v80 = stdlib.sub(4, v46);
            const v81 = stdlib.add(v54, v80);
            const v82 = stdlib.mod(v81, 3);
            const v83 = v78 ? 0 : 1;
            const v84 = v75 ? 2 : v83;
            const v85 = v79 ? v82 : v84;
            const v124 = stdlib.isType('bool', await interact.outcome());
            const v125 = stdlib.le(0, v85);
            const v126 = stdlib.lt(v85, 5);
            const v127 = v125 ? v126 : false;
            stdlib.assert(v127);
            const v128 = stdlib.eq(v85, 0);
            const v129 = stdlib.eq(v85, 1);
            const v130 = stdlib.eq(v85, 2);
            const v131 = stdlib.eq(v85, 3);
            const v132 = v131 ? 'Alice quits' : 'Bob quits';
            const v133 = v130 ? 'Alice wins' : v132;
            const v134 = v129 ? 'Draw' : v133;
            const v135 = v128 ? 'Bob wins' : v134;
            return [v135]; } } } } } }

export const ABI = [{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"uint256","name":"v2","type":"uint256"},{"internalType":"uint256","name":"v3","type":"uint256"},{"internalType":"address payable","name":"v6","type":"address"},{"internalType":"address payable","name":"v12","type":"address"},{"internalType":"uint256","name":"v31","type":"uint256"},{"internalType":"uint256","name":"v46","type":"uint256"}],"name":"m10","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"uint256","name":"v2","type":"uint256"},{"internalType":"uint256","name":"v3","type":"uint256"},{"internalType":"address payable","name":"v6","type":"address"},{"internalType":"address payable","name":"v12","type":"address"},{"internalType":"uint256","name":"v31","type":"uint256"},{"internalType":"uint256","name":"v46","type":"uint256"},{"internalType":"uint256","name":"v53","type":"uint256"},{"internalType":"uint256","name":"v54","type":"uint256"}],"name":"m9","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"uint256","name":"v2","type":"uint256"},{"internalType":"uint256","name":"v3","type":"uint256"},{"internalType":"address payable","name":"v6","type":"address"},{"internalType":"address payable","name":"v12","type":"address"},{"internalType":"uint256","name":"v31","type":"uint256"}],"name":"m5","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"}],"name":"m2","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"uint256","name":"v2","type":"uint256"},{"internalType":"uint256","name":"v3","type":"uint256"},{"internalType":"address payable","name":"v6","type":"address"}],"name":"m3","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"uint256","name":"v2","type":"uint256"},{"internalType":"uint256","name":"v3","type":"uint256"},{"internalType":"address payable","name":"v6","type":"address"},{"internalType":"address payable","name":"v12","type":"address"},{"internalType":"uint256","name":"v31","type":"uint256"}],"name":"m8","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"uint256","name":"v2","type":"uint256"},{"internalType":"uint256","name":"v3","type":"uint256"}],"name":"m1","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"uint256","name":"v2","type":"uint256"},{"internalType":"uint256","name":"v3","type":"uint256"},{"internalType":"address payable","name":"v6","type":"address"},{"internalType":"address payable","name":"v12","type":"address"}],"name":"m6","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"uint256","name":"v2","type":"uint256"},{"internalType":"uint256","name":"v3","type":"uint256"},{"internalType":"address payable","name":"v6","type":"address"}],"name":"m4","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"uint256","name":"v2","type":"uint256"},{"internalType":"uint256","name":"v3","type":"uint256"},{"internalType":"address payable","name":"v6","type":"address"},{"internalType":"address payable","name":"v12","type":"address"},{"internalType":"uint256","name":"v31","type":"uint256"},{"internalType":"uint256","name":"v46","type":"uint256"}],"name":"m7","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"inputs":[],"payable":true,"stateMutability":"payable","type":"constructor"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"v2","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v3","type":"uint256"}],"name":"e1","type":"event"},{"anonymous":false,"inputs":[],"name":"e2","type":"event"},{"anonymous":false,"inputs":[],"name":"e3","type":"event"},{"anonymous":false,"inputs":[],"name":"e4","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"v31","type":"uint256"}],"name":"e5","type":"event"},{"anonymous":false,"inputs":[],"name":"e6","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"v46","type":"uint256"}],"name":"e7","type":"event"},{"anonymous":false,"inputs":[],"name":"e8","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"v53","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v54","type":"uint256"}],"name":"e9","type":"event"},{"anonymous":false,"inputs":[],"name":"e10","type":"event"}];

export const Bytecode = "0x600060a08181524360c0526040608081905260e0815290209055611062806100286000396000f3fe6080604052600436106100915760003560e01c80635696e841116100595780635696e841146101df5780637a52ccb3146102275780638d2ad56814610250578063b98657da14610290578063fd4dbd5b146102c857610091565b80632d1e9042146100965780632d3d3e70146100e657806332053d601461014257806336807e781461018a57806342f8f3b8146101a7575b600080fd5b6100e4600480360360e08110156100ac57600080fd5b508035906020810135906040810135906001600160a01b03606082013581169160808101359091169060a08101359060c00135610316565b005b6100e460048036036101208110156100fd57600080fd5b508035906020810135906040810135906001600160a01b03606082013581169160808101359091169060a08101359060c08101359060e0810135906101000135610417565b6100e4600480360360c081101561015857600080fd5b508035906020810135906040810135906001600160a01b03606082013581169160808101359091169060a001356107b7565b6100e4600480360360208110156101a057600080fd5b50356108e6565b6100e4600480360360808110156101bd57600080fd5b50803590602081013590604081013590606001356001600160a01b031661095f565b6100e4600480360360c08110156101f557600080fd5b508035906020810135906040810135906001600160a01b03606082013581169160808101359091169060a00135610a5e565b6100e46004803603606081101561023d57600080fd5b5080359060208101359060400135610b57565b6100e4600480360360a081101561026657600080fd5b508035906020810135906040810135906001600160a01b0360608201358116916080013516610c3d565b6100e4600480360360808110156102a657600080fd5b50803590602081013590604081013590606001356001600160a01b0316610d2e565b6100e4600480360360e08110156102de57600080fd5b508035906020810135906040810135906001600160a01b03606082013581169160808101359091169060a08101359060c00135610e14565b6040805160076020808301919091528183018a905260608083018a9052608083018990526001600160601b031988821b811660a08501529087901b1660b483015260c8820185905260e88083018590528351808403909101815261010890920190925280519101206000541461038b57600080fd5b336001600160a01b038416146103a057600080fd5b600a87014310156103b057600080fd5b6040516001600160a01b03841690303180156108fc02916000818181858888f193505050501580156103e6573d6000803e3d6000fd5b506040517f5ef1d939728ae307281ba62215efdccdf0b99fa9cc412f247b7ab97b4729b74f90600090a16000805533ff5b6040805160076020808301919091528183018c905260608083018c9052608083018b90526001600160601b03198a821b811660a08501529089901b1660b483015260c8820187905260e88083018790528351808403909101815261010890920190925280519101206000541461048c57600080fd5b336001600160a01b038716146104a157600080fd5b600a890143106104b057600080fd5b34156104bb57600080fd5b60408051602080820185905282518083039091018152818301835260608083018590528351808403909101815260809092019092526104fa9190610f5e565b6040516020018082805190602001908083835b6020831061052c5780518252601f19909201916020918201910161050d565b6001836020036101000a0380198251168184511680821785525050505050509050019150506040516020818303038152906040528051906020012060001c841461057557600080fd5b6003811061058257600080fd5b6001600160a01b0386166108fc6002600384106105a05760006105a5565b600386105b6105d057600384106105c857600386106105c05760016105c3565b60005b6105cb565b60025b6105e1565b6003866004038501816105df57fe5b065b1461064b576000600384106105f75760006105fc565b600386105b610627576003841061061f576003861061061757600161061a565b60005b610622565b60025b610638565b60038660040385018161063657fe5b065b146106435789610646565b60005b610650565b896002025b89019081150290604051600060405180830381858888f1935050505015801561067d573d6000803e3d6000fd5b506001600160a01b0385166108fc60026003841061069c5760006106a1565b600386105b6106cc57600384106106c457600386106106bc5760016106bf565b60005b6106c7565b60025b6106dd565b6003866004038501816106db57fe5b065b14610749576000600384106106f35760006106f8565b600386105b610723576003841061071b5760038610610713576001610716565b60005b61071e565b60025b610734565b60038660040385018161073257fe5b065b1461073f5789610744565b896002025b61074c565b60005b6040518115909202916000818181858888f19350505050158015610774573d6000803e3d6000fd5b50604080518381526020810183905281517f6d32a2ce93084268e9ee8e741d4257b2c396b94a1ffc25aeb6fc4ba8fc0594d2929181900390910190a16000805533ff5b6040805160036020808301919091528183018990526060808301899052608083018890526001600160601b031987821b811660a08501529086901b1660b4830152825160a881840301815260c890920190925280519101206000541461081c57600080fd5b336001600160a01b0384161461083157600080fd5b600a8601431061084057600080fd5b341561084b57600080fd5b6040805182815290517f26bdc6b0a0806ec5cb3992c1b74dd2db228f99da5f09181980c9114c97ebf4079181900360200190a1604080516005602080830191909152438284015260608083019890985260808201969096526001600160601b031994871b851660a08201529290951b90921660b482015260c8808201929092528351808203909201825260e801909252815191012060005550565b604080516000602080830182905282840185905283518084038501815260609093019093528151919092012090541461091e57600080fd5b33600a820143101561092f57600080fd5b6040517f9b31f9e88fd11f71bfbf93b0237bc9a0900b8479a307f60435e40543e383403590600090a16000805533ff5b60408051600160208083019190915281830187905260608083018790526080830186905284901b6001600160601b03191660a08301528251808303609401815260b49092019092528051910120600054146109b957600080fd5b33600a850143106109c957600080fd5b8334146109d557600080fd5b6040517f56800b5260512456b844fc9371dbdb10a629694349bc1829c0518bc84861b0c190600090a16040805160036020808301919091524382840152606080830197909752608082019590955292851b6001600160601b031990811660a08501529190941b1660b4820152825180820360a801815260c8909101909252815191012060005550565b6040805160056020808301919091528183018990526060808301899052608083018890526001600160601b031987821b811660a08501529086901b1660b483015260c88083018590528351808403909101815260e8909201909252805191012060005414610acb57600080fd5b336001600160a01b03841614610ae057600080fd5b600a8601431015610af057600080fd5b6040516001600160a01b03841690303180156108fc02916000818181858888f19350505050158015610b26573d6000803e3d6000fd5b506040517fffa3d43ab9b6e5b34273fd049718a85553427384eed63754abc672936df0584e90600090a16000805533ff5b6040805160006020808301829052828401879052835180840385018152606090930190935281519190920120905414610b8f57600080fd5b33600a84014310610b9f57600080fd5b8183013414610bad57600080fd5b604080518481526020810184905281517fc9d006980a027d32b08195d9eab835f93f490ac52038c85ed3e8aca6e0b9b2e1929181900390910190a1604080516001602080830191909152438284015260608083019690965260808201949094529190931b6001600160601b03191660a08201528251808203609401815260b4909101909252815191012060005550565b6040805160036020808301919091528183018890526060808301889052608083018790526001600160601b031986821b811660a08501529085901b1660b4830152825160a881840301815260c8909201909252805191012060005414610ca257600080fd5b336001600160a01b03821614610cb757600080fd5b600a8501431015610cc757600080fd5b6040516001600160a01b03821690303180156108fc02916000818181858888f19350505050158015610cfd573d6000803e3d6000fd5b506040517fd6694479677b4939b334fce2b8096952c15688edcb9eb18da0a00fbde0d22fb690600090a16000805533ff5b60408051600160208083019190915281830187905260608083018790526080830186905284901b6001600160601b03191660a08301528251808303609401815260b4909201909252805191012060005414610d8857600080fd5b336001600160a01b03821614610d9d57600080fd5b600a8401431015610dad57600080fd5b6040516001600160a01b03821690303180156108fc02916000818181858888f19350505050158015610de3573d6000803e3d6000fd5b506040517f79e5f05486520ee227978e44df5013b958eb8b7c4045fd17db1c8f340f2c361190600090a16000805533ff5b6040805160056020808301919091528183018a905260608083018a9052608083018990526001600160601b031988821b811660a08501529087901b1660b483015260c88083018690528351808403909101815260e8909201909252805191012060005414610e8157600080fd5b336001600160a01b03841614610e9657600080fd5b600a87014310610ea557600080fd5b3415610eb057600080fd5b60038110610ebd57600080fd5b6040805182815290517ffc55d683ac816a7149ebdfa999ae1bcfeeae27c37c9dab64a23f617beed2a0079181900360200190a1604080516007602080830191909152438284015260608083019990995260808201979097526001600160601b031995881b861660a08201529390961b90931660b483015260c882015260e8808201929092528351808203909201825261010801909252815191012060005550565b606082518383604051602001808461ffff1661ffff1660f01b815260020183805190602001908083835b60208310610fa75780518252601f199092019160209182019101610f88565b51815160209384036101000a600019018019909216911617905285519190930192850191508083835b60208310610fef5780518252601f199092019160209182019101610fd0565b6001836020036101000a038019825116818451168082178552505050505050905001935050505060405160208183030381529060405290509291505056fea265627a7a723158201983f13bc76c64d3ff3a93526470f4eb1f42bb5abeadce6ea9e9fca76395ae4c64736f6c634300050b0032";