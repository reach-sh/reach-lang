// Automatically generated with Reach 0.1.0

import * as stdlib from '@reach-sh/stdlib';

export async function A(ctc, interact, v1, v2) {
  const txn0 = { balance: 0, value: 0 };
  const v5 = v1;
  const v6 = v2;
  const v7 = stdlib.isType('bool', await interact.params());
  const v8 = stdlib.add(v5, v6);
  const txn1 = await ctc.sendrecv('A', 'm1', [v5, v6], v8, 'e1', 10, 'e2');
  if (txn1.didTimeout) {
    stdlib.assert(true);
    return ['Alice quits']; }
  else {
    const v0 = txn1.from;
    const v9 = txn1.value;
    const v10 = stdlib.add(v5, v6);
    const v11 = stdlib.eq(v9, v10);
    stdlib.assert(v11);
    const txn2 = await ctc.recv('A', 'e3', 10, true, [v0, v5, v6], 'm4', 'e4');
    if (txn2.didTimeout) {
      stdlib.assert(true);
      return ['Bob quits']; }
    else {
      const [] = txn2.data;
      const v3 = txn2.from;
      const v13 = txn2.value;
      const v14 = stdlib.eq(v13, v5);
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
      const txn3 = await ctc.sendrecv('A', 'm5', [v0, v3, v5, v6, v31], 0, 'e5', 10, 'e6');
      if (txn3.didTimeout) {
        stdlib.assert(true);
        return ['Alice quits']; }
      else {
        const v33 = txn3.value;
        const v34 = stdlib.eq(v33, 0);
        stdlib.assert(v34);
        const txn4 = await ctc.recv('A', 'e7', 10, true, [v0, v3, v5, v6, v31], 'm8', 'e8');
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
          const txn5 = await ctc.sendrecv('A', 'm9', [v0, v3, v5, v6, v31, v46, v53, v54], 0, 'e9', 10, 'e10');
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
    stdlib.assert(true);
    return ['Alice quits']; }
  else {
    const [v5, v6] = txn1.data;
    const v0 = txn1.from;
    const v9 = txn1.value;
    const v10 = stdlib.add(v5, v6);
    const v11 = stdlib.eq(v9, v10);
    stdlib.assert(v11);
    const v12 = stdlib.isType('bool', await interact.accepts(v5, v6));
    const txn2 = await ctc.sendrecv('B', 'm3', [v0, v5, v6], v5, 'e3', 10, 'e4');
    if (txn2.didTimeout) {
      stdlib.assert(true);
      return ['Bob quits']; }
    else {
      const v3 = txn2.from;
      const v13 = txn2.value;
      const v14 = stdlib.eq(v13, v5);
      stdlib.assert(v14);
      const txn3 = await ctc.recv('B', 'e5', 10, true, [v0, v3, v5, v6], 'm6', 'e6');
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
        const txn4 = await ctc.sendrecv('B', 'm7', [v0, v3, v5, v6, v31, v46], 0, 'e7', 10, 'e8');
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
          const txn5 = await ctc.recv('B', 'e9', 10, true, [v0, v3, v5, v6, v31, v46], 'm10', 'e10');
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

export async function O(ctc, interact) {
  const txn0 = { balance: 0, value: 0 };
  const txn1 = await ctc.recv('O', 'e1', 10, true, [], 'm2', 'e2');
  if (txn1.didTimeout) {
    stdlib.assert(true);
    return ['Alice quits']; }
  else {
    const [v5, v6] = txn1.data;
    const v0 = txn1.from;
    const v9 = txn1.value;
    const v10 = stdlib.add(v5, v6);
    const v11 = stdlib.eq(v9, v10);
    stdlib.assert(v11);
    const txn2 = await ctc.recv('O', 'e3', 10, false, [v0, v5, v6], 'm4', 'e4');
    if (txn2.didTimeout) {
      stdlib.assert(true);
      return ['Bob quits']; }
    else {
      const [] = txn2.data;
      const v3 = txn2.from;
      const v13 = txn2.value;
      const v14 = stdlib.eq(v13, v5);
      stdlib.assert(v14);
      const txn3 = await ctc.recv('O', 'e5', 10, false, [v0, v3, v5, v6], 'm6', 'e6');
      if (txn3.didTimeout) {
        stdlib.assert(true);
        return ['Alice quits']; }
      else {
        const [v31] = txn3.data;
        const v33 = txn3.value;
        const v34 = stdlib.eq(v33, 0);
        stdlib.assert(v34);
        const txn4 = await ctc.recv('O', 'e7', 10, false, [v0, v3, v5, v6, v31], 'm8', 'e8');
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
          const txn5 = await ctc.recv('O', 'e9', 10, false, [v0, v3, v5, v6, v31, v46], 'm10', 'e10');
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

export const ABI = [{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v0","type":"address"},{"internalType":"address payable","name":"v3","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"},{"internalType":"uint256","name":"v31","type":"uint256"},{"internalType":"uint256","name":"v46","type":"uint256"},{"internalType":"uint256","name":"v53","type":"uint256"},{"internalType":"uint256","name":"v54","type":"uint256"}],"name":"m9","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"}],"name":"m2","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v0","type":"address"},{"internalType":"address payable","name":"v3","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"},{"internalType":"uint256","name":"v31","type":"uint256"},{"internalType":"uint256","name":"v46","type":"uint256"}],"name":"m7","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"}],"name":"m1","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v0","type":"address"},{"internalType":"address payable","name":"v3","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"},{"internalType":"uint256","name":"v31","type":"uint256"},{"internalType":"uint256","name":"v46","type":"uint256"}],"name":"m10","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v0","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"}],"name":"m4","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v0","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"}],"name":"m3","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v0","type":"address"},{"internalType":"address payable","name":"v3","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"}],"name":"m6","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v0","type":"address"},{"internalType":"address payable","name":"v3","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"},{"internalType":"uint256","name":"v31","type":"uint256"}],"name":"m8","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v0","type":"address"},{"internalType":"address payable","name":"v3","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"},{"internalType":"uint256","name":"v31","type":"uint256"}],"name":"m5","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"inputs":[],"payable":true,"stateMutability":"payable","type":"constructor"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"v5","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v6","type":"uint256"}],"name":"e1","type":"event"},{"anonymous":false,"inputs":[],"name":"e2","type":"event"},{"anonymous":false,"inputs":[],"name":"e3","type":"event"},{"anonymous":false,"inputs":[],"name":"e4","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"v31","type":"uint256"}],"name":"e5","type":"event"},{"anonymous":false,"inputs":[],"name":"e6","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"v46","type":"uint256"}],"name":"e7","type":"event"},{"anonymous":false,"inputs":[],"name":"e8","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"v53","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v54","type":"uint256"}],"name":"e9","type":"event"},{"anonymous":false,"inputs":[],"name":"e10","type":"event"}];

export const Bytecode = "0x600060a08181524360c0526040608081905260e081529020905561101a806100286000396000f3fe6080604052600436106100915760003560e01c80639ad83246116100595780639ad83246146101d6578063b8b8cfad1461020e578063d212999514610246578063d62511ba14610288578063e9398627146102d057610091565b80632fa0a8f81461009657806336807e78146100f45780634441778b146101115780637a52ccb31461015f57806380c418dc14610188575b600080fd5b6100f260048036036101208110156100ad57600080fd5b508035906001600160a01b03602082013581169160408101359091169060608101359060808101359060a08101359060c08101359060e0810135906101000135610318565b005b6100f26004803603602081101561010a57600080fd5b5035610607565b6100f2600480360360e081101561012757600080fd5b508035906001600160a01b03602082013581169160408101359091169060608101359060808101359060a08101359060c0013561067f565b6100f26004803603606081101561017557600080fd5b50803590602081013590604001356107c9565b6100f2600480360360e081101561019e57600080fd5b508035906001600160a01b03602082013581169160408101359091169060608101359060808101359060a08101359060c001356108bd565b6100f2600480360360808110156101ec57600080fd5b508035906001600160a01b0360208201351690604081013590606001356109bd565b6100f26004803603608081101561022457600080fd5b508035906001600160a01b036020820135169060408101359060600135610aa4565b6100f2600480360360a081101561025c57600080fd5b508035906001600160a01b03602082013581169160408101359091169060608101359060800135610baf565b6100f2600480360360c081101561029e57600080fd5b508035906001600160a01b03602082013581169160408101359091169060608101359060808101359060a00135610ca0565b6100f2600480360360c08110156102e657600080fd5b508035906001600160a01b03602082013581169160408101359091169060608101359060808101359060a00135610d98565b6040805160076020808301919091528183018c90526001600160601b031960608c811b8216818501528b901b1660748301526088820189905260a8820188905260c8820187905260e88083018790528351808403909101815261010890920190925280519101206000541461038c57600080fd5b610394610f97565b336001600160a01b038a16146103a957600080fd5b600a8a0143106103b857600080fd5b34156103c357600080fd5b60408051602080820186905282518083039091018152818301835260608083018690528351808403909101815260809092019092526104029190610ec8565b6040516020018082805190602001908083835b602083106104345780518252601f199092019160209182019101610415565b6001836020036101000a0380198251168184511680821785525050505050509050019150506040516020818303038152906040528051906020012060001c851461047d57600080fd5b6003821061048a57600080fd5b600380831080835290851060208301526104a55760006104ab565b80602001515b6104d55780516104cd5780602001516104c55760016104c8565b60005b6104d0565b60025b6104e6565b6003846004038301816104e457fe5b065b60408201819052600281146060830181905290156080830181905260a0830182905260c08301526001600160a01b038a16906108fc90610537578260c0015161052f5788610532565b60005b61053c565b886002025b88019081150290604051600060405180830381858888f19350505050158015610569573d6000803e3d6000fd5b50876001600160a01b03166108fc8260a00151610599578260c0015161058f5788610594565b886002025b61059c565b60005b6040518115909202916000818181858888f193505050501580156105c4573d6000803e3d6000fd5b50604080518481526020810184905281517f6d32a2ce93084268e9ee8e741d4257b2c396b94a1ffc25aeb6fc4ba8fc0594d2929181900390910190a16000805533ff5b604080516000602080830182905282840185905283518084038501815260609093019093528151919092012090541461063f57600080fd5b600a810143101561064f57600080fd5b6040517f9b31f9e88fd11f71bfbf93b0237bc9a0900b8479a307f60435e40543e383403590600090a16000805533ff5b6040805160056020808301919091528183018a90526001600160601b031960608a811b82168185015289901b1660748301526088820187905260a8820186905260c88083018690528351808403909101815260e89092019092528051910120600054146106eb57600080fd5b336001600160a01b0386161461070057600080fd5b600a8701431061070f57600080fd5b341561071a57600080fd5b6003811061072757600080fd5b6040805182815290517ffc55d683ac816a7149ebdfa999ae1bcfeeae27c37c9dab64a23f617beed2a0079181900360200190a160408051600760208083019190915243828401526001600160601b03196060998a1b81168a8401529790981b9096166074870152608886019490945260a885019290925260c884015260e880840191909152815180840390910181526101089092019052805191012060005550565b604080516000602080830182905282840187905283518084038501815260609093019093528151919092012090541461080157600080fd5b610809610fd3565b338152600a8401431061081b57600080fd5b818301341461082957600080fd5b604080518481526020810184905281517fc9d006980a027d32b08195d9eab835f93f490ac52038c85ed3e8aca6e0b9b2e1929181900390910190a15160408051600160208083019190915243828401526001600160601b0319606094851b169382019390935260748101949094526094808501939093528051808503909301835260b4909301909252805191012060005550565b6040805160076020808301919091528183018a90526001600160601b031960608a811b82168185015289901b1660748301526088820187905260a8820186905260c8820185905260e88083018590528351808403909101815261010890920190925280519101206000541461093157600080fd5b336001600160a01b0386161461094657600080fd5b600a870143101561095657600080fd5b6040516001600160a01b03861690303180156108fc02916000818181858888f1935050505015801561098c573d6000803e3d6000fd5b506040517f5ef1d939728ae307281ba62215efdccdf0b99fa9cc412f247b7ab97b4729b74f90600090a16000805533ff5b604080516001602080830191909152818301879052606086811b6001600160601b031916908301526074820185905260948083018590528351808403909101815260b4909201909252805191012060005414610a1857600080fd5b336001600160a01b03841614610a2d57600080fd5b600a8401431015610a3d57600080fd5b6040516001600160a01b03841690303180156108fc02916000818181858888f19350505050158015610a73573d6000803e3d6000fd5b506040517f79e5f05486520ee227978e44df5013b958eb8b7c4045fd17db1c8f340f2c361190600090a16000805533ff5b604080516001602080830191909152818301879052606086811b6001600160601b031916908301526074820185905260948083018590528351808403909101815260b4909201909252805191012060005414610aff57600080fd5b610b07610fd3565b338152600a85014310610b1957600080fd5b823414610b2557600080fd5b6040517f56800b5260512456b844fc9371dbdb10a629694349bc1829c0518bc84861b0c190600090a15160408051600360208083019190915243828401526001600160601b0319606097881b8116888401529390961b9092166074830152608882019390935260a8808201929092528251808203909201825260c801909152805191012060005550565b6040805160036020808301919091528183018890526001600160601b0319606088811b82168185015287901b1660748301526088820185905260a88083018590528351808403909101815260c8909201909252805191012060005414610c1457600080fd5b336001600160a01b03841614610c2957600080fd5b600a8501431015610c3957600080fd5b6040516001600160a01b03841690303180156108fc02916000818181858888f19350505050158015610c6f573d6000803e3d6000fd5b506040517fd6694479677b4939b334fce2b8096952c15688edcb9eb18da0a00fbde0d22fb690600090a16000805533ff5b6040805160056020808301919091528183018990526001600160601b0319606089811b82168185015288901b1660748301526088820186905260a8820185905260c88083018590528351808403909101815260e8909201909252805191012060005414610d0c57600080fd5b336001600160a01b03861614610d2157600080fd5b600a8601431015610d3157600080fd5b6040516001600160a01b03861690303180156108fc02916000818181858888f19350505050158015610d67573d6000803e3d6000fd5b506040517fffa3d43ab9b6e5b34273fd049718a85553427384eed63754abc672936df0584e90600090a16000805533ff5b6040805160036020808301919091528183018990526001600160601b0319606089811b82168185015288901b1660748301526088820186905260a88083018690528351808403909101815260c8909201909252805191012060005414610dfd57600080fd5b336001600160a01b03861614610e1257600080fd5b600a86014310610e2157600080fd5b3415610e2c57600080fd5b6040805182815290517f26bdc6b0a0806ec5cb3992c1b74dd2db228f99da5f09181980c9114c97ebf4079181900360200190a160408051600560208083019190915243828401526001600160601b0319606098891b8116898401529690971b9095166074860152608885019390935260a884019190915260c8808401919091528151808403909101815260e89092019052805191012060005550565b606082518383604051602001808461ffff1661ffff1660f01b815260020183805190602001908083835b60208310610f115780518252601f199092019160209182019101610ef2565b51815160209384036101000a600019018019909216911617905285519190930192850191508083835b60208310610f595780518252601f199092019160209182019101610f3a565b6001836020036101000a0380198251168184511680821785525050505050509050019350505050604051602081830303815290604052905092915050565b6040805160e081018252600080825260208201819052918101829052606081018290526080810182905260a0810182905260c081019190915290565b6040805160208101909152600081529056fea265627a7a7231582000d5c32ba2f494ac014976f5fd6f79894ce53b4ab9a2b751b7c76e363fd0540264736f6c634300050b0032";