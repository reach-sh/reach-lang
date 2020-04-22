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
    // FromConsensus
    stdlib.assert(true);
    return ['Alice quits']; }
  else {
    const v0 = txn1.from;
    const v9 = txn1.value;
    const v10 = stdlib.add(v5, v6);
    const v11 = stdlib.eq(v9, v10);
    stdlib.assert(v11);
    // FromConsensus
    const txn2 = await ctc.recv('A', 'e3', 10, true, [v0, v5, v6], 'm4', 'e4');
    if (txn2.didTimeout) {
      const v133 = txn2.balance;
      // txn.transfer(v0, v133);
      // FromConsensus
      stdlib.assert(true);
      return ['Bob quits']; }
    else {
      const [] = txn2.data;
      const v3 = txn2.from;
      const v13 = txn2.value;
      const v14 = stdlib.eq(v13, v5);
      stdlib.assert(v14);
      // FromConsensus
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
      const v27 = stdlib.keccak256(v26, v22);
      const v28 = v27;
      const v29 = stdlib.isType('bool', await interact.commits());
      const txn3 = await ctc.sendrecv('A', 'm5', [v0, v3, v5, v6, v28], 0, 'e5', 10, 'e6');
      if (txn3.didTimeout) {
        const v132 = txn3.balance;
        // txn.transfer(v3, v132);
        // FromConsensus
        stdlib.assert(true);
        return ['Alice quits']; }
      else {
        const v30 = txn3.value;
        const v31 = stdlib.eq(v30, 0);
        stdlib.assert(v31);
        // FromConsensus
        const txn4 = await ctc.recv('A', 'e7', 10, true, [v0, v3, v5, v6, v28], 'm8', 'e8');
        if (txn4.didTimeout) {
          const v131 = txn4.balance;
          // txn.transfer(v0, v131);
          // FromConsensus
          stdlib.assert(true);
          return ['Bob quits']; }
        else {
          const [v43] = txn4.data;
          const v45 = txn4.value;
          const v46 = stdlib.eq(v45, 0);
          stdlib.assert(v46);
          const v47 = stdlib.le(0, v43);
          const v48 = stdlib.lt(v43, 3);
          const v49 = v47 ? v48 : false;
          stdlib.assert(v49);
          // FromConsensus
          const v50 = v26;
          const v51 = v22;
          const v52 = stdlib.le(0, v43);
          const v53 = stdlib.lt(v43, 3);
          const v54 = v52 ? v53 : false;
          stdlib.assert(v54);
          const v55 = stdlib.eq(v43, 0);
          const v56 = stdlib.eq(v43, 1);
          const v57 = v56 ? 'PAPER' : 'SCISSORS';
          const v58 = v55 ? 'ROCK' : v57;
          const v59 = stdlib.isType('bool', await interact.reveals(v58));
          const txn5 = await ctc.sendrecv('A', 'm9', [v0, v3, v5, v6, v28, v43, v50, v51], 0, 'e9', 10, 'e10');
          if (txn5.didTimeout) {
            const v130 = txn5.balance;
            // txn.transfer(v3, v130);
            // FromConsensus
            stdlib.assert(true);
            return ['Alice quits']; }
          else {
            const v60 = txn5.value;
            const v61 = stdlib.eq(v60, 0);
            stdlib.assert(v61);
            const v62 = stdlib.keccak256(v50, v51);
            const v63 = stdlib.eq(v28, v62);
            stdlib.assert(v63);
            const v64 = stdlib.le(0, v51);
            const v65 = stdlib.lt(v51, 3);
            const v66 = v64 ? v65 : false;
            stdlib.assert(v66);
            const v67 = stdlib.le(0, v51);
            const v68 = stdlib.lt(v51, 3);
            const v69 = v67 ? v68 : false;
            const v70 = stdlib.le(0, v43);
            const v71 = stdlib.lt(v43, 3);
            const v72 = v70 ? v71 : false;
            const v73 = v69 ? v72 : false;
            const v74 = stdlib.sub(4, v43);
            const v75 = stdlib.add(v51, v74);
            const v76 = stdlib.mod(v75, 3);
            const v77 = v72 ? 0 : 1;
            const v78 = v69 ? 2 : v77;
            const v79 = v73 ? v76 : v78;
            const v109 = stdlib.eq(v79, 2);
            const v110 = stdlib.mul(2, v5);
            const v111 = stdlib.eq(v79, 0);
            const v112 = stdlib.mul(2, v5);
            const v113 = v111 ? 0 : v5;
            const v114 = v111 ? v112 : v5;
            const v115 = v109 ? v110 : v113;
            const v116 = v109 ? 0 : v114;
            const v117 = stdlib.add(v6, v115);
            // txn.transfer(v0, v117);
            // txn.transfer(v3, v116);
            // FromConsensus
            const v118 = stdlib.isType('bool', await interact.outcome());
            const v119 = stdlib.le(0, v79);
            const v120 = stdlib.lt(v79, 5);
            const v121 = v119 ? v120 : false;
            stdlib.assert(v121);
            const v122 = stdlib.eq(v79, 0);
            const v123 = stdlib.eq(v79, 1);
            const v124 = stdlib.eq(v79, 2);
            const v125 = stdlib.eq(v79, 3);
            const v126 = v125 ? 'Alice quits' : 'Bob quits';
            const v127 = v124 ? 'Alice wins' : v126;
            const v128 = v123 ? 'Draw' : v127;
            const v129 = v122 ? 'Bob wins' : v128;
            return [v129]; } } } } } }

export async function B(ctc, interact) {
  const txn0 = { balance: 0, value: 0 };
  const txn1 = await ctc.recv('B', 'e1', 10, true, [], 'm2', 'e2');
  if (txn1.didTimeout) {
    // FromConsensus
    stdlib.assert(true);
    return ['Alice quits']; }
  else {
    const [v5, v6] = txn1.data;
    const v0 = txn1.from;
    const v9 = txn1.value;
    const v10 = stdlib.add(v5, v6);
    const v11 = stdlib.eq(v9, v10);
    stdlib.assert(v11);
    // FromConsensus
    const v12 = stdlib.isType('bool', await interact.accepts(v5, v6));
    const txn2 = await ctc.sendrecv('B', 'm3', [v0, v5, v6], v5, 'e3', 10, 'e4');
    if (txn2.didTimeout) {
      const v133 = txn2.balance;
      // txn.transfer(v0, v133);
      // FromConsensus
      stdlib.assert(true);
      return ['Bob quits']; }
    else {
      const v3 = txn2.from;
      const v13 = txn2.value;
      const v14 = stdlib.eq(v13, v5);
      stdlib.assert(v14);
      // FromConsensus
      const txn3 = await ctc.recv('B', 'e5', 10, true, [v0, v3, v5, v6], 'm6', 'e6');
      if (txn3.didTimeout) {
        const v132 = txn3.balance;
        // txn.transfer(v3, v132);
        // FromConsensus
        stdlib.assert(true);
        return ['Alice quits']; }
      else {
        const [v28] = txn3.data;
        const v30 = txn3.value;
        const v31 = stdlib.eq(v30, 0);
        stdlib.assert(v31);
        // FromConsensus
        const v32 = stdlib.isType('bytes', await interact.getHand());
        const v33 = stdlib.bytes_eq(v32, 'ROCK');
        const v34 = stdlib.bytes_eq(v32, 'PAPER');
        const v35 = stdlib.bytes_eq(v32, 'SCISSORS');
        const v36 = v33 ? true : v34;
        const v37 = v36 ? true : v35;
        stdlib.assert(v37);
        const v38 = v34 ? 1 : 2;
        const v39 = v33 ? 0 : v38;
        const v43 = v39;
        const v44 = stdlib.isType('bool', await interact.shows());
        const txn4 = await ctc.sendrecv('B', 'm7', [v0, v3, v5, v6, v28, v43], 0, 'e7', 10, 'e8');
        if (txn4.didTimeout) {
          const v131 = txn4.balance;
          // txn.transfer(v0, v131);
          // FromConsensus
          stdlib.assert(true);
          return ['Bob quits']; }
        else {
          const v45 = txn4.value;
          const v46 = stdlib.eq(v45, 0);
          stdlib.assert(v46);
          const v47 = stdlib.le(0, v43);
          const v48 = stdlib.lt(v43, 3);
          const v49 = v47 ? v48 : false;
          stdlib.assert(v49);
          // FromConsensus
          const txn5 = await ctc.recv('B', 'e9', 10, true, [v0, v3, v5, v6, v28, v43], 'm10', 'e10');
          if (txn5.didTimeout) {
            const v130 = txn5.balance;
            // txn.transfer(v3, v130);
            // FromConsensus
            stdlib.assert(true);
            return ['Alice quits']; }
          else {
            const [v50, v51] = txn5.data;
            const v60 = txn5.value;
            const v61 = stdlib.eq(v60, 0);
            stdlib.assert(v61);
            const v62 = stdlib.keccak256(v50, v51);
            const v63 = stdlib.eq(v28, v62);
            stdlib.assert(v63);
            const v64 = stdlib.le(0, v51);
            const v65 = stdlib.lt(v51, 3);
            const v66 = v64 ? v65 : false;
            stdlib.assert(v66);
            const v67 = stdlib.le(0, v51);
            const v68 = stdlib.lt(v51, 3);
            const v69 = v67 ? v68 : false;
            const v70 = stdlib.le(0, v43);
            const v71 = stdlib.lt(v43, 3);
            const v72 = v70 ? v71 : false;
            const v73 = v69 ? v72 : false;
            const v74 = stdlib.sub(4, v43);
            const v75 = stdlib.add(v51, v74);
            const v76 = stdlib.mod(v75, 3);
            const v77 = v72 ? 0 : 1;
            const v78 = v69 ? 2 : v77;
            const v79 = v73 ? v76 : v78;
            const v109 = stdlib.eq(v79, 2);
            const v110 = stdlib.mul(2, v5);
            const v111 = stdlib.eq(v79, 0);
            const v112 = stdlib.mul(2, v5);
            const v113 = v111 ? 0 : v5;
            const v114 = v111 ? v112 : v5;
            const v115 = v109 ? v110 : v113;
            const v116 = v109 ? 0 : v114;
            const v117 = stdlib.add(v6, v115);
            // txn.transfer(v0, v117);
            // txn.transfer(v3, v116);
            // FromConsensus
            const v118 = stdlib.isType('bool', await interact.outcome());
            const v119 = stdlib.le(0, v79);
            const v120 = stdlib.lt(v79, 5);
            const v121 = v119 ? v120 : false;
            stdlib.assert(v121);
            const v122 = stdlib.eq(v79, 0);
            const v123 = stdlib.eq(v79, 1);
            const v124 = stdlib.eq(v79, 2);
            const v125 = stdlib.eq(v79, 3);
            const v126 = v125 ? 'Alice quits' : 'Bob quits';
            const v127 = v124 ? 'Alice wins' : v126;
            const v128 = v123 ? 'Draw' : v127;
            const v129 = v122 ? 'Bob wins' : v128;
            return [v129]; } } } } } }

export async function O(ctc, interact) {
  const txn0 = { balance: 0, value: 0 };
  const txn1 = await ctc.recv('O', 'e1', 10, true, [], 'm2', 'e2');
  if (txn1.didTimeout) {
    // FromConsensus
    stdlib.assert(true);
    return ['Alice quits']; }
  else {
    const [v5, v6] = txn1.data;
    const v0 = txn1.from;
    const v9 = txn1.value;
    const v10 = stdlib.add(v5, v6);
    const v11 = stdlib.eq(v9, v10);
    stdlib.assert(v11);
    // FromConsensus
    const txn2 = await ctc.recv('O', 'e3', 10, false, [v0, v5, v6], 'm4', 'e4');
    if (txn2.didTimeout) {
      const v133 = txn2.balance;
      // txn.transfer(v0, v133);
      // FromConsensus
      stdlib.assert(true);
      return ['Bob quits']; }
    else {
      const [] = txn2.data;
      const v3 = txn2.from;
      const v13 = txn2.value;
      const v14 = stdlib.eq(v13, v5);
      stdlib.assert(v14);
      // FromConsensus
      const txn3 = await ctc.recv('O', 'e5', 10, false, [v0, v3, v5, v6], 'm6', 'e6');
      if (txn3.didTimeout) {
        const v132 = txn3.balance;
        // txn.transfer(v3, v132);
        // FromConsensus
        stdlib.assert(true);
        return ['Alice quits']; }
      else {
        const [v28] = txn3.data;
        const v30 = txn3.value;
        const v31 = stdlib.eq(v30, 0);
        stdlib.assert(v31);
        // FromConsensus
        const txn4 = await ctc.recv('O', 'e7', 10, false, [v0, v3, v5, v6, v28], 'm8', 'e8');
        if (txn4.didTimeout) {
          const v131 = txn4.balance;
          // txn.transfer(v0, v131);
          // FromConsensus
          stdlib.assert(true);
          return ['Bob quits']; }
        else {
          const [v43] = txn4.data;
          const v45 = txn4.value;
          const v46 = stdlib.eq(v45, 0);
          stdlib.assert(v46);
          const v47 = stdlib.le(0, v43);
          const v48 = stdlib.lt(v43, 3);
          const v49 = v47 ? v48 : false;
          stdlib.assert(v49);
          // FromConsensus
          const txn5 = await ctc.recv('O', 'e9', 10, false, [v0, v3, v5, v6, v28, v43], 'm10', 'e10');
          if (txn5.didTimeout) {
            const v130 = txn5.balance;
            // txn.transfer(v3, v130);
            // FromConsensus
            stdlib.assert(true);
            return ['Alice quits']; }
          else {
            const [v50, v51] = txn5.data;
            const v60 = txn5.value;
            const v61 = stdlib.eq(v60, 0);
            stdlib.assert(v61);
            const v62 = stdlib.keccak256(v50, v51);
            const v63 = stdlib.eq(v28, v62);
            stdlib.assert(v63);
            const v64 = stdlib.le(0, v51);
            const v65 = stdlib.lt(v51, 3);
            const v66 = v64 ? v65 : false;
            stdlib.assert(v66);
            const v67 = stdlib.le(0, v51);
            const v68 = stdlib.lt(v51, 3);
            const v69 = v67 ? v68 : false;
            const v70 = stdlib.le(0, v43);
            const v71 = stdlib.lt(v43, 3);
            const v72 = v70 ? v71 : false;
            const v73 = v69 ? v72 : false;
            const v74 = stdlib.sub(4, v43);
            const v75 = stdlib.add(v51, v74);
            const v76 = stdlib.mod(v75, 3);
            const v77 = v72 ? 0 : 1;
            const v78 = v69 ? 2 : v77;
            const v79 = v73 ? v76 : v78;
            const v109 = stdlib.eq(v79, 2);
            const v110 = stdlib.mul(2, v5);
            const v111 = stdlib.eq(v79, 0);
            const v112 = stdlib.mul(2, v5);
            const v113 = v111 ? 0 : v5;
            const v114 = v111 ? v112 : v5;
            const v115 = v109 ? v110 : v113;
            const v116 = v109 ? 0 : v114;
            const v117 = stdlib.add(v6, v115);
            // txn.transfer(v0, v117);
            // txn.transfer(v3, v116);
            // FromConsensus
            const v118 = stdlib.isType('bool', await interact.outcome());
            const v119 = stdlib.le(0, v79);
            const v120 = stdlib.lt(v79, 5);
            const v121 = v119 ? v120 : false;
            stdlib.assert(v121);
            const v122 = stdlib.eq(v79, 0);
            const v123 = stdlib.eq(v79, 1);
            const v124 = stdlib.eq(v79, 2);
            const v125 = stdlib.eq(v79, 3);
            const v126 = v125 ? 'Alice quits' : 'Bob quits';
            const v127 = v124 ? 'Alice wins' : v126;
            const v128 = v123 ? 'Draw' : v127;
            const v129 = v122 ? 'Bob wins' : v128;
            return [v129]; } } } } } }

export const ABI = [{"inputs":[],"payable":true,"stateMutability":"payable","type":"constructor"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v5","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v6","type":"uint256"}],"name":"e1","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e10","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e2","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e3","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e4","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v28","type":"uint256"}],"name":"e5","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e6","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v43","type":"uint256"}],"name":"e7","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e8","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v50","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v51","type":"uint256"}],"name":"e9","type":"event"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"}],"name":"m1","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v0","type":"address"},{"internalType":"address payable","name":"v3","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"},{"internalType":"uint256","name":"v28","type":"uint256"},{"internalType":"uint256","name":"v43","type":"uint256"}],"name":"m10","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"}],"name":"m2","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v0","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"}],"name":"m3","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v0","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"}],"name":"m4","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v0","type":"address"},{"internalType":"address payable","name":"v3","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"},{"internalType":"uint256","name":"v28","type":"uint256"}],"name":"m5","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v0","type":"address"},{"internalType":"address payable","name":"v3","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"}],"name":"m6","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v0","type":"address"},{"internalType":"address payable","name":"v3","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"},{"internalType":"uint256","name":"v28","type":"uint256"},{"internalType":"uint256","name":"v43","type":"uint256"}],"name":"m7","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v0","type":"address"},{"internalType":"address payable","name":"v3","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"},{"internalType":"uint256","name":"v28","type":"uint256"}],"name":"m8","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v0","type":"address"},{"internalType":"address payable","name":"v3","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"},{"internalType":"uint256","name":"v28","type":"uint256"},{"internalType":"uint256","name":"v43","type":"uint256"},{"internalType":"uint256","name":"v50","type":"uint256"},{"internalType":"uint256","name":"v51","type":"uint256"}],"name":"m9","outputs":[],"payable":true,"stateMutability":"payable","type":"function"}];

export const Bytecode = "0x600060a08181524360c0526040608081905260e0815290209055610f24806100286000396000f3fe6080604052600436106100915760003560e01c80639ad83246116100595780639ad83246146101d6578063b8b8cfad1461020e578063d212999514610246578063d62511ba14610288578063e9398627146102d057610091565b80632fa0a8f81461009657806336807e78146100f45780634441778b146101115780637a52ccb31461015f57806380c418dc14610188575b600080fd5b6100f260048036036101208110156100ad57600080fd5b508035906001600160a01b03602082013581169160408101359091169060608101359060808101359060a08101359060c08101359060e0810135906101000135610318565b005b6100f26004803603602081101561010a57600080fd5b5035610586565b6100f2600480360360e081101561012757600080fd5b508035906001600160a01b03602082013581169160408101359091169060608101359060808101359060a08101359060c00135610609565b6100f26004803603606081101561017557600080fd5b508035906020810135906040013561075c565b6100f2600480360360e081101561019e57600080fd5b508035906001600160a01b03602082013581169160408101359091169060608101359060808101359060a08101359060c00135610856565b6100f2600480360360808110156101ec57600080fd5b508035906001600160a01b036020820135169060408101359060600135610961565b6100f26004803603608081101561022457600080fd5b508035906001600160a01b036020820135169060408101359060600135610a53565b6100f2600480360360a081101561025c57600080fd5b508035906001600160a01b03602082013581169160408101359091169060608101359060800135610b69565b6100f2600480360360c081101561029e57600080fd5b508035906001600160a01b03602082013581169160408101359091169060608101359060808101359060a00135610c65565b6100f2600480360360c08110156102e657600080fd5b508035906001600160a01b03602082013581169160408101359091169060608101359060808101359060a00135610d68565b6040805160076020808301919091528183018c90526001600160601b031960608c811b8216818501528b901b1660748301526088820189905260a8820188905260c8820187905260e88083018790528351808403909101815261010890920190925280519101206000541461038c57600080fd5b610394610ea1565b336001600160a01b038a16146103a957600080fd5b600a8a0143106103b857600080fd5b34156103c357600080fd5b6040805160208082018690528183018590528251808303840181526060909201909252805191012085146103f657600080fd5b6003821061040357600080fd5b6003808310808352908510602083015261041e576000610424565b80602001515b61044e57805161044657806020015161043e576001610441565b60005b610449565b60025b61045f565b60038460040383018161045d57fe5b065b60408201819052600281146060830181905290156080830181905260a0830182905260c08301526001600160a01b038a16906108fc906104b0578260c001516104a857886104ab565b60005b6104b5565b886002025b88019081150290604051600060405180830381858888f193505050501580156104e2573d6000803e3d6000fd5b50876001600160a01b03166108fc8260a00151610512578260c00151610508578861050d565b886002025b610515565b60005b6040518115909202916000818181858888f1935050505015801561053d573d6000803e3d6000fd5b5060408051303181526020810185905280820184905290517fdc10caa127b5173d41824d03032b6d39257767b242fadfb0a80ae411eafba1ad9181900360600190a16000805533ff5b60408051600060208083018290528284018590528351808403850181526060909301909352815191909201209054146105be57600080fd5b600a81014310156105ce57600080fd5b604080513031815290517ff04f5fc87a72102f7c0b228f8bbaf9b9aa7a2b5dc295c86538fdde91e95866e99181900360200190a16000805533ff5b6040805160056020808301919091528183018a90526001600160601b031960608a811b82168185015289901b1660748301526088820187905260a8820186905260c88083018690528351808403909101815260e890920190925280519101206000541461067557600080fd5b336001600160a01b0386161461068a57600080fd5b600a8701431061069957600080fd5b34156106a457600080fd5b600381106106b157600080fd5b60408051303181526020810183905281517f84a4de792fe96931a41b41c859e5dd7d42b5fe18149d8fc7510ee5574df8c239929181900390910190a160408051600760208083019190915243828401526001600160601b03196060998a1b81168a8401529790981b9096166074870152608886019490945260a885019290925260c884015260e880840191909152815180840390910181526101089092019052805191012060005550565b604080516000602080830182905282840187905283518084038501815260609093019093528151919092012090541461079457600080fd5b61079c610edd565b338152600a840143106107ae57600080fd5b81830134146107bc57600080fd5b60408051303181526020810185905280820184905290517f219cc811755104876269c7553666684eaaeecb90b6a7ffc6fdd5068140059b8e9181900360600190a15160408051600160208083019190915243828401526001600160601b0319606094851b169382019390935260748101949094526094808501939093528051808503909301835260b4909301909252805191012060005550565b6040805160076020808301919091528183018a90526001600160601b031960608a811b82168185015289901b1660748301526088820187905260a8820186905260c8820185905260e8808301859052835180840390910181526101089092019092528051910120600054146108ca57600080fd5b336001600160a01b038616146108df57600080fd5b600a87014310156108ef57600080fd5b6040516001600160a01b03861690303180156108fc02916000818181858888f19350505050158015610925573d6000803e3d6000fd5b50604080513031815290517f9bf9cf9ae88051b33b19923b1c1cf36013b840c9975de29305d444b55d83c6bd9181900360200190a16000805533ff5b604080516001602080830191909152818301879052606086811b6001600160601b031916908301526074820185905260948083018590528351808403909101815260b49092019092528051910120600054146109bc57600080fd5b336001600160a01b038416146109d157600080fd5b600a84014310156109e157600080fd5b6040516001600160a01b03841690303180156108fc02916000818181858888f19350505050158015610a17573d6000803e3d6000fd5b50604080513031815290517f6ce5b12953112c528c3a24a99350a573e6cb61c4e39d70f2392cc6bd7266f9699181900360200190a16000805533ff5b604080516001602080830191909152818301879052606086811b6001600160601b031916908301526074820185905260948083018590528351808403909101815260b4909201909252805191012060005414610aae57600080fd5b610ab6610edd565b338152600a85014310610ac857600080fd5b823414610ad457600080fd5b604080513031815290517f6fbec89a9bad4c7daaf5b053ac2c5ad4e0ff33c287295fe9a98cf7f3a3043f9c9181900360200190a15160408051600360208083019190915243828401526001600160601b0319606097881b8116888401529390961b9092166074830152608882019390935260a8808201929092528251808203909201825260c801909152805191012060005550565b6040805160036020808301919091528183018890526001600160601b0319606088811b82168185015287901b1660748301526088820185905260a88083018590528351808403909101815260c8909201909252805191012060005414610bce57600080fd5b336001600160a01b03841614610be357600080fd5b600a8501431015610bf357600080fd5b6040516001600160a01b03841690303180156108fc02916000818181858888f19350505050158015610c29573d6000803e3d6000fd5b50604080513031815290517fcb3347bd475fd43f41b4bc5bb011db952f2079e6ba9a82ff211988cd7871dba69181900360200190a16000805533ff5b6040805160056020808301919091528183018990526001600160601b0319606089811b82168185015288901b1660748301526088820186905260a8820185905260c88083018590528351808403909101815260e8909201909252805191012060005414610cd157600080fd5b336001600160a01b03861614610ce657600080fd5b600a8601431015610cf657600080fd5b6040516001600160a01b03861690303180156108fc02916000818181858888f19350505050158015610d2c573d6000803e3d6000fd5b50604080513031815290517f3a6f8023909a26b76d462631fcdf570dbe3740447548e09470d1ad04394a0cec9181900360200190a16000805533ff5b6040805160036020808301919091528183018990526001600160601b0319606089811b82168185015288901b1660748301526088820186905260a88083018690528351808403909101815260c8909201909252805191012060005414610dcd57600080fd5b336001600160a01b03861614610de257600080fd5b600a86014310610df157600080fd5b3415610dfc57600080fd5b60408051303181526020810183905281517fabf482d77b67111a4971bb96fe81961f83ba459eb1d8fa9f78b6908251aeef1a929181900390910190a160408051600560208083019190915243828401526001600160601b0319606098891b8116898401529690971b9095166074860152608885019390935260a884019190915260c8808401919091528151808403909101815260e89092019052805191012060005550565b6040805160e081018252600080825260208201819052918101829052606081018290526080810182905260a0810182905260c081019190915290565b6040805160208101909152600081529056fea265627a7a723158204543d1dbf2d85eafa49b74db46e2b99df06416249a6181dcc348e8466f5bbee064736f6c634300050c0032";

export const Bytecode2 = "0x60006000524360205260006040206000556074601d600039600080f3fe60b556fe6000805533fffe600080fdfe6000357f16a4dfded192ad07121c01687bb66286bda0bc04d6fe54eb1bd7a343d882c5ae143660e4141619600b5760c06024600037600760c05260045160e05260005160005260205160205260405160405260605160605260805160805260a05160a05260c0610001206000541419600b57602051331419600b57600435600a014310600b577f4fb729477bb97a7b242f41cca99fb9f0cddcef1522a61b2807e38a7239d9f15e600060c0a13160c052600080808060c05160205161fc08f119600b57600456fe6000357f8a09fab5e3df44a91e1bb2b45c7a75dcaffbb10c3338ac555a374046be46896314366124011416196010576100016024600037600760005260045160205260005160405260205160605260405160805260605160a05260805160c05260a05160e052610001610001206000541419600b57600051331419600b57600435600a01431019600b577f976627f95e13b8b39ec2aecfbc0a3d25f169ae85aab90e1b34287495011640ee604060c0a13460005260006000511460205260205119600b5760c05160405260e0516060526140016040206040526040516080511460605260605119600b5760e05160001119608052600360e0511060a052600060a051608051602b575060e35660c05260c05119600b5760e0516000111960e052600360e05110600052600060005160e05160265750600f5660205260a05160001119604052600360a051106060526000606051604051602157506034566080526000608051602051601c575060465660a05260a05160040360c05260c05160e0510160e052600360e05106600052600160006080516017575060735660205260205160026020516012575060855660405260405160005160a051600d57506098566060526060516000111960805260056060511060a052600060a0516080516008575060bd5660c05260026060511460e05260e05160001119600052600360e0511060205260006020516000516003575060eb566040526001600060e05160fe575060fc56606052604051600160605160f95750600e5660805260006060511460a05260a0516000111960c052600360a0511060e052600060e05160c05160f45750603c566000526001600060a05160ef5750604d56602052600051600160205160ea5750605f56604052600260605114606052600060e05114608052600060605160805160e5575060835660a052600160e0511460c052600060605160c05160e05750609e5660e052600260e05114600052600060605160005160db575060b956602052600060605114604052600060a05114606052600060405160605160d6575060dd56608052600160a0511460a052600060405160a05160d1575060f85660c052600260a0511460e052600060405160e05160cc5750601356600052600260605114602052604051600202604052600060605114606052604051600202608052604051600060605160c7575060495660a05260405160805160605160c25750605c5660c05260a05160405160205160bd5750606f5660e05260c051600060205160b8575060815660005260e05160605101602052600080808060205160005161fc08f119600b57600080808060005160205161fc08f119600b57600456fe90506081569050606f569050605c5690506049569050601356905060f856905060dd56905060b9569050609e5690506083569050605f569050604d569050603c569050600e56905060fc56905060eb56905060bd56905060985690506085569050607356905060465690506034569050600f56905060e3566000357f0b7057108cfa9e09322ae12f535bfa7af04232703c9aa453821df908aea08aef143660c414161960d75760a06024600037600560a05260045160c05260005160e05260205160005260405160205260605160405260805160605260a060e0206000541419600b57600051331419600b57600435600a014310600b577f4e58f06d9372c6ab5437d6ee8473425355a8189704fd7342b3e5b6b93ad4a3ff600060a0a13160a052600080808060a05160005161fc08f119600b57600456fe6000357f87308c4329e4f4b0454d89a66b71fbbd4f3660d57564c118483e4538c854efd3143660e414161960305760c06024600037600560c05260045160e05260005160005260205160205260405160405260605160605260805160805260c060e0206000541419600b57602051331419600b57600435600a01431019600b577f7d2405503d669e8e0d42b2b1731237fb0dc5c4ccc14133a0f8f17056db60ae07602060a0a13460c052600060c0511460e05260e05119600b5760a05160001119600052600360a051106020526000602051600051600e575060cc5660405260405119600b5760076060524360805260005160a05260205160c05260405160e05260605160005260805160205260a05160405261600161000120600055fe905060cc566000357f106b65c245cff1c26c3b6ef84e4c86096b75009c7546c40b3b4f7a0f680ec8ed143660a414161960f05760806024600037600360805260045160a05260005160c05260205160e052604051600052606051602052608060c0206000541419600b57602051331419600b57600435600a014310600b577fb62fd2e0bd0fa0a08db19ecbed8856c1526909b8e2fc34b339497b67794469d660006080a131608052600080808060805160205161fc08f119600b57600456fe6000357ff259efff518ff3a445d1a7d72f1302a608ed4ecac9afeaa7350fb9e7ffca5187143660c414161960135760a06024600037600360a05260045160c05260005160e05260205160005260405160205260605160405260a060c0206000541419600b57600051331419600b57600435600a01431019600b577f97bfc1a0968eb62b4ee0627106b54e4c8d3f7374e164fe1921fc823f3dc795f560206080a13460a052600060a0511460c05260c05119600b57600560e0524360005260005160205260205160405260405160605260605160805260805160a05260e060e020600055fe6000357f1907b037b96c2ceead37fd2d5c133a6b0c7b83164efe608e1c7254aee95b70e81436608414161960cd5760606024600037600160605260045160805260005160a05260205160c05260405160e052606060a0206000541419600b57600051331419600b57600435600a014310600b577f11362c0f348f1cdb17fd45407c8beb58b91350df448e9727e20e528654f0e57960006060a131606052600080808060605160005161fc08f119600b57600456fe6000357f32491628d3bba978032ce132c2ea933bbd107bd519e0c986319917461b25c4281436608414161960b15760606024600037600160605260045160805260005160a05260205160c05260405160e052606060a0206000541419600b57600435600a01431019600b577fc1b06148c88b51c02918bc49436929a610dbdb7193fc44b856245160520b805660006060a1346060526020516060511460805260805119600b57600360a0524360c05260005160e0523360005260205160205260405160405260a060c020600055fe6000357fbac6ebd41b106f55381a0967bc00cee5537249826fdcc0de37de29e6ca69aa911436602414161960655760006024600037600060005260045160205260006040206000541419600b57600435600a014310600b577f354083e34305e43504f607a379840b90a0c97b23b093bfbf711fd18fe8f9319b60006000a1600456fe6000357e02386cf5fba9a8d2a4bc437ca6b821b100edc28a40ac9b2c578dfd80990d201436606414161960335760406024600037600060405260045160605260406040206000541419600b57600435600a01431019600b577fb0faad912ce7b8154a3a2b1aac5debe799c670f51814db7da1c5b85bb3da9e4460406000a134604052602051600051016060526060516040511460805260805119600b57600160a0524360c0523360e05260005160005260205160205260a060a020600055fe";