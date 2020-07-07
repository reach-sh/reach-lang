// Automatically generated with Reach 0.1.0

// import * as stdlib from '@reach-sh/stdlib';

export async function A(stdlib, ctc, interact) {
  const txn0 = { balance: 0, value: 0 };
  const v2 = stdlib.isType('uint256', await interact.getWagerAmount());
  const v3 = v2;
  const v4 = stdlib.isType('uint256', await interact.getInitialHeap());
  const v5 = v4;
  const v6 = stdlib.random_uint256();
  const v7 = stdlib.keccak256(v6);
  const v8 = v7;
  const txn1 = await ctc.sendrecv('A', 1, 3, [v3, v5, v8], v3, false, async (txn_out, txn1) => {
    const v9 = txn1.value;
    const v10 = stdlib.eq(v9, v3);
    stdlib.assert(v10);
    return true; });
  const v0 = txn1.from;
  const v9 = txn1.value;
  const v10 = stdlib.eq(v9, v3);
  stdlib.assert(v10);
  const txn2 = await ctc.recv('A', 2, 1, 10);
  if (txn2.didTimeout) {
    const txn3 = await ctc.sendrecv('A', 10, 0, [v0, v3, v5, v8], 0, false, async (txn_out, txn3) => {
      const v82 = txn3.value;
      const v83 = stdlib.eq(v82, 0);
      stdlib.assert(v83);
      const v84 = txn3.balance;
      txn_out.transfer(v0, v84);
      return true; });
    const v82 = txn3.value;
    const v83 = stdlib.eq(v82, 0);
    stdlib.assert(v83);
    return ['B never accepted']; }
  else {
    const [v13] = txn2.data;
    const v1 = txn2.from;
    const v14 = txn2.value;
    const v15 = stdlib.eq(v14, v3);
    stdlib.assert(v15);
    const v16 = v6;
    const txn3 = await ctc.sendrecv('A', 3, 1, [v0, v1, v3, v5, v8, v13, v16], 0, 10, async (txn_out, txn3) => {
      const v17 = txn3.value;
      const v18 = stdlib.eq(v17, 0);
      stdlib.assert(v18);
      const v19 = stdlib.keccak256(v16);
      const v20 = stdlib.eq(v8, v19);
      stdlib.assert(v20);
      const v21 = stdlib.add(v16, v13);
      const v22 = stdlib.mod(v21, 2);
      const v23 = stdlib.eq(v22, 0);
      let v24 = v23;
      let v25 = v5;
      let v26 = v5;
      while (true) {
        const v27 = stdlib.add(v25, v26);
        const v28 = stdlib.gt(v27, 0);
        const v29 = v28 ? false : true;
        if (v29) {
          const v73 = v24 ? 2 : 0;
          const v74 = v24 ? 0 : 2;
          const v75 = stdlib.mul(v73, v3);
          txn_out.transfer(v0, v75);
          const v76 = stdlib.mul(v74, v3);
          txn_out.transfer(v1, v76);
          return true; }
        else {
          if (v24) {
            return true; }
          else {
            return true; } } } });
    if (txn3.didTimeout) {
      const txn4 = await ctc.recv('A', 9, 0, false);
      const [] = txn4.data;
      const v79 = txn4.value;
      const v80 = stdlib.eq(v79, 0);
      stdlib.assert(v80);
      return ['A never revealed coinflip']; }
    else {
      const v17 = txn3.value;
      const v18 = stdlib.eq(v17, 0);
      stdlib.assert(v18);
      const v19 = stdlib.keccak256(v16);
      const v20 = stdlib.eq(v8, v19);
      stdlib.assert(v20);
      const v21 = stdlib.add(v16, v13);
      const v22 = stdlib.mod(v21, 2);
      const v23 = stdlib.eq(v22, 0);
      let v24 = v23;
      let v25 = v5;
      let v26 = v5;
      while (true) {
        const v27 = stdlib.add(v25, v26);
        const v28 = stdlib.gt(v27, 0);
        const v29 = v28 ? false : true;
        if (v29) {
          const v77 = v24 ? 'A won' : 'B won';
          const v78 = stdlib.isType('bool', await interact.showOutcome(v77));
          return ['Game is over']; }
        else {
          if (v24) {
            const v33 = stdlib.isType('bool', await interact.getHeap(v25, v26));
            const v34 = v33;
            const v35 = stdlib.isType('uint256', await interact.getAmount(v25, v26));
            const v36 = v35;
            const v37 = v34 ? v25 : v26;
            const v38 = stdlib.le(v36, v37);
            stdlib.assert(v38);
            const txn4 = await ctc.sendrecv('A', 5, 2, [v0, v1, v3, v24, v25, v26, v34, v36], 0, 10, async (txn_out, txn4) => {
              const v39 = txn4.value;
              const v40 = stdlib.eq(v39, 0);
              stdlib.assert(v40);
              const v41 = v34 ? v25 : v26;
              const v42 = stdlib.le(v36, v41);
              stdlib.assert(v42);
              return true; });
            if (txn4.didTimeout) {
              const txn5 = await ctc.recv('A', 6, 0, false);
              const [] = txn5.data;
              const v50 = txn5.value;
              const v51 = stdlib.eq(v50, 0);
              stdlib.assert(v51);
              return ['A timed out move']; }
            else {
              const v39 = txn4.value;
              const v40 = stdlib.eq(v39, 0);
              stdlib.assert(v40);
              const v41 = v34 ? v25 : v26;
              const v42 = stdlib.le(v36, v41);
              stdlib.assert(v42);
              const v43 = v24 ? false : true;
              const v44 = stdlib.sub(v25, v36);
              const v45 = v24 ? false : true;
              const v46 = stdlib.sub(v26, v36);
              const v47 = v34 ? v43 : v45;
              const v48 = v34 ? v44 : v25;
              const v49 = v34 ? v26 : v46;
              v24 = v47;
              v25 = v48;
              v26 = v49;
              continue; } }
          else {
            const txn4 = await ctc.recv('A', 7, 2, 10);
            if (txn4.didTimeout) {
              const txn5 = await ctc.sendrecv('A', 8, 0, [v0, v1, v3, v24, v25, v26], 0, false, async (txn_out, txn5) => {
                const v70 = txn5.value;
                const v71 = stdlib.eq(v70, 0);
                stdlib.assert(v71);
                const v72 = txn5.balance;
                txn_out.transfer(v0, v72);
                return true; });
              const v70 = txn5.value;
              const v71 = stdlib.eq(v70, 0);
              stdlib.assert(v71);
              return ['B timed out move']; }
            else {
              const [v54, v56] = txn4.data;
              const v59 = txn4.value;
              const v60 = stdlib.eq(v59, 0);
              stdlib.assert(v60);
              const v61 = v54 ? v25 : v26;
              const v62 = stdlib.le(v56, v61);
              stdlib.assert(v62);
              const v63 = v24 ? false : true;
              const v64 = stdlib.sub(v25, v56);
              const v65 = v24 ? false : true;
              const v66 = stdlib.sub(v26, v56);
              const v67 = v54 ? v63 : v65;
              const v68 = v54 ? v64 : v25;
              const v69 = v54 ? v26 : v66;
              v24 = v67;
              v25 = v68;
              v26 = v69;
              continue; } } } } } } }

export async function B(stdlib, ctc, interact) {
  const txn0 = { balance: 0, value: 0 };
  const txn1 = await ctc.recv('B', 1, 3, false);
  const [v3, v5, v8] = txn1.data;
  const v0 = txn1.from;
  const v9 = txn1.value;
  const v10 = stdlib.eq(v9, v3);
  stdlib.assert(v10);
  const v11 = stdlib.isType('bool', await interact.acceptWager(v3, v5));
  const v12 = stdlib.random_uint256();
  const v13 = v12;
  const txn2 = await ctc.sendrecv('B', 2, 1, [v0, v3, v5, v8, v13], v3, 10, async (txn_out, txn2) => {
    const v14 = txn2.value;
    const v15 = stdlib.eq(v14, v3);
    stdlib.assert(v15);
    return true; });
  if (txn2.didTimeout) {
    const txn3 = await ctc.recv('B', 10, 0, false);
    const [] = txn3.data;
    const v82 = txn3.value;
    const v83 = stdlib.eq(v82, 0);
    stdlib.assert(v83);
    return ['B never accepted']; }
  else {
    const v1 = txn2.from;
    const v14 = txn2.value;
    const v15 = stdlib.eq(v14, v3);
    stdlib.assert(v15);
    const txn3 = await ctc.recv('B', 3, 1, 10);
    if (txn3.didTimeout) {
      const txn4 = await ctc.sendrecv('B', 9, 0, [v0, v1, v3, v5, v8, v13], 0, false, async (txn_out, txn4) => {
        const v79 = txn4.value;
        const v80 = stdlib.eq(v79, 0);
        stdlib.assert(v80);
        const v81 = txn4.balance;
        txn_out.transfer(v1, v81);
        return true; });
      const v79 = txn4.value;
      const v80 = stdlib.eq(v79, 0);
      stdlib.assert(v80);
      return ['A never revealed coinflip']; }
    else {
      const [v16] = txn3.data;
      const v17 = txn3.value;
      const v18 = stdlib.eq(v17, 0);
      stdlib.assert(v18);
      const v19 = stdlib.keccak256(v16);
      const v20 = stdlib.eq(v8, v19);
      stdlib.assert(v20);
      const v21 = stdlib.add(v16, v13);
      const v22 = stdlib.mod(v21, 2);
      const v23 = stdlib.eq(v22, 0);
      let v24 = v23;
      let v25 = v5;
      let v26 = v5;
      while (true) {
        const v27 = stdlib.add(v25, v26);
        const v28 = stdlib.gt(v27, 0);
        const v29 = v28 ? false : true;
        if (v29) {
          const v77 = v24 ? 'A won' : 'B won';
          const v78 = stdlib.isType('bool', await interact.showOutcome(v77));
          return ['Game is over']; }
        else {
          if (v24) {
            const txn4 = await ctc.recv('B', 5, 2, 10);
            if (txn4.didTimeout) {
              const txn5 = await ctc.sendrecv('B', 6, 0, [v0, v1, v3, v24, v25, v26], 0, false, async (txn_out, txn5) => {
                const v50 = txn5.value;
                const v51 = stdlib.eq(v50, 0);
                stdlib.assert(v51);
                const v52 = txn5.balance;
                txn_out.transfer(v1, v52);
                return true; });
              const v50 = txn5.value;
              const v51 = stdlib.eq(v50, 0);
              stdlib.assert(v51);
              return ['A timed out move']; }
            else {
              const [v34, v36] = txn4.data;
              const v39 = txn4.value;
              const v40 = stdlib.eq(v39, 0);
              stdlib.assert(v40);
              const v41 = v34 ? v25 : v26;
              const v42 = stdlib.le(v36, v41);
              stdlib.assert(v42);
              const v43 = v24 ? false : true;
              const v44 = stdlib.sub(v25, v36);
              const v45 = v24 ? false : true;
              const v46 = stdlib.sub(v26, v36);
              const v47 = v34 ? v43 : v45;
              const v48 = v34 ? v44 : v25;
              const v49 = v34 ? v26 : v46;
              v24 = v47;
              v25 = v48;
              v26 = v49;
              continue; } }
          else {
            const v53 = stdlib.isType('bool', await interact.getHeap(v25, v26));
            const v54 = v53;
            const v55 = stdlib.isType('uint256', await interact.getAmount(v25, v26));
            const v56 = v55;
            const v57 = v54 ? v25 : v26;
            const v58 = stdlib.le(v56, v57);
            stdlib.assert(v58);
            const txn4 = await ctc.sendrecv('B', 7, 2, [v0, v1, v3, v24, v25, v26, v54, v56], 0, 10, async (txn_out, txn4) => {
              const v59 = txn4.value;
              const v60 = stdlib.eq(v59, 0);
              stdlib.assert(v60);
              const v61 = v54 ? v25 : v26;
              const v62 = stdlib.le(v56, v61);
              stdlib.assert(v62);
              return true; });
            if (txn4.didTimeout) {
              const txn5 = await ctc.recv('B', 8, 0, false);
              const [] = txn5.data;
              const v70 = txn5.value;
              const v71 = stdlib.eq(v70, 0);
              stdlib.assert(v71);
              return ['B timed out move']; }
            else {
              const v59 = txn4.value;
              const v60 = stdlib.eq(v59, 0);
              stdlib.assert(v60);
              const v61 = v54 ? v25 : v26;
              const v62 = stdlib.le(v56, v61);
              stdlib.assert(v62);
              const v63 = v24 ? false : true;
              const v64 = stdlib.sub(v25, v56);
              const v65 = v24 ? false : true;
              const v66 = stdlib.sub(v26, v56);
              const v67 = v54 ? v63 : v65;
              const v68 = v54 ? v64 : v25;
              const v69 = v54 ? v26 : v66;
              v24 = v67;
              v25 = v68;
              v26 = v69;
              continue; } } } } } } }

export const ETH = {
  ABI: [{"inputs":[],"payable":true,"stateMutability":"payable","type":"constructor"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v3","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v5","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v8","type":"uint256"}],"name":"e1","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e10","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v13","type":"uint256"}],"name":"e2","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v16","type":"uint256"}],"name":"e3","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"},{"indexed":false,"internalType":"bool","name":"v34","type":"bool"},{"indexed":false,"internalType":"uint256","name":"v36","type":"uint256"}],"name":"e5","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e6","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"},{"indexed":false,"internalType":"bool","name":"v54","type":"bool"},{"indexed":false,"internalType":"uint256","name":"v56","type":"uint256"}],"name":"e7","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e8","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e9","type":"event"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"uint256","name":"v3","type":"uint256"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v8","type":"uint256"}],"name":"m1","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v0","type":"address"},{"internalType":"uint256","name":"v3","type":"uint256"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v8","type":"uint256"}],"name":"m10","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v0","type":"address"},{"internalType":"uint256","name":"v3","type":"uint256"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v8","type":"uint256"},{"internalType":"uint256","name":"v13","type":"uint256"}],"name":"m2","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v0","type":"address"},{"internalType":"address payable","name":"v1","type":"address"},{"internalType":"uint256","name":"v3","type":"uint256"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v8","type":"uint256"},{"internalType":"uint256","name":"v13","type":"uint256"},{"internalType":"uint256","name":"v16","type":"uint256"}],"name":"m3","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v0","type":"address"},{"internalType":"address payable","name":"v1","type":"address"},{"internalType":"uint256","name":"v3","type":"uint256"},{"internalType":"bool","name":"v24","type":"bool"},{"internalType":"uint256","name":"v25","type":"uint256"},{"internalType":"uint256","name":"v26","type":"uint256"},{"internalType":"bool","name":"v34","type":"bool"},{"internalType":"uint256","name":"v36","type":"uint256"}],"name":"m5","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v0","type":"address"},{"internalType":"address payable","name":"v1","type":"address"},{"internalType":"uint256","name":"v3","type":"uint256"},{"internalType":"bool","name":"v24","type":"bool"},{"internalType":"uint256","name":"v25","type":"uint256"},{"internalType":"uint256","name":"v26","type":"uint256"}],"name":"m6","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v0","type":"address"},{"internalType":"address payable","name":"v1","type":"address"},{"internalType":"uint256","name":"v3","type":"uint256"},{"internalType":"bool","name":"v24","type":"bool"},{"internalType":"uint256","name":"v25","type":"uint256"},{"internalType":"uint256","name":"v26","type":"uint256"},{"internalType":"bool","name":"v54","type":"bool"},{"internalType":"uint256","name":"v56","type":"uint256"}],"name":"m7","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v0","type":"address"},{"internalType":"address payable","name":"v1","type":"address"},{"internalType":"uint256","name":"v3","type":"uint256"},{"internalType":"bool","name":"v24","type":"bool"},{"internalType":"uint256","name":"v25","type":"uint256"},{"internalType":"uint256","name":"v26","type":"uint256"}],"name":"m8","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v0","type":"address"},{"internalType":"address payable","name":"v1","type":"address"},{"internalType":"uint256","name":"v3","type":"uint256"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v8","type":"uint256"},{"internalType":"uint256","name":"v13","type":"uint256"}],"name":"m9","outputs":[],"payable":true,"stateMutability":"payable","type":"function"}]
  ,
  
  Bytecode: "0x600060a08181524360c0526040608081905260e0815290209055610f69806100286000396000f3fe6080604052600436106100865760003560e01c806393dbbf9f1161005957806393dbbf9f146101e05780639bea8f2d1461021e578063b51009281461027e578063b7ae93d9146102c2578063cb8f8a5d146102f157610086565b80630dc8e07e1461008b57806318d0d7e2146100dd5780633f4673691461013d57806357ceefcc14610192575b600080fd5b6100db600480360360e08110156100a157600080fd5b508035906001600160a01b036020820135811691604081013590911690606081013590608081013515159060a08101359060c00135610341565b005b6100db60048036036101208110156100f457600080fd5b508035906001600160a01b036020820135811691604081013590911690606081013590608081013515159060a08101359060c08101359060e08101351515906101000135610464565b6100db600480360361010081101561015457600080fd5b508035906001600160a01b03602082013581169160408101359091169060608101359060808101359060a08101359060c08101359060e001356105c1565b6100db600480360360e08110156101a857600080fd5b508035906001600160a01b03602082013581169160408101359091169060608101359060808101359060a08101359060c001356106e9565b6100db600480360360a08110156101f657600080fd5b508035906001600160a01b036020820135169060408101359060608101359060800135610809565b6100db600480360361012081101561023557600080fd5b508035906001600160a01b036020820135811691604081013590911690606081013590608081013515159060a08101359060c08101359060e08101351515906101000135610917565b6100db600480360360c081101561029457600080fd5b508035906001600160a01b036020820135169060408101359060608101359060808101359060a00135610a2f565b6100db600480360360808110156102d857600080fd5b5080359060208101359060408101359060600135610b67565b6100db600480360360e081101561030757600080fd5b508035906001600160a01b036020820135811691604081013590911690606081013590608081013515159060a08101359060c00135610c5f565b6040805160036020808301919091528183018a90526001600160601b031960608a811b82168185015289901b1660748301526088820187905285151560f81b60a883015260a9820185905260c98083018590528351808403909101815260e99092019092528051910120600054146103b857600080fd5b336001600160a01b038616146103cd57600080fd5b600a870143101580156103de575060015b6103e757600080fd5b34156103f257600080fd5b6040516001600160a01b03861690303180156108fc02916000818181858888f19350505050158015610428573d6000803e3d6000fd5b50604080513031815290517fcb3347bd475fd43f41b4bc5bb011db952f2079e6ba9a82ff211988cd7871dba69181900360200190a16000805533ff5b6040805160036020808301919091528183018c90526001600160601b031960608c811b8216818501528b901b1660748301526088820189905287151560f81b60a883015260a9820187905260c98083018790528351808403909101815260e99092019092528051910120600054146104db57600080fd5b336001600160a01b038916146104f057600080fd5b600a890143106104ff57600080fd5b341561050a57600080fd5b816105155782610517565b835b81111561052357600080fd5b6040805130318152831515602082015280820183905290517f6d4e7c3d262b2a746e1b7e77efbace5a4cb85c853e38b68e16a6c022f17b51b99181900360600190a16105b688888885610584578861057c57600161057f565b60005b610593565b88610590576001610593565b60005b8661059e57886105a2565b8589035b876105af578689036105b1565b885b610d82565b505050505050505050565b6040805160026020808301919091528183018b90526001600160601b031960608b811b8216818501528a901b1660748301526088820188905260a8820187905260c8820186905260e88083018690528351808403909101815261010890920190925280519101206000541461063557600080fd5b336001600160a01b0388161461064a57600080fd5b600a8801431061065957600080fd5b341561066457600080fd5b6040805160208082018490528251808303820181529183019092528051910120831461068f57600080fd5b60408051303181526020810183905281517f94dd7e08991b8945fde2d5865f7071e72045b9800e293ff60d29c6960c5a4fb5929181900390910190a16106df878787600186860116158880610d82565b5050505050505050565b6040805160026020808301919091528183018a90526001600160601b031960608a811b82168185015289901b1660748301526088820187905260a8820186905260c8820185905260e88083018590528351808403909101815261010890920190925280519101206000541461075d57600080fd5b336001600160a01b0386161461077257600080fd5b600a87014310158015610783575060015b61078c57600080fd5b341561079757600080fd5b6040516001600160a01b03861690303180156108fc02916000818181858888f193505050501580156107cd573d6000803e3d6000fd5b50604080513031815290517fc92018b4e91e597d736654f7b1d2ec034c5fec5920e2cfe22e15b4ddcdf5e18a9181900360200190a16000805533ff5b604080516001602080830191909152818301889052606087811b6001600160601b03191690830152607482018690526094820185905260b48083018590528351808403909101815260d490920190925280519101206000541461086b57600080fd5b336001600160a01b0385161461088057600080fd5b600a85014310158015610891575060015b61089a57600080fd5b34156108a557600080fd5b6040516001600160a01b03851690303180156108fc02916000818181858888f193505050501580156108db573d6000803e3d6000fd5b50604080513031815290517f9bf9cf9ae88051b33b19923b1c1cf36013b840c9975de29305d444b55d83c6bd9181900360200190a16000805533ff5b6040805160036020808301919091528183018c90526001600160601b031960608c811b8216818501528b901b1660748301526088820189905287151560f81b60a883015260a9820187905260c98083018790528351808403909101815260e990920190925280519101206000541461098e57600080fd5b336001600160a01b038816146109a357600080fd5b600a890143106109b257600080fd5b34156109bd57600080fd5b816109c857826109ca565b835b8111156109d657600080fd5b6040805130318152831515602082015280820183905290517fdcb3c37d0f99fee1f0191abad1d9650cf8b411d24cf24119df934a5949cb15389181900360600190a16105b688888885610584578861057c57600161057f565b604080516001602080830191909152818301899052606088811b6001600160601b03191690830152607482018790526094820186905260b48083018690528351808403909101815260d4909201909252805191012060005414610a9157600080fd5b610a99610f22565b338152600a87014310610aab57600080fd5b843414610ab757600080fd5b60408051303181526020810184905281517f3a074aaea2d51955da8b38c6383720deafa2c498a47a8b8312010f91a80d49d3929181900390910190a15160408051600260208083019190915243828401526001600160601b03196060998a1b81168a8401529390981b9092166074830152608882019590955260a881019390935260c883019190915260e88083019190915282518083039091018152610108909101909152805191012060005550565b6040805160006020808301829052828401889052835180840385018152606090930190935281519190920120905414610b9f57600080fd5b610ba7610f22565b338152833414610bb657600080fd5b6040805130318152602081018690528082018590526060810184905290517fae151f568caae238ebfd8aad9de993842a2ed108505e0de35098d02f263f5f2a9181900360800190a15160408051600160208083019190915243828401526001600160601b0319606094851b16938201939093526074810195909552609485019390935260b4808501929092528251808503909201825260d4909301909152805191012060005550565b6040805160036020808301919091528183018a90526001600160601b031960608a811b82168185015289901b1660748301526088820187905285151560f81b60a883015260a9820185905260c98083018590528351808403909101815260e9909201909252805191012060005414610cd657600080fd5b336001600160a01b03871614610ceb57600080fd5b600a87014310158015610cfc575060015b610d0557600080fd5b3415610d1057600080fd5b6040516001600160a01b03871690303180156108fc02916000818181858888f19350505050158015610d46573d6000803e3d6000fd5b50604080513031815290517f3a6f8023909a26b76d462631fcdf570dbe3740447548e09470d1ad04394a0cec9181900360200190a16000805533ff5b600081830111610d93576001610d96565b60005b15610e3657856001600160a01b03166108fc8585610db5576000610db8565b60025b029081150290604051600060405180830381858888f19350505050158015610de4573d6000803e3d6000fd5b50846001600160a01b03166108fc8585610dff576002610e02565b60005b029081150290604051600060405180830381858888f19350505050158015610e2e573d6000803e3d6000fd5b506000805533ff5b8215610ead5760408051600360208083019190915243828401526001600160601b031960608a811b82168185015289901b1660748301526088820187905285151560f81b60a883015260a9820185905260c98083018590528351808403909101815260e99092019092528051910120600055610f1a565b60408051600360208083019190915243828401526001600160601b031960608a811b82168185015289901b1660748301526088820187905285151560f81b60a883015260a9820185905260c98083018590528351808403909101815260e990920190925280519101206000555b505050505050565b6040805160208101909152600081529056fea265627a7a72315820b897dd0f456ed49085d1e9231c974d7e50f583ec9e22ddd1aa0985d2def6166c64736f6c634300050c0032"
  ,
  
  Bytecode2: "0x600060005243602052600060402060005560e5601d600039600080f3fe602b56fe6000805533fffe600080fdfe60a0516080510160c052600060c0511160e0526001600060e05160245750603156600052600051607b5760605160ea57600360a0524360c05260005160e05260205160005260405160205260605160405260805160605260a05160805261a0016100012060005560e756fe6000600260605160e257506089566020526002600060605160dd5750609a5660405260405160205102606052600080808060605160005161fc08f119600b5760405160405102608052600080808060805160205161fc08f119600b57600456607a569050609a569050608956607a56600360a0524360c05260005160e05260205160005260405160205260605160405260805160605260a05160805261a0016100012060005560e75690506031566000357ff12a76d400162bc0aae1aa10dfb41ef54f650ea72fe02a03b0c0263d6b5e3c97143660a4141619600b5760806024600037600160805260045160a05260005160c05260205160e052604051600052606051602052608060c0206000541419600b57600051331419600b5760043501431019600b577f4fb729477bb97a7b242f41cca99fb9f0cddcef1522a61b2807e38a7239d9f15e60006080a13460805260006080511460a05260a05119600b573160c052600080808060c05160005161fc08f119600b57600456fe6000357f048f30bff33e70fdc04f2141dc86b60c51237de9b6bb2688a1d0a5f795f727e8143660e414161960295760c06024600037600260c05260045160e05260005160005260205160205260405160405260605160605260805160805260a05160a05260c0610001206000541419600b57602051331419600b5760043501431019600b577f4974e0511e1142aba56ace19fb538cc1d2ed6b16c2fe388f2ec8b2dbc409a55b600060c0a13460c052600060c0511460e05260e05119600b5731600052600080808060005160205161fc08f119600b57600456fe6000357f877710d946994d7419166cbd9adeb5e0f403861ac8bc1e444541a0324540e4db143660e414161960f65760c06024600037600360c05260045160e05260005160005260205160205260405160405260605160605260805160805260a05160a05260c0610001206000541419600b57600051331419600b5760043501431019600b577f4e58f06d9372c6ab5437d6ee8473425355a8189704fd7342b3e5b6b93ad4a3ff600060c0a13460c052600060c0511460e05260e05119600b5731600052600080808060005160005161fc08f119600b57600456fe6000357f33cc74160e0ebf9b717d3410fdea59b9742b9a16dd745897ef47245f6b8e682d143661240114161960d0576100016024600037600360005260045160205260005160405260205160605260405160805260605160a05260805160c05260a05160e052610001610001206000541419600b57602051331419600b5760043501431019600b577fb2ad75dd1a1c2ea09e4f34d4c4b85a6115e1bac907755a17c4861d2e9589adc4604060c0a13460005260006000511460205260205119600b5760a05160805160c05160655750607c5660405260405160e051111960605260605119600b576001600060605160605750609f5660805260e0516080510360a05260016000606051605b575060ba5660c05260e05160a0510360e05260c05160805160c0516056575060d75660005260805160a05160c0516051575060ea5660205260e05160a05160c051604c575060fd5660405260005160605260205160805260405160a05260005160c05260205160e05260405160005260605160005260805160205260a05160405260c05160605260e05160805260005160a052601056fe905060fd56905060ea56905060d756905060ba569050609f569050607c566000357f01d3de16765b92a51559e06c5f6613d53845cf7d4a83dc60253fb42838e7b8b7143660e414161960aa5760c06024600037600360c05260045160e05260005160005260205160205260405160405260605160605260805160805260a05160a05260c0610001206000541419600b57602051331419600b5760043501431019600b577fb62fd2e0bd0fa0a08db19ecbed8856c1526909b8e2fc34b339497b67794469d6600060c0a13460c052600060c0511460e05260e05119600b5731600052600080808060005160205161fc08f119600b57600456fe6000357f011691be95f25b4438491720b950335e03c91afd598d23511796fa408943d9bd1436612401141619606a576100016024600037600360005260045160205260005160405260205160605260405160805260605160a05260805160c05260a05160e052610001610001206000541419600b57600051331419600b5760043501431019600b577f03fd5991feb35c5558d096c751a1f4b9b84caacee256d79372230d29af534af0604060c0a13460005260006000511460205260205119600b5760a05160805160c05160ff575060165660405260405160e051111960605260605119600b576001600060605160fa575060395660805260e0516080510360a0526001600060605160f5575060545660c05260e05160a0510360e05260c05160805160c05160f0575060715660005260805160a05160c05160eb575060845660205260e05160a05160c05160e6575060975660405260005160605260205160805260405160a05260005160c05260205160e05260405160005260605160005260805160205260a05160405260c05160605260e05160805260005160a052601056fe9050609756905060845690506071569050605456905060395690506016566000357ffdc1e38c5cb224a235edf8cfd6edcb78df69170e21965a3c9f10c63466ff18ca143661040114161960445760e06024600037600260e05260045160005260005160205260205160405260405160605260605160805260805160a05260a05160c05260e0610001206000541419600b57600051331419600b5760043501431019600b577f82e3bb32335fc0f729b9e3e6cb90526a2c11ab7b93b46ce2dd4563afa59a8a4b602060c0a13460e052600060e0511460005260005119600b5760c0516020526120016020206020526020516080511460405260405119600b5760a05160c0510160605260026060510660805260006080511460a05260005160c05260205160e05260405160005260a05160205260605160405260605160605260c05160005260e05160205260005160405260205160605260405160805260605160a052601056fe6000357fcbfcef243db6b0d7c9f310227d0b801d855f022d4f738707a954dc36ef84e72a143660c414161960045760a06024600037600160a05260045160c05260005160e05260205160005260405160205260605160405260a060c0206000541419600b5760043501431019600b577f09c7f2b3c4192c19c472f5780297622ea1608f76a33b9d14bc66f77bc50ae71660206080a13460a05260205160a0511460c05260c05119600b57600260e052436000526000516020523360405260205160605260405160805260605160a05260805160c05260e061000120600055fe6000357f124bb19f7a1694627778a56119ea7985704195f86b3acabaaa0b7164fb691afa14366084141619604c5760606024600037600060605260045160805260606040206000541419600b5760043501431019600b577f841685f0d5ab4d9d5a74b0fbead5aa01522c2c76c9dce9fd7db4e37bac1e38bc60606000a1346060526000516060511460805260805119600b57600160a0524360c0523360e05260005160005260205160205260405160405260a060c020600055fe" };

export const ALGO = {
  LogicSigProgram: `AiAEAgYAASIyBA5BABMzABAjEkEACzMAGC0SQQADQgACJEMlQw==`
  ,
  
  ApprovalProgram: `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`
  ,
  
  ClearStateProgram: `AiABASI=` };