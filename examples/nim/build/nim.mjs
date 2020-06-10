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
  const txn1 = await ctc.sendrecv('A', 1, 3, [v3, v5, v8], v3, 10, 2, async (txn_out, txn1) => {
    const v9 = txn1.value;
    const v10 = stdlib.eq(v9, v3);
    stdlib.assert(v10);
    return true; });
  if (txn1.didTimeout) {
    return ['A never started']; }
  else {
    const v0 = txn1.from;
    const v9 = txn1.value;
    const v10 = stdlib.eq(v9, v3);
    stdlib.assert(v10);
    const txn2 = await ctc.recv('A', 3, 1, 10, true, [v0, v3, v5, v8], 4, async (txn_out, txn2) => {
      const v71 = txn2.balance;
      txn_out.transfer(v0, v71);
      return true; });
    if (txn2.didTimeout) {
      return ['B never accepted']; }
    else {
      const [v13] = txn2.data;
      const v1 = txn2.from;
      const v14 = txn2.value;
      const v15 = stdlib.eq(v14, v3);
      stdlib.assert(v15);
      const v16 = v6;
      const txn3 = await ctc.sendrecv('A', 5, 1, [v0, v1, v3, v5, v8, v13, v16], 0, 10, 6, async (txn_out, txn3) => {
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
            const v65 = v24 ? 2 : 0;
            const v66 = v24 ? 0 : 2;
            const v67 = stdlib.mul(v65, v3);
            txn_out.transfer(v0, v67);
            const v68 = stdlib.mul(v66, v3);
            txn_out.transfer(v1, v68);
            return true; }
          else {
            if (v24) {
              return true; }
            else {
              return true; } } } });
      if (txn3.didTimeout) {
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
            const v69 = stdlib.isType('bool', await interact.showOutcome(v24));
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
              const txn4 = await ctc.sendrecv('A', 8, 2, [v0, v1, v3, v24, v25, v26, v34, v36], 0, 10, 9, async (txn_out, txn4) => {
                const v39 = txn4.value;
                const v40 = stdlib.eq(v39, 0);
                stdlib.assert(v40);
                return true; });
              if (txn4.didTimeout) {
                return ['A timed out move']; }
              else {
                const v39 = txn4.value;
                const v40 = stdlib.eq(v39, 0);
                stdlib.assert(v40);
                const v41 = v24 ? false : true;
                const v42 = stdlib.sub(v25, v36);
                const v43 = v24 ? false : true;
                const v44 = stdlib.sub(v26, v36);
                const v45 = v34 ? v41 : v43;
                const v46 = v34 ? v42 : v25;
                const v47 = v34 ? v26 : v44;
                v24 = v45;
                v25 = v46;
                v26 = v47;
                continue; } }
            else {
              const txn4 = await ctc.recv('A', 10, 2, 10, true, [v0, v1, v3, v24, v25, v26], 11, async (txn_out, txn4) => {
                const v64 = txn4.balance;
                txn_out.transfer(v0, v64);
                return true; });
              if (txn4.didTimeout) {
                return ['B timed out move']; }
              else {
                const [v50, v52] = txn4.data;
                const v55 = txn4.value;
                const v56 = stdlib.eq(v55, 0);
                stdlib.assert(v56);
                const v57 = v24 ? false : true;
                const v58 = stdlib.sub(v25, v52);
                const v59 = v24 ? false : true;
                const v60 = stdlib.sub(v26, v52);
                const v61 = v50 ? v57 : v59;
                const v62 = v50 ? v58 : v25;
                const v63 = v50 ? v26 : v60;
                v24 = v61;
                v25 = v62;
                v26 = v63;
                continue; } } } } } } } }

export async function B(stdlib, ctc, interact) {
  const txn0 = { balance: 0, value: 0 };
  const txn1 = await ctc.recv('B', 1, 3, 10, true, [], 2, async (txn_out, txn1) => {
    return true; });
  if (txn1.didTimeout) {
    return ['A never started']; }
  else {
    const [v3, v5, v8] = txn1.data;
    const v0 = txn1.from;
    const v9 = txn1.value;
    const v10 = stdlib.eq(v9, v3);
    stdlib.assert(v10);
    const v11 = stdlib.isType('bool', await interact.acceptWager(v3));
    const v12 = stdlib.random_uint256();
    const v13 = v12;
    const txn2 = await ctc.sendrecv('B', 3, 1, [v0, v3, v5, v8, v13], v3, 10, 4, async (txn_out, txn2) => {
      const v14 = txn2.value;
      const v15 = stdlib.eq(v14, v3);
      stdlib.assert(v15);
      return true; });
    if (txn2.didTimeout) {
      return ['B never accepted']; }
    else {
      const v1 = txn2.from;
      const v14 = txn2.value;
      const v15 = stdlib.eq(v14, v3);
      stdlib.assert(v15);
      const txn3 = await ctc.recv('B', 5, 1, 10, true, [v0, v1, v3, v5, v8, v13], 6, async (txn_out, txn3) => {
        const v70 = txn3.balance;
        txn_out.transfer(v1, v70);
        return true; });
      if (txn3.didTimeout) {
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
            const v69 = stdlib.isType('bool', await interact.showOutcome(v24));
            return ['Game is over']; }
          else {
            if (v24) {
              const txn4 = await ctc.recv('B', 8, 2, 10, true, [v0, v1, v3, v24, v25, v26], 9, async (txn_out, txn4) => {
                const v48 = txn4.balance;
                txn_out.transfer(v1, v48);
                return true; });
              if (txn4.didTimeout) {
                return ['A timed out move']; }
              else {
                const [v34, v36] = txn4.data;
                const v39 = txn4.value;
                const v40 = stdlib.eq(v39, 0);
                stdlib.assert(v40);
                const v41 = v24 ? false : true;
                const v42 = stdlib.sub(v25, v36);
                const v43 = v24 ? false : true;
                const v44 = stdlib.sub(v26, v36);
                const v45 = v34 ? v41 : v43;
                const v46 = v34 ? v42 : v25;
                const v47 = v34 ? v26 : v44;
                v24 = v45;
                v25 = v46;
                v26 = v47;
                continue; } }
            else {
              const v49 = stdlib.isType('bool', await interact.getHeap(v25, v26));
              const v50 = v49;
              const v51 = stdlib.isType('uint256', await interact.getAmount(v25, v26));
              const v52 = v51;
              const v53 = v50 ? v25 : v26;
              const v54 = stdlib.le(v52, v53);
              stdlib.assert(v54);
              const txn4 = await ctc.sendrecv('B', 10, 2, [v0, v1, v3, v24, v25, v26, v50, v52], 0, 10, 11, async (txn_out, txn4) => {
                const v55 = txn4.value;
                const v56 = stdlib.eq(v55, 0);
                stdlib.assert(v56);
                return true; });
              if (txn4.didTimeout) {
                return ['B timed out move']; }
              else {
                const v55 = txn4.value;
                const v56 = stdlib.eq(v55, 0);
                stdlib.assert(v56);
                const v57 = v24 ? false : true;
                const v58 = stdlib.sub(v25, v52);
                const v59 = v24 ? false : true;
                const v60 = stdlib.sub(v26, v52);
                const v61 = v50 ? v57 : v59;
                const v62 = v50 ? v58 : v25;
                const v63 = v50 ? v26 : v60;
                v24 = v61;
                v25 = v62;
                v26 = v63;
                continue; } } } } } } } }

export const ETH = {
  ABI: [{"inputs":[],"payable":true,"stateMutability":"payable","type":"constructor"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v3","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v5","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v8","type":"uint256"}],"name":"e1","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"},{"indexed":false,"internalType":"bool","name":"v50","type":"bool"},{"indexed":false,"internalType":"uint256","name":"v52","type":"uint256"}],"name":"e10","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e11","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e2","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v13","type":"uint256"}],"name":"e3","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e4","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v16","type":"uint256"}],"name":"e5","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e6","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"},{"indexed":false,"internalType":"bool","name":"v34","type":"bool"},{"indexed":false,"internalType":"uint256","name":"v36","type":"uint256"}],"name":"e8","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e9","type":"event"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"uint256","name":"v3","type":"uint256"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v8","type":"uint256"}],"name":"m1","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v0","type":"address"},{"internalType":"address payable","name":"v1","type":"address"},{"internalType":"uint256","name":"v3","type":"uint256"},{"internalType":"bool","name":"v24","type":"bool"},{"internalType":"uint256","name":"v25","type":"uint256"},{"internalType":"uint256","name":"v26","type":"uint256"},{"internalType":"bool","name":"v50","type":"bool"},{"internalType":"uint256","name":"v52","type":"uint256"}],"name":"m10","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v0","type":"address"},{"internalType":"address payable","name":"v1","type":"address"},{"internalType":"uint256","name":"v3","type":"uint256"},{"internalType":"bool","name":"v24","type":"bool"},{"internalType":"uint256","name":"v25","type":"uint256"},{"internalType":"uint256","name":"v26","type":"uint256"}],"name":"m11","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"}],"name":"m2","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v0","type":"address"},{"internalType":"uint256","name":"v3","type":"uint256"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v8","type":"uint256"},{"internalType":"uint256","name":"v13","type":"uint256"}],"name":"m3","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v0","type":"address"},{"internalType":"uint256","name":"v3","type":"uint256"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v8","type":"uint256"}],"name":"m4","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v0","type":"address"},{"internalType":"address payable","name":"v1","type":"address"},{"internalType":"uint256","name":"v3","type":"uint256"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v8","type":"uint256"},{"internalType":"uint256","name":"v13","type":"uint256"},{"internalType":"uint256","name":"v16","type":"uint256"}],"name":"m5","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v0","type":"address"},{"internalType":"address payable","name":"v1","type":"address"},{"internalType":"uint256","name":"v3","type":"uint256"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v8","type":"uint256"},{"internalType":"uint256","name":"v13","type":"uint256"}],"name":"m6","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v0","type":"address"},{"internalType":"address payable","name":"v1","type":"address"},{"internalType":"uint256","name":"v3","type":"uint256"},{"internalType":"bool","name":"v24","type":"bool"},{"internalType":"uint256","name":"v25","type":"uint256"},{"internalType":"uint256","name":"v26","type":"uint256"},{"internalType":"bool","name":"v34","type":"bool"},{"internalType":"uint256","name":"v36","type":"uint256"}],"name":"m8","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v0","type":"address"},{"internalType":"address payable","name":"v1","type":"address"},{"internalType":"uint256","name":"v3","type":"uint256"},{"internalType":"bool","name":"v24","type":"bool"},{"internalType":"uint256","name":"v25","type":"uint256"},{"internalType":"uint256","name":"v26","type":"uint256"}],"name":"m9","outputs":[],"payable":true,"stateMutability":"payable","type":"function"}]
  ,
  
  Bytecode: "0x600060a08181524360c0526040608081905260e0815290209055610f9d806100286000396000f3fe6080604052600436106100915760003560e01c80639a4d12fa116100595780639a4d12fa146101e8578063b375096e14610236578063b7ae93d914610296578063c3f7ebd1146102c5578063c59bcb2d1461032557610091565b806302e87732146100965780630d8020ea146100e857806336807e781461013857806344a8c36d146101555780636bcbdb66146101aa575b600080fd5b6100e6600480360360e08110156100ac57600080fd5b508035906001600160a01b036020820135811691604081013590911690606081013590608081013515159060a08101359060c00135610369565b005b6100e6600480360360e08110156100fe57600080fd5b508035906001600160a01b036020820135811691604081013590911690606081013590608081013515159060a08101359060c00135610477565b6100e66004803603602081101561014e57600080fd5b5035610585565b6100e6600480360361010081101561016c57600080fd5b508035906001600160a01b03602082013581169160408101359091169060608101359060808101359060a08101359060c08101359060e00135610608565b6100e6600480360360a08110156101c057600080fd5b508035906001600160a01b036020820135169060408101359060608101359060800135610730565b6100e6600480360360e08110156101fe57600080fd5b508035906001600160a01b03602082013581169160408101359091169060608101359060808101359060a08101359060c00135610829565b6100e6600480360361012081101561024d57600080fd5b508035906001600160a01b036020820135811691604081013590911690606081013590608081013515159060a08101359060c08101359060e08101351515906101000135610934565b6100e6600480360360808110156102ac57600080fd5b5080359060208101359060408101359060600135610a78565b6100e660048036036101208110156102dc57600080fd5b508035906001600160a01b036020820135811691604081013590911690606081013590608081013515159060a08101359060c08101359060e08101351515906101000135610b7f565b6100e6600480360360c081101561033b57600080fd5b508035906001600160a01b036020820135169060408101359060608101359060808101359060a00135610c7e565b6040805160056020808301919091528183018a90526001600160601b031960608a811b82168185015289901b1660748301526088820187905285151560f81b60a883015260a9820185905260c98083018590528351808403909101815260e99092019092528051910120600054146103e057600080fd5b336001600160a01b038616146103f557600080fd5b600a870143101561040557600080fd5b6040516001600160a01b03861690303180156108fc02916000818181858888f1935050505015801561043b573d6000803e3d6000fd5b50604080513031815290517fc92018b4e91e597d736654f7b1d2ec034c5fec5920e2cfe22e15b4ddcdf5e18a9181900360200190a16000805533ff5b6040805160056020808301919091528183018a90526001600160601b031960608a811b82168185015289901b1660748301526088820187905285151560f81b60a883015260a9820185905260c98083018590528351808403909101815260e99092019092528051910120600054146104ee57600080fd5b336001600160a01b0387161461050357600080fd5b600a870143101561051357600080fd5b6040516001600160a01b03871690303180156108fc02916000818181858888f19350505050158015610549573d6000803e3d6000fd5b50604080513031815290517fd22b308a0739d4b2391b9fea991868a737c5ac9fca1931271dbb52121d7192ad9181900360200190a16000805533ff5b60408051600060208083018290528284018590528351808403850181526060909301909352815191909201209054146105bd57600080fd5b600a81014310156105cd57600080fd5b604080513031815290517ff04f5fc87a72102f7c0b228f8bbaf9b9aa7a2b5dc295c86538fdde91e95866e99181900360200190a16000805533ff5b6040805160036020808301919091528183018b90526001600160601b031960608b811b8216818501528a901b1660748301526088820188905260a8820187905260c8820186905260e88083018690528351808403909101815261010890920190925280519101206000541461067c57600080fd5b336001600160a01b0388161461069157600080fd5b600a880143106106a057600080fd5b34156106ab57600080fd5b604080516020808201849052825180830382018152918301909252805191012083146106d657600080fd5b60408051303181526020810183905281517fabf482d77b67111a4971bb96fe81961f83ba459eb1d8fa9f78b6908251aeef1a929181900390910190a1610726878787600186860116158880610db6565b5050505050505050565b604080516001602080830191909152818301889052606087811b6001600160601b03191690830152607482018690526094820185905260b48083018590528351808403909101815260d490920190925280519101206000541461079257600080fd5b336001600160a01b038516146107a757600080fd5b600a85014310156107b757600080fd5b6040516001600160a01b03851690303180156108fc02916000818181858888f193505050501580156107ed573d6000803e3d6000fd5b50604080513031815290517f6ce5b12953112c528c3a24a99350a573e6cb61c4e39d70f2392cc6bd7266f9699181900360200190a16000805533ff5b6040805160036020808301919091528183018a90526001600160601b031960608a811b82168185015289901b1660748301526088820187905260a8820186905260c8820185905260e88083018590528351808403909101815261010890920190925280519101206000541461089d57600080fd5b336001600160a01b038616146108b257600080fd5b600a87014310156108c257600080fd5b6040516001600160a01b03861690303180156108fc02916000818181858888f193505050501580156108f8573d6000803e3d6000fd5b50604080513031815290517fcb3347bd475fd43f41b4bc5bb011db952f2079e6ba9a82ff211988cd7871dba69181900360200190a16000805533ff5b6040805160056020808301919091528183018c90526001600160601b031960608c811b8216818501528b901b1660748301526088820189905287151560f81b60a883015260a9820187905260c98083018790528351808403909101815260e99092019092528051910120600054146109ab57600080fd5b336001600160a01b038916146109c057600080fd5b600a890143106109cf57600080fd5b34156109da57600080fd5b6040805130318152831515602082015280820183905290517f4b039c0ad919ee5be89471485bea37e7665f9bc99eaf3dbb443a84d448e2478c9181900360600190a1610a6d88888885610a3b5788610a33576001610a36565b60005b610a4a565b88610a47576001610a4a565b60005b86610a555788610a59565b8589035b87610a6657868903610a68565b885b610db6565b505050505050505050565b6040805160006020808301829052828401889052835180840385018152606090930190935281519190920120905414610ab057600080fd5b610ab8610f56565b338152600a85014310610aca57600080fd5b833414610ad657600080fd5b6040805130318152602081018690528082018590526060810184905290517fae151f568caae238ebfd8aad9de993842a2ed108505e0de35098d02f263f5f2a9181900360800190a15160408051600160208083019190915243828401526001600160601b0319606094851b16938201939093526074810195909552609485019390935260b4808501929092528251808503909201825260d4909301909152805191012060005550565b6040805160056020808301919091528183018c90526001600160601b031960608c811b8216818501528b901b1660748301526088820189905287151560f81b60a883015260a9820187905260c98083018790528351808403909101815260e9909201909252805191012060005414610bf657600080fd5b336001600160a01b03881614610c0b57600080fd5b600a89014310610c1a57600080fd5b3415610c2557600080fd5b6040805130318152831515602082015280820183905290517f8eef58f1a9464c84a7d0eece0613fb8b8273d6eca4697594722210dc164f93b39181900360600190a1610a6d88888885610a3b5788610a33576001610a36565b604080516001602080830191909152818301899052606088811b6001600160601b03191690830152607482018790526094820186905260b48083018690528351808403909101815260d4909201909252805191012060005414610ce057600080fd5b610ce8610f56565b338152600a87014310610cfa57600080fd5b843414610d0657600080fd5b60408051303181526020810184905281517f94dd7e08991b8945fde2d5865f7071e72045b9800e293ff60d29c6960c5a4fb5929181900390910190a15160408051600360208083019190915243828401526001600160601b03196060998a1b81168a8401529390981b9092166074830152608882019590955260a881019390935260c883019190915260e88083019190915282518083039091018152610108909101909152805191012060005550565b600081830111610dc7576001610dca565b60005b15610e6a57856001600160a01b03166108fc8585610de9576000610dec565b60025b029081150290604051600060405180830381858888f19350505050158015610e18573d6000803e3d6000fd5b50846001600160a01b03166108fc8585610e33576002610e36565b60005b029081150290604051600060405180830381858888f19350505050158015610e62573d6000803e3d6000fd5b506000805533ff5b8215610ee15760408051600560208083019190915243828401526001600160601b031960608a811b82168185015289901b1660748301526088820187905285151560f81b60a883015260a9820185905260c98083018590528351808403909101815260e99092019092528051910120600055610f4e565b60408051600560208083019190915243828401526001600160601b031960608a811b82168185015289901b1660748301526088820187905285151560f81b60a883015260a9820185905260c98083018590528351808403909101815260e990920190925280519101206000555b505050505050565b6040805160208101909152600081529056fea265627a7a723158208c718353b9b0cbc7444d90e4ac0e9e63c106d2a8d2cf8f8cd8a7d132019a85fe64736f6c634300050c0032"
  ,
  
  Bytecode2: "0x600060005243602052600060402060005560d1601d600039600080f3fe601556fe6000805533fffe600080fdfe60a0516080510160c052600060c0511160e0526001600060e05160245750603156600052600051607b5760605160ea57600560a0524360c05260005160e05260205160005260405160205260605160405260805160605260a05160805261a0016100012060005560e756fe6000600260605160e257506089566020526002600060605160dd5750609a5660405260405160205102606052600080808060605160005161fc08f119600b5760405160405102608052600080808060805160205161fc08f119600b57600456607a569050609a569050608956607a56600560a0524360c05260005160e05260205160005260405160205260605160405260805160605260a05160805261a0016100012060005560e75690506031566000357f62f8449c2410921fe9bf35089b36c4240c7ab304c6470c94da755a1c0e27654f143660e4141619600b5760c06024600037600560c05260045160e05260005160005260205160205260405160405260605160605260805160805260a05160a05260c0610001206000541419600b57600051331419600b57600435600a014310600b577fb561a85f1fcec4bf2ff2b443caab9a6d936b5aa994b90a75a21fca84dc4e2ccf600060c0a13160c052600080808060c05160005161fc08f119600b57600456fe6000357fad8fcbe126122cb1fcb56f8e56c5ad6f806bd0084c5ca2a0247de16ed8f8b17f14366124011416196029576100016024600037600560005260045160205260005160405260205160605260405160805260605160a05260805160c05260a05160e052610001610001206000541419600b57602051331419600b57600435600a01431019600b577fbd32649117608c5766eac6b95c3d5684ddc97114bc930290174744c210a89bc7604060c0a13460005260006000511460205260205119600b57600160006060516083575060c25660405260e0516080510360605260016000606051607e575060dd5660805260e05160a0510360a05260805160405160c0516079575060fa5660c05260805160605160c05160745750600d5660e05260a05160a05160c051606f575060205660005260005160205260205160405260405160605260c05160805260e05160a05260005160c05260205160005260405160205260605160405260805160605260a05160805260c05160a052601056fe90506020569050600d56905060fa56905060dd56905060c2566000357f327f71102c015281c47aa845827763f6fac334946918282680df35b9f5fcb113143660e414161960f05760c06024600037600560c05260045160e05260005160005260205160205260405160405260605160605260805160805260a05160a05260c0610001206000541419600b57602051331419600b57600435600a014310600b577f4974e0511e1142aba56ace19fb538cc1d2ed6b16c2fe388f2ec8b2dbc409a55b600060c0a13160c052600080808060c05160205161fc08f119600b57600456fe6000357fd268d69d496723f14ec4918dff311f15176d6586164c243eb559277ca271464314366124011416196088576100016024600037600560005260045160205260005160405260205160605260405160805260605160a05260805160c05260a05160e052610001610001206000541419600b57600051331419600b57600435600a01431019600b577f84a0582ee67edbb94e73f76b6bfe7002c33c7faca655c059e91df7a69ff73e7c604060c0a13460005260006000511460205260205119600b576001600060605160e2575060215660405260e051608051036060526001600060605160dd5750603c5660805260e05160a0510360a05260805160405160c05160d8575060595660c05260805160605160c05160d35750606c5660e05260a05160a05160c05160ce5750607f5660005260005160205260205160405260405160605260c05160805260e05160a05260005160c05260205160005260405160205260605160405260805160605260a05160805260c05160a052601056fe9050607f569050606c5690506059569050603c5690506021566000357f6224342a6c646d9e7050b4c877fbd02aaf2e450e80d9dde89292da84385babf8143660e4141619604f5760c06024600037600360c05260045160e05260005160005260205160205260405160405260605160605260805160805260a05160a05260c0610001206000541419600b57602051331419600b57600435600a014310600b577fb62fd2e0bd0fa0a08db19ecbed8856c1526909b8e2fc34b339497b67794469d6600060c0a13160c052600080808060c05160205161fc08f119600b57600456fe6000357f85bf760925c39bf90f8e6e685d10631be02f090cddb822aa5c3a53e18e524bee143661040114161960e75760e06024600037600360e05260045160005260005160205260205160405260405160605260605160805260805160a05260a05160c05260e0610001206000541419600b57600051331419600b57600435600a01431019600b577f97bfc1a0968eb62b4ee0627106b54e4c8d3f7374e164fe1921fc823f3dc795f5602060c0a13460e052600060e0511460005260005119600b5760c0516020526120016020206020526020516080511460405260405119600b5760a05160c0510160605260026060510660805260006080511460a05260005160c05260205160e05260405160005260a05160205260605160405260605160605260c05160005260e05160205260005160405260205160605260405160805260605160a052601056fe6000357f472ec3312c9fa5c285dcbd92be6c63412b28440c1bd0c71651c3816df48e139a143660a414161960ae5760806024600037600160805260045160a05260005160c05260205160e052604051600052606051602052608060c0206000541419600b57600051331419600b57600435600a014310600b577f11362c0f348f1cdb17fd45407c8beb58b91350df448e9727e20e528654f0e57960006080a131608052600080808060805160005161fc08f119600b57600456fe6000357ff995221e59dd37d6a72dbd1d925e26db7fb010216ef9c9138bca780aab8538cd143660c414161960f85760a06024600037600160a05260045160c05260005160e05260205160005260405160205260605160405260a060c0206000541419600b57600435600a01431019600b577f82e3bb32335fc0f729b9e3e6cb90526a2c11ab7b93b46ce2dd4563afa59a8a4b60206080a13460a05260205160a0511460c05260c05119600b57600360e052436000526000516020523360405260205160605260405160805260605160a05260805160c05260e061000120600055fe6000357fbac6ebd41b106f55381a0967bc00cee5537249826fdcc0de37de29e6ca69aa911436602414161960b25760006024600037600060005260045160205260006040206000541419600b57600435600a014310600b577f354083e34305e43504f607a379840b90a0c97b23b093bfbf711fd18fe8f9319b60006000a1600456fe6000357f124bb19f7a1694627778a56119ea7985704195f86b3acabaaa0b7164fb691afa1436608414161960935760606024600037600060605260045160805260606040206000541419600b57600435600a01431019600b577f841685f0d5ab4d9d5a74b0fbead5aa01522c2c76c9dce9fd7db4e37bac1e38bc60606000a1346060526000516060511460805260805119600b57600160a0524360c0523360e05260005160005260205160205260405160405260a060c020600055fe" };

export const ALGO = {
  LogicSigProgram: `AiAEAgYAASIyBA5BABMzABAjEkEACzMAGC0SQQADQgACJEMlQw==`
  ,
  
  ApprovalProgram: `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`
  ,
  
  ClearStateProgram: `AiABASI=` };