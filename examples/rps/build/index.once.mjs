// Automatically generated with Reach 0.1.0

export async function A(stdlib, ctc, interact) {
  const txn0 = { balance: 0, value: 0 };
  const v1 = stdlib.protect(stdlib.T_Tuple([stdlib.T_UInt256, stdlib.T_UInt256]), await interact.getParams());
  const v2 = v1[0];
  const v3 = v1[1];
  
  const v7 = stdlib.add(v2, v3);
  
  const txn1 = await ctc.sendrecv('A', 1, 2, [v2, v3], v7, false, null);
  const [v4, v5] = txn1.data;
  const v6 = txn1.from;
  const v8 = stdlib.add(v4, v5);
  const v9 = txn1.value;
  const v10 = stdlib.eq(v8, v9);
  stdlib.assert(v10);
  const txn2 = await ctc.recv('A', 2, 0, 10);
  if (txn2.didTimeout) {
    
    const txn3 = await ctc.sendrecv('A', 9, 0, [v6, v4, v5], 0, false, null);
    const [] = txn3.data;
    const v19 = txn3.value;
    const v20 = stdlib.eq(0, v19);
    stdlib.assert(v20);
    stdlib.assert(true);
    stdlib.protect(stdlib.T_Null, await interact.endsWith('Bob quits'));
    
    return; }
  else {
    const [] = txn2.data;
    const v14 = txn2.from;
    const v15 = txn2.value;
    const v16 = stdlib.eq(v4, v15);
    stdlib.assert(v16);
    stdlib.protect(stdlib.T_Null, await interact.partnerIs(v14));
    let v36;
    const v37 = stdlib.protect(stdlib.T_Bytes, await interact.getHand());
    const v38 = stdlib.bytes_eq(v37, 'ROCK');
    const v39 = stdlib.bytes_eq(v37, 'PAPER');
    const v40 = stdlib.bytes_eq(v37, 'SCISSORS');
    const v42 = v38 ? true : v39;
    const v44 = v42 ? true : v40;
    stdlib.assert(v44);
    if (v38) {
      v36 = 0;
       }
    else {
      if (v39) {
        v36 = 1;
         }
      else {
        v36 = 2;
         }
       }
    const v52 = stdlib.protect(stdlib.T_UInt256, await interact.random());
    const v53 = stdlib.keccak256(v52, v36);
    stdlib.protect(stdlib.T_Null, await interact.commits());
    
    
    const txn3 = await ctc.sendrecv('A', 3, 1, [v6, v4, v5, v14, v53], 0, 10, null);
    if (txn3.didTimeout) {
      const txn4 = await ctc.recv('A', 8, 0, false);
      const [] = txn4.data;
      const v60 = txn4.value;
      const v61 = stdlib.eq(0, v60);
      stdlib.assert(v61);
      stdlib.assert(true);
      stdlib.protect(stdlib.T_Null, await interact.endsWith('Alice quits'));
      
      return; }
    else {
      const [v55] = txn3.data;
      const v56 = txn3.value;
      const v57 = stdlib.eq(0, v56);
      stdlib.assert(v57);
      const txn4 = await ctc.recv('A', 4, 1, 10);
      if (txn4.didTimeout) {
        
        const txn5 = await ctc.sendrecv('A', 7, 0, [v6, v4, v5, v14, v55], 0, false, null);
        const [] = txn5.data;
        const v97 = txn5.value;
        const v98 = stdlib.eq(0, v97);
        stdlib.assert(v98);
        stdlib.assert(true);
        stdlib.protect(stdlib.T_Null, await interact.endsWith('Bob quits'));
        
        return; }
      else {
        const [v92] = txn4.data;
        const v93 = txn4.value;
        const v94 = stdlib.eq(0, v93);
        stdlib.assert(v94);
        const v112 = stdlib.le(0, v92);
        const v113 = stdlib.lt(v92, 3);
        const v115 = v112 ? v113 : false;
        stdlib.assert(v115);
        let v117;
        const v119 = stdlib.le(0, v92);
        const v120 = stdlib.lt(v92, 3);
        const v122 = v119 ? v120 : false;
        stdlib.assert(v122);
        const v123 = stdlib.eq(v92, 0);
        if (v123) {
          v117 = 'ROCK';
           }
        else {
          const v124 = stdlib.eq(v92, 1);
          if (v124) {
            v117 = 'PAPER';
             }
          else {
            v117 = 'SCISSORS';
             }
           }
        stdlib.protect(stdlib.T_Null, await interact.reveals(v117));
        
        
        const txn5 = await ctc.sendrecv('A', 5, 2, [v6, v4, v5, v14, v55, v92, v52, v36], 0, 10, null);
        if (txn5.didTimeout) {
          const txn6 = await ctc.recv('A', 6, 0, false);
          const [] = txn6.data;
          const v132 = txn6.value;
          const v133 = stdlib.eq(0, v132);
          stdlib.assert(v133);
          stdlib.assert(true);
          stdlib.protect(stdlib.T_Null, await interact.endsWith('Alice quits'));
          
          return; }
        else {
          const [v126, v127] = txn5.data;
          const v128 = txn5.value;
          const v129 = stdlib.eq(0, v128);
          stdlib.assert(v129);
          const v147 = stdlib.keccak256(v126, v127);
          const v148 = stdlib.eq(v55, v147);
          stdlib.assert(v148);
          const v150 = stdlib.le(0, v127);
          const v151 = stdlib.lt(v127, 3);
          const v153 = v150 ? v151 : false;
          stdlib.assert(v153);
          let v155;
          const v157 = stdlib.le(0, v127);
          const v158 = stdlib.lt(v127, 3);
          const v160 = v157 ? v158 : false;
          const v162 = stdlib.le(0, v92);
          const v163 = stdlib.lt(v92, 3);
          const v165 = v162 ? v163 : false;
          const v167 = v160 ? v165 : false;
          if (v167) {
            const v168 = stdlib.sub(4, v92);
            const v169 = stdlib.add(v127, v168);
            const v170 = stdlib.mod(v169, 3);
            v155 = v170;
             }
          else {
            if (v160) {
              v155 = 2;
               }
            else {
              if (v165) {
                v155 = 0;
                 }
              else {
                v155 = 1;
                 }
               }
             }
          let v228;
          const v229 = stdlib.eq(v155, 2);
          if (v229) {
            const v230 = stdlib.mul(2, v4);
            v228 = [v230, 0];
             }
          else {
            const v231 = stdlib.eq(v155, 0);
            if (v231) {
              const v232 = stdlib.mul(2, v4);
              v228 = [0, v232];
               }
            else {
              v228 = [v4, v4];
               }
             }
          let v239;
          const v241 = stdlib.le(0, v155);
          const v242 = stdlib.lt(v155, 5);
          const v244 = v241 ? v242 : false;
          stdlib.assert(v244);
          const v245 = stdlib.eq(v155, 0);
          if (v245) {
            v239 = 'Bob wins';
             }
          else {
            const v246 = stdlib.eq(v155, 1);
            if (v246) {
              v239 = 'Draw';
               }
            else {
              const v247 = stdlib.eq(v155, 2);
              if (v247) {
                v239 = 'Alice wins';
                 }
              else {
                const v248 = stdlib.eq(v155, 3);
                if (v248) {
                  v239 = 'Alice quits';
                   }
                else {
                  v239 = 'Bob quits';
                   }
                 }
               }
             }
          stdlib.protect(stdlib.T_Null, await interact.endsWith(v239));
          
          return; } } } } }
export async function B(stdlib, ctc, interact) {
  const txn0 = { balance: 0, value: 0 };
  const txn1 = await ctc.recv('B', 1, 2, false);
  const [v4, v5] = txn1.data;
  const v6 = txn1.from;
  const v8 = stdlib.add(v4, v5);
  const v9 = txn1.value;
  const v10 = stdlib.eq(v8, v9);
  stdlib.assert(v10);
  stdlib.protect(stdlib.T_Null, await interact.partnerIs(v6));
  stdlib.protect(stdlib.T_Null, await interact.acceptParams(v4, v5));
  
  
  const txn2 = await ctc.sendrecv('B', 2, 0, [v6, v4, v5], v4, 10, null);
  if (txn2.didTimeout) {
    const txn3 = await ctc.recv('B', 9, 0, false);
    const [] = txn3.data;
    const v19 = txn3.value;
    const v20 = stdlib.eq(0, v19);
    stdlib.assert(v20);
    stdlib.assert(true);
    stdlib.protect(stdlib.T_Null, await interact.endsWith('Bob quits'));
    
    return; }
  else {
    const [] = txn2.data;
    const v14 = txn2.from;
    const v15 = txn2.value;
    const v16 = stdlib.eq(v4, v15);
    stdlib.assert(v16);
    const txn3 = await ctc.recv('B', 3, 1, 10);
    if (txn3.didTimeout) {
      
      const txn4 = await ctc.sendrecv('B', 8, 0, [v6, v4, v5, v14], 0, false, null);
      const [] = txn4.data;
      const v60 = txn4.value;
      const v61 = stdlib.eq(0, v60);
      stdlib.assert(v61);
      stdlib.assert(true);
      stdlib.protect(stdlib.T_Null, await interact.endsWith('Alice quits'));
      
      return; }
    else {
      const [v55] = txn3.data;
      const v56 = txn3.value;
      const v57 = stdlib.eq(0, v56);
      stdlib.assert(v57);
      let v76;
      const v77 = stdlib.protect(stdlib.T_Bytes, await interact.getHand());
      const v78 = stdlib.bytes_eq(v77, 'ROCK');
      const v79 = stdlib.bytes_eq(v77, 'PAPER');
      const v80 = stdlib.bytes_eq(v77, 'SCISSORS');
      const v82 = v78 ? true : v79;
      const v84 = v82 ? true : v80;
      stdlib.assert(v84);
      if (v78) {
        v76 = 0;
         }
      else {
        if (v79) {
          v76 = 1;
           }
        else {
          v76 = 2;
           }
         }
      stdlib.protect(stdlib.T_Null, await interact.shows());
      
      
      const txn4 = await ctc.sendrecv('B', 4, 1, [v6, v4, v5, v14, v55, v76], 0, 10, null);
      if (txn4.didTimeout) {
        const txn5 = await ctc.recv('B', 7, 0, false);
        const [] = txn5.data;
        const v97 = txn5.value;
        const v98 = stdlib.eq(0, v97);
        stdlib.assert(v98);
        stdlib.assert(true);
        stdlib.protect(stdlib.T_Null, await interact.endsWith('Bob quits'));
        
        return; }
      else {
        const [v92] = txn4.data;
        const v93 = txn4.value;
        const v94 = stdlib.eq(0, v93);
        stdlib.assert(v94);
        const v112 = stdlib.le(0, v92);
        const v113 = stdlib.lt(v92, 3);
        const v115 = v112 ? v113 : false;
        stdlib.assert(v115);
        const txn5 = await ctc.recv('B', 5, 2, 10);
        if (txn5.didTimeout) {
          
          const txn6 = await ctc.sendrecv('B', 6, 0, [v6, v4, v5, v14, v55, v92], 0, false, null);
          const [] = txn6.data;
          const v132 = txn6.value;
          const v133 = stdlib.eq(0, v132);
          stdlib.assert(v133);
          stdlib.assert(true);
          stdlib.protect(stdlib.T_Null, await interact.endsWith('Alice quits'));
          
          return; }
        else {
          const [v126, v127] = txn5.data;
          const v128 = txn5.value;
          const v129 = stdlib.eq(0, v128);
          stdlib.assert(v129);
          const v147 = stdlib.keccak256(v126, v127);
          const v148 = stdlib.eq(v55, v147);
          stdlib.assert(v148);
          const v150 = stdlib.le(0, v127);
          const v151 = stdlib.lt(v127, 3);
          const v153 = v150 ? v151 : false;
          stdlib.assert(v153);
          let v155;
          const v157 = stdlib.le(0, v127);
          const v158 = stdlib.lt(v127, 3);
          const v160 = v157 ? v158 : false;
          const v162 = stdlib.le(0, v92);
          const v163 = stdlib.lt(v92, 3);
          const v165 = v162 ? v163 : false;
          const v167 = v160 ? v165 : false;
          if (v167) {
            const v168 = stdlib.sub(4, v92);
            const v169 = stdlib.add(v127, v168);
            const v170 = stdlib.mod(v169, 3);
            v155 = v170;
             }
          else {
            if (v160) {
              v155 = 2;
               }
            else {
              if (v165) {
                v155 = 0;
                 }
              else {
                v155 = 1;
                 }
               }
             }
          let v228;
          const v229 = stdlib.eq(v155, 2);
          if (v229) {
            const v230 = stdlib.mul(2, v4);
            v228 = [v230, 0];
             }
          else {
            const v231 = stdlib.eq(v155, 0);
            if (v231) {
              const v232 = stdlib.mul(2, v4);
              v228 = [0, v232];
               }
            else {
              v228 = [v4, v4];
               }
             }
          let v251;
          const v253 = stdlib.le(0, v155);
          const v254 = stdlib.lt(v155, 5);
          const v256 = v253 ? v254 : false;
          stdlib.assert(v256);
          const v257 = stdlib.eq(v155, 0);
          if (v257) {
            v251 = 'Bob wins';
             }
          else {
            const v258 = stdlib.eq(v155, 1);
            if (v258) {
              v251 = 'Draw';
               }
            else {
              const v259 = stdlib.eq(v155, 2);
              if (v259) {
                v251 = 'Alice wins';
                 }
              else {
                const v260 = stdlib.eq(v155, 3);
                if (v260) {
                  v251 = 'Alice quits';
                   }
                else {
                  v251 = 'Bob quits';
                   }
                 }
               }
             }
          stdlib.protect(stdlib.T_Null, await interact.endsWith(v251));
          
          return; } } } } }
export async function O(stdlib, ctc, interact) {
  const txn0 = { balance: 0, value: 0 };
  const txn1 = await ctc.recv('O', 1, 2, false);
  const [v4, v5] = txn1.data;
  const v6 = txn1.from;
  const v8 = stdlib.add(v4, v5);
  const v9 = txn1.value;
  const v10 = stdlib.eq(v8, v9);
  stdlib.assert(v10);
  const txn2 = await ctc.recv('O', 2, 0, 10);
  if (txn2.didTimeout) {
    const txn3 = await ctc.recv('O', 9, 0, false);
    const [] = txn3.data;
    const v19 = txn3.value;
    const v20 = stdlib.eq(0, v19);
    stdlib.assert(v20);
    return; }
  else {
    const [] = txn2.data;
    const v14 = txn2.from;
    const v15 = txn2.value;
    const v16 = stdlib.eq(v4, v15);
    stdlib.assert(v16);
    const txn3 = await ctc.recv('O', 3, 1, 10);
    if (txn3.didTimeout) {
      const txn4 = await ctc.recv('O', 8, 0, false);
      const [] = txn4.data;
      const v60 = txn4.value;
      const v61 = stdlib.eq(0, v60);
      stdlib.assert(v61);
      return; }
    else {
      const [v55] = txn3.data;
      const v56 = txn3.value;
      const v57 = stdlib.eq(0, v56);
      stdlib.assert(v57);
      const txn4 = await ctc.recv('O', 4, 1, 10);
      if (txn4.didTimeout) {
        const txn5 = await ctc.recv('O', 7, 0, false);
        const [] = txn5.data;
        const v97 = txn5.value;
        const v98 = stdlib.eq(0, v97);
        stdlib.assert(v98);
        return; }
      else {
        const [v92] = txn4.data;
        const v93 = txn4.value;
        const v94 = stdlib.eq(0, v93);
        stdlib.assert(v94);
        const v112 = stdlib.le(0, v92);
        const v113 = stdlib.lt(v92, 3);
        const v115 = v112 ? v113 : false;
        stdlib.assert(v115);
        const txn5 = await ctc.recv('O', 5, 2, 10);
        if (txn5.didTimeout) {
          const txn6 = await ctc.recv('O', 6, 0, false);
          const [] = txn6.data;
          const v132 = txn6.value;
          const v133 = stdlib.eq(0, v132);
          stdlib.assert(v133);
          return; }
        else {
          const [v126, v127] = txn5.data;
          const v128 = txn5.value;
          const v129 = stdlib.eq(0, v128);
          stdlib.assert(v129);
          const v147 = stdlib.keccak256(v126, v127);
          const v148 = stdlib.eq(v55, v147);
          stdlib.assert(v148);
          const v150 = stdlib.le(0, v127);
          const v151 = stdlib.lt(v127, 3);
          const v153 = v150 ? v151 : false;
          stdlib.assert(v153);
          let v155;
          const v157 = stdlib.le(0, v127);
          const v158 = stdlib.lt(v127, 3);
          const v160 = v157 ? v158 : false;
          const v162 = stdlib.le(0, v92);
          const v163 = stdlib.lt(v92, 3);
          const v165 = v162 ? v163 : false;
          const v167 = v160 ? v165 : false;
          if (v167) {
            const v168 = stdlib.sub(4, v92);
            const v169 = stdlib.add(v127, v168);
            const v170 = stdlib.mod(v169, 3);
            v155 = v170;
             }
          else {
            if (v160) {
              v155 = 2;
               }
            else {
              if (v165) {
                v155 = 0;
                 }
              else {
                v155 = 1;
                 }
               }
             }
          let v228;
          const v229 = stdlib.eq(v155, 2);
          if (v229) {
            const v230 = stdlib.mul(2, v4);
            v228 = [v230, 0];
             }
          else {
            const v231 = stdlib.eq(v155, 0);
            if (v231) {
              const v232 = stdlib.mul(2, v4);
              v228 = [0, v232];
               }
            else {
              v228 = [v4, v4];
               }
             }
          return; } } } } }

export const ETH = {
  ABI: `[{"inputs":[],"payable":true,"stateMutability":"payable","type":"constructor"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v4","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v5","type":"uint256"}],"name":"e1","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e2","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v55","type":"uint256"}],"name":"e3","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v92","type":"uint256"}],"name":"e4","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v126","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v127","type":"uint256"}],"name":"e5","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e6","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e7","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e8","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e9","type":"event"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"uint256","name":"v4","type":"uint256"},{"internalType":"uint256","name":"v5","type":"uint256"}],"name":"m1","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v6","type":"address"},{"internalType":"uint256","name":"v4","type":"uint256"},{"internalType":"uint256","name":"v5","type":"uint256"}],"name":"m2","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v6","type":"address"},{"internalType":"uint256","name":"v4","type":"uint256"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"address payable","name":"v14","type":"address"},{"internalType":"uint256","name":"v55","type":"uint256"}],"name":"m3","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v6","type":"address"},{"internalType":"uint256","name":"v4","type":"uint256"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"address payable","name":"v14","type":"address"},{"internalType":"uint256","name":"v55","type":"uint256"},{"internalType":"uint256","name":"v92","type":"uint256"}],"name":"m4","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v6","type":"address"},{"internalType":"uint256","name":"v4","type":"uint256"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"address payable","name":"v14","type":"address"},{"internalType":"uint256","name":"v55","type":"uint256"},{"internalType":"uint256","name":"v92","type":"uint256"},{"internalType":"uint256","name":"v126","type":"uint256"},{"internalType":"uint256","name":"v127","type":"uint256"}],"name":"m5","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v6","type":"address"},{"internalType":"uint256","name":"v4","type":"uint256"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"address payable","name":"v14","type":"address"},{"internalType":"uint256","name":"v55","type":"uint256"},{"internalType":"uint256","name":"v92","type":"uint256"}],"name":"m6","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v6","type":"address"},{"internalType":"uint256","name":"v4","type":"uint256"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"address payable","name":"v14","type":"address"},{"internalType":"uint256","name":"v55","type":"uint256"}],"name":"m7","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v6","type":"address"},{"internalType":"uint256","name":"v4","type":"uint256"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"address payable","name":"v14","type":"address"}],"name":"m8","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v6","type":"address"},{"internalType":"uint256","name":"v4","type":"uint256"},{"internalType":"uint256","name":"v5","type":"uint256"}],"name":"m9","outputs":[],"payable":true,"stateMutability":"payable","type":"function"}]`,
  Bytecode: `0x600060a08181524360c0526040608081905260e0815290209055610ea8806100286000396000f3fe6080604052600436106100865760003560e01c806373929c5e1161005957806373929c5e146101ab5780637a52ccb3146102075780637de71f08146102305780639ccddd3a14610268578063bb91d6e3146102a057610086565b8063050147391461008b578063103d2bab146100cd5780635e6a8eed1461011b578063718b7dd714610163575b600080fd5b6100cb600480360360a08110156100a157600080fd5b508035906001600160a01b03602082013581169160408101359160608201359160800135166102ee565b005b6100cb600480360360e08110156100e357600080fd5b508035906001600160a01b03602082013581169160408101359160608201359160808101359091169060a08101359060c001356103fe565b6100cb600480360360c081101561013157600080fd5b508035906001600160a01b03602082013581169160408101359160608201359160808101359091169060a0013561051e565b6100cb600480360360c081101561017957600080fd5b508035906001600160a01b03602082013581169160408101359160608201359160808101359091169060a00135610655565b6100cb60048036036101208110156101c257600080fd5b508035906001600160a01b03602082013581169160408101359160608201359160808101359091169060a08101359060c08101359060e081013590610100013561076d565b6100cb6004803603606081101561021d57600080fd5b50803590602081013590604001356109fb565b6100cb6004803603608081101561024657600080fd5b508035906001600160a01b036020820135169060408101359060600135610acb565b6100cb6004803603608081101561027e57600080fd5b508035906001600160a01b036020820135169060408101359060600135610bd2565b6100cb600480360360e08110156102b657600080fd5b508035906001600160a01b03602082013581169160408101359160608201359160808101359091169060a08101359060c00135610cd9565b6040805160026020808301919091528183018890526001600160601b0319606088811b821681850152607484018890526094840187905285901b1660b4830152825160a881840301815260c890920190925280519101206000541461035257600080fd5b336001600160a01b0382161461036757600080fd5b600a85014310158015610378575060015b61038157600080fd5b341561038c57600080fd5b6040516001600160a01b03821690303180156108fc02916000818181858888f193505050501580156103c2573d6000803e3d6000fd5b50604080513031815290517f3a6f8023909a26b76d462631fcdf570dbe3740447548e09470d1ad04394a0cec9181900360200190a16000805533ff5b6040805160046020808301919091528183018a90526001600160601b031960608a811b821681850152607484018a90526094840189905287901b1660b483015260c8820185905260e88083018590528351808403909101815261010890920190925280519101206000541461047257600080fd5b336001600160a01b0384161461048757600080fd5b600a87014310158015610498575060015b6104a157600080fd5b34156104ac57600080fd5b6040516001600160a01b03841690303180156108fc02916000818181858888f193505050501580156104e2573d6000803e3d6000fd5b50604080513031815290517fcb3347bd475fd43f41b4bc5bb011db952f2079e6ba9a82ff211988cd7871dba69181900360200190a16000805533ff5b6040805160026020808301919091528183018990526001600160601b0319606089811b821681850152607484018990526094840188905286901b1660b4830152825160a881840301815260c890920190925280519101206000541461058257600080fd5b336001600160a01b0386161461059757600080fd5b600a860143106105a657600080fd5b34156105b157600080fd5b60408051303181526020810183905281517f94dd7e08991b8945fde2d5865f7071e72045b9800e293ff60d29c6960c5a4fb5929181900390910190a160408051600360208083019190915243828401526001600160601b0319606098891b811689840152607483019790975260948201959095529290951b90931660b482015260c8808201939093528351808203909301835260e801909252805191012060005550565b6040805160036020808301919091528183018990526001600160601b0319606089811b821681850152607484018990526094840188905286901b1660b483015260c88083018590528351808403909101815260e89092019092528051910120600054146106c157600080fd5b336001600160a01b038616146106d657600080fd5b600a860143101580156106e7575060015b6106f057600080fd5b34156106fb57600080fd5b6040516001600160a01b03861690303180156108fc02916000818181858888f19350505050158015610731573d6000803e3d6000fd5b50604080513031815290517ffc55d683ac816a7149ebdfa999ae1bcfeeae27c37c9dab64a23f617beed2a0079181900360200190a16000805533ff5b6040805160046020808301919091528183018c90526001600160601b031960608c811b821681850152607484018c9052609484018b905289901b1660b483015260c8820187905260e8808301879052835180840390910181526101089092019092528051910120600054146107e157600080fd5b6107e9610e2b565b336001600160a01b038a16146107fe57600080fd5b600a8a01431061080d57600080fd5b341561081857600080fd5b60408051602080820186905281830185905282518083038401815260609092019092528051910120851461084b57600080fd5b6003821061085857600080fd5b600380831060408301819052908510606083015261087757600061087d565b80606001515b156108995760038460040383018161089157fe5b0681526108c4565b8060400151156108ac57600281526108c4565b8060600151156108bf57600081526108c4565b600181525b8051600214156108f457604051806040016040528089600202815260200160008152508160200181905250610939565b805161092057604051806040016040528060008152602001896002028152508160200181905250610939565b6040805180820190915288815260208082018a90528201525b6020810151516040516001600160a01b038b1691890180156108fc02916000818181858888f19350505050158015610975573d6000803e3d6000fd5b5060208082015101516040516001600160a01b0388169180156108fc02916000818181858888f193505050501580156109b2573d6000803e3d6000fd5b5060408051303181526020810185905280820184905290517f3c3023cc427ae7f284b643c954c1a90afba24284d594cded84550e2316e830f49181900360600190a16000805533ff5b6040805160006020808301829052828401879052835180840385018152606090930190935281519190920120905414610a3357600080fd5b3481830114610a4157600080fd5b60408051303181526020810184905280820183905290517f219cc811755104876269c7553666684eaaeecb90b6a7ffc6fdd5068140059b8e9181900360600190a1604080516001602080830191909152438284015233606090811b9083015260748201949094526094808201939093528151808203909301835260b4019052805191012060005550565b604080516001602080830191909152818301879052606086811b6001600160601b031916908301526074820185905260948083018590528351808403909101815260b4909201909252805191012060005414610b2657600080fd5b336001600160a01b03841614610b3b57600080fd5b600a84014310158015610b4c575060015b610b5557600080fd5b3415610b6057600080fd5b6040516001600160a01b03841690303180156108fc02916000818181858888f19350505050158015610b96573d6000803e3d6000fd5b50604080513031815290517fc92018b4e91e597d736654f7b1d2ec034c5fec5920e2cfe22e15b4ddcdf5e18a9181900360200190a16000805533ff5b604080516001602080830191909152818301879052606086811b6001600160601b031916908301526074820185905260948083018590528351808403909101815260b4909201909252805191012060005414610c2d57600080fd5b600a84014310610c3c57600080fd5b348214610c4857600080fd5b604080513031815290517ff04f5fc87a72102f7c0b228f8bbaf9b9aa7a2b5dc295c86538fdde91e95866e99181900360200190a160408051600260208083019190915243828401526001600160601b0319606096871b1686830152607482019490945260948101929092523390931b60b4820152825160a881830301815260c8909101909252815191012060005550565b6040805160036020808301919091528183018a90526001600160601b031960608a811b821681850152607484018a90526094840189905287901b1660b483015260c88083018690528351808403909101815260e8909201909252805191012060005414610d4557600080fd5b336001600160a01b03841614610d5a57600080fd5b600a87014310610d6957600080fd5b3415610d7457600080fd5b60038110610d8157600080fd5b60408051303181526020810183905281517fb71d350b59ceca5c6544e5367d61ca8cae3e36b25f8d900743d063dff3d6508b929181900390910190a160408051600460208083019190915243828401526001600160601b03196060998a1b81168a840152607483019890985260948201969096529390961b90941660b483015260c882015260e8808201939093528351808203909301835261010801909252805191012060005550565b604051806080016040528060008152602001610e45610e59565b815260006020820181905260409091015290565b60405180604001604052806000815260200160008152509056fea265627a7a723158200320712430eab91b375cdb2a680a758a0fc332ccbd56119aad59c20c807e130064736f6c634300050c0032` };