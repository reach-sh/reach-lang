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
    
    const txn3 = await ctc.sendrecv('A', 10, 0, [v6, v4, v5], 0, false, null);
    const [] = txn3.data;
    const v18 = txn3.value;
    const v19 = stdlib.eq(0, v18);
    stdlib.assert(v19);
    stdlib.assert(true);
    stdlib.protect(stdlib.T_Null, await interact.endsWith('Bob quits'));
    
    return; }
  else {
    const [] = txn2.data;
    const v13 = txn2.from;
    const v14 = txn2.value;
    const v15 = stdlib.eq(v4, v14);
    stdlib.assert(v15);
    let v32 = 0;
    let v33 = 1;
    while ((() => {
      const v57 = stdlib.eq(v33, 1);
      
      return v57; })()) {
      let v60;
      const v61 = stdlib.protect(stdlib.T_Bytes, await interact.getHand());
      const v62 = stdlib.bytes_eq(v61, 'ROCK');
      const v63 = stdlib.bytes_eq(v61, 'PAPER');
      const v64 = stdlib.bytes_eq(v61, 'SCISSORS');
      const v66 = v62 ? true : v63;
      const v68 = v66 ? true : v64;
      stdlib.assert(v68);
      if (v62) {
        v60 = 0;
         }
      else {
        if (v63) {
          v60 = 1;
           }
        else {
          v60 = 2;
           }
         }
      const v76 = stdlib.protect(stdlib.T_UInt256, await interact.random());
      const v77 = stdlib.keccak256(v76, v60);
      stdlib.protect(stdlib.T_Null, await interact.commits());
      
      
      const txn3 = await ctc.sendrecv('A', 4, 1, [v6, v4, v5, v13, v32, v77], 0, 10, null);
      if (txn3.didTimeout) {
        const txn4 = await ctc.recv('A', 9, 0, false);
        const [] = txn4.data;
        const v84 = txn4.value;
        const v85 = stdlib.eq(0, v84);
        stdlib.assert(v85);
        stdlib.assert(true);
        stdlib.protect(stdlib.T_Null, await interact.endsWith('Alice quits'));
        
        return; }
      else {
        const [v79] = txn3.data;
        const v80 = txn3.value;
        const v81 = stdlib.eq(0, v80);
        stdlib.assert(v81);
        const txn4 = await ctc.recv('A', 5, 1, 10);
        if (txn4.didTimeout) {
          
          const txn5 = await ctc.sendrecv('A', 8, 0, [v6, v4, v5, v13, v79, v32], 0, false, null);
          const [] = txn5.data;
          const v121 = txn5.value;
          const v122 = stdlib.eq(0, v121);
          stdlib.assert(v122);
          stdlib.assert(true);
          stdlib.protect(stdlib.T_Null, await interact.endsWith('Bob quits'));
          
          return; }
        else {
          const [v116] = txn4.data;
          const v117 = txn4.value;
          const v118 = stdlib.eq(0, v117);
          stdlib.assert(v118);
          const v136 = stdlib.le(0, v116);
          const v137 = stdlib.lt(v116, 3);
          const v139 = v136 ? v137 : false;
          stdlib.assert(v139);
          let v141;
          const v143 = stdlib.le(0, v116);
          const v144 = stdlib.lt(v116, 3);
          const v146 = v143 ? v144 : false;
          stdlib.assert(v146);
          const v147 = stdlib.eq(v116, 0);
          if (v147) {
            v141 = 'ROCK';
             }
          else {
            const v148 = stdlib.eq(v116, 1);
            if (v148) {
              v141 = 'PAPER';
               }
            else {
              v141 = 'SCISSORS';
               }
             }
          stdlib.protect(stdlib.T_Null, await interact.reveals(v141));
          
          
          const txn5 = await ctc.sendrecv('A', 6, 2, [v6, v4, v5, v13, v79, v116, v32, v76, v60], 0, 10, null);
          if (txn5.didTimeout) {
            const txn6 = await ctc.recv('A', 7, 0, false);
            const [] = txn6.data;
            const v156 = txn6.value;
            const v157 = stdlib.eq(0, v156);
            stdlib.assert(v157);
            stdlib.assert(true);
            stdlib.protect(stdlib.T_Null, await interact.endsWith('Alice quits'));
            
            return; }
          else {
            const [v150, v151] = txn5.data;
            const v152 = txn5.value;
            const v153 = stdlib.eq(0, v152);
            stdlib.assert(v153);
            const v171 = stdlib.keccak256(v150, v151);
            const v172 = stdlib.eq(v79, v171);
            stdlib.assert(v172);
            const v174 = stdlib.le(0, v151);
            const v175 = stdlib.lt(v151, 3);
            const v177 = v174 ? v175 : false;
            stdlib.assert(v177);
            let v179;
            const v181 = stdlib.le(0, v151);
            const v182 = stdlib.lt(v151, 3);
            const v184 = v181 ? v182 : false;
            const v186 = stdlib.le(0, v116);
            const v187 = stdlib.lt(v116, 3);
            const v189 = v186 ? v187 : false;
            const v191 = v184 ? v189 : false;
            if (v191) {
              const v192 = stdlib.sub(4, v116);
              const v193 = stdlib.add(v151, v192);
              const v194 = stdlib.mod(v193, 3);
              v179 = v194;
               }
            else {
              if (v184) {
                v179 = 2;
                 }
              else {
                if (v189) {
                  v179 = 0;
                   }
                else {
                  v179 = 1;
                   }
                 }
               }
            const v252 = stdlib.add(1, v32);
            v32 = v252;
            v33 = v179;
            continue; } } } }
    let v257;
    const v258 = stdlib.eq(v33, 2);
    if (v258) {
      const v259 = stdlib.mul(2, v4);
      v257 = [v259, 0];
       }
    else {
      const v260 = stdlib.eq(v33, 0);
      if (v260) {
        const v261 = stdlib.mul(2, v4);
        v257 = [0, v261];
         }
      else {
        v257 = [v4, v4];
         }
       }
    let v268;
    const v270 = stdlib.le(0, v33);
    const v271 = stdlib.lt(v33, 5);
    const v273 = v270 ? v271 : false;
    stdlib.assert(v273);
    const v274 = stdlib.eq(v33, 0);
    if (v274) {
      v268 = 'Bob wins';
       }
    else {
      const v275 = stdlib.eq(v33, 1);
      if (v275) {
        v268 = 'Draw';
         }
      else {
        const v276 = stdlib.eq(v33, 2);
        if (v276) {
          v268 = 'Alice wins';
           }
        else {
          const v277 = stdlib.eq(v33, 3);
          if (v277) {
            v268 = 'Alice quits';
             }
          else {
            v268 = 'Bob quits';
             }
           }
         }
       }
    stdlib.protect(stdlib.T_Null, await interact.endsWith(v268));
    
    return; } }
export async function B(stdlib, ctc, interact) {
  const txn0 = { balance: 0, value: 0 };
  const txn1 = await ctc.recv('B', 1, 2, false);
  const [v4, v5] = txn1.data;
  const v6 = txn1.from;
  const v8 = stdlib.add(v4, v5);
  const v9 = txn1.value;
  const v10 = stdlib.eq(v8, v9);
  stdlib.assert(v10);
  stdlib.protect(stdlib.T_Null, await interact.acceptParams(v4, v5));
  
  
  const txn2 = await ctc.sendrecv('B', 2, 0, [v6, v4, v5], v4, 10, null);
  if (txn2.didTimeout) {
    const txn3 = await ctc.recv('B', 10, 0, false);
    const [] = txn3.data;
    const v18 = txn3.value;
    const v19 = stdlib.eq(0, v18);
    stdlib.assert(v19);
    stdlib.assert(true);
    stdlib.protect(stdlib.T_Null, await interact.endsWith('Bob quits'));
    
    return; }
  else {
    const [] = txn2.data;
    const v13 = txn2.from;
    const v14 = txn2.value;
    const v15 = stdlib.eq(v4, v14);
    stdlib.assert(v15);
    let v32 = 0;
    let v33 = 1;
    while ((() => {
      const v57 = stdlib.eq(v33, 1);
      
      return v57; })()) {
      const txn3 = await ctc.recv('B', 4, 1, 10);
      if (txn3.didTimeout) {
        
        const txn4 = await ctc.sendrecv('B', 9, 0, [v6, v4, v5, v13, v32], 0, false, null);
        const [] = txn4.data;
        const v84 = txn4.value;
        const v85 = stdlib.eq(0, v84);
        stdlib.assert(v85);
        stdlib.assert(true);
        stdlib.protect(stdlib.T_Null, await interact.endsWith('Alice quits'));
        
        return; }
      else {
        const [v79] = txn3.data;
        const v80 = txn3.value;
        const v81 = stdlib.eq(0, v80);
        stdlib.assert(v81);
        let v100;
        const v101 = stdlib.protect(stdlib.T_Bytes, await interact.getHand());
        const v102 = stdlib.bytes_eq(v101, 'ROCK');
        const v103 = stdlib.bytes_eq(v101, 'PAPER');
        const v104 = stdlib.bytes_eq(v101, 'SCISSORS');
        const v106 = v102 ? true : v103;
        const v108 = v106 ? true : v104;
        stdlib.assert(v108);
        if (v102) {
          v100 = 0;
           }
        else {
          if (v103) {
            v100 = 1;
             }
          else {
            v100 = 2;
             }
           }
        stdlib.protect(stdlib.T_Null, await interact.shows());
        
        
        const txn4 = await ctc.sendrecv('B', 5, 1, [v6, v4, v5, v13, v79, v32, v100], 0, 10, null);
        if (txn4.didTimeout) {
          const txn5 = await ctc.recv('B', 8, 0, false);
          const [] = txn5.data;
          const v121 = txn5.value;
          const v122 = stdlib.eq(0, v121);
          stdlib.assert(v122);
          stdlib.assert(true);
          stdlib.protect(stdlib.T_Null, await interact.endsWith('Bob quits'));
          
          return; }
        else {
          const [v116] = txn4.data;
          const v117 = txn4.value;
          const v118 = stdlib.eq(0, v117);
          stdlib.assert(v118);
          const v136 = stdlib.le(0, v116);
          const v137 = stdlib.lt(v116, 3);
          const v139 = v136 ? v137 : false;
          stdlib.assert(v139);
          const txn5 = await ctc.recv('B', 6, 2, 10);
          if (txn5.didTimeout) {
            
            const txn6 = await ctc.sendrecv('B', 7, 0, [v6, v4, v5, v13, v79, v116, v32], 0, false, null);
            const [] = txn6.data;
            const v156 = txn6.value;
            const v157 = stdlib.eq(0, v156);
            stdlib.assert(v157);
            stdlib.assert(true);
            stdlib.protect(stdlib.T_Null, await interact.endsWith('Alice quits'));
            
            return; }
          else {
            const [v150, v151] = txn5.data;
            const v152 = txn5.value;
            const v153 = stdlib.eq(0, v152);
            stdlib.assert(v153);
            const v171 = stdlib.keccak256(v150, v151);
            const v172 = stdlib.eq(v79, v171);
            stdlib.assert(v172);
            const v174 = stdlib.le(0, v151);
            const v175 = stdlib.lt(v151, 3);
            const v177 = v174 ? v175 : false;
            stdlib.assert(v177);
            let v179;
            const v181 = stdlib.le(0, v151);
            const v182 = stdlib.lt(v151, 3);
            const v184 = v181 ? v182 : false;
            const v186 = stdlib.le(0, v116);
            const v187 = stdlib.lt(v116, 3);
            const v189 = v186 ? v187 : false;
            const v191 = v184 ? v189 : false;
            if (v191) {
              const v192 = stdlib.sub(4, v116);
              const v193 = stdlib.add(v151, v192);
              const v194 = stdlib.mod(v193, 3);
              v179 = v194;
               }
            else {
              if (v184) {
                v179 = 2;
                 }
              else {
                if (v189) {
                  v179 = 0;
                   }
                else {
                  v179 = 1;
                   }
                 }
               }
            const v252 = stdlib.add(1, v32);
            v32 = v252;
            v33 = v179;
            continue; } } } }
    let v257;
    const v258 = stdlib.eq(v33, 2);
    if (v258) {
      const v259 = stdlib.mul(2, v4);
      v257 = [v259, 0];
       }
    else {
      const v260 = stdlib.eq(v33, 0);
      if (v260) {
        const v261 = stdlib.mul(2, v4);
        v257 = [0, v261];
         }
      else {
        v257 = [v4, v4];
         }
       }
    let v280;
    const v282 = stdlib.le(0, v33);
    const v283 = stdlib.lt(v33, 5);
    const v285 = v282 ? v283 : false;
    stdlib.assert(v285);
    const v286 = stdlib.eq(v33, 0);
    if (v286) {
      v280 = 'Bob wins';
       }
    else {
      const v287 = stdlib.eq(v33, 1);
      if (v287) {
        v280 = 'Draw';
         }
      else {
        const v288 = stdlib.eq(v33, 2);
        if (v288) {
          v280 = 'Alice wins';
           }
        else {
          const v289 = stdlib.eq(v33, 3);
          if (v289) {
            v280 = 'Alice quits';
             }
          else {
            v280 = 'Bob quits';
             }
           }
         }
       }
    stdlib.protect(stdlib.T_Null, await interact.endsWith(v280));
    
    return; } }
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
    const txn3 = await ctc.recv('O', 10, 0, false);
    const [] = txn3.data;
    const v18 = txn3.value;
    const v19 = stdlib.eq(0, v18);
    stdlib.assert(v19);
    return; }
  else {
    const [] = txn2.data;
    const v13 = txn2.from;
    const v14 = txn2.value;
    const v15 = stdlib.eq(v4, v14);
    stdlib.assert(v15);
    let v32 = 0;
    let v33 = 1;
    while ((() => {
      const v57 = stdlib.eq(v33, 1);
      
      return v57; })()) {
      const txn3 = await ctc.recv('O', 4, 1, 10);
      if (txn3.didTimeout) {
        const txn4 = await ctc.recv('O', 9, 0, false);
        const [] = txn4.data;
        const v84 = txn4.value;
        const v85 = stdlib.eq(0, v84);
        stdlib.assert(v85);
        return; }
      else {
        const [v79] = txn3.data;
        const v80 = txn3.value;
        const v81 = stdlib.eq(0, v80);
        stdlib.assert(v81);
        const txn4 = await ctc.recv('O', 5, 1, 10);
        if (txn4.didTimeout) {
          const txn5 = await ctc.recv('O', 8, 0, false);
          const [] = txn5.data;
          const v121 = txn5.value;
          const v122 = stdlib.eq(0, v121);
          stdlib.assert(v122);
          return; }
        else {
          const [v116] = txn4.data;
          const v117 = txn4.value;
          const v118 = stdlib.eq(0, v117);
          stdlib.assert(v118);
          const v136 = stdlib.le(0, v116);
          const v137 = stdlib.lt(v116, 3);
          const v139 = v136 ? v137 : false;
          stdlib.assert(v139);
          const txn5 = await ctc.recv('O', 6, 2, 10);
          if (txn5.didTimeout) {
            const txn6 = await ctc.recv('O', 7, 0, false);
            const [] = txn6.data;
            const v156 = txn6.value;
            const v157 = stdlib.eq(0, v156);
            stdlib.assert(v157);
            return; }
          else {
            const [v150, v151] = txn5.data;
            const v152 = txn5.value;
            const v153 = stdlib.eq(0, v152);
            stdlib.assert(v153);
            const v171 = stdlib.keccak256(v150, v151);
            const v172 = stdlib.eq(v79, v171);
            stdlib.assert(v172);
            const v174 = stdlib.le(0, v151);
            const v175 = stdlib.lt(v151, 3);
            const v177 = v174 ? v175 : false;
            stdlib.assert(v177);
            let v179;
            const v181 = stdlib.le(0, v151);
            const v182 = stdlib.lt(v151, 3);
            const v184 = v181 ? v182 : false;
            const v186 = stdlib.le(0, v116);
            const v187 = stdlib.lt(v116, 3);
            const v189 = v186 ? v187 : false;
            const v191 = v184 ? v189 : false;
            if (v191) {
              const v192 = stdlib.sub(4, v116);
              const v193 = stdlib.add(v151, v192);
              const v194 = stdlib.mod(v193, 3);
              v179 = v194;
               }
            else {
              if (v184) {
                v179 = 2;
                 }
              else {
                if (v189) {
                  v179 = 0;
                   }
                else {
                  v179 = 1;
                   }
                 }
               }
            const v252 = stdlib.add(1, v32);
            v32 = v252;
            v33 = v179;
            continue; } } } }
    let v257;
    const v258 = stdlib.eq(v33, 2);
    if (v258) {
      const v259 = stdlib.mul(2, v4);
      v257 = [v259, 0];
       }
    else {
      const v260 = stdlib.eq(v33, 0);
      if (v260) {
        const v261 = stdlib.mul(2, v4);
        v257 = [0, v261];
         }
      else {
        v257 = [v4, v4];
         }
       }
    return; } }

export const ETH = {
  ABI: `[{"inputs":[],"payable":true,"stateMutability":"payable","type":"constructor"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v4","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v5","type":"uint256"}],"name":"e1","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e10","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e2","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v79","type":"uint256"}],"name":"e4","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v116","type":"uint256"}],"name":"e5","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v150","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v151","type":"uint256"}],"name":"e6","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e7","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e8","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e9","type":"event"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"uint256","name":"v4","type":"uint256"},{"internalType":"uint256","name":"v5","type":"uint256"}],"name":"m1","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v6","type":"address"},{"internalType":"uint256","name":"v4","type":"uint256"},{"internalType":"uint256","name":"v5","type":"uint256"}],"name":"m10","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v6","type":"address"},{"internalType":"uint256","name":"v4","type":"uint256"},{"internalType":"uint256","name":"v5","type":"uint256"}],"name":"m2","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v6","type":"address"},{"internalType":"uint256","name":"v4","type":"uint256"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"address payable","name":"v13","type":"address"},{"internalType":"uint256","name":"v32","type":"uint256"},{"internalType":"uint256","name":"v79","type":"uint256"}],"name":"m4","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v6","type":"address"},{"internalType":"uint256","name":"v4","type":"uint256"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"address payable","name":"v13","type":"address"},{"internalType":"uint256","name":"v79","type":"uint256"},{"internalType":"uint256","name":"v32","type":"uint256"},{"internalType":"uint256","name":"v116","type":"uint256"}],"name":"m5","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v6","type":"address"},{"internalType":"uint256","name":"v4","type":"uint256"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"address payable","name":"v13","type":"address"},{"internalType":"uint256","name":"v79","type":"uint256"},{"internalType":"uint256","name":"v116","type":"uint256"},{"internalType":"uint256","name":"v32","type":"uint256"},{"internalType":"uint256","name":"v150","type":"uint256"},{"internalType":"uint256","name":"v151","type":"uint256"}],"name":"m6","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v6","type":"address"},{"internalType":"uint256","name":"v4","type":"uint256"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"address payable","name":"v13","type":"address"},{"internalType":"uint256","name":"v79","type":"uint256"},{"internalType":"uint256","name":"v116","type":"uint256"},{"internalType":"uint256","name":"v32","type":"uint256"}],"name":"m7","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v6","type":"address"},{"internalType":"uint256","name":"v4","type":"uint256"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"address payable","name":"v13","type":"address"},{"internalType":"uint256","name":"v79","type":"uint256"},{"internalType":"uint256","name":"v32","type":"uint256"}],"name":"m8","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v6","type":"address"},{"internalType":"uint256","name":"v4","type":"uint256"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"address payable","name":"v13","type":"address"},{"internalType":"uint256","name":"v32","type":"uint256"}],"name":"m9","outputs":[],"payable":true,"stateMutability":"payable","type":"function"}]`,
  Bytecode: `0x600060a08181524360c0526040608081905260e0815290209055610f62806100286000396000f3fe6080604052600436106100865760003560e01c80637a52ccb3116100595780637a52ccb3146101db5780639337cbb3146102045780639ccddd3a1461023c578063b6ce11cf14610274578063bb91d6e3146102c957610086565b806315e74bc71461008b57806352ecf27a146100e257806368e59173146101455780637984da481461018d575b600080fd5b6100e060048036036101008110156100a257600080fd5b508035906001600160a01b03602082013581169160408101359160608201359160808101359091169060a08101359060c08101359060e00135610317565b005b6100e060048036036101408110156100f957600080fd5b508035906001600160a01b03602082013581169160408101359160608201359160808101359091169060a08101359060c08101359060e08101359061010081013590610120013561043f565b6100e0600480360360c081101561015b57600080fd5b508035906001600160a01b03602082013581169160408101359160608201359160808101359091169060a00135610601565b6100e0600480360360e08110156101a357600080fd5b508035906001600160a01b03602082013581169160408101359160608201359160808101359091169060a08101359060c00135610719565b6100e0600480360360608110156101f157600080fd5b5080359060208101359060400135610839565b6100e06004803603608081101561021a57600080fd5b508035906001600160a01b036020820135169060408101359060600135610909565b6100e06004803603608081101561025257600080fd5b508035906001600160a01b036020820135169060408101359060600135610a10565b6100e0600480360361010081101561028b57600080fd5b508035906001600160a01b03602082013581169160408101359160608201359160808101359091169060a08101359060c08101359060e00135610ad0565b6100e0600480360360e08110156102df57600080fd5b508035906001600160a01b03602082013581169160408101359160608201359160808101359091169060a08101359060c00135610c36565b6040805160056020808301919091528183018b90526001600160601b031960608b811b821681850152607484018b9052609484018a905288901b1660b483015260c8820186905260e882018590526101088083018590528351808403909101815261012890920190925280519101206000541461039357600080fd5b336001600160a01b038516146103a857600080fd5b600a880143101580156103b9575060015b6103c257600080fd5b34156103cd57600080fd5b6040516001600160a01b03851690303180156108fc02916000818181858888f19350505050158015610403573d6000803e3d6000fd5b50604080513031815290517ffc55d683ac816a7149ebdfa999ae1bcfeeae27c37c9dab64a23f617beed2a0079181900360200190a16000805533ff5b6040805160056020808301919091528183018d90526001600160601b031960608d811b821681850152607484018d9052609484018c90528a901b1660b483015260c8820188905260e88201879052610108808301879052835180840390910181526101289092019092528051910120600054146104bb57600080fd5b6104c3610edb565b336001600160a01b038b16146104d857600080fd5b600a8b0143106104e757600080fd5b34156104f257600080fd5b60408051602080820186905281830185905282518083038401815260609092019092528051910120861461052557600080fd5b6003821061053257600080fd5b6003808310602083018190529086106040830152610551576000610557565b80604001515b156105735760038560040383018161056b57fe5b06815261059e565b806020015115610586576002815261059e565b806040015115610599576000815261059e565b600181525b60408051303181526020810185905280820184905290517f3a17d070a78674d0fecc57e42515183089cfc8c61973a362a95c57f4d000ea849181900360600190a16105f48a8a8a8a886001018660000151610d7e565b5050505050505050505050565b6040805160036020808301919091528183018990526001600160601b0319606089811b821681850152607484018990526094840188905286901b1660b483015260c88083018590528351808403909101815260e890920190925280519101206000541461066d57600080fd5b336001600160a01b0383161461068257600080fd5b600a86014310158015610693575060015b61069c57600080fd5b34156106a757600080fd5b6040516001600160a01b03831690303180156108fc02916000818181858888f193505050501580156106dd573d6000803e3d6000fd5b50604080513031815290517fc92018b4e91e597d736654f7b1d2ec034c5fec5920e2cfe22e15b4ddcdf5e18a9181900360200190a16000805533ff5b6040805160046020808301919091528183018a90526001600160601b031960608a811b821681850152607484018a90526094840189905287901b1660b483015260c8820185905260e88083018590528351808403909101815261010890920190925280519101206000541461078d57600080fd5b336001600160a01b038716146107a257600080fd5b600a870143101580156107b3575060015b6107bc57600080fd5b34156107c757600080fd5b6040516001600160a01b03871690303180156108fc02916000818181858888f193505050501580156107fd573d6000803e3d6000fd5b50604080513031815290517f3a6f8023909a26b76d462631fcdf570dbe3740447548e09470d1ad04394a0cec9181900360200190a16000805533ff5b604080516000602080830182905282840187905283518084038501815260609093019093528151919092012090541461087157600080fd5b348183011461087f57600080fd5b60408051303181526020810184905280820183905290517f219cc811755104876269c7553666684eaaeecb90b6a7ffc6fdd5068140059b8e9181900360600190a1604080516001602080830191909152438284015233606090811b9083015260748201949094526094808201939093528151808203909301835260b4019052805191012060005550565b604080516001602080830191909152818301879052606086811b6001600160601b031916908301526074820185905260948083018590528351808403909101815260b490920190925280519101206000541461096457600080fd5b336001600160a01b0384161461097957600080fd5b600a8401431015801561098a575060015b61099357600080fd5b341561099e57600080fd5b6040516001600160a01b03841690303180156108fc02916000818181858888f193505050501580156109d4573d6000803e3d6000fd5b50604080513031815290517f9bf9cf9ae88051b33b19923b1c1cf36013b840c9975de29305d444b55d83c6bd9181900360200190a16000805533ff5b604080516001602080830191909152818301879052606086811b6001600160601b031916908301526074820185905260948083018590528351808403909101815260b4909201909252805191012060005414610a6b57600080fd5b600a84014310610a7a57600080fd5b348214610a8657600080fd5b604080513031815290517ff04f5fc87a72102f7c0b228f8bbaf9b9aa7a2b5dc295c86538fdde91e95866e99181900360200190a1610aca8383833360006001610d7e565b50505050565b6040805160046020808301919091528183018b90526001600160601b031960608b811b821681850152607484018b9052609484018a905288901b1660b483015260c8820186905260e880830186905283518084039091018152610108909201909252805191012060005414610b4457600080fd5b336001600160a01b03851614610b5957600080fd5b600a88014310610b6857600080fd5b3415610b7357600080fd5b60038110610b8057600080fd5b60408051303181526020810183905281517fabf482d77b67111a4971bb96fe81961f83ba459eb1d8fa9f78b6908251aeef1a929181900390910190a160408051600560208083019190915243828401526001600160601b031960609a8b1b81168b840152607483019990995260948201979097529490971b90951660b484015260c883019190915260e8820193909352610108808201939093528351808203909301835261012801909252805191012060005550565b6040805160036020808301919091528183018a90526001600160601b031960608a811b821681850152607484018a90526094840189905287901b1660b483015260c88083018690528351808403909101815260e8909201909252805191012060005414610ca257600080fd5b336001600160a01b03871614610cb757600080fd5b600a87014310610cc657600080fd5b3415610cd157600080fd5b60408051303181526020810183905281517fb71d350b59ceca5c6544e5367d61ca8cae3e36b25f8d900743d063dff3d6508b929181900390910190a160408051600460208083019190915243828401526001600160601b03196060998a1b81168a840152607483019890985260948201969096529390961b90941660b483015260c882019390935260e8808201939093528351808203909301835261010801909252805191012060005550565b610d86610efb565b6001821415610df55760408051600360208083019190915243828401526001600160601b031960608b811b821681850152607484018b9052609484018a905288901b1660b483015260c88083018790528351808403909101815260e89092019092528051910120600055610ed2565b6002821415610e1c5760408051808201909152600287028152600060208201528152610e56565b81610e3f5760408051808201909152600081526002870260208201528152610e56565b604080518082019091528681526020810187905281525b8051516040516001600160a01b03891691870180156108fc02916000818181858888f19350505050158015610e8f573d6000803e3d6000fd5b508051602001516040516001600160a01b0386169180156108fc02916000818181858888f19350505050158015610eca573d6000803e3d6000fd5b506000805533ff5b50505050505050565b604080516060810182526000808252602082018190529181019190915290565b6040518060200160405280610f0e610f13565b905290565b60405180604001604052806000815260200160008152509056fea265627a7a72315820fae7f9375484d39ae43c008b8087ee976542e54cc38c2c667504dc97e915192164736f6c634300050c0032` };