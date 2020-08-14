// Automatically generated with Reach 0.1.0

export async function A(stdlib, ctc, interact) {
  const txn0 = { balance: 0, value: 0 };
  const v2 = stdlib.protect(stdlib.T_Tuple([stdlib.T_UInt256, stdlib.T_UInt256]), await interact.getParams());
  const v3 = v2[0];
  const v4 = v2[1];
  
  const v8 = stdlib.add(v3, v4);
  
  const txn1 = await ctc.sendrecv('A', 1, 2, [v3, v4], v8, false, null);
  const [v5, v6] = txn1.data;
  const v7 = txn1.from;
  const v9 = stdlib.add(v5, v6);
  const v10 = txn1.value;
  const v11 = stdlib.eq(v9, v10);
  stdlib.assert(v11);
  const txn2 = await ctc.recv('A', 2, 0, 10);
  if (txn2.didTimeout) {
    
    const txn3 = await ctc.sendrecv('A', 11, 0, [v7, v5, v6], 0, false, null);
    const [] = txn3.data;
    const v20 = txn3.value;
    const v21 = stdlib.eq(0, v20);
    stdlib.assert(v21);
    stdlib.assert(true);
    stdlib.protect(stdlib.T_Null, await interact.endsWith('Bob quits'));
    
    return; }
  else {
    const [] = txn2.data;
    const v15 = txn2.from;
    const v16 = txn2.value;
    const v17 = stdlib.eq(v5, v16);
    stdlib.assert(v17);
    stdlib.protect(stdlib.T_Null, await interact.partnerIs(v15));
    
    let v39;
    const v40 = stdlib.protect(stdlib.T_Bytes, await interact.getHand());
    const v41 = stdlib.bytes_eq(v40, 'ROCK');
    const v42 = stdlib.bytes_eq(v40, 'PAPER');
    const v43 = stdlib.bytes_eq(v40, 'SCISSORS');
    const v45 = v41 ? true : v42;
    const v47 = v45 ? true : v43;
    stdlib.assert(v47);
    if (v41) {
      v39 = 0;
       }
    else {
      if (v42) {
        v39 = 1;
         }
      else {
        v39 = 2;
         }
       }
    const v55 = stdlib.protect(stdlib.T_UInt256, await interact.random());
    const v56 = stdlib.keccak256(v55, v39);
    stdlib.protect(stdlib.T_Null, await interact.commits());
    
    
    const txn3 = await ctc.sendrecv('A', 3, 1, [v7, v5, v6, v15, v56], 0, 10, null);
    if (txn3.didTimeout) {
      const txn4 = await ctc.recv('A', 10, 0, false);
      const [] = txn4.data;
      const v63 = txn4.value;
      const v64 = stdlib.eq(0, v63);
      stdlib.assert(v64);
      stdlib.assert(true);
      stdlib.protect(stdlib.T_Null, await interact.endsWith('Alice quits'));
      
      return; }
    else {
      const [v58] = txn3.data;
      const v59 = txn3.value;
      const v60 = stdlib.eq(0, v59);
      stdlib.assert(v60);
      const txn4 = await ctc.recv('A', 4, 1, 10);
      if (txn4.didTimeout) {
        
        const txn5 = await ctc.sendrecv('A', 9, 0, [v7, v5, v6, v15, v58], 0, false, null);
        const [] = txn5.data;
        const v100 = txn5.value;
        const v101 = stdlib.eq(0, v100);
        stdlib.assert(v101);
        stdlib.assert(true);
        stdlib.protect(stdlib.T_Null, await interact.endsWith('Bob quits'));
        
        return; }
      else {
        const [v95] = txn4.data;
        const v96 = txn4.value;
        const v97 = stdlib.eq(0, v96);
        stdlib.assert(v97);
        const v115 = stdlib.le(0, v95);
        const v116 = stdlib.lt(v95, 3);
        const v118 = v115 ? v116 : false;
        stdlib.assert(v118);
        let v120;
        const v122 = stdlib.le(0, v95);
        const v123 = stdlib.lt(v95, 3);
        const v125 = v122 ? v123 : false;
        stdlib.assert(v125);
        const v126 = stdlib.eq(v95, 0);
        if (v126) {
          v120 = 'ROCK';
           }
        else {
          const v127 = stdlib.eq(v95, 1);
          if (v127) {
            v120 = 'PAPER';
             }
          else {
            v120 = 'SCISSORS';
             }
           }
        stdlib.protect(stdlib.T_Null, await interact.reveals(v120));
        
        
        const txn5 = await ctc.sendrecv('A', 5, 2, [v7, v5, v6, v15, v58, v95, v55, v39], 0, 10, null);
        if (txn5.didTimeout) {
          const txn6 = await ctc.recv('A', 8, 0, false);
          const [] = txn6.data;
          const v135 = txn6.value;
          const v136 = stdlib.eq(0, v135);
          stdlib.assert(v136);
          stdlib.assert(true);
          stdlib.protect(stdlib.T_Null, await interact.endsWith('Alice quits'));
          
          return; }
        else {
          const [v129, v130] = txn5.data;
          const v131 = txn5.value;
          const v132 = stdlib.eq(0, v131);
          stdlib.assert(v132);
          const v150 = stdlib.keccak256(v129, v130);
          const v151 = stdlib.eq(v58, v150);
          stdlib.assert(v151);
          const v153 = stdlib.le(0, v130);
          const v154 = stdlib.lt(v130, 3);
          const v156 = v153 ? v154 : false;
          stdlib.assert(v156);
          let v158;
          const v160 = stdlib.le(0, v130);
          const v161 = stdlib.lt(v130, 3);
          const v163 = v160 ? v161 : false;
          const v165 = stdlib.le(0, v95);
          const v166 = stdlib.lt(v95, 3);
          const v168 = v165 ? v166 : false;
          const v170 = v163 ? v168 : false;
          if (v170) {
            const v171 = stdlib.sub(4, v95);
            const v172 = stdlib.add(v130, v171);
            const v173 = stdlib.mod(v172, 3);
            v158 = v173;
             }
          else {
            if (v163) {
              v158 = 2;
               }
            else {
              if (v168) {
                v158 = 0;
                 }
              else {
                v158 = 1;
                 }
               }
             }
          const txn6 = await ctc.recv('A', 6, 0, 10);
          if (txn6.didTimeout) {
            
            const txn7 = await ctc.sendrecv('A', 7, 0, [v158, v7, v5, v6, v15], 0, false, null);
            const [] = txn7.data;
            const v235 = txn7.value;
            const v236 = stdlib.eq(0, v235);
            stdlib.assert(v236);
            stdlib.assert(true);
            stdlib.protect(stdlib.T_Null, await interact.endsWith('Bob quits'));
            
            return; }
          else {
            const [] = txn6.data;
            const v231 = txn6.value;
            const v232 = stdlib.eq(0, v231);
            stdlib.assert(v232);
            let v250;
            const v251 = stdlib.eq(v158, 2);
            if (v251) {
              const v252 = stdlib.mul(2, v5);
              v250 = [v252, 0];
               }
            else {
              const v253 = stdlib.eq(v158, 0);
              if (v253) {
                const v254 = stdlib.mul(2, v5);
                v250 = [0, v254];
                 }
              else {
                v250 = [v5, v5];
                 }
               }
            let v261;
            const v263 = stdlib.le(0, v158);
            const v264 = stdlib.lt(v158, 5);
            const v266 = v263 ? v264 : false;
            stdlib.assert(v266);
            const v267 = stdlib.eq(v158, 0);
            if (v267) {
              v261 = 'Bob wins';
               }
            else {
              const v268 = stdlib.eq(v158, 1);
              if (v268) {
                v261 = 'Draw';
                 }
              else {
                const v269 = stdlib.eq(v158, 2);
                if (v269) {
                  v261 = 'Alice wins';
                   }
                else {
                  const v270 = stdlib.eq(v158, 3);
                  if (v270) {
                    v261 = 'Alice quits';
                     }
                  else {
                    v261 = 'Bob quits';
                     }
                   }
                 }
               }
            stdlib.protect(stdlib.T_Null, await interact.endsWith(v261));
            
            return; } } } } } }
export async function B(stdlib, ctc, interact) {
  const txn0 = { balance: 0, value: 0 };
  const txn1 = await ctc.recv('B', 1, 2, false);
  const [v5, v6] = txn1.data;
  const v7 = txn1.from;
  const v9 = stdlib.add(v5, v6);
  const v10 = txn1.value;
  const v11 = stdlib.eq(v9, v10);
  stdlib.assert(v11);
  stdlib.protect(stdlib.T_Null, await interact.partnerIs(v7));
  stdlib.protect(stdlib.T_Null, await interact.acceptParams(v5, v6));
  
  
  const txn2 = await ctc.sendrecv('B', 2, 0, [v7, v5, v6], v5, 10, null);
  if (txn2.didTimeout) {
    const txn3 = await ctc.recv('B', 11, 0, false);
    const [] = txn3.data;
    const v20 = txn3.value;
    const v21 = stdlib.eq(0, v20);
    stdlib.assert(v21);
    stdlib.assert(true);
    stdlib.protect(stdlib.T_Null, await interact.endsWith('Bob quits'));
    
    return; }
  else {
    const [] = txn2.data;
    const v15 = txn2.from;
    const v16 = txn2.value;
    const v17 = stdlib.eq(v5, v16);
    stdlib.assert(v17);
    const txn3 = await ctc.recv('B', 3, 1, 10);
    if (txn3.didTimeout) {
      
      const txn4 = await ctc.sendrecv('B', 10, 0, [v7, v5, v6, v15], 0, false, null);
      const [] = txn4.data;
      const v63 = txn4.value;
      const v64 = stdlib.eq(0, v63);
      stdlib.assert(v64);
      stdlib.assert(true);
      stdlib.protect(stdlib.T_Null, await interact.endsWith('Alice quits'));
      
      return; }
    else {
      const [v58] = txn3.data;
      const v59 = txn3.value;
      const v60 = stdlib.eq(0, v59);
      stdlib.assert(v60);
      let v79;
      const v80 = stdlib.protect(stdlib.T_Bytes, await interact.getHand());
      const v81 = stdlib.bytes_eq(v80, 'ROCK');
      const v82 = stdlib.bytes_eq(v80, 'PAPER');
      const v83 = stdlib.bytes_eq(v80, 'SCISSORS');
      const v85 = v81 ? true : v82;
      const v87 = v85 ? true : v83;
      stdlib.assert(v87);
      if (v81) {
        v79 = 0;
         }
      else {
        if (v82) {
          v79 = 1;
           }
        else {
          v79 = 2;
           }
         }
      stdlib.protect(stdlib.T_Null, await interact.shows());
      
      
      const txn4 = await ctc.sendrecv('B', 4, 1, [v7, v5, v6, v15, v58, v79], 0, 10, null);
      if (txn4.didTimeout) {
        const txn5 = await ctc.recv('B', 9, 0, false);
        const [] = txn5.data;
        const v100 = txn5.value;
        const v101 = stdlib.eq(0, v100);
        stdlib.assert(v101);
        stdlib.assert(true);
        stdlib.protect(stdlib.T_Null, await interact.endsWith('Bob quits'));
        
        return; }
      else {
        const [v95] = txn4.data;
        const v96 = txn4.value;
        const v97 = stdlib.eq(0, v96);
        stdlib.assert(v97);
        const v115 = stdlib.le(0, v95);
        const v116 = stdlib.lt(v95, 3);
        const v118 = v115 ? v116 : false;
        stdlib.assert(v118);
        const txn5 = await ctc.recv('B', 5, 2, 10);
        if (txn5.didTimeout) {
          
          const txn6 = await ctc.sendrecv('B', 8, 0, [v7, v5, v6, v15, v58, v95], 0, false, null);
          const [] = txn6.data;
          const v135 = txn6.value;
          const v136 = stdlib.eq(0, v135);
          stdlib.assert(v136);
          stdlib.assert(true);
          stdlib.protect(stdlib.T_Null, await interact.endsWith('Alice quits'));
          
          return; }
        else {
          const [v129, v130] = txn5.data;
          const v131 = txn5.value;
          const v132 = stdlib.eq(0, v131);
          stdlib.assert(v132);
          const v150 = stdlib.keccak256(v129, v130);
          const v151 = stdlib.eq(v58, v150);
          stdlib.assert(v151);
          const v153 = stdlib.le(0, v130);
          const v154 = stdlib.lt(v130, 3);
          const v156 = v153 ? v154 : false;
          stdlib.assert(v156);
          let v158;
          const v160 = stdlib.le(0, v130);
          const v161 = stdlib.lt(v130, 3);
          const v163 = v160 ? v161 : false;
          const v165 = stdlib.le(0, v95);
          const v166 = stdlib.lt(v95, 3);
          const v168 = v165 ? v166 : false;
          const v170 = v163 ? v168 : false;
          if (v170) {
            const v171 = stdlib.sub(4, v95);
            const v172 = stdlib.add(v130, v171);
            const v173 = stdlib.mod(v172, 3);
            v158 = v173;
             }
          else {
            if (v163) {
              v158 = 2;
               }
            else {
              if (v168) {
                v158 = 0;
                 }
              else {
                v158 = 1;
                 }
               }
             }
          
          const txn6 = await ctc.sendrecv('B', 6, 0, [v158, v7, v5, v6, v15], 0, 10, null);
          if (txn6.didTimeout) {
            const txn7 = await ctc.recv('B', 7, 0, false);
            const [] = txn7.data;
            const v235 = txn7.value;
            const v236 = stdlib.eq(0, v235);
            stdlib.assert(v236);
            stdlib.assert(true);
            stdlib.protect(stdlib.T_Null, await interact.endsWith('Bob quits'));
            
            return; }
          else {
            const [] = txn6.data;
            const v231 = txn6.value;
            const v232 = stdlib.eq(0, v231);
            stdlib.assert(v232);
            let v250;
            const v251 = stdlib.eq(v158, 2);
            if (v251) {
              const v252 = stdlib.mul(2, v5);
              v250 = [v252, 0];
               }
            else {
              const v253 = stdlib.eq(v158, 0);
              if (v253) {
                const v254 = stdlib.mul(2, v5);
                v250 = [0, v254];
                 }
              else {
                v250 = [v5, v5];
                 }
               }
            let v273;
            const v275 = stdlib.le(0, v158);
            const v276 = stdlib.lt(v158, 5);
            const v278 = v275 ? v276 : false;
            stdlib.assert(v278);
            const v279 = stdlib.eq(v158, 0);
            if (v279) {
              v273 = 'Bob wins';
               }
            else {
              const v280 = stdlib.eq(v158, 1);
              if (v280) {
                v273 = 'Draw';
                 }
              else {
                const v281 = stdlib.eq(v158, 2);
                if (v281) {
                  v273 = 'Alice wins';
                   }
                else {
                  const v282 = stdlib.eq(v158, 3);
                  if (v282) {
                    v273 = 'Alice quits';
                     }
                  else {
                    v273 = 'Bob quits';
                     }
                   }
                 }
               }
            stdlib.protect(stdlib.T_Null, await interact.endsWith(v273));
            
            return; } } } } } }
export async function O(stdlib, ctc, interact) {
  const txn0 = { balance: 0, value: 0 };
  const txn1 = await ctc.recv('O', 1, 2, false);
  const [v5, v6] = txn1.data;
  const v7 = txn1.from;
  const v9 = stdlib.add(v5, v6);
  const v10 = txn1.value;
  const v11 = stdlib.eq(v9, v10);
  stdlib.assert(v11);
  const txn2 = await ctc.recv('O', 2, 0, 10);
  if (txn2.didTimeout) {
    const txn3 = await ctc.recv('O', 11, 0, false);
    const [] = txn3.data;
    const v20 = txn3.value;
    const v21 = stdlib.eq(0, v20);
    stdlib.assert(v21);
    return; }
  else {
    const [] = txn2.data;
    const v15 = txn2.from;
    const v16 = txn2.value;
    const v17 = stdlib.eq(v5, v16);
    stdlib.assert(v17);
    const txn3 = await ctc.recv('O', 3, 1, 10);
    if (txn3.didTimeout) {
      const txn4 = await ctc.recv('O', 10, 0, false);
      const [] = txn4.data;
      const v63 = txn4.value;
      const v64 = stdlib.eq(0, v63);
      stdlib.assert(v64);
      return; }
    else {
      const [v58] = txn3.data;
      const v59 = txn3.value;
      const v60 = stdlib.eq(0, v59);
      stdlib.assert(v60);
      const txn4 = await ctc.recv('O', 4, 1, 10);
      if (txn4.didTimeout) {
        const txn5 = await ctc.recv('O', 9, 0, false);
        const [] = txn5.data;
        const v100 = txn5.value;
        const v101 = stdlib.eq(0, v100);
        stdlib.assert(v101);
        return; }
      else {
        const [v95] = txn4.data;
        const v96 = txn4.value;
        const v97 = stdlib.eq(0, v96);
        stdlib.assert(v97);
        const v115 = stdlib.le(0, v95);
        const v116 = stdlib.lt(v95, 3);
        const v118 = v115 ? v116 : false;
        stdlib.assert(v118);
        const txn5 = await ctc.recv('O', 5, 2, 10);
        if (txn5.didTimeout) {
          const txn6 = await ctc.recv('O', 8, 0, false);
          const [] = txn6.data;
          const v135 = txn6.value;
          const v136 = stdlib.eq(0, v135);
          stdlib.assert(v136);
          return; }
        else {
          const [v129, v130] = txn5.data;
          const v131 = txn5.value;
          const v132 = stdlib.eq(0, v131);
          stdlib.assert(v132);
          const v150 = stdlib.keccak256(v129, v130);
          const v151 = stdlib.eq(v58, v150);
          stdlib.assert(v151);
          const v153 = stdlib.le(0, v130);
          const v154 = stdlib.lt(v130, 3);
          const v156 = v153 ? v154 : false;
          stdlib.assert(v156);
          let v158;
          const v160 = stdlib.le(0, v130);
          const v161 = stdlib.lt(v130, 3);
          const v163 = v160 ? v161 : false;
          const v165 = stdlib.le(0, v95);
          const v166 = stdlib.lt(v95, 3);
          const v168 = v165 ? v166 : false;
          const v170 = v163 ? v168 : false;
          if (v170) {
            const v171 = stdlib.sub(4, v95);
            const v172 = stdlib.add(v130, v171);
            const v173 = stdlib.mod(v172, 3);
            v158 = v173;
             }
          else {
            if (v163) {
              v158 = 2;
               }
            else {
              if (v168) {
                v158 = 0;
                 }
              else {
                v158 = 1;
                 }
               }
             }
          const txn6 = await ctc.recv('O', 6, 0, 10);
          if (txn6.didTimeout) {
            const txn7 = await ctc.recv('O', 7, 0, false);
            const [] = txn7.data;
            const v235 = txn7.value;
            const v236 = stdlib.eq(0, v235);
            stdlib.assert(v236);
            return; }
          else {
            const [] = txn6.data;
            const v231 = txn6.value;
            const v232 = stdlib.eq(0, v231);
            stdlib.assert(v232);
            let v250;
            const v251 = stdlib.eq(v158, 2);
            if (v251) {
              const v252 = stdlib.mul(2, v5);
              v250 = [v252, 0];
               }
            else {
              const v253 = stdlib.eq(v158, 0);
              if (v253) {
                const v254 = stdlib.mul(2, v5);
                v250 = [0, v254];
                 }
              else {
                v250 = [v5, v5];
                 }
               }
            return; } } } } } }

export const ETH = {
  ABI: `[{"inputs":[],"payable":true,"stateMutability":"payable","type":"constructor"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v5","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v6","type":"uint256"}],"name":"e1","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e10","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e11","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e2","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v58","type":"uint256"}],"name":"e3","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v95","type":"uint256"}],"name":"e4","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v129","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v130","type":"uint256"}],"name":"e5","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e6","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e7","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e8","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e9","type":"event"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"}],"name":"m1","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v7","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"},{"internalType":"address payable","name":"v15","type":"address"}],"name":"m10","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v7","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"}],"name":"m11","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v7","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"}],"name":"m2","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v7","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"},{"internalType":"address payable","name":"v15","type":"address"},{"internalType":"uint256","name":"v58","type":"uint256"}],"name":"m3","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v7","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"},{"internalType":"address payable","name":"v15","type":"address"},{"internalType":"uint256","name":"v58","type":"uint256"},{"internalType":"uint256","name":"v95","type":"uint256"}],"name":"m4","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v7","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"},{"internalType":"address payable","name":"v15","type":"address"},{"internalType":"uint256","name":"v58","type":"uint256"},{"internalType":"uint256","name":"v95","type":"uint256"},{"internalType":"uint256","name":"v129","type":"uint256"},{"internalType":"uint256","name":"v130","type":"uint256"}],"name":"m5","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"uint256","name":"v158","type":"uint256"},{"internalType":"address payable","name":"v7","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"},{"internalType":"address payable","name":"v15","type":"address"}],"name":"m6","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"uint256","name":"v158","type":"uint256"},{"internalType":"address payable","name":"v7","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"},{"internalType":"address payable","name":"v15","type":"address"}],"name":"m7","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v7","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"},{"internalType":"address payable","name":"v15","type":"address"},{"internalType":"uint256","name":"v58","type":"uint256"},{"internalType":"uint256","name":"v95","type":"uint256"}],"name":"m8","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v7","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"},{"internalType":"address payable","name":"v15","type":"address"},{"internalType":"uint256","name":"v58","type":"uint256"}],"name":"m9","outputs":[],"payable":true,"stateMutability":"payable","type":"function"}]`,
  Bytecode: `0x600060a08181524360c0526040608081905260e0815290209055611197806100286000396000f3fe60806040526004361061009c5760003560e01c806368e591731161006457806368e59173146101ef57806373929c5e146102375780637984da48146102935780637a52ccb3146102e15780639ccddd3a1461030a578063bb91d6e3146103425761009c565b806304d6d980146100a15780630cdb9862146100e35780631188dea6146101295780635aee2a60146101615780635e6a8eed146101a7575b600080fd5b6100e1600480360360a08110156100b757600080fd5b508035906001600160a01b0360208201358116916040810135916060820135916080013516610390565b005b6100e1600480360360c08110156100f957600080fd5b508035906020810135906001600160a01b03604082013581169160608101359160808201359160a00135166104a0565b6100e16004803603608081101561013f57600080fd5b508035906001600160a01b0360208201351690604081013590606001356105b8565b6100e1600480360360c081101561017757600080fd5b508035906020810135906001600160a01b03604082013581169160608101359160808201359160a00135166106bf565b6100e1600480360360c08110156101bd57600080fd5b508035906001600160a01b03602082013581169160408101359160608201359160808101359091169060a00135610873565b6100e1600480360360c081101561020557600080fd5b508035906001600160a01b03602082013581169160408101359160608201359160808101359091169060a001356109aa565b6100e1600480360361012081101561024e57600080fd5b508035906001600160a01b03602082013581169160408101359160608201359160808101359091169060a08101359060c08101359060e0810135906101000135610ac2565b6100e1600480360360e08110156102a957600080fd5b508035906001600160a01b03602082013581169160408101359160608201359160808101359091169060a08101359060c00135610cc7565b6100e1600480360360608110156102f757600080fd5b5080359060208101359060400135610de7565b6100e16004803603608081101561032057600080fd5b508035906001600160a01b036020820135169060408101359060600135610eb7565b6100e1600480360360e081101561035857600080fd5b508035906001600160a01b03602082013581169160408101359160608201359160808101359091169060a08101359060c00135610fbe565b6040805160026020808301919091528183018890526001600160601b0319606088811b821681850152607484018890526094840187905285901b1660b4830152825160a881840301815260c89092019092528051910120600054146103f457600080fd5b336001600160a01b0382161461040957600080fd5b600a8501431015801561041a575060015b61042357600080fd5b341561042e57600080fd5b6040516001600160a01b03821690303180156108fc02916000818181858888f19350505050158015610464573d6000803e3d6000fd5b50604080513031815290517f9bf9cf9ae88051b33b19923b1c1cf36013b840c9975de29305d444b55d83c6bd9181900360200190a16000805533ff5b60408051600560208083019190915281830189905260608083018990526001600160601b031988821b811660808501526094840188905260b484018790529085901b1660d4830152825160c881840301815260e890920190925280519101206000541461050c57600080fd5b336001600160a01b0385161461052157600080fd5b600a86014310158015610532575060015b61053b57600080fd5b341561054657600080fd5b6040516001600160a01b03851690303180156108fc02916000818181858888f1935050505015801561057c573d6000803e3d6000fd5b50604080513031815290517ffc55d683ac816a7149ebdfa999ae1bcfeeae27c37c9dab64a23f617beed2a0079181900360200190a16000805533ff5b604080516001602080830191909152818301879052606086811b6001600160601b031916908301526074820185905260948083018590528351808403909101815260b490920190925280519101206000541461061357600080fd5b336001600160a01b0384161461062857600080fd5b600a84014310158015610639575060015b61064257600080fd5b341561064d57600080fd5b6040516001600160a01b03841690303180156108fc02916000818181858888f19350505050158015610683573d6000803e3d6000fd5b50604080513031815290517fd22b308a0739d4b2391b9fea991868a737c5ac9fca1931271dbb52121d7192ad9181900360200190a16000805533ff5b60408051600560208083019190915281830189905260608083018990526001600160601b031988821b811660808501526094840188905260b484018790529085901b1660d4830152825160c881840301815260e890920190925280519101206000541461072b57600080fd5b610733611110565b336001600160a01b0383161461074857600080fd5b600a8701431061075757600080fd5b341561076257600080fd5b600286141561078957604080518082019091526002850281526000602082015281526107c3565b856107ac57604080518082019091526000815260028502602082015281526107c3565b604080518082019091528481526020810185905281525b8051516040516001600160a01b03871691850180156108fc02916000818181858888f193505050501580156107fc573d6000803e3d6000fd5b508051602001516040516001600160a01b0384169180156108fc02916000818181858888f19350505050158015610837573d6000803e3d6000fd5b50604080513031815290517fcb3347bd475fd43f41b4bc5bb011db952f2079e6ba9a82ff211988cd7871dba69181900360200190a16000805533ff5b6040805160026020808301919091528183018990526001600160601b0319606089811b821681850152607484018990526094840188905286901b1660b4830152825160a881840301815260c89092019092528051910120600054146108d757600080fd5b336001600160a01b038616146108ec57600080fd5b600a860143106108fb57600080fd5b341561090657600080fd5b60408051303181526020810183905281517f94dd7e08991b8945fde2d5865f7071e72045b9800e293ff60d29c6960c5a4fb5929181900390910190a160408051600360208083019190915243828401526001600160601b0319606098891b811689840152607483019790975260948201959095529290951b90931660b482015260c8808201939093528351808203909301835260e801909252805191012060005550565b6040805160036020808301919091528183018990526001600160601b0319606089811b821681850152607484018990526094840188905286901b1660b483015260c88083018590528351808403909101815260e8909201909252805191012060005414610a1657600080fd5b336001600160a01b03861614610a2b57600080fd5b600a86014310158015610a3c575060015b610a4557600080fd5b3415610a5057600080fd5b6040516001600160a01b03861690303180156108fc02916000818181858888f19350505050158015610a86573d6000803e3d6000fd5b50604080513031815290517fc92018b4e91e597d736654f7b1d2ec034c5fec5920e2cfe22e15b4ddcdf5e18a9181900360200190a16000805533ff5b6040805160046020808301919091528183018c90526001600160601b031960608c811b821681850152607484018c9052609484018b905289901b1660b483015260c8820187905260e880830187905283518084039091018152610108909201909252805191012060005414610b3657600080fd5b610b3e611128565b336001600160a01b038a1614610b5357600080fd5b600a8a014310610b6257600080fd5b3415610b6d57600080fd5b604080516020808201869052818301859052825180830384018152606090920190925280519101208514610ba057600080fd5b60038210610bad57600080fd5b6003808310602083018190529085106040830152610bcc576000610bd2565b80604001515b15610bee57600384600403830181610be657fe5b068152610c19565b806020015115610c015760028152610c19565b806040015115610c145760008152610c19565b600181525b60408051303181526020810185905280820184905290517f3c3023cc427ae7f284b643c954c1a90afba24284d594cded84550e2316e830f49181900360600190a15160408051600560208083019190915243828401526060808301949094526001600160601b03199b841b8c166080830152609482019a909a5260b481019890985295901b90971660d48601525050815160c881850301815260e89093019091525080519101206000555050565b6040805160046020808301919091528183018a90526001600160601b031960608a811b821681850152607484018a90526094840189905287901b1660b483015260c8820185905260e880830185905283518084039091018152610108909201909252805191012060005414610d3b57600080fd5b336001600160a01b03841614610d5057600080fd5b600a87014310158015610d61575060015b610d6a57600080fd5b3415610d7557600080fd5b6040516001600160a01b03841690303180156108fc02916000818181858888f19350505050158015610dab573d6000803e3d6000fd5b50604080513031815290517f3a6f8023909a26b76d462631fcdf570dbe3740447548e09470d1ad04394a0cec9181900360200190a16000805533ff5b6040805160006020808301829052828401879052835180840385018152606090930190935281519190920120905414610e1f57600080fd5b3481830114610e2d57600080fd5b60408051303181526020810184905280820183905290517f219cc811755104876269c7553666684eaaeecb90b6a7ffc6fdd5068140059b8e9181900360600190a1604080516001602080830191909152438284015233606090811b9083015260748201949094526094808201939093528151808203909301835260b4019052805191012060005550565b604080516001602080830191909152818301879052606086811b6001600160601b031916908301526074820185905260948083018590528351808403909101815260b4909201909252805191012060005414610f1257600080fd5b600a84014310610f2157600080fd5b348214610f2d57600080fd5b604080513031815290517ff04f5fc87a72102f7c0b228f8bbaf9b9aa7a2b5dc295c86538fdde91e95866e99181900360200190a160408051600260208083019190915243828401526001600160601b0319606096871b1686830152607482019490945260948101929092523390931b60b4820152825160a881830301815260c8909101909252815191012060005550565b6040805160036020808301919091528183018a90526001600160601b031960608a811b821681850152607484018a90526094840189905287901b1660b483015260c88083018690528351808403909101815260e890920190925280519101206000541461102a57600080fd5b336001600160a01b0384161461103f57600080fd5b600a8701431061104e57600080fd5b341561105957600080fd5b6003811061106657600080fd5b60408051303181526020810183905281517fb71d350b59ceca5c6544e5367d61ca8cae3e36b25f8d900743d063dff3d6508b929181900390910190a160408051600460208083019190915243828401526001600160601b03196060998a1b81168a840152607483019890985260948201969096529390961b90941660b483015260c882015260e8808201939093528351808203909301835261010801909252805191012060005550565b6040518060200160405280611123611148565b905290565b604080516060810182526000808252602082018190529181019190915290565b60405180604001604052806000815260200160008152509056fea265627a7a723158204326ecda387926d40ed0e49234198a886ea03739be31f515be1736c2c6abd01b64736f6c634300050c0032` };