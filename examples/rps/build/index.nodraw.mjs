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
    
    const txn3 = await ctc.sendrecv('A', 12, 0, [v7, v5, v6], 0, false, null);
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
    
    
    const txn3 = await ctc.sendrecv('A', 3, 0, [v7, v5, v6, v15], 0, 10, null);
    if (txn3.didTimeout) {
      const txn4 = await ctc.recv('A', 11, 0, false);
      const [] = txn4.data;
      const v40 = txn4.value;
      const v41 = stdlib.eq(0, v40);
      stdlib.assert(v41);
      stdlib.assert(true);
      stdlib.protect(stdlib.T_Null, await interact.endsWith('Alice quits'));
      
      return; }
    else {
      const [] = txn3.data;
      const v36 = txn3.value;
      const v37 = stdlib.eq(0, v36);
      stdlib.assert(v37);
      let v54 = 0;
      let v55 = 1;
      while ((() => {
        const v79 = stdlib.eq(v55, 1);
        
        return v79; })()) {
        let v83;
        const v84 = stdlib.protect(stdlib.T_Bytes, await interact.getHand());
        const v85 = stdlib.bytes_eq(v84, 'ROCK');
        const v86 = stdlib.bytes_eq(v84, 'PAPER');
        const v87 = stdlib.bytes_eq(v84, 'SCISSORS');
        const v89 = v85 ? true : v86;
        const v91 = v89 ? true : v87;
        stdlib.assert(v91);
        if (v85) {
          v83 = 0;
           }
        else {
          if (v86) {
            v83 = 1;
             }
          else {
            v83 = 2;
             }
           }
        const v99 = stdlib.protect(stdlib.T_UInt256, await interact.random());
        const v100 = stdlib.keccak256(v99, v83);
        stdlib.protect(stdlib.T_Null, await interact.commits());
        
        
        const txn4 = await ctc.sendrecv('A', 5, 1, [v7, v5, v6, v15, v54, v100], 0, 10, null);
        if (txn4.didTimeout) {
          const txn5 = await ctc.recv('A', 10, 0, false);
          const [] = txn5.data;
          const v107 = txn5.value;
          const v108 = stdlib.eq(0, v107);
          stdlib.assert(v108);
          stdlib.assert(true);
          stdlib.protect(stdlib.T_Null, await interact.endsWith('Alice quits'));
          
          return; }
        else {
          const [v102] = txn4.data;
          const v103 = txn4.value;
          const v104 = stdlib.eq(0, v103);
          stdlib.assert(v104);
          const txn5 = await ctc.recv('A', 6, 1, 10);
          if (txn5.didTimeout) {
            
            const txn6 = await ctc.sendrecv('A', 9, 0, [v7, v5, v6, v15, v102, v54], 0, false, null);
            const [] = txn6.data;
            const v144 = txn6.value;
            const v145 = stdlib.eq(0, v144);
            stdlib.assert(v145);
            stdlib.assert(true);
            stdlib.protect(stdlib.T_Null, await interact.endsWith('Bob quits'));
            
            return; }
          else {
            const [v139] = txn5.data;
            const v140 = txn5.value;
            const v141 = stdlib.eq(0, v140);
            stdlib.assert(v141);
            const v159 = stdlib.le(0, v139);
            const v160 = stdlib.lt(v139, 3);
            const v162 = v159 ? v160 : false;
            stdlib.assert(v162);
            let v164;
            const v166 = stdlib.le(0, v139);
            const v167 = stdlib.lt(v139, 3);
            const v169 = v166 ? v167 : false;
            stdlib.assert(v169);
            const v170 = stdlib.eq(v139, 0);
            if (v170) {
              v164 = 'ROCK';
               }
            else {
              const v171 = stdlib.eq(v139, 1);
              if (v171) {
                v164 = 'PAPER';
                 }
              else {
                v164 = 'SCISSORS';
                 }
               }
            stdlib.protect(stdlib.T_Null, await interact.reveals(v164));
            
            
            const txn6 = await ctc.sendrecv('A', 7, 2, [v7, v5, v6, v15, v102, v139, v54, v99, v83], 0, 10, null);
            if (txn6.didTimeout) {
              const txn7 = await ctc.recv('A', 8, 0, false);
              const [] = txn7.data;
              const v179 = txn7.value;
              const v180 = stdlib.eq(0, v179);
              stdlib.assert(v180);
              stdlib.assert(true);
              stdlib.protect(stdlib.T_Null, await interact.endsWith('Alice quits'));
              
              return; }
            else {
              const [v173, v174] = txn6.data;
              const v175 = txn6.value;
              const v176 = stdlib.eq(0, v175);
              stdlib.assert(v176);
              const v194 = stdlib.keccak256(v173, v174);
              const v195 = stdlib.eq(v102, v194);
              stdlib.assert(v195);
              const v197 = stdlib.le(0, v174);
              const v198 = stdlib.lt(v174, 3);
              const v200 = v197 ? v198 : false;
              stdlib.assert(v200);
              let v202;
              const v204 = stdlib.le(0, v174);
              const v205 = stdlib.lt(v174, 3);
              const v207 = v204 ? v205 : false;
              const v209 = stdlib.le(0, v139);
              const v210 = stdlib.lt(v139, 3);
              const v212 = v209 ? v210 : false;
              const v214 = v207 ? v212 : false;
              if (v214) {
                const v215 = stdlib.sub(4, v139);
                const v216 = stdlib.add(v174, v215);
                const v217 = stdlib.mod(v216, 3);
                v202 = v217;
                 }
              else {
                if (v207) {
                  v202 = 2;
                   }
                else {
                  if (v212) {
                    v202 = 0;
                     }
                  else {
                    v202 = 1;
                     }
                   }
                 }
              const v275 = stdlib.add(1, v54);
              v54 = v275;
              v55 = v202;
              continue; } } } }
      let v281;
      const v282 = stdlib.eq(v55, 2);
      if (v282) {
        const v283 = stdlib.mul(2, v5);
        v281 = [v283, 0];
         }
      else {
        const v284 = stdlib.eq(v55, 0);
        if (v284) {
          const v285 = stdlib.mul(2, v5);
          v281 = [0, v285];
           }
        else {
          v281 = [v5, v5];
           }
         }
      let v292;
      const v294 = stdlib.le(0, v55);
      const v295 = stdlib.lt(v55, 5);
      const v297 = v294 ? v295 : false;
      stdlib.assert(v297);
      const v298 = stdlib.eq(v55, 0);
      if (v298) {
        v292 = 'Bob wins';
         }
      else {
        const v299 = stdlib.eq(v55, 1);
        if (v299) {
          v292 = 'Draw';
           }
        else {
          const v300 = stdlib.eq(v55, 2);
          if (v300) {
            v292 = 'Alice wins';
             }
          else {
            const v301 = stdlib.eq(v55, 3);
            if (v301) {
              v292 = 'Alice quits';
               }
            else {
              v292 = 'Bob quits';
               }
             }
           }
         }
      stdlib.protect(stdlib.T_Null, await interact.endsWith(v292));
      
      return; } } }
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
    const txn3 = await ctc.recv('B', 12, 0, false);
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
    const txn3 = await ctc.recv('B', 3, 0, 10);
    if (txn3.didTimeout) {
      
      const txn4 = await ctc.sendrecv('B', 11, 0, [v7, v5, v6, v15], 0, false, null);
      const [] = txn4.data;
      const v40 = txn4.value;
      const v41 = stdlib.eq(0, v40);
      stdlib.assert(v41);
      stdlib.assert(true);
      stdlib.protect(stdlib.T_Null, await interact.endsWith('Alice quits'));
      
      return; }
    else {
      const [] = txn3.data;
      const v36 = txn3.value;
      const v37 = stdlib.eq(0, v36);
      stdlib.assert(v37);
      let v54 = 0;
      let v55 = 1;
      while ((() => {
        const v79 = stdlib.eq(v55, 1);
        
        return v79; })()) {
        const txn4 = await ctc.recv('B', 5, 1, 10);
        if (txn4.didTimeout) {
          
          const txn5 = await ctc.sendrecv('B', 10, 0, [v7, v5, v6, v15, v54], 0, false, null);
          const [] = txn5.data;
          const v107 = txn5.value;
          const v108 = stdlib.eq(0, v107);
          stdlib.assert(v108);
          stdlib.assert(true);
          stdlib.protect(stdlib.T_Null, await interact.endsWith('Alice quits'));
          
          return; }
        else {
          const [v102] = txn4.data;
          const v103 = txn4.value;
          const v104 = stdlib.eq(0, v103);
          stdlib.assert(v104);
          let v123;
          const v124 = stdlib.protect(stdlib.T_Bytes, await interact.getHand());
          const v125 = stdlib.bytes_eq(v124, 'ROCK');
          const v126 = stdlib.bytes_eq(v124, 'PAPER');
          const v127 = stdlib.bytes_eq(v124, 'SCISSORS');
          const v129 = v125 ? true : v126;
          const v131 = v129 ? true : v127;
          stdlib.assert(v131);
          if (v125) {
            v123 = 0;
             }
          else {
            if (v126) {
              v123 = 1;
               }
            else {
              v123 = 2;
               }
             }
          stdlib.protect(stdlib.T_Null, await interact.shows());
          
          
          const txn5 = await ctc.sendrecv('B', 6, 1, [v7, v5, v6, v15, v102, v54, v123], 0, 10, null);
          if (txn5.didTimeout) {
            const txn6 = await ctc.recv('B', 9, 0, false);
            const [] = txn6.data;
            const v144 = txn6.value;
            const v145 = stdlib.eq(0, v144);
            stdlib.assert(v145);
            stdlib.assert(true);
            stdlib.protect(stdlib.T_Null, await interact.endsWith('Bob quits'));
            
            return; }
          else {
            const [v139] = txn5.data;
            const v140 = txn5.value;
            const v141 = stdlib.eq(0, v140);
            stdlib.assert(v141);
            const v159 = stdlib.le(0, v139);
            const v160 = stdlib.lt(v139, 3);
            const v162 = v159 ? v160 : false;
            stdlib.assert(v162);
            const txn6 = await ctc.recv('B', 7, 2, 10);
            if (txn6.didTimeout) {
              
              const txn7 = await ctc.sendrecv('B', 8, 0, [v7, v5, v6, v15, v102, v139, v54], 0, false, null);
              const [] = txn7.data;
              const v179 = txn7.value;
              const v180 = stdlib.eq(0, v179);
              stdlib.assert(v180);
              stdlib.assert(true);
              stdlib.protect(stdlib.T_Null, await interact.endsWith('Alice quits'));
              
              return; }
            else {
              const [v173, v174] = txn6.data;
              const v175 = txn6.value;
              const v176 = stdlib.eq(0, v175);
              stdlib.assert(v176);
              const v194 = stdlib.keccak256(v173, v174);
              const v195 = stdlib.eq(v102, v194);
              stdlib.assert(v195);
              const v197 = stdlib.le(0, v174);
              const v198 = stdlib.lt(v174, 3);
              const v200 = v197 ? v198 : false;
              stdlib.assert(v200);
              let v202;
              const v204 = stdlib.le(0, v174);
              const v205 = stdlib.lt(v174, 3);
              const v207 = v204 ? v205 : false;
              const v209 = stdlib.le(0, v139);
              const v210 = stdlib.lt(v139, 3);
              const v212 = v209 ? v210 : false;
              const v214 = v207 ? v212 : false;
              if (v214) {
                const v215 = stdlib.sub(4, v139);
                const v216 = stdlib.add(v174, v215);
                const v217 = stdlib.mod(v216, 3);
                v202 = v217;
                 }
              else {
                if (v207) {
                  v202 = 2;
                   }
                else {
                  if (v212) {
                    v202 = 0;
                     }
                  else {
                    v202 = 1;
                     }
                   }
                 }
              const v275 = stdlib.add(1, v54);
              v54 = v275;
              v55 = v202;
              continue; } } } }
      let v281;
      const v282 = stdlib.eq(v55, 2);
      if (v282) {
        const v283 = stdlib.mul(2, v5);
        v281 = [v283, 0];
         }
      else {
        const v284 = stdlib.eq(v55, 0);
        if (v284) {
          const v285 = stdlib.mul(2, v5);
          v281 = [0, v285];
           }
        else {
          v281 = [v5, v5];
           }
         }
      let v304;
      const v306 = stdlib.le(0, v55);
      const v307 = stdlib.lt(v55, 5);
      const v309 = v306 ? v307 : false;
      stdlib.assert(v309);
      const v310 = stdlib.eq(v55, 0);
      if (v310) {
        v304 = 'Bob wins';
         }
      else {
        const v311 = stdlib.eq(v55, 1);
        if (v311) {
          v304 = 'Draw';
           }
        else {
          const v312 = stdlib.eq(v55, 2);
          if (v312) {
            v304 = 'Alice wins';
             }
          else {
            const v313 = stdlib.eq(v55, 3);
            if (v313) {
              v304 = 'Alice quits';
               }
            else {
              v304 = 'Bob quits';
               }
             }
           }
         }
      stdlib.protect(stdlib.T_Null, await interact.endsWith(v304));
      
      return; } } }
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
    const txn3 = await ctc.recv('O', 12, 0, false);
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
    const txn3 = await ctc.recv('O', 3, 0, 10);
    if (txn3.didTimeout) {
      const txn4 = await ctc.recv('O', 11, 0, false);
      const [] = txn4.data;
      const v40 = txn4.value;
      const v41 = stdlib.eq(0, v40);
      stdlib.assert(v41);
      return; }
    else {
      const [] = txn3.data;
      const v36 = txn3.value;
      const v37 = stdlib.eq(0, v36);
      stdlib.assert(v37);
      let v54 = 0;
      let v55 = 1;
      while ((() => {
        const v79 = stdlib.eq(v55, 1);
        
        return v79; })()) {
        const txn4 = await ctc.recv('O', 5, 1, 10);
        if (txn4.didTimeout) {
          const txn5 = await ctc.recv('O', 10, 0, false);
          const [] = txn5.data;
          const v107 = txn5.value;
          const v108 = stdlib.eq(0, v107);
          stdlib.assert(v108);
          return; }
        else {
          const [v102] = txn4.data;
          const v103 = txn4.value;
          const v104 = stdlib.eq(0, v103);
          stdlib.assert(v104);
          const txn5 = await ctc.recv('O', 6, 1, 10);
          if (txn5.didTimeout) {
            const txn6 = await ctc.recv('O', 9, 0, false);
            const [] = txn6.data;
            const v144 = txn6.value;
            const v145 = stdlib.eq(0, v144);
            stdlib.assert(v145);
            return; }
          else {
            const [v139] = txn5.data;
            const v140 = txn5.value;
            const v141 = stdlib.eq(0, v140);
            stdlib.assert(v141);
            const v159 = stdlib.le(0, v139);
            const v160 = stdlib.lt(v139, 3);
            const v162 = v159 ? v160 : false;
            stdlib.assert(v162);
            const txn6 = await ctc.recv('O', 7, 2, 10);
            if (txn6.didTimeout) {
              const txn7 = await ctc.recv('O', 8, 0, false);
              const [] = txn7.data;
              const v179 = txn7.value;
              const v180 = stdlib.eq(0, v179);
              stdlib.assert(v180);
              return; }
            else {
              const [v173, v174] = txn6.data;
              const v175 = txn6.value;
              const v176 = stdlib.eq(0, v175);
              stdlib.assert(v176);
              const v194 = stdlib.keccak256(v173, v174);
              const v195 = stdlib.eq(v102, v194);
              stdlib.assert(v195);
              const v197 = stdlib.le(0, v174);
              const v198 = stdlib.lt(v174, 3);
              const v200 = v197 ? v198 : false;
              stdlib.assert(v200);
              let v202;
              const v204 = stdlib.le(0, v174);
              const v205 = stdlib.lt(v174, 3);
              const v207 = v204 ? v205 : false;
              const v209 = stdlib.le(0, v139);
              const v210 = stdlib.lt(v139, 3);
              const v212 = v209 ? v210 : false;
              const v214 = v207 ? v212 : false;
              if (v214) {
                const v215 = stdlib.sub(4, v139);
                const v216 = stdlib.add(v174, v215);
                const v217 = stdlib.mod(v216, 3);
                v202 = v217;
                 }
              else {
                if (v207) {
                  v202 = 2;
                   }
                else {
                  if (v212) {
                    v202 = 0;
                     }
                  else {
                    v202 = 1;
                     }
                   }
                 }
              const v275 = stdlib.add(1, v54);
              v54 = v275;
              v55 = v202;
              continue; } } } }
      let v281;
      const v282 = stdlib.eq(v55, 2);
      if (v282) {
        const v283 = stdlib.mul(2, v5);
        v281 = [v283, 0];
         }
      else {
        const v284 = stdlib.eq(v55, 0);
        if (v284) {
          const v285 = stdlib.mul(2, v5);
          v281 = [0, v285];
           }
        else {
          v281 = [v5, v5];
           }
         }
      return; } } }

export const ETH = {
  ABI: `[{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v7","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"},{"internalType":"address payable","name":"v15","type":"address"},{"internalType":"uint256","name":"v54","type":"uint256"},{"internalType":"uint256","name":"v102","type":"uint256"}],"name":"m5","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v7","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"},{"internalType":"address payable","name":"v15","type":"address"},{"internalType":"uint256","name":"v54","type":"uint256"}],"name":"m10","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"}],"name":"m1","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v7","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"},{"internalType":"address payable","name":"v15","type":"address"}],"name":"m11","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v7","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"}],"name":"m2","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v7","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"},{"internalType":"address payable","name":"v15","type":"address"},{"internalType":"uint256","name":"v102","type":"uint256"},{"internalType":"uint256","name":"v54","type":"uint256"},{"internalType":"uint256","name":"v139","type":"uint256"}],"name":"m6","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v7","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"},{"internalType":"address payable","name":"v15","type":"address"},{"internalType":"uint256","name":"v102","type":"uint256"},{"internalType":"uint256","name":"v139","type":"uint256"},{"internalType":"uint256","name":"v54","type":"uint256"}],"name":"m8","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v7","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"},{"internalType":"address payable","name":"v15","type":"address"}],"name":"m3","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v7","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"},{"internalType":"address payable","name":"v15","type":"address"},{"internalType":"uint256","name":"v102","type":"uint256"},{"internalType":"uint256","name":"v54","type":"uint256"}],"name":"m9","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v7","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"},{"internalType":"address payable","name":"v15","type":"address"},{"internalType":"uint256","name":"v102","type":"uint256"},{"internalType":"uint256","name":"v139","type":"uint256"},{"internalType":"uint256","name":"v54","type":"uint256"},{"internalType":"uint256","name":"v173","type":"uint256"},{"internalType":"uint256","name":"v174","type":"uint256"}],"name":"m7","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v7","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"}],"name":"m12","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"inputs":[],"payable":true,"stateMutability":"payable","type":"constructor"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v5","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v6","type":"uint256"}],"name":"e1","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e2","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e3","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v102","type":"uint256"}],"name":"e5","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v139","type":"uint256"}],"name":"e6","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v173","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v174","type":"uint256"}],"name":"e7","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e8","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e9","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e10","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e11","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e12","type":"event"}]`,
  Bytecode: `0x608060405261001160004360a0610040565b60408051601f198184030181529190528051602090910120600055610065565b61003a81610062565b82525050565b6040810161004e8285610031565b61005b6020830184610031565b9392505050565b90565b611442806100746000396000f3fe60806040526004361061009c5760003560e01c8063ab793da411610064578063ab793da414610102578063ae24d93c14610115578063c4532a9d14610128578063c98760c81461013b578063e68876941461014e578063f15594ca146101615761009c565b80630e6801d1146100a15780634f1f764d146100b65780637a52ccb3146100c9578063882ab247146100dc5780639ccddd3a146100ef575b600080fd5b6100b46100af366004610ed6565b610174565b005b6100b46100c4366004610e4f565b610266565b6100b46100d73660046110fb565b61035b565b6100b46100ea366004610dda565b61041a565b6100b46100fd366004610d79565b6104fe565b6100b4610110366004610f72565b6105cf565b6100b4610123366004610f72565b6106d3565b6100b4610136366004610dda565b6107bd565b6100b4610149366004610ed6565b61087e565b6100b461015c366004611022565b610966565b6100b461016f366004610d79565b610af4565b60048787878787876040516020016101929796959493929190611236565b6040516020818303038152906040528051906020012060001c600054146101b857600080fd5b336001600160a01b038716146101cd57600080fd5b600a870143106101dc57600080fd5b34156101e757600080fd5b6040517fabf482d77b67111a4971bb96fe81961f83ba459eb1d8fa9f78b6908251aeef1a9061021a90303190849061116e565b60405180910390a160054387878787868860405160200161024298979695949392919061129e565b60408051601f19818403018152919052805160209091012060005550505050505050565b60048686868686866040516020016102849796959493929190611236565b6040516020818303038152906040528051906020012060001c600054146102aa57600080fd5b336001600160a01b038316146102bf57600080fd5b600a860143101580156102d0575060015b6102d957600080fd5b34156102e457600080fd5b6040516001600160a01b03831690303180156108fc02916000818181858888f1935050505015801561031a573d6000803e3d6000fd5b506040517f9bf9cf9ae88051b33b19923b1c1cf36013b840c9975de29305d444b55d83c6bd9061034c90303190611160565b60405180910390a16000805533ff5b60008360405160200161036f92919061116e565b6040516020818303038152906040528051906020012060001c6000541461039557600080fd5b34818301146103a357600080fd5b6040517f219cc811755104876269c7553666684eaaeecb90b6a7ffc6fdd5068140059b8e906103d8903031908590859061139b565b60405180910390a16001433384846040516020016103fa959493929190611190565b60408051601f198184030181529190528051602090910120600055505050565b60028585858585604051602001610436969594939291906111dc565b6040516020818303038152906040528051906020012060001c6000541461045c57600080fd5b336001600160a01b0382161461047157600080fd5b600a85014310158015610482575060015b61048b57600080fd5b341561049657600080fd5b6040516001600160a01b03821690303180156108fc02916000818181858888f193505050501580156104cc573d6000803e3d6000fd5b506040517fd22b308a0739d4b2391b9fea991868a737c5ac9fca1931271dbb52121d7192ad9061034c90303190611160565b600184848484604051602001610518959493929190611190565b6040516020818303038152906040528051906020012060001c6000541461053e57600080fd5b600a8401431061054d57600080fd5b34821461055957600080fd5b6040517ff04f5fc87a72102f7c0b228f8bbaf9b9aa7a2b5dc295c86538fdde91e95866e99061058a90303190611160565b60405180910390a1600243848484336040516020016105ae969594939291906111dc565b60408051601f19818403018152919052805160209091012060005550505050565b6005888888888888886040516020016105ef98979695949392919061129e565b6040516020818303038152906040528051906020012060001c6000541461061557600080fd5b336001600160a01b0385161461062a57600080fd5b600a8801431061063957600080fd5b341561064457600080fd5b6003811061065157600080fd5b6040517f1fa1ad895cc7ba9133068b14fd5b3d9ed6f96d3a535ff2be342493855f237b6b9061068490303190849061116e565b60405180910390a1600643888888888887896040516020016106ae99989796959493929190611315565b60408051601f1981840301815291905280516020909101206000555050505050505050565b600688888888888888886040516020016106f599989796959493929190611315565b6040516020818303038152906040528051906020012060001c6000541461071b57600080fd5b336001600160a01b0385161461073057600080fd5b600a88014310158015610741575060015b61074a57600080fd5b341561075557600080fd5b6040516001600160a01b03851690303180156108fc02916000818181858888f1935050505015801561078b573d6000803e3d6000fd5b506040517f3a6f8023909a26b76d462631fcdf570dbe3740447548e09470d1ad04394a0cec9061034c90303190611160565b600285858585856040516020016107d9969594939291906111dc565b6040516020818303038152906040528051906020012060001c600054146107ff57600080fd5b336001600160a01b0385161461081457600080fd5b600a8501431061082357600080fd5b341561082e57600080fd5b6040517f6fbec89a9bad4c7daaf5b053ac2c5ad4e0ff33c287295fe9a98cf7f3a3043f9c9061085f90303190611160565b60405180910390a16108778484848460006001610bd6565b5050505050565b60058787878787878760405160200161089e98979695949392919061129e565b6040516020818303038152906040528051906020012060001c600054146108c457600080fd5b336001600160a01b038716146108d957600080fd5b600a870143101580156108ea575060015b6108f357600080fd5b34156108fe57600080fd5b6040516001600160a01b03871690303180156108fc02916000818181858888f19350505050158015610934573d6000803e3d6000fd5b506040517fc92018b4e91e597d736654f7b1d2ec034c5fec5920e2cfe22e15b4ddcdf5e18a9061034c90303190611160565b60068a8a8a8a8a8a8a8a60405160200161098899989796959493929190611315565b6040516020818303038152906040528051906020012060001c600054146109ae57600080fd5b6109b6610d0b565b336001600160a01b038b16146109cb57600080fd5b600a8b0143106109da57600080fd5b34156109e557600080fd5b82826040516020016109f892919061116e565b6040516020818303038152906040528051906020012060001c8614610a1c57600080fd5b60038210610a2957600080fd5b6003808310602083018190529086106040830152610a48576000610a4e565b80604001515b15610a6a57600385600403830181610a6257fe5b068152610a95565b806020015115610a7d5760028152610a95565b806040015115610a905760008152610a95565b600181525b6040517f5faf534620fe4d35c4670f2df8db5aff6901c4069d879904e0a4e11c119b422390610aca903031908690869061139b565b60405180910390a1610ae78a8a8a8a886001018660000151610bd6565b5050505050505050505050565b600184848484604051602001610b0e959493929190611190565b6040516020818303038152906040528051906020012060001c60005414610b3457600080fd5b336001600160a01b03841614610b4957600080fd5b600a84014310158015610b5a575060015b610b6357600080fd5b3415610b6e57600080fd5b6040516001600160a01b03841690303180156108fc02916000818181858888f19350505050158015610ba4573d6000803e3d6000fd5b506040517f0f5f4d65cf2c85506eee21a3fb54b49eb1fdb9267bbd430782deebd67e6a36399061034c90303190611160565b610bde610d2b565b6001821415610c25576004438888888888604051602001610c059796959493929190611236565b60408051601f198184030181529190528051602090910120600055610d02565b6002821415610c4c5760408051808201909152600287028152600060208201528152610c86565b81610c6f5760408051808201909152600081526002870260208201528152610c86565b604080518082019091528681526020810187905281525b8051516040516001600160a01b03891691870180156108fc02916000818181858888f19350505050158015610cbf573d6000803e3d6000fd5b508051602001516040516001600160a01b0386169180156108fc02916000818181858888f19350505050158015610cfa573d6000803e3d6000fd5b506000805533ff5b50505050505050565b604080516060810182526000808252602082018190529181019190915290565b6040518060200160405280610d3e610d43565b905290565b604051806040016040528060008152602001600081525090565b8035610d68816113df565b92915050565b8035610d68816113f6565b60008060008060808587031215610d8f57600080fd5b6000610d9b8787610d6e565b9450506020610dac87828801610d5d565b9350506040610dbd87828801610d6e565b9250506060610dce87828801610d6e565b91505092959194509250565b600080600080600060a08688031215610df257600080fd5b6000610dfe8888610d6e565b9550506020610e0f88828901610d5d565b9450506040610e2088828901610d6e565b9350506060610e3188828901610d6e565b9250506080610e4288828901610d5d565b9150509295509295909350565b60008060008060008060c08789031215610e6857600080fd5b6000610e748989610d6e565b9650506020610e8589828a01610d5d565b9550506040610e9689828a01610d6e565b9450506060610ea789828a01610d6e565b9350506080610eb889828a01610d5d565b92505060a0610ec989828a01610d6e565b9150509295509295509295565b600080600080600080600060e0888a031215610ef157600080fd5b6000610efd8a8a610d6e565b9750506020610f0e8a828b01610d5d565b9650506040610f1f8a828b01610d6e565b9550506060610f308a828b01610d6e565b9450506080610f418a828b01610d5d565b93505060a0610f528a828b01610d6e565b92505060c0610f638a828b01610d6e565b91505092959891949750929550565b600080600080600080600080610100898b031215610f8f57600080fd5b6000610f9b8b8b610d6e565b9850506020610fac8b828c01610d5d565b9750506040610fbd8b828c01610d6e565b9650506060610fce8b828c01610d6e565b9550506080610fdf8b828c01610d5d565b94505060a0610ff08b828c01610d6e565b93505060c06110018b828c01610d6e565b92505060e06110128b828c01610d6e565b9150509295985092959890939650565b6000806000806000806000806000806101408b8d03121561104257600080fd5b600061104e8d8d610d6e565b9a5050602061105f8d828e01610d5d565b99505060406110708d828e01610d6e565b98505060606110818d828e01610d6e565b97505060806110928d828e01610d5d565b96505060a06110a38d828e01610d6e565b95505060c06110b48d828e01610d6e565b94505060e06110c58d828e01610d6e565b9350506101006110d78d828e01610d6e565b9250506101206110e98d828e01610d6e565b9150509295989b9194979a5092959850565b60008060006060848603121561111057600080fd5b600061111c8686610d6e565b935050602061112d86828701610d6e565b925050604061113e86828701610d6e565b9150509250925092565b611151816113cb565b82525050565b611151816113dc565b60208101610d688284611157565b6040810161117c8285611157565b6111896020830184611157565b9392505050565b60a0810161119e8288611157565b6111ab6020830187611157565b6111b86040830186611148565b6111c56060830185611157565b6111d26080830184611157565b9695505050505050565b60c081016111ea8289611157565b6111f76020830188611157565b6112046040830187611148565b6112116060830186611157565b61121e6080830185611157565b61122b60a0830184611148565b979650505050505050565b60e08101611244828a611157565b6112516020830189611157565b61125e6040830188611148565b61126b6060830187611157565b6112786080830186611157565b61128560a0830185611148565b61129260c0830184611157565b98975050505050505050565b61010081016112ad828b611157565b6112ba602083018a611157565b6112c76040830189611148565b6112d46060830188611157565b6112e16080830187611157565b6112ee60a0830186611148565b6112fb60c0830185611157565b61130860e0830184611157565b9998505050505050505050565b6101208101611324828c611157565b611331602083018b611157565b61133e604083018a611148565b61134b6060830189611157565b6113586080830188611157565b61136560a0830187611148565b61137260c0830186611157565b61137f60e0830185611157565b61138d610100830184611157565b9a9950505050505050505050565b606081016113a98286611157565b6113b66020830185611157565b6113c36040830184611157565b949350505050565b60006001600160a01b038216610d68565b90565b6113e8816113cb565b81146113f357600080fd5b50565b6113e8816113dc56fea365627a7a7231582023c037759fd3506d7f32b248e190f148303f1d515b9bf3394cba6e9b697f00c46c6578706572696d656e74616cf564736f6c634300050b0040` };