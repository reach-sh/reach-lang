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
    
    const txn3 = await ctc.sendrecv('A', 14, 0, [v7, v5, v6], 0, false, null);
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
      const txn4 = await ctc.recv('A', 13, 0, false);
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
          const txn5 = await ctc.recv('A', 12, 0, false);
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
            
            const txn6 = await ctc.sendrecv('A', 11, 0, [v7, v5, v6, v15, v102, v54], 0, false, null);
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
              const txn7 = await ctc.recv('A', 10, 0, false);
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
              const txn7 = await ctc.recv('A', 8, 0, 10);
              if (txn7.didTimeout) {
                
                const txn8 = await ctc.sendrecv('A', 9, 0, [v202, v7, v5, v6, v15, v54], 0, false, null);
                const [] = txn8.data;
                const v279 = txn8.value;
                const v280 = stdlib.eq(0, v279);
                stdlib.assert(v280);
                stdlib.assert(true);
                stdlib.protect(stdlib.T_Null, await interact.endsWith('Bob quits'));
                
                return; }
              else {
                const [] = txn7.data;
                const v275 = txn7.value;
                const v276 = stdlib.eq(0, v275);
                stdlib.assert(v276);
                const v293 = stdlib.add(1, v54);
                v54 = v293;
                v55 = v202;
                continue; } } } } }
      let v299;
      const v300 = stdlib.eq(v55, 2);
      if (v300) {
        const v301 = stdlib.mul(2, v5);
        v299 = [v301, 0];
         }
      else {
        const v302 = stdlib.eq(v55, 0);
        if (v302) {
          const v303 = stdlib.mul(2, v5);
          v299 = [0, v303];
           }
        else {
          v299 = [v5, v5];
           }
         }
      let v310;
      const v312 = stdlib.le(0, v55);
      const v313 = stdlib.lt(v55, 5);
      const v315 = v312 ? v313 : false;
      stdlib.assert(v315);
      const v316 = stdlib.eq(v55, 0);
      if (v316) {
        v310 = 'Bob wins';
         }
      else {
        const v317 = stdlib.eq(v55, 1);
        if (v317) {
          v310 = 'Draw';
           }
        else {
          const v318 = stdlib.eq(v55, 2);
          if (v318) {
            v310 = 'Alice wins';
             }
          else {
            const v319 = stdlib.eq(v55, 3);
            if (v319) {
              v310 = 'Alice quits';
               }
            else {
              v310 = 'Bob quits';
               }
             }
           }
         }
      stdlib.protect(stdlib.T_Null, await interact.endsWith(v310));
      
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
    const txn3 = await ctc.recv('B', 14, 0, false);
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
      
      const txn4 = await ctc.sendrecv('B', 13, 0, [v7, v5, v6, v15], 0, false, null);
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
          
          const txn5 = await ctc.sendrecv('B', 12, 0, [v7, v5, v6, v15, v54], 0, false, null);
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
            const txn6 = await ctc.recv('B', 11, 0, false);
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
              
              const txn7 = await ctc.sendrecv('B', 10, 0, [v7, v5, v6, v15, v102, v139, v54], 0, false, null);
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
              
              const txn7 = await ctc.sendrecv('B', 8, 0, [v202, v7, v5, v6, v15, v54], 0, 10, null);
              if (txn7.didTimeout) {
                const txn8 = await ctc.recv('B', 9, 0, false);
                const [] = txn8.data;
                const v279 = txn8.value;
                const v280 = stdlib.eq(0, v279);
                stdlib.assert(v280);
                stdlib.assert(true);
                stdlib.protect(stdlib.T_Null, await interact.endsWith('Bob quits'));
                
                return; }
              else {
                const [] = txn7.data;
                const v275 = txn7.value;
                const v276 = stdlib.eq(0, v275);
                stdlib.assert(v276);
                const v293 = stdlib.add(1, v54);
                v54 = v293;
                v55 = v202;
                continue; } } } } }
      let v299;
      const v300 = stdlib.eq(v55, 2);
      if (v300) {
        const v301 = stdlib.mul(2, v5);
        v299 = [v301, 0];
         }
      else {
        const v302 = stdlib.eq(v55, 0);
        if (v302) {
          const v303 = stdlib.mul(2, v5);
          v299 = [0, v303];
           }
        else {
          v299 = [v5, v5];
           }
         }
      let v322;
      const v324 = stdlib.le(0, v55);
      const v325 = stdlib.lt(v55, 5);
      const v327 = v324 ? v325 : false;
      stdlib.assert(v327);
      const v328 = stdlib.eq(v55, 0);
      if (v328) {
        v322 = 'Bob wins';
         }
      else {
        const v329 = stdlib.eq(v55, 1);
        if (v329) {
          v322 = 'Draw';
           }
        else {
          const v330 = stdlib.eq(v55, 2);
          if (v330) {
            v322 = 'Alice wins';
             }
          else {
            const v331 = stdlib.eq(v55, 3);
            if (v331) {
              v322 = 'Alice quits';
               }
            else {
              v322 = 'Bob quits';
               }
             }
           }
         }
      stdlib.protect(stdlib.T_Null, await interact.endsWith(v322));
      
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
    const txn3 = await ctc.recv('O', 14, 0, false);
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
      const txn4 = await ctc.recv('O', 13, 0, false);
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
          const txn5 = await ctc.recv('O', 12, 0, false);
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
            const txn6 = await ctc.recv('O', 11, 0, false);
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
              const txn7 = await ctc.recv('O', 10, 0, false);
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
              const txn7 = await ctc.recv('O', 8, 0, 10);
              if (txn7.didTimeout) {
                const txn8 = await ctc.recv('O', 9, 0, false);
                const [] = txn8.data;
                const v279 = txn8.value;
                const v280 = stdlib.eq(0, v279);
                stdlib.assert(v280);
                return; }
              else {
                const [] = txn7.data;
                const v275 = txn7.value;
                const v276 = stdlib.eq(0, v275);
                stdlib.assert(v276);
                const v293 = stdlib.add(1, v54);
                v54 = v293;
                v55 = v202;
                continue; } } } } }
      let v299;
      const v300 = stdlib.eq(v55, 2);
      if (v300) {
        const v301 = stdlib.mul(2, v5);
        v299 = [v301, 0];
         }
      else {
        const v302 = stdlib.eq(v55, 0);
        if (v302) {
          const v303 = stdlib.mul(2, v5);
          v299 = [0, v303];
           }
        else {
          v299 = [v5, v5];
           }
         }
      return; } } }

export const ETH = {
  ABI: `[{"inputs":[],"payable":true,"stateMutability":"payable","type":"constructor"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v5","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v6","type":"uint256"}],"name":"e1","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e10","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e11","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e12","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e13","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e14","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e2","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e3","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v102","type":"uint256"}],"name":"e5","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v139","type":"uint256"}],"name":"e6","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v173","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v174","type":"uint256"}],"name":"e7","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e8","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e9","type":"event"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"}],"name":"m1","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v7","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"},{"internalType":"address payable","name":"v15","type":"address"},{"internalType":"uint256","name":"v102","type":"uint256"},{"internalType":"uint256","name":"v139","type":"uint256"},{"internalType":"uint256","name":"v54","type":"uint256"}],"name":"m10","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v7","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"},{"internalType":"address payable","name":"v15","type":"address"},{"internalType":"uint256","name":"v102","type":"uint256"},{"internalType":"uint256","name":"v54","type":"uint256"}],"name":"m11","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v7","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"},{"internalType":"address payable","name":"v15","type":"address"},{"internalType":"uint256","name":"v54","type":"uint256"}],"name":"m12","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v7","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"},{"internalType":"address payable","name":"v15","type":"address"}],"name":"m13","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v7","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"}],"name":"m14","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v7","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"}],"name":"m2","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v7","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"},{"internalType":"address payable","name":"v15","type":"address"}],"name":"m3","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v7","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"},{"internalType":"address payable","name":"v15","type":"address"},{"internalType":"uint256","name":"v54","type":"uint256"},{"internalType":"uint256","name":"v102","type":"uint256"}],"name":"m5","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v7","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"},{"internalType":"address payable","name":"v15","type":"address"},{"internalType":"uint256","name":"v102","type":"uint256"},{"internalType":"uint256","name":"v54","type":"uint256"},{"internalType":"uint256","name":"v139","type":"uint256"}],"name":"m6","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v7","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"},{"internalType":"address payable","name":"v15","type":"address"},{"internalType":"uint256","name":"v102","type":"uint256"},{"internalType":"uint256","name":"v139","type":"uint256"},{"internalType":"uint256","name":"v54","type":"uint256"},{"internalType":"uint256","name":"v173","type":"uint256"},{"internalType":"uint256","name":"v174","type":"uint256"}],"name":"m7","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"uint256","name":"v202","type":"uint256"},{"internalType":"address payable","name":"v7","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"},{"internalType":"address payable","name":"v15","type":"address"},{"internalType":"uint256","name":"v54","type":"uint256"}],"name":"m8","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"uint256","name":"v202","type":"uint256"},{"internalType":"address payable","name":"v7","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"},{"internalType":"address payable","name":"v15","type":"address"},{"internalType":"uint256","name":"v54","type":"uint256"}],"name":"m9","outputs":[],"payable":true,"stateMutability":"payable","type":"function"}]`,
  Bytecode: `0x600060a08181524360c0526040608081905260e081529020905561154e806100286000396000f3fe6080604052600436106100c25760003560e01c80637a831bb01161007f5780639f414459116100595780639f41445914610337578063ab793da414610377578063c4532a9d146103cc578063e68876941461040c576100c2565b80637a831bb01461026357806399369a28146102b15780639ccddd3a146102ff576100c2565b80630e6801d1146100c75780631b9d8f96146101175780632fcbe4df1461014f57806331ba1805146101a4578063549c01cc146101ec5780637a52ccb31461023a575b600080fd5b610115600480360360e08110156100dd57600080fd5b508035906001600160a01b03602082013581169160408101359160608201359160808101359091169060a08101359060c0013561046f565b005b6101156004803603608081101561012d57600080fd5b508035906001600160a01b0360208201351690604081013590606001356105b7565b610115600480360361010081101561016657600080fd5b508035906001600160a01b03602082013581169160408101359160608201359160808101359091169060a08101359060c08101359060e001356106be565b610115600480360360c08110156101ba57600080fd5b508035906001600160a01b03602082013581169160408101359160608201359160808101359091169060a001356107e6565b610115600480360360e081101561020257600080fd5b508035906020810135906001600160a01b03604082013581169160608101359160808201359160a08101359091169060c001356108fe565b6101156004803603606081101561025057600080fd5b5080359060208101359060400135610a1f565b610115600480360360e081101561027957600080fd5b508035906020810135906001600160a01b03604082013581169160608101359160808201359160a08101359091169060c00135610aef565b610115600480360360e08110156102c757600080fd5b508035906001600160a01b03602082013581169160408101359160608201359160808101359091169060a08101359060c00135610be1565b6101156004803603608081101561031557600080fd5b508035906001600160a01b036020820135169060408101359060600135610d01565b610115600480360360a081101561034d57600080fd5b508035906001600160a01b0360208201358116916040810135916060820135916080013516610e08565b610115600480360361010081101561038e57600080fd5b508035906001600160a01b03602082013581169160408101359160608201359160808101359091169060a08101359060c08101359060e00135610f18565b610115600480360360a08110156103e257600080fd5b508035906001600160a01b036020820135811691604081013591606082013591608001351661107e565b610115600480360361014081101561042357600080fd5b508035906001600160a01b03602082013581169160408101359160608201359160808101359091169060a08101359060c08101359060e08101359061010081013590610120013561115c565b6040805160046020808301919091528183018a90526001600160601b031960608a811b821681850152607484018a90526094840189905287901b1660b483015260c88083018690528351808403909101815260e89092019092528051910120600054146104db57600080fd5b336001600160a01b038716146104f057600080fd5b600a870143106104ff57600080fd5b341561050a57600080fd5b60408051303181526020810183905281517fabf482d77b67111a4971bb96fe81961f83ba459eb1d8fa9f78b6908251aeef1a929181900390910190a160408051600560208083019190915243828401526001600160601b03196060998a1b81168a840152607483019890985260948201969096529390961b90941660b483015260c882019390935260e8808201939093528351808203909301835261010801909252805191012060005550565b604080516001602080830191909152818301879052606086811b6001600160601b031916908301526074820185905260948083018590528351808403909101815260b490920190925280519101206000541461061257600080fd5b336001600160a01b0384161461062757600080fd5b600a84014310158015610638575060015b61064157600080fd5b341561064c57600080fd5b6040516001600160a01b03841690303180156108fc02916000818181858888f19350505050158015610682573d6000803e3d6000fd5b50604080513031815290517f824923244a4967d8ed2410dfb5a4428e6f77e8cdae6beb847e7ce9b44503ef319181900360200190a16000805533ff5b6040805160066020808301919091528183018b90526001600160601b031960608b811b821681850152607484018b9052609484018a905288901b1660b483015260c8820186905260e882018590526101088083018590528351808403909101815261012890920190925280519101206000541461073a57600080fd5b336001600160a01b0385161461074f57600080fd5b600a88014310158015610760575060015b61076957600080fd5b341561077457600080fd5b6040516001600160a01b03851690303180156108fc02916000818181858888f193505050501580156107aa573d6000803e3d6000fd5b50604080513031815290517f9bf9cf9ae88051b33b19923b1c1cf36013b840c9975de29305d444b55d83c6bd9181900360200190a16000805533ff5b6040805160046020808301919091528183018990526001600160601b0319606089811b821681850152607484018990526094840188905286901b1660b483015260c88083018590528351808403909101815260e890920190925280519101206000541461085257600080fd5b336001600160a01b0383161461086757600080fd5b600a86014310158015610878575060015b61088157600080fd5b341561088c57600080fd5b6040516001600160a01b03831690303180156108fc02916000818181858888f193505050501580156108c2573d6000803e3d6000fd5b50604080513031815290517f0f5f4d65cf2c85506eee21a3fb54b49eb1fdb9267bbd430782deebd67e6a36399181900360200190a16000805533ff5b6040805160076020808301919091528183018a905260608083018a90526001600160601b031989821b811660808501526094840189905260b484018890529086901b1660d483015260e88083018590528351808403909101815261010890920190925280519101206000541461097357600080fd5b336001600160a01b0386161461098857600080fd5b600a87014310158015610999575060015b6109a257600080fd5b34156109ad57600080fd5b6040516001600160a01b03861690303180156108fc02916000818181858888f193505050501580156109e3573d6000803e3d6000fd5b50604080513031815290517fc92018b4e91e597d736654f7b1d2ec034c5fec5920e2cfe22e15b4ddcdf5e18a9181900360200190a16000805533ff5b6040805160006020808301829052828401879052835180840385018152606090930190935281519190920120905414610a5757600080fd5b3481830114610a6557600080fd5b60408051303181526020810184905280820183905290517f219cc811755104876269c7553666684eaaeecb90b6a7ffc6fdd5068140059b8e9181900360600190a1604080516001602080830191909152438284015233606090811b9083015260748201949094526094808201939093528151808203909301835260b4019052805191012060005550565b6040805160076020808301919091528183018a905260608083018a90526001600160601b031989821b811660808501526094840189905260b484018890529086901b1660d483015260e880830185905283518084039091018152610108909201909252805191012060005414610b6457600080fd5b336001600160a01b03831614610b7957600080fd5b600a87014310610b8857600080fd5b3415610b9357600080fd5b604080513031815290517f3a6f8023909a26b76d462631fcdf570dbe3740447548e09470d1ad04394a0cec9181900360200190a1610bd885858585856001018b611373565b50505050505050565b6040805160056020808301919091528183018a90526001600160601b031960608a811b821681850152607484018a90526094840189905287901b1660b483015260c8820185905260e880830185905283518084039091018152610108909201909252805191012060005414610c5557600080fd5b336001600160a01b03871614610c6a57600080fd5b600a87014310158015610c7b575060015b610c8457600080fd5b3415610c8f57600080fd5b6040516001600160a01b03871690303180156108fc02916000818181858888f19350505050158015610cc5573d6000803e3d6000fd5b50604080513031815290517fd22b308a0739d4b2391b9fea991868a737c5ac9fca1931271dbb52121d7192ad9181900360200190a16000805533ff5b604080516001602080830191909152818301879052606086811b6001600160601b031916908301526074820185905260948083018590528351808403909101815260b4909201909252805191012060005414610d5c57600080fd5b600a84014310610d6b57600080fd5b348214610d7757600080fd5b604080513031815290517ff04f5fc87a72102f7c0b228f8bbaf9b9aa7a2b5dc295c86538fdde91e95866e99181900360200190a160408051600260208083019190915243828401526001600160601b0319606096871b1686830152607482019490945260948101929092523390931b60b4820152825160a881830301815260c8909101909252815191012060005550565b6040805160026020808301919091528183018890526001600160601b0319606088811b821681850152607484018890526094840187905285901b1660b4830152825160a881840301815260c8909201909252805191012060005414610e6c57600080fd5b336001600160a01b03821614610e8157600080fd5b600a85014310158015610e92575060015b610e9b57600080fd5b3415610ea657600080fd5b6040516001600160a01b03821690303180156108fc02916000818181858888f19350505050158015610edc573d6000803e3d6000fd5b50604080513031815290517f6b0cec327bdc12de4cddc524a17694480443eb633e915a1ec05f7b9fef3cce1e9181900360200190a16000805533ff5b6040805160056020808301919091528183018b90526001600160601b031960608b811b821681850152607484018b9052609484018a905288901b1660b483015260c8820186905260e880830186905283518084039091018152610108909201909252805191012060005414610f8c57600080fd5b336001600160a01b03851614610fa157600080fd5b600a88014310610fb057600080fd5b3415610fbb57600080fd5b60038110610fc857600080fd5b60408051303181526020810183905281517f1fa1ad895cc7ba9133068b14fd5b3d9ed6f96d3a535ff2be342493855f237b6b929181900390910190a160408051600660208083019190915243828401526001600160601b031960609a8b1b81168b840152607483019990995260948201979097529490971b90951660b484015260c883019190915260e8820193909352610108808201939093528351808203909301835261012801909252805191012060005550565b6040805160026020808301919091528183018890526001600160601b0319606088811b821681850152607484018890526094840187905285901b1660b4830152825160a881840301815260c89092019092528051910120600054146110e257600080fd5b336001600160a01b038516146110f757600080fd5b600a8501431061110657600080fd5b341561111157600080fd5b604080513031815290517f6fbec89a9bad4c7daaf5b053ac2c5ad4e0ff33c287295fe9a98cf7f3a3043f9c9181900360200190a16111558484848460006001611373565b5050505050565b6040805160066020808301919091528183018d90526001600160601b031960608d811b821681850152607484018d9052609484018c90528a901b1660b483015260c8820188905260e88201879052610108808301879052835180840390910181526101289092019092528051910120600054146111d857600080fd5b6111e06114c7565b336001600160a01b038b16146111f557600080fd5b600a8b01431061120457600080fd5b341561120f57600080fd5b60408051602080820186905281830185905282518083038401815260609092019092528051910120861461124257600080fd5b6003821061124f57600080fd5b600380831060208301819052908610604083015261126e576000611274565b80604001515b156112905760038560040383018161128857fe5b0681526112bb565b8060200151156112a357600281526112bb565b8060400151156112b657600081526112bb565b600181525b60408051303181526020810185905280820184905290517f5faf534620fe4d35c4670f2df8db5aff6901c4069d879904e0a4e11c119b42239181900360600190a15160408051600760208083019190915243828401526060808301949094526001600160601b03199c841b8d166080830152609482019b909b5260b481019990995296901b90981660d48701525060e88086019190915283518086039091018152610108909401909252505080519101206000555050565b61137b6114e7565b60018214156113ea5760408051600460208083019190915243828401526001600160601b031960608b811b821681850152607484018b9052609484018a905288901b1660b483015260c88083018790528351808403909101815260e89092019092528051910120600055610bd8565b6002821415611411576040805180820190915260028702815260006020820152815261144b565b81611434576040805180820190915260008152600287026020820152815261144b565b604080518082019091528681526020810187905281525b8051516040516001600160a01b03891691870180156108fc02916000818181858888f19350505050158015611484573d6000803e3d6000fd5b508051602001516040516001600160a01b0386169180156108fc02916000818181858888f193505050501580156114bf573d6000803e3d6000fd5b506000805533ff5b604080516060810182526000808252602082018190529181019190915290565b60405180602001604052806114fa6114ff565b905290565b60405180604001604052806000815260200160008152509056fea265627a7a72315820045a1c248f1de53f33a0be56c76c6ac095db0ed30b212896c64c0a7c73e3cd2f64736f6c634300050c0032` };