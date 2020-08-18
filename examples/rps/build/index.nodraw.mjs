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

const _ETH = {
  ABI: `[
    {
      "inputs": [],
      "stateMutability": "payable",
      "type": "constructor"
    },
    {
      "anonymous": false,
      "inputs": [
        {
          "indexed": false,
          "internalType": "uint256",
          "name": "_bal",
          "type": "uint256"
        },
        {
          "indexed": false,
          "internalType": "uint256",
          "name": "v5",
          "type": "uint256"
        },
        {
          "indexed": false,
          "internalType": "uint256",
          "name": "v6",
          "type": "uint256"
        }
      ],
      "name": "e1",
      "type": "event"
    },
    {
      "anonymous": false,
      "inputs": [
        {
          "indexed": false,
          "internalType": "uint256",
          "name": "_bal",
          "type": "uint256"
        }
      ],
      "name": "e10",
      "type": "event"
    },
    {
      "anonymous": false,
      "inputs": [
        {
          "indexed": false,
          "internalType": "uint256",
          "name": "_bal",
          "type": "uint256"
        }
      ],
      "name": "e11",
      "type": "event"
    },
    {
      "anonymous": false,
      "inputs": [
        {
          "indexed": false,
          "internalType": "uint256",
          "name": "_bal",
          "type": "uint256"
        }
      ],
      "name": "e12",
      "type": "event"
    },
    {
      "anonymous": false,
      "inputs": [
        {
          "indexed": false,
          "internalType": "uint256",
          "name": "_bal",
          "type": "uint256"
        }
      ],
      "name": "e2",
      "type": "event"
    },
    {
      "anonymous": false,
      "inputs": [
        {
          "indexed": false,
          "internalType": "uint256",
          "name": "_bal",
          "type": "uint256"
        }
      ],
      "name": "e3",
      "type": "event"
    },
    {
      "anonymous": false,
      "inputs": [
        {
          "indexed": false,
          "internalType": "uint256",
          "name": "_bal",
          "type": "uint256"
        },
        {
          "indexed": false,
          "internalType": "uint256",
          "name": "v102",
          "type": "uint256"
        }
      ],
      "name": "e5",
      "type": "event"
    },
    {
      "anonymous": false,
      "inputs": [
        {
          "indexed": false,
          "internalType": "uint256",
          "name": "_bal",
          "type": "uint256"
        },
        {
          "indexed": false,
          "internalType": "uint256",
          "name": "v139",
          "type": "uint256"
        }
      ],
      "name": "e6",
      "type": "event"
    },
    {
      "anonymous": false,
      "inputs": [
        {
          "indexed": false,
          "internalType": "uint256",
          "name": "_bal",
          "type": "uint256"
        },
        {
          "indexed": false,
          "internalType": "uint256",
          "name": "v173",
          "type": "uint256"
        },
        {
          "indexed": false,
          "internalType": "uint256",
          "name": "v174",
          "type": "uint256"
        }
      ],
      "name": "e7",
      "type": "event"
    },
    {
      "anonymous": false,
      "inputs": [
        {
          "indexed": false,
          "internalType": "uint256",
          "name": "_bal",
          "type": "uint256"
        }
      ],
      "name": "e8",
      "type": "event"
    },
    {
      "anonymous": false,
      "inputs": [
        {
          "indexed": false,
          "internalType": "uint256",
          "name": "_bal",
          "type": "uint256"
        }
      ],
      "name": "e9",
      "type": "event"
    },
    {
      "inputs": [
        {
          "internalType": "uint256",
          "name": "_last",
          "type": "uint256"
        },
        {
          "internalType": "uint256",
          "name": "v5",
          "type": "uint256"
        },
        {
          "internalType": "uint256",
          "name": "v6",
          "type": "uint256"
        }
      ],
      "name": "m1",
      "outputs": [],
      "stateMutability": "payable",
      "type": "function"
    },
    {
      "inputs": [
        {
          "internalType": "uint256",
          "name": "_last",
          "type": "uint256"
        },
        {
          "internalType": "address payable",
          "name": "v7",
          "type": "address"
        },
        {
          "internalType": "uint256",
          "name": "v5",
          "type": "uint256"
        },
        {
          "internalType": "uint256",
          "name": "v6",
          "type": "uint256"
        },
        {
          "internalType": "address payable",
          "name": "v15",
          "type": "address"
        },
        {
          "internalType": "uint256",
          "name": "v54",
          "type": "uint256"
        }
      ],
      "name": "m10",
      "outputs": [],
      "stateMutability": "payable",
      "type": "function"
    },
    {
      "inputs": [
        {
          "internalType": "uint256",
          "name": "_last",
          "type": "uint256"
        },
        {
          "internalType": "address payable",
          "name": "v7",
          "type": "address"
        },
        {
          "internalType": "uint256",
          "name": "v5",
          "type": "uint256"
        },
        {
          "internalType": "uint256",
          "name": "v6",
          "type": "uint256"
        },
        {
          "internalType": "address payable",
          "name": "v15",
          "type": "address"
        }
      ],
      "name": "m11",
      "outputs": [],
      "stateMutability": "payable",
      "type": "function"
    },
    {
      "inputs": [
        {
          "internalType": "uint256",
          "name": "_last",
          "type": "uint256"
        },
        {
          "internalType": "address payable",
          "name": "v7",
          "type": "address"
        },
        {
          "internalType": "uint256",
          "name": "v5",
          "type": "uint256"
        },
        {
          "internalType": "uint256",
          "name": "v6",
          "type": "uint256"
        }
      ],
      "name": "m12",
      "outputs": [],
      "stateMutability": "payable",
      "type": "function"
    },
    {
      "inputs": [
        {
          "internalType": "uint256",
          "name": "_last",
          "type": "uint256"
        },
        {
          "internalType": "address payable",
          "name": "v7",
          "type": "address"
        },
        {
          "internalType": "uint256",
          "name": "v5",
          "type": "uint256"
        },
        {
          "internalType": "uint256",
          "name": "v6",
          "type": "uint256"
        }
      ],
      "name": "m2",
      "outputs": [],
      "stateMutability": "payable",
      "type": "function"
    },
    {
      "inputs": [
        {
          "internalType": "uint256",
          "name": "_last",
          "type": "uint256"
        },
        {
          "internalType": "address payable",
          "name": "v7",
          "type": "address"
        },
        {
          "internalType": "uint256",
          "name": "v5",
          "type": "uint256"
        },
        {
          "internalType": "uint256",
          "name": "v6",
          "type": "uint256"
        },
        {
          "internalType": "address payable",
          "name": "v15",
          "type": "address"
        }
      ],
      "name": "m3",
      "outputs": [],
      "stateMutability": "payable",
      "type": "function"
    },
    {
      "inputs": [
        {
          "internalType": "uint256",
          "name": "_last",
          "type": "uint256"
        },
        {
          "internalType": "address payable",
          "name": "v7",
          "type": "address"
        },
        {
          "internalType": "uint256",
          "name": "v5",
          "type": "uint256"
        },
        {
          "internalType": "uint256",
          "name": "v6",
          "type": "uint256"
        },
        {
          "internalType": "address payable",
          "name": "v15",
          "type": "address"
        },
        {
          "internalType": "uint256",
          "name": "v54",
          "type": "uint256"
        },
        {
          "internalType": "uint256",
          "name": "v102",
          "type": "uint256"
        }
      ],
      "name": "m5",
      "outputs": [],
      "stateMutability": "payable",
      "type": "function"
    },
    {
      "inputs": [
        {
          "internalType": "uint256",
          "name": "_last",
          "type": "uint256"
        },
        {
          "internalType": "address payable",
          "name": "v7",
          "type": "address"
        },
        {
          "internalType": "uint256",
          "name": "v5",
          "type": "uint256"
        },
        {
          "internalType": "uint256",
          "name": "v6",
          "type": "uint256"
        },
        {
          "internalType": "address payable",
          "name": "v15",
          "type": "address"
        },
        {
          "internalType": "uint256",
          "name": "v102",
          "type": "uint256"
        },
        {
          "internalType": "uint256",
          "name": "v54",
          "type": "uint256"
        },
        {
          "internalType": "uint256",
          "name": "v139",
          "type": "uint256"
        }
      ],
      "name": "m6",
      "outputs": [],
      "stateMutability": "payable",
      "type": "function"
    },
    {
      "inputs": [
        {
          "internalType": "uint256",
          "name": "_last",
          "type": "uint256"
        },
        {
          "internalType": "address payable",
          "name": "v7",
          "type": "address"
        },
        {
          "internalType": "uint256",
          "name": "v5",
          "type": "uint256"
        },
        {
          "internalType": "uint256",
          "name": "v6",
          "type": "uint256"
        },
        {
          "internalType": "address payable",
          "name": "v15",
          "type": "address"
        },
        {
          "internalType": "uint256",
          "name": "v102",
          "type": "uint256"
        },
        {
          "internalType": "uint256",
          "name": "v139",
          "type": "uint256"
        },
        {
          "internalType": "uint256",
          "name": "v54",
          "type": "uint256"
        },
        {
          "internalType": "uint256",
          "name": "v173",
          "type": "uint256"
        },
        {
          "internalType": "uint256",
          "name": "v174",
          "type": "uint256"
        }
      ],
      "name": "m7",
      "outputs": [],
      "stateMutability": "payable",
      "type": "function"
    },
    {
      "inputs": [
        {
          "internalType": "uint256",
          "name": "_last",
          "type": "uint256"
        },
        {
          "internalType": "address payable",
          "name": "v7",
          "type": "address"
        },
        {
          "internalType": "uint256",
          "name": "v5",
          "type": "uint256"
        },
        {
          "internalType": "uint256",
          "name": "v6",
          "type": "uint256"
        },
        {
          "internalType": "address payable",
          "name": "v15",
          "type": "address"
        },
        {
          "internalType": "uint256",
          "name": "v102",
          "type": "uint256"
        },
        {
          "internalType": "uint256",
          "name": "v139",
          "type": "uint256"
        },
        {
          "internalType": "uint256",
          "name": "v54",
          "type": "uint256"
        }
      ],
      "name": "m8",
      "outputs": [],
      "stateMutability": "payable",
      "type": "function"
    },
    {
      "inputs": [
        {
          "internalType": "uint256",
          "name": "_last",
          "type": "uint256"
        },
        {
          "internalType": "address payable",
          "name": "v7",
          "type": "address"
        },
        {
          "internalType": "uint256",
          "name": "v5",
          "type": "uint256"
        },
        {
          "internalType": "uint256",
          "name": "v6",
          "type": "uint256"
        },
        {
          "internalType": "address payable",
          "name": "v15",
          "type": "address"
        },
        {
          "internalType": "uint256",
          "name": "v102",
          "type": "uint256"
        },
        {
          "internalType": "uint256",
          "name": "v54",
          "type": "uint256"
        }
      ],
      "name": "m9",
      "outputs": [],
      "stateMutability": "payable",
      "type": "function"
    }
  ]`,
  Bytecode: `0x608060405261001160004360a0610031565b60408051601f19818403018152919052805160209091012060005561003f565b918252602082015260400190565b61114b8061004e6000396000f3fe60806040526004361061009c5760003560e01c8063ab793da411610064578063ab793da414610102578063ae24d93c14610115578063c4532a9d14610128578063c98760c81461013b578063e68876941461014e578063f15594ca146101615761009c565b80630e6801d1146100a15780634f1f764d146100b65780637a52ccb3146100c9578063882ab247146100dc5780639ccddd3a146100ef575b600080fd5b6100b46100af366004610e44565b610174565b005b6100b46100c4366004610deb565b610264565b6100b46100d7366004610f93565b610356565b6100b46100ea366004610d9a565b610413565b6100b46100fd366004610d5f565b6104f4565b6100b4610110366004610ea8565b6105c3565b6100b4610123366004610ea8565b6106c5565b6100b4610136366004610d9a565b6107ac565b6100b4610149366004610e44565b61086b565b6100b461015c366004610f14565b610950565b6100b461016f366004610d5f565b610adc565b60048787878787876040516020016101929796959493929190611035565b6040516020818303038152906040528051906020012060001c600054146101b857600080fd5b336001600160a01b038716146101cd57600080fd5b600a870143106101dc57600080fd5b34156101e757600080fd5b7fabf482d77b67111a4971bb96fe81961f83ba459eb1d8fa9f78b6908251aeef1a4782604051610218929190610fc7565b60405180910390a160054387878787868860405160200161024098979695949392919061106e565b60408051601f19818403018152919052805160209091012060005550505050505050565b60048686868686866040516020016102829796959493929190611035565b6040516020818303038152906040528051906020012060001c600054146102a857600080fd5b336001600160a01b038316146102bd57600080fd5b600a860143101580156102ce575060015b6102d757600080fd5b34156102e257600080fd5b6040516001600160a01b038316904780156108fc02916000818181858888f19350505050158015610317573d6000803e3d6000fd5b507f9bf9cf9ae88051b33b19923b1c1cf36013b840c9975de29305d444b55d83c6bd476040516103479190610fbe565b60405180910390a16000805533ff5b60008360405160200161036a929190610fc7565b6040516020818303038152906040528051906020012060001c6000541461039057600080fd5b348183011461039e57600080fd5b7f219cc811755104876269c7553666684eaaeecb90b6a7ffc6fdd5068140059b8e4783836040516103d1939291906110ff565b60405180910390a16001433384846040516020016103f3959493929190610fd5565b60408051601f198184030181529190528051602090910120600055505050565b6002858585858560405160200161042f96959493929190611001565b6040516020818303038152906040528051906020012060001c6000541461045557600080fd5b336001600160a01b0382161461046a57600080fd5b600a8501431015801561047b575060015b61048457600080fd5b341561048f57600080fd5b6040516001600160a01b038216904780156108fc02916000818181858888f193505050501580156104c4573d6000803e3d6000fd5b507fd22b308a0739d4b2391b9fea991868a737c5ac9fca1931271dbb52121d7192ad476040516103479190610fbe565b60018484848460405160200161050e959493929190610fd5565b6040516020818303038152906040528051906020012060001c6000541461053457600080fd5b600a8401431061054357600080fd5b34821461054f57600080fd5b7ff04f5fc87a72102f7c0b228f8bbaf9b9aa7a2b5dc295c86538fdde91e95866e94760405161057e9190610fbe565b60405180910390a1600243848484336040516020016105a296959493929190611001565b60408051601f19818403018152919052805160209091012060005550505050565b6005888888888888886040516020016105e398979695949392919061106e565b6040516020818303038152906040528051906020012060001c6000541461060957600080fd5b336001600160a01b0385161461061e57600080fd5b600a8801431061062d57600080fd5b341561063857600080fd5b6003811061064557600080fd5b7f1fa1ad895cc7ba9133068b14fd5b3d9ed6f96d3a535ff2be342493855f237b6b4782604051610676929190610fc7565b60405180910390a1600643888888888887896040516020016106a0999897969594939291906110b2565b60408051601f1981840301815291905280516020909101206000555050505050505050565b600688888888888888886040516020016106e7999897969594939291906110b2565b6040516020818303038152906040528051906020012060001c6000541461070d57600080fd5b336001600160a01b0385161461072257600080fd5b600a88014310158015610733575060015b61073c57600080fd5b341561074757600080fd5b6040516001600160a01b038516904780156108fc02916000818181858888f1935050505015801561077c573d6000803e3d6000fd5b507f3a6f8023909a26b76d462631fcdf570dbe3740447548e09470d1ad04394a0cec476040516103479190610fbe565b600285858585856040516020016107c896959493929190611001565b6040516020818303038152906040528051906020012060001c600054146107ee57600080fd5b336001600160a01b0385161461080357600080fd5b600a8501431061081257600080fd5b341561081d57600080fd5b7f6fbec89a9bad4c7daaf5b053ac2c5ad4e0ff33c287295fe9a98cf7f3a3043f9c4760405161084c9190610fbe565b60405180910390a16108648484848460006001610bbb565b5050505050565b60058787878787878760405160200161088b98979695949392919061106e565b6040516020818303038152906040528051906020012060001c600054146108b157600080fd5b336001600160a01b038716146108c657600080fd5b600a870143101580156108d7575060015b6108e057600080fd5b34156108eb57600080fd5b6040516001600160a01b038716904780156108fc02916000818181858888f19350505050158015610920573d6000803e3d6000fd5b507fc92018b4e91e597d736654f7b1d2ec034c5fec5920e2cfe22e15b4ddcdf5e18a476040516103479190610fbe565b60068a8a8a8a8a8a8a8a604051602001610972999897969594939291906110b2565b6040516020818303038152906040528051906020012060001c6000541461099857600080fd5b6109a0610cf0565b336001600160a01b038b16146109b557600080fd5b600a8b0143106109c457600080fd5b34156109cf57600080fd5b82826040516020016109e2929190610fc7565b6040516020818303038152906040528051906020012060001c8614610a0657600080fd5b60038210610a1357600080fd5b6003808310602083018190529086106040830152610a32576000610a38565b80604001515b15610a5457600385600403830181610a4c57fe5b068152610a7f565b806020015115610a675760028152610a7f565b806040015115610a7a5760008152610a7f565b600181525b7f5faf534620fe4d35c4670f2df8db5aff6901c4069d879904e0a4e11c119b4223478484604051610ab2939291906110ff565b60405180910390a1610acf8a8a8a8a886001018660000151610bbb565b5050505050505050505050565b600184848484604051602001610af6959493929190610fd5565b6040516020818303038152906040528051906020012060001c60005414610b1c57600080fd5b336001600160a01b03841614610b3157600080fd5b600a84014310158015610b42575060015b610b4b57600080fd5b3415610b5657600080fd5b6040516001600160a01b038416904780156108fc02916000818181858888f19350505050158015610b8b573d6000803e3d6000fd5b507f0f5f4d65cf2c85506eee21a3fb54b49eb1fdb9267bbd430782deebd67e6a3639476040516103479190610fbe565b610bc3610d10565b6001821415610c0a576004438888888888604051602001610bea9796959493929190611035565b60408051601f198184030181529190528051602090910120600055610ce7565b6002821415610c315760408051808201909152600287028152600060208201528152610c6b565b81610c545760408051808201909152600081526002870260208201528152610c6b565b604080518082019091528681526020810187905281525b8051516040516001600160a01b03891691870180156108fc02916000818181858888f19350505050158015610ca4573d6000803e3d6000fd5b508051602001516040516001600160a01b0386169180156108fc02916000818181858888f19350505050158015610cdf573d6000803e3d6000fd5b506000805533ff5b50505050505050565b604080516060810182526000808252602082018190529181019190915290565b6040518060200160405280610d23610d28565b905290565b604051806040016040528060008152602001600081525090565b80356001600160a01b0381168114610d5957600080fd5b92915050565b60008060008060808587031215610d74578384fd5b84359350610d858660208701610d42565b93969395505050506040820135916060013590565b600080600080600060a08688031215610db1578081fd5b85359450610dc28760208801610d42565b93506040860135925060608601359150610ddf8760808801610d42565b90509295509295909350565b60008060008060008060c08789031215610e03578081fd5b86359550610e148860208901610d42565b94506040870135935060608701359250610e318860808901610d42565b915060a087013590509295509295509295565b600080600080600080600060e0888a031215610e5e578081fd5b87359650610e6f8960208a01610d42565b95506040880135945060608801359350610e8c8960808a01610d42565b925060a0880135915060c0880135905092959891949750929550565b600080600080600080600080610100898b031215610ec4578081fd5b88359750610ed58a60208b01610d42565b96506040890135955060608901359450610ef28a60808b01610d42565b979a969950949793969560a0850135955060c08501359460e001359350915050565b6000806000806000806000806000806101408b8d031215610f33578182fd5b8a359950610f448c60208d01610d42565b985060408b0135975060608b01359650610f618c60808d01610d42565b999c989b50969995989760a0870135975060c08701359660e08101359650610100810135955061012001359350915050565b600080600060608486031215610fa7578283fd5b505081359360208301359350604090920135919050565b90815260200190565b918252602082015260400190565b94855260208501939093526001600160a01b039190911660408401526060830152608082015260a00190565b95865260208601949094526001600160a01b039283166040860152606085019190915260808401521660a082015260c00190565b96875260208701959095526001600160a01b039384166040870152606086019290925260808501521660a083015260c082015260e00190565b97885260208801969096526001600160a01b0394851660408801526060870193909352608086019190915290911660a084015260c083015260e08201526101000190565b98895260208901979097526001600160a01b0395861660408901526060880194909452608087019290925290921660a085015260c084019190915260e08301526101008201526101200190565b928352602083019190915260408201526060019056fea2646970667358221220d93a96655682d78692634bf9e363f6b9cadeda7f275081f7fe590a2ab209493164736f6c63430007000033` };

export const _Connectors = {
  ETH: _ETH };
