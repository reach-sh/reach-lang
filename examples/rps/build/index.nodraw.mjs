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
  const v12 = stdlib.eq(v9, v10);
  stdlib.assert(v12);
  const txn2 = await ctc.recv('A', 2, 0, 10);
  if (txn2.didTimeout) {
    
    const txn3 = await ctc.sendrecv('A', 12, 0, [v7, v5, v6], 0, false, null);
    const [] = txn3.data;
    const v22 = txn3.value;
    const v24 = stdlib.eq(0, v22);
    stdlib.assert(v24);
    stdlib.protect(stdlib.T_Null, await interact.endsWith('Bob quits'));
    
    return; }
  else {
    const [] = txn2.data;
    const v16 = txn2.from;
    const v17 = txn2.value;
    const v19 = stdlib.eq(v5, v17);
    stdlib.assert(v19);
    stdlib.protect(stdlib.T_Null, await interact.partnerIs(v16));
    
    
    const txn3 = await ctc.sendrecv('A', 3, 0, [v7, v5, v6, v16], 0, 10, null);
    if (txn3.didTimeout) {
      const txn4 = await ctc.recv('A', 11, 0, false);
      const [] = txn4.data;
      const v50 = txn4.value;
      const v52 = stdlib.eq(0, v50);
      stdlib.assert(v52);
      stdlib.protect(stdlib.T_Null, await interact.endsWith('Alice quits'));
      
      return; }
    else {
      const [] = txn3.data;
      const v45 = txn3.value;
      const v47 = stdlib.eq(0, v45);
      stdlib.assert(v47);
      let v71 = 0;
      let v72 = 1;
      while ((() => {
        const v96 = stdlib.eq(v72, 1);
        
        return v96; })()) {
        let v100;
        const v101 = stdlib.protect(stdlib.T_Bytes, await interact.getHand());
        const v103 = stdlib.bytes_eq(v101, 'ROCK');
        const v105 = stdlib.bytes_eq(v101, 'PAPER');
        const v107 = stdlib.bytes_eq(v101, 'SCISSORS');
        const v108 = v103 ? true : v105;
        const v109 = v108 ? true : v107;
        stdlib.assert(v109);
        if (v103) {
          v100 = 0;
           }
        else {
          if (v105) {
            v100 = 1;
             }
          else {
            v100 = 2;
             }
           }
        const v116 = stdlib.protect(stdlib.T_UInt256, await interact.random());
        const v117 = stdlib.keccak256(v116, v100);
        stdlib.protect(stdlib.T_Null, await interact.commits());
        
        
        const txn4 = await ctc.sendrecv('A', 5, 1, [v7, v5, v6, v16, v71, v117], 0, 10, null);
        if (txn4.didTimeout) {
          const txn5 = await ctc.recv('A', 10, 0, false);
          const [] = txn5.data;
          const v125 = txn5.value;
          const v127 = stdlib.eq(0, v125);
          stdlib.assert(v127);
          stdlib.protect(stdlib.T_Null, await interact.endsWith('Alice quits'));
          
          return; }
        else {
          const [v119] = txn4.data;
          const v120 = txn4.value;
          const v122 = stdlib.eq(0, v120);
          stdlib.assert(v122);
          const txn5 = await ctc.recv('A', 6, 1, 10);
          if (txn5.didTimeout) {
            
            const txn6 = await ctc.sendrecv('A', 9, 0, [v7, v5, v6, v16, v119, v71], 0, false, null);
            const [] = txn6.data;
            const v170 = txn6.value;
            const v172 = stdlib.eq(0, v170);
            stdlib.assert(v172);
            stdlib.protect(stdlib.T_Null, await interact.endsWith('Bob quits'));
            
            return; }
          else {
            const [v164] = txn5.data;
            const v165 = txn5.value;
            const v167 = stdlib.eq(0, v165);
            stdlib.assert(v167);
            const v192 = stdlib.le(0, v164);
            const v193 = stdlib.lt(v164, 3);
            const v194 = v192 ? v193 : false;
            stdlib.assert(v194);
            let v196;
            const v202 = stdlib.eq(v164, 0);
            if (v202) {
              v196 = 'ROCK';
               }
            else {
              const v204 = stdlib.eq(v164, 1);
              if (v204) {
                v196 = 'PAPER';
                 }
              else {
                v196 = 'SCISSORS';
                 }
               }
            stdlib.protect(stdlib.T_Null, await interact.reveals(v196));
            
            
            const txn6 = await ctc.sendrecv('A', 7, 2, [v7, v5, v6, v16, v119, v164, v71, v116, v100], 0, 10, null);
            if (txn6.didTimeout) {
              const txn7 = await ctc.recv('A', 8, 0, false);
              const [] = txn7.data;
              const v213 = txn7.value;
              const v215 = stdlib.eq(0, v213);
              stdlib.assert(v215);
              stdlib.protect(stdlib.T_Null, await interact.endsWith('Alice quits'));
              
              return; }
            else {
              const [v206, v207] = txn6.data;
              const v208 = txn6.value;
              const v210 = stdlib.eq(0, v208);
              stdlib.assert(v210);
              const v235 = stdlib.keccak256(v206, v207);
              const v237 = stdlib.eq(v119, v235);
              stdlib.assert(v237);
              const v239 = stdlib.le(0, v207);
              const v240 = stdlib.lt(v207, 3);
              const v241 = v239 ? v240 : false;
              stdlib.assert(v241);
              let v243;
              const v245 = stdlib.le(0, v207);
              const v246 = stdlib.lt(v207, 3);
              const v247 = v245 ? v246 : false;
              const v249 = stdlib.le(0, v164);
              const v250 = stdlib.lt(v164, 3);
              const v251 = v249 ? v250 : false;
              const v252 = v247 ? v251 : false;
              if (v252) {
                const v253 = stdlib.sub(4, v164);
                const v254 = stdlib.add(v207, v253);
                const v255 = stdlib.mod(v254, 3);
                v243 = v255;
                 }
              else {
                if (v247) {
                  v243 = 2;
                   }
                else {
                  if (v251) {
                    v243 = 0;
                     }
                  else {
                    v243 = 1;
                     }
                   }
                 }
              const v312 = stdlib.add(1, v71);
              v71 = v312;
              v72 = v243;
              continue; } } } }
      let v319;
      const v321 = stdlib.eq(v72, 2);
      if (v321) {
        const v322 = stdlib.mul(2, v5);
        v319 = [v322, 0];
         }
      else {
        const v324 = stdlib.eq(v72, 0);
        if (v324) {
          const v325 = stdlib.mul(2, v5);
          v319 = [0, v325];
           }
        else {
          v319 = [v5, v5];
           }
         }
      let v332;
      const v338 = stdlib.eq(v72, 0);
      if (v338) {
        v332 = 'Bob wins';
         }
      else {
        const v340 = stdlib.eq(v72, 1);
        if (v340) {
          v332 = 'Draw';
           }
        else {
          const v342 = stdlib.eq(v72, 2);
          if (v342) {
            v332 = 'Alice wins';
             }
          else {
            const v344 = stdlib.eq(v72, 3);
            if (v344) {
              v332 = 'Alice quits';
               }
            else {
              v332 = 'Bob quits';
               }
             }
           }
         }
      stdlib.protect(stdlib.T_Null, await interact.endsWith(v332));
      
      return; } } }
export async function B(stdlib, ctc, interact) {
  const txn0 = { balance: 0, value: 0 };
  const txn1 = await ctc.recv('B', 1, 2, false);
  const [v5, v6] = txn1.data;
  const v7 = txn1.from;
  const v9 = stdlib.add(v5, v6);
  const v10 = txn1.value;
  const v12 = stdlib.eq(v9, v10);
  stdlib.assert(v12);
  stdlib.protect(stdlib.T_Null, await interact.partnerIs(v7));
  stdlib.protect(stdlib.T_Null, await interact.acceptParams(v5, v6));
  
  
  const txn2 = await ctc.sendrecv('B', 2, 0, [v7, v5, v6], v5, 10, null);
  if (txn2.didTimeout) {
    const txn3 = await ctc.recv('B', 12, 0, false);
    const [] = txn3.data;
    const v22 = txn3.value;
    const v24 = stdlib.eq(0, v22);
    stdlib.assert(v24);
    stdlib.protect(stdlib.T_Null, await interact.endsWith('Bob quits'));
    
    return; }
  else {
    const [] = txn2.data;
    const v16 = txn2.from;
    const v17 = txn2.value;
    const v19 = stdlib.eq(v5, v17);
    stdlib.assert(v19);
    const txn3 = await ctc.recv('B', 3, 0, 10);
    if (txn3.didTimeout) {
      
      const txn4 = await ctc.sendrecv('B', 11, 0, [v7, v5, v6, v16], 0, false, null);
      const [] = txn4.data;
      const v50 = txn4.value;
      const v52 = stdlib.eq(0, v50);
      stdlib.assert(v52);
      stdlib.protect(stdlib.T_Null, await interact.endsWith('Alice quits'));
      
      return; }
    else {
      const [] = txn3.data;
      const v45 = txn3.value;
      const v47 = stdlib.eq(0, v45);
      stdlib.assert(v47);
      let v71 = 0;
      let v72 = 1;
      while ((() => {
        const v96 = stdlib.eq(v72, 1);
        
        return v96; })()) {
        const txn4 = await ctc.recv('B', 5, 1, 10);
        if (txn4.didTimeout) {
          
          const txn5 = await ctc.sendrecv('B', 10, 0, [v7, v5, v6, v16, v71], 0, false, null);
          const [] = txn5.data;
          const v125 = txn5.value;
          const v127 = stdlib.eq(0, v125);
          stdlib.assert(v127);
          stdlib.protect(stdlib.T_Null, await interact.endsWith('Alice quits'));
          
          return; }
        else {
          const [v119] = txn4.data;
          const v120 = txn4.value;
          const v122 = stdlib.eq(0, v120);
          stdlib.assert(v122);
          let v148;
          const v149 = stdlib.protect(stdlib.T_Bytes, await interact.getHand());
          const v151 = stdlib.bytes_eq(v149, 'ROCK');
          const v153 = stdlib.bytes_eq(v149, 'PAPER');
          const v155 = stdlib.bytes_eq(v149, 'SCISSORS');
          const v156 = v151 ? true : v153;
          const v157 = v156 ? true : v155;
          stdlib.assert(v157);
          if (v151) {
            v148 = 0;
             }
          else {
            if (v153) {
              v148 = 1;
               }
            else {
              v148 = 2;
               }
             }
          stdlib.protect(stdlib.T_Null, await interact.shows());
          
          
          const txn5 = await ctc.sendrecv('B', 6, 1, [v7, v5, v6, v16, v119, v71, v148], 0, 10, null);
          if (txn5.didTimeout) {
            const txn6 = await ctc.recv('B', 9, 0, false);
            const [] = txn6.data;
            const v170 = txn6.value;
            const v172 = stdlib.eq(0, v170);
            stdlib.assert(v172);
            stdlib.protect(stdlib.T_Null, await interact.endsWith('Bob quits'));
            
            return; }
          else {
            const [v164] = txn5.data;
            const v165 = txn5.value;
            const v167 = stdlib.eq(0, v165);
            stdlib.assert(v167);
            const v192 = stdlib.le(0, v164);
            const v193 = stdlib.lt(v164, 3);
            const v194 = v192 ? v193 : false;
            stdlib.assert(v194);
            const txn6 = await ctc.recv('B', 7, 2, 10);
            if (txn6.didTimeout) {
              
              const txn7 = await ctc.sendrecv('B', 8, 0, [v7, v5, v6, v16, v119, v164, v71], 0, false, null);
              const [] = txn7.data;
              const v213 = txn7.value;
              const v215 = stdlib.eq(0, v213);
              stdlib.assert(v215);
              stdlib.protect(stdlib.T_Null, await interact.endsWith('Alice quits'));
              
              return; }
            else {
              const [v206, v207] = txn6.data;
              const v208 = txn6.value;
              const v210 = stdlib.eq(0, v208);
              stdlib.assert(v210);
              const v235 = stdlib.keccak256(v206, v207);
              const v237 = stdlib.eq(v119, v235);
              stdlib.assert(v237);
              const v239 = stdlib.le(0, v207);
              const v240 = stdlib.lt(v207, 3);
              const v241 = v239 ? v240 : false;
              stdlib.assert(v241);
              let v243;
              const v245 = stdlib.le(0, v207);
              const v246 = stdlib.lt(v207, 3);
              const v247 = v245 ? v246 : false;
              const v249 = stdlib.le(0, v164);
              const v250 = stdlib.lt(v164, 3);
              const v251 = v249 ? v250 : false;
              const v252 = v247 ? v251 : false;
              if (v252) {
                const v253 = stdlib.sub(4, v164);
                const v254 = stdlib.add(v207, v253);
                const v255 = stdlib.mod(v254, 3);
                v243 = v255;
                 }
              else {
                if (v247) {
                  v243 = 2;
                   }
                else {
                  if (v251) {
                    v243 = 0;
                     }
                  else {
                    v243 = 1;
                     }
                   }
                 }
              const v312 = stdlib.add(1, v71);
              v71 = v312;
              v72 = v243;
              continue; } } } }
      let v319;
      const v321 = stdlib.eq(v72, 2);
      if (v321) {
        const v322 = stdlib.mul(2, v5);
        v319 = [v322, 0];
         }
      else {
        const v324 = stdlib.eq(v72, 0);
        if (v324) {
          const v325 = stdlib.mul(2, v5);
          v319 = [0, v325];
           }
        else {
          v319 = [v5, v5];
           }
         }
      let v347;
      const v353 = stdlib.eq(v72, 0);
      if (v353) {
        v347 = 'Bob wins';
         }
      else {
        const v355 = stdlib.eq(v72, 1);
        if (v355) {
          v347 = 'Draw';
           }
        else {
          const v357 = stdlib.eq(v72, 2);
          if (v357) {
            v347 = 'Alice wins';
             }
          else {
            const v359 = stdlib.eq(v72, 3);
            if (v359) {
              v347 = 'Alice quits';
               }
            else {
              v347 = 'Bob quits';
               }
             }
           }
         }
      stdlib.protect(stdlib.T_Null, await interact.endsWith(v347));
      
      return; } } }
export async function O(stdlib, ctc, interact) {
  const txn0 = { balance: 0, value: 0 };
  const txn1 = await ctc.recv('O', 1, 2, false);
  const [v5, v6] = txn1.data;
  const v7 = txn1.from;
  const v9 = stdlib.add(v5, v6);
  const v10 = txn1.value;
  const v12 = stdlib.eq(v9, v10);
  stdlib.assert(v12);
  const txn2 = await ctc.recv('O', 2, 0, 10);
  if (txn2.didTimeout) {
    const txn3 = await ctc.recv('O', 12, 0, false);
    const [] = txn3.data;
    const v22 = txn3.value;
    const v24 = stdlib.eq(0, v22);
    stdlib.assert(v24);
    return; }
  else {
    const [] = txn2.data;
    const v16 = txn2.from;
    const v17 = txn2.value;
    const v19 = stdlib.eq(v5, v17);
    stdlib.assert(v19);
    const txn3 = await ctc.recv('O', 3, 0, 10);
    if (txn3.didTimeout) {
      const txn4 = await ctc.recv('O', 11, 0, false);
      const [] = txn4.data;
      const v50 = txn4.value;
      const v52 = stdlib.eq(0, v50);
      stdlib.assert(v52);
      return; }
    else {
      const [] = txn3.data;
      const v45 = txn3.value;
      const v47 = stdlib.eq(0, v45);
      stdlib.assert(v47);
      let v71 = 0;
      let v72 = 1;
      while ((() => {
        const v96 = stdlib.eq(v72, 1);
        
        return v96; })()) {
        const txn4 = await ctc.recv('O', 5, 1, 10);
        if (txn4.didTimeout) {
          const txn5 = await ctc.recv('O', 10, 0, false);
          const [] = txn5.data;
          const v125 = txn5.value;
          const v127 = stdlib.eq(0, v125);
          stdlib.assert(v127);
          return; }
        else {
          const [v119] = txn4.data;
          const v120 = txn4.value;
          const v122 = stdlib.eq(0, v120);
          stdlib.assert(v122);
          const txn5 = await ctc.recv('O', 6, 1, 10);
          if (txn5.didTimeout) {
            const txn6 = await ctc.recv('O', 9, 0, false);
            const [] = txn6.data;
            const v170 = txn6.value;
            const v172 = stdlib.eq(0, v170);
            stdlib.assert(v172);
            return; }
          else {
            const [v164] = txn5.data;
            const v165 = txn5.value;
            const v167 = stdlib.eq(0, v165);
            stdlib.assert(v167);
            const v192 = stdlib.le(0, v164);
            const v193 = stdlib.lt(v164, 3);
            const v194 = v192 ? v193 : false;
            stdlib.assert(v194);
            const txn6 = await ctc.recv('O', 7, 2, 10);
            if (txn6.didTimeout) {
              const txn7 = await ctc.recv('O', 8, 0, false);
              const [] = txn7.data;
              const v213 = txn7.value;
              const v215 = stdlib.eq(0, v213);
              stdlib.assert(v215);
              return; }
            else {
              const [v206, v207] = txn6.data;
              const v208 = txn6.value;
              const v210 = stdlib.eq(0, v208);
              stdlib.assert(v210);
              const v235 = stdlib.keccak256(v206, v207);
              const v237 = stdlib.eq(v119, v235);
              stdlib.assert(v237);
              const v239 = stdlib.le(0, v207);
              const v240 = stdlib.lt(v207, 3);
              const v241 = v239 ? v240 : false;
              stdlib.assert(v241);
              let v243;
              const v245 = stdlib.le(0, v207);
              const v246 = stdlib.lt(v207, 3);
              const v247 = v245 ? v246 : false;
              const v249 = stdlib.le(0, v164);
              const v250 = stdlib.lt(v164, 3);
              const v251 = v249 ? v250 : false;
              const v252 = v247 ? v251 : false;
              if (v252) {
                const v253 = stdlib.sub(4, v164);
                const v254 = stdlib.add(v207, v253);
                const v255 = stdlib.mod(v254, 3);
                v243 = v255;
                 }
              else {
                if (v247) {
                  v243 = 2;
                   }
                else {
                  if (v251) {
                    v243 = 0;
                     }
                  else {
                    v243 = 1;
                     }
                   }
                 }
              const v312 = stdlib.add(1, v71);
              v71 = v312;
              v72 = v243;
              continue; } } } }
      let v319;
      const v321 = stdlib.eq(v72, 2);
      if (v321) {
        const v322 = stdlib.mul(2, v5);
        v319 = [v322, 0];
         }
      else {
        const v324 = stdlib.eq(v72, 0);
        if (v324) {
          const v325 = stdlib.mul(2, v5);
          v319 = [0, v325];
           }
        else {
          v319 = [v5, v5];
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
          "name": "v119",
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
          "name": "v164",
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
          "name": "v206",
          "type": "uint256"
        },
        {
          "indexed": false,
          "internalType": "uint256",
          "name": "v207",
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
          "name": "v16",
          "type": "address"
        },
        {
          "internalType": "uint256",
          "name": "v71",
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
          "name": "v16",
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
          "name": "v16",
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
          "name": "v16",
          "type": "address"
        },
        {
          "internalType": "uint256",
          "name": "v71",
          "type": "uint256"
        },
        {
          "internalType": "uint256",
          "name": "v119",
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
          "name": "v16",
          "type": "address"
        },
        {
          "internalType": "uint256",
          "name": "v119",
          "type": "uint256"
        },
        {
          "internalType": "uint256",
          "name": "v71",
          "type": "uint256"
        },
        {
          "internalType": "uint256",
          "name": "v164",
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
          "name": "v16",
          "type": "address"
        },
        {
          "internalType": "uint256",
          "name": "v119",
          "type": "uint256"
        },
        {
          "internalType": "uint256",
          "name": "v164",
          "type": "uint256"
        },
        {
          "internalType": "uint256",
          "name": "v71",
          "type": "uint256"
        },
        {
          "internalType": "uint256",
          "name": "v206",
          "type": "uint256"
        },
        {
          "internalType": "uint256",
          "name": "v207",
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
          "name": "v16",
          "type": "address"
        },
        {
          "internalType": "uint256",
          "name": "v119",
          "type": "uint256"
        },
        {
          "internalType": "uint256",
          "name": "v164",
          "type": "uint256"
        },
        {
          "internalType": "uint256",
          "name": "v71",
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
          "name": "v16",
          "type": "address"
        },
        {
          "internalType": "uint256",
          "name": "v119",
          "type": "uint256"
        },
        {
          "internalType": "uint256",
          "name": "v71",
          "type": "uint256"
        }
      ],
      "name": "m9",
      "outputs": [],
      "stateMutability": "payable",
      "type": "function"
    }
  ]`,
  Bytecode: `0x608060405261001160004360a0610031565b60408051601f19818403018152919052805160209091012060005561003f565b918252602082015260400190565b61114b8061004e6000396000f3fe60806040526004361061009c5760003560e01c8063ab793da411610064578063ab793da414610102578063ae24d93c14610115578063c4532a9d14610128578063c98760c81461013b578063e68876941461014e578063f15594ca146101615761009c565b80630e6801d1146100a15780634f1f764d146100b65780637a52ccb3146100c9578063882ab247146100dc5780639ccddd3a146100ef575b600080fd5b6100b46100af366004610e44565b610174565b005b6100b46100c4366004610deb565b610264565b6100b46100d7366004610f93565b610356565b6100b46100ea366004610d9a565b610413565b6100b46100fd366004610d5f565b6104f4565b6100b4610110366004610ea8565b6105c3565b6100b4610123366004610ea8565b6106c5565b6100b4610136366004610d9a565b6107ac565b6100b4610149366004610e44565b61086b565b6100b461015c366004610f14565b610950565b6100b461016f366004610d5f565b610adc565b60048787878787876040516020016101929796959493929190611035565b6040516020818303038152906040528051906020012060001c600054146101b857600080fd5b336001600160a01b038716146101cd57600080fd5b600a870143106101dc57600080fd5b34156101e757600080fd5b7fabf482d77b67111a4971bb96fe81961f83ba459eb1d8fa9f78b6908251aeef1a4782604051610218929190610fc7565b60405180910390a160054387878787868860405160200161024098979695949392919061106e565b60408051601f19818403018152919052805160209091012060005550505050505050565b60048686868686866040516020016102829796959493929190611035565b6040516020818303038152906040528051906020012060001c600054146102a857600080fd5b336001600160a01b038316146102bd57600080fd5b600a860143101580156102ce575060015b6102d757600080fd5b34156102e257600080fd5b6040516001600160a01b038316904780156108fc02916000818181858888f19350505050158015610317573d6000803e3d6000fd5b507f9bf9cf9ae88051b33b19923b1c1cf36013b840c9975de29305d444b55d83c6bd476040516103479190610fbe565b60405180910390a16000805533ff5b60008360405160200161036a929190610fc7565b6040516020818303038152906040528051906020012060001c6000541461039057600080fd5b348183011461039e57600080fd5b7f219cc811755104876269c7553666684eaaeecb90b6a7ffc6fdd5068140059b8e4783836040516103d1939291906110ff565b60405180910390a16001433384846040516020016103f3959493929190610fd5565b60408051601f198184030181529190528051602090910120600055505050565b6002858585858560405160200161042f96959493929190611001565b6040516020818303038152906040528051906020012060001c6000541461045557600080fd5b336001600160a01b0382161461046a57600080fd5b600a8501431015801561047b575060015b61048457600080fd5b341561048f57600080fd5b6040516001600160a01b038216904780156108fc02916000818181858888f193505050501580156104c4573d6000803e3d6000fd5b507fd22b308a0739d4b2391b9fea991868a737c5ac9fca1931271dbb52121d7192ad476040516103479190610fbe565b60018484848460405160200161050e959493929190610fd5565b6040516020818303038152906040528051906020012060001c6000541461053457600080fd5b600a8401431061054357600080fd5b34821461054f57600080fd5b7ff04f5fc87a72102f7c0b228f8bbaf9b9aa7a2b5dc295c86538fdde91e95866e94760405161057e9190610fbe565b60405180910390a1600243848484336040516020016105a296959493929190611001565b60408051601f19818403018152919052805160209091012060005550505050565b6005888888888888886040516020016105e398979695949392919061106e565b6040516020818303038152906040528051906020012060001c6000541461060957600080fd5b336001600160a01b0385161461061e57600080fd5b600a8801431061062d57600080fd5b341561063857600080fd5b6003811061064557600080fd5b7f1fa1ad895cc7ba9133068b14fd5b3d9ed6f96d3a535ff2be342493855f237b6b4782604051610676929190610fc7565b60405180910390a1600643888888888887896040516020016106a0999897969594939291906110b2565b60408051601f1981840301815291905280516020909101206000555050505050505050565b600688888888888888886040516020016106e7999897969594939291906110b2565b6040516020818303038152906040528051906020012060001c6000541461070d57600080fd5b336001600160a01b0385161461072257600080fd5b600a88014310158015610733575060015b61073c57600080fd5b341561074757600080fd5b6040516001600160a01b038516904780156108fc02916000818181858888f1935050505015801561077c573d6000803e3d6000fd5b507f3a6f8023909a26b76d462631fcdf570dbe3740447548e09470d1ad04394a0cec476040516103479190610fbe565b600285858585856040516020016107c896959493929190611001565b6040516020818303038152906040528051906020012060001c600054146107ee57600080fd5b336001600160a01b0385161461080357600080fd5b600a8501431061081257600080fd5b341561081d57600080fd5b7f6fbec89a9bad4c7daaf5b053ac2c5ad4e0ff33c287295fe9a98cf7f3a3043f9c4760405161084c9190610fbe565b60405180910390a16108648484848460006001610bbb565b5050505050565b60058787878787878760405160200161088b98979695949392919061106e565b6040516020818303038152906040528051906020012060001c600054146108b157600080fd5b336001600160a01b038716146108c657600080fd5b600a870143101580156108d7575060015b6108e057600080fd5b34156108eb57600080fd5b6040516001600160a01b038716904780156108fc02916000818181858888f19350505050158015610920573d6000803e3d6000fd5b507fc92018b4e91e597d736654f7b1d2ec034c5fec5920e2cfe22e15b4ddcdf5e18a476040516103479190610fbe565b60068a8a8a8a8a8a8a8a604051602001610972999897969594939291906110b2565b6040516020818303038152906040528051906020012060001c6000541461099857600080fd5b6109a0610cf0565b336001600160a01b038b16146109b557600080fd5b600a8b0143106109c457600080fd5b34156109cf57600080fd5b82826040516020016109e2929190610fc7565b6040516020818303038152906040528051906020012060001c8614610a0657600080fd5b60038210610a1357600080fd5b6003808310602083018190529086106040830152610a32576000610a38565b80604001515b15610a5457600385600403830181610a4c57fe5b068152610a7f565b806020015115610a675760028152610a7f565b806040015115610a7a5760008152610a7f565b600181525b7f5faf534620fe4d35c4670f2df8db5aff6901c4069d879904e0a4e11c119b4223478484604051610ab2939291906110ff565b60405180910390a1610acf8a8a8a8a886001018660000151610bbb565b5050505050505050505050565b600184848484604051602001610af6959493929190610fd5565b6040516020818303038152906040528051906020012060001c60005414610b1c57600080fd5b336001600160a01b03841614610b3157600080fd5b600a84014310158015610b42575060015b610b4b57600080fd5b3415610b5657600080fd5b6040516001600160a01b038416904780156108fc02916000818181858888f19350505050158015610b8b573d6000803e3d6000fd5b507f0f5f4d65cf2c85506eee21a3fb54b49eb1fdb9267bbd430782deebd67e6a3639476040516103479190610fbe565b610bc3610d10565b6001821415610c0a576004438888888888604051602001610bea9796959493929190611035565b60408051601f198184030181529190528051602090910120600055610ce7565b6002821415610c315760408051808201909152600287028152600060208201528152610c6b565b81610c545760408051808201909152600081526002870260208201528152610c6b565b604080518082019091528681526020810187905281525b8051516040516001600160a01b03891691870180156108fc02916000818181858888f19350505050158015610ca4573d6000803e3d6000fd5b508051602001516040516001600160a01b0386169180156108fc02916000818181858888f19350505050158015610cdf573d6000803e3d6000fd5b506000805533ff5b50505050505050565b604080516060810182526000808252602082018190529181019190915290565b6040518060200160405280610d23610d28565b905290565b604051806040016040528060008152602001600081525090565b80356001600160a01b0381168114610d5957600080fd5b92915050565b60008060008060808587031215610d74578384fd5b84359350610d858660208701610d42565b93969395505050506040820135916060013590565b600080600080600060a08688031215610db1578081fd5b85359450610dc28760208801610d42565b93506040860135925060608601359150610ddf8760808801610d42565b90509295509295909350565b60008060008060008060c08789031215610e03578081fd5b86359550610e148860208901610d42565b94506040870135935060608701359250610e318860808901610d42565b915060a087013590509295509295509295565b600080600080600080600060e0888a031215610e5e578081fd5b87359650610e6f8960208a01610d42565b95506040880135945060608801359350610e8c8960808a01610d42565b925060a0880135915060c0880135905092959891949750929550565b600080600080600080600080610100898b031215610ec4578081fd5b88359750610ed58a60208b01610d42565b96506040890135955060608901359450610ef28a60808b01610d42565b979a969950949793969560a0850135955060c08501359460e001359350915050565b6000806000806000806000806000806101408b8d031215610f33578182fd5b8a359950610f448c60208d01610d42565b985060408b0135975060608b01359650610f618c60808d01610d42565b999c989b50969995989760a0870135975060c08701359660e08101359650610100810135955061012001359350915050565b600080600060608486031215610fa7578283fd5b505081359360208301359350604090920135919050565b90815260200190565b918252602082015260400190565b94855260208501939093526001600160a01b039190911660408401526060830152608082015260a00190565b95865260208601949094526001600160a01b039283166040860152606085019190915260808401521660a082015260c00190565b96875260208701959095526001600160a01b039384166040870152606086019290925260808501521660a083015260c082015260e00190565b97885260208801969096526001600160a01b0394851660408801526060870193909352608086019190915290911660a084015260c083015260e08201526101000190565b98895260208901979097526001600160a01b0395861660408901526060880194909452608087019290925290921660a085015260c084019190915260e08301526101008201526101200190565b928352602083019190915260408201526060019056fea264697066735822122026eec5513e1b32e4ddb59ce6fae9f559f7ad2b86fb1f780b8945e42e42073c7964736f6c63430007000033` };

export const _Connectors = {
  ETH: _ETH };
