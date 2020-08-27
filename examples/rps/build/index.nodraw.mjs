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
          "components": [
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
          "internalType": "struct ReachContract.a1",
          "name": "_a",
          "type": "tuple"
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
          "components": [
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
          "internalType": "struct ReachContract.a10",
          "name": "_a",
          "type": "tuple"
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
          "components": [
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
          "internalType": "struct ReachContract.a11",
          "name": "_a",
          "type": "tuple"
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
          "components": [
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
          "internalType": "struct ReachContract.a12",
          "name": "_a",
          "type": "tuple"
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
          "components": [
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
          "internalType": "struct ReachContract.a2",
          "name": "_a",
          "type": "tuple"
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
          "components": [
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
          "internalType": "struct ReachContract.a3",
          "name": "_a",
          "type": "tuple"
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
          "components": [
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
          "internalType": "struct ReachContract.a5",
          "name": "_a",
          "type": "tuple"
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
          "components": [
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
          "internalType": "struct ReachContract.a6",
          "name": "_a",
          "type": "tuple"
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
          "components": [
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
          "internalType": "struct ReachContract.a7",
          "name": "_a",
          "type": "tuple"
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
          "components": [
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
          "internalType": "struct ReachContract.a8",
          "name": "_a",
          "type": "tuple"
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
          "components": [
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
          "internalType": "struct ReachContract.a9",
          "name": "_a",
          "type": "tuple"
        }
      ],
      "name": "m9",
      "outputs": [],
      "stateMutability": "payable",
      "type": "function"
    }
  ]`,
  Bytecode: `0x608060405261001160004360a0610031565b60408051601f19818403018152919052805160209091012060005561003f565b918252602082015260400190565b6114108061004e6000396000f3fe60806040526004361061009c5760003560e01c8063669a15e811610064578063669a15e8146101025780636d7cf075146101155780637d1a25bb146101285780639d7312991461013b578063c3f855771461014e578063cc68dcb7146101615761009c565b80630d838328146100a15780632b4ec70a146100b65780634336d4ae146100c957806358262512146100dc5780635abe6ad1146100ef575b600080fd5b6100b46100af36600461120e565b610174565b005b6100b46100c43660046111e2565b610274565b6100b46100d73660046111f3565b6103bd565b6100b46100ea366004611255565b6104f1565b6100b46100fd36600461123a565b610637565b6100b46101103660046111f3565b610779565b6100b4610123366004611255565b6108d9565b6100b461013636600461123a565b610a38565b6100b4610149366004611271565b610b78565b6100b461015c36600461120e565b610dd7565b6100b461016f366004611229565b610efa565b6001813561018860408401602085016111b4565b836040013584606001356040516020016101a695949392919061129a565b6040516020818303038152906040528051906020012060001c600054146101cc57600080fd5b8035600a0143106101dc57600080fd5b348160400135146101ec57600080fd5b7ff04f5fc87a72102f7c0b228f8bbaf9b9aa7a2b5dc295c86538fdde91e95866e94760405161021b9190611283565b60405180910390a160024361023660408401602085016111b4565b8360400135846060013533604051602001610256969594939291906112c6565b60408051601f19818403018152919052805160209091012060005550565b6004813561028860408401602085016111b4565b604084013560608501356102a260a08701608088016111b4565b8660a001356040516020016102bd97969594939291906112fa565b6040516020818303038152906040528051906020012060001c600054146102e357600080fd5b6102f360a08201608083016111b4565b6001600160a01b0316336001600160a01b03161461031057600080fd5b8035600a014310801590610322575060015b61032b57600080fd5b341561033657600080fd5b61034660a08201608083016111b4565b6001600160a01b03166108fc479081150290604051600060405180830381858888f1935050505015801561037e573d6000803e3d6000fd5b507f9bf9cf9ae88051b33b19923b1c1cf36013b840c9975de29305d444b55d83c6bd476040516103ae9190611283565b60405180910390a16000805533ff5b600281356103d160408401602085016111b4565b604084013560608501356103eb60a08701608088016111b4565b604051602001610400969594939291906112c6565b6040516020818303038152906040528051906020012060001c6000541461042657600080fd5b61043660a08201608083016111b4565b6001600160a01b0316336001600160a01b03161461045357600080fd5b8035600a014310801590610465575060015b61046e57600080fd5b341561047957600080fd5b61048960a08201608083016111b4565b6001600160a01b03166108fc479081150290604051600060405180830381858888f193505050501580156104c1573d6000803e3d6000fd5b507fd22b308a0739d4b2391b9fea991868a737c5ac9fca1931271dbb52121d7192ad476040516103ae9190611283565b6006813561050560408401602085016111b4565b6040840135606085013561051f60a08701608088016111b4565b8660a001358760c001358860e0013560405160200161054699989796959493929190611377565b6040516020818303038152906040528051906020012060001c6000541461056c57600080fd5b61057c60a08201608083016111b4565b6001600160a01b0316336001600160a01b03161461059957600080fd5b8035600a0143108015906105ab575060015b6105b457600080fd5b34156105bf57600080fd5b6105cf60a08201608083016111b4565b6001600160a01b03166108fc479081150290604051600060405180830381858888f19350505050158015610607573d6000803e3d6000fd5b507f3a6f8023909a26b76d462631fcdf570dbe3740447548e09470d1ad04394a0cec476040516103ae9190611283565b6004813561064b60408401602085016111b4565b6040840135606085013561066560a08701608088016111b4565b8660a0013560405160200161068097969594939291906112fa565b6040516020818303038152906040528051906020012060001c600054146106a657600080fd5b6106b660408201602083016111b4565b6001600160a01b0316336001600160a01b0316146106d357600080fd5b8035600a0143106106e357600080fd5b34156106ee57600080fd5b7fabf482d77b67111a4971bb96fe81961f83ba459eb1d8fa9f78b6908251aeef1a478260c0013560405161072392919061128c565b60405180910390a160054361073e60408401602085016111b4565b6040840135606085013561075860a08701608088016111b4565b8660c001358760a00135604051602001610256989796959493929190611333565b6002813561078d60408401602085016111b4565b604084013560608501356107a760a08701608088016111b4565b6040516020016107bc969594939291906112c6565b6040516020818303038152906040528051906020012060001c600054146107e257600080fd5b6107f260408201602083016111b4565b6001600160a01b0316336001600160a01b03161461080f57600080fd5b8035600a01431061081f57600080fd5b341561082a57600080fd5b7f6fbec89a9bad4c7daaf5b053ac2c5ad4e0ff33c287295fe9a98cf7f3a3043f9c476040516108599190611283565b60405180910390a16108d66040518060c0016040528083602001602081019061088291906111b4565b6001600160a01b0316815260200183604001358152602001836060013581526020018360800160208101906108b791906111b4565b6001600160a01b03168152602001600081526020016001815250610fb0565b50565b600581356108ed60408401602085016111b4565b6040840135606085013561090760a08701608088016111b4565b8660a001358760c00135604051602001610928989796959493929190611333565b6040516020818303038152906040528051906020012060001c6000541461094e57600080fd5b61095e60a08201608083016111b4565b6001600160a01b0316336001600160a01b03161461097b57600080fd5b8035600a01431061098b57600080fd5b341561099657600080fd5b60038160e00135106109a757600080fd5b7f1fa1ad895cc7ba9133068b14fd5b3d9ed6f96d3a535ff2be342493855f237b6b478260e001356040516109dc92919061128c565b60405180910390a16006436109f760408401602085016111b4565b60408401356060850135610a1160a08701608088016111b4565b8660a001358760e001358860c0013560405160200161025699989796959493929190611377565b60058135610a4c60408401602085016111b4565b60408401356060850135610a6660a08701608088016111b4565b8660a001358760c00135604051602001610a87989796959493929190611333565b6040516020818303038152906040528051906020012060001c60005414610aad57600080fd5b610abd60408201602083016111b4565b6001600160a01b0316336001600160a01b031614610ada57600080fd5b8035600a014310801590610aec575060015b610af557600080fd5b3415610b0057600080fd5b610b1060408201602083016111b4565b6001600160a01b03166108fc479081150290604051600060405180830381858888f19350505050158015610b48573d6000803e3d6000fd5b507fc92018b4e91e597d736654f7b1d2ec034c5fec5920e2cfe22e15b4ddcdf5e18a476040516103ae9190611283565b60068135610b8c60408401602085016111b4565b60408401356060850135610ba660a08701608088016111b4565b8660a001358760c001358860e00135604051602001610bcd99989796959493929190611377565b6040516020818303038152906040528051906020012060001c60005414610bf357600080fd5b610bfb611117565b610c0b60408301602084016111b4565b6001600160a01b0316336001600160a01b031614610c2857600080fd5b8135600a014310610c3857600080fd5b3415610c4357600080fd5b816101000135826101200135604051602001610c6092919061128c565b6040516020818303038152906040528051906020012060001c8260a0013514610c8857600080fd5b600382610120013510610c9a57600080fd5b600361012083013581116020830181905260c0840135919091106040830152610cc4576000610cca565b80604001515b15610ce857600361012083013560c084013560040301068152610d13565b806020015115610cfb5760028152610d13565b806040015115610d0e5760008152610d13565b600181525b7f5faf534620fe4d35c4670f2df8db5aff6901c4069d879904e0a4e11c119b422347836101000135846101200135604051610d50939291906113c4565b60405180910390a1610dd36040518060c00160405280846020016020810190610d7991906111b4565b6001600160a01b031681526020018460400135815260200184606001358152602001846080016020810190610dae91906111b4565b6001600160a01b03168152600160e08601350160208201528351604090910152610fb0565b5050565b60018135610deb60408401602085016111b4565b83604001358460600135604051602001610e0995949392919061129a565b6040516020818303038152906040528051906020012060001c60005414610e2f57600080fd5b610e3f60408201602083016111b4565b6001600160a01b0316336001600160a01b031614610e5c57600080fd5b8035600a014310801590610e6e575060015b610e7757600080fd5b3415610e8257600080fd5b610e9260408201602083016111b4565b6001600160a01b03166108fc479081150290604051600060405180830381858888f19350505050158015610eca573d6000803e3d6000fd5b507f0f5f4d65cf2c85506eee21a3fb54b49eb1fdb9267bbd430782deebd67e6a3639476040516103ae9190611283565b604051610f0f9060009083359060200161128c565b6040516020818303038152906040528051906020012060001c60005414610f3557600080fd5b34816040013582602001350114610f4b57600080fd5b7f219cc811755104876269c7553666684eaaeecb90b6a7ffc6fdd5068140059b8e4782602001358360400135604051610f86939291906113c4565b60405180910390a1600143338360200135846040013560405160200161025695949392919061129a565b610fb8611137565b60018260a0015114156110175760044383600001518460200151856040015186606001518760800151604051602001610ff797969594939291906112fa565b60408051601f198184030181529190528051602090910120600055610dd3565b60028260a00151141561104657604080518082019091526020808401516002028252600090820152815261108c565b60a082015161107157604080518082019091526000815260208381015160020290820152815261108c565b60408051808201909152602080840180518352519082015281525b815181515160408085015190516001600160a01b0390931692910180156108fc02916000818181858888f193505050501580156110cd573d6000803e3d6000fd5b5060608201518151602001516040516001600160a01b039092169181156108fc0291906000818181858888f1935050505015801561110f573d6000803e3d6000fd5b506000805533ff5b604080516060810182526000808252602082018190529181019190915290565b604051806020016040528061114a61114f565b905290565b604051806040016040528060008152602001600081525090565b600060a0828403121561117a578081fd5b50919050565b60006080828403121561117a578081fd5b600060e0828403121561117a578081fd5b6000610100828403121561117a578081fd5b6000602082840312156111c5578081fd5b81356001600160a01b03811681146111db578182fd5b9392505050565b600060c0828403121561117a578081fd5b600060a08284031215611204578081fd5b6111db8383611169565b60006080828403121561121f578081fd5b6111db8383611180565b60006060828403121561117a578081fd5b600060e0828403121561124b578081fd5b6111db8383611191565b60006101008284031215611267578081fd5b6111db83836111a2565b6000610140828403121561117a578081fd5b90815260200190565b918252602082015260400190565b94855260208501939093526001600160a01b039190911660408401526060830152608082015260a00190565b95865260208601949094526001600160a01b039283166040860152606085019190915260808401521660a082015260c00190565b96875260208701959095526001600160a01b039384166040870152606086019290925260808501521660a083015260c082015260e00190565b97885260208801969096526001600160a01b0394851660408801526060870193909352608086019190915290911660a084015260c083015260e08201526101000190565b98895260208901979097526001600160a01b0395861660408901526060880194909452608087019290925290921660a085015260c084019190915260e08301526101008201526101200190565b928352602083019190915260408201526060019056fea264697066735822122055b5f2ad2e757d2832d2966b99c76ac06e6601fa79d7cdb0342b28d59638f44564736f6c63430007000033` };

export const _Connectors = {
  ETH: _ETH };
