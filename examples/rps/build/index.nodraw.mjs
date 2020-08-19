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
    stdlib.assert(true);
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
      const v52 = txn4.value;
      const v54 = stdlib.eq(0, v52);
      stdlib.assert(v54);
      stdlib.assert(true);
      stdlib.protect(stdlib.T_Null, await interact.endsWith('Alice quits'));
      
      return; }
    else {
      const [] = txn3.data;
      const v47 = txn3.value;
      const v49 = stdlib.eq(0, v47);
      stdlib.assert(v49);
      let v75 = 0;
      let v76 = 1;
      while ((() => {
        const v104 = stdlib.eq(v76, 1);
        
        return v104; })()) {
        let v108;
        const v109 = stdlib.protect(stdlib.T_Bytes, await interact.getHand());
        const v111 = stdlib.bytes_eq(v109, 'ROCK');
        const v113 = stdlib.bytes_eq(v109, 'PAPER');
        const v115 = stdlib.bytes_eq(v109, 'SCISSORS');
        const v117 = v111 ? true : v113;
        const v119 = v117 ? true : v115;
        stdlib.assert(v119);
        if (v111) {
          v108 = 0;
           }
        else {
          if (v113) {
            v108 = 1;
             }
          else {
            v108 = 2;
             }
           }
        const v127 = stdlib.protect(stdlib.T_UInt256, await interact.random());
        const v128 = stdlib.keccak256(v127, v108);
        stdlib.protect(stdlib.T_Null, await interact.commits());
        
        
        const txn4 = await ctc.sendrecv('A', 5, 1, [v7, v5, v6, v16, v75, v128], 0, 10, null);
        if (txn4.didTimeout) {
          const txn5 = await ctc.recv('A', 10, 0, false);
          const [] = txn5.data;
          const v136 = txn5.value;
          const v138 = stdlib.eq(0, v136);
          stdlib.assert(v138);
          stdlib.assert(true);
          stdlib.protect(stdlib.T_Null, await interact.endsWith('Alice quits'));
          
          return; }
        else {
          const [v130] = txn4.data;
          const v131 = txn4.value;
          const v133 = stdlib.eq(0, v131);
          stdlib.assert(v133);
          const txn5 = await ctc.recv('A', 6, 1, 10);
          if (txn5.didTimeout) {
            
            const txn6 = await ctc.sendrecv('A', 9, 0, [v7, v5, v6, v16, v130, v75], 0, false, null);
            const [] = txn6.data;
            const v186 = txn6.value;
            const v188 = stdlib.eq(0, v186);
            stdlib.assert(v188);
            stdlib.assert(true);
            stdlib.protect(stdlib.T_Null, await interact.endsWith('Bob quits'));
            
            return; }
          else {
            const [v180] = txn5.data;
            const v181 = txn5.value;
            const v183 = stdlib.eq(0, v181);
            stdlib.assert(v183);
            const v210 = stdlib.le(0, v180);
            const v211 = stdlib.lt(v180, 3);
            const v213 = v210 ? v211 : false;
            stdlib.assert(v213);
            let v215;
            const v217 = stdlib.le(0, v180);
            const v218 = stdlib.lt(v180, 3);
            const v220 = v217 ? v218 : false;
            stdlib.assert(v220);
            const v222 = stdlib.eq(v180, 0);
            if (v222) {
              v215 = 'ROCK';
               }
            else {
              const v224 = stdlib.eq(v180, 1);
              if (v224) {
                v215 = 'PAPER';
                 }
              else {
                v215 = 'SCISSORS';
                 }
               }
            stdlib.protect(stdlib.T_Null, await interact.reveals(v215));
            
            
            const txn6 = await ctc.sendrecv('A', 7, 2, [v7, v5, v6, v16, v130, v180, v75, v127, v108], 0, 10, null);
            if (txn6.didTimeout) {
              const txn7 = await ctc.recv('A', 8, 0, false);
              const [] = txn7.data;
              const v233 = txn7.value;
              const v235 = stdlib.eq(0, v233);
              stdlib.assert(v235);
              stdlib.assert(true);
              stdlib.protect(stdlib.T_Null, await interact.endsWith('Alice quits'));
              
              return; }
            else {
              const [v226, v227] = txn6.data;
              const v228 = txn6.value;
              const v230 = stdlib.eq(0, v228);
              stdlib.assert(v230);
              const v257 = stdlib.keccak256(v226, v227);
              const v259 = stdlib.eq(v130, v257);
              stdlib.assert(v259);
              const v261 = stdlib.le(0, v227);
              const v262 = stdlib.lt(v227, 3);
              const v264 = v261 ? v262 : false;
              stdlib.assert(v264);
              let v266;
              const v268 = stdlib.le(0, v227);
              const v269 = stdlib.lt(v227, 3);
              const v271 = v268 ? v269 : false;
              const v273 = stdlib.le(0, v180);
              const v274 = stdlib.lt(v180, 3);
              const v276 = v273 ? v274 : false;
              const v278 = v271 ? v276 : false;
              if (v278) {
                const v279 = stdlib.sub(4, v180);
                const v280 = stdlib.add(v227, v279);
                const v281 = stdlib.mod(v280, 3);
                v266 = v281;
                 }
              else {
                if (v271) {
                  v266 = 2;
                   }
                else {
                  if (v276) {
                    v266 = 0;
                     }
                  else {
                    v266 = 1;
                     }
                   }
                 }
              const v349 = stdlib.add(1, v75);
              v75 = v349;
              v76 = v266;
              continue; } } } }
      let v356;
      const v358 = stdlib.eq(v76, 2);
      if (v358) {
        const v359 = stdlib.mul(2, v5);
        v356 = [v359, 0];
         }
      else {
        const v361 = stdlib.eq(v76, 0);
        if (v361) {
          const v362 = stdlib.mul(2, v5);
          v356 = [0, v362];
           }
        else {
          v356 = [v5, v5];
           }
         }
      let v369;
      const v371 = stdlib.le(0, v76);
      const v372 = stdlib.lt(v76, 5);
      const v374 = v371 ? v372 : false;
      stdlib.assert(v374);
      const v376 = stdlib.eq(v76, 0);
      if (v376) {
        v369 = 'Bob wins';
         }
      else {
        const v378 = stdlib.eq(v76, 1);
        if (v378) {
          v369 = 'Draw';
           }
        else {
          const v380 = stdlib.eq(v76, 2);
          if (v380) {
            v369 = 'Alice wins';
             }
          else {
            const v382 = stdlib.eq(v76, 3);
            if (v382) {
              v369 = 'Alice quits';
               }
            else {
              v369 = 'Bob quits';
               }
             }
           }
         }
      stdlib.protect(stdlib.T_Null, await interact.endsWith(v369));
      
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
    stdlib.assert(true);
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
      const v52 = txn4.value;
      const v54 = stdlib.eq(0, v52);
      stdlib.assert(v54);
      stdlib.assert(true);
      stdlib.protect(stdlib.T_Null, await interact.endsWith('Alice quits'));
      
      return; }
    else {
      const [] = txn3.data;
      const v47 = txn3.value;
      const v49 = stdlib.eq(0, v47);
      stdlib.assert(v49);
      let v75 = 0;
      let v76 = 1;
      while ((() => {
        const v104 = stdlib.eq(v76, 1);
        
        return v104; })()) {
        const txn4 = await ctc.recv('B', 5, 1, 10);
        if (txn4.didTimeout) {
          
          const txn5 = await ctc.sendrecv('B', 10, 0, [v7, v5, v6, v16, v75], 0, false, null);
          const [] = txn5.data;
          const v136 = txn5.value;
          const v138 = stdlib.eq(0, v136);
          stdlib.assert(v138);
          stdlib.assert(true);
          stdlib.protect(stdlib.T_Null, await interact.endsWith('Alice quits'));
          
          return; }
        else {
          const [v130] = txn4.data;
          const v131 = txn4.value;
          const v133 = stdlib.eq(0, v131);
          stdlib.assert(v133);
          let v161;
          const v162 = stdlib.protect(stdlib.T_Bytes, await interact.getHand());
          const v164 = stdlib.bytes_eq(v162, 'ROCK');
          const v166 = stdlib.bytes_eq(v162, 'PAPER');
          const v168 = stdlib.bytes_eq(v162, 'SCISSORS');
          const v170 = v164 ? true : v166;
          const v172 = v170 ? true : v168;
          stdlib.assert(v172);
          if (v164) {
            v161 = 0;
             }
          else {
            if (v166) {
              v161 = 1;
               }
            else {
              v161 = 2;
               }
             }
          stdlib.protect(stdlib.T_Null, await interact.shows());
          
          
          const txn5 = await ctc.sendrecv('B', 6, 1, [v7, v5, v6, v16, v130, v75, v161], 0, 10, null);
          if (txn5.didTimeout) {
            const txn6 = await ctc.recv('B', 9, 0, false);
            const [] = txn6.data;
            const v186 = txn6.value;
            const v188 = stdlib.eq(0, v186);
            stdlib.assert(v188);
            stdlib.assert(true);
            stdlib.protect(stdlib.T_Null, await interact.endsWith('Bob quits'));
            
            return; }
          else {
            const [v180] = txn5.data;
            const v181 = txn5.value;
            const v183 = stdlib.eq(0, v181);
            stdlib.assert(v183);
            const v210 = stdlib.le(0, v180);
            const v211 = stdlib.lt(v180, 3);
            const v213 = v210 ? v211 : false;
            stdlib.assert(v213);
            const txn6 = await ctc.recv('B', 7, 2, 10);
            if (txn6.didTimeout) {
              
              const txn7 = await ctc.sendrecv('B', 8, 0, [v7, v5, v6, v16, v130, v180, v75], 0, false, null);
              const [] = txn7.data;
              const v233 = txn7.value;
              const v235 = stdlib.eq(0, v233);
              stdlib.assert(v235);
              stdlib.assert(true);
              stdlib.protect(stdlib.T_Null, await interact.endsWith('Alice quits'));
              
              return; }
            else {
              const [v226, v227] = txn6.data;
              const v228 = txn6.value;
              const v230 = stdlib.eq(0, v228);
              stdlib.assert(v230);
              const v257 = stdlib.keccak256(v226, v227);
              const v259 = stdlib.eq(v130, v257);
              stdlib.assert(v259);
              const v261 = stdlib.le(0, v227);
              const v262 = stdlib.lt(v227, 3);
              const v264 = v261 ? v262 : false;
              stdlib.assert(v264);
              let v266;
              const v268 = stdlib.le(0, v227);
              const v269 = stdlib.lt(v227, 3);
              const v271 = v268 ? v269 : false;
              const v273 = stdlib.le(0, v180);
              const v274 = stdlib.lt(v180, 3);
              const v276 = v273 ? v274 : false;
              const v278 = v271 ? v276 : false;
              if (v278) {
                const v279 = stdlib.sub(4, v180);
                const v280 = stdlib.add(v227, v279);
                const v281 = stdlib.mod(v280, 3);
                v266 = v281;
                 }
              else {
                if (v271) {
                  v266 = 2;
                   }
                else {
                  if (v276) {
                    v266 = 0;
                     }
                  else {
                    v266 = 1;
                     }
                   }
                 }
              const v349 = stdlib.add(1, v75);
              v75 = v349;
              v76 = v266;
              continue; } } } }
      let v356;
      const v358 = stdlib.eq(v76, 2);
      if (v358) {
        const v359 = stdlib.mul(2, v5);
        v356 = [v359, 0];
         }
      else {
        const v361 = stdlib.eq(v76, 0);
        if (v361) {
          const v362 = stdlib.mul(2, v5);
          v356 = [0, v362];
           }
        else {
          v356 = [v5, v5];
           }
         }
      let v385;
      const v387 = stdlib.le(0, v76);
      const v388 = stdlib.lt(v76, 5);
      const v390 = v387 ? v388 : false;
      stdlib.assert(v390);
      const v392 = stdlib.eq(v76, 0);
      if (v392) {
        v385 = 'Bob wins';
         }
      else {
        const v394 = stdlib.eq(v76, 1);
        if (v394) {
          v385 = 'Draw';
           }
        else {
          const v396 = stdlib.eq(v76, 2);
          if (v396) {
            v385 = 'Alice wins';
             }
          else {
            const v398 = stdlib.eq(v76, 3);
            if (v398) {
              v385 = 'Alice quits';
               }
            else {
              v385 = 'Bob quits';
               }
             }
           }
         }
      stdlib.protect(stdlib.T_Null, await interact.endsWith(v385));
      
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
      const v52 = txn4.value;
      const v54 = stdlib.eq(0, v52);
      stdlib.assert(v54);
      return; }
    else {
      const [] = txn3.data;
      const v47 = txn3.value;
      const v49 = stdlib.eq(0, v47);
      stdlib.assert(v49);
      let v75 = 0;
      let v76 = 1;
      while ((() => {
        const v104 = stdlib.eq(v76, 1);
        
        return v104; })()) {
        const txn4 = await ctc.recv('O', 5, 1, 10);
        if (txn4.didTimeout) {
          const txn5 = await ctc.recv('O', 10, 0, false);
          const [] = txn5.data;
          const v136 = txn5.value;
          const v138 = stdlib.eq(0, v136);
          stdlib.assert(v138);
          return; }
        else {
          const [v130] = txn4.data;
          const v131 = txn4.value;
          const v133 = stdlib.eq(0, v131);
          stdlib.assert(v133);
          const txn5 = await ctc.recv('O', 6, 1, 10);
          if (txn5.didTimeout) {
            const txn6 = await ctc.recv('O', 9, 0, false);
            const [] = txn6.data;
            const v186 = txn6.value;
            const v188 = stdlib.eq(0, v186);
            stdlib.assert(v188);
            return; }
          else {
            const [v180] = txn5.data;
            const v181 = txn5.value;
            const v183 = stdlib.eq(0, v181);
            stdlib.assert(v183);
            const v210 = stdlib.le(0, v180);
            const v211 = stdlib.lt(v180, 3);
            const v213 = v210 ? v211 : false;
            stdlib.assert(v213);
            const txn6 = await ctc.recv('O', 7, 2, 10);
            if (txn6.didTimeout) {
              const txn7 = await ctc.recv('O', 8, 0, false);
              const [] = txn7.data;
              const v233 = txn7.value;
              const v235 = stdlib.eq(0, v233);
              stdlib.assert(v235);
              return; }
            else {
              const [v226, v227] = txn6.data;
              const v228 = txn6.value;
              const v230 = stdlib.eq(0, v228);
              stdlib.assert(v230);
              const v257 = stdlib.keccak256(v226, v227);
              const v259 = stdlib.eq(v130, v257);
              stdlib.assert(v259);
              const v261 = stdlib.le(0, v227);
              const v262 = stdlib.lt(v227, 3);
              const v264 = v261 ? v262 : false;
              stdlib.assert(v264);
              let v266;
              const v268 = stdlib.le(0, v227);
              const v269 = stdlib.lt(v227, 3);
              const v271 = v268 ? v269 : false;
              const v273 = stdlib.le(0, v180);
              const v274 = stdlib.lt(v180, 3);
              const v276 = v273 ? v274 : false;
              const v278 = v271 ? v276 : false;
              if (v278) {
                const v279 = stdlib.sub(4, v180);
                const v280 = stdlib.add(v227, v279);
                const v281 = stdlib.mod(v280, 3);
                v266 = v281;
                 }
              else {
                if (v271) {
                  v266 = 2;
                   }
                else {
                  if (v276) {
                    v266 = 0;
                     }
                  else {
                    v266 = 1;
                     }
                   }
                 }
              const v349 = stdlib.add(1, v75);
              v75 = v349;
              v76 = v266;
              continue; } } } }
      let v356;
      const v358 = stdlib.eq(v76, 2);
      if (v358) {
        const v359 = stdlib.mul(2, v5);
        v356 = [v359, 0];
         }
      else {
        const v361 = stdlib.eq(v76, 0);
        if (v361) {
          const v362 = stdlib.mul(2, v5);
          v356 = [0, v362];
           }
        else {
          v356 = [v5, v5];
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
          "name": "v130",
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
          "name": "v180",
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
          "name": "v226",
          "type": "uint256"
        },
        {
          "indexed": false,
          "internalType": "uint256",
          "name": "v227",
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
          "name": "v75",
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
          "name": "v75",
          "type": "uint256"
        },
        {
          "internalType": "uint256",
          "name": "v130",
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
          "name": "v130",
          "type": "uint256"
        },
        {
          "internalType": "uint256",
          "name": "v75",
          "type": "uint256"
        },
        {
          "internalType": "uint256",
          "name": "v180",
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
          "name": "v130",
          "type": "uint256"
        },
        {
          "internalType": "uint256",
          "name": "v180",
          "type": "uint256"
        },
        {
          "internalType": "uint256",
          "name": "v75",
          "type": "uint256"
        },
        {
          "internalType": "uint256",
          "name": "v226",
          "type": "uint256"
        },
        {
          "internalType": "uint256",
          "name": "v227",
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
          "name": "v130",
          "type": "uint256"
        },
        {
          "internalType": "uint256",
          "name": "v180",
          "type": "uint256"
        },
        {
          "internalType": "uint256",
          "name": "v75",
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
          "name": "v130",
          "type": "uint256"
        },
        {
          "internalType": "uint256",
          "name": "v75",
          "type": "uint256"
        }
      ],
      "name": "m9",
      "outputs": [],
      "stateMutability": "payable",
      "type": "function"
    }
  ]`,
  Bytecode: `0x608060405261001160004360a0610031565b60408051601f19818403018152919052805160209091012060005561003f565b918252602082015260400190565b61114b8061004e6000396000f3fe60806040526004361061009c5760003560e01c8063ab793da411610064578063ab793da414610102578063ae24d93c14610115578063c4532a9d14610128578063c98760c81461013b578063e68876941461014e578063f15594ca146101615761009c565b80630e6801d1146100a15780634f1f764d146100b65780637a52ccb3146100c9578063882ab247146100dc5780639ccddd3a146100ef575b600080fd5b6100b46100af366004610e44565b610174565b005b6100b46100c4366004610deb565b610264565b6100b46100d7366004610f93565b610356565b6100b46100ea366004610d9a565b610413565b6100b46100fd366004610d5f565b6104f4565b6100b4610110366004610ea8565b6105c3565b6100b4610123366004610ea8565b6106c5565b6100b4610136366004610d9a565b6107ac565b6100b4610149366004610e44565b61086b565b6100b461015c366004610f14565b610950565b6100b461016f366004610d5f565b610adc565b60048787878787876040516020016101929796959493929190611035565b6040516020818303038152906040528051906020012060001c600054146101b857600080fd5b336001600160a01b038716146101cd57600080fd5b600a870143106101dc57600080fd5b34156101e757600080fd5b7fabf482d77b67111a4971bb96fe81961f83ba459eb1d8fa9f78b6908251aeef1a4782604051610218929190610fc7565b60405180910390a160054387878787868860405160200161024098979695949392919061106e565b60408051601f19818403018152919052805160209091012060005550505050505050565b60048686868686866040516020016102829796959493929190611035565b6040516020818303038152906040528051906020012060001c600054146102a857600080fd5b336001600160a01b038316146102bd57600080fd5b600a860143101580156102ce575060015b6102d757600080fd5b34156102e257600080fd5b6040516001600160a01b038316904780156108fc02916000818181858888f19350505050158015610317573d6000803e3d6000fd5b507f9bf9cf9ae88051b33b19923b1c1cf36013b840c9975de29305d444b55d83c6bd476040516103479190610fbe565b60405180910390a16000805533ff5b60008360405160200161036a929190610fc7565b6040516020818303038152906040528051906020012060001c6000541461039057600080fd5b348183011461039e57600080fd5b7f219cc811755104876269c7553666684eaaeecb90b6a7ffc6fdd5068140059b8e4783836040516103d1939291906110ff565b60405180910390a16001433384846040516020016103f3959493929190610fd5565b60408051601f198184030181529190528051602090910120600055505050565b6002858585858560405160200161042f96959493929190611001565b6040516020818303038152906040528051906020012060001c6000541461045557600080fd5b336001600160a01b0382161461046a57600080fd5b600a8501431015801561047b575060015b61048457600080fd5b341561048f57600080fd5b6040516001600160a01b038216904780156108fc02916000818181858888f193505050501580156104c4573d6000803e3d6000fd5b507fd22b308a0739d4b2391b9fea991868a737c5ac9fca1931271dbb52121d7192ad476040516103479190610fbe565b60018484848460405160200161050e959493929190610fd5565b6040516020818303038152906040528051906020012060001c6000541461053457600080fd5b600a8401431061054357600080fd5b34821461054f57600080fd5b7ff04f5fc87a72102f7c0b228f8bbaf9b9aa7a2b5dc295c86538fdde91e95866e94760405161057e9190610fbe565b60405180910390a1600243848484336040516020016105a296959493929190611001565b60408051601f19818403018152919052805160209091012060005550505050565b6005888888888888886040516020016105e398979695949392919061106e565b6040516020818303038152906040528051906020012060001c6000541461060957600080fd5b336001600160a01b0385161461061e57600080fd5b600a8801431061062d57600080fd5b341561063857600080fd5b6003811061064557600080fd5b7f1fa1ad895cc7ba9133068b14fd5b3d9ed6f96d3a535ff2be342493855f237b6b4782604051610676929190610fc7565b60405180910390a1600643888888888887896040516020016106a0999897969594939291906110b2565b60408051601f1981840301815291905280516020909101206000555050505050505050565b600688888888888888886040516020016106e7999897969594939291906110b2565b6040516020818303038152906040528051906020012060001c6000541461070d57600080fd5b336001600160a01b0385161461072257600080fd5b600a88014310158015610733575060015b61073c57600080fd5b341561074757600080fd5b6040516001600160a01b038516904780156108fc02916000818181858888f1935050505015801561077c573d6000803e3d6000fd5b507f3a6f8023909a26b76d462631fcdf570dbe3740447548e09470d1ad04394a0cec476040516103479190610fbe565b600285858585856040516020016107c896959493929190611001565b6040516020818303038152906040528051906020012060001c600054146107ee57600080fd5b336001600160a01b0385161461080357600080fd5b600a8501431061081257600080fd5b341561081d57600080fd5b7f6fbec89a9bad4c7daaf5b053ac2c5ad4e0ff33c287295fe9a98cf7f3a3043f9c4760405161084c9190610fbe565b60405180910390a16108648484848460006001610bbb565b5050505050565b60058787878787878760405160200161088b98979695949392919061106e565b6040516020818303038152906040528051906020012060001c600054146108b157600080fd5b336001600160a01b038716146108c657600080fd5b600a870143101580156108d7575060015b6108e057600080fd5b34156108eb57600080fd5b6040516001600160a01b038716904780156108fc02916000818181858888f19350505050158015610920573d6000803e3d6000fd5b507fc92018b4e91e597d736654f7b1d2ec034c5fec5920e2cfe22e15b4ddcdf5e18a476040516103479190610fbe565b60068a8a8a8a8a8a8a8a604051602001610972999897969594939291906110b2565b6040516020818303038152906040528051906020012060001c6000541461099857600080fd5b6109a0610cf0565b336001600160a01b038b16146109b557600080fd5b600a8b0143106109c457600080fd5b34156109cf57600080fd5b82826040516020016109e2929190610fc7565b6040516020818303038152906040528051906020012060001c8614610a0657600080fd5b60038210610a1357600080fd5b6003808310602083018190529086106040830152610a32576000610a38565b80604001515b15610a5457600385600403830181610a4c57fe5b068152610a7f565b806020015115610a675760028152610a7f565b806040015115610a7a5760008152610a7f565b600181525b7f5faf534620fe4d35c4670f2df8db5aff6901c4069d879904e0a4e11c119b4223478484604051610ab2939291906110ff565b60405180910390a1610acf8a8a8a8a886001018660000151610bbb565b5050505050505050505050565b600184848484604051602001610af6959493929190610fd5565b6040516020818303038152906040528051906020012060001c60005414610b1c57600080fd5b336001600160a01b03841614610b3157600080fd5b600a84014310158015610b42575060015b610b4b57600080fd5b3415610b5657600080fd5b6040516001600160a01b038416904780156108fc02916000818181858888f19350505050158015610b8b573d6000803e3d6000fd5b507f0f5f4d65cf2c85506eee21a3fb54b49eb1fdb9267bbd430782deebd67e6a3639476040516103479190610fbe565b610bc3610d10565b6001821415610c0a576004438888888888604051602001610bea9796959493929190611035565b60408051601f198184030181529190528051602090910120600055610ce7565b6002821415610c315760408051808201909152600287028152600060208201528152610c6b565b81610c545760408051808201909152600081526002870260208201528152610c6b565b604080518082019091528681526020810187905281525b8051516040516001600160a01b03891691870180156108fc02916000818181858888f19350505050158015610ca4573d6000803e3d6000fd5b508051602001516040516001600160a01b0386169180156108fc02916000818181858888f19350505050158015610cdf573d6000803e3d6000fd5b506000805533ff5b50505050505050565b604080516060810182526000808252602082018190529181019190915290565b6040518060200160405280610d23610d28565b905290565b604051806040016040528060008152602001600081525090565b80356001600160a01b0381168114610d5957600080fd5b92915050565b60008060008060808587031215610d74578384fd5b84359350610d858660208701610d42565b93969395505050506040820135916060013590565b600080600080600060a08688031215610db1578081fd5b85359450610dc28760208801610d42565b93506040860135925060608601359150610ddf8760808801610d42565b90509295509295909350565b60008060008060008060c08789031215610e03578081fd5b86359550610e148860208901610d42565b94506040870135935060608701359250610e318860808901610d42565b915060a087013590509295509295509295565b600080600080600080600060e0888a031215610e5e578081fd5b87359650610e6f8960208a01610d42565b95506040880135945060608801359350610e8c8960808a01610d42565b925060a0880135915060c0880135905092959891949750929550565b600080600080600080600080610100898b031215610ec4578081fd5b88359750610ed58a60208b01610d42565b96506040890135955060608901359450610ef28a60808b01610d42565b979a969950949793969560a0850135955060c08501359460e001359350915050565b6000806000806000806000806000806101408b8d031215610f33578182fd5b8a359950610f448c60208d01610d42565b985060408b0135975060608b01359650610f618c60808d01610d42565b999c989b50969995989760a0870135975060c08701359660e08101359650610100810135955061012001359350915050565b600080600060608486031215610fa7578283fd5b505081359360208301359350604090920135919050565b90815260200190565b918252602082015260400190565b94855260208501939093526001600160a01b039190911660408401526060830152608082015260a00190565b95865260208601949094526001600160a01b039283166040860152606085019190915260808401521660a082015260c00190565b96875260208701959095526001600160a01b039384166040870152606086019290925260808501521660a083015260c082015260e00190565b97885260208801969096526001600160a01b0394851660408801526060870193909352608086019190915290911660a084015260c083015260e08201526101000190565b98895260208901979097526001600160a01b0395861660408901526060880194909452608087019290925290921660a085015260c084019190915260e08301526101008201526101200190565b928352602083019190915260408201526060019056fea264697066735822122055df8157f281e81c2b1c65b965b39df9960bfa32c3b3e147b09fcee9f0f5f0ca64736f6c63430007000033` };

export const _Connectors = {
  ETH: _ETH };
