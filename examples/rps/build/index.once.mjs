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
    
    const txn3 = await ctc.sendrecv('A', 9, 0, [v7, v5, v6], 0, false, null);
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
    
    let v48;
    const v49 = stdlib.protect(stdlib.T_Bytes, await interact.getHand());
    const v51 = stdlib.bytes_eq(v49, 'ROCK');
    const v53 = stdlib.bytes_eq(v49, 'PAPER');
    const v55 = stdlib.bytes_eq(v49, 'SCISSORS');
    const v56 = v51 ? true : v53;
    const v57 = v56 ? true : v55;
    stdlib.assert(v57);
    if (v51) {
      v48 = 0;
       }
    else {
      if (v53) {
        v48 = 1;
         }
      else {
        v48 = 2;
         }
       }
    const v64 = stdlib.protect(stdlib.T_UInt256, await interact.random());
    const v65 = stdlib.keccak256(v64, v48);
    stdlib.protect(stdlib.T_Null, await interact.commits());
    
    
    const txn3 = await ctc.sendrecv('A', 3, 1, [v7, v5, v6, v16, v65], 0, 10, null);
    if (txn3.didTimeout) {
      const txn4 = await ctc.recv('A', 8, 0, false);
      const [] = txn4.data;
      const v73 = txn4.value;
      const v75 = stdlib.eq(0, v73);
      stdlib.assert(v75);
      stdlib.protect(stdlib.T_Null, await interact.endsWith('Alice quits'));
      
      return; }
    else {
      const [v67] = txn3.data;
      const v68 = txn3.value;
      const v70 = stdlib.eq(0, v68);
      stdlib.assert(v70);
      const txn4 = await ctc.recv('A', 4, 1, 10);
      if (txn4.didTimeout) {
        
        const txn5 = await ctc.sendrecv('A', 7, 0, [v7, v5, v6, v16, v67], 0, false, null);
        const [] = txn5.data;
        const v118 = txn5.value;
        const v120 = stdlib.eq(0, v118);
        stdlib.assert(v120);
        stdlib.protect(stdlib.T_Null, await interact.endsWith('Bob quits'));
        
        return; }
      else {
        const [v112] = txn4.data;
        const v113 = txn4.value;
        const v115 = stdlib.eq(0, v113);
        stdlib.assert(v115);
        const v140 = stdlib.le(0, v112);
        const v141 = stdlib.lt(v112, 3);
        const v142 = v140 ? v141 : false;
        stdlib.assert(v142);
        let v144;
        const v150 = stdlib.eq(v112, 0);
        if (v150) {
          v144 = 'ROCK';
           }
        else {
          const v152 = stdlib.eq(v112, 1);
          if (v152) {
            v144 = 'PAPER';
             }
          else {
            v144 = 'SCISSORS';
             }
           }
        stdlib.protect(stdlib.T_Null, await interact.reveals(v144));
        
        
        const txn5 = await ctc.sendrecv('A', 5, 2, [v7, v5, v6, v16, v67, v112, v64, v48], 0, 10, null);
        if (txn5.didTimeout) {
          const txn6 = await ctc.recv('A', 6, 0, false);
          const [] = txn6.data;
          const v161 = txn6.value;
          const v163 = stdlib.eq(0, v161);
          stdlib.assert(v163);
          stdlib.protect(stdlib.T_Null, await interact.endsWith('Alice quits'));
          
          return; }
        else {
          const [v154, v155] = txn5.data;
          const v156 = txn5.value;
          const v158 = stdlib.eq(0, v156);
          stdlib.assert(v158);
          const v183 = stdlib.keccak256(v154, v155);
          const v185 = stdlib.eq(v67, v183);
          stdlib.assert(v185);
          const v187 = stdlib.le(0, v155);
          const v188 = stdlib.lt(v155, 3);
          const v189 = v187 ? v188 : false;
          stdlib.assert(v189);
          let v191;
          const v193 = stdlib.le(0, v155);
          const v194 = stdlib.lt(v155, 3);
          const v195 = v193 ? v194 : false;
          const v197 = stdlib.le(0, v112);
          const v198 = stdlib.lt(v112, 3);
          const v199 = v197 ? v198 : false;
          const v200 = v195 ? v199 : false;
          if (v200) {
            const v201 = stdlib.sub(4, v112);
            const v202 = stdlib.add(v155, v201);
            const v203 = stdlib.mod(v202, 3);
            v191 = v203;
             }
          else {
            if (v195) {
              v191 = 2;
               }
            else {
              if (v199) {
                v191 = 0;
                 }
              else {
                v191 = 1;
                 }
               }
             }
          let v261;
          const v263 = stdlib.eq(v191, 2);
          if (v263) {
            const v264 = stdlib.mul(2, v5);
            v261 = [v264, 0];
             }
          else {
            const v266 = stdlib.eq(v191, 0);
            if (v266) {
              const v267 = stdlib.mul(2, v5);
              v261 = [0, v267];
               }
            else {
              v261 = [v5, v5];
               }
             }
          let v274;
          const v280 = stdlib.eq(v191, 0);
          if (v280) {
            v274 = 'Bob wins';
             }
          else {
            const v282 = stdlib.eq(v191, 1);
            if (v282) {
              v274 = 'Draw';
               }
            else {
              const v284 = stdlib.eq(v191, 2);
              if (v284) {
                v274 = 'Alice wins';
                 }
              else {
                const v286 = stdlib.eq(v191, 3);
                if (v286) {
                  v274 = 'Alice quits';
                   }
                else {
                  v274 = 'Bob quits';
                   }
                 }
               }
             }
          stdlib.protect(stdlib.T_Null, await interact.endsWith(v274));
          
          return; } } } } }
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
    const txn3 = await ctc.recv('B', 9, 0, false);
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
    const txn3 = await ctc.recv('B', 3, 1, 10);
    if (txn3.didTimeout) {
      
      const txn4 = await ctc.sendrecv('B', 8, 0, [v7, v5, v6, v16], 0, false, null);
      const [] = txn4.data;
      const v73 = txn4.value;
      const v75 = stdlib.eq(0, v73);
      stdlib.assert(v75);
      stdlib.protect(stdlib.T_Null, await interact.endsWith('Alice quits'));
      
      return; }
    else {
      const [v67] = txn3.data;
      const v68 = txn3.value;
      const v70 = stdlib.eq(0, v68);
      stdlib.assert(v70);
      let v96;
      const v97 = stdlib.protect(stdlib.T_Bytes, await interact.getHand());
      const v99 = stdlib.bytes_eq(v97, 'ROCK');
      const v101 = stdlib.bytes_eq(v97, 'PAPER');
      const v103 = stdlib.bytes_eq(v97, 'SCISSORS');
      const v104 = v99 ? true : v101;
      const v105 = v104 ? true : v103;
      stdlib.assert(v105);
      if (v99) {
        v96 = 0;
         }
      else {
        if (v101) {
          v96 = 1;
           }
        else {
          v96 = 2;
           }
         }
      stdlib.protect(stdlib.T_Null, await interact.shows());
      
      
      const txn4 = await ctc.sendrecv('B', 4, 1, [v7, v5, v6, v16, v67, v96], 0, 10, null);
      if (txn4.didTimeout) {
        const txn5 = await ctc.recv('B', 7, 0, false);
        const [] = txn5.data;
        const v118 = txn5.value;
        const v120 = stdlib.eq(0, v118);
        stdlib.assert(v120);
        stdlib.protect(stdlib.T_Null, await interact.endsWith('Bob quits'));
        
        return; }
      else {
        const [v112] = txn4.data;
        const v113 = txn4.value;
        const v115 = stdlib.eq(0, v113);
        stdlib.assert(v115);
        const v140 = stdlib.le(0, v112);
        const v141 = stdlib.lt(v112, 3);
        const v142 = v140 ? v141 : false;
        stdlib.assert(v142);
        const txn5 = await ctc.recv('B', 5, 2, 10);
        if (txn5.didTimeout) {
          
          const txn6 = await ctc.sendrecv('B', 6, 0, [v7, v5, v6, v16, v67, v112], 0, false, null);
          const [] = txn6.data;
          const v161 = txn6.value;
          const v163 = stdlib.eq(0, v161);
          stdlib.assert(v163);
          stdlib.protect(stdlib.T_Null, await interact.endsWith('Alice quits'));
          
          return; }
        else {
          const [v154, v155] = txn5.data;
          const v156 = txn5.value;
          const v158 = stdlib.eq(0, v156);
          stdlib.assert(v158);
          const v183 = stdlib.keccak256(v154, v155);
          const v185 = stdlib.eq(v67, v183);
          stdlib.assert(v185);
          const v187 = stdlib.le(0, v155);
          const v188 = stdlib.lt(v155, 3);
          const v189 = v187 ? v188 : false;
          stdlib.assert(v189);
          let v191;
          const v193 = stdlib.le(0, v155);
          const v194 = stdlib.lt(v155, 3);
          const v195 = v193 ? v194 : false;
          const v197 = stdlib.le(0, v112);
          const v198 = stdlib.lt(v112, 3);
          const v199 = v197 ? v198 : false;
          const v200 = v195 ? v199 : false;
          if (v200) {
            const v201 = stdlib.sub(4, v112);
            const v202 = stdlib.add(v155, v201);
            const v203 = stdlib.mod(v202, 3);
            v191 = v203;
             }
          else {
            if (v195) {
              v191 = 2;
               }
            else {
              if (v199) {
                v191 = 0;
                 }
              else {
                v191 = 1;
                 }
               }
             }
          let v261;
          const v263 = stdlib.eq(v191, 2);
          if (v263) {
            const v264 = stdlib.mul(2, v5);
            v261 = [v264, 0];
             }
          else {
            const v266 = stdlib.eq(v191, 0);
            if (v266) {
              const v267 = stdlib.mul(2, v5);
              v261 = [0, v267];
               }
            else {
              v261 = [v5, v5];
               }
             }
          let v289;
          const v295 = stdlib.eq(v191, 0);
          if (v295) {
            v289 = 'Bob wins';
             }
          else {
            const v297 = stdlib.eq(v191, 1);
            if (v297) {
              v289 = 'Draw';
               }
            else {
              const v299 = stdlib.eq(v191, 2);
              if (v299) {
                v289 = 'Alice wins';
                 }
              else {
                const v301 = stdlib.eq(v191, 3);
                if (v301) {
                  v289 = 'Alice quits';
                   }
                else {
                  v289 = 'Bob quits';
                   }
                 }
               }
             }
          stdlib.protect(stdlib.T_Null, await interact.endsWith(v289));
          
          return; } } } } }
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
    const txn3 = await ctc.recv('O', 9, 0, false);
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
    const txn3 = await ctc.recv('O', 3, 1, 10);
    if (txn3.didTimeout) {
      const txn4 = await ctc.recv('O', 8, 0, false);
      const [] = txn4.data;
      const v73 = txn4.value;
      const v75 = stdlib.eq(0, v73);
      stdlib.assert(v75);
      return; }
    else {
      const [v67] = txn3.data;
      const v68 = txn3.value;
      const v70 = stdlib.eq(0, v68);
      stdlib.assert(v70);
      const txn4 = await ctc.recv('O', 4, 1, 10);
      if (txn4.didTimeout) {
        const txn5 = await ctc.recv('O', 7, 0, false);
        const [] = txn5.data;
        const v118 = txn5.value;
        const v120 = stdlib.eq(0, v118);
        stdlib.assert(v120);
        return; }
      else {
        const [v112] = txn4.data;
        const v113 = txn4.value;
        const v115 = stdlib.eq(0, v113);
        stdlib.assert(v115);
        const v140 = stdlib.le(0, v112);
        const v141 = stdlib.lt(v112, 3);
        const v142 = v140 ? v141 : false;
        stdlib.assert(v142);
        const txn5 = await ctc.recv('O', 5, 2, 10);
        if (txn5.didTimeout) {
          const txn6 = await ctc.recv('O', 6, 0, false);
          const [] = txn6.data;
          const v161 = txn6.value;
          const v163 = stdlib.eq(0, v161);
          stdlib.assert(v163);
          return; }
        else {
          const [v154, v155] = txn5.data;
          const v156 = txn5.value;
          const v158 = stdlib.eq(0, v156);
          stdlib.assert(v158);
          const v183 = stdlib.keccak256(v154, v155);
          const v185 = stdlib.eq(v67, v183);
          stdlib.assert(v185);
          const v187 = stdlib.le(0, v155);
          const v188 = stdlib.lt(v155, 3);
          const v189 = v187 ? v188 : false;
          stdlib.assert(v189);
          let v191;
          const v193 = stdlib.le(0, v155);
          const v194 = stdlib.lt(v155, 3);
          const v195 = v193 ? v194 : false;
          const v197 = stdlib.le(0, v112);
          const v198 = stdlib.lt(v112, 3);
          const v199 = v197 ? v198 : false;
          const v200 = v195 ? v199 : false;
          if (v200) {
            const v201 = stdlib.sub(4, v112);
            const v202 = stdlib.add(v155, v201);
            const v203 = stdlib.mod(v202, 3);
            v191 = v203;
             }
          else {
            if (v195) {
              v191 = 2;
               }
            else {
              if (v199) {
                v191 = 0;
                 }
              else {
                v191 = 1;
                 }
               }
             }
          let v261;
          const v263 = stdlib.eq(v191, 2);
          if (v263) {
            const v264 = stdlib.mul(2, v5);
            v261 = [v264, 0];
             }
          else {
            const v266 = stdlib.eq(v191, 0);
            if (v266) {
              const v267 = stdlib.mul(2, v5);
              v261 = [0, v267];
               }
            else {
              v261 = [v5, v5];
               }
             }
          return; } } } } }

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
        },
        {
          "indexed": false,
          "internalType": "uint256",
          "name": "v67",
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
          "name": "v112",
          "type": "uint256"
        }
      ],
      "name": "e4",
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
          "name": "v154",
          "type": "uint256"
        },
        {
          "indexed": false,
          "internalType": "uint256",
          "name": "v155",
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
            },
            {
              "internalType": "uint256",
              "name": "v67",
              "type": "uint256"
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
              "name": "v67",
              "type": "uint256"
            },
            {
              "internalType": "uint256",
              "name": "v112",
              "type": "uint256"
            }
          ],
          "internalType": "struct ReachContract.a4",
          "name": "_a",
          "type": "tuple"
        }
      ],
      "name": "m4",
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
              "name": "v67",
              "type": "uint256"
            },
            {
              "internalType": "uint256",
              "name": "v112",
              "type": "uint256"
            },
            {
              "internalType": "uint256",
              "name": "v154",
              "type": "uint256"
            },
            {
              "internalType": "uint256",
              "name": "v155",
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
              "name": "v67",
              "type": "uint256"
            },
            {
              "internalType": "uint256",
              "name": "v112",
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
              "name": "v67",
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
  Bytecode: `0x608060405261001160004360a0610031565b60408051601f19818403018152919052805160209091012060005561003f565b918252602082015260400190565b610fc48061004e6000396000f3fe6080604052600436106100865760003560e01c806342b7bd311161005957806342b7bd31146100d9578063b2633906146100ec578063cc68dcb7146100ff578063d1d9fa7c14610112578063eaf8621b1461012557610086565b80630d8383281461008b5780632ae4e588146100a0578063324c5ad5146100b357806337248a0c146100c6575b600080fd5b61009e610099366004610e10565b610138565b005b61009e6100ae366004610e46565b610238565b61009e6100c1366004610e73565b610387565b61009e6100d4366004610e61565b6104bb565b61009e6100e7366004610e2b565b6107b4565b61009e6100fa366004610e46565b6108ea565b61009e61010d366004610dff565b610a3d565b61009e610120366004610e2b565b610af3565b61009e610133366004610e10565b610c2d565b6001813561014c6040840160208501610dd1565b8360400135846060013560405160200161016a959493929190610e9b565b6040516020818303038152906040528051906020012060001c6000541461019057600080fd5b8035600a0143106101a057600080fd5b348160400135146101b057600080fd5b7ff04f5fc87a72102f7c0b228f8bbaf9b9aa7a2b5dc295c86538fdde91e95866e9476040516101df9190610e84565b60405180910390a16002436101fa6040840160208501610dd1565b836040013584606001353360405160200161021a96959493929190610ec7565b60408051601f19818403018152919052805160209091012060005550565b6004813561024c6040840160208501610dd1565b6040840135606085013561026660a0870160808801610dd1565b8660a001358760c00135604051602001610287989796959493929190610f34565b6040516020818303038152906040528051906020012060001c600054146102ad57600080fd5b6102bd60a0820160808301610dd1565b6001600160a01b0316336001600160a01b0316146102da57600080fd5b8035600a0143108015906102ec575060015b6102f557600080fd5b341561030057600080fd5b61031060a0820160808301610dd1565b6001600160a01b03166108fc479081150290604051600060405180830381858888f19350505050158015610348573d6000803e3d6000fd5b507fcb3347bd475fd43f41b4bc5bb011db952f2079e6ba9a82ff211988cd7871dba6476040516103789190610e84565b60405180910390a16000805533ff5b6002813561039b6040840160208501610dd1565b604084013560608501356103b560a0870160808801610dd1565b6040516020016103ca96959493929190610ec7565b6040516020818303038152906040528051906020012060001c600054146103f057600080fd5b61040060a0820160808301610dd1565b6001600160a01b0316336001600160a01b03161461041d57600080fd5b8035600a01431080159061042f575060015b61043857600080fd5b341561044357600080fd5b61045360a0820160808301610dd1565b6001600160a01b03166108fc479081150290604051600060405180830381858888f1935050505015801561048b573d6000803e3d6000fd5b507f3a6f8023909a26b76d462631fcdf570dbe3740447548e09470d1ad04394a0cec476040516103789190610e84565b600481356104cf6040840160208501610dd1565b604084013560608501356104e960a0870160808801610dd1565b8660a001358760c0013560405160200161050a989796959493929190610f34565b6040516020818303038152906040528051906020012060001c6000541461053057600080fd5b610538610d50565b6105486040830160208401610dd1565b6001600160a01b0316336001600160a01b03161461056557600080fd5b8135600a01431061057557600080fd5b341561058057600080fd5b8160e0013582610100013560405160200161059c929190610e8d565b6040516020818303038152906040528051906020012060001c8260a00135146105c457600080fd5b6003826101000135106105d657600080fd5b600361010083013581116040830181905260c0840135919091106060830152610600576000610606565b80606001515b1561062457600361010083013560c08401356004030106815261064f565b806040015115610637576002815261064f565b80606001511561064a576000815261064f565b600181525b80516002141561068357604051806040016040528083604001356002028152602001600081525081602001819052506106d0565b80516106b357604051806040016040528060008152602001836040013560020281525081602001819052506106d0565b604080518082018252908301358082526020808301919091528201525b6106e06040830160208401610dd1565b6001600160a01b03166108fc8260200151600001518460600135019081150290604051600060405180830381858888f19350505050158015610726573d6000803e3d6000fd5b5061073760a0830160808401610dd1565b6001600160a01b03166108fc8260200151602001519081150290604051600060405180830381858888f19350505050158015610777573d6000803e3d6000fd5b507f3c3023cc427ae7f284b643c954c1a90afba24284d594cded84550e2316e830f4478360e0013584610100013560405161037893929190610f78565b600281356107c86040840160208501610dd1565b604084013560608501356107e260a0870160808801610dd1565b6040516020016107f796959493929190610ec7565b6040516020818303038152906040528051906020012060001c6000541461081d57600080fd5b61082d6040820160208301610dd1565b6001600160a01b0316336001600160a01b03161461084a57600080fd5b8035600a01431061085a57600080fd5b341561086557600080fd5b7f94dd7e08991b8945fde2d5865f7071e72045b9800e293ff60d29c6960c5a4fb5478260a0013560405161089a929190610e8d565b60405180910390a16003436108b56040840160208501610dd1565b604084013560608501356108cf60a0870160808801610dd1565b8660a0013560405160200161021a9796959493929190610efb565b600381356108fe6040840160208501610dd1565b6040840135606085013561091860a0870160808801610dd1565b8660a001356040516020016109339796959493929190610efb565b6040516020818303038152906040528051906020012060001c6000541461095957600080fd5b61096960a0820160808301610dd1565b6001600160a01b0316336001600160a01b03161461098657600080fd5b8035600a01431061099657600080fd5b34156109a157600080fd5b60038160c00135106109b257600080fd5b7fb71d350b59ceca5c6544e5367d61ca8cae3e36b25f8d900743d063dff3d6508b478260c001356040516109e7929190610e8d565b60405180910390a1600443610a026040840160208501610dd1565b60408401356060850135610a1c60a0870160808801610dd1565b8660a001358760c0013560405160200161021a989796959493929190610f34565b604051610a5290600090833590602001610e8d565b6040516020818303038152906040528051906020012060001c60005414610a7857600080fd5b34816040013582602001350114610a8e57600080fd5b7f219cc811755104876269c7553666684eaaeecb90b6a7ffc6fdd5068140059b8e4782602001358360400135604051610ac993929190610f78565b60405180910390a1600143338360200135846040013560405160200161021a959493929190610e9b565b60038135610b076040840160208501610dd1565b60408401356060850135610b2160a0870160808801610dd1565b8660a00135604051602001610b3c9796959493929190610efb565b6040516020818303038152906040528051906020012060001c60005414610b6257600080fd5b610b726040820160208301610dd1565b6001600160a01b0316336001600160a01b031614610b8f57600080fd5b8035600a014310801590610ba1575060015b610baa57600080fd5b3415610bb557600080fd5b610bc56040820160208301610dd1565b6001600160a01b03166108fc479081150290604051600060405180830381858888f19350505050158015610bfd573d6000803e3d6000fd5b507ffc55d683ac816a7149ebdfa999ae1bcfeeae27c37c9dab64a23f617beed2a007476040516103789190610e84565b60018135610c416040840160208501610dd1565b83604001358460600135604051602001610c5f959493929190610e9b565b6040516020818303038152906040528051906020012060001c60005414610c8557600080fd5b610c956040820160208301610dd1565b6001600160a01b0316336001600160a01b031614610cb257600080fd5b8035600a014310801590610cc4575060015b610ccd57600080fd5b3415610cd857600080fd5b610ce86040820160208301610dd1565b6001600160a01b03166108fc479081150290604051600060405180830381858888f19350505050158015610d20573d6000803e3d6000fd5b507fc92018b4e91e597d736654f7b1d2ec034c5fec5920e2cfe22e15b4ddcdf5e18a476040516103789190610e84565b604051806080016040528060008152602001610d6a610d7e565b815260006020820181905260409091015290565b604051806040016040528060008152602001600081525090565b600060808284031215610da9578081fd5b50919050565b600060c08284031215610da9578081fd5b600060e08284031215610da9578081fd5b600060208284031215610de2578081fd5b81356001600160a01b0381168114610df8578182fd5b9392505050565b600060608284031215610da9578081fd5b600060808284031215610e21578081fd5b610df88383610d98565b600060c08284031215610e3c578081fd5b610df88383610daf565b600060e08284031215610e57578081fd5b610df88383610dc0565b60006101208284031215610da9578081fd5b600060a08284031215610da9578081fd5b90815260200190565b918252602082015260400190565b94855260208501939093526001600160a01b039190911660408401526060830152608082015260a00190565b95865260208601949094526001600160a01b039283166040860152606085019190915260808401521660a082015260c00190565b96875260208701959095526001600160a01b039384166040870152606086019290925260808501521660a083015260c082015260e00190565b97885260208801969096526001600160a01b0394851660408801526060870193909352608086019190915290911660a084015260c083015260e08201526101000190565b928352602083019190915260408201526060019056fea2646970667358221220ad9ea9b1f0ddf3aaa9c59cebfc7595721c2efa2168fc8e87013ed848bb41ee1264736f6c63430007000033` };

export const _Connectors = {
  ETH: _ETH };
