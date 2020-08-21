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
        },
        {
          "internalType": "uint256",
          "name": "v67",
          "type": "uint256"
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
          "name": "v67",
          "type": "uint256"
        },
        {
          "internalType": "uint256",
          "name": "v112",
          "type": "uint256"
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
          "name": "v67",
          "type": "uint256"
        },
        {
          "internalType": "uint256",
          "name": "v112",
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
          "name": "v67",
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
        }
      ],
      "name": "m9",
      "outputs": [],
      "stateMutability": "payable",
      "type": "function"
    }
  ]`,
  Bytecode: `0x608060405261001160004360a0610031565b60408051601f19818403018152919052805160209091012060005561003f565b918252602082015260400190565b610e218061004e6000396000f3fe6080604052600436106100865760003560e01c806373929c5e1161005957806373929c5e146100d95780637a52ccb3146100ec5780637de71f08146100ff5780639ccddd3a14610112578063bb91d6e31461012557610086565b8063050147391461008b578063103d2bab146100a05780635e6a8eed146100b3578063718b7dd7146100c6575b600080fd5b61009e610099366004610b32565b610138565b005b61009e6100ae366004610bdc565b610228565b61009e6100c1366004610b83565b61030d565b61009e6100d4366004610b83565b6103f8565b61009e6100e7366004610c40565b6104db565b61009e6100fa366004610cb6565b61072a565b61009e61010d366004610af7565b6107e7565b61009e610120366004610af7565b6108c6565b61009e610133366004610bdc565b610995565b6002858585858560405160200161015496959493929190610d24565b6040516020818303038152906040528051906020012060001c6000541461017a57600080fd5b336001600160a01b0382161461018f57600080fd5b600a850143101580156101a0575060015b6101a957600080fd5b34156101b457600080fd5b6040516001600160a01b038216904780156108fc02916000818181858888f193505050501580156101e9573d6000803e3d6000fd5b507f3a6f8023909a26b76d462631fcdf570dbe3740447548e09470d1ad04394a0cec476040516102199190610ce1565b60405180910390a16000805533ff5b600487878787878787604051602001610248989796959493929190610d91565b6040516020818303038152906040528051906020012060001c6000541461026e57600080fd5b336001600160a01b0384161461028357600080fd5b600a87014310158015610294575060015b61029d57600080fd5b34156102a857600080fd5b6040516001600160a01b038416904780156108fc02916000818181858888f193505050501580156102dd573d6000803e3d6000fd5b507fcb3347bd475fd43f41b4bc5bb011db952f2079e6ba9a82ff211988cd7871dba6476040516102199190610ce1565b6002868686868660405160200161032996959493929190610d24565b6040516020818303038152906040528051906020012060001c6000541461034f57600080fd5b336001600160a01b0386161461036457600080fd5b600a8601431061037357600080fd5b341561037e57600080fd5b7f94dd7e08991b8945fde2d5865f7071e72045b9800e293ff60d29c6960c5a4fb547826040516103af929190610cea565b60405180910390a160034386868686866040516020016103d59796959493929190610d58565b60408051601f198184030181529190528051602090910120600055505050505050565b60038686868686866040516020016104169796959493929190610d58565b6040516020818303038152906040528051906020012060001c6000541461043c57600080fd5b336001600160a01b0386161461045157600080fd5b600a86014310158015610462575060015b61046b57600080fd5b341561047657600080fd5b6040516001600160a01b038616904780156108fc02916000818181858888f193505050501580156104ab573d6000803e3d6000fd5b507ffc55d683ac816a7149ebdfa999ae1bcfeeae27c37c9dab64a23f617beed2a007476040516102199190610ce1565b6004898989898989896040516020016104fb989796959493929190610d91565b6040516020818303038152906040528051906020012060001c6000541461052157600080fd5b610529610a92565b336001600160a01b038a161461053e57600080fd5b600a8a01431061054d57600080fd5b341561055857600080fd5b828260405160200161056b929190610cea565b6040516020818303038152906040528051906020012060001c851461058f57600080fd5b6003821061059c57600080fd5b60038083106040830181905290851060608301526105bb5760006105c1565b80606001515b156105dd576003846004038301816105d557fe5b068152610608565b8060400151156105f05760028152610608565b8060600151156106035760008152610608565b600181525b8051600214156106385760405180604001604052808960020281526020016000815250816020018190525061067d565b80516106645760405180604001604052806000815260200189600202815250816020018190525061067d565b6040805180820190915288815260208082018a90528201525b6020810151516040516001600160a01b038b1691890180156108fc02916000818181858888f193505050501580156106b9573d6000803e3d6000fd5b5060208082015101516040516001600160a01b0388169180156108fc02916000818181858888f193505050501580156106f6573d6000803e3d6000fd5b507f3c3023cc427ae7f284b643c954c1a90afba24284d594cded84550e2316e830f447848460405161021993929190610dd5565b60008360405160200161073e929190610cea565b6040516020818303038152906040528051906020012060001c6000541461076457600080fd5b348183011461077257600080fd5b7f219cc811755104876269c7553666684eaaeecb90b6a7ffc6fdd5068140059b8e4783836040516107a593929190610dd5565b60405180910390a16001433384846040516020016107c7959493929190610cf8565b60408051601f198184030181529190528051602090910120600055505050565b600184848484604051602001610801959493929190610cf8565b6040516020818303038152906040528051906020012060001c6000541461082757600080fd5b336001600160a01b0384161461083c57600080fd5b600a8401431015801561084d575060015b61085657600080fd5b341561086157600080fd5b6040516001600160a01b038416904780156108fc02916000818181858888f19350505050158015610896573d6000803e3d6000fd5b507fc92018b4e91e597d736654f7b1d2ec034c5fec5920e2cfe22e15b4ddcdf5e18a476040516102199190610ce1565b6001848484846040516020016108e0959493929190610cf8565b6040516020818303038152906040528051906020012060001c6000541461090657600080fd5b600a8401431061091557600080fd5b34821461092157600080fd5b7ff04f5fc87a72102f7c0b228f8bbaf9b9aa7a2b5dc295c86538fdde91e95866e9476040516109509190610ce1565b60405180910390a16002438484843360405160200161097496959493929190610d24565b60408051601f19818403018152919052805160209091012060005550505050565b60038787878787876040516020016109b39796959493929190610d58565b6040516020818303038152906040528051906020012060001c600054146109d957600080fd5b336001600160a01b038416146109ee57600080fd5b600a870143106109fd57600080fd5b3415610a0857600080fd5b60038110610a1557600080fd5b7fb71d350b59ceca5c6544e5367d61ca8cae3e36b25f8d900743d063dff3d6508b4782604051610a46929190610cea565b60405180910390a1600443878787878787604051602001610a6e989796959493929190610d91565b60408051601f19818403018152919052805160209091012060005550505050505050565b604051806080016040528060008152602001610aac610ac0565b815260006020820181905260409091015290565b604051806040016040528060008152602001600081525090565b80356001600160a01b0381168114610af157600080fd5b92915050565b60008060008060808587031215610b0c578384fd5b84359350610b1d8660208701610ada565b93969395505050506040820135916060013590565b600080600080600060a08688031215610b49578081fd5b85359450610b5a8760208801610ada565b93506040860135925060608601359150610b778760808801610ada565b90509295509295909350565b60008060008060008060c08789031215610b9b578081fd5b86359550610bac8860208901610ada565b94506040870135935060608701359250610bc98860808901610ada565b915060a087013590509295509295509295565b600080600080600080600060e0888a031215610bf6578081fd5b87359650610c078960208a01610ada565b95506040880135945060608801359350610c248960808a01610ada565b925060a0880135915060c0880135905092959891949750929550565b60008060008060008060008060006101208a8c031215610c5e578182fd5b89359850610c6f8b60208c01610ada565b975060408a0135965060608a01359550610c8c8b60808c01610ada565b989b979a50959894979660a0860135965060c08601359560e0810135955061010001359350915050565b600080600060608486031215610cca578283fd5b505081359360208301359350604090920135919050565b90815260200190565b918252602082015260400190565b94855260208501939093526001600160a01b039190911660408401526060830152608082015260a00190565b95865260208601949094526001600160a01b039283166040860152606085019190915260808401521660a082015260c00190565b96875260208701959095526001600160a01b039384166040870152606086019290925260808501521660a083015260c082015260e00190565b97885260208801969096526001600160a01b0394851660408801526060870193909352608086019190915290911660a084015260c083015260e08201526101000190565b928352602083019190915260408201526060019056fea26469706673582212203a38507b27f81e360e9dbe9754cf1ab1f45dd28001a398b326b7230c5a5743bd64736f6c63430007000033` };

export const _Connectors = {
  ETH: _ETH };
