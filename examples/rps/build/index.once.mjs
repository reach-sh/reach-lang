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
    
    const txn3 = await ctc.sendrecv('A', 9, 0, [v7, v5, v6], 0, false, null);
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
      const txn4 = await ctc.recv('A', 8, 0, false);
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
        
        const txn5 = await ctc.sendrecv('A', 7, 0, [v7, v5, v6, v15, v58], 0, false, null);
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
          const txn6 = await ctc.recv('A', 6, 0, false);
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
          let v232;
          const v233 = stdlib.eq(v158, 2);
          if (v233) {
            const v234 = stdlib.mul(2, v5);
            v232 = [v234, 0];
             }
          else {
            const v235 = stdlib.eq(v158, 0);
            if (v235) {
              const v236 = stdlib.mul(2, v5);
              v232 = [0, v236];
               }
            else {
              v232 = [v5, v5];
               }
             }
          let v243;
          const v245 = stdlib.le(0, v158);
          const v246 = stdlib.lt(v158, 5);
          const v248 = v245 ? v246 : false;
          stdlib.assert(v248);
          const v249 = stdlib.eq(v158, 0);
          if (v249) {
            v243 = 'Bob wins';
             }
          else {
            const v250 = stdlib.eq(v158, 1);
            if (v250) {
              v243 = 'Draw';
               }
            else {
              const v251 = stdlib.eq(v158, 2);
              if (v251) {
                v243 = 'Alice wins';
                 }
              else {
                const v252 = stdlib.eq(v158, 3);
                if (v252) {
                  v243 = 'Alice quits';
                   }
                else {
                  v243 = 'Bob quits';
                   }
                 }
               }
             }
          stdlib.protect(stdlib.T_Null, await interact.endsWith(v243));
          
          return; } } } } }
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
    const txn3 = await ctc.recv('B', 9, 0, false);
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
      
      const txn4 = await ctc.sendrecv('B', 8, 0, [v7, v5, v6, v15], 0, false, null);
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
        const txn5 = await ctc.recv('B', 7, 0, false);
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
          
          const txn6 = await ctc.sendrecv('B', 6, 0, [v7, v5, v6, v15, v58, v95], 0, false, null);
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
          let v232;
          const v233 = stdlib.eq(v158, 2);
          if (v233) {
            const v234 = stdlib.mul(2, v5);
            v232 = [v234, 0];
             }
          else {
            const v235 = stdlib.eq(v158, 0);
            if (v235) {
              const v236 = stdlib.mul(2, v5);
              v232 = [0, v236];
               }
            else {
              v232 = [v5, v5];
               }
             }
          let v255;
          const v257 = stdlib.le(0, v158);
          const v258 = stdlib.lt(v158, 5);
          const v260 = v257 ? v258 : false;
          stdlib.assert(v260);
          const v261 = stdlib.eq(v158, 0);
          if (v261) {
            v255 = 'Bob wins';
             }
          else {
            const v262 = stdlib.eq(v158, 1);
            if (v262) {
              v255 = 'Draw';
               }
            else {
              const v263 = stdlib.eq(v158, 2);
              if (v263) {
                v255 = 'Alice wins';
                 }
              else {
                const v264 = stdlib.eq(v158, 3);
                if (v264) {
                  v255 = 'Alice quits';
                   }
                else {
                  v255 = 'Bob quits';
                   }
                 }
               }
             }
          stdlib.protect(stdlib.T_Null, await interact.endsWith(v255));
          
          return; } } } } }
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
    const txn3 = await ctc.recv('O', 9, 0, false);
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
      const txn4 = await ctc.recv('O', 8, 0, false);
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
        const txn5 = await ctc.recv('O', 7, 0, false);
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
          const txn6 = await ctc.recv('O', 6, 0, false);
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
          let v232;
          const v233 = stdlib.eq(v158, 2);
          if (v233) {
            const v234 = stdlib.mul(2, v5);
            v232 = [v234, 0];
             }
          else {
            const v235 = stdlib.eq(v158, 0);
            if (v235) {
              const v236 = stdlib.mul(2, v5);
              v232 = [0, v236];
               }
            else {
              v232 = [v5, v5];
               }
             }
          return; } } } } }

export const ETH = {
  ABI: `[
    {
      "inputs": [],
      "payable": true,
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
          "name": "v58",
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
          "name": "v95",
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
          "name": "v129",
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
      "constant": false,
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
      "payable": true,
      "stateMutability": "payable",
      "type": "function"
    },
    {
      "constant": false,
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
      "payable": true,
      "stateMutability": "payable",
      "type": "function"
    },
    {
      "constant": false,
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
          "name": "v58",
          "type": "uint256"
        }
      ],
      "name": "m3",
      "outputs": [],
      "payable": true,
      "stateMutability": "payable",
      "type": "function"
    },
    {
      "constant": false,
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
          "name": "v58",
          "type": "uint256"
        },
        {
          "internalType": "uint256",
          "name": "v95",
          "type": "uint256"
        }
      ],
      "name": "m4",
      "outputs": [],
      "payable": true,
      "stateMutability": "payable",
      "type": "function"
    },
    {
      "constant": false,
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
          "name": "v58",
          "type": "uint256"
        },
        {
          "internalType": "uint256",
          "name": "v95",
          "type": "uint256"
        },
        {
          "internalType": "uint256",
          "name": "v129",
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
      "payable": true,
      "stateMutability": "payable",
      "type": "function"
    },
    {
      "constant": false,
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
          "name": "v58",
          "type": "uint256"
        },
        {
          "internalType": "uint256",
          "name": "v95",
          "type": "uint256"
        }
      ],
      "name": "m6",
      "outputs": [],
      "payable": true,
      "stateMutability": "payable",
      "type": "function"
    },
    {
      "constant": false,
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
          "name": "v58",
          "type": "uint256"
        }
      ],
      "name": "m7",
      "outputs": [],
      "payable": true,
      "stateMutability": "payable",
      "type": "function"
    },
    {
      "constant": false,
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
      "name": "m8",
      "outputs": [],
      "payable": true,
      "stateMutability": "payable",
      "type": "function"
    },
    {
      "constant": false,
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
      "payable": true,
      "stateMutability": "payable",
      "type": "function"
    }
  ]`,
  Bytecode: `0x608060405261001160004360a0610040565b60408051601f198184030181529190528051602090910120600055610065565b61003a81610062565b82525050565b6040810161004e8285610031565b61005b6020830184610031565b9392505050565b90565b61108a806100746000396000f3fe6080604052600436106100865760003560e01c806373929c5e1161005957806373929c5e146100d95780637a52ccb3146100ec5780637de71f08146100ff5780639ccddd3a14610112578063bb91d6e31461012557610086565b8063050147391461008b578063103d2bab146100a05780635e6a8eed146100b3578063718b7dd7146100c6575b600080fd5b61009e610099366004610b6d565b610138565b005b61009e6100ae366004610c69565b61022b565b61009e6100c1366004610be2565b610313565b61009e6100d4366004610be2565b610400565b61009e6100e7366004610d05565b6104e6565b61009e6100fa366004610dc9565b610737565b61009e61010d366004610b0c565b6107f6565b61009e610120366004610b0c565b6108d8565b61009e610133366004610c69565b6109a9565b6002858585858560405160200161015496959493929190610eaa565b6040516020818303038152906040528051906020012060001c6000541461017a57600080fd5b336001600160a01b0382161461018f57600080fd5b600a850143101580156101a0575060015b6101a957600080fd5b34156101b457600080fd5b6040516001600160a01b03821690303180156108fc02916000818181858888f193505050501580156101ea573d6000803e3d6000fd5b506040517f3a6f8023909a26b76d462631fcdf570dbe3740447548e09470d1ad04394a0cec9061021c90303190610e2e565b60405180910390a16000805533ff5b60048787878787878760405160200161024b989796959493929190610f6c565b6040516020818303038152906040528051906020012060001c6000541461027157600080fd5b336001600160a01b0384161461028657600080fd5b600a87014310158015610297575060015b6102a057600080fd5b34156102ab57600080fd5b6040516001600160a01b03841690303180156108fc02916000818181858888f193505050501580156102e1573d6000803e3d6000fd5b506040517fcb3347bd475fd43f41b4bc5bb011db952f2079e6ba9a82ff211988cd7871dba69061021c90303190610e2e565b6002868686868660405160200161032f96959493929190610eaa565b6040516020818303038152906040528051906020012060001c6000541461035557600080fd5b336001600160a01b0386161461036a57600080fd5b600a8601431061037957600080fd5b341561038457600080fd5b6040517f94dd7e08991b8945fde2d5865f7071e72045b9800e293ff60d29c6960c5a4fb5906103b7903031908490610e3c565b60405180910390a160034386868686866040516020016103dd9796959493929190610f04565b60408051601f198184030181529190528051602090910120600055505050505050565b600386868686868660405160200161041e9796959493929190610f04565b6040516020818303038152906040528051906020012060001c6000541461044457600080fd5b336001600160a01b0386161461045957600080fd5b600a8601431015801561046a575060015b61047357600080fd5b341561047e57600080fd5b6040516001600160a01b03861690303180156108fc02916000818181858888f193505050501580156104b4573d6000803e3d6000fd5b506040517ffc55d683ac816a7149ebdfa999ae1bcfeeae27c37c9dab64a23f617beed2a0079061021c90303190610e2e565b600489898989898989604051602001610506989796959493929190610f6c565b6040516020818303038152906040528051906020012060001c6000541461052c57600080fd5b610534610aa8565b336001600160a01b038a161461054957600080fd5b600a8a01431061055857600080fd5b341561056357600080fd5b8282604051602001610576929190610e3c565b6040516020818303038152906040528051906020012060001c851461059a57600080fd5b600382106105a757600080fd5b60038083106040830181905290851060608301526105c65760006105cc565b80606001515b156105e8576003846004038301816105e057fe5b068152610613565b8060400151156105fb5760028152610613565b80606001511561060e5760008152610613565b600181525b80516002141561064357604051806040016040528089600202815260200160008152508160200181905250610688565b805161066f57604051806040016040528060008152602001896002028152508160200181905250610688565b6040805180820190915288815260208082018a90528201525b6020810151516040516001600160a01b038b1691890180156108fc02916000818181858888f193505050501580156106c4573d6000803e3d6000fd5b5060208082015101516040516001600160a01b0388169180156108fc02916000818181858888f19350505050158015610701573d6000803e3d6000fd5b506040517f3c3023cc427ae7f284b643c954c1a90afba24284d594cded84550e2316e830f49061021c9030319086908690610fe3565b60008360405160200161074b929190610e3c565b6040516020818303038152906040528051906020012060001c6000541461077157600080fd5b348183011461077f57600080fd5b6040517f219cc811755104876269c7553666684eaaeecb90b6a7ffc6fdd5068140059b8e906107b49030319085908590610fe3565b60405180910390a16001433384846040516020016107d6959493929190610e5e565b60408051601f198184030181529190528051602090910120600055505050565b600184848484604051602001610810959493929190610e5e565b6040516020818303038152906040528051906020012060001c6000541461083657600080fd5b336001600160a01b0384161461084b57600080fd5b600a8401431015801561085c575060015b61086557600080fd5b341561087057600080fd5b6040516001600160a01b03841690303180156108fc02916000818181858888f193505050501580156108a6573d6000803e3d6000fd5b506040517fc92018b4e91e597d736654f7b1d2ec034c5fec5920e2cfe22e15b4ddcdf5e18a9061021c90303190610e2e565b6001848484846040516020016108f2959493929190610e5e565b6040516020818303038152906040528051906020012060001c6000541461091857600080fd5b600a8401431061092757600080fd5b34821461093357600080fd5b6040517ff04f5fc87a72102f7c0b228f8bbaf9b9aa7a2b5dc295c86538fdde91e95866e99061096490303190610e2e565b60405180910390a16002438484843360405160200161098896959493929190610eaa565b60408051601f19818403018152919052805160209091012060005550505050565b60038787878787876040516020016109c79796959493929190610f04565b6040516020818303038152906040528051906020012060001c600054146109ed57600080fd5b336001600160a01b03841614610a0257600080fd5b600a87014310610a1157600080fd5b3415610a1c57600080fd5b60038110610a2957600080fd5b6040517fb71d350b59ceca5c6544e5367d61ca8cae3e36b25f8d900743d063dff3d6508b90610a5c903031908490610e3c565b60405180910390a1600443878787878787604051602001610a84989796959493929190610f6c565b60408051601f19818403018152919052805160209091012060005550505050505050565b604051806080016040528060008152602001610ac2610ad6565b815260006020820181905260409091015290565b604051806040016040528060008152602001600081525090565b8035610afb81611027565b92915050565b8035610afb8161103e565b60008060008060808587031215610b2257600080fd5b6000610b2e8787610b01565b9450506020610b3f87828801610af0565b9350506040610b5087828801610b01565b9250506060610b6187828801610b01565b91505092959194509250565b600080600080600060a08688031215610b8557600080fd5b6000610b918888610b01565b9550506020610ba288828901610af0565b9450506040610bb388828901610b01565b9350506060610bc488828901610b01565b9250506080610bd588828901610af0565b9150509295509295909350565b60008060008060008060c08789031215610bfb57600080fd5b6000610c078989610b01565b9650506020610c1889828a01610af0565b9550506040610c2989828a01610b01565b9450506060610c3a89828a01610b01565b9350506080610c4b89828a01610af0565b92505060a0610c5c89828a01610b01565b9150509295509295509295565b600080600080600080600060e0888a031215610c8457600080fd5b6000610c908a8a610b01565b9750506020610ca18a828b01610af0565b9650506040610cb28a828b01610b01565b9550506060610cc38a828b01610b01565b9450506080610cd48a828b01610af0565b93505060a0610ce58a828b01610b01565b92505060c0610cf68a828b01610b01565b91505092959891949750929550565b60008060008060008060008060006101208a8c031215610d2457600080fd5b6000610d308c8c610b01565b9950506020610d418c828d01610af0565b9850506040610d528c828d01610b01565b9750506060610d638c828d01610b01565b9650506080610d748c828d01610af0565b95505060a0610d858c828d01610b01565b94505060c0610d968c828d01610b01565b93505060e0610da78c828d01610b01565b925050610100610db98c828d01610b01565b9150509295985092959850929598565b600080600060608486031215610dde57600080fd5b6000610dea8686610b01565b9350506020610dfb86828701610b01565b9250506040610e0c86828701610b01565b9150509250925092565b610e1f81611013565b82525050565b610e1f81611024565b60208101610afb8284610e25565b60408101610e4a8285610e25565b610e576020830184610e25565b9392505050565b60a08101610e6c8288610e25565b610e796020830187610e25565b610e866040830186610e16565b610e936060830185610e25565b610ea06080830184610e25565b9695505050505050565b60c08101610eb88289610e25565b610ec56020830188610e25565b610ed26040830187610e16565b610edf6060830186610e25565b610eec6080830185610e25565b610ef960a0830184610e16565b979650505050505050565b60e08101610f12828a610e25565b610f1f6020830189610e25565b610f2c6040830188610e16565b610f396060830187610e25565b610f466080830186610e25565b610f5360a0830185610e16565b610f6060c0830184610e25565b98975050505050505050565b6101008101610f7b828b610e25565b610f88602083018a610e25565b610f956040830189610e16565b610fa26060830188610e25565b610faf6080830187610e25565b610fbc60a0830186610e16565b610fc960c0830185610e25565b610fd660e0830184610e25565b9998505050505050505050565b60608101610ff18286610e25565b610ffe6020830185610e25565b61100b6040830184610e25565b949350505050565b60006001600160a01b038216610afb565b90565b61103081611013565b811461103b57600080fd5b50565b6110308161102456fea365627a7a72315820f13f4ef534927e82222473c9d1326c7aaa55f679cec807c5214e76bd334840716c6578706572696d656e74616cf564736f6c634300050d0040` };