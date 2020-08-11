// Automatically generated with Reach 0.1.0

export async function A(stdlib, ctc, interact) {
  const txn0 = { balance: 0, value: 0 };
  const v2 = stdlib.protect(stdlib.Array(stdlib.UInt256, stdlib.UInt256), await interact.getParams());
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
  stdlib.assert(true);
  const txn2 = await ctc.recv('A', 2, 0, 10);
  if (txn2.didTimeout) {
    
    const txn3 = await ctc.sendrecv('A', 10, 0, [v7, v5, v6], 0, false, null);
    const [] = txn3.data;
    const v22 = txn3.value;
    const v23 = stdlib.eq(0, v22);
    stdlib.assert(v23);
    return 'Bob quits' }
  else {
    const [] = txn2.data;
    const v14 = txn2.from;
    const v15 = txn2.value;
    const v16 = stdlib.eq(v5, v15);
    stdlib.assert(v16);
    let v25 = 0;
    let v26 = 1;
    while ((() => {
      const v50 = stdlib.eq(v26, 1);
      
      return v50; })()) {
      let v53;
      const v54 = stdlib.protect(stdlib.Bytes, await interact.getHand());
      const v55 = stdlib.bytes_eq(v54, 'ROCK');
      const v56 = stdlib.bytes_eq(v54, 'PAPER');
      const v57 = stdlib.bytes_eq(v54, 'SCISSORS');
      const v59 = v55 ? true : v56;
      const v61 = v59 ? true : v57;
      stdlib.assert(v61);
      if (v55) {
        v53 = 0;
         }
      else {
        if (v56) {
          v53 = 1;
           }
        else {
          v53 = 2;
           }
         }
      const v69 = stdlib.random_uint256();
      const v70 = stdlib.keccak256(v69, v53);
      stdlib.protect(stdlib.Null, await interact.commits());
      
      stdlib.assert(true);
      
      const txn3 = await ctc.sendrecv('A', 4, 1, [v7, v5, v6, v14, v25, v70], 0, 10, null);
      if (txn3.didTimeout) {
        const txn4 = await ctc.recv('A', 9, 0, false);
        const [] = txn4.data;
        const v80 = txn4.value;
        const v81 = stdlib.eq(0, v80);
        stdlib.assert(v81);
        return 'Alice quits' }
      else {
        const [v72] = txn3.data;
        const v73 = txn3.value;
        const v74 = stdlib.eq(0, v73);
        stdlib.assert(v74);
        stdlib.assert(true);
        const txn4 = await ctc.recv('A', 5, 1, 10);
        if (txn4.didTimeout) {
          
          const txn5 = await ctc.sendrecv('A', 8, 0, [v7, v5, v6, v14, v72, v25], 0, false, null);
          const [] = txn5.data;
          const v109 = txn5.value;
          const v110 = stdlib.eq(0, v109);
          stdlib.assert(v110);
          return 'Bob quits' }
        else {
          const [v101] = txn4.data;
          const v102 = txn4.value;
          const v103 = stdlib.eq(0, v102);
          stdlib.assert(v103);
          const v113 = stdlib.le(0, v101);
          const v114 = stdlib.lt(v101, 3);
          const v116 = v113 ? v114 : false;
          stdlib.assert(v116);
          let v118;
          const v120 = stdlib.le(0, v101);
          const v121 = stdlib.lt(v101, 3);
          const v123 = v120 ? v121 : false;
          stdlib.assert(v123);
          const v124 = stdlib.eq(v101, 0);
          if (v124) {
            v118 = 'ROCK';
             }
          else {
            const v125 = stdlib.eq(v101, 1);
            if (v125) {
              v118 = 'PAPER';
               }
            else {
              v118 = 'SCISSORS';
               }
             }
          stdlib.protect(stdlib.Null, await interact.reveals(v118));
          
          stdlib.assert(true);
          
          const txn5 = await ctc.sendrecv('A', 6, 2, [v7, v5, v6, v14, v72, v101, v25, v69, v53], 0, 10, null);
          if (txn5.didTimeout) {
            const txn6 = await ctc.recv('A', 7, 0, false);
            const [] = txn6.data;
            const v136 = txn6.value;
            const v137 = stdlib.eq(0, v136);
            stdlib.assert(v137);
            return 'Alice quits' }
          else {
            const [v127, v128] = txn5.data;
            const v129 = txn5.value;
            const v130 = stdlib.eq(0, v129);
            stdlib.assert(v130);
            const v140 = stdlib.keccak256(v127, v128);
            const v141 = stdlib.eq(v72, v140);
            stdlib.assert(v141);
            const v143 = stdlib.le(0, v128);
            const v144 = stdlib.lt(v128, 3);
            const v146 = v143 ? v144 : false;
            stdlib.assert(v146);
            let v148;
            const v150 = stdlib.le(0, v128);
            const v151 = stdlib.lt(v128, 3);
            const v153 = v150 ? v151 : false;
            const v155 = stdlib.le(0, v101);
            const v156 = stdlib.lt(v101, 3);
            const v158 = v155 ? v156 : false;
            const v160 = v153 ? v158 : false;
            if (v160) {
              const v161 = stdlib.sub(4, v101);
              const v162 = stdlib.add(v128, v161);
              const v163 = stdlib.mod(v162, 3);
              v148 = v163;
               }
            else {
              if (v153) {
                v148 = 2;
                 }
              else {
                if (v158) {
                  v148 = 0;
                   }
                else {
                  v148 = 1;
                   }
                 }
               }
            const v221 = stdlib.add(1, v25);
            v25 = v221;
            v26 = v148;
            continue; } } } }
    let v226;
    const v227 = stdlib.eq(v26, 2);
    if (v227) {
      const v228 = stdlib.mul(2, v5);
      v226 = [v228, 0];
       }
    else {
      const v229 = stdlib.eq(v26, 0);
      if (v229) {
        const v230 = stdlib.mul(2, v5);
        v226 = [0, v230];
         }
      else {
        v226 = [v5, v5];
         }
       }
    let v234;
    const v236 = stdlib.le(0, v26);
    const v237 = stdlib.lt(v26, 5);
    const v239 = v236 ? v237 : false;
    stdlib.assert(v239);
    const v240 = stdlib.eq(v26, 0);
    if (v240) {
      v234 = 'Bob wins';
       }
    else {
      const v241 = stdlib.eq(v26, 1);
      if (v241) {
        v234 = 'Draw';
         }
      else {
        const v242 = stdlib.eq(v26, 2);
        if (v242) {
          v234 = 'Alice wins';
           }
        else {
          const v243 = stdlib.eq(v26, 3);
          if (v243) {
            v234 = 'Alice quits';
             }
          else {
            v234 = 'Bob quits';
             }
           }
         }
       }
    return v234 } }
export async function B(stdlib, ctc, interact) {
  const txn0 = { balance: 0, value: 0 };
  const txn1 = await ctc.recv('B', 1, 2, false);
  const [v5, v6] = txn1.data;
  const v7 = txn1.from;
  const v9 = stdlib.add(v5, v6);
  const v10 = txn1.value;
  const v11 = stdlib.eq(v9, v10);
  stdlib.assert(v11);
  stdlib.protect(stdlib.Null, await interact.acceptParams(v5, v6));
  
  stdlib.assert(true);
  
  const txn2 = await ctc.sendrecv('B', 2, 0, [v7, v5, v6], v5, 10, null);
  if (txn2.didTimeout) {
    const txn3 = await ctc.recv('B', 10, 0, false);
    const [] = txn3.data;
    const v22 = txn3.value;
    const v23 = stdlib.eq(0, v22);
    stdlib.assert(v23);
    return 'Bob quits' }
  else {
    const [] = txn2.data;
    const v14 = txn2.from;
    const v15 = txn2.value;
    const v16 = stdlib.eq(v5, v15);
    stdlib.assert(v16);
    let v25 = 0;
    let v26 = 1;
    while ((() => {
      const v50 = stdlib.eq(v26, 1);
      
      return v50; })()) {
      stdlib.assert(true);
      const txn3 = await ctc.recv('B', 4, 1, 10);
      if (txn3.didTimeout) {
        
        const txn4 = await ctc.sendrecv('B', 9, 0, [v7, v5, v6, v14, v25], 0, false, null);
        const [] = txn4.data;
        const v80 = txn4.value;
        const v81 = stdlib.eq(0, v80);
        stdlib.assert(v81);
        return 'Alice quits' }
      else {
        const [v72] = txn3.data;
        const v73 = txn3.value;
        const v74 = stdlib.eq(0, v73);
        stdlib.assert(v74);
        let v85;
        const v86 = stdlib.protect(stdlib.Bytes, await interact.getHand());
        const v87 = stdlib.bytes_eq(v86, 'ROCK');
        const v88 = stdlib.bytes_eq(v86, 'PAPER');
        const v89 = stdlib.bytes_eq(v86, 'SCISSORS');
        const v91 = v87 ? true : v88;
        const v93 = v91 ? true : v89;
        stdlib.assert(v93);
        if (v87) {
          v85 = 0;
           }
        else {
          if (v88) {
            v85 = 1;
             }
          else {
            v85 = 2;
             }
           }
        stdlib.protect(stdlib.Null, await interact.shows());
        
        stdlib.assert(true);
        
        const txn4 = await ctc.sendrecv('B', 5, 1, [v7, v5, v6, v14, v72, v25, v85], 0, 10, null);
        if (txn4.didTimeout) {
          const txn5 = await ctc.recv('B', 8, 0, false);
          const [] = txn5.data;
          const v109 = txn5.value;
          const v110 = stdlib.eq(0, v109);
          stdlib.assert(v110);
          return 'Bob quits' }
        else {
          const [v101] = txn4.data;
          const v102 = txn4.value;
          const v103 = stdlib.eq(0, v102);
          stdlib.assert(v103);
          const v113 = stdlib.le(0, v101);
          const v114 = stdlib.lt(v101, 3);
          const v116 = v113 ? v114 : false;
          stdlib.assert(v116);
          stdlib.assert(true);
          const txn5 = await ctc.recv('B', 6, 2, 10);
          if (txn5.didTimeout) {
            
            const txn6 = await ctc.sendrecv('B', 7, 0, [v7, v5, v6, v14, v72, v101, v25], 0, false, null);
            const [] = txn6.data;
            const v136 = txn6.value;
            const v137 = stdlib.eq(0, v136);
            stdlib.assert(v137);
            return 'Alice quits' }
          else {
            const [v127, v128] = txn5.data;
            const v129 = txn5.value;
            const v130 = stdlib.eq(0, v129);
            stdlib.assert(v130);
            const v140 = stdlib.keccak256(v127, v128);
            const v141 = stdlib.eq(v72, v140);
            stdlib.assert(v141);
            const v143 = stdlib.le(0, v128);
            const v144 = stdlib.lt(v128, 3);
            const v146 = v143 ? v144 : false;
            stdlib.assert(v146);
            let v148;
            const v150 = stdlib.le(0, v128);
            const v151 = stdlib.lt(v128, 3);
            const v153 = v150 ? v151 : false;
            const v155 = stdlib.le(0, v101);
            const v156 = stdlib.lt(v101, 3);
            const v158 = v155 ? v156 : false;
            const v160 = v153 ? v158 : false;
            if (v160) {
              const v161 = stdlib.sub(4, v101);
              const v162 = stdlib.add(v128, v161);
              const v163 = stdlib.mod(v162, 3);
              v148 = v163;
               }
            else {
              if (v153) {
                v148 = 2;
                 }
              else {
                if (v158) {
                  v148 = 0;
                   }
                else {
                  v148 = 1;
                   }
                 }
               }
            const v221 = stdlib.add(1, v25);
            v25 = v221;
            v26 = v148;
            continue; } } } }
    let v226;
    const v227 = stdlib.eq(v26, 2);
    if (v227) {
      const v228 = stdlib.mul(2, v5);
      v226 = [v228, 0];
       }
    else {
      const v229 = stdlib.eq(v26, 0);
      if (v229) {
        const v230 = stdlib.mul(2, v5);
        v226 = [0, v230];
         }
      else {
        v226 = [v5, v5];
         }
       }
    let v234;
    const v236 = stdlib.le(0, v26);
    const v237 = stdlib.lt(v26, 5);
    const v239 = v236 ? v237 : false;
    stdlib.assert(v239);
    const v240 = stdlib.eq(v26, 0);
    if (v240) {
      v234 = 'Bob wins';
       }
    else {
      const v241 = stdlib.eq(v26, 1);
      if (v241) {
        v234 = 'Draw';
         }
      else {
        const v242 = stdlib.eq(v26, 2);
        if (v242) {
          v234 = 'Alice wins';
           }
        else {
          const v243 = stdlib.eq(v26, 3);
          if (v243) {
            v234 = 'Alice quits';
             }
          else {
            v234 = 'Bob quits';
             }
           }
         }
       }
    return v234 } }
export async function O(stdlib, ctc, interact) {
  const txn0 = { balance: 0, value: 0 };
  const txn1 = await ctc.recv('O', 1, 2, false);
  const [v5, v6] = txn1.data;
  const v7 = txn1.from;
  const v9 = stdlib.add(v5, v6);
  const v10 = txn1.value;
  const v11 = stdlib.eq(v9, v10);
  stdlib.assert(v11);
  stdlib.assert(true);
  const txn2 = await ctc.recv('O', 2, 0, 10);
  if (txn2.didTimeout) {
    const txn3 = await ctc.recv('O', 10, 0, false);
    const [] = txn3.data;
    const v22 = txn3.value;
    const v23 = stdlib.eq(0, v22);
    stdlib.assert(v23);
    return 'Bob quits' }
  else {
    const [] = txn2.data;
    const v14 = txn2.from;
    const v15 = txn2.value;
    const v16 = stdlib.eq(v5, v15);
    stdlib.assert(v16);
    let v25 = 0;
    let v26 = 1;
    while ((() => {
      const v50 = stdlib.eq(v26, 1);
      
      return v50; })()) {
      stdlib.assert(true);
      const txn3 = await ctc.recv('O', 4, 1, 10);
      if (txn3.didTimeout) {
        const txn4 = await ctc.recv('O', 9, 0, false);
        const [] = txn4.data;
        const v80 = txn4.value;
        const v81 = stdlib.eq(0, v80);
        stdlib.assert(v81);
        return 'Alice quits' }
      else {
        const [v72] = txn3.data;
        const v73 = txn3.value;
        const v74 = stdlib.eq(0, v73);
        stdlib.assert(v74);
        stdlib.assert(true);
        const txn4 = await ctc.recv('O', 5, 1, 10);
        if (txn4.didTimeout) {
          const txn5 = await ctc.recv('O', 8, 0, false);
          const [] = txn5.data;
          const v109 = txn5.value;
          const v110 = stdlib.eq(0, v109);
          stdlib.assert(v110);
          return 'Bob quits' }
        else {
          const [v101] = txn4.data;
          const v102 = txn4.value;
          const v103 = stdlib.eq(0, v102);
          stdlib.assert(v103);
          const v113 = stdlib.le(0, v101);
          const v114 = stdlib.lt(v101, 3);
          const v116 = v113 ? v114 : false;
          stdlib.assert(v116);
          stdlib.assert(true);
          const txn5 = await ctc.recv('O', 6, 2, 10);
          if (txn5.didTimeout) {
            const txn6 = await ctc.recv('O', 7, 0, false);
            const [] = txn6.data;
            const v136 = txn6.value;
            const v137 = stdlib.eq(0, v136);
            stdlib.assert(v137);
            return 'Alice quits' }
          else {
            const [v127, v128] = txn5.data;
            const v129 = txn5.value;
            const v130 = stdlib.eq(0, v129);
            stdlib.assert(v130);
            const v140 = stdlib.keccak256(v127, v128);
            const v141 = stdlib.eq(v72, v140);
            stdlib.assert(v141);
            const v143 = stdlib.le(0, v128);
            const v144 = stdlib.lt(v128, 3);
            const v146 = v143 ? v144 : false;
            stdlib.assert(v146);
            let v148;
            const v150 = stdlib.le(0, v128);
            const v151 = stdlib.lt(v128, 3);
            const v153 = v150 ? v151 : false;
            const v155 = stdlib.le(0, v101);
            const v156 = stdlib.lt(v101, 3);
            const v158 = v155 ? v156 : false;
            const v160 = v153 ? v158 : false;
            if (v160) {
              const v161 = stdlib.sub(4, v101);
              const v162 = stdlib.add(v128, v161);
              const v163 = stdlib.mod(v162, 3);
              v148 = v163;
               }
            else {
              if (v153) {
                v148 = 2;
                 }
              else {
                if (v158) {
                  v148 = 0;
                   }
                else {
                  v148 = 1;
                   }
                 }
               }
            const v221 = stdlib.add(1, v25);
            v25 = v221;
            v26 = v148;
            continue; } } } }
    let v226;
    const v227 = stdlib.eq(v26, 2);
    if (v227) {
      const v228 = stdlib.mul(2, v5);
      v226 = [v228, 0];
       }
    else {
      const v229 = stdlib.eq(v26, 0);
      if (v229) {
        const v230 = stdlib.mul(2, v5);
        v226 = [0, v230];
         }
      else {
        v226 = [v5, v5];
         }
       }
    let v234;
    const v236 = stdlib.le(0, v26);
    const v237 = stdlib.lt(v26, 5);
    const v239 = v236 ? v237 : false;
    stdlib.assert(v239);
    const v240 = stdlib.eq(v26, 0);
    if (v240) {
      v234 = 'Bob wins';
       }
    else {
      const v241 = stdlib.eq(v26, 1);
      if (v241) {
        v234 = 'Draw';
         }
      else {
        const v242 = stdlib.eq(v26, 2);
        if (v242) {
          v234 = 'Alice wins';
           }
        else {
          const v243 = stdlib.eq(v26, 3);
          if (v243) {
            v234 = 'Alice quits';
             }
          else {
            v234 = 'Bob quits';
             }
           }
         }
       }
    return v234 } }

export const ETH = {
  ABI: `[{"inputs":[],"payable":true,"stateMutability":"payable","type":"constructor"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v5","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v6","type":"uint256"}],"name":"e1","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e10","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e2","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v72","type":"uint256"}],"name":"e4","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v101","type":"uint256"}],"name":"e5","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v127","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v128","type":"uint256"}],"name":"e6","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e7","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e8","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e9","type":"event"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"}],"name":"m1","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v7","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"}],"name":"m10","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v7","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"}],"name":"m2","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v7","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"},{"internalType":"address payable","name":"v14","type":"address"},{"internalType":"uint256","name":"v25","type":"uint256"},{"internalType":"uint256","name":"v72","type":"uint256"}],"name":"m4","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v7","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"},{"internalType":"address payable","name":"v14","type":"address"},{"internalType":"uint256","name":"v72","type":"uint256"},{"internalType":"uint256","name":"v25","type":"uint256"},{"internalType":"uint256","name":"v101","type":"uint256"}],"name":"m5","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v7","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"},{"internalType":"address payable","name":"v14","type":"address"},{"internalType":"uint256","name":"v72","type":"uint256"},{"internalType":"uint256","name":"v101","type":"uint256"},{"internalType":"uint256","name":"v25","type":"uint256"},{"internalType":"uint256","name":"v127","type":"uint256"},{"internalType":"uint256","name":"v128","type":"uint256"}],"name":"m6","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v7","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"},{"internalType":"address payable","name":"v14","type":"address"},{"internalType":"uint256","name":"v72","type":"uint256"},{"internalType":"uint256","name":"v101","type":"uint256"},{"internalType":"uint256","name":"v25","type":"uint256"}],"name":"m7","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v7","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"},{"internalType":"address payable","name":"v14","type":"address"},{"internalType":"uint256","name":"v72","type":"uint256"},{"internalType":"uint256","name":"v25","type":"uint256"}],"name":"m8","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v7","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"},{"internalType":"address payable","name":"v14","type":"address"},{"internalType":"uint256","name":"v25","type":"uint256"}],"name":"m9","outputs":[],"payable":true,"stateMutability":"payable","type":"function"}]`,
  Bytecode: `0x600060a08181524360c0526040608081905260e0815290209055610e7e806100286000396000f3fe6080604052600436106100865760003560e01c80637a52ccb3116100595780637a52ccb3146101db5780639337cbb3146102045780639ccddd3a1461023c578063b6ce11cf14610274578063bb91d6e3146102c957610086565b806315e74bc71461008b57806352ecf27a146100e257806368e59173146101455780637984da481461018d575b600080fd5b6100e060048036036101008110156100a257600080fd5b508035906001600160a01b03602082013581169160408101359160608201359160808101359091169060a08101359060c08101359060e00135610317565b005b6100e060048036036101408110156100f957600080fd5b508035906001600160a01b03602082013581169160408101359160608201359160808101359091169060a08101359060c08101359060e08101359061010081013590610120013561040f565b6100e0600480360360c081101561015b57600080fd5b508035906001600160a01b03602082013581169160408101359160608201359160808101359091169060a001356105d1565b6100e0600480360360e08110156101a357600080fd5b508035906001600160a01b03602082013581169160408101359160608201359160808101359091169060a08101359060c001356106b1565b6100e0600480360360608110156101f157600080fd5b5080359060208101359060400135610799565b6100e06004803603608081101561021a57600080fd5b508035906001600160a01b03602082013516906040810135906060013561086b565b6100e06004803603608081101561025257600080fd5b508035906001600160a01b036020820135169060408101359060600135610936565b6100e0600480360361010081101561028b57600080fd5b508035906001600160a01b03602082013581169160408101359160608201359160808101359091169060a08101359060c08101359060e001356109f6565b6100e0600480360360e08110156102df57600080fd5b508035906001600160a01b03602082013581169160408101359160608201359160808101359091169060a08101359060c00135610b5b565b6040805160056020808301919091528183018b90526001600160601b031960608b811b821681850152607484018b9052609484018a905288901b1660b483015260c8820186905260e882018590526101088083018590528351808403909101815261012890920190925280519101206000541461039357600080fd5b336001600160a01b038516146103a857600080fd5b600a880143101580156103bd5750601e880143105b6103c657600080fd5b34156103d157600080fd5b6040516001600160a01b03851690303180156108fc02916000818181858888f19350505050158015610407573d6000803e3d6000fd5b506000805533ff5b6040805160056020808301919091528183018d90526001600160601b031960608d811b821681850152607484018d9052609484018c90528a901b1660b483015260c8820188905260e882018790526101088083018790528351808403909101815261012890920190925280519101206000541461048b57600080fd5b610493610df7565b336001600160a01b038b16146104a857600080fd5b60288b0143106104b757600080fd5b34156104c257600080fd5b6040805160208082018690528183018590528251808303840181526060909201909252805191012086146104f557600080fd5b6003821061050257600080fd5b6003808310602083018190529086106040830152610521576000610527565b80604001515b156105435760038560040383018161053b57fe5b06815261056e565b806020015115610556576002815261056e565b806040015115610569576000815261056e565b600181525b6105838a8a8a8a886001018660000151610ca2565b60408051303181526020810185905280820184905290517f3a17d070a78674d0fecc57e42515183089cfc8c61973a362a95c57f4d000ea849181900360600190a15050505050505050505050565b6040805160036020808301919091528183018990526001600160601b0319606089811b821681850152607484018990526094840188905286901b1660b483015260c88083018590528351808403909101815260e890920190925280519101206000541461063d57600080fd5b336001600160a01b0383161461065257600080fd5b600a860143101580156106675750600a860143105b61067057600080fd5b341561067b57600080fd5b6040516001600160a01b03831690303180156108fc02916000818181858888f19350505050158015610407573d6000803e3d6000fd5b6040805160046020808301919091528183018a90526001600160601b031960608a811b821681850152607484018a90526094840189905287901b1660b483015260c8820185905260e88083018590528351808403909101815261010890920190925280519101206000541461072557600080fd5b336001600160a01b0387161461073a57600080fd5b600a8701431015801561074f57506014870143105b61075857600080fd5b341561076357600080fd5b6040516001600160a01b03871690303180156108fc02916000818181858888f19350505050158015610407573d6000803e3d6000fd5b60408051600060208083018290528284018790528351808403850181526060909301909352815191909201209054146107d157600080fd5b34818301146107df57600080fd5b604080516001602080830191909152438284015233606090811b908301526074820185905260948083018590528351808403909101815260b48301808552815191909201206000553031905260d4810184905260f4810183905290517f219cc811755104876269c7553666684eaaeecb90b6a7ffc6fdd5068140059b8e918190036101140190a1505050565b604080516001602080830191909152818301879052606086811b6001600160601b031916908301526074820185905260948083018590528351808403909101815260b49092019092528051910120600054146108c657600080fd5b336001600160a01b038416146108db57600080fd5b600a840143101580156108ec575060015b6108f557600080fd5b341561090057600080fd5b6040516001600160a01b03841690303180156108fc02916000818181858888f19350505050158015610407573d6000803e3d6000fd5b604080516001602080830191909152818301879052606086811b6001600160601b031916908301526074820185905260948083018590528351808403909101815260b490920190925280519101206000541461099157600080fd5b600a840143106109a057600080fd5b3482146109ac57600080fd5b6109bc8383833360006001610ca2565b604080513031815290517ff04f5fc87a72102f7c0b228f8bbaf9b9aa7a2b5dc295c86538fdde91e95866e99181900360200190a150505050565b6040805160046020808301919091528183018b90526001600160601b031960608b811b821681850152607484018b9052609484018a905288901b1660b483015260c8820186905260e880830186905283518084039091018152610108909201909252805191012060005414610a6a57600080fd5b336001600160a01b03851614610a7f57600080fd5b601e88014310610a8e57600080fd5b3415610a9957600080fd5b60038110610aa657600080fd5b60408051600560208083019190915243828401526001600160601b031960608b811b821681850152607484018b9052609484018a905288901b1660b483015260c8820186905260e882018490526101088083018690528351808403909101815261012883018085528151919092012060005530319052610148810183905290517fabf482d77b67111a4971bb96fe81961f83ba459eb1d8fa9f78b6908251aeef1a918190036101680190a15050505050505050565b6040805160036020808301919091528183018a90526001600160601b031960608a811b821681850152607484018a90526094840189905287901b1660b483015260c88083018690528351808403909101815260e8909201909252805191012060005414610bc757600080fd5b336001600160a01b03871614610bdc57600080fd5b601487014310610beb57600080fd5b3415610bf657600080fd5b60408051600460208083019190915243828401526001600160601b031960608a811b821681850152607484018a90526094840189905287901b1660b483015260c8820184905260e88083018690528351808403909101815261010883018085528151919092012060005530319052610128810183905290517fb71d350b59ceca5c6544e5367d61ca8cae3e36b25f8d900743d063dff3d6508b918190036101480190a150505050505050565b610caa610e17565b6001821415610d195760408051600360208083019190915243828401526001600160601b031960608b811b821681850152607484018b9052609484018a905288901b1660b483015260c88083018790528351808403909101815260e89092019092528051910120600055610dee565b6002821415610d405760408051808201909152600287028152600060208201528152610d7a565b81610d635760408051808201909152600081526002870260208201528152610d7a565b604080518082019091528681526020810187905281525b8051516040516001600160a01b03891691870180156108fc02916000818181858888f19350505050158015610db3573d6000803e3d6000fd5b508051602001516040516001600160a01b0386169180156108fc02916000818181858888f19350505050158015610407573d6000803e3d6000fd5b50505050505050565b604080516060810182526000808252602082018190529181019190915290565b6040518060200160405280610e2a610e2f565b905290565b60405180604001604052806000815260200160008152509056fea265627a7a723158200d8fe427c94a6b2e78c7805af71914dd1abb825bb733056f4f6e15533030b4a264736f6c634300050c0032` };