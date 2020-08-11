// Automatically generated with Reach 0.1.0

export async function A(stdlib, ctc, interact) {
  const txn0 = { balance: 0, value: 0 };
  const v2 = await interact.getParams();
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
    
    const txn3 = await ctc.sendrecv('A', 9, 0, [v7, v5, v6], 0, false, null);
    const [] = txn3.data;
    const v23 = txn3.value;
    const v24 = stdlib.eq(0, v23);
    stdlib.assert(v24);
    return 'Bob quits' }
  else {
    const [] = txn2.data;
    const v15 = txn2.from;
    const v16 = txn2.value;
    const v17 = stdlib.eq(v5, v16);
    stdlib.assert(v17);
    await interact.partnerIs(v15);
    let v29;
    const v30 = await interact.getHand();
    const v31 = stdlib.bytes_eq(v30, 'ROCK');
    const v32 = stdlib.bytes_eq(v30, 'PAPER');
    const v33 = stdlib.bytes_eq(v30, 'SCISSORS');
    const v35 = v31 ? true : v32;
    const v37 = v35 ? true : v33;
    stdlib.assert(v37);
    if (v31) {
      v29 = 0;
       }
    else {
      if (v32) {
        v29 = 1;
         }
      else {
        v29 = 2;
         }
       }
    const v45 = stdlib.random_uint256();
    const v46 = stdlib.keccak256(v45, v29);
    await interact.commits();
    
    stdlib.assert(true);
    
    const txn3 = await ctc.sendrecv('A', 3, 1, [v7, v5, v6, v15, v46], 0, 10, null);
    if (txn3.didTimeout) {
      const txn4 = await ctc.recv('A', 8, 0, false);
      const [] = txn4.data;
      const v56 = txn4.value;
      const v57 = stdlib.eq(0, v56);
      stdlib.assert(v57);
      return 'Alice quits' }
    else {
      const [v48] = txn3.data;
      const v49 = txn3.value;
      const v50 = stdlib.eq(0, v49);
      stdlib.assert(v50);
      stdlib.assert(true);
      const txn4 = await ctc.recv('A', 4, 1, 10);
      if (txn4.didTimeout) {
        
        const txn5 = await ctc.sendrecv('A', 7, 0, [v7, v5, v6, v15, v48], 0, false, null);
        const [] = txn5.data;
        const v85 = txn5.value;
        const v86 = stdlib.eq(0, v85);
        stdlib.assert(v86);
        return 'Bob quits' }
      else {
        const [v77] = txn4.data;
        const v78 = txn4.value;
        const v79 = stdlib.eq(0, v78);
        stdlib.assert(v79);
        const v89 = stdlib.le(0, v77);
        const v90 = stdlib.lt(v77, 3);
        const v92 = v89 ? v90 : false;
        stdlib.assert(v92);
        let v94;
        const v96 = stdlib.le(0, v77);
        const v97 = stdlib.lt(v77, 3);
        const v99 = v96 ? v97 : false;
        stdlib.assert(v99);
        const v100 = stdlib.eq(v77, 0);
        if (v100) {
          v94 = 'ROCK';
           }
        else {
          const v101 = stdlib.eq(v77, 1);
          if (v101) {
            v94 = 'PAPER';
             }
          else {
            v94 = 'SCISSORS';
             }
           }
        await interact.reveals(v94);
        
        stdlib.assert(true);
        
        const txn5 = await ctc.sendrecv('A', 5, 2, [v7, v5, v6, v15, v48, v77, v45, v29], 0, 10, null);
        if (txn5.didTimeout) {
          const txn6 = await ctc.recv('A', 6, 0, false);
          const [] = txn6.data;
          const v112 = txn6.value;
          const v113 = stdlib.eq(0, v112);
          stdlib.assert(v113);
          return 'Alice quits' }
        else {
          const [v103, v104] = txn5.data;
          const v105 = txn5.value;
          const v106 = stdlib.eq(0, v105);
          stdlib.assert(v106);
          const v116 = stdlib.keccak256(v103, v104);
          const v117 = stdlib.eq(v48, v116);
          stdlib.assert(v117);
          const v119 = stdlib.le(0, v104);
          const v120 = stdlib.lt(v104, 3);
          const v122 = v119 ? v120 : false;
          stdlib.assert(v122);
          let v124;
          const v126 = stdlib.le(0, v104);
          const v127 = stdlib.lt(v104, 3);
          const v129 = v126 ? v127 : false;
          const v131 = stdlib.le(0, v77);
          const v132 = stdlib.lt(v77, 3);
          const v134 = v131 ? v132 : false;
          const v136 = v129 ? v134 : false;
          if (v136) {
            const v137 = stdlib.sub(4, v77);
            const v138 = stdlib.add(v104, v137);
            const v139 = stdlib.mod(v138, 3);
            v124 = v139;
             }
          else {
            if (v129) {
              v124 = 2;
               }
            else {
              if (v134) {
                v124 = 0;
                 }
              else {
                v124 = 1;
                 }
               }
             }
          const v197 = stdlib.eq(v124, 2);
          if (v197) {
            let v198;
            const v199 = stdlib.eq(v124, 2);
            if (v199) {
              const v200 = stdlib.mul(2, v5);
              v198 = [v200, 0];
               }
            else {
              const v201 = stdlib.eq(v124, 0);
              if (v201) {
                const v202 = stdlib.mul(2, v5);
                v198 = [0, v202];
                 }
              else {
                v198 = [v5, v5];
                 }
               }
            let v206;
            const v208 = stdlib.le(0, v124);
            const v209 = stdlib.lt(v124, 5);
            const v211 = v208 ? v209 : false;
            stdlib.assert(v211);
            const v212 = stdlib.eq(v124, 0);
            if (v212) {
              v206 = 'Bob wins';
               }
            else {
              const v213 = stdlib.eq(v124, 1);
              if (v213) {
                v206 = 'Draw';
                 }
              else {
                const v214 = stdlib.eq(v124, 2);
                if (v214) {
                  v206 = 'Alice wins';
                   }
                else {
                  const v215 = stdlib.eq(v124, 3);
                  if (v215) {
                    v206 = 'Alice quits';
                     }
                  else {
                    v206 = 'Bob quits';
                     }
                   }
                 }
               }
            return v206 }
          else {
            let v198;
            const v199 = stdlib.eq(v124, 2);
            if (v199) {
              const v200 = stdlib.mul(2, v5);
              v198 = [v200, 0];
               }
            else {
              const v201 = stdlib.eq(v124, 0);
              if (v201) {
                const v202 = stdlib.mul(2, v5);
                v198 = [0, v202];
                 }
              else {
                v198 = [v5, v5];
                 }
               }
            let v206;
            const v208 = stdlib.le(0, v124);
            const v209 = stdlib.lt(v124, 5);
            const v211 = v208 ? v209 : false;
            stdlib.assert(v211);
            const v212 = stdlib.eq(v124, 0);
            if (v212) {
              v206 = 'Bob wins';
               }
            else {
              const v213 = stdlib.eq(v124, 1);
              if (v213) {
                v206 = 'Draw';
                 }
              else {
                const v214 = stdlib.eq(v124, 2);
                if (v214) {
                  v206 = 'Alice wins';
                   }
                else {
                  const v215 = stdlib.eq(v124, 3);
                  if (v215) {
                    v206 = 'Alice quits';
                     }
                  else {
                    v206 = 'Bob quits';
                     }
                   }
                 }
               }
            return v206 } } } } } }
export async function B(stdlib, ctc, interact) {
  const txn0 = { balance: 0, value: 0 };
  const txn1 = await ctc.recv('B', 1, 2, false);
  const [v5, v6] = txn1.data;
  const v7 = txn1.from;
  const v9 = stdlib.add(v5, v6);
  const v10 = txn1.value;
  const v11 = stdlib.eq(v9, v10);
  stdlib.assert(v11);
  await interact.partnerIs(v7);
  await interact.acceptParams(v5, v6);
  
  stdlib.assert(true);
  
  const txn2 = await ctc.sendrecv('B', 2, 0, [v7, v5, v6], v5, 10, null);
  if (txn2.didTimeout) {
    const txn3 = await ctc.recv('B', 9, 0, false);
    const [] = txn3.data;
    const v23 = txn3.value;
    const v24 = stdlib.eq(0, v23);
    stdlib.assert(v24);
    return 'Bob quits' }
  else {
    const [] = txn2.data;
    const v15 = txn2.from;
    const v16 = txn2.value;
    const v17 = stdlib.eq(v5, v16);
    stdlib.assert(v17);
    stdlib.assert(true);
    const txn3 = await ctc.recv('B', 3, 1, 10);
    if (txn3.didTimeout) {
      
      const txn4 = await ctc.sendrecv('B', 8, 0, [v7, v5, v6, v15], 0, false, null);
      const [] = txn4.data;
      const v56 = txn4.value;
      const v57 = stdlib.eq(0, v56);
      stdlib.assert(v57);
      return 'Alice quits' }
    else {
      const [v48] = txn3.data;
      const v49 = txn3.value;
      const v50 = stdlib.eq(0, v49);
      stdlib.assert(v50);
      let v61;
      const v62 = await interact.getHand();
      const v63 = stdlib.bytes_eq(v62, 'ROCK');
      const v64 = stdlib.bytes_eq(v62, 'PAPER');
      const v65 = stdlib.bytes_eq(v62, 'SCISSORS');
      const v67 = v63 ? true : v64;
      const v69 = v67 ? true : v65;
      stdlib.assert(v69);
      if (v63) {
        v61 = 0;
         }
      else {
        if (v64) {
          v61 = 1;
           }
        else {
          v61 = 2;
           }
         }
      await interact.shows();
      
      stdlib.assert(true);
      
      const txn4 = await ctc.sendrecv('B', 4, 1, [v7, v5, v6, v15, v48, v61], 0, 10, null);
      if (txn4.didTimeout) {
        const txn5 = await ctc.recv('B', 7, 0, false);
        const [] = txn5.data;
        const v85 = txn5.value;
        const v86 = stdlib.eq(0, v85);
        stdlib.assert(v86);
        return 'Bob quits' }
      else {
        const [v77] = txn4.data;
        const v78 = txn4.value;
        const v79 = stdlib.eq(0, v78);
        stdlib.assert(v79);
        const v89 = stdlib.le(0, v77);
        const v90 = stdlib.lt(v77, 3);
        const v92 = v89 ? v90 : false;
        stdlib.assert(v92);
        stdlib.assert(true);
        const txn5 = await ctc.recv('B', 5, 2, 10);
        if (txn5.didTimeout) {
          
          const txn6 = await ctc.sendrecv('B', 6, 0, [v7, v5, v6, v15, v48, v77], 0, false, null);
          const [] = txn6.data;
          const v112 = txn6.value;
          const v113 = stdlib.eq(0, v112);
          stdlib.assert(v113);
          return 'Alice quits' }
        else {
          const [v103, v104] = txn5.data;
          const v105 = txn5.value;
          const v106 = stdlib.eq(0, v105);
          stdlib.assert(v106);
          const v116 = stdlib.keccak256(v103, v104);
          const v117 = stdlib.eq(v48, v116);
          stdlib.assert(v117);
          const v119 = stdlib.le(0, v104);
          const v120 = stdlib.lt(v104, 3);
          const v122 = v119 ? v120 : false;
          stdlib.assert(v122);
          let v124;
          const v126 = stdlib.le(0, v104);
          const v127 = stdlib.lt(v104, 3);
          const v129 = v126 ? v127 : false;
          const v131 = stdlib.le(0, v77);
          const v132 = stdlib.lt(v77, 3);
          const v134 = v131 ? v132 : false;
          const v136 = v129 ? v134 : false;
          if (v136) {
            const v137 = stdlib.sub(4, v77);
            const v138 = stdlib.add(v104, v137);
            const v139 = stdlib.mod(v138, 3);
            v124 = v139;
             }
          else {
            if (v129) {
              v124 = 2;
               }
            else {
              if (v134) {
                v124 = 0;
                 }
              else {
                v124 = 1;
                 }
               }
             }
          const v197 = stdlib.eq(v124, 2);
          if (v197) {
            let v198;
            const v199 = stdlib.eq(v124, 2);
            if (v199) {
              const v200 = stdlib.mul(2, v5);
              v198 = [v200, 0];
               }
            else {
              const v201 = stdlib.eq(v124, 0);
              if (v201) {
                const v202 = stdlib.mul(2, v5);
                v198 = [0, v202];
                 }
              else {
                v198 = [v5, v5];
                 }
               }
            let v206;
            const v208 = stdlib.le(0, v124);
            const v209 = stdlib.lt(v124, 5);
            const v211 = v208 ? v209 : false;
            stdlib.assert(v211);
            const v212 = stdlib.eq(v124, 0);
            if (v212) {
              v206 = 'Bob wins';
               }
            else {
              const v213 = stdlib.eq(v124, 1);
              if (v213) {
                v206 = 'Draw';
                 }
              else {
                const v214 = stdlib.eq(v124, 2);
                if (v214) {
                  v206 = 'Alice wins';
                   }
                else {
                  const v215 = stdlib.eq(v124, 3);
                  if (v215) {
                    v206 = 'Alice quits';
                     }
                  else {
                    v206 = 'Bob quits';
                     }
                   }
                 }
               }
            return v206 }
          else {
            let v198;
            const v199 = stdlib.eq(v124, 2);
            if (v199) {
              const v200 = stdlib.mul(2, v5);
              v198 = [v200, 0];
               }
            else {
              const v201 = stdlib.eq(v124, 0);
              if (v201) {
                const v202 = stdlib.mul(2, v5);
                v198 = [0, v202];
                 }
              else {
                v198 = [v5, v5];
                 }
               }
            let v206;
            const v208 = stdlib.le(0, v124);
            const v209 = stdlib.lt(v124, 5);
            const v211 = v208 ? v209 : false;
            stdlib.assert(v211);
            const v212 = stdlib.eq(v124, 0);
            if (v212) {
              v206 = 'Bob wins';
               }
            else {
              const v213 = stdlib.eq(v124, 1);
              if (v213) {
                v206 = 'Draw';
                 }
              else {
                const v214 = stdlib.eq(v124, 2);
                if (v214) {
                  v206 = 'Alice wins';
                   }
                else {
                  const v215 = stdlib.eq(v124, 3);
                  if (v215) {
                    v206 = 'Alice quits';
                     }
                  else {
                    v206 = 'Bob quits';
                     }
                   }
                 }
               }
            return v206 } } } } } }
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
    const txn3 = await ctc.recv('O', 9, 0, false);
    const [] = txn3.data;
    const v23 = txn3.value;
    const v24 = stdlib.eq(0, v23);
    stdlib.assert(v24);
    return 'Bob quits' }
  else {
    const [] = txn2.data;
    const v15 = txn2.from;
    const v16 = txn2.value;
    const v17 = stdlib.eq(v5, v16);
    stdlib.assert(v17);
    stdlib.assert(true);
    const txn3 = await ctc.recv('O', 3, 1, 10);
    if (txn3.didTimeout) {
      const txn4 = await ctc.recv('O', 8, 0, false);
      const [] = txn4.data;
      const v56 = txn4.value;
      const v57 = stdlib.eq(0, v56);
      stdlib.assert(v57);
      return 'Alice quits' }
    else {
      const [v48] = txn3.data;
      const v49 = txn3.value;
      const v50 = stdlib.eq(0, v49);
      stdlib.assert(v50);
      stdlib.assert(true);
      const txn4 = await ctc.recv('O', 4, 1, 10);
      if (txn4.didTimeout) {
        const txn5 = await ctc.recv('O', 7, 0, false);
        const [] = txn5.data;
        const v85 = txn5.value;
        const v86 = stdlib.eq(0, v85);
        stdlib.assert(v86);
        return 'Bob quits' }
      else {
        const [v77] = txn4.data;
        const v78 = txn4.value;
        const v79 = stdlib.eq(0, v78);
        stdlib.assert(v79);
        const v89 = stdlib.le(0, v77);
        const v90 = stdlib.lt(v77, 3);
        const v92 = v89 ? v90 : false;
        stdlib.assert(v92);
        stdlib.assert(true);
        const txn5 = await ctc.recv('O', 5, 2, 10);
        if (txn5.didTimeout) {
          const txn6 = await ctc.recv('O', 6, 0, false);
          const [] = txn6.data;
          const v112 = txn6.value;
          const v113 = stdlib.eq(0, v112);
          stdlib.assert(v113);
          return 'Alice quits' }
        else {
          const [v103, v104] = txn5.data;
          const v105 = txn5.value;
          const v106 = stdlib.eq(0, v105);
          stdlib.assert(v106);
          const v116 = stdlib.keccak256(v103, v104);
          const v117 = stdlib.eq(v48, v116);
          stdlib.assert(v117);
          const v119 = stdlib.le(0, v104);
          const v120 = stdlib.lt(v104, 3);
          const v122 = v119 ? v120 : false;
          stdlib.assert(v122);
          let v124;
          const v126 = stdlib.le(0, v104);
          const v127 = stdlib.lt(v104, 3);
          const v129 = v126 ? v127 : false;
          const v131 = stdlib.le(0, v77);
          const v132 = stdlib.lt(v77, 3);
          const v134 = v131 ? v132 : false;
          const v136 = v129 ? v134 : false;
          if (v136) {
            const v137 = stdlib.sub(4, v77);
            const v138 = stdlib.add(v104, v137);
            const v139 = stdlib.mod(v138, 3);
            v124 = v139;
             }
          else {
            if (v129) {
              v124 = 2;
               }
            else {
              if (v134) {
                v124 = 0;
                 }
              else {
                v124 = 1;
                 }
               }
             }
          const v197 = stdlib.eq(v124, 2);
          if (v197) {
            let v198;
            const v199 = stdlib.eq(v124, 2);
            if (v199) {
              const v200 = stdlib.mul(2, v5);
              v198 = [v200, 0];
               }
            else {
              const v201 = stdlib.eq(v124, 0);
              if (v201) {
                const v202 = stdlib.mul(2, v5);
                v198 = [0, v202];
                 }
              else {
                v198 = [v5, v5];
                 }
               }
            let v206;
            const v208 = stdlib.le(0, v124);
            const v209 = stdlib.lt(v124, 5);
            const v211 = v208 ? v209 : false;
            stdlib.assert(v211);
            const v212 = stdlib.eq(v124, 0);
            if (v212) {
              v206 = 'Bob wins';
               }
            else {
              const v213 = stdlib.eq(v124, 1);
              if (v213) {
                v206 = 'Draw';
                 }
              else {
                const v214 = stdlib.eq(v124, 2);
                if (v214) {
                  v206 = 'Alice wins';
                   }
                else {
                  const v215 = stdlib.eq(v124, 3);
                  if (v215) {
                    v206 = 'Alice quits';
                     }
                  else {
                    v206 = 'Bob quits';
                     }
                   }
                 }
               }
            return v206 }
          else {
            let v198;
            const v199 = stdlib.eq(v124, 2);
            if (v199) {
              const v200 = stdlib.mul(2, v5);
              v198 = [v200, 0];
               }
            else {
              const v201 = stdlib.eq(v124, 0);
              if (v201) {
                const v202 = stdlib.mul(2, v5);
                v198 = [0, v202];
                 }
              else {
                v198 = [v5, v5];
                 }
               }
            let v206;
            const v208 = stdlib.le(0, v124);
            const v209 = stdlib.lt(v124, 5);
            const v211 = v208 ? v209 : false;
            stdlib.assert(v211);
            const v212 = stdlib.eq(v124, 0);
            if (v212) {
              v206 = 'Bob wins';
               }
            else {
              const v213 = stdlib.eq(v124, 1);
              if (v213) {
                v206 = 'Draw';
                 }
              else {
                const v214 = stdlib.eq(v124, 2);
                if (v214) {
                  v206 = 'Alice wins';
                   }
                else {
                  const v215 = stdlib.eq(v124, 3);
                  if (v215) {
                    v206 = 'Alice quits';
                     }
                  else {
                    v206 = 'Bob quits';
                     }
                   }
                 }
               }
            return v206 } } } } } }

export const ETH = {
  ABI: `[{"inputs":[],"payable":true,"stateMutability":"payable","type":"constructor"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v5","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v6","type":"uint256"}],"name":"e1","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e2","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v48","type":"uint256"}],"name":"e3","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v77","type":"uint256"}],"name":"e4","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v103","type":"uint256"},{"indexed":false,"internalType":"uint256","name":"v104","type":"uint256"}],"name":"e5","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e6","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e7","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e8","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"_bal","type":"uint256"}],"name":"e9","type":"event"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"}],"name":"m1","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v7","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"}],"name":"m2","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v7","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"},{"internalType":"address payable","name":"v15","type":"address"},{"internalType":"uint256","name":"v48","type":"uint256"}],"name":"m3","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v7","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"},{"internalType":"address payable","name":"v15","type":"address"},{"internalType":"uint256","name":"v48","type":"uint256"},{"internalType":"uint256","name":"v77","type":"uint256"}],"name":"m4","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v7","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"},{"internalType":"address payable","name":"v15","type":"address"},{"internalType":"uint256","name":"v48","type":"uint256"},{"internalType":"uint256","name":"v77","type":"uint256"},{"internalType":"uint256","name":"v103","type":"uint256"},{"internalType":"uint256","name":"v104","type":"uint256"}],"name":"m5","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v7","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"},{"internalType":"address payable","name":"v15","type":"address"},{"internalType":"uint256","name":"v48","type":"uint256"},{"internalType":"uint256","name":"v77","type":"uint256"}],"name":"m6","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v7","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"},{"internalType":"address payable","name":"v15","type":"address"},{"internalType":"uint256","name":"v48","type":"uint256"}],"name":"m7","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v7","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"},{"internalType":"address payable","name":"v15","type":"address"}],"name":"m8","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"internalType":"uint256","name":"_last","type":"uint256"},{"internalType":"address payable","name":"v7","type":"address"},{"internalType":"uint256","name":"v5","type":"uint256"},{"internalType":"uint256","name":"v6","type":"uint256"}],"name":"m9","outputs":[],"payable":true,"stateMutability":"payable","type":"function"}]`,
  Bytecode: `0x600060a08181524360c0526040608081905260e0815290209055610daa806100286000396000f3fe6080604052600436106100865760003560e01c806373929c5e1161005957806373929c5e146101ab5780637a52ccb3146102075780637de71f08146102305780639ccddd3a14610268578063bb91d6e3146102a057610086565b8063050147391461008b578063103d2bab146100cd5780635e6a8eed1461011b578063718b7dd714610163575b600080fd5b6100cb600480360360a08110156100a157600080fd5b508035906001600160a01b03602082013581169160408101359160608201359160800135166102ee565b005b6100cb600480360360e08110156100e357600080fd5b508035906001600160a01b03602082013581169160408101359160608201359160808101359091169060a08101359060c001356103ce565b6100cb600480360360c081101561013157600080fd5b508035906001600160a01b03602082013581169160408101359160608201359160808101359091169060a001356104b6565b6100cb600480360360c081101561017957600080fd5b508035906001600160a01b03602082013581169160408101359160608201359160808101359091169060a001356105ec565b6100cb60048036036101208110156101c257600080fd5b508035906001600160a01b03602082013581169160408101359160608201359160808101359091169060a08101359060c08101359060e08101359061010001356106cc565b6100cb6004803603606081101561021d57600080fd5b508035906020810135906040013561097f565b6100cb6004803603608081101561024657600080fd5b508035906001600160a01b036020820135169060408101359060600135610a51565b6100cb6004803603608081101561027e57600080fd5b508035906001600160a01b036020820135169060408101359060600135610ad4565b6100cb600480360360e08110156102b657600080fd5b508035906001600160a01b03602082013581169160408101359160608201359160808101359091169060a08101359060c00135610bd9565b6040805160026020808301919091528183018890526001600160601b0319606088811b821681850152607484018890526094840187905285901b1660b4830152825160a881840301815260c890920190925280519101206000541461035257600080fd5b336001600160a01b0382161461036757600080fd5b600a8501431015801561037c5750600a850143105b61038557600080fd5b341561039057600080fd5b6040516001600160a01b03821690303180156108fc02916000818181858888f193505050501580156103c6573d6000803e3d6000fd5b506000805533ff5b6040805160046020808301919091528183018a90526001600160601b031960608a811b821681850152607484018a90526094840189905287901b1660b483015260c8820185905260e88083018590528351808403909101815261010890920190925280519101206000541461044257600080fd5b336001600160a01b0384161461045757600080fd5b600a8701431015801561046c5750601e870143105b61047557600080fd5b341561048057600080fd5b6040516001600160a01b03841690303180156108fc02916000818181858888f193505050501580156103c6573d6000803e3d6000fd5b6040805160026020808301919091528183018990526001600160601b0319606089811b821681850152607484018990526094840188905286901b1660b4830152825160a881840301815260c890920190925280519101206000541461051a57600080fd5b336001600160a01b0386161461052f57600080fd5b60148601431061053e57600080fd5b341561054957600080fd5b60408051600360208083019190915243828401526001600160601b0319606089811b821681850152607484018990526094840188905286901b1660b483015260c88083018590528351808403909101815260e883018085528151919092012060005530319052610108810183905290517f94dd7e08991b8945fde2d5865f7071e72045b9800e293ff60d29c6960c5a4fb5918190036101280190a1505050505050565b6040805160036020808301919091528183018990526001600160601b0319606089811b821681850152607484018990526094840188905286901b1660b483015260c88083018590528351808403909101815260e890920190925280519101206000541461065857600080fd5b336001600160a01b0386161461066d57600080fd5b600a8601431015801561068257506014860143105b61068b57600080fd5b341561069657600080fd5b6040516001600160a01b03861690303180156108fc02916000818181858888f193505050501580156103c6573d6000803e3d6000fd5b6040805160046020808301919091528183018c90526001600160601b031960608c811b821681850152607484018c9052609484018b905289901b1660b483015260c8820187905260e88083018790528351808403909101815261010890920190925280519101206000541461074057600080fd5b610748610d2d565b336001600160a01b038a161461075d57600080fd5b60288a01431061076c57600080fd5b341561077757600080fd5b6040805160208082018690528183018590528251808303840181526060909201909252805191012085146107aa57600080fd5b600382106107b757600080fd5b60038083106040830181905290851060608301526107d65760006107dc565b80606001515b156107f8576003846004038301816107f057fe5b068152610823565b80604001511561080b5760028152610823565b80606001511561081e5760008152610823565b600181525b80516002141561094f576040516001600160a01b038a16906108fc9060009081818181818888f19350505050158015610860573d6000803e3d6000fd5b50805160021415610891576040518060400160405280896002028152602001600081525081602001819052506108d6565b80516108bd576040518060400160405280600081526020018960020281525081602001819052506108d6565b6040805180820190915288815260208082018a90528201525b6020810151516040516001600160a01b038b1691890180156108fc02916000818181858888f19350505050158015610912573d6000803e3d6000fd5b5060208082015101516040516001600160a01b0388169180156108fc02916000818181858888f193505050501580156103c6573d6000803e3d6000fd5b805160021415610891576040518060400160405280896002028152602001600081525081602001819052506108d6565b60408051600060208083018290528284018790528351808403850181526060909301909352815191909201209054146109b757600080fd5b34818301146109c557600080fd5b604080516001602080830191909152438284015233606090811b908301526074820185905260948083018590528351808403909101815260b48301808552815191909201206000553031905260d4810184905260f4810183905290517f219cc811755104876269c7553666684eaaeecb90b6a7ffc6fdd5068140059b8e918190036101140190a1505050565b604080516001602080830191909152818301879052606086811b6001600160601b031916908301526074820185905260948083018590528351808403909101815260b4909201909252805191012060005414610aac57600080fd5b336001600160a01b03841614610ac157600080fd5b600a8401431015801561046c5750610475565b604080516001602080830191909152818301879052606086811b6001600160601b031916908301526074820185905260948083018590528351808403909101815260b4909201909252805191012060005414610b2f57600080fd5b600a84014310610b3e57600080fd5b348214610b4a57600080fd5b6040805160026020808301919091524382840152606086811b6001600160601b03191681840152607483018690526094830185905233901b60b4830152825180830360a801815260c88301808552815191909201206000553031905290517ff04f5fc87a72102f7c0b228f8bbaf9b9aa7a2b5dc295c86538fdde91e95866e99160e8908290030190a150505050565b6040805160036020808301919091528183018a90526001600160601b031960608a811b821681850152607484018a90526094840189905287901b1660b483015260c88083018690528351808403909101815260e8909201909252805191012060005414610c4557600080fd5b336001600160a01b03841614610c5a57600080fd5b601e87014310610c6957600080fd5b3415610c7457600080fd5b60038110610c8157600080fd5b60408051600460208083019190915243828401526001600160601b031960608a811b821681850152607484018a90526094840189905287901b1660b483015260c8820185905260e88083018590528351808403909101815261010883018085528151919092012060005530319052610128810183905290517fb71d350b59ceca5c6544e5367d61ca8cae3e36b25f8d900743d063dff3d6508b918190036101480190a150505050505050565b604051806080016040528060008152602001610d47610d5b565b815260006020820181905260409091015290565b60405180604001604052806000815260200160008152509056fea265627a7a72315820203ade8df0be83d4bb2c4f30e5e34cb0de7d722fade6230e6fddd2aeac99d98864736f6c634300050c0032` };