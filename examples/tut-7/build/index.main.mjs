// Automatically generated with Reach 0.1.2
export const _version = '0.1.2';

export async function Alice(stdlib, ctc, interact) {
  
  
  const txn1 = await ctc.sendrecv('Alice', 1, 1, [stdlib.T_UInt256], [stdlib.protect(stdlib.T_UInt256, interact.wager, null)], stdlib.protect(stdlib.T_UInt256, interact.wager, null), [stdlib.T_UInt256], false, ((txn1) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.digest(stdlib.bigNumberify(0));
    const [v27] = txn1.data;
    const v29 = txn1.value;
    const v28 = txn1.from;
    
    const v30 = stdlib.eq(v29, v27);
    stdlib.assert(v30, {
      at: './index.rsh:45:20:after expr stmt semicolon',
      fs: [],
      msg: 'pay amount correct',
      who: 'Alice' });
    const v31 = stdlib.bigNumberify(0);
    const v32 = stdlib.add(v31, v29);
    sim_r.nextSt = stdlib.digest(stdlib.bigNumberify(1), v32, v28, v27);
    sim_r.isHalt = false;
    return sim_r; }));
  const [v27] = txn1.data;
  const v29 = txn1.value;
  const v28 = txn1.from;
  const v30 = stdlib.eq(v29, v27);
  stdlib.assert(v30, {
    at: './index.rsh:45:20:after expr stmt semicolon',
    fs: [],
    msg: 'pay amount correct',
    who: 'Alice' });
  const v31 = stdlib.bigNumberify(0);
  const v32 = stdlib.add(v31, v29);
  const txn2 = await ctc.recv('Alice', 2, 0, [], stdlib.bigNumberify(10));
  if (txn2.didTimeout) {
    
    const txn3 = await ctc.sendrecv('Alice', 10, 0, [stdlib.T_UInt256, stdlib.T_Address, stdlib.T_UInt256], [v32, v28, v27], stdlib.bigNumberify(0), [], false, ((txn3) => {
      const sim_r = { txns: [] };
      sim_r.prevSt = stdlib.digest(stdlib.bigNumberify(1), v32, v28, v27);
      const [] = txn3.data;
      const v39 = txn3.value;
      
      const v40 = stdlib.eq(v39, stdlib.bigNumberify(0));
      stdlib.assert(v40, {
        at: 'reach standard library:78:16:after expr stmt semicolon',
        fs: ['at ./index.rsh:51:41:application call to "closeTo (as function)" (defined at: reach standard library:77:8:function exp)'],
        msg: 'pay amount correct',
        who: 'Alice' });
      const v41 = v32;
      const v42 = stdlib.add(v41, v39);
      const v43 = v42;
      sim_r.txns.push({
        amt: v43,
        to: v28 });
      sim_r.nextSt = stdlib.digest();
      sim_r.isHalt = true;
      return sim_r; }));
    const [] = txn3.data;
    const v39 = txn3.value;
    const v40 = stdlib.eq(v39, stdlib.bigNumberify(0));
    stdlib.assert(v40, {
      at: 'reach standard library:78:16:after expr stmt semicolon',
      fs: ['at ./index.rsh:51:41:application call to "closeTo (as function)" (defined at: reach standard library:77:8:function exp)'],
      msg: 'pay amount correct',
      who: 'Alice' });
    const v41 = v32;
    const v42 = stdlib.add(v41, v39);
    const v43 = v42;
    ;
    stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
      at: './index.rsh:40:33:application',
      fs: ['at ./index.rsh:40:39:after expr stmt semicolon call to "function" (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:81:8:application call to "informTimeout (as function)" (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:51:41:application call to "closeTo (as function)" (defined at: reach standard library:77:8:function exp)'],
      msg: 'informTimeout',
      who: 'Alice' });
    
    return; }
  else {
    const [] = txn2.data;
    const v36 = txn2.value;
    const v35 = txn2.from;
    const v37 = stdlib.eq(v36, v27);
    stdlib.assert(v37, {
      at: './index.rsh:51:60:after expr stmt semicolon',
      fs: [],
      msg: 'pay amount correct',
      who: 'Alice' });
    const v55 = v32;
    const v56 = stdlib.add(v55, v36);
    const v57 = v56;
    let v58 = v57;
    let v59 = stdlib.bigNumberify(1);
    while ((() => {
      const v70 = stdlib.eq(v59, stdlib.bigNumberify(1));
      
      return v70; })()) {
      const v72 = stdlib.protect(stdlib.T_UInt256, await interact.getHand(), {
        at: './index.rsh:59:42:application',
        fs: ['at ./index.rsh:61:51:after expr stmt semicolon call to "function" (defined at: ./index.rsh:58:19:function exp)'],
        msg: 'getHand',
        who: 'Alice' });
      const v74 = stdlib.protect(stdlib.T_UInt256, await interact.random(), {
        at: 'reach standard library:70:31:application',
        fs: ['at ./index.rsh:60:52:application call to "makeCommitment (as function)" (defined at: reach standard library:69:8:function exp)', 'at ./index.rsh:61:51:after expr stmt semicolon call to "function" (defined at: ./index.rsh:58:19:function exp)'],
        msg: 'random',
        who: 'Alice' });
      const v75 = stdlib.digest(v74, v72);
      
      
      const txn3 = await ctc.sendrecv('Alice', 4, 1, [stdlib.T_Address, stdlib.T_UInt256, stdlib.T_Address, stdlib.T_UInt256, stdlib.T_Digest], [v28, v27, v35, v58, v75], stdlib.bigNumberify(0), [stdlib.T_Digest], stdlib.bigNumberify(10), ((txn3) => {
        const sim_r = { txns: [] };
        sim_r.prevSt = stdlib.digest(stdlib.bigNumberify(3), v28, v27, v35, v58);
        const [v76] = txn3.data;
        const v77 = txn3.value;
        
        const v78 = stdlib.eq(v77, stdlib.bigNumberify(0));
        stdlib.assert(v78, {
          at: './index.rsh:63:62:after expr stmt semicolon',
          fs: [],
          msg: 'pay amount correct',
          who: 'Alice' });
        const v96 = v58;
        const v97 = stdlib.add(v96, v77);
        sim_r.nextSt = stdlib.digest(stdlib.bigNumberify(4), v97, v28, v27, v35, v76);
        sim_r.isHalt = false;
        return sim_r; }));
      if (txn3.didTimeout) {
        const txn4 = await ctc.recv('Alice', 9, 0, [], false);
        const [] = txn4.data;
        const v80 = txn4.value;
        const v81 = stdlib.eq(v80, stdlib.bigNumberify(0));
        stdlib.assert(v81, {
          at: 'reach standard library:78:16:after expr stmt semicolon',
          fs: ['at ./index.rsh:63:43:application call to "closeTo (as function)" (defined at: reach standard library:77:8:function exp)'],
          msg: 'pay amount correct',
          who: 'Alice' });
        const v82 = v58;
        const v83 = stdlib.add(v82, v80);
        const v84 = v83;
        ;
        stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
          at: './index.rsh:40:33:application',
          fs: ['at ./index.rsh:40:39:after expr stmt semicolon call to "function" (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:81:8:application call to "informTimeout (as function)" (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:63:43:application call to "closeTo (as function)" (defined at: reach standard library:77:8:function exp)'],
          msg: 'informTimeout',
          who: 'Alice' });
        
        return; }
      else {
        const [v76] = txn3.data;
        const v77 = txn3.value;
        const v78 = stdlib.eq(v77, stdlib.bigNumberify(0));
        stdlib.assert(v78, {
          at: './index.rsh:63:62:after expr stmt semicolon',
          fs: [],
          msg: 'pay amount correct',
          who: 'Alice' });
        const v96 = v58;
        const v97 = stdlib.add(v96, v77);
        const txn4 = await ctc.recv('Alice', 5, 1, [stdlib.T_UInt256], stdlib.bigNumberify(10));
        if (txn4.didTimeout) {
          
          const txn5 = await ctc.sendrecv('Alice', 8, 0, [stdlib.T_UInt256, stdlib.T_Address, stdlib.T_UInt256, stdlib.T_Address, stdlib.T_Digest], [v97, v28, v27, v35, v76], stdlib.bigNumberify(0), [], false, ((txn5) => {
            const sim_r = { txns: [] };
            sim_r.prevSt = stdlib.digest(stdlib.bigNumberify(4), v97, v28, v27, v35, v76);
            const [] = txn5.data;
            const v104 = txn5.value;
            
            const v105 = stdlib.eq(v104, stdlib.bigNumberify(0));
            stdlib.assert(v105, {
              at: 'reach standard library:78:16:after expr stmt semicolon',
              fs: ['at ./index.rsh:70:43:application call to "closeTo (as function)" (defined at: reach standard library:77:8:function exp)'],
              msg: 'pay amount correct',
              who: 'Alice' });
            const v106 = v97;
            const v107 = stdlib.add(v106, v104);
            const v108 = v107;
            sim_r.txns.push({
              amt: v108,
              to: v28 });
            sim_r.nextSt = stdlib.digest();
            sim_r.isHalt = true;
            return sim_r; }));
          const [] = txn5.data;
          const v104 = txn5.value;
          const v105 = stdlib.eq(v104, stdlib.bigNumberify(0));
          stdlib.assert(v105, {
            at: 'reach standard library:78:16:after expr stmt semicolon',
            fs: ['at ./index.rsh:70:43:application call to "closeTo (as function)" (defined at: reach standard library:77:8:function exp)'],
            msg: 'pay amount correct',
            who: 'Alice' });
          const v106 = v97;
          const v107 = stdlib.add(v106, v104);
          const v108 = v107;
          ;
          stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
            at: './index.rsh:40:33:application',
            fs: ['at ./index.rsh:40:39:after expr stmt semicolon call to "function" (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:81:8:application call to "informTimeout (as function)" (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:70:43:application call to "closeTo (as function)" (defined at: reach standard library:77:8:function exp)'],
            msg: 'informTimeout',
            who: 'Alice' });
          
          return; }
        else {
          const [v100] = txn4.data;
          const v101 = txn4.value;
          const v102 = stdlib.eq(v101, stdlib.bigNumberify(0));
          stdlib.assert(v102, {
            at: './index.rsh:70:62:after expr stmt semicolon',
            fs: [],
            msg: 'pay amount correct',
            who: 'Alice' });
          const v120 = v97;
          const v121 = stdlib.add(v120, v101);
          
          
          const txn5 = await ctc.sendrecv('Alice', 6, 2, [stdlib.T_UInt256, stdlib.T_Address, stdlib.T_UInt256, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt256, stdlib.T_UInt256, stdlib.T_UInt256], [v121, v28, v27, v35, v76, v100, v74, v72], stdlib.bigNumberify(0), [stdlib.T_UInt256, stdlib.T_UInt256], stdlib.bigNumberify(10), ((txn5) => {
            const sim_r = { txns: [] };
            sim_r.prevSt = stdlib.digest(stdlib.bigNumberify(5), v121, v28, v27, v35, v76, v100);
            const [v123, v124] = txn5.data;
            const v125 = txn5.value;
            
            const v126 = stdlib.eq(v125, stdlib.bigNumberify(0));
            stdlib.assert(v126, {
              at: './index.rsh:76:62:after expr stmt semicolon',
              fs: [],
              msg: 'pay amount correct',
              who: 'Alice' });
            const v144 = v121;
            const v145 = stdlib.add(v144, v125);
            const v147 = stdlib.digest(v123, v124);
            const v149 = stdlib.digestEq(v76, v147);
            stdlib.assert(v149, {
              at: 'reach standard library:75:17:application',
              fs: ['at ./index.rsh:77:24:application call to "checkCommitment (as function)" (defined at: reach standard library:74:8:function exp)'],
              msg: null,
              who: 'Alice' });
            const v151 = stdlib.sub(stdlib.bigNumberify(4), v100);
            const v152 = stdlib.add(v124, v151);
            const v153 = stdlib.mod(v152, stdlib.bigNumberify(3));
            const v154 = v145;
            
            return sim_r; }));
          if (txn5.didTimeout) {
            const txn6 = await ctc.recv('Alice', 7, 0, [], false);
            const [] = txn6.data;
            const v128 = txn6.value;
            const v129 = stdlib.eq(v128, stdlib.bigNumberify(0));
            stdlib.assert(v129, {
              at: 'reach standard library:78:16:after expr stmt semicolon',
              fs: ['at ./index.rsh:76:43:application call to "closeTo (as function)" (defined at: reach standard library:77:8:function exp)'],
              msg: 'pay amount correct',
              who: 'Alice' });
            const v130 = v121;
            const v131 = stdlib.add(v130, v128);
            const v132 = v131;
            ;
            stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
              at: './index.rsh:40:33:application',
              fs: ['at ./index.rsh:40:39:after expr stmt semicolon call to "function" (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:81:8:application call to "informTimeout (as function)" (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:76:43:application call to "closeTo (as function)" (defined at: reach standard library:77:8:function exp)'],
              msg: 'informTimeout',
              who: 'Alice' });
            
            return; }
          else {
            const [v123, v124] = txn5.data;
            const v125 = txn5.value;
            const v126 = stdlib.eq(v125, stdlib.bigNumberify(0));
            stdlib.assert(v126, {
              at: './index.rsh:76:62:after expr stmt semicolon',
              fs: [],
              msg: 'pay amount correct',
              who: 'Alice' });
            const v144 = v121;
            const v145 = stdlib.add(v144, v125);
            const v147 = stdlib.digest(v123, v124);
            const v149 = stdlib.digestEq(v76, v147);
            stdlib.assert(v149, {
              at: 'reach standard library:75:17:application',
              fs: ['at ./index.rsh:77:24:application call to "checkCommitment (as function)" (defined at: reach standard library:74:8:function exp)'],
              msg: null,
              who: 'Alice' });
            const v151 = stdlib.sub(stdlib.bigNumberify(4), v100);
            const v152 = stdlib.add(v124, v151);
            const v153 = stdlib.mod(v152, stdlib.bigNumberify(3));
            const v154 = v145;
            v58 = v154;
            v59 = v153;
            continue; } } } }
    const v160 = stdlib.mul(stdlib.bigNumberify(2), v27);
    const v162 = stdlib.eq(v59, stdlib.bigNumberify(2));
    const v163 = v162 ? v28 : v35;
    ;
    stdlib.protect(stdlib.T_Null, await interact.seeOutcome(v59), {
      at: './index.rsh:87:28:application',
      fs: ['at ./index.rsh:87:41:after expr stmt semicolon call to "function" (defined at: ./index.rsh:86:23:function exp)'],
      msg: 'seeOutcome',
      who: 'Alice' });
    
    return; } }
export async function Bob(stdlib, ctc, interact) {
  const txn1 = await ctc.recv('Bob', 1, 1, [stdlib.T_UInt256], false);
  const [v27] = txn1.data;
  const v29 = txn1.value;
  const v28 = txn1.from;
  const v30 = stdlib.eq(v29, v27);
  stdlib.assert(v30, {
    at: './index.rsh:45:20:after expr stmt semicolon',
    fs: [],
    msg: 'pay amount correct',
    who: 'Bob' });
  const v31 = stdlib.bigNumberify(0);
  const v32 = stdlib.add(v31, v29);
  stdlib.protect(stdlib.T_Null, await interact.acceptWager(v27), {
    at: './index.rsh:49:29:application',
    fs: ['at ./index.rsh:49:40:after expr stmt semicolon call to "function" (defined at: ./index.rsh:48:17:function exp)'],
    msg: 'acceptWager',
    who: 'Bob' });
  
  
  const txn2 = await ctc.sendrecv('Bob', 2, 0, [stdlib.T_UInt256, stdlib.T_Address, stdlib.T_UInt256], [v32, v28, v27], v27, [], stdlib.bigNumberify(10), ((txn2) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.digest(stdlib.bigNumberify(1), v32, v28, v27);
    const [] = txn2.data;
    const v36 = txn2.value;
    const v35 = txn2.from;
    
    const v37 = stdlib.eq(v36, v27);
    stdlib.assert(v37, {
      at: './index.rsh:51:60:after expr stmt semicolon',
      fs: [],
      msg: 'pay amount correct',
      who: 'Bob' });
    const v55 = v32;
    const v56 = stdlib.add(v55, v36);
    const v57 = v56;
    
    return sim_r; }));
  if (txn2.didTimeout) {
    const txn3 = await ctc.recv('Bob', 10, 0, [], false);
    const [] = txn3.data;
    const v39 = txn3.value;
    const v40 = stdlib.eq(v39, stdlib.bigNumberify(0));
    stdlib.assert(v40, {
      at: 'reach standard library:78:16:after expr stmt semicolon',
      fs: ['at ./index.rsh:51:41:application call to "closeTo (as function)" (defined at: reach standard library:77:8:function exp)'],
      msg: 'pay amount correct',
      who: 'Bob' });
    const v41 = v32;
    const v42 = stdlib.add(v41, v39);
    const v43 = v42;
    ;
    stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
      at: './index.rsh:40:33:application',
      fs: ['at ./index.rsh:40:39:after expr stmt semicolon call to "function" (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:81:8:application call to "informTimeout (as function)" (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:51:41:application call to "closeTo (as function)" (defined at: reach standard library:77:8:function exp)'],
      msg: 'informTimeout',
      who: 'Bob' });
    
    return; }
  else {
    const [] = txn2.data;
    const v36 = txn2.value;
    const v35 = txn2.from;
    const v37 = stdlib.eq(v36, v27);
    stdlib.assert(v37, {
      at: './index.rsh:51:60:after expr stmt semicolon',
      fs: [],
      msg: 'pay amount correct',
      who: 'Bob' });
    const v55 = v32;
    const v56 = stdlib.add(v55, v36);
    const v57 = v56;
    let v58 = v57;
    let v59 = stdlib.bigNumberify(1);
    while ((() => {
      const v70 = stdlib.eq(v59, stdlib.bigNumberify(1));
      
      return v70; })()) {
      const txn3 = await ctc.recv('Bob', 4, 1, [stdlib.T_Digest], stdlib.bigNumberify(10));
      if (txn3.didTimeout) {
        
        const txn4 = await ctc.sendrecv('Bob', 9, 0, [stdlib.T_Address, stdlib.T_UInt256, stdlib.T_Address, stdlib.T_UInt256], [v28, v27, v35, v58], stdlib.bigNumberify(0), [], false, ((txn4) => {
          const sim_r = { txns: [] };
          sim_r.prevSt = stdlib.digest(stdlib.bigNumberify(3), v28, v27, v35, v58);
          const [] = txn4.data;
          const v80 = txn4.value;
          
          const v81 = stdlib.eq(v80, stdlib.bigNumberify(0));
          stdlib.assert(v81, {
            at: 'reach standard library:78:16:after expr stmt semicolon',
            fs: ['at ./index.rsh:63:43:application call to "closeTo (as function)" (defined at: reach standard library:77:8:function exp)'],
            msg: 'pay amount correct',
            who: 'Bob' });
          const v82 = v58;
          const v83 = stdlib.add(v82, v80);
          const v84 = v83;
          sim_r.txns.push({
            amt: v84,
            to: v35 });
          sim_r.nextSt = stdlib.digest();
          sim_r.isHalt = true;
          return sim_r; }));
        const [] = txn4.data;
        const v80 = txn4.value;
        const v81 = stdlib.eq(v80, stdlib.bigNumberify(0));
        stdlib.assert(v81, {
          at: 'reach standard library:78:16:after expr stmt semicolon',
          fs: ['at ./index.rsh:63:43:application call to "closeTo (as function)" (defined at: reach standard library:77:8:function exp)'],
          msg: 'pay amount correct',
          who: 'Bob' });
        const v82 = v58;
        const v83 = stdlib.add(v82, v80);
        const v84 = v83;
        ;
        stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
          at: './index.rsh:40:33:application',
          fs: ['at ./index.rsh:40:39:after expr stmt semicolon call to "function" (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:81:8:application call to "informTimeout (as function)" (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:63:43:application call to "closeTo (as function)" (defined at: reach standard library:77:8:function exp)'],
          msg: 'informTimeout',
          who: 'Bob' });
        
        return; }
      else {
        const [v76] = txn3.data;
        const v77 = txn3.value;
        const v78 = stdlib.eq(v77, stdlib.bigNumberify(0));
        stdlib.assert(v78, {
          at: './index.rsh:63:62:after expr stmt semicolon',
          fs: [],
          msg: 'pay amount correct',
          who: 'Bob' });
        const v96 = v58;
        const v97 = stdlib.add(v96, v77);
        const v99 = stdlib.protect(stdlib.T_UInt256, await interact.getHand(), {
          at: './index.rsh:68:52:application',
          fs: ['at ./index.rsh:68:59:after expr stmt semicolon call to "function" (defined at: ./index.rsh:67:19:function exp)'],
          msg: 'getHand',
          who: 'Bob' });
        
        
        const txn4 = await ctc.sendrecv('Bob', 5, 1, [stdlib.T_UInt256, stdlib.T_Address, stdlib.T_UInt256, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt256], [v97, v28, v27, v35, v76, v99], stdlib.bigNumberify(0), [stdlib.T_UInt256], stdlib.bigNumberify(10), ((txn4) => {
          const sim_r = { txns: [] };
          sim_r.prevSt = stdlib.digest(stdlib.bigNumberify(4), v97, v28, v27, v35, v76);
          const [v100] = txn4.data;
          const v101 = txn4.value;
          
          const v102 = stdlib.eq(v101, stdlib.bigNumberify(0));
          stdlib.assert(v102, {
            at: './index.rsh:70:62:after expr stmt semicolon',
            fs: [],
            msg: 'pay amount correct',
            who: 'Bob' });
          const v120 = v97;
          const v121 = stdlib.add(v120, v101);
          sim_r.nextSt = stdlib.digest(stdlib.bigNumberify(5), v121, v28, v27, v35, v76, v100);
          sim_r.isHalt = false;
          return sim_r; }));
        if (txn4.didTimeout) {
          const txn5 = await ctc.recv('Bob', 8, 0, [], false);
          const [] = txn5.data;
          const v104 = txn5.value;
          const v105 = stdlib.eq(v104, stdlib.bigNumberify(0));
          stdlib.assert(v105, {
            at: 'reach standard library:78:16:after expr stmt semicolon',
            fs: ['at ./index.rsh:70:43:application call to "closeTo (as function)" (defined at: reach standard library:77:8:function exp)'],
            msg: 'pay amount correct',
            who: 'Bob' });
          const v106 = v97;
          const v107 = stdlib.add(v106, v104);
          const v108 = v107;
          ;
          stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
            at: './index.rsh:40:33:application',
            fs: ['at ./index.rsh:40:39:after expr stmt semicolon call to "function" (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:81:8:application call to "informTimeout (as function)" (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:70:43:application call to "closeTo (as function)" (defined at: reach standard library:77:8:function exp)'],
            msg: 'informTimeout',
            who: 'Bob' });
          
          return; }
        else {
          const [v100] = txn4.data;
          const v101 = txn4.value;
          const v102 = stdlib.eq(v101, stdlib.bigNumberify(0));
          stdlib.assert(v102, {
            at: './index.rsh:70:62:after expr stmt semicolon',
            fs: [],
            msg: 'pay amount correct',
            who: 'Bob' });
          const v120 = v97;
          const v121 = stdlib.add(v120, v101);
          const txn5 = await ctc.recv('Bob', 6, 2, [stdlib.T_UInt256, stdlib.T_UInt256], stdlib.bigNumberify(10));
          if (txn5.didTimeout) {
            
            const txn6 = await ctc.sendrecv('Bob', 7, 0, [stdlib.T_UInt256, stdlib.T_Address, stdlib.T_UInt256, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt256], [v121, v28, v27, v35, v76, v100], stdlib.bigNumberify(0), [], false, ((txn6) => {
              const sim_r = { txns: [] };
              sim_r.prevSt = stdlib.digest(stdlib.bigNumberify(5), v121, v28, v27, v35, v76, v100);
              const [] = txn6.data;
              const v128 = txn6.value;
              
              const v129 = stdlib.eq(v128, stdlib.bigNumberify(0));
              stdlib.assert(v129, {
                at: 'reach standard library:78:16:after expr stmt semicolon',
                fs: ['at ./index.rsh:76:43:application call to "closeTo (as function)" (defined at: reach standard library:77:8:function exp)'],
                msg: 'pay amount correct',
                who: 'Bob' });
              const v130 = v121;
              const v131 = stdlib.add(v130, v128);
              const v132 = v131;
              sim_r.txns.push({
                amt: v132,
                to: v35 });
              sim_r.nextSt = stdlib.digest();
              sim_r.isHalt = true;
              return sim_r; }));
            const [] = txn6.data;
            const v128 = txn6.value;
            const v129 = stdlib.eq(v128, stdlib.bigNumberify(0));
            stdlib.assert(v129, {
              at: 'reach standard library:78:16:after expr stmt semicolon',
              fs: ['at ./index.rsh:76:43:application call to "closeTo (as function)" (defined at: reach standard library:77:8:function exp)'],
              msg: 'pay amount correct',
              who: 'Bob' });
            const v130 = v121;
            const v131 = stdlib.add(v130, v128);
            const v132 = v131;
            ;
            stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
              at: './index.rsh:40:33:application',
              fs: ['at ./index.rsh:40:39:after expr stmt semicolon call to "function" (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:81:8:application call to "informTimeout (as function)" (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:76:43:application call to "closeTo (as function)" (defined at: reach standard library:77:8:function exp)'],
              msg: 'informTimeout',
              who: 'Bob' });
            
            return; }
          else {
            const [v123, v124] = txn5.data;
            const v125 = txn5.value;
            const v126 = stdlib.eq(v125, stdlib.bigNumberify(0));
            stdlib.assert(v126, {
              at: './index.rsh:76:62:after expr stmt semicolon',
              fs: [],
              msg: 'pay amount correct',
              who: 'Bob' });
            const v144 = v121;
            const v145 = stdlib.add(v144, v125);
            const v147 = stdlib.digest(v123, v124);
            const v149 = stdlib.digestEq(v76, v147);
            stdlib.assert(v149, {
              at: 'reach standard library:75:17:application',
              fs: ['at ./index.rsh:77:24:application call to "checkCommitment (as function)" (defined at: reach standard library:74:8:function exp)'],
              msg: null,
              who: 'Bob' });
            const v151 = stdlib.sub(stdlib.bigNumberify(4), v100);
            const v152 = stdlib.add(v124, v151);
            const v153 = stdlib.mod(v152, stdlib.bigNumberify(3));
            const v154 = v145;
            v58 = v154;
            v59 = v153;
            continue; } } } }
    const v160 = stdlib.mul(stdlib.bigNumberify(2), v27);
    const v162 = stdlib.eq(v59, stdlib.bigNumberify(2));
    const v163 = v162 ? v28 : v35;
    ;
    stdlib.protect(stdlib.T_Null, await interact.seeOutcome(v59), {
      at: './index.rsh:87:28:application',
      fs: ['at ./index.rsh:87:41:after expr stmt semicolon call to "function" (defined at: ./index.rsh:86:23:function exp)'],
      msg: 'seeOutcome',
      who: 'Bob' });
    
    return; } }

const _ALGO = {
  appApproval: `#pragma version 2
  // Check that we're an App
  txn TypeEnum
  int appl
  ==
  bz revert
  txn RekeyTo
  global ZeroAddress
  ==
  bz revert
  // Check that everyone's here
  global GroupSize
  int 4
  >=
  bz revert
  // Check txnAppl (us)
  txn GroupIndex
  int 0
  ==
  bz revert
  // Check txnFromHandler
  int 0
  gtxn 2 Sender
  byte "{{m1}}"
  ==
  ||
  gtxn 2 Sender
  byte "{{m2}}"
  ==
  ||
  gtxn 2 Sender
  byte "{{m4}}"
  ==
  ||
  gtxn 2 Sender
  byte "{{m5}}"
  ==
  ||
  gtxn 2 Sender
  byte "{{m6}}"
  ==
  ||
  gtxn 2 Sender
  byte "{{m7}}"
  ==
  ||
  gtxn 2 Sender
  byte "{{m8}}"
  ==
  ||
  gtxn 2 Sender
  byte "{{m9}}"
  ==
  ||
  gtxn 2 Sender
  byte "{{m10}}"
  ==
  ||
  bz revert
  byte base64(cw==)
  app_global_get
  gtxna 2 Args 0
  ==
  bz revert
  byte base64(bA==)
  app_global_get
  gtxna 2 Args 4
  btoi
  ==
  bz revert
  // Don't check anyone else, because Handler does
  // Update state
  byte base64(cw==)
  gtxna 2 Args 1
  app_global_put
  byte base64(bA==)
  global Round
  app_global_put
  byte base64(aA==)
  gtxna 2 Args 2
  btoi
  app_global_put
  byte base64(aA==)
  app_global_get
  bnz halted
  txn OnCompletion
  int NoOp
  ==
  bz revert
  b done
  halted:
  txn OnCompletion
  int DeleteApplication
  ==
  bz revert
  b done
  revert:
  int 0
  return
  done:
  int 1
  return
  `,
  appApproval0: `#pragma version 2
  // Check that we're an App
  txn TypeEnum
  int appl
  ==
  bz revert
  txn RekeyTo
  global ZeroAddress
  ==
  bz revert
  txn Sender
  byte "{{Deployer}}"
  ==
  bz revert
  txn ApplicationID
  bz init
  global GroupSize
  int 11
  ==
  bz revert
  txn OnCompletion
  int UpdateApplication
  ==
  bz revert
  byte base64(cw==)
  int 0
  itob
  keccak256
  app_global_put
  byte base64(bA==)
  global Round
  app_global_put
  byte base64(aA==)
  int 0
  app_global_put
  b done
  init:
  global GroupSize
  int 1
  ==
  bz revert
  txn OnCompletion
  int NoOp
  ==
  bz revert
  b done
  revert:
  int 0
  return
  done:
  int 1
  return
  `,
  appClear: `#pragma version 2
  // We're alone
  global GroupSize
  int 1
  ==
  bz revert
  // We're halted
  byte base64(aA==)
  app_global_get
  int 1
  ==
  bz revert
  b done
  revert:
  int 0
  return
  done:
  int 1
  return
  `,
  ctc: `#pragma version 2
  // Check size
  global GroupSize
  int 4
  >=
  bz revert
  // Check txnAppl
  gtxn 0 TypeEnum
  int appl
  ==
  bz revert
  gtxn 0 ApplicationID
  byte "{{ApplicationID}}"
  btoi
  ==
  bz revert
  // Don't check anything else, because app does
  // Check us
  txn TypeEnum
  int pay
  ==
  bz revert
  txn RekeyTo
  global ZeroAddress
  ==
  bz revert
  txn CloseRemainderTo
  global ZeroAddress
  ==
  bz revert
  txn GroupIndex
  int 4
  >=
  bz revert
  b done
  revert:
  int 0
  return
  done:
  int 1
  return
  `,
  steps: [null, `#pragma version 2
  // Check txnAppl
  gtxn 0 TypeEnum
  int appl
  ==
  bz revert
  gtxn 0 ApplicationID
  byte "{{ApplicationID}}"
  btoi
  ==
  bz revert
  // Check txnToHandler
  gtxn 1 TypeEnum
  int pay
  ==
  bz revert
  gtxn 1 Receiver
  txn Sender
  ==
  bz revert
  gtxn 1 Amount
  gtxn 2 Fee
  ==
  bz revert
  // Check txnToContract
  gtxn 3 TypeEnum
  int pay
  ==
  bz revert
  gtxn 3 Receiver
  byte "{{ContractAddr}}"
  ==
  bz revert
  // Check txnFromHandler (us)
  txn GroupIndex
  int 2
  ==
  bz revert
  txn TypeEnum
  int pay
  ==
  bz revert
  txn Amount
  int 0
  ==
  bz revert
  txn Receiver
  gtxn 1 Sender
  ==
  bz revert
  txn NumArgs
  int 6
  ==
  bz revert
  int 0
  itob
  keccak256
  arg 0
  ==
  bz revert
  // Run body
  gtxn 3 Amount
  arg 3
  btoi
  -
  arg 5
  btoi
  ==
  bz revert
  int 0
  gtxn 3 Amount
  arg 3
  btoi
  -
  +
  store 255
  int 1
  itob
  load 255
  itob
  concat
  gtxn 3 Sender
  concat
  arg 5
  concat
  keccak256
  arg 1
  ==
  bz revert
  arg 2
  btoi
  int 0
  ==
  bz revert
  b done
  // Check GroupSize
  global GroupSize
  int 4
  ==
  bz revert
  arg 3
  btoi
  int 0
  ==
  bz revert
  // Check time limits
  revert:
  int 0
  return
  done:
  int 1
  return
  `, `#pragma version 2
  // Check txnAppl
  gtxn 0 TypeEnum
  int appl
  ==
  bz revert
  gtxn 0 ApplicationID
  byte "{{ApplicationID}}"
  btoi
  ==
  bz revert
  // Check txnToHandler
  gtxn 1 TypeEnum
  int pay
  ==
  bz revert
  gtxn 1 Receiver
  txn Sender
  ==
  bz revert
  gtxn 1 Amount
  gtxn 2 Fee
  ==
  bz revert
  // Check txnToContract
  gtxn 3 TypeEnum
  int pay
  ==
  bz revert
  gtxn 3 Receiver
  byte "{{ContractAddr}}"
  ==
  bz revert
  // Check txnFromHandler (us)
  txn GroupIndex
  int 2
  ==
  bz revert
  txn TypeEnum
  int pay
  ==
  bz revert
  txn Amount
  int 0
  ==
  bz revert
  txn Receiver
  gtxn 1 Sender
  ==
  bz revert
  txn NumArgs
  int 8
  ==
  bz revert
  int 1
  itob
  arg 5
  concat
  arg 6
  concat
  arg 7
  concat
  keccak256
  arg 0
  ==
  bz revert
  // Run body
  gtxn 3 Amount
  arg 3
  btoi
  -
  arg 7
  btoi
  ==
  bz revert
  // XXX jump
  // Check GroupSize
  global GroupSize
  int 4
  ==
  bz revert
  arg 3
  btoi
  int 0
  ==
  bz revert
  // Check time limits
  arg 4
  btoi
  int 10
  +
  dup
  gtxn 0 LastValid
  ==
  bz revert
  dup
  gtxn 1 LastValid
  ==
  bz revert
  dup
  gtxn 2 LastValid
  ==
  bz revert
  dup
  gtxn 3 LastValid
  ==
  bz revert
  pop
  revert:
  int 0
  return
  done:
  int 1
  return
  `, null, `#pragma version 2
  // Check txnAppl
  gtxn 0 TypeEnum
  int appl
  ==
  bz revert
  gtxn 0 ApplicationID
  byte "{{ApplicationID}}"
  btoi
  ==
  bz revert
  // Check txnToHandler
  gtxn 1 TypeEnum
  int pay
  ==
  bz revert
  gtxn 1 Receiver
  txn Sender
  ==
  bz revert
  gtxn 1 Amount
  gtxn 2 Fee
  ==
  bz revert
  // Check txnToContract
  gtxn 3 TypeEnum
  int pay
  ==
  bz revert
  gtxn 3 Receiver
  byte "{{ContractAddr}}"
  ==
  bz revert
  // Check txnFromHandler (us)
  txn GroupIndex
  int 2
  ==
  bz revert
  txn TypeEnum
  int pay
  ==
  bz revert
  txn Amount
  int 0
  ==
  bz revert
  txn Receiver
  gtxn 1 Sender
  ==
  bz revert
  txn NumArgs
  int 10
  ==
  bz revert
  gtxn 3 Sender
  arg 5
  ==
  bz revert
  int 3
  itob
  arg 5
  concat
  arg 6
  concat
  arg 7
  concat
  arg 8
  concat
  keccak256
  arg 0
  ==
  bz revert
  // Run body
  gtxn 3 Amount
  arg 3
  btoi
  -
  int 0
  ==
  bz revert
  arg 8
  btoi
  gtxn 3 Amount
  arg 3
  btoi
  -
  +
  store 255
  int 4
  itob
  load 255
  itob
  concat
  arg 5
  concat
  arg 6
  concat
  arg 7
  concat
  arg 9
  concat
  keccak256
  arg 1
  ==
  bz revert
  arg 2
  btoi
  int 0
  ==
  bz revert
  b done
  // Check GroupSize
  global GroupSize
  int 4
  ==
  bz revert
  arg 3
  btoi
  int 0
  ==
  bz revert
  // Check time limits
  arg 4
  btoi
  int 10
  +
  dup
  gtxn 0 LastValid
  ==
  bz revert
  dup
  gtxn 1 LastValid
  ==
  bz revert
  dup
  gtxn 2 LastValid
  ==
  bz revert
  dup
  gtxn 3 LastValid
  ==
  bz revert
  pop
  revert:
  int 0
  return
  done:
  int 1
  return
  `, `#pragma version 2
  // Check txnAppl
  gtxn 0 TypeEnum
  int appl
  ==
  bz revert
  gtxn 0 ApplicationID
  byte "{{ApplicationID}}"
  btoi
  ==
  bz revert
  // Check txnToHandler
  gtxn 1 TypeEnum
  int pay
  ==
  bz revert
  gtxn 1 Receiver
  txn Sender
  ==
  bz revert
  gtxn 1 Amount
  gtxn 2 Fee
  ==
  bz revert
  // Check txnToContract
  gtxn 3 TypeEnum
  int pay
  ==
  bz revert
  gtxn 3 Receiver
  byte "{{ContractAddr}}"
  ==
  bz revert
  // Check txnFromHandler (us)
  txn GroupIndex
  int 2
  ==
  bz revert
  txn TypeEnum
  int pay
  ==
  bz revert
  txn Amount
  int 0
  ==
  bz revert
  txn Receiver
  gtxn 1 Sender
  ==
  bz revert
  txn NumArgs
  int 11
  ==
  bz revert
  gtxn 3 Sender
  arg 8
  ==
  bz revert
  int 4
  itob
  arg 5
  concat
  arg 6
  concat
  arg 7
  concat
  arg 8
  concat
  arg 9
  concat
  keccak256
  arg 0
  ==
  bz revert
  // Run body
  gtxn 3 Amount
  arg 3
  btoi
  -
  int 0
  ==
  bz revert
  arg 5
  btoi
  gtxn 3 Amount
  arg 3
  btoi
  -
  +
  store 255
  int 5
  itob
  load 255
  itob
  concat
  arg 6
  concat
  arg 7
  concat
  arg 8
  concat
  arg 9
  concat
  arg 10
  concat
  keccak256
  arg 1
  ==
  bz revert
  arg 2
  btoi
  int 0
  ==
  bz revert
  b done
  // Check GroupSize
  global GroupSize
  int 4
  ==
  bz revert
  arg 3
  btoi
  int 0
  ==
  bz revert
  // Check time limits
  arg 4
  btoi
  int 10
  +
  dup
  gtxn 0 LastValid
  ==
  bz revert
  dup
  gtxn 1 LastValid
  ==
  bz revert
  dup
  gtxn 2 LastValid
  ==
  bz revert
  dup
  gtxn 3 LastValid
  ==
  bz revert
  pop
  revert:
  int 0
  return
  done:
  int 1
  return
  `, `#pragma version 2
  // Check txnAppl
  gtxn 0 TypeEnum
  int appl
  ==
  bz revert
  gtxn 0 ApplicationID
  byte "{{ApplicationID}}"
  btoi
  ==
  bz revert
  // Check txnToHandler
  gtxn 1 TypeEnum
  int pay
  ==
  bz revert
  gtxn 1 Receiver
  txn Sender
  ==
  bz revert
  gtxn 1 Amount
  gtxn 2 Fee
  ==
  bz revert
  // Check txnToContract
  gtxn 3 TypeEnum
  int pay
  ==
  bz revert
  gtxn 3 Receiver
  byte "{{ContractAddr}}"
  ==
  bz revert
  // Check txnFromHandler (us)
  txn GroupIndex
  int 2
  ==
  bz revert
  txn TypeEnum
  int pay
  ==
  bz revert
  txn Amount
  int 0
  ==
  bz revert
  txn Receiver
  gtxn 1 Sender
  ==
  bz revert
  txn NumArgs
  int 13
  ==
  bz revert
  gtxn 3 Sender
  arg 6
  ==
  bz revert
  int 5
  itob
  arg 5
  concat
  arg 6
  concat
  arg 7
  concat
  arg 8
  concat
  arg 9
  concat
  arg 10
  concat
  keccak256
  arg 0
  ==
  bz revert
  // Run body
  gtxn 3 Amount
  arg 3
  btoi
  -
  int 0
  ==
  bz revert
  arg 9
  arg 11
  arg 12
  concat
  keccak256
  ==
  bz revert
  // XXX jump
  // Check GroupSize
  global GroupSize
  int 4
  ==
  bz revert
  arg 3
  btoi
  int 0
  ==
  bz revert
  // Check time limits
  arg 4
  btoi
  int 10
  +
  dup
  gtxn 0 LastValid
  ==
  bz revert
  dup
  gtxn 1 LastValid
  ==
  bz revert
  dup
  gtxn 2 LastValid
  ==
  bz revert
  dup
  gtxn 3 LastValid
  ==
  bz revert
  pop
  revert:
  int 0
  return
  done:
  int 1
  return
  `, `#pragma version 2
  // Check txnAppl
  gtxn 0 TypeEnum
  int appl
  ==
  bz revert
  gtxn 0 ApplicationID
  byte "{{ApplicationID}}"
  btoi
  ==
  bz revert
  // Check txnToHandler
  gtxn 1 TypeEnum
  int pay
  ==
  bz revert
  gtxn 1 Receiver
  txn Sender
  ==
  bz revert
  gtxn 1 Amount
  gtxn 2 Fee
  ==
  bz revert
  // Check txnToContract
  gtxn 3 TypeEnum
  int pay
  ==
  bz revert
  gtxn 3 Receiver
  byte "{{ContractAddr}}"
  ==
  bz revert
  // Check txnFromHandler (us)
  txn GroupIndex
  int 2
  ==
  bz revert
  txn TypeEnum
  int pay
  ==
  bz revert
  txn Amount
  int 0
  ==
  bz revert
  txn Receiver
  gtxn 1 Sender
  ==
  bz revert
  txn NumArgs
  int 11
  ==
  bz revert
  gtxn 3 Sender
  arg 8
  ==
  bz revert
  int 5
  itob
  arg 5
  concat
  arg 6
  concat
  arg 7
  concat
  arg 8
  concat
  arg 9
  concat
  arg 10
  concat
  keccak256
  arg 0
  ==
  bz revert
  // Run body
  gtxn 3 Amount
  arg 3
  btoi
  -
  int 0
  ==
  bz revert
  gtxn 4 TypeEnum
  int pay
  ==
  bz revert
  gtxn 4 Receiver
  arg 8
  ==
  bz revert
  gtxn 4 Amount
  arg 5
  btoi
  gtxn 3 Amount
  arg 3
  btoi
  -
  +
  ==
  bz revert
  gtxn 4 Sender
  byte "{{ContractAddr}}"
  ==
  bz revert
  arg 2
  btoi
  int 1
  ==
  bz revert
  b done
  // Check GroupSize
  global GroupSize
  int 5
  ==
  bz revert
  arg 3
  btoi
  gtxn 4 Fee
  ==
  bz revert
  // Check time limits
  arg 4
  btoi
  int 10
  +
  dup
  gtxn 0 FirstValid
  ==
  bz revert
  dup
  gtxn 1 FirstValid
  ==
  bz revert
  dup
  gtxn 2 FirstValid
  ==
  bz revert
  dup
  gtxn 3 FirstValid
  ==
  bz revert
  dup
  gtxn 4 FirstValid
  ==
  bz revert
  pop
  revert:
  int 0
  return
  done:
  int 1
  return
  `, `#pragma version 2
  // Check txnAppl
  gtxn 0 TypeEnum
  int appl
  ==
  bz revert
  gtxn 0 ApplicationID
  byte "{{ApplicationID}}"
  btoi
  ==
  bz revert
  // Check txnToHandler
  gtxn 1 TypeEnum
  int pay
  ==
  bz revert
  gtxn 1 Receiver
  txn Sender
  ==
  bz revert
  gtxn 1 Amount
  gtxn 2 Fee
  ==
  bz revert
  // Check txnToContract
  gtxn 3 TypeEnum
  int pay
  ==
  bz revert
  gtxn 3 Receiver
  byte "{{ContractAddr}}"
  ==
  bz revert
  // Check txnFromHandler (us)
  txn GroupIndex
  int 2
  ==
  bz revert
  txn TypeEnum
  int pay
  ==
  bz revert
  txn Amount
  int 0
  ==
  bz revert
  txn Receiver
  gtxn 1 Sender
  ==
  bz revert
  txn NumArgs
  int 10
  ==
  bz revert
  gtxn 3 Sender
  arg 6
  ==
  bz revert
  int 4
  itob
  arg 5
  concat
  arg 6
  concat
  arg 7
  concat
  arg 8
  concat
  arg 9
  concat
  keccak256
  arg 0
  ==
  bz revert
  // Run body
  gtxn 3 Amount
  arg 3
  btoi
  -
  int 0
  ==
  bz revert
  gtxn 4 TypeEnum
  int pay
  ==
  bz revert
  gtxn 4 Receiver
  arg 6
  ==
  bz revert
  gtxn 4 Amount
  arg 5
  btoi
  gtxn 3 Amount
  arg 3
  btoi
  -
  +
  ==
  bz revert
  gtxn 4 Sender
  byte "{{ContractAddr}}"
  ==
  bz revert
  arg 2
  btoi
  int 1
  ==
  bz revert
  b done
  // Check GroupSize
  global GroupSize
  int 5
  ==
  bz revert
  arg 3
  btoi
  gtxn 4 Fee
  ==
  bz revert
  // Check time limits
  arg 4
  btoi
  int 10
  +
  dup
  gtxn 0 FirstValid
  ==
  bz revert
  dup
  gtxn 1 FirstValid
  ==
  bz revert
  dup
  gtxn 2 FirstValid
  ==
  bz revert
  dup
  gtxn 3 FirstValid
  ==
  bz revert
  dup
  gtxn 4 FirstValid
  ==
  bz revert
  pop
  revert:
  int 0
  return
  done:
  int 1
  return
  `, `#pragma version 2
  // Check txnAppl
  gtxn 0 TypeEnum
  int appl
  ==
  bz revert
  gtxn 0 ApplicationID
  byte "{{ApplicationID}}"
  btoi
  ==
  bz revert
  // Check txnToHandler
  gtxn 1 TypeEnum
  int pay
  ==
  bz revert
  gtxn 1 Receiver
  txn Sender
  ==
  bz revert
  gtxn 1 Amount
  gtxn 2 Fee
  ==
  bz revert
  // Check txnToContract
  gtxn 3 TypeEnum
  int pay
  ==
  bz revert
  gtxn 3 Receiver
  byte "{{ContractAddr}}"
  ==
  bz revert
  // Check txnFromHandler (us)
  txn GroupIndex
  int 2
  ==
  bz revert
  txn TypeEnum
  int pay
  ==
  bz revert
  txn Amount
  int 0
  ==
  bz revert
  txn Receiver
  gtxn 1 Sender
  ==
  bz revert
  txn NumArgs
  int 9
  ==
  bz revert
  gtxn 3 Sender
  arg 7
  ==
  bz revert
  int 3
  itob
  arg 5
  concat
  arg 6
  concat
  arg 7
  concat
  arg 8
  concat
  keccak256
  arg 0
  ==
  bz revert
  // Run body
  gtxn 3 Amount
  arg 3
  btoi
  -
  int 0
  ==
  bz revert
  gtxn 4 TypeEnum
  int pay
  ==
  bz revert
  gtxn 4 Receiver
  arg 7
  ==
  bz revert
  gtxn 4 Amount
  arg 8
  btoi
  gtxn 3 Amount
  arg 3
  btoi
  -
  +
  ==
  bz revert
  gtxn 4 Sender
  byte "{{ContractAddr}}"
  ==
  bz revert
  arg 2
  btoi
  int 1
  ==
  bz revert
  b done
  // Check GroupSize
  global GroupSize
  int 5
  ==
  bz revert
  arg 3
  btoi
  gtxn 4 Fee
  ==
  bz revert
  // Check time limits
  arg 4
  btoi
  int 10
  +
  dup
  gtxn 0 FirstValid
  ==
  bz revert
  dup
  gtxn 1 FirstValid
  ==
  bz revert
  dup
  gtxn 2 FirstValid
  ==
  bz revert
  dup
  gtxn 3 FirstValid
  ==
  bz revert
  dup
  gtxn 4 FirstValid
  ==
  bz revert
  pop
  revert:
  int 0
  return
  done:
  int 1
  return
  `, `#pragma version 2
  // Check txnAppl
  gtxn 0 TypeEnum
  int appl
  ==
  bz revert
  gtxn 0 ApplicationID
  byte "{{ApplicationID}}"
  btoi
  ==
  bz revert
  // Check txnToHandler
  gtxn 1 TypeEnum
  int pay
  ==
  bz revert
  gtxn 1 Receiver
  txn Sender
  ==
  bz revert
  gtxn 1 Amount
  gtxn 2 Fee
  ==
  bz revert
  // Check txnToContract
  gtxn 3 TypeEnum
  int pay
  ==
  bz revert
  gtxn 3 Receiver
  byte "{{ContractAddr}}"
  ==
  bz revert
  // Check txnFromHandler (us)
  txn GroupIndex
  int 2
  ==
  bz revert
  txn TypeEnum
  int pay
  ==
  bz revert
  txn Amount
  int 0
  ==
  bz revert
  txn Receiver
  gtxn 1 Sender
  ==
  bz revert
  txn NumArgs
  int 8
  ==
  bz revert
  gtxn 3 Sender
  arg 6
  ==
  bz revert
  int 1
  itob
  arg 5
  concat
  arg 6
  concat
  arg 7
  concat
  keccak256
  arg 0
  ==
  bz revert
  // Run body
  gtxn 3 Amount
  arg 3
  btoi
  -
  int 0
  ==
  bz revert
  gtxn 4 TypeEnum
  int pay
  ==
  bz revert
  gtxn 4 Receiver
  arg 6
  ==
  bz revert
  gtxn 4 Amount
  arg 5
  btoi
  gtxn 3 Amount
  arg 3
  btoi
  -
  +
  ==
  bz revert
  gtxn 4 Sender
  byte "{{ContractAddr}}"
  ==
  bz revert
  arg 2
  btoi
  int 1
  ==
  bz revert
  b done
  // Check GroupSize
  global GroupSize
  int 5
  ==
  bz revert
  arg 3
  btoi
  gtxn 4 Fee
  ==
  bz revert
  // Check time limits
  arg 4
  btoi
  int 10
  +
  dup
  gtxn 0 FirstValid
  ==
  bz revert
  dup
  gtxn 1 FirstValid
  ==
  bz revert
  dup
  gtxn 2 FirstValid
  ==
  bz revert
  dup
  gtxn 3 FirstValid
  ==
  bz revert
  dup
  gtxn 4 FirstValid
  ==
  bz revert
  pop
  revert:
  int 0
  return
  done:
  int 1
  return
  `],
  unsupported: true };
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
          "name": "v27",
          "type": "uint256"
        }
      ],
      "name": "e1",
      "type": "event"
    },
    {
      "anonymous": false,
      "inputs": [],
      "name": "e10",
      "type": "event"
    },
    {
      "anonymous": false,
      "inputs": [],
      "name": "e2",
      "type": "event"
    },
    {
      "anonymous": false,
      "inputs": [
        {
          "indexed": false,
          "internalType": "uint256",
          "name": "v76",
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
          "name": "v100",
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
          "name": "v123",
          "type": "uint256"
        },
        {
          "indexed": false,
          "internalType": "uint256",
          "name": "v124",
          "type": "uint256"
        }
      ],
      "name": "e6",
      "type": "event"
    },
    {
      "anonymous": false,
      "inputs": [],
      "name": "e7",
      "type": "event"
    },
    {
      "anonymous": false,
      "inputs": [],
      "name": "e8",
      "type": "event"
    },
    {
      "anonymous": false,
      "inputs": [],
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
              "name": "v27",
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
              "internalType": "uint256",
              "name": "v32",
              "type": "uint256"
            },
            {
              "internalType": "address payable",
              "name": "v28",
              "type": "address"
            },
            {
              "internalType": "uint256",
              "name": "v27",
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
              "internalType": "uint256",
              "name": "v32",
              "type": "uint256"
            },
            {
              "internalType": "address payable",
              "name": "v28",
              "type": "address"
            },
            {
              "internalType": "uint256",
              "name": "v27",
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
              "name": "v28",
              "type": "address"
            },
            {
              "internalType": "uint256",
              "name": "v27",
              "type": "uint256"
            },
            {
              "internalType": "address payable",
              "name": "v35",
              "type": "address"
            },
            {
              "internalType": "uint256",
              "name": "v58",
              "type": "uint256"
            },
            {
              "internalType": "uint256",
              "name": "v76",
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
              "internalType": "uint256",
              "name": "v97",
              "type": "uint256"
            },
            {
              "internalType": "address payable",
              "name": "v28",
              "type": "address"
            },
            {
              "internalType": "uint256",
              "name": "v27",
              "type": "uint256"
            },
            {
              "internalType": "address payable",
              "name": "v35",
              "type": "address"
            },
            {
              "internalType": "uint256",
              "name": "v76",
              "type": "uint256"
            },
            {
              "internalType": "uint256",
              "name": "v100",
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
              "internalType": "uint256",
              "name": "v121",
              "type": "uint256"
            },
            {
              "internalType": "address payable",
              "name": "v28",
              "type": "address"
            },
            {
              "internalType": "uint256",
              "name": "v27",
              "type": "uint256"
            },
            {
              "internalType": "address payable",
              "name": "v35",
              "type": "address"
            },
            {
              "internalType": "uint256",
              "name": "v76",
              "type": "uint256"
            },
            {
              "internalType": "uint256",
              "name": "v100",
              "type": "uint256"
            },
            {
              "internalType": "uint256",
              "name": "v123",
              "type": "uint256"
            },
            {
              "internalType": "uint256",
              "name": "v124",
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
              "internalType": "uint256",
              "name": "v121",
              "type": "uint256"
            },
            {
              "internalType": "address payable",
              "name": "v28",
              "type": "address"
            },
            {
              "internalType": "uint256",
              "name": "v27",
              "type": "uint256"
            },
            {
              "internalType": "address payable",
              "name": "v35",
              "type": "address"
            },
            {
              "internalType": "uint256",
              "name": "v76",
              "type": "uint256"
            },
            {
              "internalType": "uint256",
              "name": "v100",
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
              "internalType": "uint256",
              "name": "v97",
              "type": "uint256"
            },
            {
              "internalType": "address payable",
              "name": "v28",
              "type": "address"
            },
            {
              "internalType": "uint256",
              "name": "v27",
              "type": "uint256"
            },
            {
              "internalType": "address payable",
              "name": "v35",
              "type": "address"
            },
            {
              "internalType": "uint256",
              "name": "v76",
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
              "name": "v28",
              "type": "address"
            },
            {
              "internalType": "uint256",
              "name": "v27",
              "type": "uint256"
            },
            {
              "internalType": "address payable",
              "name": "v35",
              "type": "address"
            },
            {
              "internalType": "uint256",
              "name": "v58",
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
  Bytecode: `0x608060405261001160004360a0610031565b60408051601f19818403018152919052805160209091012060005561003f565b918252602082015260400190565b610f328061004e6000396000f3fe6080604052600436106100865760003560e01c80636c75e587116100595780636c75e587146100d957806371570cb5146100ec57806378088fec146100ff578063ebcdfe5814610112578063f156095d1461012557610086565b806303fcf1691461008b5780631a9f6f72146100a05780632745f356146100b35780635602050f146100c6575b600080fd5b61009e610099366004610da0565b610138565b005b61009e6100ae366004610dcc565b610207565b61009e6100c1366004610df9565b61034e565b61009e6100d4366004610db1565b610489565b61009e6100e7366004610de7565b6105ce565b61009e6100fa366004610db1565b610797565b61009e61010d366004610d85565b6108d8565b61009e610120366004610dcc565b6109d7565b61009e610133366004610d85565b610b28565b60405161014d90600090833590602001610e13565b6040516020818303038152906040528051906020012060001c6000541461017357600080fd5b61017b610d0b565b8160200135341461018b57600080fd5b3481526040517f3680e78b6fdf571695c81f108d81181ea63f50c100e6375e765b14bd7ac0adbb906101c290602085013590610e0a565b60405180910390a180516040516101e89160019143919033906020808901359101610e55565b60408051601f1981840301815291905280516020909101206000555050565b6005813560208301356102206060850160408601610d57565b606085013561023560a0870160808801610d57565b8660a001358760c00135604051602001610256989796959493929190610eba565b6040516020818303038152906040528051906020012060001c6000541461027c57600080fd5b61028c60a0820160808301610d57565b6001600160a01b0316336001600160a01b0316146102a957600080fd5b8035600a0143108015906102bb575060015b6102c457600080fd5b34156102cf57600080fd5b6102df60a0820160808301610d57565b6001600160a01b03166108fc348360200135019081150290604051600060405180830381858888f1935050505015801561031d573d6000803e3d6000fd5b506040517f865db884b03ae248a5b1e887fed68b9b0d97adb2d5660a93e9b1d6da53f070b990600090a16000805533ff5b600381356103626040840160208501610d57565b60408401356103776080860160608701610d57565b856080013560405160200161039196959493929190610e21565b6040516020818303038152906040528051906020012060001c600054146103b757600080fd5b6103c76080820160608301610d57565b6001600160a01b0316336001600160a01b0316146103e457600080fd5b8035600a0143108015906103f6575060015b6103ff57600080fd5b341561040a57600080fd5b61041a6080820160608301610d57565b6001600160a01b03166108fc348360800135019081150290604051600060405180830381858888f19350505050158015610458573d6000803e3d6000fd5b506040517f5ba509c4dbb2c82e8f38fec2166bedd2c18c53fb19ce6aa5ab42111e51e0744990600090a16000805533ff5b6003813561049d6040840160208501610d57565b60408401356104b26080860160608701610d57565b85608001356040516020016104cc96959493929190610e21565b6040516020818303038152906040528051906020012060001c600054146104f257600080fd5b6104fa610d0b565b61050a6040830160208401610d57565b6001600160a01b0316336001600160a01b03161461052757600080fd5b8135600a01431061053757600080fd5b341561054257600080fd5b6080820135340181526040517f6ce5b12953112c528c3a24a99350a573e6cb61c4e39d70f2392cc6bd7266f9699061057f9060a085013590610e0a565b60405180910390a18051600490439061059e6040860160208701610d57565b60408601356105b36080880160608901610d57565b8760a001356040516020016101e89796959493929190610e81565b6005813560208301356105e76060850160408601610d57565b60608501356105fc60a0870160808801610d57565b8660a001358760c0013560405160200161061d989796959493929190610eba565b6040516020818303038152906040528051906020012060001c6000541461064357600080fd5b6106536060820160408301610d57565b6001600160a01b0316336001600160a01b03161461067057600080fd5b8035600a01431061068057600080fd5b341561068b57600080fd5b8060e001358161010001356040516020016106a7929190610e13565b6040516020818303038152906040528051906020012060001c8160a00135146106cf57600080fd5b7f1fa1ad895cc7ba9133068b14fd5b3d9ed6f96d3a535ff2be342493855f237b6b8160e00135826101000135604051610709929190610e13565b60405180910390a16107946040518060a001604052808360400160208101906107329190610d57565b6001600160a01b031681526060840135602082015260400161075a60a0850160808601610d57565b6001600160a01b0316815260200134846020013501815260200160038460c00135600403856101000135018161078c57fe5b069052610c52565b50565b6004813560208301356107b06060850160408601610d57565b60608501356107c560a0870160808801610d57565b8660a001356040516020016107e09796959493929190610e81565b6040516020818303038152906040528051906020012060001c6000541461080657600080fd5b6108166060820160408301610d57565b6001600160a01b0316336001600160a01b03161461083357600080fd5b8035600a014310801590610845575060015b61084e57600080fd5b341561085957600080fd5b6108696060820160408301610d57565b6001600160a01b03166108fc348360200135019081150290604051600060405180830381858888f193505050501580156108a7573d6000803e3d6000fd5b506040517fffa3d43ab9b6e5b34273fd049718a85553427384eed63754abc672936df0584e90600090a16000805533ff5b6001813560208301356108f16060850160408601610d57565b846060013560405160200161090a959493929190610e55565b6040516020818303038152906040528051906020012060001c6000541461093057600080fd5b8035600a01431061094057600080fd5b8060600135341461095057600080fd5b6040517f9b31f9e88fd11f71bfbf93b0237bc9a0900b8479a307f60435e40543e383403590600090a16107946040518060a0016040528083604001602081019061099a9190610d57565b6001600160a01b0316815260200183606001358152602001336001600160a01b031681526020013484602001350181526020016001815250610c52565b6004813560208301356109f06060850160408601610d57565b6060850135610a0560a0870160808801610d57565b8660a00135604051602001610a209796959493929190610e81565b6040516020818303038152906040528051906020012060001c60005414610a4657600080fd5b610a4e610d0b565b610a5e60a0830160808401610d57565b6001600160a01b0316336001600160a01b031614610a7b57600080fd5b8135600a014310610a8b57600080fd5b3415610a9657600080fd5b6020820135340181526040517f26bdc6b0a0806ec5cb3992c1b74dd2db228f99da5f09181980c9114c97ebf40790610ad39060c085013590610e0a565b60405180910390a180516005904390610af26060860160408701610d57565b6060860135610b0760a0880160808901610d57565b8760a001358860c001356040516020016101e8989796959493929190610eba565b600181356020830135610b416060850160408601610d57565b8460600135604051602001610b5a959493929190610e55565b6040516020818303038152906040528051906020012060001c60005414610b8057600080fd5b610b906060820160408301610d57565b6001600160a01b0316336001600160a01b031614610bad57600080fd5b8035600a014310801590610bbf575060015b610bc857600080fd5b3415610bd357600080fd5b610be36060820160408301610d57565b6001600160a01b03166108fc348360200135019081150290604051600060405180830381858888f19350505050158015610c21573d6000803e3d6000fd5b506040517f5ef1d939728ae307281ba62215efdccdf0b99fa9cc412f247b7ab97b4729b74f90600090a16000805533ff5b600181608001511415610cab576003438260000151836020015184604001518560600151604051602001610c8b96959493929190610e21565b60408051601f198184030181529190528051602090910120600055610794565b6002816080015114610cc1578060400151610cc4565b80515b6001600160a01b03166108fc82602001516002029081150290604051600060405180830381858888f19350505050158015610d03573d6000803e3d6000fd5b506000805533ff5b6040518060200160405280600081525090565b600060808284031215610d2f578081fd5b50919050565b600060c08284031215610d2f578081fd5b600060e08284031215610d2f578081fd5b600060208284031215610d68578081fd5b81356001600160a01b0381168114610d7e578182fd5b9392505050565b600060808284031215610d96578081fd5b610d7e8383610d1e565b600060408284031215610d2f578081fd5b600060c08284031215610dc2578081fd5b610d7e8383610d35565b600060e08284031215610ddd578081fd5b610d7e8383610d46565b60006101208284031215610d2f578081fd5b600060a08284031215610d2f578081fd5b90815260200190565b918252602082015260400190565b95865260208601949094526001600160a01b039283166040860152606085019190915216608083015260a082015260c00190565b948552602085019390935260408401919091526001600160a01b03166060830152608082015260a00190565b968752602087019590955260408601939093526001600160a01b03918216606086015260808501521660a083015260c082015260e00190565b978852602088019690965260408701949094526001600160a01b03928316606087015260808601919091521660a084015260c083015260e0820152610100019056fea2646970667358221220a2bbb9acbf7a305b8e45d8af6fe3dff0e459a2839c6d922d0b93bb6020be98cb64736f6c63430007010033`,
  deployMode: `DM_constructor` };

export const _Connectors = {
  ALGO: _ALGO,
  ETH: _ETH };
