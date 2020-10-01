// Automatically generated with Reach 0.1.2
export const _version = '0.1.2';

export async function Alice(stdlib, ctc, interact) {
  const txn0 = { balance: 0, value: 0 };
  
  
  const txn1 = await ctc.sendrecv('Alice', stdlib.bigNumberify(1), stdlib.bigNumberify(1), [stdlib.T_UInt256], [stdlib.protect(stdlib.T_UInt256, interact.wager, null)], stdlib.protect(stdlib.T_UInt256, interact.wager, null), [stdlib.T_UInt256], false, ((txn1) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.keccak256();
    const [v27] = txn1.data;
    const v28 = txn1.from;
    
    const v29 = txn1.value;
    const v31 = stdlib.eq(v27, v29);
    stdlib.assert(v31, {
      at: './index.rsh:application',
      fs: [],
      who: 'Alice' });
    sim_r.nextSt = stdlib.keccak256(v28, v27);
    sim_r.isHalt = false;
    return sim_r; }));
  const [v27] = txn1.data;
  const v28 = txn1.from;
  const v29 = txn1.value;
  const v31 = stdlib.eq(v27, v29);
  stdlib.assert(v31, {
    at: './index.rsh:application',
    fs: [],
    who: 'Alice' });
  const txn2 = await ctc.recv('Alice', stdlib.bigNumberify(2), stdlib.bigNumberify(0), [], stdlib.bigNumberify(10));
  if (txn2.didTimeout) {
    
    const txn3 = await ctc.sendrecv('Alice', stdlib.bigNumberify(10), stdlib.bigNumberify(0), [stdlib.T_Address, stdlib.T_UInt256], [v28, v27], stdlib.bigNumberify(0), [], false, ((txn3) => {
      const sim_r = { txns: [] };
      sim_r.prevSt = stdlib.keccak256(v28, v27);
      const [] = txn3.data;
      
      const v39 = txn3.value;
      const v41 = stdlib.eq(stdlib.bigNumberify(0), v39);
      stdlib.assert(v41, {
        at: 'reach standard library:application',
        fs: ['at ./index.rsh:51:41:application call to "closeTo (as function)" (defined at: reach standard library:73:8:function exp)'],
        who: 'Alice' });
      const v42 = txn3.balance;
      sim_r.txns.push({
        amt: v42,
        to: v28 });
      sim_r.nextSt = '';
      sim_r.isHalt = true;
      return sim_r; }));
    const [] = txn3.data;
    const v39 = txn3.value;
    const v41 = stdlib.eq(stdlib.bigNumberify(0), v39);
    stdlib.assert(v41, {
      at: 'reach standard library:application',
      fs: ['at ./index.rsh:51:41:application call to "closeTo (as function)" (defined at: reach standard library:73:8:function exp)'],
      who: 'Alice' });
    const v42 = txn3.balance;
    ;
    stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
      at: './index.rsh:40:33:application',
      fs: ['at ./index.rsh:40:39:after expr stmt semicolon call to "function" (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:77:8:application call to "informTimeout (as function)" (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:51:41:application call to "closeTo (as function)" (defined at: reach standard library:73:8:function exp)'],
      who: 'Alice' });
    
    return; }
  else {
    const [] = txn2.data;
    const v34 = txn2.from;
    const v35 = txn2.value;
    const v37 = stdlib.eq(v27, v35);
    stdlib.assert(v37, {
      at: './index.rsh:application',
      fs: [],
      who: 'Alice' });
    let v48 = stdlib.bigNumberify(1);
    while ((() => {
      const v59 = stdlib.eq(v48, stdlib.bigNumberify(1));
      
      return v59; })()) {
      const v61 = stdlib.protect(stdlib.T_UInt256, await interact.getHand(), {
        at: './index.rsh:59:42:application',
        fs: ['at ./index.rsh:61:51:after expr stmt semicolon call to "function" (defined at: ./index.rsh:58:19:function exp)'],
        who: 'Alice' });
      const v63 = stdlib.protect(stdlib.T_UInt256, await interact.random(), {
        at: 'reach standard library:66:31:application',
        fs: ['at ./index.rsh:60:52:application call to "makeCommitment (as function)" (defined at: reach standard library:65:8:function exp)', 'at ./index.rsh:61:51:after expr stmt semicolon call to "function" (defined at: ./index.rsh:58:19:function exp)'],
        who: 'Alice' });
      const v64 = stdlib.keccak256(v63, v61);
      
      
      const txn3 = await ctc.sendrecv('Alice', stdlib.bigNumberify(4), stdlib.bigNumberify(1), [stdlib.T_Address, stdlib.T_UInt256, stdlib.T_Address, stdlib.T_UInt256], [v28, v27, v34, v64], stdlib.bigNumberify(0), [stdlib.T_UInt256], stdlib.bigNumberify(10), ((txn3) => {
        const sim_r = { txns: [] };
        sim_r.prevSt = stdlib.keccak256(v28, v27, v34);
        const [v65] = txn3.data;
        
        const v66 = txn3.value;
        const v68 = stdlib.eq(stdlib.bigNumberify(0), v66);
        stdlib.assert(v68, {
          at: './index.rsh:application',
          fs: [],
          who: 'Alice' });
        sim_r.nextSt = stdlib.keccak256(v28, v27, v34, v65);
        sim_r.isHalt = false;
        return sim_r; }));
      if (txn3.didTimeout) {
        const txn4 = await ctc.recv('Alice', stdlib.bigNumberify(9), stdlib.bigNumberify(0), [], false);
        const [] = txn4.data;
        const v70 = txn4.value;
        const v72 = stdlib.eq(stdlib.bigNumberify(0), v70);
        stdlib.assert(v72, {
          at: 'reach standard library:application',
          fs: ['at ./index.rsh:63:43:application call to "closeTo (as function)" (defined at: reach standard library:73:8:function exp)'],
          who: 'Alice' });
        const v73 = txn4.balance;
        ;
        stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
          at: './index.rsh:40:33:application',
          fs: ['at ./index.rsh:40:39:after expr stmt semicolon call to "function" (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:77:8:application call to "informTimeout (as function)" (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:63:43:application call to "closeTo (as function)" (defined at: reach standard library:73:8:function exp)'],
          who: 'Alice' });
        
        return; }
      else {
        const [v65] = txn3.data;
        const v66 = txn3.value;
        const v68 = stdlib.eq(stdlib.bigNumberify(0), v66);
        stdlib.assert(v68, {
          at: './index.rsh:application',
          fs: [],
          who: 'Alice' });
        const txn4 = await ctc.recv('Alice', stdlib.bigNumberify(5), stdlib.bigNumberify(1), [stdlib.T_UInt256], stdlib.bigNumberify(10));
        if (txn4.didTimeout) {
          
          const txn5 = await ctc.sendrecv('Alice', stdlib.bigNumberify(8), stdlib.bigNumberify(0), [stdlib.T_Address, stdlib.T_UInt256, stdlib.T_Address, stdlib.T_UInt256], [v28, v27, v34, v65], stdlib.bigNumberify(0), [], false, ((txn5) => {
            const sim_r = { txns: [] };
            sim_r.prevSt = stdlib.keccak256(v28, v27, v34, v65);
            const [] = txn5.data;
            
            const v86 = txn5.value;
            const v88 = stdlib.eq(stdlib.bigNumberify(0), v86);
            stdlib.assert(v88, {
              at: 'reach standard library:application',
              fs: ['at ./index.rsh:70:43:application call to "closeTo (as function)" (defined at: reach standard library:73:8:function exp)'],
              who: 'Alice' });
            const v89 = txn5.balance;
            sim_r.txns.push({
              amt: v89,
              to: v28 });
            sim_r.nextSt = '';
            sim_r.isHalt = true;
            return sim_r; }));
          const [] = txn5.data;
          const v86 = txn5.value;
          const v88 = stdlib.eq(stdlib.bigNumberify(0), v86);
          stdlib.assert(v88, {
            at: 'reach standard library:application',
            fs: ['at ./index.rsh:70:43:application call to "closeTo (as function)" (defined at: reach standard library:73:8:function exp)'],
            who: 'Alice' });
          const v89 = txn5.balance;
          ;
          stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
            at: './index.rsh:40:33:application',
            fs: ['at ./index.rsh:40:39:after expr stmt semicolon call to "function" (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:77:8:application call to "informTimeout (as function)" (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:70:43:application call to "closeTo (as function)" (defined at: reach standard library:73:8:function exp)'],
            who: 'Alice' });
          
          return; }
        else {
          const [v81] = txn4.data;
          const v82 = txn4.value;
          const v84 = stdlib.eq(stdlib.bigNumberify(0), v82);
          stdlib.assert(v84, {
            at: './index.rsh:application',
            fs: [],
            who: 'Alice' });
          
          
          const txn5 = await ctc.sendrecv('Alice', stdlib.bigNumberify(6), stdlib.bigNumberify(2), [stdlib.T_Address, stdlib.T_UInt256, stdlib.T_Address, stdlib.T_UInt256, stdlib.T_UInt256, stdlib.T_UInt256, stdlib.T_UInt256], [v28, v27, v34, v65, v81, v63, v61], stdlib.bigNumberify(0), [stdlib.T_UInt256, stdlib.T_UInt256], stdlib.bigNumberify(10), ((txn5) => {
            const sim_r = { txns: [] };
            sim_r.prevSt = stdlib.keccak256(v28, v27, v34, v65, v81);
            const [v96, v97] = txn5.data;
            
            const v98 = txn5.value;
            const v100 = stdlib.eq(stdlib.bigNumberify(0), v98);
            stdlib.assert(v100, {
              at: './index.rsh:application',
              fs: [],
              who: 'Alice' });
            const v112 = stdlib.keccak256(v96, v97);
            const v114 = stdlib.eq(v65, v112);
            stdlib.assert(v114, {
              at: 'reach standard library:71:17:application',
              fs: ['at ./index.rsh:77:24:application call to "checkCommitment (as function)" (defined at: reach standard library:70:8:function exp)'],
              who: 'Alice' });
            const v116 = stdlib.sub(stdlib.bigNumberify(4), v81);
            const v117 = stdlib.add(v97, v116);
            const v118 = stdlib.mod(v117, stdlib.bigNumberify(3));
            
            return sim_r; }));
          if (txn5.didTimeout) {
            const txn6 = await ctc.recv('Alice', stdlib.bigNumberify(7), stdlib.bigNumberify(0), [], false);
            const [] = txn6.data;
            const v102 = txn6.value;
            const v104 = stdlib.eq(stdlib.bigNumberify(0), v102);
            stdlib.assert(v104, {
              at: 'reach standard library:application',
              fs: ['at ./index.rsh:76:43:application call to "closeTo (as function)" (defined at: reach standard library:73:8:function exp)'],
              who: 'Alice' });
            const v105 = txn6.balance;
            ;
            stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
              at: './index.rsh:40:33:application',
              fs: ['at ./index.rsh:40:39:after expr stmt semicolon call to "function" (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:77:8:application call to "informTimeout (as function)" (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:76:43:application call to "closeTo (as function)" (defined at: reach standard library:73:8:function exp)'],
              who: 'Alice' });
            
            return; }
          else {
            const [v96, v97] = txn5.data;
            const v98 = txn5.value;
            const v100 = stdlib.eq(stdlib.bigNumberify(0), v98);
            stdlib.assert(v100, {
              at: './index.rsh:application',
              fs: [],
              who: 'Alice' });
            const v112 = stdlib.keccak256(v96, v97);
            const v114 = stdlib.eq(v65, v112);
            stdlib.assert(v114, {
              at: 'reach standard library:71:17:application',
              fs: ['at ./index.rsh:77:24:application call to "checkCommitment (as function)" (defined at: reach standard library:70:8:function exp)'],
              who: 'Alice' });
            const v116 = stdlib.sub(stdlib.bigNumberify(4), v81);
            const v117 = stdlib.add(v97, v116);
            const v118 = stdlib.mod(v117, stdlib.bigNumberify(3));
            v48 = v118;
            continue; } } } }
    const v124 = stdlib.mul(stdlib.bigNumberify(2), v27);
    const v126 = stdlib.eq(v48, stdlib.bigNumberify(2));
    const v127 = v126 ? v28 : v34;
    ;
    stdlib.protect(stdlib.T_Null, await interact.seeOutcome(v48), {
      at: './index.rsh:87:28:application',
      fs: ['at ./index.rsh:87:41:after expr stmt semicolon call to "function" (defined at: ./index.rsh:86:23:function exp)'],
      who: 'Alice' });
    
    return; } }
export async function Bob(stdlib, ctc, interact) {
  const txn0 = { balance: 0, value: 0 };
  const txn1 = await ctc.recv('Bob', stdlib.bigNumberify(1), stdlib.bigNumberify(1), [stdlib.T_UInt256], false);
  const [v27] = txn1.data;
  const v28 = txn1.from;
  const v29 = txn1.value;
  const v31 = stdlib.eq(v27, v29);
  stdlib.assert(v31, {
    at: './index.rsh:application',
    fs: [],
    who: 'Bob' });
  stdlib.protect(stdlib.T_Null, await interact.acceptWager(v27), {
    at: './index.rsh:49:29:application',
    fs: ['at ./index.rsh:49:40:after expr stmt semicolon call to "function" (defined at: ./index.rsh:48:17:function exp)'],
    who: 'Bob' });
  
  
  const txn2 = await ctc.sendrecv('Bob', stdlib.bigNumberify(2), stdlib.bigNumberify(0), [stdlib.T_Address, stdlib.T_UInt256], [v28, v27], v27, [], stdlib.bigNumberify(10), ((txn2) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.keccak256(v28, v27);
    const [] = txn2.data;
    const v34 = txn2.from;
    
    const v35 = txn2.value;
    const v37 = stdlib.eq(v27, v35);
    stdlib.assert(v37, {
      at: './index.rsh:application',
      fs: [],
      who: 'Bob' });
    
    return sim_r; }));
  if (txn2.didTimeout) {
    const txn3 = await ctc.recv('Bob', stdlib.bigNumberify(10), stdlib.bigNumberify(0), [], false);
    const [] = txn3.data;
    const v39 = txn3.value;
    const v41 = stdlib.eq(stdlib.bigNumberify(0), v39);
    stdlib.assert(v41, {
      at: 'reach standard library:application',
      fs: ['at ./index.rsh:51:41:application call to "closeTo (as function)" (defined at: reach standard library:73:8:function exp)'],
      who: 'Bob' });
    const v42 = txn3.balance;
    ;
    stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
      at: './index.rsh:40:33:application',
      fs: ['at ./index.rsh:40:39:after expr stmt semicolon call to "function" (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:77:8:application call to "informTimeout (as function)" (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:51:41:application call to "closeTo (as function)" (defined at: reach standard library:73:8:function exp)'],
      who: 'Bob' });
    
    return; }
  else {
    const [] = txn2.data;
    const v34 = txn2.from;
    const v35 = txn2.value;
    const v37 = stdlib.eq(v27, v35);
    stdlib.assert(v37, {
      at: './index.rsh:application',
      fs: [],
      who: 'Bob' });
    let v48 = stdlib.bigNumberify(1);
    while ((() => {
      const v59 = stdlib.eq(v48, stdlib.bigNumberify(1));
      
      return v59; })()) {
      const txn3 = await ctc.recv('Bob', stdlib.bigNumberify(4), stdlib.bigNumberify(1), [stdlib.T_UInt256], stdlib.bigNumberify(10));
      if (txn3.didTimeout) {
        
        const txn4 = await ctc.sendrecv('Bob', stdlib.bigNumberify(9), stdlib.bigNumberify(0), [stdlib.T_Address, stdlib.T_UInt256, stdlib.T_Address], [v28, v27, v34], stdlib.bigNumberify(0), [], false, ((txn4) => {
          const sim_r = { txns: [] };
          sim_r.prevSt = stdlib.keccak256(v28, v27, v34);
          const [] = txn4.data;
          
          const v70 = txn4.value;
          const v72 = stdlib.eq(stdlib.bigNumberify(0), v70);
          stdlib.assert(v72, {
            at: 'reach standard library:application',
            fs: ['at ./index.rsh:63:43:application call to "closeTo (as function)" (defined at: reach standard library:73:8:function exp)'],
            who: 'Bob' });
          const v73 = txn4.balance;
          sim_r.txns.push({
            amt: v73,
            to: v34 });
          sim_r.nextSt = '';
          sim_r.isHalt = true;
          return sim_r; }));
        const [] = txn4.data;
        const v70 = txn4.value;
        const v72 = stdlib.eq(stdlib.bigNumberify(0), v70);
        stdlib.assert(v72, {
          at: 'reach standard library:application',
          fs: ['at ./index.rsh:63:43:application call to "closeTo (as function)" (defined at: reach standard library:73:8:function exp)'],
          who: 'Bob' });
        const v73 = txn4.balance;
        ;
        stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
          at: './index.rsh:40:33:application',
          fs: ['at ./index.rsh:40:39:after expr stmt semicolon call to "function" (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:77:8:application call to "informTimeout (as function)" (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:63:43:application call to "closeTo (as function)" (defined at: reach standard library:73:8:function exp)'],
          who: 'Bob' });
        
        return; }
      else {
        const [v65] = txn3.data;
        const v66 = txn3.value;
        const v68 = stdlib.eq(stdlib.bigNumberify(0), v66);
        stdlib.assert(v68, {
          at: './index.rsh:application',
          fs: [],
          who: 'Bob' });
        const v80 = stdlib.protect(stdlib.T_UInt256, await interact.getHand(), {
          at: './index.rsh:68:52:application',
          fs: ['at ./index.rsh:68:59:after expr stmt semicolon call to "function" (defined at: ./index.rsh:67:19:function exp)'],
          who: 'Bob' });
        
        
        const txn4 = await ctc.sendrecv('Bob', stdlib.bigNumberify(5), stdlib.bigNumberify(1), [stdlib.T_Address, stdlib.T_UInt256, stdlib.T_Address, stdlib.T_UInt256, stdlib.T_UInt256], [v28, v27, v34, v65, v80], stdlib.bigNumberify(0), [stdlib.T_UInt256], stdlib.bigNumberify(10), ((txn4) => {
          const sim_r = { txns: [] };
          sim_r.prevSt = stdlib.keccak256(v28, v27, v34, v65);
          const [v81] = txn4.data;
          
          const v82 = txn4.value;
          const v84 = stdlib.eq(stdlib.bigNumberify(0), v82);
          stdlib.assert(v84, {
            at: './index.rsh:application',
            fs: [],
            who: 'Bob' });
          sim_r.nextSt = stdlib.keccak256(v28, v27, v34, v65, v81);
          sim_r.isHalt = false;
          return sim_r; }));
        if (txn4.didTimeout) {
          const txn5 = await ctc.recv('Bob', stdlib.bigNumberify(8), stdlib.bigNumberify(0), [], false);
          const [] = txn5.data;
          const v86 = txn5.value;
          const v88 = stdlib.eq(stdlib.bigNumberify(0), v86);
          stdlib.assert(v88, {
            at: 'reach standard library:application',
            fs: ['at ./index.rsh:70:43:application call to "closeTo (as function)" (defined at: reach standard library:73:8:function exp)'],
            who: 'Bob' });
          const v89 = txn5.balance;
          ;
          stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
            at: './index.rsh:40:33:application',
            fs: ['at ./index.rsh:40:39:after expr stmt semicolon call to "function" (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:77:8:application call to "informTimeout (as function)" (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:70:43:application call to "closeTo (as function)" (defined at: reach standard library:73:8:function exp)'],
            who: 'Bob' });
          
          return; }
        else {
          const [v81] = txn4.data;
          const v82 = txn4.value;
          const v84 = stdlib.eq(stdlib.bigNumberify(0), v82);
          stdlib.assert(v84, {
            at: './index.rsh:application',
            fs: [],
            who: 'Bob' });
          const txn5 = await ctc.recv('Bob', stdlib.bigNumberify(6), stdlib.bigNumberify(2), [stdlib.T_UInt256, stdlib.T_UInt256], stdlib.bigNumberify(10));
          if (txn5.didTimeout) {
            
            const txn6 = await ctc.sendrecv('Bob', stdlib.bigNumberify(7), stdlib.bigNumberify(0), [stdlib.T_Address, stdlib.T_UInt256, stdlib.T_Address, stdlib.T_UInt256, stdlib.T_UInt256], [v28, v27, v34, v65, v81], stdlib.bigNumberify(0), [], false, ((txn6) => {
              const sim_r = { txns: [] };
              sim_r.prevSt = stdlib.keccak256(v28, v27, v34, v65, v81);
              const [] = txn6.data;
              
              const v102 = txn6.value;
              const v104 = stdlib.eq(stdlib.bigNumberify(0), v102);
              stdlib.assert(v104, {
                at: 'reach standard library:application',
                fs: ['at ./index.rsh:76:43:application call to "closeTo (as function)" (defined at: reach standard library:73:8:function exp)'],
                who: 'Bob' });
              const v105 = txn6.balance;
              sim_r.txns.push({
                amt: v105,
                to: v34 });
              sim_r.nextSt = '';
              sim_r.isHalt = true;
              return sim_r; }));
            const [] = txn6.data;
            const v102 = txn6.value;
            const v104 = stdlib.eq(stdlib.bigNumberify(0), v102);
            stdlib.assert(v104, {
              at: 'reach standard library:application',
              fs: ['at ./index.rsh:76:43:application call to "closeTo (as function)" (defined at: reach standard library:73:8:function exp)'],
              who: 'Bob' });
            const v105 = txn6.balance;
            ;
            stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
              at: './index.rsh:40:33:application',
              fs: ['at ./index.rsh:40:39:after expr stmt semicolon call to "function" (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:77:8:application call to "informTimeout (as function)" (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:76:43:application call to "closeTo (as function)" (defined at: reach standard library:73:8:function exp)'],
              who: 'Bob' });
            
            return; }
          else {
            const [v96, v97] = txn5.data;
            const v98 = txn5.value;
            const v100 = stdlib.eq(stdlib.bigNumberify(0), v98);
            stdlib.assert(v100, {
              at: './index.rsh:application',
              fs: [],
              who: 'Bob' });
            const v112 = stdlib.keccak256(v96, v97);
            const v114 = stdlib.eq(v65, v112);
            stdlib.assert(v114, {
              at: 'reach standard library:71:17:application',
              fs: ['at ./index.rsh:77:24:application call to "checkCommitment (as function)" (defined at: reach standard library:70:8:function exp)'],
              who: 'Bob' });
            const v116 = stdlib.sub(stdlib.bigNumberify(4), v81);
            const v117 = stdlib.add(v97, v116);
            const v118 = stdlib.mod(v117, stdlib.bigNumberify(3));
            v48 = v118;
            continue; } } } }
    const v124 = stdlib.mul(stdlib.bigNumberify(2), v27);
    const v126 = stdlib.eq(v48, stdlib.bigNumberify(2));
    const v127 = v126 ? v28 : v34;
    ;
    stdlib.protect(stdlib.T_Null, await interact.seeOutcome(v48), {
      at: './index.rsh:87:28:application',
      fs: ['at ./index.rsh:87:41:after expr stmt semicolon call to "function" (defined at: ./index.rsh:86:23:function exp)'],
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
  byte base64(aA==)
  app_global_get
  bnz halted
  txn OnCompletion
  int NoOp
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
  gtxn 1 Sender
  byte "{{m1}}"
  ==
  ||
  gtxn 1 Sender
  byte "{{m2}}"
  ==
  ||
  gtxn 1 Sender
  byte "{{m4}}"
  ==
  ||
  gtxn 1 Sender
  byte "{{m5}}"
  ==
  ||
  gtxn 1 Sender
  byte "{{m6}}"
  ==
  ||
  gtxn 1 Sender
  byte "{{m7}}"
  ==
  ||
  gtxn 1 Sender
  byte "{{m8}}"
  ==
  ||
  gtxn 1 Sender
  byte "{{m9}}"
  ==
  ||
  gtxn 1 Sender
  byte "{{m10}}"
  ==
  ||
  bz revert
  byte base64(cw==)
  app_global_get
  gtxna 1 Args 0
  ==
  bz revert
  byte base64(bA==)
  app_global_get
  gtxna 1 Args 3
  btoi
  ==
  bz revert
  // Don't check anyone else, because Handler does
  // Update state
  byte base64(cw==)
  gtxna 1 Args 1
  app_global_put
  byte base64(bA==)
  global Round
  app_global_put
  byte base64(aA==)
  gtxna 1 Args 2
  btoi
  app_global_put
  b done
  halted:
  txn OnCompletion
  int DeleteApplication
  ==
  bz revert
  global GroupSize
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
  global GroupSize
  int 1
  ==
  bz revert
  txn Sender
  byte "{{Deployer}}"
  ==
  bz revert
  txn ApplicationID
  bz init
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
  m1: `#pragma version 2
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
  gtxn 2 TypeEnum
  int pay
  ==
  bz revert
  gtxn 2 Receiver
  txn Sender
  ==
  bz revert
  gtxn 2 Amount
  gtxn 1 Fee
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
  int 1
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
  gtxn 2 Sender
  ==
  bz revert
  txn NumArgs
  int 5
  ==
  bz revert
  int 0
  itob
  keccak256
  arg 0
  ==
  bz revert
  // Run body
  arg 4
  btoi
  gtxn 3 Amount
  ==
  bz revert
  int 1
  itob
  gtxn 3 Sender
  concat
  arg 4
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
  // Check time limits
  revert:
  int 0
  return
  done:
  int 1
  return
  `,
  m10: `#pragma version 2
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
  gtxn 2 TypeEnum
  int pay
  ==
  bz revert
  gtxn 2 Receiver
  txn Sender
  ==
  bz revert
  gtxn 2 Amount
  gtxn 1 Fee
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
  int 1
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
  gtxn 2 Sender
  ==
  bz revert
  txn NumArgs
  int 6
  ==
  bz revert
  gtxn 3 Sender
  arg 4
  ==
  bz revert
  int 1
  itob
  arg 4
  concat
  arg 5
  concat
  keccak256
  arg 0
  ==
  bz revert
  // Run body
  int 0
  gtxn 3 Amount
  ==
  bz revert
  gtxn 4 TypeEnum
  int pay
  ==
  bz revert
  gtxn 4 Receiver
  arg 4
  ==
  bz revert
  gtxn 4 Amount
  // XXX BALANCE
  ==
  bz revert
  gtxn 4 Sender
  byte "{{ContractAddr}}"
  ==
  bz revert
  byte base64()
  arg 1
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
  // Check time limits
  arg 3
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
  `,
  m2: `#pragma version 2
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
  gtxn 2 TypeEnum
  int pay
  ==
  bz revert
  gtxn 2 Receiver
  txn Sender
  ==
  bz revert
  gtxn 2 Amount
  gtxn 1 Fee
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
  int 1
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
  gtxn 2 Sender
  ==
  bz revert
  txn NumArgs
  int 6
  ==
  bz revert
  int 1
  itob
  arg 4
  concat
  arg 5
  concat
  keccak256
  arg 0
  ==
  bz revert
  // Run body
  arg 5
  btoi
  gtxn 3 Amount
  ==
  bz revert
  // XXX jump
  // Check GroupSize
  global GroupSize
  int 4
  ==
  bz revert
  // Check time limits
  arg 3
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
  `,
  m4: `#pragma version 2
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
  gtxn 2 TypeEnum
  int pay
  ==
  bz revert
  gtxn 2 Receiver
  txn Sender
  ==
  bz revert
  gtxn 2 Amount
  gtxn 1 Fee
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
  int 1
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
  gtxn 2 Sender
  ==
  bz revert
  txn NumArgs
  int 8
  ==
  bz revert
  gtxn 3 Sender
  arg 4
  ==
  bz revert
  int 3
  itob
  arg 4
  concat
  arg 5
  concat
  arg 6
  concat
  keccak256
  arg 0
  ==
  bz revert
  // Run body
  int 0
  gtxn 3 Amount
  ==
  bz revert
  int 4
  itob
  arg 4
  concat
  arg 5
  concat
  arg 6
  concat
  arg 7
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
  // Check time limits
  arg 3
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
  `,
  m5: `#pragma version 2
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
  gtxn 2 TypeEnum
  int pay
  ==
  bz revert
  gtxn 2 Receiver
  txn Sender
  ==
  bz revert
  gtxn 2 Amount
  gtxn 1 Fee
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
  int 1
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
  gtxn 2 Sender
  ==
  bz revert
  txn NumArgs
  int 9
  ==
  bz revert
  gtxn 3 Sender
  arg 6
  ==
  bz revert
  int 4
  itob
  arg 4
  concat
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
  int 0
  gtxn 3 Amount
  ==
  bz revert
  int 5
  itob
  arg 4
  concat
  arg 5
  concat
  arg 6
  concat
  arg 7
  concat
  arg 8
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
  // Check time limits
  arg 3
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
  `,
  m6: `#pragma version 2
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
  gtxn 2 TypeEnum
  int pay
  ==
  bz revert
  gtxn 2 Receiver
  txn Sender
  ==
  bz revert
  gtxn 2 Amount
  gtxn 1 Fee
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
  int 1
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
  gtxn 2 Sender
  ==
  bz revert
  txn NumArgs
  int 11
  ==
  bz revert
  gtxn 3 Sender
  arg 4
  ==
  bz revert
  int 5
  itob
  arg 4
  concat
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
  int 0
  gtxn 3 Amount
  ==
  bz revert
  arg 7
  btoi
  arg 9
  arg 10
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
  // Check time limits
  arg 3
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
  `,
  m7: `#pragma version 2
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
  gtxn 2 TypeEnum
  int pay
  ==
  bz revert
  gtxn 2 Receiver
  txn Sender
  ==
  bz revert
  gtxn 2 Amount
  gtxn 1 Fee
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
  int 1
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
  gtxn 2 Sender
  ==
  bz revert
  txn NumArgs
  int 9
  ==
  bz revert
  gtxn 3 Sender
  arg 6
  ==
  bz revert
  int 5
  itob
  arg 4
  concat
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
  int 0
  gtxn 3 Amount
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
  // XXX BALANCE
  ==
  bz revert
  gtxn 4 Sender
  byte "{{ContractAddr}}"
  ==
  bz revert
  byte base64()
  arg 1
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
  // Check time limits
  arg 3
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
  `,
  m8: `#pragma version 2
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
  gtxn 2 TypeEnum
  int pay
  ==
  bz revert
  gtxn 2 Receiver
  txn Sender
  ==
  bz revert
  gtxn 2 Amount
  gtxn 1 Fee
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
  int 1
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
  gtxn 2 Sender
  ==
  bz revert
  txn NumArgs
  int 8
  ==
  bz revert
  gtxn 3 Sender
  arg 4
  ==
  bz revert
  int 4
  itob
  arg 4
  concat
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
  int 0
  gtxn 3 Amount
  ==
  bz revert
  gtxn 4 TypeEnum
  int pay
  ==
  bz revert
  gtxn 4 Receiver
  arg 4
  ==
  bz revert
  gtxn 4 Amount
  // XXX BALANCE
  ==
  bz revert
  gtxn 4 Sender
  byte "{{ContractAddr}}"
  ==
  bz revert
  byte base64()
  arg 1
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
  // Check time limits
  arg 3
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
  `,
  m9: `#pragma version 2
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
  gtxn 2 TypeEnum
  int pay
  ==
  bz revert
  gtxn 2 Receiver
  txn Sender
  ==
  bz revert
  gtxn 2 Amount
  gtxn 1 Fee
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
  int 1
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
  gtxn 2 Sender
  ==
  bz revert
  txn NumArgs
  int 7
  ==
  bz revert
  gtxn 3 Sender
  arg 6
  ==
  bz revert
  int 3
  itob
  arg 4
  concat
  arg 5
  concat
  arg 6
  concat
  keccak256
  arg 0
  ==
  bz revert
  // Run body
  int 0
  gtxn 3 Amount
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
  // XXX BALANCE
  ==
  bz revert
  gtxn 4 Sender
  byte "{{ContractAddr}}"
  ==
  bz revert
  byte base64()
  arg 1
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
  // Check time limits
  arg 3
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
  `,
  steps: `9`,
  unsupported: `True` };
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
          "name": "v27",
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
          "name": "v65",
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
          "name": "v81",
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
          "name": "v96",
          "type": "uint256"
        },
        {
          "indexed": false,
          "internalType": "uint256",
          "name": "v97",
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
              "name": "v34",
              "type": "address"
            },
            {
              "internalType": "uint256",
              "name": "v65",
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
              "name": "v34",
              "type": "address"
            },
            {
              "internalType": "uint256",
              "name": "v65",
              "type": "uint256"
            },
            {
              "internalType": "uint256",
              "name": "v81",
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
              "name": "v34",
              "type": "address"
            },
            {
              "internalType": "uint256",
              "name": "v65",
              "type": "uint256"
            },
            {
              "internalType": "uint256",
              "name": "v81",
              "type": "uint256"
            },
            {
              "internalType": "uint256",
              "name": "v96",
              "type": "uint256"
            },
            {
              "internalType": "uint256",
              "name": "v97",
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
              "name": "v34",
              "type": "address"
            },
            {
              "internalType": "uint256",
              "name": "v65",
              "type": "uint256"
            },
            {
              "internalType": "uint256",
              "name": "v81",
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
              "name": "v34",
              "type": "address"
            },
            {
              "internalType": "uint256",
              "name": "v65",
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
              "name": "v34",
              "type": "address"
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
  Bytecode: `0x608060405261001160004360a0610031565b60408051601f19818403018152919052805160209091012060005561003f565b918252602082015260400190565b610e838061004e6000396000f3fe6080604052600436106100865760003560e01c80639cb54e40116100595780639cb54e40146100d9578063b665f1cf146100ec578063bc3ab437146100ff578063db3d194014610112578063f3993feb1461012557610086565b806303fcf1691461008b57806347945529146100a05780638f7c75ae146100b357806399857a94146100c6575b600080fd5b61009e610099366004610cf7565b610138565b005b61009e6100ae366004610d23565b6101fa565b61009e6100c1366004610d3e565b610343565b61009e6100d4366004610d08565b6104f1565b61009e6100e7366004610cdc565b610625565b61009e6100fa366004610d23565b610717565b61009e61010d366004610cdc565b61084d565b61009e610120366004610d08565b61096a565b61009e610133366004610d50565b610a94565b60405161014d90600090833590602001610d6a565b6040516020818303038152906040528051906020012060001c6000541461017357600080fd5b3481602001351461018357600080fd5b7fc9d006980a027d32b08195d9eab835f93f490ac52038c85ed3e8aca6e0b9b2e14782602001356040516101b8929190610d6a565b60405180910390a16001433383602001356040516020016101dc9493929190610d78565b60408051601f19818403018152919052805160209091012060005550565b6005813561020e6040840160208501610cae565b60408401356102236080860160608701610cae565b85608001358660a001356040516020016102439796959493929190610dfc565b6040516020818303038152906040528051906020012060001c6000541461026957600080fd5b6102796080820160608301610cae565b6001600160a01b0316336001600160a01b03161461029657600080fd5b8035600a0143108015906102a8575060015b6102b157600080fd5b34156102bc57600080fd5b6102cc6080820160608301610cae565b6001600160a01b03166108fc479081150290604051600060405180830381858888f19350505050158015610304573d6000803e3d6000fd5b507ffc55d683ac816a7149ebdfa999ae1bcfeeae27c37c9dab64a23f617beed2a007476040516103349190610d61565b60405180910390a16000805533ff5b600581356103576040840160208501610cae565b604084013561036c6080860160608701610cae565b85608001358660a0013560405160200161038c9796959493929190610dfc565b6040516020818303038152906040528051906020012060001c600054146103b257600080fd5b6103c26040820160208301610cae565b6001600160a01b0316336001600160a01b0316146103df57600080fd5b8035600a0143106103ef57600080fd5b34156103fa57600080fd5b8060c001358160e00135604051602001610415929190610d6a565b6040516020818303038152906040528051906020012060001c81608001351461043d57600080fd5b7f3a17d070a78674d0fecc57e42515183089cfc8c61973a362a95c57f4d000ea84478260c001358360e0013560405161047893929190610e37565b60405180910390a16104ee60405180608001604052808360200160208101906104a19190610cae565b6001600160a01b031681526040808501356020830152016104c86080850160608601610cae565b6001600160a01b03168152602001600360e085013560a086013560040301069052610bc2565b50565b600481356105056040840160208501610cae565b604084013561051a6080860160608701610cae565b856080013560405160200161053496959493929190610dc8565b6040516020818303038152906040528051906020012060001c6000541461055a57600080fd5b61056a6040820160208301610cae565b6001600160a01b0316336001600160a01b03161461058757600080fd5b8035600a014310801590610599575060015b6105a257600080fd5b34156105ad57600080fd5b6105bd6040820160208301610cae565b6001600160a01b03166108fc479081150290604051600060405180830381858888f193505050501580156105f5573d6000803e3d6000fd5b507f3a6f8023909a26b76d462631fcdf570dbe3740447548e09470d1ad04394a0cec476040516103349190610d61565b600181356106396040840160208501610cae565b83604001356040516020016106519493929190610d78565b6040516020818303038152906040528051906020012060001c6000541461067757600080fd5b8035600a01431061068757600080fd5b3481604001351461069757600080fd5b7ff04f5fc87a72102f7c0b228f8bbaf9b9aa7a2b5dc295c86538fdde91e95866e9476040516106c69190610d61565b60405180910390a16104ee60405180608001604052808360200160208101906106ef9190610cae565b6001600160a01b03168152604080850135602083015233908201526001606090910152610bc2565b6004813561072b6040840160208501610cae565b60408401356107406080860160608701610cae565b856080013560405160200161075a96959493929190610dc8565b6040516020818303038152906040528051906020012060001c6000541461078057600080fd5b6107906080820160608301610cae565b6001600160a01b0316336001600160a01b0316146107ad57600080fd5b8035600a0143106107bd57600080fd5b34156107c857600080fd5b7fabf482d77b67111a4971bb96fe81961f83ba459eb1d8fa9f78b6908251aeef1a478260a001356040516107fd929190610d6a565b60405180910390a16005436108186040840160208501610cae565b604084013561082d6080860160608701610cae565b85608001358660a001356040516020016101dc9796959493929190610dfc565b600181356108616040840160208501610cae565b83604001356040516020016108799493929190610d78565b6040516020818303038152906040528051906020012060001c6000541461089f57600080fd5b6108af6040820160208301610cae565b6001600160a01b0316336001600160a01b0316146108cc57600080fd5b8035600a0143108015906108de575060015b6108e757600080fd5b34156108f257600080fd5b6109026040820160208301610cae565b6001600160a01b03166108fc479081150290604051600060405180830381858888f1935050505015801561093a573d6000803e3d6000fd5b507f9bf9cf9ae88051b33b19923b1c1cf36013b840c9975de29305d444b55d83c6bd476040516103349190610d61565b6003813561097e6040840160208501610cae565b60408401356109936080860160608701610cae565b6040516020016109a7959493929190610d9c565b6040516020818303038152906040528051906020012060001c600054146109cd57600080fd5b6109dd6040820160208301610cae565b6001600160a01b0316336001600160a01b0316146109fa57600080fd5b8035600a014310610a0a57600080fd5b3415610a1557600080fd5b7fb71d350b59ceca5c6544e5367d61ca8cae3e36b25f8d900743d063dff3d6508b478260800135604051610a4a929190610d6a565b60405180910390a1600443610a656040840160208501610cae565b6040840135610a7a6080860160608701610cae565b85608001356040516020016101dc96959493929190610dc8565b60038135610aa86040840160208501610cae565b6040840135610abd6080860160608701610cae565b604051602001610ad1959493929190610d9c565b6040516020818303038152906040528051906020012060001c60005414610af757600080fd5b610b076080820160608301610cae565b6001600160a01b0316336001600160a01b031614610b2457600080fd5b8035600a014310801590610b36575060015b610b3f57600080fd5b3415610b4a57600080fd5b610b5a6080820160608301610cae565b6001600160a01b03166108fc479081150290604051600060405180830381858888f19350505050158015610b92573d6000803e3d6000fd5b507fc92018b4e91e597d736654f7b1d2ec034c5fec5920e2cfe22e15b4ddcdf5e18a476040516103349190610d61565b600181606001511415610c1557600343826000015183602001518460400151604051602001610bf5959493929190610d9c565b60408051601f1981840301815291905280516020909101206000556104ee565b6002816060015114610c2b578060400151610c2e565b80515b6001600160a01b03166108fc82602001516002029081150290604051600060405180830381858888f19350505050158015610c6d573d6000803e3d6000fd5b506000805533ff5b600060608284031215610c86578081fd5b50919050565b600060a08284031215610c86578081fd5b600060c08284031215610c86578081fd5b600060208284031215610cbf578081fd5b81356001600160a01b0381168114610cd5578182fd5b9392505050565b600060608284031215610ced578081fd5b610cd58383610c75565b600060408284031215610c86578081fd5b600060a08284031215610d19578081fd5b610cd58383610c8c565b600060c08284031215610d34578081fd5b610cd58383610c9d565b60006101008284031215610c86578081fd5b600060808284031215610c86578081fd5b90815260200190565b918252602082015260400190565b93845260208401929092526001600160a01b03166040830152606082015260800190565b94855260208501939093526001600160a01b039182166040850152606084015216608082015260a00190565b95865260208601949094526001600160a01b039283166040860152606085019190915216608083015260a082015260c00190565b96875260208701959095526001600160a01b0393841660408701526060860192909252909116608084015260a083015260c082015260e00190565b928352602083019190915260408201526060019056fea2646970667358221220ce6cdab90d383c358a18de0896deb17c09138c3c323e56e3478f765a2f923bd864736f6c63430007010033`,
  deployMode: `DM_constructor` };

export const _Connectors = {
  ALGO: _ALGO,
  ETH: _ETH };
