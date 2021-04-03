// Automatically generated with Reach 0.1.2
/* eslint-disable */
export const _version = '0.1.2';


export async function Alice(ctc, interact) {
  const stdlib = ctc.stdlib;
  const ctc0 = stdlib.T_UInt;
  const ctc1 = stdlib.T_Tuple([ctc0, ctc0]);
  const ctc2 = stdlib.T_Digest;
  const ctc3 = stdlib.T_Null;
  const ctc4 = stdlib.T_Address;
  const ctc5 = stdlib.T_Tuple([ctc0, ctc4, ctc0, ctc0, ctc4, ctc0, ctc0]);
  const ctc6 = stdlib.T_Tuple([ctc0, ctc4, ctc0, ctc0, ctc4, ctc0]);
  const ctc7 = stdlib.T_Tuple([]);
  const ctc8 = stdlib.T_Tuple([ctc0, ctc4, ctc0, ctc0, ctc4, ctc2, ctc0, ctc0, ctc0]);
  const ctc9 = stdlib.T_Tuple([ctc0, ctc4, ctc0, ctc0, ctc4, ctc2, ctc0, ctc0]);
  const ctc10 = stdlib.T_Tuple([ctc0, ctc4, ctc0, ctc0, ctc4, ctc2, ctc0]);
  const ctc11 = stdlib.T_Tuple([ctc0, ctc4, ctc0, ctc0, ctc2, ctc4, ctc0, ctc0, ctc0]);
  const ctc12 = stdlib.T_Tuple([ctc0, ctc4, ctc0, ctc0, ctc2, ctc4, ctc0, ctc0]);
  const ctc13 = stdlib.T_Tuple([ctc0, ctc4, ctc0, ctc0, ctc2, ctc0, ctc0]);
  const ctc14 = stdlib.T_Tuple([ctc0, ctc4, ctc0, ctc0, ctc2, ctc0]);
  const ctc15 = stdlib.T_Tuple([ctc0]);
  
  
  const v41 = await ctc.creationTime();
  const v37 = stdlib.protect(ctc0, interact.DEADLINE, null);
  const v38 = stdlib.protect(ctc0, interact.firstHand, null);
  const v39 = stdlib.protect(ctc0, interact.wager, null);
  const v45 = stdlib.protect(ctc0, await interact.random(), {
    at: 'reach standard library:60:31:application',
    fs: ['at ./tut1.rsh:49:72:application call to "makeCommitment" (defined at: reach standard library:59:8:function exp)', 'at ./tut1.rsh:45:13:application call to [unknown function] (defined at: ./tut1.rsh:45:17:function exp)'],
    msg: 'random',
    who: 'Alice'
     });
  const v46 = stdlib.digest(ctc1, [v45, v38]);
  const txn1 = await (ctc.sendrecv('Alice', 1, 3, stdlib.checkedBigNumberify('./tut1.rsh:53:9:dot', stdlib.UInt_max, 0), [ctc0, ctc0, ctc0, ctc2], [v41, v39, v37, v46], v39, [ctc0, ctc0, ctc2], true, true, false, (async (txn1) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.digest(ctc1, [stdlib.checkedBigNumberify('./tut1.rsh:53:9:dot', stdlib.UInt_max, 0), v41]);
    sim_r.prevSt_noPrevTime = stdlib.digest(ctc15, [stdlib.checkedBigNumberify('./tut1.rsh:53:9:dot', stdlib.UInt_max, 0)]);
    const [v49, v50, v51] = txn1.data;
    const v52 = txn1.value;
    const v56 = txn1.time;
    const v48 = txn1.from;
    
    const v53 = stdlib.eq(v52, v49);
    stdlib.assert(v53, {
      at: './tut1.rsh:53:9:dot',
      fs: [],
      msg: 'pay amount correct',
      who: 'Alice'
       });
    stdlib.assert(true, {
      at: './tut1.rsh:53:9:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Alice'
       });
    const v55 = stdlib.add(stdlib.checkedBigNumberify('./tut1.rsh:compileDApp', stdlib.UInt_max, 0), v52);
    sim_r.nextSt = stdlib.digest(ctc13, [stdlib.checkedBigNumberify('./tut1.rsh:55:15:after expr stmt semicolon', stdlib.UInt_max, 1), v48, v49, v50, v51, v55, v56]);
    sim_r.nextSt_noTime = stdlib.digest(ctc14, [stdlib.checkedBigNumberify('./tut1.rsh:55:15:after expr stmt semicolon', stdlib.UInt_max, 1), v48, v49, v50, v51, v55]);
    sim_r.isHalt = false;
    
    return sim_r;
     })));
  const [v49, v50, v51] = txn1.data;
  const v52 = txn1.value;
  const v56 = txn1.time;
  const v48 = txn1.from;
  const v53 = stdlib.eq(v52, v49);
  stdlib.assert(v53, {
    at: './tut1.rsh:53:9:dot',
    fs: [],
    msg: 'pay amount correct',
    who: 'Alice'
     });
  stdlib.assert(true, {
    at: './tut1.rsh:53:9:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Alice'
     });
  const v55 = stdlib.add(stdlib.checkedBigNumberify('./tut1.rsh:compileDApp', stdlib.UInt_max, 0), v52);
  const txn2 = await (ctc.recv('Alice', 2, 1, [ctc0], false, v50));
  if (txn2.didTimeout) {
    const txn3 = await (ctc.sendrecv('Alice', 3, 0, stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 5), [ctc4, ctc0, ctc0, ctc2, ctc0, ctc0], [v48, v49, v50, v51, v55, v56], stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), [], true, true, false, (async (txn3) => {
      const sim_r = { txns: [] };
      sim_r.prevSt = stdlib.digest(ctc13, [stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 1), v48, v49, v50, v51, v55, v56]);
      sim_r.prevSt_noPrevTime = stdlib.digest(ctc14, [stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 1), v48, v49, v50, v51, v55]);
      const [] = txn3.data;
      const v241 = txn3.value;
      const v246 = txn3.time;
      const v240 = txn3.from;
      
      const v242 = stdlib.eq(v241, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
      stdlib.assert(v242, {
        at: 'reach standard library:68:7:dot',
        fs: ['at ./tut1.rsh:64:41:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
        msg: 'pay amount correct',
        who: 'Alice'
         });
      const v243 = stdlib.addressEq(v48, v240);
      stdlib.assert(v243, {
        at: 'reach standard library:68:7:dot',
        fs: ['at ./tut1.rsh:64:41:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
        msg: 'sender correct',
        who: 'Alice'
         });
      const v245 = stdlib.add(v55, v241);
      sim_r.txns.push({
        amt: v245,
        to: v48
         });
      sim_r.nextSt = stdlib.digest(ctc7, []);
      sim_r.nextSt_noTime = stdlib.digest(ctc7, []);
      sim_r.isHalt = true;
      
      return sim_r;
       })));
    const [] = txn3.data;
    const v241 = txn3.value;
    const v246 = txn3.time;
    const v240 = txn3.from;
    const v242 = stdlib.eq(v241, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
    stdlib.assert(v242, {
      at: 'reach standard library:68:7:dot',
      fs: ['at ./tut1.rsh:64:41:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
      msg: 'pay amount correct',
      who: 'Alice'
       });
    const v243 = stdlib.addressEq(v48, v240);
    stdlib.assert(v243, {
      at: 'reach standard library:68:7:dot',
      fs: ['at ./tut1.rsh:64:41:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
      msg: 'sender correct',
      who: 'Alice'
       });
    const v245 = stdlib.add(v55, v241);
    ;
    stdlib.protect(ctc3, await interact.informTimeout(), {
      at: './tut1.rsh:43:33:application',
      fs: ['at ./tut1.rsh:42:13:application call to [unknown function] (defined at: ./tut1.rsh:42:25:function exp)', 'at reach standard library:71:8:application call to "after" (defined at: ./tut1.rsh:41:32:function exp)', 'at ./tut1.rsh:64:41:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
      msg: 'informTimeout',
      who: 'Alice'
       });
    return;
     }
  else {
    const [v60] = txn2.data;
    const v61 = txn2.value;
    const v65 = txn2.time;
    const v59 = txn2.from;
    const v62 = stdlib.eq(v61, v49);
    stdlib.assert(v62, {
      at: './tut1.rsh:62:9:dot',
      fs: [],
      msg: 'pay amount correct',
      who: 'Alice'
       });
    stdlib.assert(true, {
      at: './tut1.rsh:62:9:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Alice'
       });
    const v64 = stdlib.add(v55, v61);
    const txn3 = await (ctc.sendrecv('Alice', 4, 2, stdlib.checkedBigNumberify('./tut1.rsh:70:9:dot', stdlib.UInt_max, 7), [ctc4, ctc0, ctc0, ctc2, ctc4, ctc0, ctc0, ctc0, ctc0, ctc0], [v48, v49, v50, v51, v59, v60, v64, v65, v45, v38], stdlib.checkedBigNumberify('./tut1.rsh:decimal', stdlib.UInt_max, 0), [ctc0, ctc0], true, true, v50, (async (txn3) => {
      const sim_r = { txns: [] };
      sim_r.prevSt = stdlib.digest(ctc11, [stdlib.checkedBigNumberify('./tut1.rsh:70:9:dot', stdlib.UInt_max, 2), v48, v49, v50, v51, v59, v60, v64, v65]);
      sim_r.prevSt_noPrevTime = stdlib.digest(ctc12, [stdlib.checkedBigNumberify('./tut1.rsh:70:9:dot', stdlib.UInt_max, 2), v48, v49, v50, v51, v59, v60, v64]);
      const [v69, v70] = txn3.data;
      const v71 = txn3.value;
      const v76 = txn3.time;
      const v68 = txn3.from;
      
      const v72 = stdlib.eq(v71, stdlib.checkedBigNumberify('./tut1.rsh:decimal', stdlib.UInt_max, 0));
      stdlib.assert(v72, {
        at: './tut1.rsh:70:9:dot',
        fs: [],
        msg: 'pay amount correct',
        who: 'Alice'
         });
      const v73 = stdlib.addressEq(v48, v68);
      stdlib.assert(v73, {
        at: './tut1.rsh:70:9:dot',
        fs: [],
        msg: 'sender correct',
        who: 'Alice'
         });
      const v75 = stdlib.add(v64, v71);
      const v78 = stdlib.digest(ctc1, [v69, v70]);
      const v79 = stdlib.eq(v51, v78);
      stdlib.assert(v79, {
        at: 'reach standard library:65:17:application',
        fs: ['at ./tut1.rsh:72:22:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)'],
        msg: null,
        who: 'Alice'
         });
      const v82 = stdlib.sub(stdlib.checkedBigNumberify('./tut1.rsh:7:18:decimal', stdlib.UInt_max, 4), v60);
      const v83 = stdlib.add(v70, v82);
      const v84 = stdlib.mod(v83, stdlib.checkedBigNumberify('./tut1.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v86 = v84;
      const v259 = v75;
      const v260 = v76;
      
      if ((() => {
        const v96 = stdlib.eq(v86, stdlib.checkedBigNumberify('./tut1.rsh:makeEnum', stdlib.UInt_max, 1));
        
        return v96; })()) {
        sim_r.nextSt = stdlib.digest(ctc5, [stdlib.checkedBigNumberify('./tut1.rsh:77:17:after expr stmt semicolon', stdlib.UInt_max, 6), v48, v49, v50, v59, v259, v260]);
        sim_r.nextSt_noTime = stdlib.digest(ctc6, [stdlib.checkedBigNumberify('./tut1.rsh:77:17:after expr stmt semicolon', stdlib.UInt_max, 6), v48, v49, v50, v59, v259]);
        sim_r.isHalt = false;
         }
      else {
        const v203 = stdlib.eq(v86, stdlib.checkedBigNumberify('./tut1.rsh:makeEnum', stdlib.UInt_max, 2));
        const v206 = stdlib.mul(stdlib.checkedBigNumberify('./tut1.rsh:104:16:decimal', stdlib.UInt_max, 2), v49);
        const v208 = v203 ? v48 : v59;
        sim_r.txns.push({
          amt: v206,
          to: v208
           });
        sim_r.nextSt = stdlib.digest(ctc7, []);
        sim_r.nextSt_noTime = stdlib.digest(ctc7, []);
        sim_r.isHalt = true;
         }
      return sim_r;
       })));
    if (txn3.didTimeout) {
      const txn4 = await (ctc.recv('Alice', 5, 0, [], false, false));
      const [] = txn4.data;
      const v221 = txn4.value;
      const v226 = txn4.time;
      const v220 = txn4.from;
      const v222 = stdlib.eq(v221, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
      stdlib.assert(v222, {
        at: 'reach standard library:68:7:dot',
        fs: ['at ./tut1.rsh:71:40:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
        msg: 'pay amount correct',
        who: 'Alice'
         });
      const v223 = stdlib.addressEq(v59, v220);
      stdlib.assert(v223, {
        at: 'reach standard library:68:7:dot',
        fs: ['at ./tut1.rsh:71:40:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
        msg: 'sender correct',
        who: 'Alice'
         });
      const v225 = stdlib.add(v64, v221);
      ;
      stdlib.protect(ctc3, await interact.informTimeout(), {
        at: './tut1.rsh:43:33:application',
        fs: ['at ./tut1.rsh:42:13:application call to [unknown function] (defined at: ./tut1.rsh:42:25:function exp)', 'at reach standard library:71:8:application call to "after" (defined at: ./tut1.rsh:41:32:function exp)', 'at ./tut1.rsh:71:40:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
        msg: 'informTimeout',
        who: 'Alice'
         });
      return;
       }
    else {
      const [v69, v70] = txn3.data;
      const v71 = txn3.value;
      const v76 = txn3.time;
      const v68 = txn3.from;
      const v72 = stdlib.eq(v71, stdlib.checkedBigNumberify('./tut1.rsh:decimal', stdlib.UInt_max, 0));
      stdlib.assert(v72, {
        at: './tut1.rsh:70:9:dot',
        fs: [],
        msg: 'pay amount correct',
        who: 'Alice'
         });
      const v73 = stdlib.addressEq(v48, v68);
      stdlib.assert(v73, {
        at: './tut1.rsh:70:9:dot',
        fs: [],
        msg: 'sender correct',
        who: 'Alice'
         });
      const v75 = stdlib.add(v64, v71);
      const v78 = stdlib.digest(ctc1, [v69, v70]);
      const v79 = stdlib.eq(v51, v78);
      stdlib.assert(v79, {
        at: 'reach standard library:65:17:application',
        fs: ['at ./tut1.rsh:72:22:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)'],
        msg: null,
        who: 'Alice'
         });
      const v82 = stdlib.sub(stdlib.checkedBigNumberify('./tut1.rsh:7:18:decimal', stdlib.UInt_max, 4), v60);
      const v83 = stdlib.add(v70, v82);
      const v84 = stdlib.mod(v83, stdlib.checkedBigNumberify('./tut1.rsh:7:32:decimal', stdlib.UInt_max, 3));
      let v86 = v84;
      let v259 = v75;
      let v260 = v76;
      
      while ((() => {
        const v96 = stdlib.eq(v86, stdlib.checkedBigNumberify('./tut1.rsh:makeEnum', stdlib.UInt_max, 1));
        
        return v96; })()) {
        const v99 = stdlib.protect(ctc0, await interact.getHand(), {
          at: './tut1.rsh:80:42:application',
          fs: ['at ./tut1.rsh:79:15:application call to [unknown function] (defined at: ./tut1.rsh:79:19:function exp)'],
          msg: 'getHand',
          who: 'Alice'
           });
        const v101 = stdlib.protect(ctc0, await interact.random(), {
          at: 'reach standard library:60:31:application',
          fs: ['at ./tut1.rsh:81:52:application call to "makeCommitment" (defined at: reach standard library:59:8:function exp)', 'at ./tut1.rsh:79:15:application call to [unknown function] (defined at: ./tut1.rsh:79:19:function exp)'],
          msg: 'random',
          who: 'Alice'
           });
        const v102 = stdlib.digest(ctc1, [v101, v99]);
        const txn4 = await (ctc.sendrecv('Alice', 8, 1, stdlib.checkedBigNumberify('./tut1.rsh:83:11:dot', stdlib.UInt_max, 5), [ctc4, ctc0, ctc0, ctc4, ctc0, ctc0, ctc2], [v48, v49, v50, v59, v259, v260, v102], stdlib.checkedBigNumberify('./tut1.rsh:decimal', stdlib.UInt_max, 0), [ctc2], true, true, v50, (async (txn4) => {
          const sim_r = { txns: [] };
          sim_r.prevSt = stdlib.digest(ctc5, [stdlib.checkedBigNumberify('./tut1.rsh:83:11:dot', stdlib.UInt_max, 6), v48, v49, v50, v59, v259, v260]);
          sim_r.prevSt_noPrevTime = stdlib.digest(ctc6, [stdlib.checkedBigNumberify('./tut1.rsh:83:11:dot', stdlib.UInt_max, 6), v48, v49, v50, v59, v259]);
          const [v105] = txn4.data;
          const v106 = txn4.value;
          const v111 = txn4.time;
          const v104 = txn4.from;
          
          const v107 = stdlib.eq(v106, stdlib.checkedBigNumberify('./tut1.rsh:decimal', stdlib.UInt_max, 0));
          stdlib.assert(v107, {
            at: './tut1.rsh:83:11:dot',
            fs: [],
            msg: 'pay amount correct',
            who: 'Alice'
             });
          const v108 = stdlib.addressEq(v48, v104);
          stdlib.assert(v108, {
            at: './tut1.rsh:83:11:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Alice'
             });
          const v110 = stdlib.add(v259, v106);
          sim_r.nextSt = stdlib.digest(ctc9, [stdlib.checkedBigNumberify('./tut1.rsh:85:17:after expr stmt semicolon', stdlib.UInt_max, 8), v48, v49, v50, v59, v105, v110, v111]);
          sim_r.nextSt_noTime = stdlib.digest(ctc10, [stdlib.checkedBigNumberify('./tut1.rsh:85:17:after expr stmt semicolon', stdlib.UInt_max, 8), v48, v49, v50, v59, v105, v110]);
          sim_r.isHalt = false;
          
          return sim_r;
           })));
        if (txn4.didTimeout) {
          const txn5 = await (ctc.recv('Alice', 9, 0, [], false, false));
          const [] = txn5.data;
          const v185 = txn5.value;
          const v190 = txn5.time;
          const v184 = txn5.from;
          const v186 = stdlib.eq(v185, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
          stdlib.assert(v186, {
            at: 'reach standard library:68:7:dot',
            fs: ['at ./tut1.rsh:84:43:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
            msg: 'pay amount correct',
            who: 'Alice'
             });
          const v187 = stdlib.addressEq(v59, v184);
          stdlib.assert(v187, {
            at: 'reach standard library:68:7:dot',
            fs: ['at ./tut1.rsh:84:43:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
            msg: 'sender correct',
            who: 'Alice'
             });
          const v189 = stdlib.add(v259, v185);
          ;
          stdlib.protect(ctc3, await interact.informTimeout(), {
            at: './tut1.rsh:43:33:application',
            fs: ['at ./tut1.rsh:42:13:application call to [unknown function] (defined at: ./tut1.rsh:42:25:function exp)', 'at reach standard library:71:8:application call to "after" (defined at: ./tut1.rsh:41:32:function exp)', 'at ./tut1.rsh:84:43:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
            msg: 'informTimeout',
            who: 'Alice'
             });
          return;
           }
        else {
          const [v105] = txn4.data;
          const v106 = txn4.value;
          const v111 = txn4.time;
          const v104 = txn4.from;
          const v107 = stdlib.eq(v106, stdlib.checkedBigNumberify('./tut1.rsh:decimal', stdlib.UInt_max, 0));
          stdlib.assert(v107, {
            at: './tut1.rsh:83:11:dot',
            fs: [],
            msg: 'pay amount correct',
            who: 'Alice'
             });
          const v108 = stdlib.addressEq(v48, v104);
          stdlib.assert(v108, {
            at: './tut1.rsh:83:11:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Alice'
             });
          const v110 = stdlib.add(v259, v106);
          const txn5 = await (ctc.recv('Alice', 10, 1, [ctc0], false, v50));
          if (txn5.didTimeout) {
            const txn6 = await (ctc.sendrecv('Alice', 11, 0, stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 6), [ctc4, ctc0, ctc0, ctc4, ctc2, ctc0, ctc0], [v48, v49, v50, v59, v105, v110, v111], stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), [], true, true, false, (async (txn6) => {
              const sim_r = { txns: [] };
              sim_r.prevSt = stdlib.digest(ctc9, [stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 8), v48, v49, v50, v59, v105, v110, v111]);
              sim_r.prevSt_noPrevTime = stdlib.digest(ctc10, [stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 8), v48, v49, v50, v59, v105, v110]);
              const [] = txn6.data;
              const v165 = txn6.value;
              const v170 = txn6.time;
              const v164 = txn6.from;
              
              const v166 = stdlib.eq(v165, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
              stdlib.assert(v166, {
                at: 'reach standard library:68:7:dot',
                fs: ['at ./tut1.rsh:91:43:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
                msg: 'pay amount correct',
                who: 'Alice'
                 });
              const v167 = stdlib.addressEq(v48, v164);
              stdlib.assert(v167, {
                at: 'reach standard library:68:7:dot',
                fs: ['at ./tut1.rsh:91:43:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
                msg: 'sender correct',
                who: 'Alice'
                 });
              const v169 = stdlib.add(v110, v165);
              sim_r.txns.push({
                amt: v169,
                to: v48
                 });
              sim_r.nextSt = stdlib.digest(ctc7, []);
              sim_r.nextSt_noTime = stdlib.digest(ctc7, []);
              sim_r.isHalt = true;
              
              return sim_r;
               })));
            const [] = txn6.data;
            const v165 = txn6.value;
            const v170 = txn6.time;
            const v164 = txn6.from;
            const v166 = stdlib.eq(v165, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v166, {
              at: 'reach standard library:68:7:dot',
              fs: ['at ./tut1.rsh:91:43:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
              msg: 'pay amount correct',
              who: 'Alice'
               });
            const v167 = stdlib.addressEq(v48, v164);
            stdlib.assert(v167, {
              at: 'reach standard library:68:7:dot',
              fs: ['at ./tut1.rsh:91:43:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
              msg: 'sender correct',
              who: 'Alice'
               });
            const v169 = stdlib.add(v110, v165);
            ;
            stdlib.protect(ctc3, await interact.informTimeout(), {
              at: './tut1.rsh:43:33:application',
              fs: ['at ./tut1.rsh:42:13:application call to [unknown function] (defined at: ./tut1.rsh:42:25:function exp)', 'at reach standard library:71:8:application call to "after" (defined at: ./tut1.rsh:41:32:function exp)', 'at ./tut1.rsh:91:43:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
              msg: 'informTimeout',
              who: 'Alice'
               });
            return;
             }
          else {
            const [v116] = txn5.data;
            const v117 = txn5.value;
            const v122 = txn5.time;
            const v115 = txn5.from;
            const v118 = stdlib.eq(v117, stdlib.checkedBigNumberify('./tut1.rsh:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v118, {
              at: './tut1.rsh:90:11:dot',
              fs: [],
              msg: 'pay amount correct',
              who: 'Alice'
               });
            const v119 = stdlib.addressEq(v59, v115);
            stdlib.assert(v119, {
              at: './tut1.rsh:90:11:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Alice'
               });
            const v121 = stdlib.add(v110, v117);
            const txn6 = await (ctc.sendrecv('Alice', 12, 2, stdlib.checkedBigNumberify('./tut1.rsh:96:11:dot', stdlib.UInt_max, 7), [ctc4, ctc0, ctc0, ctc4, ctc2, ctc0, ctc0, ctc0, ctc0, ctc0], [v48, v49, v50, v59, v105, v116, v121, v122, v101, v99], stdlib.checkedBigNumberify('./tut1.rsh:decimal', stdlib.UInt_max, 0), [ctc0, ctc0], true, true, v50, (async (txn6) => {
              const sim_r = { txns: [] };
              sim_r.prevSt = stdlib.digest(ctc8, [stdlib.checkedBigNumberify('./tut1.rsh:96:11:dot', stdlib.UInt_max, 10), v48, v49, v50, v59, v105, v116, v121, v122]);
              sim_r.prevSt_noPrevTime = stdlib.digest(ctc9, [stdlib.checkedBigNumberify('./tut1.rsh:96:11:dot', stdlib.UInt_max, 10), v48, v49, v50, v59, v105, v116, v121]);
              const [v126, v127] = txn6.data;
              const v128 = txn6.value;
              const v133 = txn6.time;
              const v125 = txn6.from;
              
              const v129 = stdlib.eq(v128, stdlib.checkedBigNumberify('./tut1.rsh:decimal', stdlib.UInt_max, 0));
              stdlib.assert(v129, {
                at: './tut1.rsh:96:11:dot',
                fs: [],
                msg: 'pay amount correct',
                who: 'Alice'
                 });
              const v130 = stdlib.addressEq(v48, v125);
              stdlib.assert(v130, {
                at: './tut1.rsh:96:11:dot',
                fs: [],
                msg: 'sender correct',
                who: 'Alice'
                 });
              const v132 = stdlib.add(v121, v128);
              const v135 = stdlib.digest(ctc1, [v126, v127]);
              const v136 = stdlib.eq(v105, v135);
              stdlib.assert(v136, {
                at: 'reach standard library:65:17:application',
                fs: ['at ./tut1.rsh:98:24:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)'],
                msg: null,
                who: 'Alice'
                 });
              const v139 = stdlib.sub(stdlib.checkedBigNumberify('./tut1.rsh:7:18:decimal', stdlib.UInt_max, 4), v116);
              const v140 = stdlib.add(v127, v139);
              const v141 = stdlib.mod(v140, stdlib.checkedBigNumberify('./tut1.rsh:7:32:decimal', stdlib.UInt_max, 3));
              const cv86 = v141;
              const cv259 = v132;
              const cv260 = v133;
              
              (() => {
                const v86 = cv86;
                const v259 = cv259;
                const v260 = cv260;
                
                if ((() => {
                  const v96 = stdlib.eq(v86, stdlib.checkedBigNumberify('./tut1.rsh:makeEnum', stdlib.UInt_max, 1));
                  
                  return v96; })()) {
                  sim_r.nextSt = stdlib.digest(ctc5, [stdlib.checkedBigNumberify('./tut1.rsh:77:17:after expr stmt semicolon', stdlib.UInt_max, 6), v48, v49, v50, v59, v259, v260]);
                  sim_r.nextSt_noTime = stdlib.digest(ctc6, [stdlib.checkedBigNumberify('./tut1.rsh:77:17:after expr stmt semicolon', stdlib.UInt_max, 6), v48, v49, v50, v59, v259]);
                  sim_r.isHalt = false;
                   }
                else {
                  const v203 = stdlib.eq(v86, stdlib.checkedBigNumberify('./tut1.rsh:makeEnum', stdlib.UInt_max, 2));
                  const v206 = stdlib.mul(stdlib.checkedBigNumberify('./tut1.rsh:104:16:decimal', stdlib.UInt_max, 2), v49);
                  const v208 = v203 ? v48 : v59;
                  sim_r.txns.push({
                    amt: v206,
                    to: v208
                     });
                  sim_r.nextSt = stdlib.digest(ctc7, []);
                  sim_r.nextSt_noTime = stdlib.digest(ctc7, []);
                  sim_r.isHalt = true;
                   } })();
              return sim_r;
               })));
            if (txn6.didTimeout) {
              const txn7 = await (ctc.recv('Alice', 13, 0, [], false, false));
              const [] = txn7.data;
              const v145 = txn7.value;
              const v150 = txn7.time;
              const v144 = txn7.from;
              const v146 = stdlib.eq(v145, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
              stdlib.assert(v146, {
                at: 'reach standard library:68:7:dot',
                fs: ['at ./tut1.rsh:97:43:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
                msg: 'pay amount correct',
                who: 'Alice'
                 });
              const v147 = stdlib.addressEq(v59, v144);
              stdlib.assert(v147, {
                at: 'reach standard library:68:7:dot',
                fs: ['at ./tut1.rsh:97:43:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
                msg: 'sender correct',
                who: 'Alice'
                 });
              const v149 = stdlib.add(v121, v145);
              ;
              stdlib.protect(ctc3, await interact.informTimeout(), {
                at: './tut1.rsh:43:33:application',
                fs: ['at ./tut1.rsh:42:13:application call to [unknown function] (defined at: ./tut1.rsh:42:25:function exp)', 'at reach standard library:71:8:application call to "after" (defined at: ./tut1.rsh:41:32:function exp)', 'at ./tut1.rsh:97:43:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
                msg: 'informTimeout',
                who: 'Alice'
                 });
              return;
               }
            else {
              const [v126, v127] = txn6.data;
              const v128 = txn6.value;
              const v133 = txn6.time;
              const v125 = txn6.from;
              const v129 = stdlib.eq(v128, stdlib.checkedBigNumberify('./tut1.rsh:decimal', stdlib.UInt_max, 0));
              stdlib.assert(v129, {
                at: './tut1.rsh:96:11:dot',
                fs: [],
                msg: 'pay amount correct',
                who: 'Alice'
                 });
              const v130 = stdlib.addressEq(v48, v125);
              stdlib.assert(v130, {
                at: './tut1.rsh:96:11:dot',
                fs: [],
                msg: 'sender correct',
                who: 'Alice'
                 });
              const v132 = stdlib.add(v121, v128);
              const v135 = stdlib.digest(ctc1, [v126, v127]);
              const v136 = stdlib.eq(v105, v135);
              stdlib.assert(v136, {
                at: 'reach standard library:65:17:application',
                fs: ['at ./tut1.rsh:98:24:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)'],
                msg: null,
                who: 'Alice'
                 });
              const v139 = stdlib.sub(stdlib.checkedBigNumberify('./tut1.rsh:7:18:decimal', stdlib.UInt_max, 4), v116);
              const v140 = stdlib.add(v127, v139);
              const v141 = stdlib.mod(v140, stdlib.checkedBigNumberify('./tut1.rsh:7:32:decimal', stdlib.UInt_max, 3));
              const cv86 = v141;
              const cv259 = v132;
              const cv260 = v133;
              
              v86 = cv86;
              v259 = cv259;
              v260 = cv260;
              
              continue; }
             }
           }
         }
      const v203 = stdlib.eq(v86, stdlib.checkedBigNumberify('./tut1.rsh:makeEnum', stdlib.UInt_max, 2));
      const v206 = stdlib.mul(stdlib.checkedBigNumberify('./tut1.rsh:104:16:decimal', stdlib.UInt_max, 2), v49);
      const v208 = v203 ? v48 : v59;
      ;
      stdlib.protect(ctc3, await interact.seeOutcome(v86), {
        at: './tut1.rsh:108:28:application',
        fs: ['at ./tut1.rsh:107:11:application call to [unknown function] (defined at: ./tut1.rsh:107:23:function exp)'],
        msg: 'seeOutcome',
        who: 'Alice'
         });
      return; }
     }
  
  
   }
export async function Bob(ctc, interact) {
  const stdlib = ctc.stdlib;
  const ctc0 = stdlib.T_UInt;
  const ctc1 = stdlib.T_Digest;
  const ctc2 = stdlib.T_Null;
  const ctc3 = stdlib.T_Tuple([ctc0, ctc0]);
  const ctc4 = stdlib.T_Tuple([]);
  const ctc5 = stdlib.T_Address;
  const ctc6 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc0, ctc5, ctc1, ctc0, ctc0, ctc0]);
  const ctc7 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc0, ctc5, ctc1, ctc0, ctc0]);
  const ctc8 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc0, ctc5, ctc1, ctc0]);
  const ctc9 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc0, ctc5, ctc0, ctc0]);
  const ctc10 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc0, ctc5, ctc0]);
  const ctc11 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc0, ctc1, ctc5, ctc0, ctc0, ctc0]);
  const ctc12 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc0, ctc1, ctc5, ctc0, ctc0]);
  const ctc13 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc0, ctc1, ctc0, ctc0]);
  const ctc14 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc0, ctc1, ctc0]);
  
  
  const v41 = await ctc.creationTime();
  const v40 = stdlib.protect(ctc0, interact.firstHand, null);
  const txn1 = await (ctc.recv('Bob', 1, 3, [ctc0, ctc0, ctc1], false, false));
  const [v49, v50, v51] = txn1.data;
  const v52 = txn1.value;
  const v56 = txn1.time;
  const v48 = txn1.from;
  const v53 = stdlib.eq(v52, v49);
  stdlib.assert(v53, {
    at: './tut1.rsh:53:9:dot',
    fs: [],
    msg: 'pay amount correct',
    who: 'Bob'
     });
  stdlib.assert(true, {
    at: './tut1.rsh:53:9:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Bob'
     });
  const v55 = stdlib.add(stdlib.checkedBigNumberify('./tut1.rsh:compileDApp', stdlib.UInt_max, 0), v52);
  stdlib.protect(ctc2, await interact.acceptWager(v49), {
    at: './tut1.rsh:59:29:application',
    fs: ['at ./tut1.rsh:58:13:application call to [unknown function] (defined at: ./tut1.rsh:58:17:function exp)'],
    msg: 'acceptWager',
    who: 'Bob'
     });
  const txn2 = await (ctc.sendrecv('Bob', 2, 1, stdlib.checkedBigNumberify('./tut1.rsh:62:9:dot', stdlib.UInt_max, 5), [ctc5, ctc0, ctc0, ctc1, ctc0, ctc0, ctc0], [v48, v49, v50, v51, v55, v56, v40], v49, [ctc0], true, true, v50, (async (txn2) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.digest(ctc13, [stdlib.checkedBigNumberify('./tut1.rsh:62:9:dot', stdlib.UInt_max, 1), v48, v49, v50, v51, v55, v56]);
    sim_r.prevSt_noPrevTime = stdlib.digest(ctc14, [stdlib.checkedBigNumberify('./tut1.rsh:62:9:dot', stdlib.UInt_max, 1), v48, v49, v50, v51, v55]);
    const [v60] = txn2.data;
    const v61 = txn2.value;
    const v65 = txn2.time;
    const v59 = txn2.from;
    
    const v62 = stdlib.eq(v61, v49);
    stdlib.assert(v62, {
      at: './tut1.rsh:62:9:dot',
      fs: [],
      msg: 'pay amount correct',
      who: 'Bob'
       });
    stdlib.assert(true, {
      at: './tut1.rsh:62:9:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Bob'
       });
    const v64 = stdlib.add(v55, v61);
    sim_r.nextSt = stdlib.digest(ctc11, [stdlib.checkedBigNumberify('./tut1.rsh:65:15:after expr stmt semicolon', stdlib.UInt_max, 2), v48, v49, v50, v51, v59, v60, v64, v65]);
    sim_r.nextSt_noTime = stdlib.digest(ctc12, [stdlib.checkedBigNumberify('./tut1.rsh:65:15:after expr stmt semicolon', stdlib.UInt_max, 2), v48, v49, v50, v51, v59, v60, v64]);
    sim_r.isHalt = false;
    
    return sim_r;
     })));
  if (txn2.didTimeout) {
    const txn3 = await (ctc.recv('Bob', 3, 0, [], false, false));
    const [] = txn3.data;
    const v241 = txn3.value;
    const v246 = txn3.time;
    const v240 = txn3.from;
    const v242 = stdlib.eq(v241, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
    stdlib.assert(v242, {
      at: 'reach standard library:68:7:dot',
      fs: ['at ./tut1.rsh:64:41:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
      msg: 'pay amount correct',
      who: 'Bob'
       });
    const v243 = stdlib.addressEq(v48, v240);
    stdlib.assert(v243, {
      at: 'reach standard library:68:7:dot',
      fs: ['at ./tut1.rsh:64:41:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
      msg: 'sender correct',
      who: 'Bob'
       });
    const v245 = stdlib.add(v55, v241);
    ;
    stdlib.protect(ctc2, await interact.informTimeout(), {
      at: './tut1.rsh:43:33:application',
      fs: ['at ./tut1.rsh:42:13:application call to [unknown function] (defined at: ./tut1.rsh:42:25:function exp)', 'at reach standard library:71:8:application call to "after" (defined at: ./tut1.rsh:41:32:function exp)', 'at ./tut1.rsh:64:41:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
      msg: 'informTimeout',
      who: 'Bob'
       });
    return;
     }
  else {
    const [v60] = txn2.data;
    const v61 = txn2.value;
    const v65 = txn2.time;
    const v59 = txn2.from;
    const v62 = stdlib.eq(v61, v49);
    stdlib.assert(v62, {
      at: './tut1.rsh:62:9:dot',
      fs: [],
      msg: 'pay amount correct',
      who: 'Bob'
       });
    stdlib.assert(true, {
      at: './tut1.rsh:62:9:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Bob'
       });
    const v64 = stdlib.add(v55, v61);
    const txn3 = await (ctc.recv('Bob', 4, 2, [ctc0, ctc0], false, v50));
    if (txn3.didTimeout) {
      const txn4 = await (ctc.sendrecv('Bob', 5, 0, stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 7), [ctc5, ctc0, ctc0, ctc1, ctc5, ctc0, ctc0, ctc0], [v48, v49, v50, v51, v59, v60, v64, v65], stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), [], true, true, false, (async (txn4) => {
        const sim_r = { txns: [] };
        sim_r.prevSt = stdlib.digest(ctc11, [stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 2), v48, v49, v50, v51, v59, v60, v64, v65]);
        sim_r.prevSt_noPrevTime = stdlib.digest(ctc12, [stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 2), v48, v49, v50, v51, v59, v60, v64]);
        const [] = txn4.data;
        const v221 = txn4.value;
        const v226 = txn4.time;
        const v220 = txn4.from;
        
        const v222 = stdlib.eq(v221, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
        stdlib.assert(v222, {
          at: 'reach standard library:68:7:dot',
          fs: ['at ./tut1.rsh:71:40:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
          msg: 'pay amount correct',
          who: 'Bob'
           });
        const v223 = stdlib.addressEq(v59, v220);
        stdlib.assert(v223, {
          at: 'reach standard library:68:7:dot',
          fs: ['at ./tut1.rsh:71:40:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
          msg: 'sender correct',
          who: 'Bob'
           });
        const v225 = stdlib.add(v64, v221);
        sim_r.txns.push({
          amt: v225,
          to: v59
           });
        sim_r.nextSt = stdlib.digest(ctc4, []);
        sim_r.nextSt_noTime = stdlib.digest(ctc4, []);
        sim_r.isHalt = true;
        
        return sim_r;
         })));
      const [] = txn4.data;
      const v221 = txn4.value;
      const v226 = txn4.time;
      const v220 = txn4.from;
      const v222 = stdlib.eq(v221, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
      stdlib.assert(v222, {
        at: 'reach standard library:68:7:dot',
        fs: ['at ./tut1.rsh:71:40:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
        msg: 'pay amount correct',
        who: 'Bob'
         });
      const v223 = stdlib.addressEq(v59, v220);
      stdlib.assert(v223, {
        at: 'reach standard library:68:7:dot',
        fs: ['at ./tut1.rsh:71:40:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
        msg: 'sender correct',
        who: 'Bob'
         });
      const v225 = stdlib.add(v64, v221);
      ;
      stdlib.protect(ctc2, await interact.informTimeout(), {
        at: './tut1.rsh:43:33:application',
        fs: ['at ./tut1.rsh:42:13:application call to [unknown function] (defined at: ./tut1.rsh:42:25:function exp)', 'at reach standard library:71:8:application call to "after" (defined at: ./tut1.rsh:41:32:function exp)', 'at ./tut1.rsh:71:40:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
        msg: 'informTimeout',
        who: 'Bob'
         });
      return;
       }
    else {
      const [v69, v70] = txn3.data;
      const v71 = txn3.value;
      const v76 = txn3.time;
      const v68 = txn3.from;
      const v72 = stdlib.eq(v71, stdlib.checkedBigNumberify('./tut1.rsh:decimal', stdlib.UInt_max, 0));
      stdlib.assert(v72, {
        at: './tut1.rsh:70:9:dot',
        fs: [],
        msg: 'pay amount correct',
        who: 'Bob'
         });
      const v73 = stdlib.addressEq(v48, v68);
      stdlib.assert(v73, {
        at: './tut1.rsh:70:9:dot',
        fs: [],
        msg: 'sender correct',
        who: 'Bob'
         });
      const v75 = stdlib.add(v64, v71);
      const v78 = stdlib.digest(ctc3, [v69, v70]);
      const v79 = stdlib.eq(v51, v78);
      stdlib.assert(v79, {
        at: 'reach standard library:65:17:application',
        fs: ['at ./tut1.rsh:72:22:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)'],
        msg: null,
        who: 'Bob'
         });
      const v82 = stdlib.sub(stdlib.checkedBigNumberify('./tut1.rsh:7:18:decimal', stdlib.UInt_max, 4), v60);
      const v83 = stdlib.add(v70, v82);
      const v84 = stdlib.mod(v83, stdlib.checkedBigNumberify('./tut1.rsh:7:32:decimal', stdlib.UInt_max, 3));
      let v86 = v84;
      let v259 = v75;
      let v260 = v76;
      
      while ((() => {
        const v96 = stdlib.eq(v86, stdlib.checkedBigNumberify('./tut1.rsh:makeEnum', stdlib.UInt_max, 1));
        
        return v96; })()) {
        const txn4 = await (ctc.recv('Bob', 8, 1, [ctc1], false, v50));
        if (txn4.didTimeout) {
          const txn5 = await (ctc.sendrecv('Bob', 9, 0, stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 5), [ctc5, ctc0, ctc0, ctc5, ctc0, ctc0], [v48, v49, v50, v59, v259, v260], stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), [], true, true, false, (async (txn5) => {
            const sim_r = { txns: [] };
            sim_r.prevSt = stdlib.digest(ctc9, [stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 6), v48, v49, v50, v59, v259, v260]);
            sim_r.prevSt_noPrevTime = stdlib.digest(ctc10, [stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 6), v48, v49, v50, v59, v259]);
            const [] = txn5.data;
            const v185 = txn5.value;
            const v190 = txn5.time;
            const v184 = txn5.from;
            
            const v186 = stdlib.eq(v185, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v186, {
              at: 'reach standard library:68:7:dot',
              fs: ['at ./tut1.rsh:84:43:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
              msg: 'pay amount correct',
              who: 'Bob'
               });
            const v187 = stdlib.addressEq(v59, v184);
            stdlib.assert(v187, {
              at: 'reach standard library:68:7:dot',
              fs: ['at ./tut1.rsh:84:43:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
              msg: 'sender correct',
              who: 'Bob'
               });
            const v189 = stdlib.add(v259, v185);
            sim_r.txns.push({
              amt: v189,
              to: v59
               });
            sim_r.nextSt = stdlib.digest(ctc4, []);
            sim_r.nextSt_noTime = stdlib.digest(ctc4, []);
            sim_r.isHalt = true;
            
            return sim_r;
             })));
          const [] = txn5.data;
          const v185 = txn5.value;
          const v190 = txn5.time;
          const v184 = txn5.from;
          const v186 = stdlib.eq(v185, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
          stdlib.assert(v186, {
            at: 'reach standard library:68:7:dot',
            fs: ['at ./tut1.rsh:84:43:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
            msg: 'pay amount correct',
            who: 'Bob'
             });
          const v187 = stdlib.addressEq(v59, v184);
          stdlib.assert(v187, {
            at: 'reach standard library:68:7:dot',
            fs: ['at ./tut1.rsh:84:43:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
            msg: 'sender correct',
            who: 'Bob'
             });
          const v189 = stdlib.add(v259, v185);
          ;
          stdlib.protect(ctc2, await interact.informTimeout(), {
            at: './tut1.rsh:43:33:application',
            fs: ['at ./tut1.rsh:42:13:application call to [unknown function] (defined at: ./tut1.rsh:42:25:function exp)', 'at reach standard library:71:8:application call to "after" (defined at: ./tut1.rsh:41:32:function exp)', 'at ./tut1.rsh:84:43:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
            msg: 'informTimeout',
            who: 'Bob'
             });
          return;
           }
        else {
          const [v105] = txn4.data;
          const v106 = txn4.value;
          const v111 = txn4.time;
          const v104 = txn4.from;
          const v107 = stdlib.eq(v106, stdlib.checkedBigNumberify('./tut1.rsh:decimal', stdlib.UInt_max, 0));
          stdlib.assert(v107, {
            at: './tut1.rsh:83:11:dot',
            fs: [],
            msg: 'pay amount correct',
            who: 'Bob'
             });
          const v108 = stdlib.addressEq(v48, v104);
          stdlib.assert(v108, {
            at: './tut1.rsh:83:11:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Bob'
             });
          const v110 = stdlib.add(v259, v106);
          const v114 = stdlib.protect(ctc0, await interact.getHand(), {
            at: './tut1.rsh:89:52:application',
            fs: ['at ./tut1.rsh:88:15:application call to [unknown function] (defined at: ./tut1.rsh:88:19:function exp)'],
            msg: 'getHand',
            who: 'Bob'
             });
          const txn5 = await (ctc.sendrecv('Bob', 10, 1, stdlib.checkedBigNumberify('./tut1.rsh:90:11:dot', stdlib.UInt_max, 6), [ctc5, ctc0, ctc0, ctc5, ctc1, ctc0, ctc0, ctc0], [v48, v49, v50, v59, v105, v110, v111, v114], stdlib.checkedBigNumberify('./tut1.rsh:decimal', stdlib.UInt_max, 0), [ctc0], true, true, v50, (async (txn5) => {
            const sim_r = { txns: [] };
            sim_r.prevSt = stdlib.digest(ctc7, [stdlib.checkedBigNumberify('./tut1.rsh:90:11:dot', stdlib.UInt_max, 8), v48, v49, v50, v59, v105, v110, v111]);
            sim_r.prevSt_noPrevTime = stdlib.digest(ctc8, [stdlib.checkedBigNumberify('./tut1.rsh:90:11:dot', stdlib.UInt_max, 8), v48, v49, v50, v59, v105, v110]);
            const [v116] = txn5.data;
            const v117 = txn5.value;
            const v122 = txn5.time;
            const v115 = txn5.from;
            
            const v118 = stdlib.eq(v117, stdlib.checkedBigNumberify('./tut1.rsh:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v118, {
              at: './tut1.rsh:90:11:dot',
              fs: [],
              msg: 'pay amount correct',
              who: 'Bob'
               });
            const v119 = stdlib.addressEq(v59, v115);
            stdlib.assert(v119, {
              at: './tut1.rsh:90:11:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Bob'
               });
            const v121 = stdlib.add(v110, v117);
            sim_r.nextSt = stdlib.digest(ctc6, [stdlib.checkedBigNumberify('./tut1.rsh:92:17:after expr stmt semicolon', stdlib.UInt_max, 10), v48, v49, v50, v59, v105, v116, v121, v122]);
            sim_r.nextSt_noTime = stdlib.digest(ctc7, [stdlib.checkedBigNumberify('./tut1.rsh:92:17:after expr stmt semicolon', stdlib.UInt_max, 10), v48, v49, v50, v59, v105, v116, v121]);
            sim_r.isHalt = false;
            
            return sim_r;
             })));
          if (txn5.didTimeout) {
            const txn6 = await (ctc.recv('Bob', 11, 0, [], false, false));
            const [] = txn6.data;
            const v165 = txn6.value;
            const v170 = txn6.time;
            const v164 = txn6.from;
            const v166 = stdlib.eq(v165, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v166, {
              at: 'reach standard library:68:7:dot',
              fs: ['at ./tut1.rsh:91:43:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
              msg: 'pay amount correct',
              who: 'Bob'
               });
            const v167 = stdlib.addressEq(v48, v164);
            stdlib.assert(v167, {
              at: 'reach standard library:68:7:dot',
              fs: ['at ./tut1.rsh:91:43:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
              msg: 'sender correct',
              who: 'Bob'
               });
            const v169 = stdlib.add(v110, v165);
            ;
            stdlib.protect(ctc2, await interact.informTimeout(), {
              at: './tut1.rsh:43:33:application',
              fs: ['at ./tut1.rsh:42:13:application call to [unknown function] (defined at: ./tut1.rsh:42:25:function exp)', 'at reach standard library:71:8:application call to "after" (defined at: ./tut1.rsh:41:32:function exp)', 'at ./tut1.rsh:91:43:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
              msg: 'informTimeout',
              who: 'Bob'
               });
            return;
             }
          else {
            const [v116] = txn5.data;
            const v117 = txn5.value;
            const v122 = txn5.time;
            const v115 = txn5.from;
            const v118 = stdlib.eq(v117, stdlib.checkedBigNumberify('./tut1.rsh:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v118, {
              at: './tut1.rsh:90:11:dot',
              fs: [],
              msg: 'pay amount correct',
              who: 'Bob'
               });
            const v119 = stdlib.addressEq(v59, v115);
            stdlib.assert(v119, {
              at: './tut1.rsh:90:11:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Bob'
               });
            const v121 = stdlib.add(v110, v117);
            const txn6 = await (ctc.recv('Bob', 12, 2, [ctc0, ctc0], false, v50));
            if (txn6.didTimeout) {
              const txn7 = await (ctc.sendrecv('Bob', 13, 0, stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 7), [ctc5, ctc0, ctc0, ctc5, ctc1, ctc0, ctc0, ctc0], [v48, v49, v50, v59, v105, v116, v121, v122], stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), [], true, true, false, (async (txn7) => {
                const sim_r = { txns: [] };
                sim_r.prevSt = stdlib.digest(ctc6, [stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 10), v48, v49, v50, v59, v105, v116, v121, v122]);
                sim_r.prevSt_noPrevTime = stdlib.digest(ctc7, [stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 10), v48, v49, v50, v59, v105, v116, v121]);
                const [] = txn7.data;
                const v145 = txn7.value;
                const v150 = txn7.time;
                const v144 = txn7.from;
                
                const v146 = stdlib.eq(v145, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
                stdlib.assert(v146, {
                  at: 'reach standard library:68:7:dot',
                  fs: ['at ./tut1.rsh:97:43:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
                  msg: 'pay amount correct',
                  who: 'Bob'
                   });
                const v147 = stdlib.addressEq(v59, v144);
                stdlib.assert(v147, {
                  at: 'reach standard library:68:7:dot',
                  fs: ['at ./tut1.rsh:97:43:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
                  msg: 'sender correct',
                  who: 'Bob'
                   });
                const v149 = stdlib.add(v121, v145);
                sim_r.txns.push({
                  amt: v149,
                  to: v59
                   });
                sim_r.nextSt = stdlib.digest(ctc4, []);
                sim_r.nextSt_noTime = stdlib.digest(ctc4, []);
                sim_r.isHalt = true;
                
                return sim_r;
                 })));
              const [] = txn7.data;
              const v145 = txn7.value;
              const v150 = txn7.time;
              const v144 = txn7.from;
              const v146 = stdlib.eq(v145, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
              stdlib.assert(v146, {
                at: 'reach standard library:68:7:dot',
                fs: ['at ./tut1.rsh:97:43:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
                msg: 'pay amount correct',
                who: 'Bob'
                 });
              const v147 = stdlib.addressEq(v59, v144);
              stdlib.assert(v147, {
                at: 'reach standard library:68:7:dot',
                fs: ['at ./tut1.rsh:97:43:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
                msg: 'sender correct',
                who: 'Bob'
                 });
              const v149 = stdlib.add(v121, v145);
              ;
              stdlib.protect(ctc2, await interact.informTimeout(), {
                at: './tut1.rsh:43:33:application',
                fs: ['at ./tut1.rsh:42:13:application call to [unknown function] (defined at: ./tut1.rsh:42:25:function exp)', 'at reach standard library:71:8:application call to "after" (defined at: ./tut1.rsh:41:32:function exp)', 'at ./tut1.rsh:97:43:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
                msg: 'informTimeout',
                who: 'Bob'
                 });
              return;
               }
            else {
              const [v126, v127] = txn6.data;
              const v128 = txn6.value;
              const v133 = txn6.time;
              const v125 = txn6.from;
              const v129 = stdlib.eq(v128, stdlib.checkedBigNumberify('./tut1.rsh:decimal', stdlib.UInt_max, 0));
              stdlib.assert(v129, {
                at: './tut1.rsh:96:11:dot',
                fs: [],
                msg: 'pay amount correct',
                who: 'Bob'
                 });
              const v130 = stdlib.addressEq(v48, v125);
              stdlib.assert(v130, {
                at: './tut1.rsh:96:11:dot',
                fs: [],
                msg: 'sender correct',
                who: 'Bob'
                 });
              const v132 = stdlib.add(v121, v128);
              const v135 = stdlib.digest(ctc3, [v126, v127]);
              const v136 = stdlib.eq(v105, v135);
              stdlib.assert(v136, {
                at: 'reach standard library:65:17:application',
                fs: ['at ./tut1.rsh:98:24:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)'],
                msg: null,
                who: 'Bob'
                 });
              const v139 = stdlib.sub(stdlib.checkedBigNumberify('./tut1.rsh:7:18:decimal', stdlib.UInt_max, 4), v116);
              const v140 = stdlib.add(v127, v139);
              const v141 = stdlib.mod(v140, stdlib.checkedBigNumberify('./tut1.rsh:7:32:decimal', stdlib.UInt_max, 3));
              const cv86 = v141;
              const cv259 = v132;
              const cv260 = v133;
              
              v86 = cv86;
              v259 = cv259;
              v260 = cv260;
              
              continue; }
             }
           }
         }
      const v203 = stdlib.eq(v86, stdlib.checkedBigNumberify('./tut1.rsh:makeEnum', stdlib.UInt_max, 2));
      const v206 = stdlib.mul(stdlib.checkedBigNumberify('./tut1.rsh:104:16:decimal', stdlib.UInt_max, 2), v49);
      const v208 = v203 ? v48 : v59;
      ;
      stdlib.protect(ctc2, await interact.seeOutcome(v86), {
        at: './tut1.rsh:108:28:application',
        fs: ['at ./tut1.rsh:107:11:application call to [unknown function] (defined at: ./tut1.rsh:107:23:function exp)'],
        msg: 'seeOutcome',
        who: 'Bob'
         });
      return; }
     }
  
  
   }

const _ALGO = {
  appApproval: `#pragma version 3
// Check that we're an App
txn TypeEnum
int appl
==
assert
txn RekeyTo
global ZeroAddress
==
assert
// Check that everyone's here
global GroupSize
int 4
>=
assert
// Check txnAppl (us)
txn GroupIndex
int 0
==
assert
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
byte "{{m3}}"
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
gtxn 2 Sender
byte "{{m11}}"
==
||
gtxn 2 Sender
byte "{{m12}}"
==
||
gtxn 2 Sender
byte "{{m13}}"
==
||
assert
byte base64(cw==)
app_global_get
gtxna 0 ApplicationArgs 0
==
assert
byte base64(bA==)
app_global_get
gtxna 0 ApplicationArgs 4
btoi
==
assert
// Don't check anyone else, because Handler does
// Update state
byte base64(cw==)
gtxna 0 ApplicationArgs 1
app_global_put
byte base64(bA==)
global Round
app_global_put
byte base64(aA==)
gtxna 0 ApplicationArgs 2
btoi
app_global_put
byte base64(aA==)
app_global_get
bnz halted
txn OnCompletion
int NoOp
==
assert
b done
halted:
txn OnCompletion
int DeleteApplication
==
assert
done:
int 1
return
`,
  appApproval0: `#pragma version 3
// Check that we're an App
txn TypeEnum
int appl
==
assert
txn RekeyTo
global ZeroAddress
==
assert
txn Sender
byte "{{Deployer}}"
==
assert
txn ApplicationID
bz init
global GroupSize
int 2
==
assert
txn OnCompletion
int UpdateApplication
==
assert
byte base64(cw==)
// compute state in HM_Set 0
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
assert
txn OnCompletion
int NoOp
==
assert
done:
int 1
return
`,
  appClear: `#pragma version 3
// We're alone
global GroupSize
int 1
==
assert
// We're halted
byte base64(aA==)
app_global_get
int 1
==
assert
done:
int 1
return
`,
  ctc: `#pragma version 3
// Check size
global GroupSize
int 4
>=
assert
// Check txnAppl
gtxn 0 TypeEnum
int appl
==
assert
gtxn 0 ApplicationID
byte "{{ApplicationID}}"
btoi
==
assert
// Don't check anything else, because app does
// Check us
txn TypeEnum
int pay
==
assert
txn RekeyTo
global ZeroAddress
==
assert
global ZeroAddress
byte "{{Deployer}}"
global GroupSize
int 1
-
txn GroupIndex
==
gtxna 0 ApplicationArgs 2
btoi
&&
select
txn CloseRemainderTo
==
assert
txn GroupIndex
int 4
>=
assert
done:
int 1
return
`,
  stepargs: [0, 129, 177, 169, 225, 209, 0, 0, 201, 169, 209, 201, 225, 209],
  steps: [null, `#pragma version 3
// Handler 1
// Check txnAppl
gtxn 0 TypeEnum
int appl
==
assert
gtxn 0 ApplicationID
byte "{{ApplicationID}}"
btoi
==
assert
gtxn 0 NumAppArgs
int 8
==
assert
// Check txnToHandler
gtxn 1 TypeEnum
int pay
==
assert
gtxn 1 Receiver
txn Sender
==
assert
gtxn 1 Amount
gtxn 2 Fee
int 100000
+
==
assert
// Check txnToContract
gtxn 3 TypeEnum
int pay
==
assert
gtxn 3 Receiver
byte "{{ContractAddr}}"
==
assert
// Check txnFromHandler (us)
txn GroupIndex
int 2
==
assert
txn TypeEnum
int pay
==
assert
txn Amount
int 0
==
assert
txn Receiver
gtxn 1 Sender
==
assert
// compute state in HM_Check 0
int 0
itob
keccak256
gtxna 0 ApplicationArgs 0
==
assert
txn CloseRemainderTo
gtxn 1 Sender
==
assert
// Run body
// Just "pay amount correct"
// "./tut1.rsh:53:9:dot"
// "[]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
gtxna 0 ApplicationArgs 5
btoi
==
assert
// Just "sender correct"
// "./tut1.rsh:53:9:dot"
// "[]"
int 1
assert
int 0
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
+
store 255
// compute state in HM_Set 1
int 1
itob
gtxn 3 Sender
concat
gtxna 0 ApplicationArgs 5
concat
gtxna 0 ApplicationArgs 6
concat
gtxna 0 ApplicationArgs 7
concat
load 255
itob
concat
keccak256
gtxna 0 ApplicationArgs 1
==
assert
gtxna 0 ApplicationArgs 2
btoi
int 0
==
assert
b done
// Check GroupSize
global GroupSize
int 4
==
assert
gtxna 0 ApplicationArgs 3
btoi
int 0
==
assert
// Check time limits
done:
int 1
return
`, `#pragma version 3
// Handler 2
// Check txnAppl
gtxn 0 TypeEnum
int appl
==
assert
gtxn 0 ApplicationID
byte "{{ApplicationID}}"
btoi
==
assert
gtxn 0 NumAppArgs
int 11
==
assert
// Check txnToHandler
gtxn 1 TypeEnum
int pay
==
assert
gtxn 1 Receiver
txn Sender
==
assert
gtxn 1 Amount
gtxn 2 Fee
int 100000
+
==
assert
// Check txnToContract
gtxn 3 TypeEnum
int pay
==
assert
gtxn 3 Receiver
byte "{{ContractAddr}}"
==
assert
// Check txnFromHandler (us)
txn GroupIndex
int 2
==
assert
txn TypeEnum
int pay
==
assert
txn Amount
int 0
==
assert
txn Receiver
gtxn 1 Sender
==
assert
// compute state in HM_Check 1
int 1
itob
gtxna 0 ApplicationArgs 5
concat
gtxna 0 ApplicationArgs 6
concat
gtxna 0 ApplicationArgs 7
concat
gtxna 0 ApplicationArgs 8
concat
gtxna 0 ApplicationArgs 9
concat
keccak256
gtxna 0 ApplicationArgs 0
==
assert
txn CloseRemainderTo
gtxn 1 Sender
==
assert
// Run body
// Just "pay amount correct"
// "./tut1.rsh:62:9:dot"
// "[]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
gtxna 0 ApplicationArgs 6
btoi
==
assert
// Just "sender correct"
// "./tut1.rsh:62:9:dot"
// "[]"
int 1
assert
gtxna 0 ApplicationArgs 9
btoi
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
+
store 255
// compute state in HM_Set 2
int 2
itob
gtxna 0 ApplicationArgs 5
concat
gtxna 0 ApplicationArgs 6
concat
gtxna 0 ApplicationArgs 7
concat
gtxna 0 ApplicationArgs 8
concat
gtxn 3 Sender
concat
gtxna 0 ApplicationArgs 10
concat
load 255
itob
concat
keccak256
gtxna 0 ApplicationArgs 1
==
assert
gtxna 0 ApplicationArgs 2
btoi
int 0
==
assert
b done
// Check GroupSize
global GroupSize
int 4
==
assert
gtxna 0 ApplicationArgs 3
btoi
int 0
==
assert
// Check time limits
gtxna 0 ApplicationArgs 4
btoi
gtxna 0 ApplicationArgs 7
btoi
+
dup
gtxn 0 LastValid
==
assert
dup
gtxn 1 LastValid
==
assert
dup
gtxn 2 LastValid
==
assert
dup
gtxn 3 LastValid
==
assert
pop
done:
int 1
return
`, `#pragma version 3
// Handler 3
// Check txnAppl
gtxn 0 TypeEnum
int appl
==
assert
gtxn 0 ApplicationID
byte "{{ApplicationID}}"
btoi
==
assert
gtxn 0 NumAppArgs
int 10
==
assert
// Check txnToHandler
gtxn 1 TypeEnum
int pay
==
assert
gtxn 1 Receiver
txn Sender
==
assert
gtxn 1 Amount
gtxn 2 Fee
int 100000
+
==
assert
// Check txnToContract
gtxn 3 TypeEnum
int pay
==
assert
gtxn 3 Receiver
byte "{{ContractAddr}}"
==
assert
// Check txnFromHandler (us)
txn GroupIndex
int 2
==
assert
txn TypeEnum
int pay
==
assert
txn Amount
int 0
==
assert
txn Receiver
gtxn 1 Sender
==
assert
// compute state in HM_Check 1
int 1
itob
gtxna 0 ApplicationArgs 5
concat
gtxna 0 ApplicationArgs 6
concat
gtxna 0 ApplicationArgs 7
concat
gtxna 0 ApplicationArgs 8
concat
gtxna 0 ApplicationArgs 9
concat
keccak256
gtxna 0 ApplicationArgs 0
==
assert
txn CloseRemainderTo
gtxn 1 Sender
==
assert
// Run body
// Just "pay amount correct"
// "reach standard library:68:7:dot"
// "[at ./tut1.rsh:64:41:application call to \"closeTo\" (defined at: reach standard library:67:8:function exp)]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "reach standard library:68:7:dot"
// "[at ./tut1.rsh:64:41:application call to \"closeTo\" (defined at: reach standard library:67:8:function exp)]"
gtxna 0 ApplicationArgs 5
gtxn 3 Sender
==
assert
gtxn 4 TypeEnum
int pay
==
assert
gtxn 4 Receiver
gtxna 0 ApplicationArgs 5
==
assert
gtxn 4 Amount
gtxna 0 ApplicationArgs 9
btoi
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
+
==
assert
gtxn 4 Sender
byte "{{ContractAddr}}"
==
assert
gtxn 5 TypeEnum
int pay
==
assert
// We don't check the receiver
gtxn 5 Amount
int 0
==
assert
gtxn 5 Sender
byte "{{ContractAddr}}"
==
assert
gtxna 0 ApplicationArgs 2
btoi
int 1
==
assert
b done
// Check GroupSize
global GroupSize
int 6
==
assert
gtxna 0 ApplicationArgs 3
btoi
gtxn 5 Fee
gtxn 4 Fee
+
==
assert
// Check time limits
gtxna 0 ApplicationArgs 4
btoi
gtxna 0 ApplicationArgs 7
btoi
+
dup
gtxn 0 FirstValid
==
assert
dup
gtxn 1 FirstValid
==
assert
dup
gtxn 2 FirstValid
==
assert
dup
gtxn 3 FirstValid
==
assert
dup
gtxn 4 FirstValid
==
assert
dup
gtxn 5 FirstValid
==
assert
pop
done:
int 1
return
`, `#pragma version 3
// Handler 4
// Check txnAppl
gtxn 0 TypeEnum
int appl
==
assert
gtxn 0 ApplicationID
byte "{{ApplicationID}}"
btoi
==
assert
gtxn 0 NumAppArgs
int 14
==
assert
// Check txnToHandler
gtxn 1 TypeEnum
int pay
==
assert
gtxn 1 Receiver
txn Sender
==
assert
gtxn 1 Amount
gtxn 2 Fee
int 100000
+
==
assert
// Check txnToContract
gtxn 3 TypeEnum
int pay
==
assert
gtxn 3 Receiver
byte "{{ContractAddr}}"
==
assert
// Check txnFromHandler (us)
txn GroupIndex
int 2
==
assert
txn TypeEnum
int pay
==
assert
txn Amount
int 0
==
assert
txn Receiver
gtxn 1 Sender
==
assert
// compute state in HM_Check 2
int 2
itob
gtxna 0 ApplicationArgs 5
concat
gtxna 0 ApplicationArgs 6
concat
gtxna 0 ApplicationArgs 7
concat
gtxna 0 ApplicationArgs 8
concat
gtxna 0 ApplicationArgs 9
concat
gtxna 0 ApplicationArgs 10
concat
gtxna 0 ApplicationArgs 11
concat
keccak256
gtxna 0 ApplicationArgs 0
==
assert
txn CloseRemainderTo
gtxn 1 Sender
==
assert
// Run body
// Just "pay amount correct"
// "./tut1.rsh:70:9:dot"
// "[]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "./tut1.rsh:70:9:dot"
// "[]"
gtxna 0 ApplicationArgs 5
gtxn 3 Sender
==
assert
gtxna 0 ApplicationArgs 11
btoi
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
+
store 255
// Nothing
// "reach standard library:65:17:application"
// "[at ./tut1.rsh:72:22:application call to \"checkCommitment\" (defined at: reach standard library:64:8:function exp)]"
gtxna 0 ApplicationArgs 8
gtxna 0 ApplicationArgs 12
gtxna 0 ApplicationArgs 13
concat
keccak256
==
assert
gtxna 0 ApplicationArgs 13
btoi
int 4
gtxna 0 ApplicationArgs 10
btoi
-
+
int 3
%
dup
store 254
int 1
==
bz l0
// compute state in HM_Set 6
int 6
itob
gtxna 0 ApplicationArgs 5
concat
gtxna 0 ApplicationArgs 6
concat
gtxna 0 ApplicationArgs 7
concat
gtxna 0 ApplicationArgs 9
concat
load 255
itob
concat
keccak256
gtxna 0 ApplicationArgs 1
==
assert
gtxna 0 ApplicationArgs 2
btoi
int 0
==
assert
b done
// Check GroupSize
global GroupSize
int 4
==
assert
gtxna 0 ApplicationArgs 3
btoi
int 0
==
assert
// Check time limits
gtxna 0 ApplicationArgs 4
btoi
gtxna 0 ApplicationArgs 7
btoi
+
dup
gtxn 0 LastValid
==
assert
dup
gtxn 1 LastValid
==
assert
dup
gtxn 2 LastValid
==
assert
dup
gtxn 3 LastValid
==
assert
pop
l0:
gtxn 4 TypeEnum
int pay
==
assert
gtxn 4 Receiver
gtxna 0 ApplicationArgs 9
gtxna 0 ApplicationArgs 5
load 254
int 2
==
select
==
assert
gtxn 4 Amount
int 2
gtxna 0 ApplicationArgs 6
btoi
*
==
assert
gtxn 4 Sender
byte "{{ContractAddr}}"
==
assert
gtxn 5 TypeEnum
int pay
==
assert
// We don't check the receiver
gtxn 5 Amount
int 0
==
assert
gtxn 5 Sender
byte "{{ContractAddr}}"
==
assert
gtxna 0 ApplicationArgs 2
btoi
int 1
==
assert
b done
// Check GroupSize
global GroupSize
int 6
==
assert
gtxna 0 ApplicationArgs 3
btoi
gtxn 5 Fee
gtxn 4 Fee
+
==
assert
// Check time limits
gtxna 0 ApplicationArgs 4
btoi
gtxna 0 ApplicationArgs 7
btoi
+
dup
gtxn 0 LastValid
==
assert
dup
gtxn 1 LastValid
==
assert
dup
gtxn 2 LastValid
==
assert
dup
gtxn 3 LastValid
==
assert
dup
gtxn 4 LastValid
==
assert
dup
gtxn 5 LastValid
==
assert
pop
done:
int 1
return
`, `#pragma version 3
// Handler 5
// Check txnAppl
gtxn 0 TypeEnum
int appl
==
assert
gtxn 0 ApplicationID
byte "{{ApplicationID}}"
btoi
==
assert
gtxn 0 NumAppArgs
int 12
==
assert
// Check txnToHandler
gtxn 1 TypeEnum
int pay
==
assert
gtxn 1 Receiver
txn Sender
==
assert
gtxn 1 Amount
gtxn 2 Fee
int 100000
+
==
assert
// Check txnToContract
gtxn 3 TypeEnum
int pay
==
assert
gtxn 3 Receiver
byte "{{ContractAddr}}"
==
assert
// Check txnFromHandler (us)
txn GroupIndex
int 2
==
assert
txn TypeEnum
int pay
==
assert
txn Amount
int 0
==
assert
txn Receiver
gtxn 1 Sender
==
assert
// compute state in HM_Check 2
int 2
itob
gtxna 0 ApplicationArgs 5
concat
gtxna 0 ApplicationArgs 6
concat
gtxna 0 ApplicationArgs 7
concat
gtxna 0 ApplicationArgs 8
concat
gtxna 0 ApplicationArgs 9
concat
gtxna 0 ApplicationArgs 10
concat
gtxna 0 ApplicationArgs 11
concat
keccak256
gtxna 0 ApplicationArgs 0
==
assert
txn CloseRemainderTo
gtxn 1 Sender
==
assert
// Run body
// Just "pay amount correct"
// "reach standard library:68:7:dot"
// "[at ./tut1.rsh:71:40:application call to \"closeTo\" (defined at: reach standard library:67:8:function exp)]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "reach standard library:68:7:dot"
// "[at ./tut1.rsh:71:40:application call to \"closeTo\" (defined at: reach standard library:67:8:function exp)]"
gtxna 0 ApplicationArgs 9
gtxn 3 Sender
==
assert
gtxn 4 TypeEnum
int pay
==
assert
gtxn 4 Receiver
gtxna 0 ApplicationArgs 9
==
assert
gtxn 4 Amount
gtxna 0 ApplicationArgs 11
btoi
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
+
==
assert
gtxn 4 Sender
byte "{{ContractAddr}}"
==
assert
gtxn 5 TypeEnum
int pay
==
assert
// We don't check the receiver
gtxn 5 Amount
int 0
==
assert
gtxn 5 Sender
byte "{{ContractAddr}}"
==
assert
gtxna 0 ApplicationArgs 2
btoi
int 1
==
assert
b done
// Check GroupSize
global GroupSize
int 6
==
assert
gtxna 0 ApplicationArgs 3
btoi
gtxn 5 Fee
gtxn 4 Fee
+
==
assert
// Check time limits
gtxna 0 ApplicationArgs 4
btoi
gtxna 0 ApplicationArgs 7
btoi
+
dup
gtxn 0 FirstValid
==
assert
dup
gtxn 1 FirstValid
==
assert
dup
gtxn 2 FirstValid
==
assert
dup
gtxn 3 FirstValid
==
assert
dup
gtxn 4 FirstValid
==
assert
dup
gtxn 5 FirstValid
==
assert
pop
done:
int 1
return
`, null, null, `#pragma version 3
// Handler 8
// Check txnAppl
gtxn 0 TypeEnum
int appl
==
assert
gtxn 0 ApplicationID
byte "{{ApplicationID}}"
btoi
==
assert
gtxn 0 NumAppArgs
int 11
==
assert
// Check txnToHandler
gtxn 1 TypeEnum
int pay
==
assert
gtxn 1 Receiver
txn Sender
==
assert
gtxn 1 Amount
gtxn 2 Fee
int 100000
+
==
assert
// Check txnToContract
gtxn 3 TypeEnum
int pay
==
assert
gtxn 3 Receiver
byte "{{ContractAddr}}"
==
assert
// Check txnFromHandler (us)
txn GroupIndex
int 2
==
assert
txn TypeEnum
int pay
==
assert
txn Amount
int 0
==
assert
txn Receiver
gtxn 1 Sender
==
assert
// compute state in HM_Check 6
int 6
itob
gtxna 0 ApplicationArgs 5
concat
gtxna 0 ApplicationArgs 6
concat
gtxna 0 ApplicationArgs 7
concat
gtxna 0 ApplicationArgs 8
concat
gtxna 0 ApplicationArgs 9
concat
keccak256
gtxna 0 ApplicationArgs 0
==
assert
txn CloseRemainderTo
gtxn 1 Sender
==
assert
// Run body
// Just "pay amount correct"
// "./tut1.rsh:83:11:dot"
// "[]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "./tut1.rsh:83:11:dot"
// "[]"
gtxna 0 ApplicationArgs 5
gtxn 3 Sender
==
assert
gtxna 0 ApplicationArgs 9
btoi
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
+
store 255
// compute state in HM_Set 8
int 8
itob
gtxna 0 ApplicationArgs 5
concat
gtxna 0 ApplicationArgs 6
concat
gtxna 0 ApplicationArgs 7
concat
gtxna 0 ApplicationArgs 8
concat
gtxna 0 ApplicationArgs 10
concat
load 255
itob
concat
keccak256
gtxna 0 ApplicationArgs 1
==
assert
gtxna 0 ApplicationArgs 2
btoi
int 0
==
assert
b done
// Check GroupSize
global GroupSize
int 4
==
assert
gtxna 0 ApplicationArgs 3
btoi
int 0
==
assert
// Check time limits
gtxna 0 ApplicationArgs 4
btoi
gtxna 0 ApplicationArgs 7
btoi
+
dup
gtxn 0 LastValid
==
assert
dup
gtxn 1 LastValid
==
assert
dup
gtxn 2 LastValid
==
assert
dup
gtxn 3 LastValid
==
assert
pop
done:
int 1
return
`, `#pragma version 3
// Handler 9
// Check txnAppl
gtxn 0 TypeEnum
int appl
==
assert
gtxn 0 ApplicationID
byte "{{ApplicationID}}"
btoi
==
assert
gtxn 0 NumAppArgs
int 10
==
assert
// Check txnToHandler
gtxn 1 TypeEnum
int pay
==
assert
gtxn 1 Receiver
txn Sender
==
assert
gtxn 1 Amount
gtxn 2 Fee
int 100000
+
==
assert
// Check txnToContract
gtxn 3 TypeEnum
int pay
==
assert
gtxn 3 Receiver
byte "{{ContractAddr}}"
==
assert
// Check txnFromHandler (us)
txn GroupIndex
int 2
==
assert
txn TypeEnum
int pay
==
assert
txn Amount
int 0
==
assert
txn Receiver
gtxn 1 Sender
==
assert
// compute state in HM_Check 6
int 6
itob
gtxna 0 ApplicationArgs 5
concat
gtxna 0 ApplicationArgs 6
concat
gtxna 0 ApplicationArgs 7
concat
gtxna 0 ApplicationArgs 8
concat
gtxna 0 ApplicationArgs 9
concat
keccak256
gtxna 0 ApplicationArgs 0
==
assert
txn CloseRemainderTo
gtxn 1 Sender
==
assert
// Run body
// Just "pay amount correct"
// "reach standard library:68:7:dot"
// "[at ./tut1.rsh:84:43:application call to \"closeTo\" (defined at: reach standard library:67:8:function exp)]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "reach standard library:68:7:dot"
// "[at ./tut1.rsh:84:43:application call to \"closeTo\" (defined at: reach standard library:67:8:function exp)]"
gtxna 0 ApplicationArgs 8
gtxn 3 Sender
==
assert
gtxn 4 TypeEnum
int pay
==
assert
gtxn 4 Receiver
gtxna 0 ApplicationArgs 8
==
assert
gtxn 4 Amount
gtxna 0 ApplicationArgs 9
btoi
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
+
==
assert
gtxn 4 Sender
byte "{{ContractAddr}}"
==
assert
gtxn 5 TypeEnum
int pay
==
assert
// We don't check the receiver
gtxn 5 Amount
int 0
==
assert
gtxn 5 Sender
byte "{{ContractAddr}}"
==
assert
gtxna 0 ApplicationArgs 2
btoi
int 1
==
assert
b done
// Check GroupSize
global GroupSize
int 6
==
assert
gtxna 0 ApplicationArgs 3
btoi
gtxn 5 Fee
gtxn 4 Fee
+
==
assert
// Check time limits
gtxna 0 ApplicationArgs 4
btoi
gtxna 0 ApplicationArgs 7
btoi
+
dup
gtxn 0 FirstValid
==
assert
dup
gtxn 1 FirstValid
==
assert
dup
gtxn 2 FirstValid
==
assert
dup
gtxn 3 FirstValid
==
assert
dup
gtxn 4 FirstValid
==
assert
dup
gtxn 5 FirstValid
==
assert
pop
done:
int 1
return
`, `#pragma version 3
// Handler 10
// Check txnAppl
gtxn 0 TypeEnum
int appl
==
assert
gtxn 0 ApplicationID
byte "{{ApplicationID}}"
btoi
==
assert
gtxn 0 NumAppArgs
int 12
==
assert
// Check txnToHandler
gtxn 1 TypeEnum
int pay
==
assert
gtxn 1 Receiver
txn Sender
==
assert
gtxn 1 Amount
gtxn 2 Fee
int 100000
+
==
assert
// Check txnToContract
gtxn 3 TypeEnum
int pay
==
assert
gtxn 3 Receiver
byte "{{ContractAddr}}"
==
assert
// Check txnFromHandler (us)
txn GroupIndex
int 2
==
assert
txn TypeEnum
int pay
==
assert
txn Amount
int 0
==
assert
txn Receiver
gtxn 1 Sender
==
assert
// compute state in HM_Check 8
int 8
itob
gtxna 0 ApplicationArgs 5
concat
gtxna 0 ApplicationArgs 6
concat
gtxna 0 ApplicationArgs 7
concat
gtxna 0 ApplicationArgs 8
concat
gtxna 0 ApplicationArgs 9
concat
gtxna 0 ApplicationArgs 10
concat
keccak256
gtxna 0 ApplicationArgs 0
==
assert
txn CloseRemainderTo
gtxn 1 Sender
==
assert
// Run body
// Just "pay amount correct"
// "./tut1.rsh:90:11:dot"
// "[]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "./tut1.rsh:90:11:dot"
// "[]"
gtxna 0 ApplicationArgs 8
gtxn 3 Sender
==
assert
gtxna 0 ApplicationArgs 10
btoi
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
+
store 255
// compute state in HM_Set 10
int 10
itob
gtxna 0 ApplicationArgs 5
concat
gtxna 0 ApplicationArgs 6
concat
gtxna 0 ApplicationArgs 7
concat
gtxna 0 ApplicationArgs 8
concat
gtxna 0 ApplicationArgs 9
concat
gtxna 0 ApplicationArgs 11
concat
load 255
itob
concat
keccak256
gtxna 0 ApplicationArgs 1
==
assert
gtxna 0 ApplicationArgs 2
btoi
int 0
==
assert
b done
// Check GroupSize
global GroupSize
int 4
==
assert
gtxna 0 ApplicationArgs 3
btoi
int 0
==
assert
// Check time limits
gtxna 0 ApplicationArgs 4
btoi
gtxna 0 ApplicationArgs 7
btoi
+
dup
gtxn 0 LastValid
==
assert
dup
gtxn 1 LastValid
==
assert
dup
gtxn 2 LastValid
==
assert
dup
gtxn 3 LastValid
==
assert
pop
done:
int 1
return
`, `#pragma version 3
// Handler 11
// Check txnAppl
gtxn 0 TypeEnum
int appl
==
assert
gtxn 0 ApplicationID
byte "{{ApplicationID}}"
btoi
==
assert
gtxn 0 NumAppArgs
int 11
==
assert
// Check txnToHandler
gtxn 1 TypeEnum
int pay
==
assert
gtxn 1 Receiver
txn Sender
==
assert
gtxn 1 Amount
gtxn 2 Fee
int 100000
+
==
assert
// Check txnToContract
gtxn 3 TypeEnum
int pay
==
assert
gtxn 3 Receiver
byte "{{ContractAddr}}"
==
assert
// Check txnFromHandler (us)
txn GroupIndex
int 2
==
assert
txn TypeEnum
int pay
==
assert
txn Amount
int 0
==
assert
txn Receiver
gtxn 1 Sender
==
assert
// compute state in HM_Check 8
int 8
itob
gtxna 0 ApplicationArgs 5
concat
gtxna 0 ApplicationArgs 6
concat
gtxna 0 ApplicationArgs 7
concat
gtxna 0 ApplicationArgs 8
concat
gtxna 0 ApplicationArgs 9
concat
gtxna 0 ApplicationArgs 10
concat
keccak256
gtxna 0 ApplicationArgs 0
==
assert
txn CloseRemainderTo
gtxn 1 Sender
==
assert
// Run body
// Just "pay amount correct"
// "reach standard library:68:7:dot"
// "[at ./tut1.rsh:91:43:application call to \"closeTo\" (defined at: reach standard library:67:8:function exp)]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "reach standard library:68:7:dot"
// "[at ./tut1.rsh:91:43:application call to \"closeTo\" (defined at: reach standard library:67:8:function exp)]"
gtxna 0 ApplicationArgs 5
gtxn 3 Sender
==
assert
gtxn 4 TypeEnum
int pay
==
assert
gtxn 4 Receiver
gtxna 0 ApplicationArgs 5
==
assert
gtxn 4 Amount
gtxna 0 ApplicationArgs 10
btoi
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
+
==
assert
gtxn 4 Sender
byte "{{ContractAddr}}"
==
assert
gtxn 5 TypeEnum
int pay
==
assert
// We don't check the receiver
gtxn 5 Amount
int 0
==
assert
gtxn 5 Sender
byte "{{ContractAddr}}"
==
assert
gtxna 0 ApplicationArgs 2
btoi
int 1
==
assert
b done
// Check GroupSize
global GroupSize
int 6
==
assert
gtxna 0 ApplicationArgs 3
btoi
gtxn 5 Fee
gtxn 4 Fee
+
==
assert
// Check time limits
gtxna 0 ApplicationArgs 4
btoi
gtxna 0 ApplicationArgs 7
btoi
+
dup
gtxn 0 FirstValid
==
assert
dup
gtxn 1 FirstValid
==
assert
dup
gtxn 2 FirstValid
==
assert
dup
gtxn 3 FirstValid
==
assert
dup
gtxn 4 FirstValid
==
assert
dup
gtxn 5 FirstValid
==
assert
pop
done:
int 1
return
`, `#pragma version 3
// Handler 12
// Check txnAppl
gtxn 0 TypeEnum
int appl
==
assert
gtxn 0 ApplicationID
byte "{{ApplicationID}}"
btoi
==
assert
gtxn 0 NumAppArgs
int 14
==
assert
// Check txnToHandler
gtxn 1 TypeEnum
int pay
==
assert
gtxn 1 Receiver
txn Sender
==
assert
gtxn 1 Amount
gtxn 2 Fee
int 100000
+
==
assert
// Check txnToContract
gtxn 3 TypeEnum
int pay
==
assert
gtxn 3 Receiver
byte "{{ContractAddr}}"
==
assert
// Check txnFromHandler (us)
txn GroupIndex
int 2
==
assert
txn TypeEnum
int pay
==
assert
txn Amount
int 0
==
assert
txn Receiver
gtxn 1 Sender
==
assert
// compute state in HM_Check 10
int 10
itob
gtxna 0 ApplicationArgs 5
concat
gtxna 0 ApplicationArgs 6
concat
gtxna 0 ApplicationArgs 7
concat
gtxna 0 ApplicationArgs 8
concat
gtxna 0 ApplicationArgs 9
concat
gtxna 0 ApplicationArgs 10
concat
gtxna 0 ApplicationArgs 11
concat
keccak256
gtxna 0 ApplicationArgs 0
==
assert
txn CloseRemainderTo
gtxn 1 Sender
==
assert
// Run body
// Just "pay amount correct"
// "./tut1.rsh:96:11:dot"
// "[]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "./tut1.rsh:96:11:dot"
// "[]"
gtxna 0 ApplicationArgs 5
gtxn 3 Sender
==
assert
gtxna 0 ApplicationArgs 11
btoi
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
+
store 255
// Nothing
// "reach standard library:65:17:application"
// "[at ./tut1.rsh:98:24:application call to \"checkCommitment\" (defined at: reach standard library:64:8:function exp)]"
gtxna 0 ApplicationArgs 9
gtxna 0 ApplicationArgs 12
gtxna 0 ApplicationArgs 13
concat
keccak256
==
assert
gtxna 0 ApplicationArgs 13
btoi
int 4
gtxna 0 ApplicationArgs 10
btoi
-
+
int 3
%
dup
store 254
int 1
==
bz l0
// compute state in HM_Set 6
int 6
itob
gtxna 0 ApplicationArgs 5
concat
gtxna 0 ApplicationArgs 6
concat
gtxna 0 ApplicationArgs 7
concat
gtxna 0 ApplicationArgs 8
concat
load 255
itob
concat
keccak256
gtxna 0 ApplicationArgs 1
==
assert
gtxna 0 ApplicationArgs 2
btoi
int 0
==
assert
b done
// Check GroupSize
global GroupSize
int 4
==
assert
gtxna 0 ApplicationArgs 3
btoi
int 0
==
assert
// Check time limits
gtxna 0 ApplicationArgs 4
btoi
gtxna 0 ApplicationArgs 7
btoi
+
dup
gtxn 0 LastValid
==
assert
dup
gtxn 1 LastValid
==
assert
dup
gtxn 2 LastValid
==
assert
dup
gtxn 3 LastValid
==
assert
pop
l0:
gtxn 4 TypeEnum
int pay
==
assert
gtxn 4 Receiver
gtxna 0 ApplicationArgs 8
gtxna 0 ApplicationArgs 5
load 254
int 2
==
select
==
assert
gtxn 4 Amount
int 2
gtxna 0 ApplicationArgs 6
btoi
*
==
assert
gtxn 4 Sender
byte "{{ContractAddr}}"
==
assert
gtxn 5 TypeEnum
int pay
==
assert
// We don't check the receiver
gtxn 5 Amount
int 0
==
assert
gtxn 5 Sender
byte "{{ContractAddr}}"
==
assert
gtxna 0 ApplicationArgs 2
btoi
int 1
==
assert
b done
// Check GroupSize
global GroupSize
int 6
==
assert
gtxna 0 ApplicationArgs 3
btoi
gtxn 5 Fee
gtxn 4 Fee
+
==
assert
// Check time limits
gtxna 0 ApplicationArgs 4
btoi
gtxna 0 ApplicationArgs 7
btoi
+
dup
gtxn 0 LastValid
==
assert
dup
gtxn 1 LastValid
==
assert
dup
gtxn 2 LastValid
==
assert
dup
gtxn 3 LastValid
==
assert
dup
gtxn 4 LastValid
==
assert
dup
gtxn 5 LastValid
==
assert
pop
done:
int 1
return
`, `#pragma version 3
// Handler 13
// Check txnAppl
gtxn 0 TypeEnum
int appl
==
assert
gtxn 0 ApplicationID
byte "{{ApplicationID}}"
btoi
==
assert
gtxn 0 NumAppArgs
int 12
==
assert
// Check txnToHandler
gtxn 1 TypeEnum
int pay
==
assert
gtxn 1 Receiver
txn Sender
==
assert
gtxn 1 Amount
gtxn 2 Fee
int 100000
+
==
assert
// Check txnToContract
gtxn 3 TypeEnum
int pay
==
assert
gtxn 3 Receiver
byte "{{ContractAddr}}"
==
assert
// Check txnFromHandler (us)
txn GroupIndex
int 2
==
assert
txn TypeEnum
int pay
==
assert
txn Amount
int 0
==
assert
txn Receiver
gtxn 1 Sender
==
assert
// compute state in HM_Check 10
int 10
itob
gtxna 0 ApplicationArgs 5
concat
gtxna 0 ApplicationArgs 6
concat
gtxna 0 ApplicationArgs 7
concat
gtxna 0 ApplicationArgs 8
concat
gtxna 0 ApplicationArgs 9
concat
gtxna 0 ApplicationArgs 10
concat
gtxna 0 ApplicationArgs 11
concat
keccak256
gtxna 0 ApplicationArgs 0
==
assert
txn CloseRemainderTo
gtxn 1 Sender
==
assert
// Run body
// Just "pay amount correct"
// "reach standard library:68:7:dot"
// "[at ./tut1.rsh:97:43:application call to \"closeTo\" (defined at: reach standard library:67:8:function exp)]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "reach standard library:68:7:dot"
// "[at ./tut1.rsh:97:43:application call to \"closeTo\" (defined at: reach standard library:67:8:function exp)]"
gtxna 0 ApplicationArgs 8
gtxn 3 Sender
==
assert
gtxn 4 TypeEnum
int pay
==
assert
gtxn 4 Receiver
gtxna 0 ApplicationArgs 8
==
assert
gtxn 4 Amount
gtxna 0 ApplicationArgs 11
btoi
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
+
==
assert
gtxn 4 Sender
byte "{{ContractAddr}}"
==
assert
gtxn 5 TypeEnum
int pay
==
assert
// We don't check the receiver
gtxn 5 Amount
int 0
==
assert
gtxn 5 Sender
byte "{{ContractAddr}}"
==
assert
gtxna 0 ApplicationArgs 2
btoi
int 1
==
assert
b done
// Check GroupSize
global GroupSize
int 6
==
assert
gtxna 0 ApplicationArgs 3
btoi
gtxn 5 Fee
gtxn 4 Fee
+
==
assert
// Check time limits
gtxna 0 ApplicationArgs 4
btoi
gtxna 0 ApplicationArgs 7
btoi
+
dup
gtxn 0 FirstValid
==
assert
dup
gtxn 1 FirstValid
==
assert
dup
gtxn 2 FirstValid
==
assert
dup
gtxn 3 FirstValid
==
assert
dup
gtxn 4 FirstValid
==
assert
dup
gtxn 5 FirstValid
==
assert
pop
done:
int 1
return
`],
  unsupported: false
   };
const _ETH = {
  ABI: `[
  {
    "inputs": [],
    "stateMutability": "payable",
    "type": "constructor"
  },
  {
    "anonymous": false,
    "inputs": [],
    "name": "e0",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "components": [
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v41",
                "type": "uint256"
              }
            ],
            "internalType": "struct T0",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v50",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v51",
                "type": "uint256"
              }
            ],
            "internalType": "struct T2",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct T3",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "e1",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "components": [
          {
            "components": [
              {
                "internalType": "address payable",
                "name": "v48",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v50",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v59",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v105",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v110",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v111",
                "type": "uint256"
              }
            ],
            "internalType": "struct T18",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v116",
                "type": "uint256"
              }
            ],
            "internalType": "struct T23",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct T24",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "e10",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "components": [
          {
            "components": [
              {
                "internalType": "address payable",
                "name": "v48",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v50",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v59",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v105",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v110",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v111",
                "type": "uint256"
              }
            ],
            "internalType": "struct T18",
            "name": "svs",
            "type": "tuple"
          },
          {
            "internalType": "bool",
            "name": "msg",
            "type": "bool"
          }
        ],
        "indexed": false,
        "internalType": "struct T25",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "e11",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "components": [
          {
            "components": [
              {
                "internalType": "address payable",
                "name": "v48",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v50",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v59",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v105",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v116",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v121",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v122",
                "type": "uint256"
              }
            ],
            "internalType": "struct T22",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v126",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v127",
                "type": "uint256"
              }
            ],
            "internalType": "struct T26",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct T27",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "e12",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "components": [
          {
            "components": [
              {
                "internalType": "address payable",
                "name": "v48",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v50",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v59",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v105",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v116",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v121",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v122",
                "type": "uint256"
              }
            ],
            "internalType": "struct T22",
            "name": "svs",
            "type": "tuple"
          },
          {
            "internalType": "bool",
            "name": "msg",
            "type": "bool"
          }
        ],
        "indexed": false,
        "internalType": "struct T28",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "e13",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "components": [
          {
            "components": [
              {
                "internalType": "address payable",
                "name": "v48",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v50",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v51",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v55",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v56",
                "type": "uint256"
              }
            ],
            "internalType": "struct T1",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v60",
                "type": "uint256"
              }
            ],
            "internalType": "struct T5",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct T6",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "e2",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "components": [
          {
            "components": [
              {
                "internalType": "address payable",
                "name": "v48",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v50",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v51",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v55",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v56",
                "type": "uint256"
              }
            ],
            "internalType": "struct T1",
            "name": "svs",
            "type": "tuple"
          },
          {
            "internalType": "bool",
            "name": "msg",
            "type": "bool"
          }
        ],
        "indexed": false,
        "internalType": "struct T8",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "e3",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "components": [
          {
            "components": [
              {
                "internalType": "address payable",
                "name": "v48",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v50",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v51",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v59",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v60",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v64",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v65",
                "type": "uint256"
              }
            ],
            "internalType": "struct T4",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v69",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v70",
                "type": "uint256"
              }
            ],
            "internalType": "struct T12",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct T13",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "e4",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "components": [
          {
            "components": [
              {
                "internalType": "address payable",
                "name": "v48",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v50",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v51",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v59",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v60",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v64",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v65",
                "type": "uint256"
              }
            ],
            "internalType": "struct T4",
            "name": "svs",
            "type": "tuple"
          },
          {
            "internalType": "bool",
            "name": "msg",
            "type": "bool"
          }
        ],
        "indexed": false,
        "internalType": "struct T14",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "e5",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "components": [
          {
            "components": [
              {
                "internalType": "address payable",
                "name": "v48",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v50",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v59",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v259",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v260",
                "type": "uint256"
              }
            ],
            "internalType": "struct T15",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v105",
                "type": "uint256"
              }
            ],
            "internalType": "struct T19",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct T20",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "e8",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "components": [
          {
            "components": [
              {
                "internalType": "address payable",
                "name": "v48",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v50",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v59",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v259",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v260",
                "type": "uint256"
              }
            ],
            "internalType": "struct T15",
            "name": "svs",
            "type": "tuple"
          },
          {
            "internalType": "bool",
            "name": "msg",
            "type": "bool"
          }
        ],
        "indexed": false,
        "internalType": "struct T21",
        "name": "_a",
        "type": "tuple"
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
            "components": [
              {
                "internalType": "uint256",
                "name": "v41",
                "type": "uint256"
              }
            ],
            "internalType": "struct T0",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v50",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v51",
                "type": "uint256"
              }
            ],
            "internalType": "struct T2",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "internalType": "struct T3",
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
            "components": [
              {
                "internalType": "address payable",
                "name": "v48",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v50",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v59",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v105",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v110",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v111",
                "type": "uint256"
              }
            ],
            "internalType": "struct T18",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v116",
                "type": "uint256"
              }
            ],
            "internalType": "struct T23",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "internalType": "struct T24",
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
            "components": [
              {
                "internalType": "address payable",
                "name": "v48",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v50",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v59",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v105",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v110",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v111",
                "type": "uint256"
              }
            ],
            "internalType": "struct T18",
            "name": "svs",
            "type": "tuple"
          },
          {
            "internalType": "bool",
            "name": "msg",
            "type": "bool"
          }
        ],
        "internalType": "struct T25",
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
            "components": [
              {
                "internalType": "address payable",
                "name": "v48",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v50",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v59",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v105",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v116",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v121",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v122",
                "type": "uint256"
              }
            ],
            "internalType": "struct T22",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v126",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v127",
                "type": "uint256"
              }
            ],
            "internalType": "struct T26",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "internalType": "struct T27",
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
            "components": [
              {
                "internalType": "address payable",
                "name": "v48",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v50",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v59",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v105",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v116",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v121",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v122",
                "type": "uint256"
              }
            ],
            "internalType": "struct T22",
            "name": "svs",
            "type": "tuple"
          },
          {
            "internalType": "bool",
            "name": "msg",
            "type": "bool"
          }
        ],
        "internalType": "struct T28",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "m13",
    "outputs": [],
    "stateMutability": "payable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "components": [
          {
            "components": [
              {
                "internalType": "address payable",
                "name": "v48",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v50",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v51",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v55",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v56",
                "type": "uint256"
              }
            ],
            "internalType": "struct T1",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v60",
                "type": "uint256"
              }
            ],
            "internalType": "struct T5",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "internalType": "struct T6",
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
            "components": [
              {
                "internalType": "address payable",
                "name": "v48",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v50",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v51",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v55",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v56",
                "type": "uint256"
              }
            ],
            "internalType": "struct T1",
            "name": "svs",
            "type": "tuple"
          },
          {
            "internalType": "bool",
            "name": "msg",
            "type": "bool"
          }
        ],
        "internalType": "struct T8",
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
            "components": [
              {
                "internalType": "address payable",
                "name": "v48",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v50",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v51",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v59",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v60",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v64",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v65",
                "type": "uint256"
              }
            ],
            "internalType": "struct T4",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v69",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v70",
                "type": "uint256"
              }
            ],
            "internalType": "struct T12",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "internalType": "struct T13",
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
            "components": [
              {
                "internalType": "address payable",
                "name": "v48",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v50",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v51",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v59",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v60",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v64",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v65",
                "type": "uint256"
              }
            ],
            "internalType": "struct T4",
            "name": "svs",
            "type": "tuple"
          },
          {
            "internalType": "bool",
            "name": "msg",
            "type": "bool"
          }
        ],
        "internalType": "struct T14",
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
            "components": [
              {
                "internalType": "address payable",
                "name": "v48",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v50",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v59",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v259",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v260",
                "type": "uint256"
              }
            ],
            "internalType": "struct T15",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v105",
                "type": "uint256"
              }
            ],
            "internalType": "struct T19",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "internalType": "struct T20",
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
            "components": [
              {
                "internalType": "address payable",
                "name": "v48",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v50",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v59",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v259",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v260",
                "type": "uint256"
              }
            ],
            "internalType": "struct T15",
            "name": "svs",
            "type": "tuple"
          },
          {
            "internalType": "bool",
            "name": "msg",
            "type": "bool"
          }
        ],
        "internalType": "struct T21",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "m9",
    "outputs": [],
    "stateMutability": "payable",
    "type": "function"
  },
  {
    "stateMutability": "payable",
    "type": "receive"
  }
]`,
  Bytecode: `0x608060408190527f49ff028a829527a47ec6839c7147b484eccf5a2a94853eddac09cef44d9d4e9e90600090a160408051602080820183524382528251808201845260008082529251815283518083018490529051818501528351808203850181526060909101909352825192019190912090556118f5806100826000396000f3fe6080604052600436106100a05760003560e01c80638f71c4b6116100645780638f71c4b61461010d57806392b91a3114610120578063a437860b14610133578063abb65ccc14610146578063d22bdb5814610159578063d34b6dfc1461016c576100a7565b80632bf4f873146100ac5780632bfd6934146100c1578063571d5205146100d457806360fdfb87146100e757806372f6740b146100fa576100a7565b366100a757005b600080fd5b6100bf6100ba3660046114c3565b61017f565b005b6100bf6100cf366004611470565b610320565b6100bf6100e236600461148c565b610446565b6100bf6100f5366004611454565b61055d565b6100bf6101083660046114a7565b61071f565b6100bf61011b36600461148c565b61092f565b6100bf61012e366004611454565b610b14565b6100bf610141366004611470565b610c62565b6100bf61015436600461148c565b610d79565b6100bf61016736600461148c565b610f68565b6100bf61017a3660046114a7565b611079565b60408051600060208201528235918101919091526060016040516020818303038152906040528051906020012060001c600054146101bc57600080fd5b60008080556040805160208101909152908152346020830135146101df57600080fd5b6101ea34600061183b565b815260408051833581526020808501359082015283820135818301526060808501359082015290517f2bb570a5feee0f446e450005a048c78efd478914692e1f0be1009bac144b11709181900360800190a161027e6040518060c0016040528060006001600160a01b0316815260200160008152602001600081526020016000815260200160008152602001600081525090565b338152602083810135818301908152604080860135818501908152606080880135818701908152875160808089019182524360a0808b01918252875160019a81019a909a528a516001600160a01b0316978a019790975296519388019390935292519186019190915251918401919091525160c08301525160e0820152610100015b60408051601f198184030181529190528051602090910120600055505050565b60405161033490600a908390602001611811565b6040516020818303038152906040528051906020012060001c6000541461035a57600080fd5b60008055610370604082013560e083013561183b565b431015801561037d575060015b61038657600080fd5b341561039157600080fd5b336103a26080830160608401611433565b6001600160a01b0316146103b557600080fd5b6103c56080820160608301611433565b6001600160a01b03166108fc6103df3460c085013561183b565b6040518115909202916000818181858888f19350505050158015610407573d6000803e3d6000fd5b507f69dd1b34aedf23239d695825c063b0ccab540698bc04080155ef5bbab022a1ba8160405161043791906117a9565b60405180910390a16000805533ff5b60405161045a9060069083906020016117d4565b6040516020818303038152906040528051906020012060001c6000541461048057600080fd5b60008055610496604082013560a083013561183b565b43101580156104a3575060015b6104ac57600080fd5b34156104b757600080fd5b336104c86080830160608401611433565b6001600160a01b0316146104db57600080fd5b6104eb6080820160608301611433565b6001600160a01b03166108fc61050534608085013561183b565b6040518115909202916000818181858888f1935050505015801561052d573d6000803e3d6000fd5b507fca13e6f8d91ff4ca5d5587cef0f51406a1cbd78ee61e6b614471da01a93eeea281604051610437919061171c565b60405161057190600a908390602001611811565b6040516020818303038152906040528051906020012060001c6000541461059757600080fd5b600080556105ad604082013560e083013561183b565b43106105b857600080fd5b34156105c357600080fd5b336105d16020830183611433565b6001600160a01b0316146105e457600080fd5b6040805161010083013560208201526101208301359181019190915260600160408051601f19818403018152919052805160209091012060808201351461062a57600080fd5b7f7da4df75c439201a8e39c13804f6f50b438ef5089390c42c43a237b942a7c16c81604051610659919061179a565b60405180910390a1610669611371565b6106766020830183611433565b81516001600160a01b039091169052805160208084013591015280516040808401359101526106ab6080830160608401611433565b81516001600160a01b0390911660609091015260036106cf60a08401356004611872565b6106de9061012085013561183b565b6106e89190611889565b6020820151526106fc3460c084013561183b565b6020808301805190910191909152514360409091015261071b8161118a565b5050565b6040516107339060089083906020016117e8565b6040516020818303038152906040528051906020012060001c6000541461075957600080fd5b60008080556040805160208101825291825261077c9083013560c084013561183b565b431061078757600080fd5b341561079257600080fd5b336107a36080840160608501611433565b6001600160a01b0316146107b657600080fd5b6107c43460a084013561183b565b81526040517fb5c2fe74458044d02de0cf289cfe60daebe99955f43c74bd817aa32ebd30bb49906107f690849061174c565b60405180910390a161085860405180610100016040528060006001600160a01b03168152602001600081526020016000815260200160006001600160a01b03168152602001600081526020016000815260200160008152602001600081525090565b6108656020840184611433565b6001600160a01b0316815260208084013590820152604080840135908201526108946080840160608501611433565b6001600160a01b03908116606083810191825260808681013581860190815260e08089013560a0808901918252895160c0808b0191825243858c0190815260408051600a6020808301919091528e518e16828401528e01519a81019a909a528c015197890197909752975190981690860152915194840194909452519282019290925291516101008301525161012082015261014001610300565b6040516109439060019083906020016117fd565b6040516020818303038152906040528051906020012060001c6000541461096957600080fd5b60008080556040805160208101825291825261098c9083013560a084013561183b565b431061099757600080fd5b346020830135146109a757600080fd5b6109b534608084013561183b565b81526040517f9bfbd5986241222572c780ca3c93186365207930439d70d8812e93d77208b9ec906109e79084906117b8565b60405180910390a1610a4960405180610100016040528060006001600160a01b0316815260200160008152602001600081526020016000815260200160006001600160a01b031681526020016000815260200160008152602001600081525090565b610a566020840184611433565b6001600160a01b0316815260208084013581830152604080850135818401526060808601359084015233608084015260c08086013560a08501528451908401524360e0840152516103009160029184910160006101208201905083825260018060a01b038084511660208401526020840151604084015260408401516060840152606084015160808401528060808501511660a08401525060a083015160c083015260c083015160e083015260e08301516101008301529392505050565b604051610b28906002908390602001611826565b6040516020818303038152906040528051906020012060001c60005414610b4e57600080fd5b60008055610b64604082013560e083013561183b565b4310610b6f57600080fd5b3415610b7a57600080fd5b33610b886020830183611433565b6001600160a01b031614610b9b57600080fd5b6040805161010083013560208201526101208301359181019190915260600160408051601f198184030181529190528051602090910120606082013514610be157600080fd5b7f94867f4474e5a69dad1ab334e9b419f7aef4a590534cf35772ef55b613c168e981604051610c1091906116a1565b60405180910390a1610c20611371565b610c2d6020830183611433565b81516001600160a01b039091169052805160208084013591015280516040808401359101526106ab60a0830160808401611433565b604051610c76906002908390602001611826565b6040516020818303038152906040528051906020012060001c60005414610c9c57600080fd5b60008055610cb2604082013560e083013561183b565b4310158015610cbf575060015b610cc857600080fd5b3415610cd357600080fd5b33610ce460a0830160808401611433565b6001600160a01b031614610cf757600080fd5b610d0760a0820160808301611433565b6001600160a01b03166108fc610d213460c085013561183b565b6040518115909202916000818181858888f19350505050158015610d49573d6000803e3d6000fd5b507fc292a98c9160ad377062bc69de738d1c79919b1aba82516476823d2be3a085a08160405161043791906116cd565b604051610d8d9060069083906020016117d4565b6040516020818303038152906040528051906020012060001c60005414610db357600080fd5b600080805560408051602081018252918252610dd69083013560a084013561183b565b4310610de157600080fd5b3415610dec57600080fd5b33610dfa6020840184611433565b6001600160a01b031614610e0d57600080fd5b610e1b34608084013561183b565b81526040517fdaaf5830bac0927d0eb7e607cca3bafc032c01e985c2937f25e78351cc41ccc390610e4d908490611700565b60405180910390a1610ea76040518060e0016040528060006001600160a01b03168152602001600081526020016000815260200160006001600160a01b031681526020016000815260200160008152602001600081525090565b610eb46020840184611433565b6001600160a01b031681526020808401359082015260408084013590820152610ee36080840160608501611433565b6001600160a01b03908116606083810191825260c0868101356080808701918252875160a080890191825243858a019081526040805160086020808301919091528c518c16828401528c0151988101989098528a0151938701939093529551909616948401949094525190820152915160e08301525161010082015261012001610300565b604051610f7c9060019083906020016117fd565b6040516020818303038152906040528051906020012060001c60005414610fa257600080fd5b60008055610fb8604082013560a083013561183b565b4310158015610fc5575060015b610fce57600080fd5b3415610fd957600080fd5b33610fe76020830183611433565b6001600160a01b031614610ffa57600080fd5b6110076020820182611433565b6001600160a01b03166108fc61102134608085013561183b565b6040518115909202916000818181858888f19350505050158015611049573d6000803e3d6000fd5b507f17040e3ed853a8df776cd092f1357f15488d98d460f66cd5e6b0cb07d5bc8ae38160405161043791906117c6565b60405161108d9060089083906020016117e8565b6040516020818303038152906040528051906020012060001c600054146110b357600080fd5b600080556110c9604082013560c083013561183b565b43101580156110d6575060015b6110df57600080fd5b34156110ea57600080fd5b336110f86020830183611433565b6001600160a01b03161461110b57600080fd5b6111186020820182611433565b6001600160a01b03166108fc6111323460a085013561183b565b6040518115909202916000818181858888f1935050505015801561115a573d6000803e3d6000fd5b507f3ed1473309a228b1330dadeda04661997e11292de069751738dd383ff44916b4816040516104379190611769565b60208101515160011415611298576111e36040518060c0016040528060006001600160a01b03168152602001600081526020016000815260200160006001600160a01b0316815260200160008152602001600081525090565b8151516001600160a01b039081168252825160209081015181840190815284516040908101518186019081528651606090810151861681880190815285890180518701516080808b01918252915186015160a0808c0191825287516006818c01529b518b168c8901529751948b01949094529351908901525190951692860192909252905160c0850152915160e080850191909152825180850390910181526101009093019091528151910120600055611301565b6040805160c081018252600091810182815260608083018481526080840185815260a085018681528486526020808701979097528751516001600160a01b0390811690955287518701519092528651909201519092169052918301515190915261071b81611304565b50565b80516060015160021461131c57805160400151611320565b8051515b6001600160a01b03166108fc82600001516020015160026113419190611853565b6040518115909202916000818181858888f19350505050158015611369573d6000803e3d6000fd5b506000805533ff5b6040805160c0810182526000918101828152606082018390526080820183905260a082019290925290819081526020016113c560405180606001604052806000815260200160008152602001600081525090565b905290565b80356001600160a01b03811681146113e157600080fd5b919050565b600061014082840312156113f8578081fd5b50919050565b600061012082840312156113f8578081fd5b600060e082840312156113f8578081fd5b600061010082840312156113f8578081fd5b600060208284031215611444578081fd5b61144d826113ca565b9392505050565b60006101408284031215611466578081fd5b61144d83836113e6565b60006101208284031215611482578081fd5b61144d83836113fe565b600060e0828403121561149d578081fd5b61144d8383611410565b600061010082840312156114b9578081fd5b61144d8383611421565b6000608082840312156113f8578081fd5b6001600160a01b03806114e6836113ca565b16835260208201356020840152604082013560408401528061150a606084016113ca565b166060840152506080818101359083015260a090810135910152565b6001600160a01b0380611538836113ca565b16835260208201356020840152604082013560408401528061155c606084016113ca565b166060840152506080810135608083015260a081013560a083015260c081013560c08301525050565b6001600160a01b03611596826113ca565b1682526020810135602083015260408101356040830152606081013560608301526080810135608083015260a081013560a08301525050565b6001600160a01b03806115e1836113ca565b168352602082013560208401526040820135604084015280611605606084016113ca565b166060840152506080810135608083015260a081013560a083015260c081013560c083015260e081013560e08301525050565b6001600160a01b038061164a836113ca565b16835260208201356020840152604082013560408401526060820135606084015280611678608084016113ca565b1660808401525060a081013560a083015260c081013560c083015260e081013560e08301525050565b61014081016116b08284611638565b610100838101358382015261012080850135908401525092915050565b61012081016116dc8284611638565b610100808401358015158082146116f257600080fd5b808386015250505092915050565b60e0810161170e82846114d4565b60c092830135919092015290565b60e0810161172a82846114d4565b60c083013580151580821461173e57600080fd5b8060c0850152505092915050565b610100810161175b8284611526565b60e092830135919092015290565b61010081016117788284611526565b60e083013580151580821461178c57600080fd5b8060e0850152505092915050565b61014081016116b082846115cf565b61012081016116dc82846115cf565b60e0810161170e8284611585565b60e0810161172a8284611585565b82815260e0810161144d60208301846114d4565b828152610100810161144d6020830184611526565b82815260e0810161144d6020830184611585565b828152610120810161144d60208301846115cf565b828152610120810161144d6020830184611638565b6000821982111561184e5761184e6118a9565b500190565b600081600019048311821515161561186d5761186d6118a9565b500290565b600082821015611884576118846118a9565b500390565b6000826118a457634e487b7160e01b81526012600452602481fd5b500690565b634e487b7160e01b600052601160045260246000fdfea2646970667358221220a54cf777ca28ad371eec5abea233c38033b1c13b8d5069c5e7ad1acea123c76764736f6c63430008020033`,
  deployMode: `DM_constructor`
   };

export const _Connectors = {
  ALGO: _ALGO,
  ETH: _ETH
   };

