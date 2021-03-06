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
  const ctc5 = stdlib.T_Tuple([ctc0, ctc4, ctc0, ctc4, ctc0, ctc0]);
  const ctc6 = stdlib.T_Tuple([ctc0, ctc4, ctc0, ctc4, ctc0]);
  const ctc7 = stdlib.T_Tuple([]);
  const ctc8 = stdlib.T_Tuple([ctc0, ctc4, ctc0, ctc4, ctc2, ctc0, ctc0, ctc0]);
  const ctc9 = stdlib.T_Tuple([ctc0, ctc4, ctc0, ctc4, ctc2, ctc0, ctc0]);
  const ctc10 = stdlib.T_Tuple([ctc0, ctc4, ctc0, ctc4, ctc2, ctc0]);
  const ctc11 = stdlib.T_Tuple([ctc0, ctc4, ctc0, ctc0, ctc0]);
  const ctc12 = stdlib.T_Tuple([ctc0, ctc4, ctc0, ctc0]);
  const ctc13 = stdlib.T_Tuple([ctc0]);
  
  
  const v32 = await ctc.creationTime();
  const v31 = stdlib.protect(ctc0, interact.wager, null);
  const txn1 = await (ctc.sendrecv('Alice', 1, 1, stdlib.checkedBigNumberify('./index.rsh:44:9:dot', stdlib.UInt_max, 0), [ctc0, ctc0], [v32, v31], v31, [ctc0], true, true, false, ((txn1) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.digest(ctc1, [stdlib.checkedBigNumberify('./index.rsh:44:9:dot', stdlib.UInt_max, 0), v32]);
    sim_r.prevSt_noPrevTime = stdlib.digest(ctc13, [stdlib.checkedBigNumberify('./index.rsh:44:9:dot', stdlib.UInt_max, 0)]);
    const [v36] = txn1.data;
    const v37 = txn1.value;
    const v41 = txn1.time;
    const v35 = txn1.from;
    
    const v38 = stdlib.eq(v37, v36);
    stdlib.assert(v38, {
      at: './index.rsh:44:9:dot',
      fs: [],
      msg: 'pay amount correct',
      who: 'Alice'
       });
    stdlib.assert(true, {
      at: './index.rsh:44:9:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Alice'
       });
    const v40 = stdlib.add(stdlib.checkedBigNumberify('./index.rsh:compileDApp', stdlib.UInt_max, 0), v37);
    sim_r.nextSt = stdlib.digest(ctc11, [stdlib.checkedBigNumberify('./index.rsh:46:15:after expr stmt semicolon', stdlib.UInt_max, 1), v35, v36, v40, v41]);
    sim_r.nextSt_noTime = stdlib.digest(ctc12, [stdlib.checkedBigNumberify('./index.rsh:46:15:after expr stmt semicolon', stdlib.UInt_max, 1), v35, v36, v40]);
    sim_r.isHalt = false;
    
    return sim_r;
     })));
  const [v36] = txn1.data;
  const v37 = txn1.value;
  const v41 = txn1.time;
  const v35 = txn1.from;
  const v38 = stdlib.eq(v37, v36);
  stdlib.assert(v38, {
    at: './index.rsh:44:9:dot',
    fs: [],
    msg: 'pay amount correct',
    who: 'Alice'
     });
  stdlib.assert(true, {
    at: './index.rsh:44:9:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Alice'
     });
  const v40 = stdlib.add(stdlib.checkedBigNumberify('./index.rsh:compileDApp', stdlib.UInt_max, 0), v37);
  const txn2 = await (ctc.recv('Alice', 2, 0, [], false, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10)));
  if (txn2.didTimeout) {
    const txn3 = await (ctc.sendrecv('Alice', 3, 0, stdlib.checkedBigNumberify('reach standard library:66:7:dot', stdlib.UInt_max, 3), [ctc4, ctc0, ctc0, ctc0], [v35, v36, v40, v41], stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), [], true, true, false, ((txn3) => {
      const sim_r = { txns: [] };
      sim_r.prevSt = stdlib.digest(ctc11, [stdlib.checkedBigNumberify('reach standard library:66:7:dot', stdlib.UInt_max, 1), v35, v36, v40, v41]);
      sim_r.prevSt_noPrevTime = stdlib.digest(ctc12, [stdlib.checkedBigNumberify('reach standard library:66:7:dot', stdlib.UInt_max, 1), v35, v36, v40]);
      const [] = txn3.data;
      const v185 = txn3.value;
      const v190 = txn3.time;
      const v184 = txn3.from;
      
      const v186 = stdlib.eq(v185, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
      stdlib.assert(v186, {
        at: 'reach standard library:66:7:dot',
        fs: ['at ./index.rsh:51:41:application call to "closeTo" (defined at: reach standard library:65:8:function exp)'],
        msg: 'pay amount correct',
        who: 'Alice'
         });
      const v187 = stdlib.addressEq(v35, v184);
      stdlib.assert(v187, {
        at: 'reach standard library:66:7:dot',
        fs: ['at ./index.rsh:51:41:application call to "closeTo" (defined at: reach standard library:65:8:function exp)'],
        msg: 'sender correct',
        who: 'Alice'
         });
      const v189 = stdlib.add(v40, v185);
      sim_r.txns.push({
        amt: v189,
        to: v35
         });
      sim_r.nextSt = stdlib.digest(ctc7, []);
      sim_r.nextSt_noTime = stdlib.digest(ctc7, []);
      sim_r.isHalt = true;
      
      return sim_r;
       })));
    const [] = txn3.data;
    const v185 = txn3.value;
    const v190 = txn3.time;
    const v184 = txn3.from;
    const v186 = stdlib.eq(v185, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
    stdlib.assert(v186, {
      at: 'reach standard library:66:7:dot',
      fs: ['at ./index.rsh:51:41:application call to "closeTo" (defined at: reach standard library:65:8:function exp)'],
      msg: 'pay amount correct',
      who: 'Alice'
       });
    const v187 = stdlib.addressEq(v35, v184);
    stdlib.assert(v187, {
      at: 'reach standard library:66:7:dot',
      fs: ['at ./index.rsh:51:41:application call to "closeTo" (defined at: reach standard library:65:8:function exp)'],
      msg: 'sender correct',
      who: 'Alice'
       });
    const v189 = stdlib.add(v40, v185);
    ;
    stdlib.protect(ctc3, await interact.informTimeout(), {
      at: './index.rsh:40:33:application',
      fs: ['at ./index.rsh:39:13:application call to [unknown function] (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:69:8:application call to "after" (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:51:41:application call to "closeTo" (defined at: reach standard library:65:8:function exp)'],
      msg: 'informTimeout',
      who: 'Alice'
       });
    return;
     }
  else {
    const [] = txn2.data;
    const v45 = txn2.value;
    const v49 = txn2.time;
    const v44 = txn2.from;
    const v46 = stdlib.eq(v45, v36);
    stdlib.assert(v46, {
      at: './index.rsh:50:9:dot',
      fs: [],
      msg: 'pay amount correct',
      who: 'Alice'
       });
    stdlib.assert(true, {
      at: './index.rsh:50:9:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Alice'
       });
    const v48 = stdlib.add(v40, v45);
    let v50 = stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1);
    let v203 = v48;
    let v204 = v49;
    
    while ((() => {
      const v60 = stdlib.eq(v50, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
      
      return v60; })()) {
      const v63 = stdlib.protect(ctc0, await interact.getHand(), {
        at: './index.rsh:59:42:application',
        fs: ['at ./index.rsh:58:15:application call to [unknown function] (defined at: ./index.rsh:58:19:function exp)'],
        msg: 'getHand',
        who: 'Alice'
         });
      const v65 = stdlib.protect(ctc0, await interact.random(), {
        at: 'reach standard library:58:31:application',
        fs: ['at ./index.rsh:60:52:application call to "makeCommitment" (defined at: reach standard library:57:8:function exp)', 'at ./index.rsh:58:15:application call to [unknown function] (defined at: ./index.rsh:58:19:function exp)'],
        msg: 'random',
        who: 'Alice'
         });
      const v66 = stdlib.digest(ctc1, [v65, v63]);
      const txn3 = await (ctc.sendrecv('Alice', 6, 1, stdlib.checkedBigNumberify('./index.rsh:62:11:dot', stdlib.UInt_max, 4), [ctc4, ctc0, ctc4, ctc0, ctc0, ctc2], [v35, v36, v44, v203, v204, v66], stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), [ctc2], true, true, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10), ((txn3) => {
        const sim_r = { txns: [] };
        sim_r.prevSt = stdlib.digest(ctc5, [stdlib.checkedBigNumberify('./index.rsh:62:11:dot', stdlib.UInt_max, 4), v35, v36, v44, v203, v204]);
        sim_r.prevSt_noPrevTime = stdlib.digest(ctc6, [stdlib.checkedBigNumberify('./index.rsh:62:11:dot', stdlib.UInt_max, 4), v35, v36, v44, v203]);
        const [v69] = txn3.data;
        const v70 = txn3.value;
        const v75 = txn3.time;
        const v68 = txn3.from;
        
        const v71 = stdlib.eq(v70, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
        stdlib.assert(v71, {
          at: './index.rsh:62:11:dot',
          fs: [],
          msg: 'pay amount correct',
          who: 'Alice'
           });
        const v72 = stdlib.addressEq(v35, v68);
        stdlib.assert(v72, {
          at: './index.rsh:62:11:dot',
          fs: [],
          msg: 'sender correct',
          who: 'Alice'
           });
        const v74 = stdlib.add(v203, v70);
        sim_r.nextSt = stdlib.digest(ctc9, [stdlib.checkedBigNumberify('./index.rsh:64:17:after expr stmt semicolon', stdlib.UInt_max, 6), v35, v36, v44, v69, v74, v75]);
        sim_r.nextSt_noTime = stdlib.digest(ctc10, [stdlib.checkedBigNumberify('./index.rsh:64:17:after expr stmt semicolon', stdlib.UInt_max, 6), v35, v36, v44, v69, v74]);
        sim_r.isHalt = false;
        
        return sim_r;
         })));
      if (txn3.didTimeout) {
        const txn4 = await (ctc.recv('Alice', 7, 0, [], false, false));
        const [] = txn4.data;
        const v149 = txn4.value;
        const v154 = txn4.time;
        const v148 = txn4.from;
        const v150 = stdlib.eq(v149, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
        stdlib.assert(v150, {
          at: 'reach standard library:66:7:dot',
          fs: ['at ./index.rsh:63:43:application call to "closeTo" (defined at: reach standard library:65:8:function exp)'],
          msg: 'pay amount correct',
          who: 'Alice'
           });
        const v151 = stdlib.addressEq(v44, v148);
        stdlib.assert(v151, {
          at: 'reach standard library:66:7:dot',
          fs: ['at ./index.rsh:63:43:application call to "closeTo" (defined at: reach standard library:65:8:function exp)'],
          msg: 'sender correct',
          who: 'Alice'
           });
        const v153 = stdlib.add(v203, v149);
        ;
        stdlib.protect(ctc3, await interact.informTimeout(), {
          at: './index.rsh:40:33:application',
          fs: ['at ./index.rsh:39:13:application call to [unknown function] (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:69:8:application call to "after" (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:63:43:application call to "closeTo" (defined at: reach standard library:65:8:function exp)'],
          msg: 'informTimeout',
          who: 'Alice'
           });
        return;
         }
      else {
        const [v69] = txn3.data;
        const v70 = txn3.value;
        const v75 = txn3.time;
        const v68 = txn3.from;
        const v71 = stdlib.eq(v70, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
        stdlib.assert(v71, {
          at: './index.rsh:62:11:dot',
          fs: [],
          msg: 'pay amount correct',
          who: 'Alice'
           });
        const v72 = stdlib.addressEq(v35, v68);
        stdlib.assert(v72, {
          at: './index.rsh:62:11:dot',
          fs: [],
          msg: 'sender correct',
          who: 'Alice'
           });
        const v74 = stdlib.add(v203, v70);
        const txn4 = await (ctc.recv('Alice', 8, 1, [ctc0], false, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10)));
        if (txn4.didTimeout) {
          const txn5 = await (ctc.sendrecv('Alice', 9, 0, stdlib.checkedBigNumberify('reach standard library:66:7:dot', stdlib.UInt_max, 5), [ctc4, ctc0, ctc4, ctc2, ctc0, ctc0], [v35, v36, v44, v69, v74, v75], stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), [], true, true, false, ((txn5) => {
            const sim_r = { txns: [] };
            sim_r.prevSt = stdlib.digest(ctc9, [stdlib.checkedBigNumberify('reach standard library:66:7:dot', stdlib.UInt_max, 6), v35, v36, v44, v69, v74, v75]);
            sim_r.prevSt_noPrevTime = stdlib.digest(ctc10, [stdlib.checkedBigNumberify('reach standard library:66:7:dot', stdlib.UInt_max, 6), v35, v36, v44, v69, v74]);
            const [] = txn5.data;
            const v129 = txn5.value;
            const v134 = txn5.time;
            const v128 = txn5.from;
            
            const v130 = stdlib.eq(v129, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v130, {
              at: 'reach standard library:66:7:dot',
              fs: ['at ./index.rsh:70:43:application call to "closeTo" (defined at: reach standard library:65:8:function exp)'],
              msg: 'pay amount correct',
              who: 'Alice'
               });
            const v131 = stdlib.addressEq(v35, v128);
            stdlib.assert(v131, {
              at: 'reach standard library:66:7:dot',
              fs: ['at ./index.rsh:70:43:application call to "closeTo" (defined at: reach standard library:65:8:function exp)'],
              msg: 'sender correct',
              who: 'Alice'
               });
            const v133 = stdlib.add(v74, v129);
            sim_r.txns.push({
              amt: v133,
              to: v35
               });
            sim_r.nextSt = stdlib.digest(ctc7, []);
            sim_r.nextSt_noTime = stdlib.digest(ctc7, []);
            sim_r.isHalt = true;
            
            return sim_r;
             })));
          const [] = txn5.data;
          const v129 = txn5.value;
          const v134 = txn5.time;
          const v128 = txn5.from;
          const v130 = stdlib.eq(v129, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
          stdlib.assert(v130, {
            at: 'reach standard library:66:7:dot',
            fs: ['at ./index.rsh:70:43:application call to "closeTo" (defined at: reach standard library:65:8:function exp)'],
            msg: 'pay amount correct',
            who: 'Alice'
             });
          const v131 = stdlib.addressEq(v35, v128);
          stdlib.assert(v131, {
            at: 'reach standard library:66:7:dot',
            fs: ['at ./index.rsh:70:43:application call to "closeTo" (defined at: reach standard library:65:8:function exp)'],
            msg: 'sender correct',
            who: 'Alice'
             });
          const v133 = stdlib.add(v74, v129);
          ;
          stdlib.protect(ctc3, await interact.informTimeout(), {
            at: './index.rsh:40:33:application',
            fs: ['at ./index.rsh:39:13:application call to [unknown function] (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:69:8:application call to "after" (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:70:43:application call to "closeTo" (defined at: reach standard library:65:8:function exp)'],
            msg: 'informTimeout',
            who: 'Alice'
             });
          return;
           }
        else {
          const [v80] = txn4.data;
          const v81 = txn4.value;
          const v86 = txn4.time;
          const v79 = txn4.from;
          const v82 = stdlib.eq(v81, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
          stdlib.assert(v82, {
            at: './index.rsh:69:11:dot',
            fs: [],
            msg: 'pay amount correct',
            who: 'Alice'
             });
          const v83 = stdlib.addressEq(v44, v79);
          stdlib.assert(v83, {
            at: './index.rsh:69:11:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Alice'
             });
          const v85 = stdlib.add(v74, v81);
          const txn5 = await (ctc.sendrecv('Alice', 10, 2, stdlib.checkedBigNumberify('./index.rsh:75:11:dot', stdlib.UInt_max, 6), [ctc4, ctc0, ctc4, ctc2, ctc0, ctc0, ctc0, ctc0, ctc0], [v35, v36, v44, v69, v80, v85, v86, v65, v63], stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), [ctc0, ctc0], true, true, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10), ((txn5) => {
            const sim_r = { txns: [] };
            sim_r.prevSt = stdlib.digest(ctc8, [stdlib.checkedBigNumberify('./index.rsh:75:11:dot', stdlib.UInt_max, 8), v35, v36, v44, v69, v80, v85, v86]);
            sim_r.prevSt_noPrevTime = stdlib.digest(ctc9, [stdlib.checkedBigNumberify('./index.rsh:75:11:dot', stdlib.UInt_max, 8), v35, v36, v44, v69, v80, v85]);
            const [v90, v91] = txn5.data;
            const v92 = txn5.value;
            const v97 = txn5.time;
            const v89 = txn5.from;
            
            const v93 = stdlib.eq(v92, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v93, {
              at: './index.rsh:75:11:dot',
              fs: [],
              msg: 'pay amount correct',
              who: 'Alice'
               });
            const v94 = stdlib.addressEq(v35, v89);
            stdlib.assert(v94, {
              at: './index.rsh:75:11:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Alice'
               });
            const v96 = stdlib.add(v85, v92);
            const v99 = stdlib.digest(ctc1, [v90, v91]);
            const v100 = stdlib.eq(v69, v99);
            stdlib.assert(v100, {
              at: 'reach standard library:63:17:application',
              fs: ['at ./index.rsh:77:24:application call to "checkCommitment" (defined at: reach standard library:62:8:function exp)'],
              msg: null,
              who: 'Alice'
               });
            const v103 = stdlib.sub(stdlib.checkedBigNumberify('./index.rsh:7:18:decimal', stdlib.UInt_max, 4), v80);
            const v104 = stdlib.add(v91, v103);
            const v105 = stdlib.mod(v104, stdlib.checkedBigNumberify('./index.rsh:7:32:decimal', stdlib.UInt_max, 3));
            const cv50 = v105;
            const cv203 = v96;
            const cv204 = v97;
            
            (() => {
              const v50 = cv50;
              const v203 = cv203;
              const v204 = cv204;
              
              if ((() => {
                const v60 = stdlib.eq(v50, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
                
                return v60; })()) {
                sim_r.nextSt = stdlib.digest(ctc5, [stdlib.checkedBigNumberify('./index.rsh:56:17:after expr stmt semicolon', stdlib.UInt_max, 4), v35, v36, v44, v203, v204]);
                sim_r.nextSt_noTime = stdlib.digest(ctc6, [stdlib.checkedBigNumberify('./index.rsh:56:17:after expr stmt semicolon', stdlib.UInt_max, 4), v35, v36, v44, v203]);
                sim_r.isHalt = false;
                 }
              else {
                const v167 = stdlib.eq(v50, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
                const v170 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:83:16:decimal', stdlib.UInt_max, 2), v36);
                const v172 = v167 ? v35 : v44;
                sim_r.txns.push({
                  amt: v170,
                  to: v172
                   });
                sim_r.nextSt = stdlib.digest(ctc7, []);
                sim_r.nextSt_noTime = stdlib.digest(ctc7, []);
                sim_r.isHalt = true;
                 } })();
            return sim_r;
             })));
          if (txn5.didTimeout) {
            const txn6 = await (ctc.recv('Alice', 11, 0, [], false, false));
            const [] = txn6.data;
            const v109 = txn6.value;
            const v114 = txn6.time;
            const v108 = txn6.from;
            const v110 = stdlib.eq(v109, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v110, {
              at: 'reach standard library:66:7:dot',
              fs: ['at ./index.rsh:76:43:application call to "closeTo" (defined at: reach standard library:65:8:function exp)'],
              msg: 'pay amount correct',
              who: 'Alice'
               });
            const v111 = stdlib.addressEq(v44, v108);
            stdlib.assert(v111, {
              at: 'reach standard library:66:7:dot',
              fs: ['at ./index.rsh:76:43:application call to "closeTo" (defined at: reach standard library:65:8:function exp)'],
              msg: 'sender correct',
              who: 'Alice'
               });
            const v113 = stdlib.add(v85, v109);
            ;
            stdlib.protect(ctc3, await interact.informTimeout(), {
              at: './index.rsh:40:33:application',
              fs: ['at ./index.rsh:39:13:application call to [unknown function] (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:69:8:application call to "after" (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:76:43:application call to "closeTo" (defined at: reach standard library:65:8:function exp)'],
              msg: 'informTimeout',
              who: 'Alice'
               });
            return;
             }
          else {
            const [v90, v91] = txn5.data;
            const v92 = txn5.value;
            const v97 = txn5.time;
            const v89 = txn5.from;
            const v93 = stdlib.eq(v92, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v93, {
              at: './index.rsh:75:11:dot',
              fs: [],
              msg: 'pay amount correct',
              who: 'Alice'
               });
            const v94 = stdlib.addressEq(v35, v89);
            stdlib.assert(v94, {
              at: './index.rsh:75:11:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Alice'
               });
            const v96 = stdlib.add(v85, v92);
            const v99 = stdlib.digest(ctc1, [v90, v91]);
            const v100 = stdlib.eq(v69, v99);
            stdlib.assert(v100, {
              at: 'reach standard library:63:17:application',
              fs: ['at ./index.rsh:77:24:application call to "checkCommitment" (defined at: reach standard library:62:8:function exp)'],
              msg: null,
              who: 'Alice'
               });
            const v103 = stdlib.sub(stdlib.checkedBigNumberify('./index.rsh:7:18:decimal', stdlib.UInt_max, 4), v80);
            const v104 = stdlib.add(v91, v103);
            const v105 = stdlib.mod(v104, stdlib.checkedBigNumberify('./index.rsh:7:32:decimal', stdlib.UInt_max, 3));
            const cv50 = v105;
            const cv203 = v96;
            const cv204 = v97;
            
            v50 = cv50;
            v203 = cv203;
            v204 = cv204;
            
            continue; }
           }
         }
       }
    const v167 = stdlib.eq(v50, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
    const v170 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:83:16:decimal', stdlib.UInt_max, 2), v36);
    const v172 = v167 ? v35 : v44;
    ;
    stdlib.protect(ctc3, await interact.seeOutcome(v50), {
      at: './index.rsh:87:28:application',
      fs: ['at ./index.rsh:86:11:application call to [unknown function] (defined at: ./index.rsh:86:23:function exp)'],
      msg: 'seeOutcome',
      who: 'Alice'
       });
    return; }
  
  
   }
export async function Bob(ctc, interact) {
  const stdlib = ctc.stdlib;
  const ctc0 = stdlib.T_UInt;
  const ctc1 = stdlib.T_Null;
  const ctc2 = stdlib.T_Digest;
  const ctc3 = stdlib.T_Tuple([ctc0, ctc0]);
  const ctc4 = stdlib.T_Tuple([]);
  const ctc5 = stdlib.T_Address;
  const ctc6 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc5, ctc2, ctc0, ctc0, ctc0]);
  const ctc7 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc5, ctc2, ctc0, ctc0]);
  const ctc8 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc5, ctc2, ctc0]);
  const ctc9 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc5, ctc0, ctc0]);
  const ctc10 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc5, ctc0]);
  const ctc11 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc0, ctc0]);
  const ctc12 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc0]);
  
  
  const v32 = await ctc.creationTime();
  const txn1 = await (ctc.recv('Bob', 1, 1, [ctc0], false, false));
  const [v36] = txn1.data;
  const v37 = txn1.value;
  const v41 = txn1.time;
  const v35 = txn1.from;
  const v38 = stdlib.eq(v37, v36);
  stdlib.assert(v38, {
    at: './index.rsh:44:9:dot',
    fs: [],
    msg: 'pay amount correct',
    who: 'Bob'
     });
  stdlib.assert(true, {
    at: './index.rsh:44:9:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Bob'
     });
  const v40 = stdlib.add(stdlib.checkedBigNumberify('./index.rsh:compileDApp', stdlib.UInt_max, 0), v37);
  stdlib.protect(ctc1, await interact.acceptWager(v36), {
    at: './index.rsh:49:29:application',
    fs: ['at ./index.rsh:48:13:application call to [unknown function] (defined at: ./index.rsh:48:17:function exp)'],
    msg: 'acceptWager',
    who: 'Bob'
     });
  const txn2 = await (ctc.sendrecv('Bob', 2, 0, stdlib.checkedBigNumberify('./index.rsh:50:9:dot', stdlib.UInt_max, 3), [ctc5, ctc0, ctc0, ctc0], [v35, v36, v40, v41], v36, [], true, true, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10), ((txn2) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.digest(ctc11, [stdlib.checkedBigNumberify('./index.rsh:50:9:dot', stdlib.UInt_max, 1), v35, v36, v40, v41]);
    sim_r.prevSt_noPrevTime = stdlib.digest(ctc12, [stdlib.checkedBigNumberify('./index.rsh:50:9:dot', stdlib.UInt_max, 1), v35, v36, v40]);
    const [] = txn2.data;
    const v45 = txn2.value;
    const v49 = txn2.time;
    const v44 = txn2.from;
    
    const v46 = stdlib.eq(v45, v36);
    stdlib.assert(v46, {
      at: './index.rsh:50:9:dot',
      fs: [],
      msg: 'pay amount correct',
      who: 'Bob'
       });
    stdlib.assert(true, {
      at: './index.rsh:50:9:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Bob'
       });
    const v48 = stdlib.add(v40, v45);
    const v50 = stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1);
    const v203 = v48;
    const v204 = v49;
    
    if ((() => {
      const v60 = stdlib.eq(v50, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
      
      return v60; })()) {
      sim_r.nextSt = stdlib.digest(ctc9, [stdlib.checkedBigNumberify('./index.rsh:56:17:after expr stmt semicolon', stdlib.UInt_max, 4), v35, v36, v44, v203, v204]);
      sim_r.nextSt_noTime = stdlib.digest(ctc10, [stdlib.checkedBigNumberify('./index.rsh:56:17:after expr stmt semicolon', stdlib.UInt_max, 4), v35, v36, v44, v203]);
      sim_r.isHalt = false;
       }
    else {
      const v167 = stdlib.eq(v50, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
      const v170 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:83:16:decimal', stdlib.UInt_max, 2), v36);
      const v172 = v167 ? v35 : v44;
      sim_r.txns.push({
        amt: v170,
        to: v172
         });
      sim_r.nextSt = stdlib.digest(ctc4, []);
      sim_r.nextSt_noTime = stdlib.digest(ctc4, []);
      sim_r.isHalt = true;
       }
    return sim_r;
     })));
  if (txn2.didTimeout) {
    const txn3 = await (ctc.recv('Bob', 3, 0, [], false, false));
    const [] = txn3.data;
    const v185 = txn3.value;
    const v190 = txn3.time;
    const v184 = txn3.from;
    const v186 = stdlib.eq(v185, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
    stdlib.assert(v186, {
      at: 'reach standard library:66:7:dot',
      fs: ['at ./index.rsh:51:41:application call to "closeTo" (defined at: reach standard library:65:8:function exp)'],
      msg: 'pay amount correct',
      who: 'Bob'
       });
    const v187 = stdlib.addressEq(v35, v184);
    stdlib.assert(v187, {
      at: 'reach standard library:66:7:dot',
      fs: ['at ./index.rsh:51:41:application call to "closeTo" (defined at: reach standard library:65:8:function exp)'],
      msg: 'sender correct',
      who: 'Bob'
       });
    const v189 = stdlib.add(v40, v185);
    ;
    stdlib.protect(ctc1, await interact.informTimeout(), {
      at: './index.rsh:40:33:application',
      fs: ['at ./index.rsh:39:13:application call to [unknown function] (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:69:8:application call to "after" (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:51:41:application call to "closeTo" (defined at: reach standard library:65:8:function exp)'],
      msg: 'informTimeout',
      who: 'Bob'
       });
    return;
     }
  else {
    const [] = txn2.data;
    const v45 = txn2.value;
    const v49 = txn2.time;
    const v44 = txn2.from;
    const v46 = stdlib.eq(v45, v36);
    stdlib.assert(v46, {
      at: './index.rsh:50:9:dot',
      fs: [],
      msg: 'pay amount correct',
      who: 'Bob'
       });
    stdlib.assert(true, {
      at: './index.rsh:50:9:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Bob'
       });
    const v48 = stdlib.add(v40, v45);
    let v50 = stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1);
    let v203 = v48;
    let v204 = v49;
    
    while ((() => {
      const v60 = stdlib.eq(v50, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
      
      return v60; })()) {
      const txn3 = await (ctc.recv('Bob', 6, 1, [ctc2], false, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10)));
      if (txn3.didTimeout) {
        const txn4 = await (ctc.sendrecv('Bob', 7, 0, stdlib.checkedBigNumberify('reach standard library:66:7:dot', stdlib.UInt_max, 4), [ctc5, ctc0, ctc5, ctc0, ctc0], [v35, v36, v44, v203, v204], stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), [], true, true, false, ((txn4) => {
          const sim_r = { txns: [] };
          sim_r.prevSt = stdlib.digest(ctc9, [stdlib.checkedBigNumberify('reach standard library:66:7:dot', stdlib.UInt_max, 4), v35, v36, v44, v203, v204]);
          sim_r.prevSt_noPrevTime = stdlib.digest(ctc10, [stdlib.checkedBigNumberify('reach standard library:66:7:dot', stdlib.UInt_max, 4), v35, v36, v44, v203]);
          const [] = txn4.data;
          const v149 = txn4.value;
          const v154 = txn4.time;
          const v148 = txn4.from;
          
          const v150 = stdlib.eq(v149, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
          stdlib.assert(v150, {
            at: 'reach standard library:66:7:dot',
            fs: ['at ./index.rsh:63:43:application call to "closeTo" (defined at: reach standard library:65:8:function exp)'],
            msg: 'pay amount correct',
            who: 'Bob'
             });
          const v151 = stdlib.addressEq(v44, v148);
          stdlib.assert(v151, {
            at: 'reach standard library:66:7:dot',
            fs: ['at ./index.rsh:63:43:application call to "closeTo" (defined at: reach standard library:65:8:function exp)'],
            msg: 'sender correct',
            who: 'Bob'
             });
          const v153 = stdlib.add(v203, v149);
          sim_r.txns.push({
            amt: v153,
            to: v44
             });
          sim_r.nextSt = stdlib.digest(ctc4, []);
          sim_r.nextSt_noTime = stdlib.digest(ctc4, []);
          sim_r.isHalt = true;
          
          return sim_r;
           })));
        const [] = txn4.data;
        const v149 = txn4.value;
        const v154 = txn4.time;
        const v148 = txn4.from;
        const v150 = stdlib.eq(v149, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
        stdlib.assert(v150, {
          at: 'reach standard library:66:7:dot',
          fs: ['at ./index.rsh:63:43:application call to "closeTo" (defined at: reach standard library:65:8:function exp)'],
          msg: 'pay amount correct',
          who: 'Bob'
           });
        const v151 = stdlib.addressEq(v44, v148);
        stdlib.assert(v151, {
          at: 'reach standard library:66:7:dot',
          fs: ['at ./index.rsh:63:43:application call to "closeTo" (defined at: reach standard library:65:8:function exp)'],
          msg: 'sender correct',
          who: 'Bob'
           });
        const v153 = stdlib.add(v203, v149);
        ;
        stdlib.protect(ctc1, await interact.informTimeout(), {
          at: './index.rsh:40:33:application',
          fs: ['at ./index.rsh:39:13:application call to [unknown function] (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:69:8:application call to "after" (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:63:43:application call to "closeTo" (defined at: reach standard library:65:8:function exp)'],
          msg: 'informTimeout',
          who: 'Bob'
           });
        return;
         }
      else {
        const [v69] = txn3.data;
        const v70 = txn3.value;
        const v75 = txn3.time;
        const v68 = txn3.from;
        const v71 = stdlib.eq(v70, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
        stdlib.assert(v71, {
          at: './index.rsh:62:11:dot',
          fs: [],
          msg: 'pay amount correct',
          who: 'Bob'
           });
        const v72 = stdlib.addressEq(v35, v68);
        stdlib.assert(v72, {
          at: './index.rsh:62:11:dot',
          fs: [],
          msg: 'sender correct',
          who: 'Bob'
           });
        const v74 = stdlib.add(v203, v70);
        const v78 = stdlib.protect(ctc0, await interact.getHand(), {
          at: './index.rsh:68:52:application',
          fs: ['at ./index.rsh:67:15:application call to [unknown function] (defined at: ./index.rsh:67:19:function exp)'],
          msg: 'getHand',
          who: 'Bob'
           });
        const txn4 = await (ctc.sendrecv('Bob', 8, 1, stdlib.checkedBigNumberify('./index.rsh:69:11:dot', stdlib.UInt_max, 5), [ctc5, ctc0, ctc5, ctc2, ctc0, ctc0, ctc0], [v35, v36, v44, v69, v74, v75, v78], stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), [ctc0], true, true, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10), ((txn4) => {
          const sim_r = { txns: [] };
          sim_r.prevSt = stdlib.digest(ctc7, [stdlib.checkedBigNumberify('./index.rsh:69:11:dot', stdlib.UInt_max, 6), v35, v36, v44, v69, v74, v75]);
          sim_r.prevSt_noPrevTime = stdlib.digest(ctc8, [stdlib.checkedBigNumberify('./index.rsh:69:11:dot', stdlib.UInt_max, 6), v35, v36, v44, v69, v74]);
          const [v80] = txn4.data;
          const v81 = txn4.value;
          const v86 = txn4.time;
          const v79 = txn4.from;
          
          const v82 = stdlib.eq(v81, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
          stdlib.assert(v82, {
            at: './index.rsh:69:11:dot',
            fs: [],
            msg: 'pay amount correct',
            who: 'Bob'
             });
          const v83 = stdlib.addressEq(v44, v79);
          stdlib.assert(v83, {
            at: './index.rsh:69:11:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Bob'
             });
          const v85 = stdlib.add(v74, v81);
          sim_r.nextSt = stdlib.digest(ctc6, [stdlib.checkedBigNumberify('./index.rsh:71:17:after expr stmt semicolon', stdlib.UInt_max, 8), v35, v36, v44, v69, v80, v85, v86]);
          sim_r.nextSt_noTime = stdlib.digest(ctc7, [stdlib.checkedBigNumberify('./index.rsh:71:17:after expr stmt semicolon', stdlib.UInt_max, 8), v35, v36, v44, v69, v80, v85]);
          sim_r.isHalt = false;
          
          return sim_r;
           })));
        if (txn4.didTimeout) {
          const txn5 = await (ctc.recv('Bob', 9, 0, [], false, false));
          const [] = txn5.data;
          const v129 = txn5.value;
          const v134 = txn5.time;
          const v128 = txn5.from;
          const v130 = stdlib.eq(v129, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
          stdlib.assert(v130, {
            at: 'reach standard library:66:7:dot',
            fs: ['at ./index.rsh:70:43:application call to "closeTo" (defined at: reach standard library:65:8:function exp)'],
            msg: 'pay amount correct',
            who: 'Bob'
             });
          const v131 = stdlib.addressEq(v35, v128);
          stdlib.assert(v131, {
            at: 'reach standard library:66:7:dot',
            fs: ['at ./index.rsh:70:43:application call to "closeTo" (defined at: reach standard library:65:8:function exp)'],
            msg: 'sender correct',
            who: 'Bob'
             });
          const v133 = stdlib.add(v74, v129);
          ;
          stdlib.protect(ctc1, await interact.informTimeout(), {
            at: './index.rsh:40:33:application',
            fs: ['at ./index.rsh:39:13:application call to [unknown function] (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:69:8:application call to "after" (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:70:43:application call to "closeTo" (defined at: reach standard library:65:8:function exp)'],
            msg: 'informTimeout',
            who: 'Bob'
             });
          return;
           }
        else {
          const [v80] = txn4.data;
          const v81 = txn4.value;
          const v86 = txn4.time;
          const v79 = txn4.from;
          const v82 = stdlib.eq(v81, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
          stdlib.assert(v82, {
            at: './index.rsh:69:11:dot',
            fs: [],
            msg: 'pay amount correct',
            who: 'Bob'
             });
          const v83 = stdlib.addressEq(v44, v79);
          stdlib.assert(v83, {
            at: './index.rsh:69:11:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Bob'
             });
          const v85 = stdlib.add(v74, v81);
          const txn5 = await (ctc.recv('Bob', 10, 2, [ctc0, ctc0], false, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10)));
          if (txn5.didTimeout) {
            const txn6 = await (ctc.sendrecv('Bob', 11, 0, stdlib.checkedBigNumberify('reach standard library:66:7:dot', stdlib.UInt_max, 6), [ctc5, ctc0, ctc5, ctc2, ctc0, ctc0, ctc0], [v35, v36, v44, v69, v80, v85, v86], stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), [], true, true, false, ((txn6) => {
              const sim_r = { txns: [] };
              sim_r.prevSt = stdlib.digest(ctc6, [stdlib.checkedBigNumberify('reach standard library:66:7:dot', stdlib.UInt_max, 8), v35, v36, v44, v69, v80, v85, v86]);
              sim_r.prevSt_noPrevTime = stdlib.digest(ctc7, [stdlib.checkedBigNumberify('reach standard library:66:7:dot', stdlib.UInt_max, 8), v35, v36, v44, v69, v80, v85]);
              const [] = txn6.data;
              const v109 = txn6.value;
              const v114 = txn6.time;
              const v108 = txn6.from;
              
              const v110 = stdlib.eq(v109, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
              stdlib.assert(v110, {
                at: 'reach standard library:66:7:dot',
                fs: ['at ./index.rsh:76:43:application call to "closeTo" (defined at: reach standard library:65:8:function exp)'],
                msg: 'pay amount correct',
                who: 'Bob'
                 });
              const v111 = stdlib.addressEq(v44, v108);
              stdlib.assert(v111, {
                at: 'reach standard library:66:7:dot',
                fs: ['at ./index.rsh:76:43:application call to "closeTo" (defined at: reach standard library:65:8:function exp)'],
                msg: 'sender correct',
                who: 'Bob'
                 });
              const v113 = stdlib.add(v85, v109);
              sim_r.txns.push({
                amt: v113,
                to: v44
                 });
              sim_r.nextSt = stdlib.digest(ctc4, []);
              sim_r.nextSt_noTime = stdlib.digest(ctc4, []);
              sim_r.isHalt = true;
              
              return sim_r;
               })));
            const [] = txn6.data;
            const v109 = txn6.value;
            const v114 = txn6.time;
            const v108 = txn6.from;
            const v110 = stdlib.eq(v109, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v110, {
              at: 'reach standard library:66:7:dot',
              fs: ['at ./index.rsh:76:43:application call to "closeTo" (defined at: reach standard library:65:8:function exp)'],
              msg: 'pay amount correct',
              who: 'Bob'
               });
            const v111 = stdlib.addressEq(v44, v108);
            stdlib.assert(v111, {
              at: 'reach standard library:66:7:dot',
              fs: ['at ./index.rsh:76:43:application call to "closeTo" (defined at: reach standard library:65:8:function exp)'],
              msg: 'sender correct',
              who: 'Bob'
               });
            const v113 = stdlib.add(v85, v109);
            ;
            stdlib.protect(ctc1, await interact.informTimeout(), {
              at: './index.rsh:40:33:application',
              fs: ['at ./index.rsh:39:13:application call to [unknown function] (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:69:8:application call to "after" (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:76:43:application call to "closeTo" (defined at: reach standard library:65:8:function exp)'],
              msg: 'informTimeout',
              who: 'Bob'
               });
            return;
             }
          else {
            const [v90, v91] = txn5.data;
            const v92 = txn5.value;
            const v97 = txn5.time;
            const v89 = txn5.from;
            const v93 = stdlib.eq(v92, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v93, {
              at: './index.rsh:75:11:dot',
              fs: [],
              msg: 'pay amount correct',
              who: 'Bob'
               });
            const v94 = stdlib.addressEq(v35, v89);
            stdlib.assert(v94, {
              at: './index.rsh:75:11:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Bob'
               });
            const v96 = stdlib.add(v85, v92);
            const v99 = stdlib.digest(ctc3, [v90, v91]);
            const v100 = stdlib.eq(v69, v99);
            stdlib.assert(v100, {
              at: 'reach standard library:63:17:application',
              fs: ['at ./index.rsh:77:24:application call to "checkCommitment" (defined at: reach standard library:62:8:function exp)'],
              msg: null,
              who: 'Bob'
               });
            const v103 = stdlib.sub(stdlib.checkedBigNumberify('./index.rsh:7:18:decimal', stdlib.UInt_max, 4), v80);
            const v104 = stdlib.add(v91, v103);
            const v105 = stdlib.mod(v104, stdlib.checkedBigNumberify('./index.rsh:7:32:decimal', stdlib.UInt_max, 3));
            const cv50 = v105;
            const cv203 = v96;
            const cv204 = v97;
            
            v50 = cv50;
            v203 = cv203;
            v204 = cv204;
            
            continue; }
           }
         }
       }
    const v167 = stdlib.eq(v50, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
    const v170 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:83:16:decimal', stdlib.UInt_max, 2), v36);
    const v172 = v167 ? v35 : v44;
    ;
    stdlib.protect(ctc1, await interact.seeOutcome(v50), {
      at: './index.rsh:87:28:application',
      fs: ['at ./index.rsh:86:11:application call to [unknown function] (defined at: ./index.rsh:86:23:function exp)'],
      msg: 'seeOutcome',
      who: 'Bob'
       });
    return; }
  
  
   }

const _ALGO = {
  appApproval: `#pragma version 2
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
gtxn 2 Sender
byte "{{m11}}"
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
  appApproval0: `#pragma version 2
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
int 11
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
  appClear: `#pragma version 2
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
  ctc: `#pragma version 2
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
txn CloseRemainderTo
global ZeroAddress
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
  stepargs: [0, 89, 129, 129, 0, 0, 193, 161, 201, 193, 217, 201],
  steps: [null, `#pragma version 2
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
int 6
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
// Run body
// Just "pay amount correct"
// "./index.rsh:44:9:dot"
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
// "./index.rsh:44:9:dot"
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
`, `#pragma version 2
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
keccak256
gtxna 0 ApplicationArgs 0
==
assert
// Run body
// Just "pay amount correct"
// "./index.rsh:50:9:dot"
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
// "./index.rsh:50:9:dot"
// "[]"
int 1
assert
gtxna 0 ApplicationArgs 7
btoi
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
+
store 255
int 1
int 1
==
bz l0
// compute state in HM_Set 4
int 4
itob
gtxna 0 ApplicationArgs 5
concat
gtxna 0 ApplicationArgs 6
concat
gtxn 3 Sender
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
int 10
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
int 1
int 2
==
gtxna 0 ApplicationArgs 5
gtxn 3 Sender
ite
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
gtxna 0 ApplicationArgs 2
btoi
int 1
==
assert
b done
// Check GroupSize
global GroupSize
int 5
==
assert
gtxna 0 ApplicationArgs 3
btoi
gtxn 4 Fee
==
assert
// Check time limits
gtxna 0 ApplicationArgs 4
btoi
int 10
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
pop
done:
int 1
return
`, `#pragma version 2
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
keccak256
gtxna 0 ApplicationArgs 0
==
assert
// Run body
// Just "pay amount correct"
// "reach standard library:66:7:dot"
// "[at ./index.rsh:51:41:application call to \"closeTo\" (defined at: reach standard library:65:8:function exp)]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "reach standard library:66:7:dot"
// "[at ./index.rsh:51:41:application call to \"closeTo\" (defined at: reach standard library:65:8:function exp)]"
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
gtxna 0 ApplicationArgs 7
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
gtxna 0 ApplicationArgs 2
btoi
int 1
==
assert
b done
// Check GroupSize
global GroupSize
int 5
==
assert
gtxna 0 ApplicationArgs 3
btoi
gtxn 4 Fee
==
assert
// Check time limits
gtxna 0 ApplicationArgs 4
btoi
int 10
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
pop
done:
int 1
return
`, null, null, `#pragma version 2
// Handler 6
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
// compute state in HM_Check 4
int 4
itob
gtxna 0 ApplicationArgs 5
concat
gtxna 0 ApplicationArgs 6
concat
gtxna 0 ApplicationArgs 7
concat
gtxna 0 ApplicationArgs 8
concat
keccak256
gtxna 0 ApplicationArgs 0
==
assert
// Run body
// Just "pay amount correct"
// "./index.rsh:62:11:dot"
// "[]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "./index.rsh:62:11:dot"
// "[]"
gtxna 0 ApplicationArgs 5
gtxn 3 Sender
==
assert
gtxna 0 ApplicationArgs 8
btoi
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
+
store 255
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
int 10
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
`, `#pragma version 2
// Handler 7
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
int 9
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
// compute state in HM_Check 4
int 4
itob
gtxna 0 ApplicationArgs 5
concat
gtxna 0 ApplicationArgs 6
concat
gtxna 0 ApplicationArgs 7
concat
gtxna 0 ApplicationArgs 8
concat
keccak256
gtxna 0 ApplicationArgs 0
==
assert
// Run body
// Just "pay amount correct"
// "reach standard library:66:7:dot"
// "[at ./index.rsh:63:43:application call to \"closeTo\" (defined at: reach standard library:65:8:function exp)]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "reach standard library:66:7:dot"
// "[at ./index.rsh:63:43:application call to \"closeTo\" (defined at: reach standard library:65:8:function exp)]"
gtxna 0 ApplicationArgs 7
gtxn 3 Sender
==
assert
gtxn 4 TypeEnum
int pay
==
assert
gtxn 4 Receiver
gtxna 0 ApplicationArgs 7
==
assert
gtxn 4 Amount
gtxna 0 ApplicationArgs 8
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
gtxna 0 ApplicationArgs 2
btoi
int 1
==
assert
b done
// Check GroupSize
global GroupSize
int 5
==
assert
gtxna 0 ApplicationArgs 3
btoi
gtxn 4 Fee
==
assert
// Check time limits
gtxna 0 ApplicationArgs 4
btoi
int 10
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
pop
done:
int 1
return
`, `#pragma version 2
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
// Run body
// Just "pay amount correct"
// "./index.rsh:69:11:dot"
// "[]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "./index.rsh:69:11:dot"
// "[]"
gtxna 0 ApplicationArgs 7
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
int 10
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
`, `#pragma version 2
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
// Run body
// Just "pay amount correct"
// "reach standard library:66:7:dot"
// "[at ./index.rsh:70:43:application call to \"closeTo\" (defined at: reach standard library:65:8:function exp)]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "reach standard library:66:7:dot"
// "[at ./index.rsh:70:43:application call to \"closeTo\" (defined at: reach standard library:65:8:function exp)]"
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
gtxna 0 ApplicationArgs 2
btoi
int 1
==
assert
b done
// Check GroupSize
global GroupSize
int 5
==
assert
gtxna 0 ApplicationArgs 3
btoi
gtxn 4 Fee
==
assert
// Check time limits
gtxna 0 ApplicationArgs 4
btoi
int 10
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
pop
done:
int 1
return
`, `#pragma version 2
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
int 13
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
// Run body
// Just "pay amount correct"
// "./index.rsh:75:11:dot"
// "[]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "./index.rsh:75:11:dot"
// "[]"
gtxna 0 ApplicationArgs 5
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
// Nothing
// "reach standard library:63:17:application"
// "[at ./index.rsh:77:24:application call to \"checkCommitment\" (defined at: reach standard library:62:8:function exp)]"
gtxna 0 ApplicationArgs 8
gtxna 0 ApplicationArgs 11
gtxna 0 ApplicationArgs 12
concat
keccak256
==
assert
gtxna 0 ApplicationArgs 12
btoi
int 4
gtxna 0 ApplicationArgs 9
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
// compute state in HM_Set 4
int 4
itob
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
gtxna 0 ApplicationArgs 4
btoi
int 10
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
load 254
int 2
==
gtxna 0 ApplicationArgs 5
gtxna 0 ApplicationArgs 7
ite
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
gtxna 0 ApplicationArgs 2
btoi
int 1
==
assert
b done
// Check GroupSize
global GroupSize
int 5
==
assert
gtxna 0 ApplicationArgs 3
btoi
gtxn 4 Fee
==
assert
// Check time limits
gtxna 0 ApplicationArgs 4
btoi
int 10
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
pop
done:
int 1
return
`, `#pragma version 2
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
// Run body
// Just "pay amount correct"
// "reach standard library:66:7:dot"
// "[at ./index.rsh:76:43:application call to \"closeTo\" (defined at: reach standard library:65:8:function exp)]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "reach standard library:66:7:dot"
// "[at ./index.rsh:76:43:application call to \"closeTo\" (defined at: reach standard library:65:8:function exp)]"
gtxna 0 ApplicationArgs 7
gtxn 3 Sender
==
assert
gtxn 4 TypeEnum
int pay
==
assert
gtxn 4 Receiver
gtxna 0 ApplicationArgs 7
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
gtxna 0 ApplicationArgs 2
btoi
int 1
==
assert
b done
// Check GroupSize
global GroupSize
int 5
==
assert
gtxna 0 ApplicationArgs 3
btoi
gtxn 4 Fee
==
assert
// Check time limits
gtxna 0 ApplicationArgs 4
btoi
int 10
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
                "name": "v32",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.T0",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v36",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.T2",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct ReachContract.T3",
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
                "name": "v35",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v36",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v44",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v69",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v80",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v85",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v86",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.T16",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v90",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v91",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.T20",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct ReachContract.T21",
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
                "name": "v35",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v36",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v44",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v69",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v80",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v85",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v86",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.T16",
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
        "internalType": "struct ReachContract.T22",
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
                "name": "v35",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v36",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v40",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v41",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.T1",
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
        "internalType": "struct ReachContract.T8",
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
                "name": "v35",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v36",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v40",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v41",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.T1",
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
        "internalType": "struct ReachContract.T8",
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
                "name": "v35",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v36",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v44",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v203",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v204",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.T9",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v69",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.T13",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct ReachContract.T14",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "e6",
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
                "name": "v35",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v36",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v44",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v203",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v204",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.T9",
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
        "internalType": "struct ReachContract.T15",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "e7",
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
                "name": "v35",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v36",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v44",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v69",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v74",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v75",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.T12",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v80",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.T17",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct ReachContract.T18",
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
                "name": "v35",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v36",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v44",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v69",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v74",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v75",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.T12",
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
        "internalType": "struct ReachContract.T19",
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
                "name": "v32",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.T0",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v36",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.T2",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "internalType": "struct ReachContract.T3",
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
                "name": "v35",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v36",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v44",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v69",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v80",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v85",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v86",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.T16",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v90",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v91",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.T20",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "internalType": "struct ReachContract.T21",
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
                "name": "v35",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v36",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v44",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v69",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v80",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v85",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v86",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.T16",
            "name": "svs",
            "type": "tuple"
          },
          {
            "internalType": "bool",
            "name": "msg",
            "type": "bool"
          }
        ],
        "internalType": "struct ReachContract.T22",
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
                "name": "v35",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v36",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v40",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v41",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.T1",
            "name": "svs",
            "type": "tuple"
          },
          {
            "internalType": "bool",
            "name": "msg",
            "type": "bool"
          }
        ],
        "internalType": "struct ReachContract.T8",
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
                "name": "v35",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v36",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v40",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v41",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.T1",
            "name": "svs",
            "type": "tuple"
          },
          {
            "internalType": "bool",
            "name": "msg",
            "type": "bool"
          }
        ],
        "internalType": "struct ReachContract.T8",
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
                "name": "v35",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v36",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v44",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v203",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v204",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.T9",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v69",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.T13",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "internalType": "struct ReachContract.T14",
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
            "components": [
              {
                "internalType": "address payable",
                "name": "v35",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v36",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v44",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v203",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v204",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.T9",
            "name": "svs",
            "type": "tuple"
          },
          {
            "internalType": "bool",
            "name": "msg",
            "type": "bool"
          }
        ],
        "internalType": "struct ReachContract.T15",
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
            "components": [
              {
                "internalType": "address payable",
                "name": "v35",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v36",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v44",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v69",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v74",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v75",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.T12",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v80",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.T17",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "internalType": "struct ReachContract.T18",
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
                "name": "v35",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v36",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v44",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v69",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v74",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v75",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.T12",
            "name": "svs",
            "type": "tuple"
          },
          {
            "internalType": "bool",
            "name": "msg",
            "type": "bool"
          }
        ],
        "internalType": "struct ReachContract.T19",
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
  Bytecode: `0x608060408190527f49ff028a829527a47ec6839c7147b484eccf5a2a94853eddac09cef44d9d4e9e90600090a161003461007a565b43815261003f61007a565b8151815260405161005790600090839060200161008d565b60408051601f1981840301815291905280516020909101206000555061009c9050565b6040518060200160405280600081525090565b91825251602082015260400190565b611470806100ab6000396000f3fe6080604052600436106100865760003560e01c80639532ef01116100595780639532ef01146100d9578063b32a639f146100ec578063b861cb10146100ff578063dc0f106b14610112578063e4f83a8f1461012557610086565b8063108acff51461008b5780634188e022146100a05780636cc4a844146100b35780636dacfd6f146100c6575b600080fd5b61009e610099366004610f8a565b610138565b005b61009e6100ae366004610f3a565b610251565b61009e6100c1366004610f1f565b61035b565b61009e6100d4366004610f3a565b6104c0565b61009e6100e7366004610f79565b610612565b61009e6100fa366004610f67565b6106df565b61009e61010d366004610f1f565b6107ef565b61009e610120366004610f55565b6108ff565b61009e610133366004610f8a565b610aac565b60405161014c9060019083906020016112f9565b6040516020818303038152906040528051906020012060001c6000541461017257600080fd5b610181600a60608301356113b6565b431015801561018e575060015b61019757600080fd5b34156101a257600080fd5b336101b06020830183610efe565b6001600160a01b0316146101c357600080fd5b6101d06020820182610efe565b6001600160a01b03166108fc6101ea3460408501356113b6565b6040518115909202916000818181858888f19350505050158015610212573d6000803e3d6000fd5b507f613b30050768160fb8fb1fedba26a4639c7df8d370861f403061c2d46f9802e58160405161024291906111d8565b60405180910390a16000805533ff5b604051610265906006908390602001611217565b6040516020818303038152906040528051906020012060001c6000541461028b57600080fd5b61029a600a60a08301356113b6565b43101580156102a7575060015b6102b057600080fd5b34156102bb57600080fd5b336102c96020830183610efe565b6001600160a01b0316146102dc57600080fd5b6102e96020820182610efe565b6001600160a01b03166108fc6103033460808501356113b6565b6040518115909202916000818181858888f1935050505015801561032b573d6000803e3d6000fd5b507fe30737f1ebfc963c65c5913e78ab44df878e431e05d360e374cda18b462b262b816040516102429190611135565b60405161036f906004908390602001611349565b6040516020818303038152906040528051906020012060001c6000541461039557600080fd5b61039d610cde565b6103ac600a60808401356113b6565b43106103b757600080fd5b34156103c257600080fd5b336103d06020840184610efe565b6001600160a01b0316146103e357600080fd5b6103f13460608401356113b6565b81526040517f16424d059cabc243859f670786693b7e657c3f04cbc39631fa14608999bfaef9906104239084906110cd565b60405180910390a1610433610cf1565b6104406020840184610efe565b6001600160a01b03168152602080840135908201526104656060840160408501610efe565b6001600160a01b031660408083019190915260a0808501356060840152835160808401524390830152516104a090600690839060200161122b565b60408051601f198184030181529190528051602090910120600055505050565b6040516104d4906006908390602001611217565b6040516020818303038152906040528051906020012060001c600054146104fa57600080fd5b610502610cde565b610511600a60a08401356113b6565b431061051c57600080fd5b341561052757600080fd5b336105386060840160408501610efe565b6001600160a01b03161461054b57600080fd5b6105593460808401356113b6565b81526040517fa03e2b199cbd4c163bca89aa8e3581bcf82ee511c6ed7b600ee5e8a3e78842b49061058b908490611119565b60405180910390a161059b610d39565b6105a86020840184610efe565b6001600160a01b03168152602080840135908201526105cd6060840160408501610efe565b6001600160a01b03166040808301919091526060808501359083015260c0808501356080840152835160a08401524390830152516104a0906008908390602001611297565b604051610626906000908390602001611208565b6040516020818303038152906040528051906020012060001c6000541461064c57600080fd5b610654610cde565b3460208301351461066457600080fd5b61066f3460006113b6565b81526040517ff2c62eba998811305a23599b2e6d212befbd7ded3a73f4c08bfb9aefe08dc166906106a19084906111c1565b60405180910390a16106b1610d88565b338152602080840135818301528251604080840191909152436060840152516104a09160019184910161130d565b6040516106f3906008908390602001611282565b6040516020818303038152906040528051906020012060001c6000541461071957600080fd5b610728600a60c08301356113b6565b4310158015610735575060015b61073e57600080fd5b341561074957600080fd5b3361075a6060830160408401610efe565b6001600160a01b03161461076d57600080fd5b61077d6060820160408301610efe565b6001600160a01b03166108fc6107973460a08501356113b6565b6040518115909202916000818181858888f193505050501580156107bf573d6000803e3d6000fd5b507fe7cd06eed96e73ebf1eef9fa7a8d285d298aa9f119f81a14f729b421ad209bfa816040516102429190611190565b604051610803906004908390602001611349565b6040516020818303038152906040528051906020012060001c6000541461082957600080fd5b610838600a60808301356113b6565b4310158015610845575060015b61084e57600080fd5b341561085957600080fd5b3361086a6060830160408401610efe565b6001600160a01b03161461087d57600080fd5b61088d6060820160408301610efe565b6001600160a01b03166108fc6108a73460608501356113b6565b6040518115909202916000818181858888f193505050501580156108cf573d6000803e3d6000fd5b507f5005c4e6004c19e98ada43b5f5d05731c1a82aa5d9215871f88ceb584e0f83e08160405161024291906110e9565b604051610913906008908390602001611282565b6040516020818303038152906040528051906020012060001c6000541461093957600080fd5b610948600a60c08301356113b6565b431061095357600080fd5b341561095e57600080fd5b3361096c6020830183610efe565b6001600160a01b03161461097f57600080fd5b60405161099b9060e083013590610100840135906020016113a8565b60408051601f1981840301815291905280516020909101206060820135146109c257600080fd5b7f352ea7fc48371f0bd43d7d1ad042d3e6a673947e2deccddfd368812813abc472816040516109f19190611165565b60405180910390a1610a01610db9565b610a0e6020830183610efe565b81516001600160a01b0390911690528051602080840135910152610a386060830160408401610efe565b81516001600160a01b039091166040909101526003610a5c608084013560046113ed565b610a6b906101008501356113b6565b610a759190611404565b602082015152610a893460a08401356113b6565b60208083018051909101919091525143604090910152610aa881610b99565b5050565b604051610ac09060019083906020016112f9565b6040516020818303038152906040528051906020012060001c60005414610ae657600080fd5b610af5600a60608301356113b6565b4310610b0057600080fd5b34602082013514610b1057600080fd5b7fe2fcb5361608dd42d825c4e917fd4fca89057bb8eb0b7e34b8c2813a114cc15281604051610b3f91906111d8565b60405180910390a1610b4f610db9565b610b5c6020830183610efe565b81516001600160a01b0390911690528051602080840135918101919091528151336040918201529082015160019052610a899034908401356113b6565b60208101515160011415610c2257610baf610dde565b8151516001600160a01b039081168252825160209081015181840152835160409081015190921682840152808401805182015160608501525182015160808401529051610c019160049184910161135d565b60408051601f19818403018152919052805160209091012060005550610c6e565b610c2a610e1f565b81515181516001600160a01b0391821690528251602090810151835182015283516040908101518451931692019190915282015151815160600152610aa881610c71565b50565b805160600151600214610c8957805160400151610c8d565b8051515b6001600160a01b03166108fc8260000151602001516002610cae91906113ce565b6040518115909202916000818181858888f19350505050158015610cd6573d6000803e3d6000fd5b506000805533ff5b6040518060200160405280600081525090565b6040518060c0016040528060006001600160a01b031681526020016000815260200160006001600160a01b031681526020016000815260200160008152602001600081525090565b6040518060e0016040528060006001600160a01b031681526020016000815260200160006001600160a01b03168152602001600081526020016000815260200160008152602001600081525090565b604051806080016040528060006001600160a01b031681526020016000815260200160008152602001600081525090565b6040518060400160405280610dcc610e3f565b8152602001610dd9610e5f565b905290565b6040518060a0016040528060006001600160a01b031681526020016000815260200160006001600160a01b0316815260200160008152602001600081525090565b6040518060400160405280610e32610e80565b8152600060209091015290565b604080516060810182526000808252602082018190529181019190915290565b60405180606001604052806000815260200160008152602001600081525090565b604051806080016040528060006001600160a01b031681526020016000815260200160006001600160a01b03168152602001600081525090565b80356001600160a01b0381168114610ed157600080fd5b919050565b600060c08284031215610ee7578081fd5b50919050565b600060e08284031215610ee7578081fd5b600060208284031215610f0f578081fd5b610f1882610eba565b9392505050565b600060c08284031215610f30578081fd5b610f188383610ed6565b600060e08284031215610f4b578081fd5b610f188383610eed565b60006101208284031215610ee7578081fd5b60006101008284031215610ee7578081fd5b600060408284031215610ee7578081fd5b600060a08284031215610ee7578081fd5b6001600160a01b0380610fad83610eba565b1683526020820135602084015280610fc760408401610eba565b16604084015250606081013560608301526080810135608083015260a081013560a08301525050565b6001600160a01b038061100283610eba565b168352602082013560208401528061101c60408401610eba565b16604084015250606081013560608301526080810135608083015260a081013560a083015260c081013560c08301525050565b6001600160a01b0361106082610eba565b1682526020810135602083015260408101356040830152606081013560608301525050565b6001600160a01b038061109783610eba565b16835260208201356020840152806110b160408401610eba565b1660408401525060608181013590830152608090810135910152565b60c081016110db8284611085565b60a092830135919092015290565b60c081016110f78284611085565b60a083013580151580821461110b57600080fd5b8060a0850152505092915050565b60e081016111278284610f9b565b60c092830135919092015290565b60e081016111438284610f9b565b60c083013580151580821461115757600080fd5b8060c0850152505092915050565b61012081016111748284610ff0565b60e083013560e083015261010080840135818401525092915050565b610100810161119f8284610ff0565b60e08301358015158082146111b357600080fd5b8060e0850152505092915050565b813581526020918201359181019190915260400190565b60a081016111e6828461104f565b60808301358015158082146111fa57600080fd5b806080850152505092915050565b91825235602082015260400190565b82815260e08101610f186020830184610f9b565b600060e08201905083825260018060a01b03808451166020840152602084015160408401528060408501511660608401525060608301516080830152608083015160a083015260a083015160c08301529392505050565b8281526101008101610f186020830184610ff0565b60006101008201905083825260018060a01b03808451166020840152602084015160408401528060408501511660608401525060608301516080830152608083015160a083015260a083015160c083015260c083015160e08301529392505050565b82815260a08101610f18602083018461104f565b91825280516001600160a01b03166020808401919091528101516040808401919091528101516060808401919091520151608082015260a00190565b82815260c08101610f186020830184611085565b91825280516001600160a01b0390811660208085019190915282015160408085019190915282015116606080840191909152810151608080840191909152015160a082015260c00190565b918252602082015260400190565b600082198211156113c9576113c9611424565b500190565b60008160001904831182151516156113e8576113e8611424565b500290565b6000828210156113ff576113ff611424565b500390565b60008261141f57634e487b7160e01b81526012600452602481fd5b500690565b634e487b7160e01b600052601160045260246000fdfea26469706673582212204b7eb02bfe07e2555b149a55f13243277efbc3cc138e1ffa870e2bc75054bd1764736f6c63430008000033`,
  deployMode: `DM_constructor`
   };

export const _Connectors = {
  ALGO: _ALGO,
  ETH: _ETH
   };

