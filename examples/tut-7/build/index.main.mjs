// Automatically generated with Reach 0.1.2
/* eslint-disable no-unused-vars, no-empty-pattern, no-useless-escape, no-loop-func */
export const _version = '0.1.2';


export async function Alice(ctc, interact) {
  const stdlib = ctc.stdlib;
  
  const v32 = await ctc.creationTime();
  const v31 = stdlib.protect(stdlib.T_UInt, interact.wager, null);
  const txn1 = await (ctc.sendrecv('Alice', 1, 1, stdlib.checkedBigNumberify('./index.rsh:44:9:dot', stdlib.UInt_max, 0), [stdlib.T_UInt, stdlib.T_UInt], [v32, v31], v31, [stdlib.T_UInt], true, true, false, ((txn1) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:44:9:dot', stdlib.UInt_max, 0), v32]);
    sim_r.prevSt_noPrevTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:44:9:dot', stdlib.UInt_max, 0)]);
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
    sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:46:15:after expr stmt semicolon', stdlib.UInt_max, 1), v35, v36, v40, v41]);
    sim_r.nextSt_noTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:46:15:after expr stmt semicolon', stdlib.UInt_max, 1), v35, v36, v40]);
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
    const txn3 = await (ctc.sendrecv('Alice', 3, 0, stdlib.checkedBigNumberify('reach standard library:66:7:dot', stdlib.UInt_max, 3), [stdlib.T_Address, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt], [v35, v36, v40, v41], stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), [], true, true, false, ((txn3) => {
      const sim_r = { txns: [] };
      sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('reach standard library:66:7:dot', stdlib.UInt_max, 1), v35, v36, v40, v41]);
      sim_r.prevSt_noPrevTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('reach standard library:66:7:dot', stdlib.UInt_max, 1), v35, v36, v40]);
      const [] = txn3.data;
      const v185 = txn3.value;
      const v190 = txn3.time;
      const v184 = txn3.from;
      
      const v186 = stdlib.eq(v185, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
      stdlib.assert(v186, {
        at: 'reach standard library:66:7:dot',
        fs: ['at ./index.rsh:51:41:application call to [unknown function] (defined at: reach standard library:65:8:function exp)'],
        msg: 'pay amount correct',
        who: 'Alice'
         });
      const v187 = stdlib.addressEq(v35, v184);
      stdlib.assert(v187, {
        at: 'reach standard library:66:7:dot',
        fs: ['at ./index.rsh:51:41:application call to [unknown function] (defined at: reach standard library:65:8:function exp)'],
        msg: 'sender correct',
        who: 'Alice'
         });
      const v189 = stdlib.add(v40, v185);
      sim_r.txns.push({
        amt: v189,
        to: v35
         });
      sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([]), []);
      sim_r.nextSt_noTime = stdlib.digest(stdlib.T_Tuple([]), []);
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
      fs: ['at ./index.rsh:51:41:application call to [unknown function] (defined at: reach standard library:65:8:function exp)'],
      msg: 'pay amount correct',
      who: 'Alice'
       });
    const v187 = stdlib.addressEq(v35, v184);
    stdlib.assert(v187, {
      at: 'reach standard library:66:7:dot',
      fs: ['at ./index.rsh:51:41:application call to [unknown function] (defined at: reach standard library:65:8:function exp)'],
      msg: 'sender correct',
      who: 'Alice'
       });
    const v189 = stdlib.add(v40, v185);
    ;
    stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
      at: './index.rsh:40:33:application',
      fs: ['at ./index.rsh:39:13:application call to [unknown function] (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:69:8:application call to [unknown function] (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:51:41:application call to [unknown function] (defined at: reach standard library:65:8:function exp)'],
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
    let v205 = v41;
    
    while ((() => {
      const v60 = stdlib.eq(v50, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
      
      return v60; })()) {
      const v63 = stdlib.protect(stdlib.T_UInt, await interact.getHand(), {
        at: './index.rsh:59:42:application',
        fs: ['at ./index.rsh:58:15:application call to [unknown function] (defined at: ./index.rsh:58:19:function exp)'],
        msg: 'getHand',
        who: 'Alice'
         });
      const v65 = stdlib.protect(stdlib.T_UInt, await interact.random(), {
        at: 'reach standard library:58:31:application',
        fs: ['at ./index.rsh:60:52:application call to [unknown function] (defined at: reach standard library:57:8:function exp)', 'at ./index.rsh:58:15:application call to [unknown function] (defined at: ./index.rsh:58:19:function exp)'],
        msg: 'random',
        who: 'Alice'
         });
      const v66 = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt]), [v65, v63]);
      const txn3 = await (ctc.sendrecv('Alice', 6, 1, stdlib.checkedBigNumberify('./index.rsh:62:11:dot', stdlib.UInt_max, 4), [stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Digest], [v35, v36, v44, v203, v204, v66], stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), [stdlib.T_Digest], true, true, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10), ((txn3) => {
        const sim_r = { txns: [] };
        sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:62:11:dot', stdlib.UInt_max, 4), v35, v36, v44, v203, v204]);
        sim_r.prevSt_noPrevTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:62:11:dot', stdlib.UInt_max, 4), v35, v36, v44, v203]);
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
        sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:64:17:after expr stmt semicolon', stdlib.UInt_max, 6), v35, v36, v44, v69, v74, v75]);
        sim_r.nextSt_noTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:64:17:after expr stmt semicolon', stdlib.UInt_max, 6), v35, v36, v44, v69, v74]);
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
          fs: ['at ./index.rsh:63:43:application call to [unknown function] (defined at: reach standard library:65:8:function exp)'],
          msg: 'pay amount correct',
          who: 'Alice'
           });
        const v151 = stdlib.addressEq(v44, v148);
        stdlib.assert(v151, {
          at: 'reach standard library:66:7:dot',
          fs: ['at ./index.rsh:63:43:application call to [unknown function] (defined at: reach standard library:65:8:function exp)'],
          msg: 'sender correct',
          who: 'Alice'
           });
        const v153 = stdlib.add(v203, v149);
        ;
        stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
          at: './index.rsh:40:33:application',
          fs: ['at ./index.rsh:39:13:application call to [unknown function] (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:69:8:application call to [unknown function] (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:63:43:application call to [unknown function] (defined at: reach standard library:65:8:function exp)'],
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
        const txn4 = await (ctc.recv('Alice', 8, 1, [stdlib.T_UInt], false, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10)));
        if (txn4.didTimeout) {
          const txn5 = await (ctc.sendrecv('Alice', 9, 0, stdlib.checkedBigNumberify('reach standard library:66:7:dot', stdlib.UInt_max, 5), [stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt], [v35, v36, v44, v69, v74, v75], stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), [], true, true, false, ((txn5) => {
            const sim_r = { txns: [] };
            sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('reach standard library:66:7:dot', stdlib.UInt_max, 6), v35, v36, v44, v69, v74, v75]);
            sim_r.prevSt_noPrevTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt]), [stdlib.checkedBigNumberify('reach standard library:66:7:dot', stdlib.UInt_max, 6), v35, v36, v44, v69, v74]);
            const [] = txn5.data;
            const v129 = txn5.value;
            const v134 = txn5.time;
            const v128 = txn5.from;
            
            const v130 = stdlib.eq(v129, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v130, {
              at: 'reach standard library:66:7:dot',
              fs: ['at ./index.rsh:70:43:application call to [unknown function] (defined at: reach standard library:65:8:function exp)'],
              msg: 'pay amount correct',
              who: 'Alice'
               });
            const v131 = stdlib.addressEq(v35, v128);
            stdlib.assert(v131, {
              at: 'reach standard library:66:7:dot',
              fs: ['at ./index.rsh:70:43:application call to [unknown function] (defined at: reach standard library:65:8:function exp)'],
              msg: 'sender correct',
              who: 'Alice'
               });
            const v133 = stdlib.add(v74, v129);
            sim_r.txns.push({
              amt: v133,
              to: v35
               });
            sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([]), []);
            sim_r.nextSt_noTime = stdlib.digest(stdlib.T_Tuple([]), []);
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
            fs: ['at ./index.rsh:70:43:application call to [unknown function] (defined at: reach standard library:65:8:function exp)'],
            msg: 'pay amount correct',
            who: 'Alice'
             });
          const v131 = stdlib.addressEq(v35, v128);
          stdlib.assert(v131, {
            at: 'reach standard library:66:7:dot',
            fs: ['at ./index.rsh:70:43:application call to [unknown function] (defined at: reach standard library:65:8:function exp)'],
            msg: 'sender correct',
            who: 'Alice'
             });
          const v133 = stdlib.add(v74, v129);
          ;
          stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
            at: './index.rsh:40:33:application',
            fs: ['at ./index.rsh:39:13:application call to [unknown function] (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:69:8:application call to [unknown function] (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:70:43:application call to [unknown function] (defined at: reach standard library:65:8:function exp)'],
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
          const txn5 = await (ctc.sendrecv('Alice', 10, 2, stdlib.checkedBigNumberify('./index.rsh:75:11:dot', stdlib.UInt_max, 6), [stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt], [v35, v36, v44, v69, v80, v85, v86, v65, v63], stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), [stdlib.T_UInt, stdlib.T_UInt], true, true, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10), ((txn5) => {
            const sim_r = { txns: [] };
            sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:75:11:dot', stdlib.UInt_max, 8), v35, v36, v44, v69, v80, v85, v86]);
            sim_r.prevSt_noPrevTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:75:11:dot', stdlib.UInt_max, 8), v35, v36, v44, v69, v80, v85]);
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
            const v99 = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt]), [v90, v91]);
            const v100 = stdlib.eq(v69, v99);
            stdlib.assert(v100, {
              at: 'reach standard library:63:17:application',
              fs: ['at ./index.rsh:77:24:application call to [unknown function] (defined at: reach standard library:62:8:function exp)'],
              msg: null,
              who: 'Alice'
               });
            const v103 = stdlib.sub(stdlib.checkedBigNumberify('./index.rsh:7:18:decimal', stdlib.UInt_max, 4), v80);
            const v104 = stdlib.add(v91, v103);
            const v105 = stdlib.mod(v104, stdlib.checkedBigNumberify('./index.rsh:7:32:decimal', stdlib.UInt_max, 3));
            const cv50 = v105;
            const cv203 = v96;
            const cv204 = v97;
            const cv205 = v86;
            
            (() => {
              const v50 = cv50;
              const v203 = cv203;
              const v204 = cv204;
              const v205 = cv205;
              
              if ((() => {
                const v60 = stdlib.eq(v50, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
                
                return v60; })()) {
                sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:56:17:after expr stmt semicolon', stdlib.UInt_max, 4), v35, v36, v44, v203, v204]);
                sim_r.nextSt_noTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:56:17:after expr stmt semicolon', stdlib.UInt_max, 4), v35, v36, v44, v203]);
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
                sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([]), []);
                sim_r.nextSt_noTime = stdlib.digest(stdlib.T_Tuple([]), []);
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
              fs: ['at ./index.rsh:76:43:application call to [unknown function] (defined at: reach standard library:65:8:function exp)'],
              msg: 'pay amount correct',
              who: 'Alice'
               });
            const v111 = stdlib.addressEq(v44, v108);
            stdlib.assert(v111, {
              at: 'reach standard library:66:7:dot',
              fs: ['at ./index.rsh:76:43:application call to [unknown function] (defined at: reach standard library:65:8:function exp)'],
              msg: 'sender correct',
              who: 'Alice'
               });
            const v113 = stdlib.add(v85, v109);
            ;
            stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
              at: './index.rsh:40:33:application',
              fs: ['at ./index.rsh:39:13:application call to [unknown function] (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:69:8:application call to [unknown function] (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:76:43:application call to [unknown function] (defined at: reach standard library:65:8:function exp)'],
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
            const v99 = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt]), [v90, v91]);
            const v100 = stdlib.eq(v69, v99);
            stdlib.assert(v100, {
              at: 'reach standard library:63:17:application',
              fs: ['at ./index.rsh:77:24:application call to [unknown function] (defined at: reach standard library:62:8:function exp)'],
              msg: null,
              who: 'Alice'
               });
            const v103 = stdlib.sub(stdlib.checkedBigNumberify('./index.rsh:7:18:decimal', stdlib.UInt_max, 4), v80);
            const v104 = stdlib.add(v91, v103);
            const v105 = stdlib.mod(v104, stdlib.checkedBigNumberify('./index.rsh:7:32:decimal', stdlib.UInt_max, 3));
            const cv50 = v105;
            const cv203 = v96;
            const cv204 = v97;
            const cv205 = v86;
            
            v50 = cv50;
            v203 = cv203;
            v204 = cv204;
            v205 = cv205;
            
            continue; }
           }
         }
       }
    const v167 = stdlib.eq(v50, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
    const v170 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:83:16:decimal', stdlib.UInt_max, 2), v36);
    const v172 = v167 ? v35 : v44;
    ;
    stdlib.protect(stdlib.T_Null, await interact.seeOutcome(v50), {
      at: './index.rsh:87:28:application',
      fs: ['at ./index.rsh:86:11:application call to [unknown function] (defined at: ./index.rsh:86:23:function exp)'],
      msg: 'seeOutcome',
      who: 'Alice'
       });
    return; }
  
  
   }
export async function Bob(ctc, interact) {
  const stdlib = ctc.stdlib;
  
  const v32 = await ctc.creationTime();
  const txn1 = await (ctc.recv('Bob', 1, 1, [stdlib.T_UInt], false, false));
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
  stdlib.protect(stdlib.T_Null, await interact.acceptWager(v36), {
    at: './index.rsh:49:29:application',
    fs: ['at ./index.rsh:48:13:application call to [unknown function] (defined at: ./index.rsh:48:17:function exp)'],
    msg: 'acceptWager',
    who: 'Bob'
     });
  const txn2 = await (ctc.sendrecv('Bob', 2, 0, stdlib.checkedBigNumberify('./index.rsh:50:9:dot', stdlib.UInt_max, 3), [stdlib.T_Address, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt], [v35, v36, v40, v41], v36, [], true, true, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10), ((txn2) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:50:9:dot', stdlib.UInt_max, 1), v35, v36, v40, v41]);
    sim_r.prevSt_noPrevTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:50:9:dot', stdlib.UInt_max, 1), v35, v36, v40]);
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
    const v205 = v41;
    
    if ((() => {
      const v60 = stdlib.eq(v50, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
      
      return v60; })()) {
      sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:56:17:after expr stmt semicolon', stdlib.UInt_max, 4), v35, v36, v44, v203, v204]);
      sim_r.nextSt_noTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:56:17:after expr stmt semicolon', stdlib.UInt_max, 4), v35, v36, v44, v203]);
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
      sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([]), []);
      sim_r.nextSt_noTime = stdlib.digest(stdlib.T_Tuple([]), []);
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
      fs: ['at ./index.rsh:51:41:application call to [unknown function] (defined at: reach standard library:65:8:function exp)'],
      msg: 'pay amount correct',
      who: 'Bob'
       });
    const v187 = stdlib.addressEq(v35, v184);
    stdlib.assert(v187, {
      at: 'reach standard library:66:7:dot',
      fs: ['at ./index.rsh:51:41:application call to [unknown function] (defined at: reach standard library:65:8:function exp)'],
      msg: 'sender correct',
      who: 'Bob'
       });
    const v189 = stdlib.add(v40, v185);
    ;
    stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
      at: './index.rsh:40:33:application',
      fs: ['at ./index.rsh:39:13:application call to [unknown function] (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:69:8:application call to [unknown function] (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:51:41:application call to [unknown function] (defined at: reach standard library:65:8:function exp)'],
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
    let v205 = v41;
    
    while ((() => {
      const v60 = stdlib.eq(v50, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
      
      return v60; })()) {
      const txn3 = await (ctc.recv('Bob', 6, 1, [stdlib.T_Digest], false, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10)));
      if (txn3.didTimeout) {
        const txn4 = await (ctc.sendrecv('Bob', 7, 0, stdlib.checkedBigNumberify('reach standard library:66:7:dot', stdlib.UInt_max, 4), [stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_UInt], [v35, v36, v44, v203, v204], stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), [], true, true, false, ((txn4) => {
          const sim_r = { txns: [] };
          sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('reach standard library:66:7:dot', stdlib.UInt_max, 4), v35, v36, v44, v203, v204]);
          sim_r.prevSt_noPrevTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt]), [stdlib.checkedBigNumberify('reach standard library:66:7:dot', stdlib.UInt_max, 4), v35, v36, v44, v203]);
          const [] = txn4.data;
          const v149 = txn4.value;
          const v154 = txn4.time;
          const v148 = txn4.from;
          
          const v150 = stdlib.eq(v149, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
          stdlib.assert(v150, {
            at: 'reach standard library:66:7:dot',
            fs: ['at ./index.rsh:63:43:application call to [unknown function] (defined at: reach standard library:65:8:function exp)'],
            msg: 'pay amount correct',
            who: 'Bob'
             });
          const v151 = stdlib.addressEq(v44, v148);
          stdlib.assert(v151, {
            at: 'reach standard library:66:7:dot',
            fs: ['at ./index.rsh:63:43:application call to [unknown function] (defined at: reach standard library:65:8:function exp)'],
            msg: 'sender correct',
            who: 'Bob'
             });
          const v153 = stdlib.add(v203, v149);
          sim_r.txns.push({
            amt: v153,
            to: v44
             });
          sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([]), []);
          sim_r.nextSt_noTime = stdlib.digest(stdlib.T_Tuple([]), []);
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
          fs: ['at ./index.rsh:63:43:application call to [unknown function] (defined at: reach standard library:65:8:function exp)'],
          msg: 'pay amount correct',
          who: 'Bob'
           });
        const v151 = stdlib.addressEq(v44, v148);
        stdlib.assert(v151, {
          at: 'reach standard library:66:7:dot',
          fs: ['at ./index.rsh:63:43:application call to [unknown function] (defined at: reach standard library:65:8:function exp)'],
          msg: 'sender correct',
          who: 'Bob'
           });
        const v153 = stdlib.add(v203, v149);
        ;
        stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
          at: './index.rsh:40:33:application',
          fs: ['at ./index.rsh:39:13:application call to [unknown function] (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:69:8:application call to [unknown function] (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:63:43:application call to [unknown function] (defined at: reach standard library:65:8:function exp)'],
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
        const v78 = stdlib.protect(stdlib.T_UInt, await interact.getHand(), {
          at: './index.rsh:68:52:application',
          fs: ['at ./index.rsh:67:15:application call to [unknown function] (defined at: ./index.rsh:67:19:function exp)'],
          msg: 'getHand',
          who: 'Bob'
           });
        const txn4 = await (ctc.sendrecv('Bob', 8, 1, stdlib.checkedBigNumberify('./index.rsh:69:11:dot', stdlib.UInt_max, 5), [stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt], [v35, v36, v44, v69, v74, v75, v78], stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), [stdlib.T_UInt], true, true, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10), ((txn4) => {
          const sim_r = { txns: [] };
          sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:69:11:dot', stdlib.UInt_max, 6), v35, v36, v44, v69, v74, v75]);
          sim_r.prevSt_noPrevTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:69:11:dot', stdlib.UInt_max, 6), v35, v36, v44, v69, v74]);
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
          sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:71:17:after expr stmt semicolon', stdlib.UInt_max, 8), v35, v36, v44, v69, v80, v85, v86]);
          sim_r.nextSt_noTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:71:17:after expr stmt semicolon', stdlib.UInt_max, 8), v35, v36, v44, v69, v80, v85]);
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
            fs: ['at ./index.rsh:70:43:application call to [unknown function] (defined at: reach standard library:65:8:function exp)'],
            msg: 'pay amount correct',
            who: 'Bob'
             });
          const v131 = stdlib.addressEq(v35, v128);
          stdlib.assert(v131, {
            at: 'reach standard library:66:7:dot',
            fs: ['at ./index.rsh:70:43:application call to [unknown function] (defined at: reach standard library:65:8:function exp)'],
            msg: 'sender correct',
            who: 'Bob'
             });
          const v133 = stdlib.add(v74, v129);
          ;
          stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
            at: './index.rsh:40:33:application',
            fs: ['at ./index.rsh:39:13:application call to [unknown function] (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:69:8:application call to [unknown function] (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:70:43:application call to [unknown function] (defined at: reach standard library:65:8:function exp)'],
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
          const txn5 = await (ctc.recv('Bob', 10, 2, [stdlib.T_UInt, stdlib.T_UInt], false, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10)));
          if (txn5.didTimeout) {
            const txn6 = await (ctc.sendrecv('Bob', 11, 0, stdlib.checkedBigNumberify('reach standard library:66:7:dot', stdlib.UInt_max, 6), [stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt], [v35, v36, v44, v69, v80, v85, v86], stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), [], true, true, false, ((txn6) => {
              const sim_r = { txns: [] };
              sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('reach standard library:66:7:dot', stdlib.UInt_max, 8), v35, v36, v44, v69, v80, v85, v86]);
              sim_r.prevSt_noPrevTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('reach standard library:66:7:dot', stdlib.UInt_max, 8), v35, v36, v44, v69, v80, v85]);
              const [] = txn6.data;
              const v109 = txn6.value;
              const v114 = txn6.time;
              const v108 = txn6.from;
              
              const v110 = stdlib.eq(v109, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
              stdlib.assert(v110, {
                at: 'reach standard library:66:7:dot',
                fs: ['at ./index.rsh:76:43:application call to [unknown function] (defined at: reach standard library:65:8:function exp)'],
                msg: 'pay amount correct',
                who: 'Bob'
                 });
              const v111 = stdlib.addressEq(v44, v108);
              stdlib.assert(v111, {
                at: 'reach standard library:66:7:dot',
                fs: ['at ./index.rsh:76:43:application call to [unknown function] (defined at: reach standard library:65:8:function exp)'],
                msg: 'sender correct',
                who: 'Bob'
                 });
              const v113 = stdlib.add(v85, v109);
              sim_r.txns.push({
                amt: v113,
                to: v44
                 });
              sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([]), []);
              sim_r.nextSt_noTime = stdlib.digest(stdlib.T_Tuple([]), []);
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
              fs: ['at ./index.rsh:76:43:application call to [unknown function] (defined at: reach standard library:65:8:function exp)'],
              msg: 'pay amount correct',
              who: 'Bob'
               });
            const v111 = stdlib.addressEq(v44, v108);
            stdlib.assert(v111, {
              at: 'reach standard library:66:7:dot',
              fs: ['at ./index.rsh:76:43:application call to [unknown function] (defined at: reach standard library:65:8:function exp)'],
              msg: 'sender correct',
              who: 'Bob'
               });
            const v113 = stdlib.add(v85, v109);
            ;
            stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
              at: './index.rsh:40:33:application',
              fs: ['at ./index.rsh:39:13:application call to [unknown function] (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:69:8:application call to [unknown function] (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:76:43:application call to [unknown function] (defined at: reach standard library:65:8:function exp)'],
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
            const v99 = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt]), [v90, v91]);
            const v100 = stdlib.eq(v69, v99);
            stdlib.assert(v100, {
              at: 'reach standard library:63:17:application',
              fs: ['at ./index.rsh:77:24:application call to [unknown function] (defined at: reach standard library:62:8:function exp)'],
              msg: null,
              who: 'Bob'
               });
            const v103 = stdlib.sub(stdlib.checkedBigNumberify('./index.rsh:7:18:decimal', stdlib.UInt_max, 4), v80);
            const v104 = stdlib.add(v91, v103);
            const v105 = stdlib.mod(v104, stdlib.checkedBigNumberify('./index.rsh:7:32:decimal', stdlib.UInt_max, 3));
            const cv50 = v105;
            const cv203 = v96;
            const cv204 = v97;
            const cv205 = v86;
            
            v50 = cv50;
            v203 = cv203;
            v204 = cv204;
            v205 = cv205;
            
            continue; }
           }
         }
       }
    const v167 = stdlib.eq(v50, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
    const v170 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:83:16:decimal', stdlib.UInt_max, 2), v36);
    const v172 = v167 ? v35 : v44;
    ;
    stdlib.protect(stdlib.T_Null, await interact.seeOutcome(v50), {
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
gtxna 2 Args 0
==
assert
byte base64(bA==)
app_global_get
gtxna 2 Args 4
btoi
==
assert
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
txn NumArgs
int 6
==
assert
// compute state in HM_Check 0
int 0
itob
keccak256
arg 0
==
assert
// Run body
// Just "pay amount correct"
// "./index.rsh:44:9:dot"
// "[]"
gtxn 3 Amount
arg 3
btoi
-
arg 5
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
arg 3
btoi
-
+
store 255
// compute state in HM_Set 1
int 1
itob
gtxn 3 Sender
concat
arg 5
concat
load 255
itob
concat
keccak256
arg 1
==
assert
arg 2
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
arg 3
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
txn NumArgs
int 8
==
assert
// compute state in HM_Check 1
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
assert
// Run body
// Just "pay amount correct"
// "./index.rsh:50:9:dot"
// "[]"
gtxn 3 Amount
arg 3
btoi
-
arg 6
btoi
==
assert
// Just "sender correct"
// "./index.rsh:50:9:dot"
// "[]"
int 1
assert
arg 7
btoi
gtxn 3 Amount
arg 3
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
arg 5
concat
arg 6
concat
gtxn 3 Sender
concat
load 255
itob
concat
keccak256
arg 1
==
assert
arg 2
btoi
int 0
==
assert
b done
l0:
gtxn 4 TypeEnum
int pay
==
assert
gtxn 4 Receiver
int 1
int 2
==
arg 5
gtxn 3 Sender
ite
==
assert
gtxn 4 Amount
int 2
arg 6
btoi
*
==
assert
gtxn 4 Sender
byte "{{ContractAddr}}"
==
assert
arg 2
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
arg 3
btoi
gtxn 4 Fee
==
assert
// Check time limits
arg 4
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
txn NumArgs
int 8
==
assert
// compute state in HM_Check 1
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
assert
// Run body
// Just "pay amount correct"
// "reach standard library:66:7:dot"
// "[at ./index.rsh:51:41:application call to [unknown function] (defined at: reach standard library:65:8:function exp)]"
gtxn 3 Amount
arg 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "reach standard library:66:7:dot"
// "[at ./index.rsh:51:41:application call to [unknown function] (defined at: reach standard library:65:8:function exp)]"
arg 5
gtxn 3 Sender
==
assert
gtxn 4 TypeEnum
int pay
==
assert
gtxn 4 Receiver
arg 5
==
assert
gtxn 4 Amount
arg 7
btoi
gtxn 3 Amount
arg 3
btoi
-
+
==
assert
gtxn 4 Sender
byte "{{ContractAddr}}"
==
assert
arg 2
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
arg 3
btoi
gtxn 4 Fee
==
assert
// Check time limits
arg 4
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
txn NumArgs
int 10
==
assert
// compute state in HM_Check 4
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
keccak256
arg 0
==
assert
// Run body
// Just "pay amount correct"
// "./index.rsh:62:11:dot"
// "[]"
gtxn 3 Amount
arg 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "./index.rsh:62:11:dot"
// "[]"
arg 5
gtxn 3 Sender
==
assert
arg 8
btoi
gtxn 3 Amount
arg 3
btoi
-
+
store 255
// compute state in HM_Set 6
int 6
itob
arg 5
concat
arg 6
concat
arg 7
concat
arg 9
concat
load 255
itob
concat
keccak256
arg 1
==
assert
arg 2
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
arg 3
btoi
int 0
==
assert
// Check time limits
arg 4
btoi
int 10
int 10
+
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
txn NumArgs
int 9
==
assert
// compute state in HM_Check 4
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
keccak256
arg 0
==
assert
// Run body
// Just "pay amount correct"
// "reach standard library:66:7:dot"
// "[at ./index.rsh:63:43:application call to [unknown function] (defined at: reach standard library:65:8:function exp)]"
gtxn 3 Amount
arg 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "reach standard library:66:7:dot"
// "[at ./index.rsh:63:43:application call to [unknown function] (defined at: reach standard library:65:8:function exp)]"
arg 7
gtxn 3 Sender
==
assert
gtxn 4 TypeEnum
int pay
==
assert
gtxn 4 Receiver
arg 7
==
assert
gtxn 4 Amount
arg 8
btoi
gtxn 3 Amount
arg 3
btoi
-
+
==
assert
gtxn 4 Sender
byte "{{ContractAddr}}"
==
assert
arg 2
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
arg 3
btoi
gtxn 4 Fee
==
assert
// Check time limits
arg 4
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
txn NumArgs
int 11
==
assert
// compute state in HM_Check 6
int 6
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
assert
// Run body
// Just "pay amount correct"
// "./index.rsh:69:11:dot"
// "[]"
gtxn 3 Amount
arg 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "./index.rsh:69:11:dot"
// "[]"
arg 7
gtxn 3 Sender
==
assert
arg 9
btoi
gtxn 3 Amount
arg 3
btoi
-
+
store 255
// compute state in HM_Set 8
int 8
itob
arg 5
concat
arg 6
concat
arg 7
concat
arg 8
concat
arg 10
concat
load 255
itob
concat
keccak256
arg 1
==
assert
arg 2
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
arg 3
btoi
int 0
==
assert
// Check time limits
arg 4
btoi
int 10
int 10
+
int 10
+
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
txn NumArgs
int 10
==
assert
// compute state in HM_Check 6
int 6
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
assert
// Run body
// Just "pay amount correct"
// "reach standard library:66:7:dot"
// "[at ./index.rsh:70:43:application call to [unknown function] (defined at: reach standard library:65:8:function exp)]"
gtxn 3 Amount
arg 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "reach standard library:66:7:dot"
// "[at ./index.rsh:70:43:application call to [unknown function] (defined at: reach standard library:65:8:function exp)]"
arg 5
gtxn 3 Sender
==
assert
gtxn 4 TypeEnum
int pay
==
assert
gtxn 4 Receiver
arg 5
==
assert
gtxn 4 Amount
arg 9
btoi
gtxn 3 Amount
arg 3
btoi
-
+
==
assert
gtxn 4 Sender
byte "{{ContractAddr}}"
==
assert
arg 2
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
arg 3
btoi
gtxn 4 Fee
==
assert
// Check time limits
arg 4
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
txn NumArgs
int 13
==
assert
// compute state in HM_Check 8
int 8
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
assert
// Run body
// Just "pay amount correct"
// "./index.rsh:75:11:dot"
// "[]"
gtxn 3 Amount
arg 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "./index.rsh:75:11:dot"
// "[]"
arg 5
gtxn 3 Sender
==
assert
arg 10
btoi
gtxn 3 Amount
arg 3
btoi
-
+
store 255
// Nothing
// "reach standard library:63:17:application"
// "[at ./index.rsh:77:24:application call to [unknown function] (defined at: reach standard library:62:8:function exp)]"
arg 8
arg 11
arg 12
concat
keccak256
==
assert
arg 12
btoi
int 4
arg 9
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
arg 5
concat
arg 6
concat
arg 7
concat
load 255
itob
concat
keccak256
arg 1
==
assert
arg 2
btoi
int 0
==
assert
b done
l0:
gtxn 4 TypeEnum
int pay
==
assert
gtxn 4 Receiver
load 254
int 2
==
arg 5
arg 7
ite
==
assert
gtxn 4 Amount
int 2
arg 6
btoi
*
==
assert
gtxn 4 Sender
byte "{{ContractAddr}}"
==
assert
arg 2
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
arg 3
btoi
gtxn 4 Fee
==
assert
// Check time limits
arg 4
btoi
int 10
int 10
+
int 10
+
int 10
+
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
txn NumArgs
int 11
==
assert
// compute state in HM_Check 8
int 8
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
assert
// Run body
// Just "pay amount correct"
// "reach standard library:66:7:dot"
// "[at ./index.rsh:76:43:application call to [unknown function] (defined at: reach standard library:65:8:function exp)]"
gtxn 3 Amount
arg 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "reach standard library:66:7:dot"
// "[at ./index.rsh:76:43:application call to [unknown function] (defined at: reach standard library:65:8:function exp)]"
arg 7
gtxn 3 Sender
==
assert
gtxn 4 TypeEnum
int pay
==
assert
gtxn 4 Receiver
arg 7
==
assert
gtxn 4 Amount
arg 10
btoi
gtxn 3 Amount
arg 3
btoi
-
+
==
assert
gtxn 4 Sender
byte "{{ContractAddr}}"
==
assert
arg 2
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
arg 3
btoi
gtxn 4 Fee
==
assert
// Check time limits
arg 4
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
            "internalType": "struct ReachContract.a0postsvs",
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
            "internalType": "struct ReachContract.a1msg",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct ReachContract.a1",
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
            "internalType": "struct ReachContract.a8postsvs",
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
            "internalType": "struct ReachContract.a10msg",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct ReachContract.a10",
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
            "internalType": "struct ReachContract.a8postsvs",
            "name": "svs",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct ReachContract.a11",
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
            "internalType": "struct ReachContract.a1postsvs",
            "name": "svs",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct ReachContract.a2",
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
            "internalType": "struct ReachContract.a1postsvs",
            "name": "svs",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct ReachContract.a3",
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
            "internalType": "struct ReachContract.a4postsvs",
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
            "internalType": "struct ReachContract.a6msg",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct ReachContract.a6",
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
            "internalType": "struct ReachContract.a4postsvs",
            "name": "svs",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct ReachContract.a7",
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
            "internalType": "struct ReachContract.a6postsvs",
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
            "internalType": "struct ReachContract.a8msg",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct ReachContract.a8",
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
            "internalType": "struct ReachContract.a6postsvs",
            "name": "svs",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct ReachContract.a9",
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
            "internalType": "struct ReachContract.a0postsvs",
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
            "internalType": "struct ReachContract.a1msg",
            "name": "msg",
            "type": "tuple"
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
            "internalType": "struct ReachContract.a8postsvs",
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
            "internalType": "struct ReachContract.a10msg",
            "name": "msg",
            "type": "tuple"
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
            "internalType": "struct ReachContract.a8postsvs",
            "name": "svs",
            "type": "tuple"
          }
        ],
        "internalType": "struct ReachContract.a11",
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
            "internalType": "struct ReachContract.a1postsvs",
            "name": "svs",
            "type": "tuple"
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
            "internalType": "struct ReachContract.a1postsvs",
            "name": "svs",
            "type": "tuple"
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
            "internalType": "struct ReachContract.a4postsvs",
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
            "internalType": "struct ReachContract.a6msg",
            "name": "msg",
            "type": "tuple"
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
            "internalType": "struct ReachContract.a4postsvs",
            "name": "svs",
            "type": "tuple"
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
            "internalType": "struct ReachContract.a6postsvs",
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
            "internalType": "struct ReachContract.a8msg",
            "name": "msg",
            "type": "tuple"
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
            "internalType": "struct ReachContract.a6postsvs",
            "name": "svs",
            "type": "tuple"
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
  Bytecode: `0x608060408190527f49ff028a829527a47ec6839c7147b484eccf5a2a94853eddac09cef44d9d4e9e90600090a161003461007a565b43815261003f61007a565b8151815260405161005790600090839060200161008d565b60408051601f1981840301815291905280516020909101206000555061009c9050565b6040518060200160405280600081525090565b91825251602082015260400190565b61147a806100ab6000396000f3fe6080604052600436106100865760003560e01c80638b513b1f116100595780638b513b1f146100d95780638d20bc42146100ec5780639532ef01146100ff578063dc0f106b14610112578063f5aa5a421461012557610086565b80632e6453cc1461008b57806332a5928d146100a05780636cc4a844146100b35780636dacfd6f146100c6575b600080fd5b61009e610099366004611017565b610138565b005b61009e6100ae366004610fe1565b610257565b61009e6100c1366004610ffc565b610361565b61009e6100d4366004610fb5565b6104d1565b61009e6100e7366004610fe1565b610639565b61009e6100fa366004610fb5565b610754565b61009e61010d366004610fd0565b610864565b61009e610120366004610fa3565b610931565b61009e610133366004610ffc565b610b08565b60405161014c906004908390602001611271565b6040516020818303038152906040528051906020012060001c6000541461017257600080fd5b610181600a60808301356113c0565b431015801561018e575060015b61019757600080fd5b34156101a257600080fd5b336101b36060830160408401610f82565b6001600160a01b0316146101c657600080fd5b6101d66060820160408301610f82565b6001600160a01b03166108fc6101f03460608501356113c0565b6040518115909202916000818181858888f19350505050158015610218573d6000803e3d6000fd5b507f30cb8287e966e3d7021b3ac9899109f2f8fdb380b1791d6f301768e4174fe1f88160405161024891906111da565b60405180910390a16000805533ff5b60405161026b906001908390602001611221565b6040516020818303038152906040528051906020012060001c6000541461029157600080fd5b6102a0600a60608301356113c0565b43101580156102ad575060015b6102b657600080fd5b34156102c157600080fd5b336102cf6020830183610f82565b6001600160a01b0316146102e257600080fd5b6102ef6020820182610f82565b6001600160a01b03166108fc6103093460408501356113c0565b6040518115909202916000818181858888f19350505050158015610331573d6000803e3d6000fd5b507f88b9720f2f4655921324cbb52751bf1684df44f57ba750f9a50c5a6cf87f14388160405161024891906111b0565b604051610375906004908390602001611271565b6040516020818303038152906040528051906020012060001c6000541461039b57600080fd5b6103a3610d57565b600a6103b38160808501356113c0565b6103bd91906113c0565b43106103c857600080fd5b34156103d357600080fd5b336103e16020840184610f82565b6001600160a01b0316146103f457600080fd5b6104023460608401356113c0565b81526040517f16424d059cabc243859f670786693b7e657c3f04cbc39631fa14608999bfaef9906104349084906111be565b60405180910390a1610444610d6a565b6104516020840184610f82565b6001600160a01b03168152602080840135908201526104766060840160408501610f82565b6001600160a01b031660408083019190915260a0808501356060840152835160808401524390830152516104b19060069083906020016112e4565b60408051601f198184030181529190528051602090910120600055505050565b6040516104e59060069083906020016112d0565b6040516020818303038152906040528051906020012060001c6000541461050b57600080fd5b610513610d57565b600a806105248160a08601356113c0565b61052e91906113c0565b61053891906113c0565b431061054357600080fd5b341561054e57600080fd5b3361055f6060840160408501610f82565b6001600160a01b03161461057257600080fd5b6105803460808401356113c0565b81526040517fa03e2b199cbd4c163bca89aa8e3581bcf82ee511c6ed7b600ee5e8a3e78842b4906105b29084906111e8565b60405180910390a16105c2610db2565b6105cf6020840184610f82565b6001600160a01b03168152602080840135908201526105f46060840160408501610f82565b6001600160a01b03166040808301919091526060808501359083015260c0808501356080840152835160a08401524390830152516104b1906008908390602001611350565b60405161064d906001908390602001611221565b6040516020818303038152906040528051906020012060001c6000541461067357600080fd5b610682600a60608301356113c0565b431061068d57600080fd5b3460208201351461069d57600080fd5b7f1b6c319555642e7ccd14c9ec83bd3a26d9469f9b1c26d4fa430499a5eb7ec09c816040516106cc91906111b0565b60405180910390a16106dc610e01565b6106e96020830183610f82565b81516001600160a01b03909116905280516020808401359181019190915281513360409182015290820151600190526107269034908401356113c0565b60208083018051909101919091528051436040909101525160608084013591015261075081610c12565b5050565b60405161076890600890839060200161133b565b6040516020818303038152906040528051906020012060001c6000541461078e57600080fd5b61079d600a60c08301356113c0565b43101580156107aa575060015b6107b357600080fd5b34156107be57600080fd5b336107cf6060830160408401610f82565b6001600160a01b0316146107e257600080fd5b6107f26060820160408301610f82565b6001600160a01b03166108fc61080c3460a08501356113c0565b6040518115909202916000818181858888f19350505050158015610834573d6000803e3d6000fd5b507fff606702526c61dfd9001dc8007dc7aa69588310924937cca3d16cf053a8912b816040516102489190611185565b604051610878906000908390602001611212565b6040516020818303038152906040528051906020012060001c6000541461089e57600080fd5b6108a6610d57565b346020830135146108b657600080fd5b6108c13460006113c0565b81526040517ff2c62eba998811305a23599b2e6d212befbd7ded3a73f4c08bfb9aefe08dc166906108f3908490611199565b60405180910390a1610903610e26565b338152602080840135818301528251604080840191909152436060840152516104b191600191849101611235565b60405161094590600890839060200161133b565b6040516020818303038152906040528051906020012060001c6000541461096b57600080fd5b600a808061097d8160c08601356113c0565b61098791906113c0565b61099191906113c0565b61099b91906113c0565b43106109a657600080fd5b34156109b157600080fd5b336109bf6020830183610f82565b6001600160a01b0316146109d257600080fd5b6040516109ee9060e083013590610100840135906020016113b2565b60408051601f198184030181529190528051602090910120606082013514610a1557600080fd5b7f352ea7fc48371f0bd43d7d1ad042d3e6a673947e2deccddfd368812813abc47281604051610a44919061115a565b60405180910390a1610a54610e01565b610a616020830183610f82565b81516001600160a01b0390911690528051602080840135910152610a8b6060830160408401610f82565b81516001600160a01b039091166040909101526003610aaf608084013560046113f7565b610abe906101008501356113c0565b610ac8919061140e565b602082015152610adc3460a08401356113c0565b60208083018051909101919091528051436040909101525160c083013560609091015261075081610c12565b604051610b1c9060069083906020016112d0565b6040516020818303038152906040528051906020012060001c60005414610b4257600080fd5b610b51600a60a08301356113c0565b4310158015610b5e575060015b610b6757600080fd5b3415610b7257600080fd5b33610b806020830183610f82565b6001600160a01b031614610b9357600080fd5b610ba06020820182610f82565b6001600160a01b03166108fc610bba3460808501356113c0565b6040518115909202916000818181858888f19350505050158015610be2573d6000803e3d6000fd5b507f0608b566092f92bab36d763cae7ac63f95ffcb7a407274c378383f52d53d68bb816040516102489190611204565b60208101515160011415610c9b57610c28610e57565b8151516001600160a01b039081168252825160209081015181840152835160409081015190921682840152808401805182015160608501525182015160808401529051610c7a91600491849101611285565b60408051601f19818403018152919052805160209091012060005550610ce7565b610ca3610e98565b81515181516001600160a01b039182169052825160209081015183518201528351604090810151845193169201919091528201515181516060015261075081610cea565b50565b805160600151600214610d0257805160400151610d06565b8051515b6001600160a01b03166108fc8260000151602001516002610d2791906113d8565b6040518115909202916000818181858888f19350505050158015610d4f573d6000803e3d6000fd5b506000805533ff5b6040518060200160405280600081525090565b6040518060c0016040528060006001600160a01b031681526020016000815260200160006001600160a01b031681526020016000815260200160008152602001600081525090565b6040518060e0016040528060006001600160a01b031681526020016000815260200160006001600160a01b03168152602001600081526020016000815260200160008152602001600081525090565b6040518060400160405280610e14610eab565b8152602001610e21610ecb565b905290565b604051806080016040528060006001600160a01b031681526020016000815260200160008152602001600081525090565b6040518060a0016040528060006001600160a01b031681526020016000815260200160006001600160a01b0316815260200160008152602001600081525090565b6040518060200160405280610e21610ef3565b604080516060810182526000808252602082018190529181019190915290565b6040518060800160405280600081526020016000815260200160008152602001600081525090565b604051806080016040528060006001600160a01b031681526020016000815260200160006001600160a01b03168152602001600081525090565b80356001600160a01b0381168114610f4457600080fd5b919050565b600060e08284031215610f5a578081fd5b50919050565b600060808284031215610f5a578081fd5b600060c08284031215610f5a578081fd5b600060208284031215610f93578081fd5b610f9c82610f2d565b9392505050565b60006101208284031215610f5a578081fd5b600060e08284031215610fc6578081fd5b610f9c8383610f49565b600060408284031215610f5a578081fd5b600060808284031215610ff2578081fd5b610f9c8383610f60565b600060c0828403121561100d578081fd5b610f9c8383610f71565b600060a08284031215610f5a578081fd5b6001600160a01b0361103982610f2d565b1682526020810135602083015260408101356040830152606081013560608301525050565b6001600160a01b038061107083610f2d565b168352602082013560208401528061108a60408401610f2d565b1660408401525060608181013590830152608090810135910152565b6001600160a01b03806110b883610f2d565b16835260208201356020840152806110d260408401610f2d565b16604084015250606081013560608301526080810135608083015260a081013560a08301525050565b6001600160a01b038061110d83610f2d565b168352602082013560208401528061112760408401610f2d565b16604084015250606081013560608301526080810135608083015260a081013560a083015260c081013560c08301525050565b610120810161116982846110fb565b60e083013560e083015261010080840135818401525092915050565b60e0810161119382846110fb565b92915050565b813581526020918201359181019190915260400190565b608081016111938284611028565b60c081016111cc828461105e565b60a092830135919092015290565b60a08101611193828461105e565b60e081016111f682846110a6565b60c092830135919092015290565b60c0810161119382846110a6565b91825235602082015260400190565b82815260a08101610f9c6020830184611028565b91825280516001600160a01b03166020808401919091528101516040808401919091528101516060808401919091520151608082015260a00190565b82815260c08101610f9c602083018461105e565b91825280516001600160a01b0390811660208085019190915282015160408085019190915282015116606080840191909152810151608080840191909152015160a082015260c00190565b82815260e08101610f9c60208301846110a6565b600060e08201905083825260018060a01b03808451166020840152602084015160408401528060408501511660608401525060608301516080830152608083015160a083015260a083015160c08301529392505050565b8281526101008101610f9c60208301846110fb565b60006101008201905083825260018060a01b03808451166020840152602084015160408401528060408501511660608401525060608301516080830152608083015160a083015260a083015160c083015260c083015160e08301529392505050565b918252602082015260400190565b600082198211156113d3576113d361142e565b500190565b60008160001904831182151516156113f2576113f261142e565b500290565b6000828210156114095761140961142e565b500390565b60008261142957634e487b7160e01b81526012600452602481fd5b500690565b634e487b7160e01b600052601160045260246000fdfea2646970667358221220a9272ac970e384ea103313ff91ef94e2b00d881501bbe8ba2a9f2a35b547df0764736f6c63430008000033`,
  deployMode: `DM_constructor`
   };

export const _Connectors = {
  ALGO: _ALGO,
  ETH: _ETH
   };

