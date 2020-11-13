// Automatically generated with Reach 0.1.2
/* eslint-disable no-unused-vars, no-empty-pattern, no-useless-escape, no-loop-func */
export const _version = '0.1.2';


export async function Alice(stdlib, ctc, interact) {
  const txn1 = await ctc.sendrecv('Alice', 1, 1, [stdlib.T_UInt], [stdlib.protect(stdlib.T_UInt, interact.wager, null)], stdlib.protect(stdlib.T_UInt, interact.wager, null), [stdlib.T_UInt], false, ((txn1) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:44:8:dot', stdlib.UInt_max, 0)]);
    const [v34] = txn1.data;
    const v36 = txn1.value;
    const v35 = txn1.from;
    
    const v37 = stdlib.eq(v36, v34);
    stdlib.assert(v37, {
      at: './index.rsh:45:20:after expr stmt semicolon',
      fs: [],
      msg: 'pay amount correct',
      who: 'Alice'
       });
    const v38 = stdlib.checkedBigNumberify('./index.rsh:compileDApp', stdlib.UInt_max, 0);
    const v41 = stdlib.add(v38, v36);
    sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:46:15:after expr stmt semicolon', stdlib.UInt_max, 1), v41, v35, v34]);
    sim_r.isHalt = false;
    
    return sim_r;
     }));
  const [v34] = txn1.data;
  const v36 = txn1.value;
  const v35 = txn1.from;
  const v37 = stdlib.eq(v36, v34);
  stdlib.assert(v37, {
    at: './index.rsh:45:20:after expr stmt semicolon',
    fs: [],
    msg: 'pay amount correct',
    who: 'Alice'
     });
  const v38 = stdlib.checkedBigNumberify('./index.rsh:compileDApp', stdlib.UInt_max, 0);
  const v41 = stdlib.add(v38, v36);
  const txn2 = await ctc.recv('Alice', 2, 0, [], stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10));
  if (txn2.didTimeout) {
    const txn3 = await ctc.sendrecv('Alice', 10, 0, [stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt], [v41, v35, v34], stdlib.checkedBigNumberify('reach standard library:77:16:after expr stmt semicolon', stdlib.UInt_max, 0), [], false, ((txn3) => {
      const sim_r = { txns: [] };
      sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt]), [stdlib.checkedBigNumberify('reach standard library:77:6:dot', stdlib.UInt_max, 1), v41, v35, v34]);
      const [] = txn3.data;
      const v49 = txn3.value;
      
      const v50 = stdlib.eq(v49, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
      stdlib.assert(v50, {
        at: 'reach standard library:77:16:after expr stmt semicolon',
        fs: ['at ./index.rsh:51:41:application call to "function" (defined at: reach standard library:76:8:function exp)'],
        msg: 'pay amount correct',
        who: 'Alice'
         });
      const v51 = v41;
      const v54 = stdlib.add(v51, v49);
      const v55 = v54;
      sim_r.txns.push({
        amt: v55,
        to: v35
         });
      sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([]), []);
      sim_r.isHalt = true;
      
      return sim_r;
       }));
    const [] = txn3.data;
    const v49 = txn3.value;
    const v50 = stdlib.eq(v49, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
    stdlib.assert(v50, {
      at: 'reach standard library:77:16:after expr stmt semicolon',
      fs: ['at ./index.rsh:51:41:application call to "function" (defined at: reach standard library:76:8:function exp)'],
      msg: 'pay amount correct',
      who: 'Alice'
       });
    const v51 = v41;
    const v54 = stdlib.add(v51, v49);
    const v55 = v54;
    ;
    stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
      at: './index.rsh:40:33:application',
      fs: ['at ./index.rsh:40:39:after expr stmt semicolon call to "function" (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:80:8:application call to "function" (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:51:41:application call to "function" (defined at: reach standard library:76:8:function exp)'],
      msg: 'informTimeout',
      who: 'Alice'
       });
    return;
     }
  else {
    const [] = txn2.data;
    const v46 = txn2.value;
    const v45 = txn2.from;
    const v47 = stdlib.eq(v46, v34);
    stdlib.assert(v47, {
      at: './index.rsh:51:60:after expr stmt semicolon',
      fs: [],
      msg: 'pay amount correct',
      who: 'Alice'
       });
    const v70 = v41;
    const v73 = stdlib.add(v70, v46);
    const v74 = v73;
    let v75 = v74;
    let v76 = stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1);
    
    while ((() => {
      const v88 = stdlib.eq(v76, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
      
      return v88; })()) {
      const v91 = stdlib.protect(stdlib.T_UInt, await interact.getHand(), {
        at: './index.rsh:59:42:application',
        fs: ['at ./index.rsh:61:51:after expr stmt semicolon call to "function" (defined at: ./index.rsh:58:19:function exp)'],
        msg: 'getHand',
        who: 'Alice'
         });
      const v93 = stdlib.protect(stdlib.T_UInt, await interact.random(), {
        at: 'reach standard library:69:31:application',
        fs: ['at ./index.rsh:60:52:application call to "function" (defined at: reach standard library:68:8:function exp)', 'at ./index.rsh:61:51:after expr stmt semicolon call to "function" (defined at: ./index.rsh:58:19:function exp)'],
        msg: 'random',
        who: 'Alice'
         });
      const v94 = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt]), [v93, v91]);
      const txn3 = await ctc.sendrecv('Alice', 4, 1, [stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Digest], [v35, v34, v45, v75, v94], stdlib.checkedBigNumberify('./index.rsh:63:62:after expr stmt semicolon', stdlib.UInt_max, 0), [stdlib.T_Digest], stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10), ((txn3) => {
        const sim_r = { txns: [] };
        sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:62:10:dot', stdlib.UInt_max, 3), v35, v34, v45, v75]);
        const [v95] = txn3.data;
        const v96 = txn3.value;
        
        const v97 = stdlib.eq(v96, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
        stdlib.assert(v97, {
          at: './index.rsh:63:62:after expr stmt semicolon',
          fs: [],
          msg: 'pay amount correct',
          who: 'Alice'
           });
        const v120 = v75;
        const v123 = stdlib.add(v120, v96);
        sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Digest]), [stdlib.checkedBigNumberify('./index.rsh:64:17:after expr stmt semicolon', stdlib.UInt_max, 4), v123, v35, v34, v45, v95]);
        sim_r.isHalt = false;
        
        return sim_r;
         }));
      if (txn3.didTimeout) {
        const txn4 = await ctc.recv('Alice', 9, 0, [], false);
        const [] = txn4.data;
        const v99 = txn4.value;
        const v100 = stdlib.eq(v99, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
        stdlib.assert(v100, {
          at: 'reach standard library:77:16:after expr stmt semicolon',
          fs: ['at ./index.rsh:63:43:application call to "function" (defined at: reach standard library:76:8:function exp)'],
          msg: 'pay amount correct',
          who: 'Alice'
           });
        const v101 = v75;
        const v104 = stdlib.add(v101, v99);
        const v105 = v104;
        ;
        stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
          at: './index.rsh:40:33:application',
          fs: ['at ./index.rsh:40:39:after expr stmt semicolon call to "function" (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:80:8:application call to "function" (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:63:43:application call to "function" (defined at: reach standard library:76:8:function exp)'],
          msg: 'informTimeout',
          who: 'Alice'
           });
        return;
         }
      else {
        const [v95] = txn3.data;
        const v96 = txn3.value;
        const v97 = stdlib.eq(v96, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
        stdlib.assert(v97, {
          at: './index.rsh:63:62:after expr stmt semicolon',
          fs: [],
          msg: 'pay amount correct',
          who: 'Alice'
           });
        const v120 = v75;
        const v123 = stdlib.add(v120, v96);
        const txn4 = await ctc.recv('Alice', 5, 1, [stdlib.T_UInt], stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10));
        if (txn4.didTimeout) {
          const txn5 = await ctc.sendrecv('Alice', 8, 0, [stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Digest], [v123, v35, v34, v45, v95], stdlib.checkedBigNumberify('reach standard library:77:16:after expr stmt semicolon', stdlib.UInt_max, 0), [], false, ((txn5) => {
            const sim_r = { txns: [] };
            sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Digest]), [stdlib.checkedBigNumberify('reach standard library:77:6:dot', stdlib.UInt_max, 4), v123, v35, v34, v45, v95]);
            const [] = txn5.data;
            const v131 = txn5.value;
            
            const v132 = stdlib.eq(v131, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v132, {
              at: 'reach standard library:77:16:after expr stmt semicolon',
              fs: ['at ./index.rsh:70:43:application call to "function" (defined at: reach standard library:76:8:function exp)'],
              msg: 'pay amount correct',
              who: 'Alice'
               });
            const v133 = v123;
            const v136 = stdlib.add(v133, v131);
            const v137 = v136;
            sim_r.txns.push({
              amt: v137,
              to: v35
               });
            sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([]), []);
            sim_r.isHalt = true;
            
            return sim_r;
             }));
          const [] = txn5.data;
          const v131 = txn5.value;
          const v132 = stdlib.eq(v131, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
          stdlib.assert(v132, {
            at: 'reach standard library:77:16:after expr stmt semicolon',
            fs: ['at ./index.rsh:70:43:application call to "function" (defined at: reach standard library:76:8:function exp)'],
            msg: 'pay amount correct',
            who: 'Alice'
             });
          const v133 = v123;
          const v136 = stdlib.add(v133, v131);
          const v137 = v136;
          ;
          stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
            at: './index.rsh:40:33:application',
            fs: ['at ./index.rsh:40:39:after expr stmt semicolon call to "function" (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:80:8:application call to "function" (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:70:43:application call to "function" (defined at: reach standard library:76:8:function exp)'],
            msg: 'informTimeout',
            who: 'Alice'
             });
          return;
           }
        else {
          const [v127] = txn4.data;
          const v128 = txn4.value;
          const v129 = stdlib.eq(v128, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
          stdlib.assert(v129, {
            at: './index.rsh:70:62:after expr stmt semicolon',
            fs: [],
            msg: 'pay amount correct',
            who: 'Alice'
             });
          const v152 = v123;
          const v155 = stdlib.add(v152, v128);
          const txn5 = await ctc.sendrecv('Alice', 6, 2, [stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt], [v155, v35, v34, v45, v95, v127, v93, v91], stdlib.checkedBigNumberify('./index.rsh:76:62:after expr stmt semicolon', stdlib.UInt_max, 0), [stdlib.T_UInt, stdlib.T_UInt], stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10), ((txn5) => {
            const sim_r = { txns: [] };
            sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:75:10:dot', stdlib.UInt_max, 5), v155, v35, v34, v45, v95, v127]);
            const [v158, v159] = txn5.data;
            const v160 = txn5.value;
            
            const v161 = stdlib.eq(v160, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v161, {
              at: './index.rsh:76:62:after expr stmt semicolon',
              fs: [],
              msg: 'pay amount correct',
              who: 'Alice'
               });
            const v184 = v155;
            const v187 = stdlib.add(v184, v160);
            const v189 = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt]), [v158, v159]);
            const v191 = stdlib.digestEq(v95, v189);
            stdlib.assert(v191, {
              at: 'reach standard library:74:17:application',
              fs: ['at ./index.rsh:77:24:application call to "function" (defined at: reach standard library:73:8:function exp)'],
              msg: null,
              who: 'Alice'
               });
            const v194 = stdlib.sub(stdlib.checkedBigNumberify('./index.rsh:7:18:decimal', stdlib.UInt_max, 4), v127);
            const v197 = stdlib.add(v159, v194);
            const v198 = stdlib.mod(v197, stdlib.checkedBigNumberify('./index.rsh:7:32:decimal', stdlib.UInt_max, 3));
            const v199 = v187;
            const cv75 = v199;
            const cv76 = v198;
            
            (() => {
              const v75 = cv75;
              const v76 = cv76;
              
              if ((() => {
                const v88 = stdlib.eq(v76, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
                
                return v88; })()) {
                sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:56:17:after expr stmt semicolon', stdlib.UInt_max, 3), v35, v34, v45, v75]);
                sim_r.isHalt = false;
                 }
              else {
                const v201 = stdlib.eq(v76, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
                const v205 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:83:16:decimal', stdlib.UInt_max, 2), v34);
                const v209 = v201 ? v35 : v45;
                sim_r.txns.push({
                  amt: v205,
                  to: v209
                   });
                sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([]), []);
                sim_r.isHalt = true;
                 } })();
            return sim_r;
             }));
          if (txn5.didTimeout) {
            const txn6 = await ctc.recv('Alice', 7, 0, [], false);
            const [] = txn6.data;
            const v163 = txn6.value;
            const v164 = stdlib.eq(v163, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v164, {
              at: 'reach standard library:77:16:after expr stmt semicolon',
              fs: ['at ./index.rsh:76:43:application call to "function" (defined at: reach standard library:76:8:function exp)'],
              msg: 'pay amount correct',
              who: 'Alice'
               });
            const v165 = v155;
            const v168 = stdlib.add(v165, v163);
            const v169 = v168;
            ;
            stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
              at: './index.rsh:40:33:application',
              fs: ['at ./index.rsh:40:39:after expr stmt semicolon call to "function" (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:80:8:application call to "function" (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:76:43:application call to "function" (defined at: reach standard library:76:8:function exp)'],
              msg: 'informTimeout',
              who: 'Alice'
               });
            return;
             }
          else {
            const [v158, v159] = txn5.data;
            const v160 = txn5.value;
            const v161 = stdlib.eq(v160, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v161, {
              at: './index.rsh:76:62:after expr stmt semicolon',
              fs: [],
              msg: 'pay amount correct',
              who: 'Alice'
               });
            const v184 = v155;
            const v187 = stdlib.add(v184, v160);
            const v189 = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt]), [v158, v159]);
            const v191 = stdlib.digestEq(v95, v189);
            stdlib.assert(v191, {
              at: 'reach standard library:74:17:application',
              fs: ['at ./index.rsh:77:24:application call to "function" (defined at: reach standard library:73:8:function exp)'],
              msg: null,
              who: 'Alice'
               });
            const v194 = stdlib.sub(stdlib.checkedBigNumberify('./index.rsh:7:18:decimal', stdlib.UInt_max, 4), v127);
            const v197 = stdlib.add(v159, v194);
            const v198 = stdlib.mod(v197, stdlib.checkedBigNumberify('./index.rsh:7:32:decimal', stdlib.UInt_max, 3));
            const v199 = v187;
            const cv75 = v199;
            const cv76 = v198;
            
            v75 = cv75;
            v76 = cv76;
            
            continue; }
           }
         }
       }
    const v201 = stdlib.eq(v76, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
    const v205 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:83:16:decimal', stdlib.UInt_max, 2), v34);
    const v209 = v201 ? v35 : v45;
    ;
    stdlib.protect(stdlib.T_Null, await interact.seeOutcome(v76), {
      at: './index.rsh:87:28:application',
      fs: ['at ./index.rsh:87:41:after expr stmt semicolon call to "function" (defined at: ./index.rsh:86:23:function exp)'],
      msg: 'seeOutcome',
      who: 'Alice'
       });
    return; }
  
   }
export async function Bob(stdlib, ctc, interact) {
  const txn1 = await ctc.recv('Bob', 1, 1, [stdlib.T_UInt], false);
  const [v34] = txn1.data;
  const v36 = txn1.value;
  const v35 = txn1.from;
  const v37 = stdlib.eq(v36, v34);
  stdlib.assert(v37, {
    at: './index.rsh:45:20:after expr stmt semicolon',
    fs: [],
    msg: 'pay amount correct',
    who: 'Bob'
     });
  const v38 = stdlib.checkedBigNumberify('./index.rsh:compileDApp', stdlib.UInt_max, 0);
  const v41 = stdlib.add(v38, v36);
  stdlib.protect(stdlib.T_Null, await interact.acceptWager(v34), {
    at: './index.rsh:49:29:application',
    fs: ['at ./index.rsh:49:40:after expr stmt semicolon call to "function" (defined at: ./index.rsh:48:17:function exp)'],
    msg: 'acceptWager',
    who: 'Bob'
     });
  const txn2 = await ctc.sendrecv('Bob', 2, 0, [stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt], [v41, v35, v34], v34, [], stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10), ((txn2) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:50:8:dot', stdlib.UInt_max, 1), v41, v35, v34]);
    const [] = txn2.data;
    const v46 = txn2.value;
    const v45 = txn2.from;
    
    const v47 = stdlib.eq(v46, v34);
    stdlib.assert(v47, {
      at: './index.rsh:51:60:after expr stmt semicolon',
      fs: [],
      msg: 'pay amount correct',
      who: 'Bob'
       });
    const v70 = v41;
    const v73 = stdlib.add(v70, v46);
    const v74 = v73;
    const v75 = v74;
    const v76 = stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1);
    
    if ((() => {
      const v88 = stdlib.eq(v76, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
      
      return v88; })()) {
      sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:56:17:after expr stmt semicolon', stdlib.UInt_max, 3), v35, v34, v45, v75]);
      sim_r.isHalt = false;
       }
    else {
      const v201 = stdlib.eq(v76, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
      const v205 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:83:16:decimal', stdlib.UInt_max, 2), v34);
      const v209 = v201 ? v35 : v45;
      sim_r.txns.push({
        amt: v205,
        to: v209
         });
      sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([]), []);
      sim_r.isHalt = true;
       }
    return sim_r;
     }));
  if (txn2.didTimeout) {
    const txn3 = await ctc.recv('Bob', 10, 0, [], false);
    const [] = txn3.data;
    const v49 = txn3.value;
    const v50 = stdlib.eq(v49, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
    stdlib.assert(v50, {
      at: 'reach standard library:77:16:after expr stmt semicolon',
      fs: ['at ./index.rsh:51:41:application call to "function" (defined at: reach standard library:76:8:function exp)'],
      msg: 'pay amount correct',
      who: 'Bob'
       });
    const v51 = v41;
    const v54 = stdlib.add(v51, v49);
    const v55 = v54;
    ;
    stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
      at: './index.rsh:40:33:application',
      fs: ['at ./index.rsh:40:39:after expr stmt semicolon call to "function" (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:80:8:application call to "function" (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:51:41:application call to "function" (defined at: reach standard library:76:8:function exp)'],
      msg: 'informTimeout',
      who: 'Bob'
       });
    return;
     }
  else {
    const [] = txn2.data;
    const v46 = txn2.value;
    const v45 = txn2.from;
    const v47 = stdlib.eq(v46, v34);
    stdlib.assert(v47, {
      at: './index.rsh:51:60:after expr stmt semicolon',
      fs: [],
      msg: 'pay amount correct',
      who: 'Bob'
       });
    const v70 = v41;
    const v73 = stdlib.add(v70, v46);
    const v74 = v73;
    let v75 = v74;
    let v76 = stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1);
    
    while ((() => {
      const v88 = stdlib.eq(v76, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
      
      return v88; })()) {
      const txn3 = await ctc.recv('Bob', 4, 1, [stdlib.T_Digest], stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10));
      if (txn3.didTimeout) {
        const txn4 = await ctc.sendrecv('Bob', 9, 0, [stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt], [v35, v34, v45, v75], stdlib.checkedBigNumberify('reach standard library:77:16:after expr stmt semicolon', stdlib.UInt_max, 0), [], false, ((txn4) => {
          const sim_r = { txns: [] };
          sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt]), [stdlib.checkedBigNumberify('reach standard library:77:6:dot', stdlib.UInt_max, 3), v35, v34, v45, v75]);
          const [] = txn4.data;
          const v99 = txn4.value;
          
          const v100 = stdlib.eq(v99, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
          stdlib.assert(v100, {
            at: 'reach standard library:77:16:after expr stmt semicolon',
            fs: ['at ./index.rsh:63:43:application call to "function" (defined at: reach standard library:76:8:function exp)'],
            msg: 'pay amount correct',
            who: 'Bob'
             });
          const v101 = v75;
          const v104 = stdlib.add(v101, v99);
          const v105 = v104;
          sim_r.txns.push({
            amt: v105,
            to: v45
             });
          sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([]), []);
          sim_r.isHalt = true;
          
          return sim_r;
           }));
        const [] = txn4.data;
        const v99 = txn4.value;
        const v100 = stdlib.eq(v99, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
        stdlib.assert(v100, {
          at: 'reach standard library:77:16:after expr stmt semicolon',
          fs: ['at ./index.rsh:63:43:application call to "function" (defined at: reach standard library:76:8:function exp)'],
          msg: 'pay amount correct',
          who: 'Bob'
           });
        const v101 = v75;
        const v104 = stdlib.add(v101, v99);
        const v105 = v104;
        ;
        stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
          at: './index.rsh:40:33:application',
          fs: ['at ./index.rsh:40:39:after expr stmt semicolon call to "function" (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:80:8:application call to "function" (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:63:43:application call to "function" (defined at: reach standard library:76:8:function exp)'],
          msg: 'informTimeout',
          who: 'Bob'
           });
        return;
         }
      else {
        const [v95] = txn3.data;
        const v96 = txn3.value;
        const v97 = stdlib.eq(v96, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
        stdlib.assert(v97, {
          at: './index.rsh:63:62:after expr stmt semicolon',
          fs: [],
          msg: 'pay amount correct',
          who: 'Bob'
           });
        const v120 = v75;
        const v123 = stdlib.add(v120, v96);
        const v126 = stdlib.protect(stdlib.T_UInt, await interact.getHand(), {
          at: './index.rsh:68:52:application',
          fs: ['at ./index.rsh:68:59:after expr stmt semicolon call to "function" (defined at: ./index.rsh:67:19:function exp)'],
          msg: 'getHand',
          who: 'Bob'
           });
        const txn4 = await ctc.sendrecv('Bob', 5, 1, [stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt], [v123, v35, v34, v45, v95, v126], stdlib.checkedBigNumberify('./index.rsh:70:62:after expr stmt semicolon', stdlib.UInt_max, 0), [stdlib.T_UInt], stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10), ((txn4) => {
          const sim_r = { txns: [] };
          sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Digest]), [stdlib.checkedBigNumberify('./index.rsh:69:10:dot', stdlib.UInt_max, 4), v123, v35, v34, v45, v95]);
          const [v127] = txn4.data;
          const v128 = txn4.value;
          
          const v129 = stdlib.eq(v128, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
          stdlib.assert(v129, {
            at: './index.rsh:70:62:after expr stmt semicolon',
            fs: [],
            msg: 'pay amount correct',
            who: 'Bob'
             });
          const v152 = v123;
          const v155 = stdlib.add(v152, v128);
          sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:71:17:after expr stmt semicolon', stdlib.UInt_max, 5), v155, v35, v34, v45, v95, v127]);
          sim_r.isHalt = false;
          
          return sim_r;
           }));
        if (txn4.didTimeout) {
          const txn5 = await ctc.recv('Bob', 8, 0, [], false);
          const [] = txn5.data;
          const v131 = txn5.value;
          const v132 = stdlib.eq(v131, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
          stdlib.assert(v132, {
            at: 'reach standard library:77:16:after expr stmt semicolon',
            fs: ['at ./index.rsh:70:43:application call to "function" (defined at: reach standard library:76:8:function exp)'],
            msg: 'pay amount correct',
            who: 'Bob'
             });
          const v133 = v123;
          const v136 = stdlib.add(v133, v131);
          const v137 = v136;
          ;
          stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
            at: './index.rsh:40:33:application',
            fs: ['at ./index.rsh:40:39:after expr stmt semicolon call to "function" (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:80:8:application call to "function" (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:70:43:application call to "function" (defined at: reach standard library:76:8:function exp)'],
            msg: 'informTimeout',
            who: 'Bob'
             });
          return;
           }
        else {
          const [v127] = txn4.data;
          const v128 = txn4.value;
          const v129 = stdlib.eq(v128, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
          stdlib.assert(v129, {
            at: './index.rsh:70:62:after expr stmt semicolon',
            fs: [],
            msg: 'pay amount correct',
            who: 'Bob'
             });
          const v152 = v123;
          const v155 = stdlib.add(v152, v128);
          const txn5 = await ctc.recv('Bob', 6, 2, [stdlib.T_UInt, stdlib.T_UInt], stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10));
          if (txn5.didTimeout) {
            const txn6 = await ctc.sendrecv('Bob', 7, 0, [stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt], [v155, v35, v34, v45, v95, v127], stdlib.checkedBigNumberify('reach standard library:77:16:after expr stmt semicolon', stdlib.UInt_max, 0), [], false, ((txn6) => {
              const sim_r = { txns: [] };
              sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt]), [stdlib.checkedBigNumberify('reach standard library:77:6:dot', stdlib.UInt_max, 5), v155, v35, v34, v45, v95, v127]);
              const [] = txn6.data;
              const v163 = txn6.value;
              
              const v164 = stdlib.eq(v163, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
              stdlib.assert(v164, {
                at: 'reach standard library:77:16:after expr stmt semicolon',
                fs: ['at ./index.rsh:76:43:application call to "function" (defined at: reach standard library:76:8:function exp)'],
                msg: 'pay amount correct',
                who: 'Bob'
                 });
              const v165 = v155;
              const v168 = stdlib.add(v165, v163);
              const v169 = v168;
              sim_r.txns.push({
                amt: v169,
                to: v45
                 });
              sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([]), []);
              sim_r.isHalt = true;
              
              return sim_r;
               }));
            const [] = txn6.data;
            const v163 = txn6.value;
            const v164 = stdlib.eq(v163, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v164, {
              at: 'reach standard library:77:16:after expr stmt semicolon',
              fs: ['at ./index.rsh:76:43:application call to "function" (defined at: reach standard library:76:8:function exp)'],
              msg: 'pay amount correct',
              who: 'Bob'
               });
            const v165 = v155;
            const v168 = stdlib.add(v165, v163);
            const v169 = v168;
            ;
            stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
              at: './index.rsh:40:33:application',
              fs: ['at ./index.rsh:40:39:after expr stmt semicolon call to "function" (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:80:8:application call to "function" (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:76:43:application call to "function" (defined at: reach standard library:76:8:function exp)'],
              msg: 'informTimeout',
              who: 'Bob'
               });
            return;
             }
          else {
            const [v158, v159] = txn5.data;
            const v160 = txn5.value;
            const v161 = stdlib.eq(v160, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v161, {
              at: './index.rsh:76:62:after expr stmt semicolon',
              fs: [],
              msg: 'pay amount correct',
              who: 'Bob'
               });
            const v184 = v155;
            const v187 = stdlib.add(v184, v160);
            const v189 = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt]), [v158, v159]);
            const v191 = stdlib.digestEq(v95, v189);
            stdlib.assert(v191, {
              at: 'reach standard library:74:17:application',
              fs: ['at ./index.rsh:77:24:application call to "function" (defined at: reach standard library:73:8:function exp)'],
              msg: null,
              who: 'Bob'
               });
            const v194 = stdlib.sub(stdlib.checkedBigNumberify('./index.rsh:7:18:decimal', stdlib.UInt_max, 4), v127);
            const v197 = stdlib.add(v159, v194);
            const v198 = stdlib.mod(v197, stdlib.checkedBigNumberify('./index.rsh:7:32:decimal', stdlib.UInt_max, 3));
            const v199 = v187;
            const cv75 = v199;
            const cv76 = v198;
            
            v75 = cv75;
            v76 = cv76;
            
            continue; }
           }
         }
       }
    const v201 = stdlib.eq(v76, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
    const v205 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:83:16:decimal', stdlib.UInt_max, 2), v34);
    const v209 = v201 ? v35 : v45;
    ;
    stdlib.protect(stdlib.T_Null, await interact.seeOutcome(v76), {
      at: './index.rsh:87:28:application',
      fs: ['at ./index.rsh:87:41:after expr stmt semicolon call to "function" (defined at: ./index.rsh:86:23:function exp)'],
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
  stepargs: [0, 89, 129, 0, 193, 201, 217, 201, 193, 161, 129],
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
int 0
itob
keccak256
arg 0
==
assert
// Run body
// Just "pay amount correct"
// "./index.rsh:45:20:after expr stmt semicolon"
// "[]"
gtxn 3 Amount
arg 3
btoi
-
arg 5
btoi
==
assert
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
// "./index.rsh:51:60:after expr stmt semicolon"
// "[]"
gtxn 3 Amount
arg 3
btoi
-
arg 7
btoi
==
assert
int 1
dup
store 255
int 1
==
bz l0
int 3
itob
arg 6
concat
arg 7
concat
gtxn 3 Sender
concat
arg 5
btoi
gtxn 3 Amount
arg 3
btoi
-
+
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
load 255
int 2
==
arg 6
gtxn 3 Sender
ite
==
assert
gtxn 4 Amount
int 2
arg 7
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
`, null, `#pragma version 2
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
gtxn 3 Sender
arg 5
==
assert
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
assert
// Run body
// Just "pay amount correct"
// "./index.rsh:63:62:after expr stmt semicolon"
// "[]"
gtxn 3 Amount
arg 3
btoi
-
int 0
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
gtxn 3 Sender
arg 8
==
assert
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
assert
// Run body
// Just "pay amount correct"
// "./index.rsh:70:62:after expr stmt semicolon"
// "[]"
gtxn 3 Amount
arg 3
btoi
-
int 0
==
assert
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
int 13
==
assert
gtxn 3 Sender
arg 6
==
assert
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
assert
// Run body
// Just "pay amount correct"
// "./index.rsh:76:62:after expr stmt semicolon"
// "[]"
gtxn 3 Amount
arg 3
btoi
-
int 0
==
assert
// Nothing
// "reach standard library:74:17:application"
// "[at ./index.rsh:77:24:application call to \"function\" (defined at: reach standard library:73:8:function exp)]"
arg 9
arg 11
arg 12
concat
keccak256
==
assert
arg 12
btoi
int 4
arg 10
btoi
-
+
int 3
%
dup
store 255
int 1
==
bz l0
int 3
itob
arg 6
concat
arg 7
concat
arg 8
concat
arg 5
btoi
gtxn 3 Amount
arg 3
btoi
-
+
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
load 255
int 2
==
arg 6
arg 8
ite
==
assert
gtxn 4 Amount
int 2
arg 7
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
int 11
==
assert
gtxn 3 Sender
arg 8
==
assert
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
assert
// Run body
// Just "pay amount correct"
// "reach standard library:77:16:after expr stmt semicolon"
// "[at ./index.rsh:76:43:application call to \"function\" (defined at: reach standard library:76:8:function exp)]"
gtxn 3 Amount
arg 3
btoi
-
int 0
==
assert
gtxn 4 TypeEnum
int pay
==
assert
gtxn 4 Receiver
arg 8
==
assert
gtxn 4 Amount
arg 5
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
int 10
==
assert
gtxn 3 Sender
arg 6
==
assert
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
assert
// Run body
// Just "pay amount correct"
// "reach standard library:77:16:after expr stmt semicolon"
// "[at ./index.rsh:70:43:application call to \"function\" (defined at: reach standard library:76:8:function exp)]"
gtxn 3 Amount
arg 3
btoi
-
int 0
==
assert
gtxn 4 TypeEnum
int pay
==
assert
gtxn 4 Receiver
arg 6
==
assert
gtxn 4 Amount
arg 5
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
int 9
==
assert
gtxn 3 Sender
arg 7
==
assert
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
assert
// Run body
// Just "pay amount correct"
// "reach standard library:77:16:after expr stmt semicolon"
// "[at ./index.rsh:63:43:application call to \"function\" (defined at: reach standard library:76:8:function exp)]"
gtxn 3 Amount
arg 3
btoi
-
int 0
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
int 8
==
assert
gtxn 3 Sender
arg 6
==
assert
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
// "reach standard library:77:16:after expr stmt semicolon"
// "[at ./index.rsh:51:41:application call to \"function\" (defined at: reach standard library:76:8:function exp)]"
gtxn 3 Amount
arg 3
btoi
-
int 0
==
assert
gtxn 4 TypeEnum
int pay
==
assert
gtxn 4 Receiver
arg 6
==
assert
gtxn 4 Amount
arg 5
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
        "indexed": false,
        "internalType": "uint256",
        "name": "v34",
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
        "name": "v127",
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
        "name": "v158",
        "type": "uint256"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "v159",
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
            "name": "v34",
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
            "name": "v41",
            "type": "uint256"
          },
          {
            "internalType": "address payable",
            "name": "v35",
            "type": "address"
          },
          {
            "internalType": "uint256",
            "name": "v34",
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
            "name": "v41",
            "type": "uint256"
          },
          {
            "internalType": "address payable",
            "name": "v35",
            "type": "address"
          },
          {
            "internalType": "uint256",
            "name": "v34",
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
            "name": "v35",
            "type": "address"
          },
          {
            "internalType": "uint256",
            "name": "v34",
            "type": "uint256"
          },
          {
            "internalType": "address payable",
            "name": "v45",
            "type": "address"
          },
          {
            "internalType": "uint256",
            "name": "v75",
            "type": "uint256"
          },
          {
            "internalType": "uint256",
            "name": "v95",
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
            "name": "v123",
            "type": "uint256"
          },
          {
            "internalType": "address payable",
            "name": "v35",
            "type": "address"
          },
          {
            "internalType": "uint256",
            "name": "v34",
            "type": "uint256"
          },
          {
            "internalType": "address payable",
            "name": "v45",
            "type": "address"
          },
          {
            "internalType": "uint256",
            "name": "v95",
            "type": "uint256"
          },
          {
            "internalType": "uint256",
            "name": "v127",
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
            "name": "v155",
            "type": "uint256"
          },
          {
            "internalType": "address payable",
            "name": "v35",
            "type": "address"
          },
          {
            "internalType": "uint256",
            "name": "v34",
            "type": "uint256"
          },
          {
            "internalType": "address payable",
            "name": "v45",
            "type": "address"
          },
          {
            "internalType": "uint256",
            "name": "v95",
            "type": "uint256"
          },
          {
            "internalType": "uint256",
            "name": "v127",
            "type": "uint256"
          },
          {
            "internalType": "uint256",
            "name": "v158",
            "type": "uint256"
          },
          {
            "internalType": "uint256",
            "name": "v159",
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
            "name": "v155",
            "type": "uint256"
          },
          {
            "internalType": "address payable",
            "name": "v35",
            "type": "address"
          },
          {
            "internalType": "uint256",
            "name": "v34",
            "type": "uint256"
          },
          {
            "internalType": "address payable",
            "name": "v45",
            "type": "address"
          },
          {
            "internalType": "uint256",
            "name": "v95",
            "type": "uint256"
          },
          {
            "internalType": "uint256",
            "name": "v127",
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
            "name": "v123",
            "type": "uint256"
          },
          {
            "internalType": "address payable",
            "name": "v35",
            "type": "address"
          },
          {
            "internalType": "uint256",
            "name": "v34",
            "type": "uint256"
          },
          {
            "internalType": "address payable",
            "name": "v45",
            "type": "address"
          },
          {
            "internalType": "uint256",
            "name": "v95",
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
            "name": "v35",
            "type": "address"
          },
          {
            "internalType": "uint256",
            "name": "v34",
            "type": "uint256"
          },
          {
            "internalType": "address payable",
            "name": "v45",
            "type": "address"
          },
          {
            "internalType": "uint256",
            "name": "v75",
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
  Bytecode: `0x608060408190527f49ff028a829527a47ec6839c7147b484eccf5a2a94853eddac09cef44d9d4e9e90600090a1600043604051602001610040929190610060565b60408051601f19818403018152919052805160209091012060005561006e565b918252602082015260400190565b6110b28061007d6000396000f3fe6080604052600436106100865760003560e01c80636c75e587116100595780636c75e587146100d957806371570cb5146100ec57806378088fec146100ff578063ebcdfe5814610112578063f156095d1461012557610086565b806303fcf1691461008b5780631a9f6f72146100a05780632745f356146100b35780635602050f146100c6575b600080fd5b61009e610099366004610eac565b610138565b005b61009e6100ae366004610ed8565b610211565b61009e6100c1366004610f05565b610363565b61009e6100d4366004610ebd565b6104a9565b61009e6100e7366004610ef3565b6105fc565b61009e6100fa366004610ebd565b6107e1565b61009e61010d366004610e91565b61092d565b61009e610120366004610ed8565b610a3a565b61009e610133366004610e91565b610b99565b60405161014d90600090833590602001610f93565b6040516020818303038152906040528051906020012060001c6000541461017357600080fd5b61017b610e17565b8160200135341461018b57600080fd5b610196600034610cce565b81526040517f3680e78b6fdf571695c81f108d81181ea63f50c100e6375e765b14bd7ac0adbb906101cc90602085013590610f8a565b60405180910390a180516040516101f29160019143919033906020808901359101610fd5565b60408051601f1981840301815291905280516020909101206000555050565b60058135602083013561022a6060850160408601610e63565b606085013561023f60a0870160808801610e63565b8660a001358760c0013560405160200161026098979695949392919061103a565b6040516020818303038152906040528051906020012060001c6000541461028657600080fd5b61029660a0820160808301610e63565b6001600160a01b0316336001600160a01b0316146102b357600080fd5b6102bf8135600a610cce565b43101580156102cc575060015b6102d557600080fd5b34156102e057600080fd5b6102f060a0820160808301610e63565b6001600160a01b03166108fc61030a836020013534610cce565b6040518115909202916000818181858888f19350505050158015610332573d6000803e3d6000fd5b506040517f865db884b03ae248a5b1e887fed68b9b0d97adb2d5660a93e9b1d6da53f070b990600090a16000805533ff5b600381356103776040840160208501610e63565b604084013561038c6080860160608701610e63565b85608001356040516020016103a696959493929190610fa1565b6040516020818303038152906040528051906020012060001c600054146103cc57600080fd5b6103dc6080820160608301610e63565b6001600160a01b0316336001600160a01b0316146103f957600080fd5b6104058135600a610cce565b4310158015610412575060015b61041b57600080fd5b341561042657600080fd5b6104366080820160608301610e63565b6001600160a01b03166108fc610450836080013534610cce565b6040518115909202916000818181858888f19350505050158015610478573d6000803e3d6000fd5b506040517f5ba509c4dbb2c82e8f38fec2166bedd2c18c53fb19ce6aa5ab42111e51e0744990600090a16000805533ff5b600381356104bd6040840160208501610e63565b60408401356104d26080860160608701610e63565b85608001356040516020016104ec96959493929190610fa1565b6040516020818303038152906040528051906020012060001c6000541461051257600080fd5b61051a610e17565b61052a6040830160208401610e63565b6001600160a01b0316336001600160a01b03161461054757600080fd5b6105538235600a610cce565b431061055e57600080fd5b341561056957600080fd5b610577826080013534610cce565b81526040517f6ce5b12953112c528c3a24a99350a573e6cb61c4e39d70f2392cc6bd7266f969906105ad9060a085013590610f8a565b60405180910390a1805160049043906105cc6040860160208701610e63565b60408601356105e16080880160608901610e63565b8760a001356040516020016101f29796959493929190611001565b6005813560208301356106156060850160408601610e63565b606085013561062a60a0870160808801610e63565b8660a001358760c0013560405160200161064b98979695949392919061103a565b6040516020818303038152906040528051906020012060001c6000541461067157600080fd5b6106816060820160408301610e63565b6001600160a01b0316336001600160a01b03161461069e57600080fd5b6106aa8135600a610cce565b43106106b557600080fd5b34156106c057600080fd5b8060e001358161010001356040516020016106dc929190610f93565b6040516020818303038152906040528051906020012060001c8160a001351461070457600080fd5b7f1fa1ad895cc7ba9133068b14fd5b3d9ed6f96d3a535ff2be342493855f237b6b8160e0013582610100013560405161073e929190610f93565b60405180910390a16107de6040518060a001604052808360400160208101906107679190610e63565b6001600160a01b031681526060840135602082015260400161078f60a0850160808601610e63565b6001600160a01b031681526020016107ab846020013534610cce565b815260200160036107cf8561010001356107ca60048860c00135610d00565b610cce565b816107d657fe5b069052610d23565b50565b6004813560208301356107fa6060850160408601610e63565b606085013561080f60a0870160808801610e63565b8660a0013560405160200161082a9796959493929190611001565b6040516020818303038152906040528051906020012060001c6000541461085057600080fd5b6108606060820160408301610e63565b6001600160a01b0316336001600160a01b03161461087d57600080fd5b6108898135600a610cce565b4310158015610896575060015b61089f57600080fd5b34156108aa57600080fd5b6108ba6060820160408301610e63565b6001600160a01b03166108fc6108d4836020013534610cce565b6040518115909202916000818181858888f193505050501580156108fc573d6000803e3d6000fd5b506040517fffa3d43ab9b6e5b34273fd049718a85553427384eed63754abc672936df0584e90600090a16000805533ff5b6001813560208301356109466060850160408601610e63565b846060013560405160200161095f959493929190610fd5565b6040516020818303038152906040528051906020012060001c6000541461098557600080fd5b6109918135600a610cce565b431061099c57600080fd5b806060013534146109ac57600080fd5b6040517f9b31f9e88fd11f71bfbf93b0237bc9a0900b8479a307f60435e40543e383403590600090a16107de6040518060a001604052808360400160208101906109f69190610e63565b6001600160a01b0316815260200183606001358152602001336001600160a01b03168152602001610a2b846020013534610cce565b81526020016001815250610d23565b600481356020830135610a536060850160408601610e63565b6060850135610a6860a0870160808801610e63565b8660a00135604051602001610a839796959493929190611001565b6040516020818303038152906040528051906020012060001c60005414610aa957600080fd5b610ab1610e17565b610ac160a0830160808401610e63565b6001600160a01b0316336001600160a01b031614610ade57600080fd5b610aea8235600a610cce565b4310610af557600080fd5b3415610b0057600080fd5b610b0e826020013534610cce565b81526040517f26bdc6b0a0806ec5cb3992c1b74dd2db228f99da5f09181980c9114c97ebf40790610b449060c085013590610f8a565b60405180910390a180516005904390610b636060860160408701610e63565b6060860135610b7860a0880160808901610e63565b8760a001358860c001356040516020016101f298979695949392919061103a565b600181356020830135610bb26060850160408601610e63565b8460600135604051602001610bcb959493929190610fd5565b6040516020818303038152906040528051906020012060001c60005414610bf157600080fd5b610c016060820160408301610e63565b6001600160a01b0316336001600160a01b031614610c1e57600080fd5b610c2a8135600a610cce565b4310158015610c37575060015b610c4057600080fd5b3415610c4b57600080fd5b610c5b6060820160408301610e63565b6001600160a01b03166108fc610c75836020013534610cce565b6040518115909202916000818181858888f19350505050158015610c9d573d6000803e3d6000fd5b506040517f5ef1d939728ae307281ba62215efdccdf0b99fa9cc412f247b7ab97b4729b74f90600090a16000805533ff5b80820182811015610cfa5760405162461bcd60e51b8152600401610cf190610f64565b60405180910390fd5b92915050565b80820382811115610cfa5760405162461bcd60e51b8152600401610cf190610f16565b600181608001511415610d7c576003438260000151836020015184604001518560600151604051602001610d5c96959493929190610fa1565b60408051601f1981840301815291905280516020909101206000556107de565b6002816080015114610d92578060400151610d95565b80515b6001600160a01b03166108fc610db060028460200151610de0565b6040518115909202916000818181858888f19350505050158015610dd8573d6000803e3d6000fd5b506000805533ff5b6000811580610dfb57505080820282828281610df857fe5b04145b610cfa5760405162461bcd60e51b8152600401610cf190610f3e565b6040518060200160405280600081525090565b600060808284031215610e3b578081fd5b50919050565b600060c08284031215610e3b578081fd5b600060e08284031215610e3b578081fd5b600060208284031215610e74578081fd5b81356001600160a01b0381168114610e8a578182fd5b9392505050565b600060808284031215610ea2578081fd5b610e8a8383610e2a565b600060408284031215610e3b578081fd5b600060c08284031215610ece578081fd5b610e8a8383610e41565b600060e08284031215610ee9578081fd5b610e8a8383610e52565b60006101208284031215610e3b578081fd5b600060a08284031215610e3b578081fd5b6020808252600e908201526d1cdd58881ddc985c185c9bdd5b9960921b604082015260600190565b6020808252600c908201526b6d756c206f766572666c6f7760a01b604082015260600190565b6020808252600c908201526b616464206f766572666c6f7760a01b604082015260600190565b90815260200190565b918252602082015260400190565b95865260208601949094526001600160a01b039283166040860152606085019190915216608083015260a082015260c00190565b948552602085019390935260408401919091526001600160a01b03166060830152608082015260a00190565b968752602087019590955260408601939093526001600160a01b03918216606086015260808501521660a083015260c082015260e00190565b978852602088019690965260408701949094526001600160a01b03928316606087015260808601919091521660a084015260c083015260e0820152610100019056fea2646970667358221220f8d8c15c993a4d48d99c4ba0c2248ac7b4e7c790bf54b81d4d1c5eac7e94426b64736f6c63430007040033`,
  deployMode: `DM_constructor`
   };

export const _Connectors = {
  ALGO: _ALGO,
  ETH: _ETH
   };

