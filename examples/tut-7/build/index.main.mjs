// Automatically generated with Reach 0.1.2
/* eslint-disable no-unused-vars, no-empty-pattern, no-useless-escape, no-loop-func */
export const _version = '0.1.2';


export async function Alice(stdlib, ctc, interact) {
  const txn1 = await ctc.sendrecv('Alice', 1, 1, [stdlib.T_UInt], [stdlib.protect(stdlib.T_UInt, interact.wager, null)], stdlib.protect(stdlib.T_UInt, interact.wager, null), [stdlib.T_UInt], false, ((txn1) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:44:8:dot', stdlib.UInt_max, 0)]);
    const [v47] = txn1.data;
    const v49 = txn1.value;
    const v48 = txn1.from;
    
    const v50 = stdlib.eq(v49, v47);
    stdlib.assert(v50, {
      at: './index.rsh:45:20:after expr stmt semicolon',
      fs: [],
      msg: 'pay amount correct',
      who: 'Alice'
       });
    const v51 = stdlib.checkedBigNumberify('./index.rsh:compileDApp', stdlib.UInt_max, 0);
    const v54 = stdlib.add(v51, v49);
    sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:46:15:after expr stmt semicolon', stdlib.UInt_max, 1), v54, v48, v47]);
    sim_r.isHalt = false;
    
    return sim_r;
     }));
  const [v47] = txn1.data;
  const v49 = txn1.value;
  const v48 = txn1.from;
  const v50 = stdlib.eq(v49, v47);
  stdlib.assert(v50, {
    at: './index.rsh:45:20:after expr stmt semicolon',
    fs: [],
    msg: 'pay amount correct',
    who: 'Alice'
     });
  const v51 = stdlib.checkedBigNumberify('./index.rsh:compileDApp', stdlib.UInt_max, 0);
  const v54 = stdlib.add(v51, v49);
  const txn2 = await ctc.recv('Alice', 2, 0, [], stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10));
  if (txn2.didTimeout) {
    const txn3 = await ctc.sendrecv('Alice', 10, 0, [stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt], [v54, v48, v47], stdlib.checkedBigNumberify('reach standard library:77:16:after expr stmt semicolon', stdlib.UInt_max, 0), [], false, ((txn3) => {
      const sim_r = { txns: [] };
      sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt]), [stdlib.checkedBigNumberify('reach standard library:77:6:dot', stdlib.UInt_max, 1), v54, v48, v47]);
      const [] = txn3.data;
      const v62 = txn3.value;
      
      const v63 = stdlib.eq(v62, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
      stdlib.assert(v63, {
        at: 'reach standard library:77:16:after expr stmt semicolon',
        fs: ['at ./index.rsh:51:41:application call to "function" (defined at: reach standard library:76:8:function exp)'],
        msg: 'pay amount correct',
        who: 'Alice'
         });
      const v64 = v54;
      const v67 = stdlib.add(v64, v62);
      const v68 = v67;
      sim_r.txns.push({
        amt: v68,
        to: v48
         });
      sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([]), []);
      sim_r.isHalt = true;
      
      return sim_r;
       }));
    const [] = txn3.data;
    const v62 = txn3.value;
    const v63 = stdlib.eq(v62, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
    stdlib.assert(v63, {
      at: 'reach standard library:77:16:after expr stmt semicolon',
      fs: ['at ./index.rsh:51:41:application call to "function" (defined at: reach standard library:76:8:function exp)'],
      msg: 'pay amount correct',
      who: 'Alice'
       });
    const v64 = v54;
    const v67 = stdlib.add(v64, v62);
    const v68 = v67;
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
    const v59 = txn2.value;
    const v58 = txn2.from;
    const v60 = stdlib.eq(v59, v47);
    stdlib.assert(v60, {
      at: './index.rsh:51:60:after expr stmt semicolon',
      fs: [],
      msg: 'pay amount correct',
      who: 'Alice'
       });
    const v83 = v54;
    const v86 = stdlib.add(v83, v59);
    const v87 = v86;
    let v88 = v87;
    let v89 = stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1);
    
    while ((() => {
      const v103 = stdlib.eq(v89, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
      
      return v103; })()) {
      const v107 = stdlib.protect(stdlib.T_UInt, await interact.getHand(), {
        at: './index.rsh:59:42:application',
        fs: ['at ./index.rsh:61:51:after expr stmt semicolon call to "function" (defined at: ./index.rsh:58:19:function exp)'],
        msg: 'getHand',
        who: 'Alice'
         });
      const v109 = stdlib.protect(stdlib.T_UInt, await interact.random(), {
        at: 'reach standard library:69:31:application',
        fs: ['at ./index.rsh:60:52:application call to "function" (defined at: reach standard library:68:8:function exp)', 'at ./index.rsh:61:51:after expr stmt semicolon call to "function" (defined at: ./index.rsh:58:19:function exp)'],
        msg: 'random',
        who: 'Alice'
         });
      const v110 = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt]), [v109, v107]);
      const txn3 = await ctc.sendrecv('Alice', 4, 1, [stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Digest], [v48, v47, v58, v88, v110], stdlib.checkedBigNumberify('./index.rsh:63:62:after expr stmt semicolon', stdlib.UInt_max, 0), [stdlib.T_Digest], stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10), ((txn3) => {
        const sim_r = { txns: [] };
        sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:62:10:dot', stdlib.UInt_max, 3), v48, v47, v58, v88]);
        const [v112] = txn3.data;
        const v113 = txn3.value;
        
        const v114 = stdlib.eq(v113, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
        stdlib.assert(v114, {
          at: './index.rsh:63:62:after expr stmt semicolon',
          fs: [],
          msg: 'pay amount correct',
          who: 'Alice'
           });
        const v137 = v88;
        const v140 = stdlib.add(v137, v113);
        sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Digest]), [stdlib.checkedBigNumberify('./index.rsh:64:17:after expr stmt semicolon', stdlib.UInt_max, 4), v140, v48, v47, v58, v112]);
        sim_r.isHalt = false;
        
        return sim_r;
         }));
      if (txn3.didTimeout) {
        const txn4 = await ctc.recv('Alice', 9, 0, [], false);
        const [] = txn4.data;
        const v116 = txn4.value;
        const v117 = stdlib.eq(v116, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
        stdlib.assert(v117, {
          at: 'reach standard library:77:16:after expr stmt semicolon',
          fs: ['at ./index.rsh:63:43:application call to "function" (defined at: reach standard library:76:8:function exp)'],
          msg: 'pay amount correct',
          who: 'Alice'
           });
        const v118 = v88;
        const v121 = stdlib.add(v118, v116);
        const v122 = v121;
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
        const [v112] = txn3.data;
        const v113 = txn3.value;
        const v114 = stdlib.eq(v113, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
        stdlib.assert(v114, {
          at: './index.rsh:63:62:after expr stmt semicolon',
          fs: [],
          msg: 'pay amount correct',
          who: 'Alice'
           });
        const v137 = v88;
        const v140 = stdlib.add(v137, v113);
        const txn4 = await ctc.recv('Alice', 5, 1, [stdlib.T_UInt], stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10));
        if (txn4.didTimeout) {
          const txn5 = await ctc.sendrecv('Alice', 8, 0, [stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Digest], [v140, v48, v47, v58, v112], stdlib.checkedBigNumberify('reach standard library:77:16:after expr stmt semicolon', stdlib.UInt_max, 0), [], false, ((txn5) => {
            const sim_r = { txns: [] };
            sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Digest]), [stdlib.checkedBigNumberify('reach standard library:77:6:dot', stdlib.UInt_max, 4), v140, v48, v47, v58, v112]);
            const [] = txn5.data;
            const v148 = txn5.value;
            
            const v149 = stdlib.eq(v148, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v149, {
              at: 'reach standard library:77:16:after expr stmt semicolon',
              fs: ['at ./index.rsh:70:43:application call to "function" (defined at: reach standard library:76:8:function exp)'],
              msg: 'pay amount correct',
              who: 'Alice'
               });
            const v150 = v140;
            const v153 = stdlib.add(v150, v148);
            const v154 = v153;
            sim_r.txns.push({
              amt: v154,
              to: v48
               });
            sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([]), []);
            sim_r.isHalt = true;
            
            return sim_r;
             }));
          const [] = txn5.data;
          const v148 = txn5.value;
          const v149 = stdlib.eq(v148, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
          stdlib.assert(v149, {
            at: 'reach standard library:77:16:after expr stmt semicolon',
            fs: ['at ./index.rsh:70:43:application call to "function" (defined at: reach standard library:76:8:function exp)'],
            msg: 'pay amount correct',
            who: 'Alice'
             });
          const v150 = v140;
          const v153 = stdlib.add(v150, v148);
          const v154 = v153;
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
          const [v144] = txn4.data;
          const v145 = txn4.value;
          const v146 = stdlib.eq(v145, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
          stdlib.assert(v146, {
            at: './index.rsh:70:62:after expr stmt semicolon',
            fs: [],
            msg: 'pay amount correct',
            who: 'Alice'
             });
          const v169 = v140;
          const v172 = stdlib.add(v169, v145);
          const txn5 = await ctc.sendrecv('Alice', 6, 2, [stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt], [v172, v48, v47, v58, v112, v144, v109, v107], stdlib.checkedBigNumberify('./index.rsh:76:62:after expr stmt semicolon', stdlib.UInt_max, 0), [stdlib.T_UInt, stdlib.T_UInt], stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10), ((txn5) => {
            const sim_r = { txns: [] };
            sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:75:10:dot', stdlib.UInt_max, 5), v172, v48, v47, v58, v112, v144]);
            const [v175, v176] = txn5.data;
            const v177 = txn5.value;
            
            const v178 = stdlib.eq(v177, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v178, {
              at: './index.rsh:76:62:after expr stmt semicolon',
              fs: [],
              msg: 'pay amount correct',
              who: 'Alice'
               });
            const v201 = v172;
            const v204 = stdlib.add(v201, v177);
            const v206 = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt]), [v175, v176]);
            const v208 = stdlib.digestEq(v112, v206);
            stdlib.assert(v208, {
              at: 'reach standard library:74:17:application',
              fs: ['at ./index.rsh:77:24:application call to "function" (defined at: reach standard library:73:8:function exp)'],
              msg: null,
              who: 'Alice'
               });
            const v213 = stdlib.sub(stdlib.checkedBigNumberify('./index.rsh:7:18:decimal', stdlib.UInt_max, 4), v144);
            const v216 = stdlib.add(v176, v213);
            const v217 = stdlib.mod(v216, stdlib.checkedBigNumberify('./index.rsh:7:32:decimal', stdlib.UInt_max, 3));
            const v219 = v204;
            const cv88 = v219;
            const cv89 = v217;
            
            (() => {
              const v88 = cv88;
              const v89 = cv89;
              
              if ((() => {
                const v103 = stdlib.eq(v89, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
                
                return v103; })()) {
                sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:56:17:after expr stmt semicolon', stdlib.UInt_max, 3), v48, v47, v58, v88]);
                sim_r.isHalt = false;
                 }
              else {
                const v221 = stdlib.eq(v89, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
                const v227 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:83:16:decimal', stdlib.UInt_max, 2), v47);
                const v232 = v221 ? v48 : v58;
                sim_r.txns.push({
                  amt: v227,
                  to: v232
                   });
                sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([]), []);
                sim_r.isHalt = true;
                 } })();
            return sim_r;
             }));
          if (txn5.didTimeout) {
            const txn6 = await ctc.recv('Alice', 7, 0, [], false);
            const [] = txn6.data;
            const v180 = txn6.value;
            const v181 = stdlib.eq(v180, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v181, {
              at: 'reach standard library:77:16:after expr stmt semicolon',
              fs: ['at ./index.rsh:76:43:application call to "function" (defined at: reach standard library:76:8:function exp)'],
              msg: 'pay amount correct',
              who: 'Alice'
               });
            const v182 = v172;
            const v185 = stdlib.add(v182, v180);
            const v186 = v185;
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
            const [v175, v176] = txn5.data;
            const v177 = txn5.value;
            const v178 = stdlib.eq(v177, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v178, {
              at: './index.rsh:76:62:after expr stmt semicolon',
              fs: [],
              msg: 'pay amount correct',
              who: 'Alice'
               });
            const v201 = v172;
            const v204 = stdlib.add(v201, v177);
            const v206 = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt]), [v175, v176]);
            const v208 = stdlib.digestEq(v112, v206);
            stdlib.assert(v208, {
              at: 'reach standard library:74:17:application',
              fs: ['at ./index.rsh:77:24:application call to "function" (defined at: reach standard library:73:8:function exp)'],
              msg: null,
              who: 'Alice'
               });
            const v213 = stdlib.sub(stdlib.checkedBigNumberify('./index.rsh:7:18:decimal', stdlib.UInt_max, 4), v144);
            const v216 = stdlib.add(v176, v213);
            const v217 = stdlib.mod(v216, stdlib.checkedBigNumberify('./index.rsh:7:32:decimal', stdlib.UInt_max, 3));
            const v219 = v204;
            const cv88 = v219;
            const cv89 = v217;
            
            v88 = cv88;
            v89 = cv89;
            
            continue; }
           }
         }
       }
    const v221 = stdlib.eq(v89, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
    const v227 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:83:16:decimal', stdlib.UInt_max, 2), v47);
    const v232 = v221 ? v48 : v58;
    ;
    stdlib.protect(stdlib.T_Null, await interact.seeOutcome(v89), {
      at: './index.rsh:87:28:application',
      fs: ['at ./index.rsh:87:41:after expr stmt semicolon call to "function" (defined at: ./index.rsh:86:23:function exp)'],
      msg: 'seeOutcome',
      who: 'Alice'
       });
    return; }
  
   }
export async function Bob(stdlib, ctc, interact) {
  const txn1 = await ctc.recv('Bob', 1, 1, [stdlib.T_UInt], false);
  const [v47] = txn1.data;
  const v49 = txn1.value;
  const v48 = txn1.from;
  const v50 = stdlib.eq(v49, v47);
  stdlib.assert(v50, {
    at: './index.rsh:45:20:after expr stmt semicolon',
    fs: [],
    msg: 'pay amount correct',
    who: 'Bob'
     });
  const v51 = stdlib.checkedBigNumberify('./index.rsh:compileDApp', stdlib.UInt_max, 0);
  const v54 = stdlib.add(v51, v49);
  stdlib.protect(stdlib.T_Null, await interact.acceptWager(v47), {
    at: './index.rsh:49:29:application',
    fs: ['at ./index.rsh:49:40:after expr stmt semicolon call to "function" (defined at: ./index.rsh:48:17:function exp)'],
    msg: 'acceptWager',
    who: 'Bob'
     });
  const txn2 = await ctc.sendrecv('Bob', 2, 0, [stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt], [v54, v48, v47], v47, [], stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10), ((txn2) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:50:8:dot', stdlib.UInt_max, 1), v54, v48, v47]);
    const [] = txn2.data;
    const v59 = txn2.value;
    const v58 = txn2.from;
    
    const v60 = stdlib.eq(v59, v47);
    stdlib.assert(v60, {
      at: './index.rsh:51:60:after expr stmt semicolon',
      fs: [],
      msg: 'pay amount correct',
      who: 'Bob'
       });
    const v83 = v54;
    const v86 = stdlib.add(v83, v59);
    const v87 = v86;
    const v88 = v87;
    const v89 = stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1);
    
    if ((() => {
      const v103 = stdlib.eq(v89, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
      
      return v103; })()) {
      sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:56:17:after expr stmt semicolon', stdlib.UInt_max, 3), v48, v47, v58, v88]);
      sim_r.isHalt = false;
       }
    else {
      const v221 = stdlib.eq(v89, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
      const v227 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:83:16:decimal', stdlib.UInt_max, 2), v47);
      const v232 = v221 ? v48 : v58;
      sim_r.txns.push({
        amt: v227,
        to: v232
         });
      sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([]), []);
      sim_r.isHalt = true;
       }
    return sim_r;
     }));
  if (txn2.didTimeout) {
    const txn3 = await ctc.recv('Bob', 10, 0, [], false);
    const [] = txn3.data;
    const v62 = txn3.value;
    const v63 = stdlib.eq(v62, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
    stdlib.assert(v63, {
      at: 'reach standard library:77:16:after expr stmt semicolon',
      fs: ['at ./index.rsh:51:41:application call to "function" (defined at: reach standard library:76:8:function exp)'],
      msg: 'pay amount correct',
      who: 'Bob'
       });
    const v64 = v54;
    const v67 = stdlib.add(v64, v62);
    const v68 = v67;
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
    const v59 = txn2.value;
    const v58 = txn2.from;
    const v60 = stdlib.eq(v59, v47);
    stdlib.assert(v60, {
      at: './index.rsh:51:60:after expr stmt semicolon',
      fs: [],
      msg: 'pay amount correct',
      who: 'Bob'
       });
    const v83 = v54;
    const v86 = stdlib.add(v83, v59);
    const v87 = v86;
    let v88 = v87;
    let v89 = stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1);
    
    while ((() => {
      const v103 = stdlib.eq(v89, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
      
      return v103; })()) {
      const txn3 = await ctc.recv('Bob', 4, 1, [stdlib.T_Digest], stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10));
      if (txn3.didTimeout) {
        const txn4 = await ctc.sendrecv('Bob', 9, 0, [stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt], [v48, v47, v58, v88], stdlib.checkedBigNumberify('reach standard library:77:16:after expr stmt semicolon', stdlib.UInt_max, 0), [], false, ((txn4) => {
          const sim_r = { txns: [] };
          sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt]), [stdlib.checkedBigNumberify('reach standard library:77:6:dot', stdlib.UInt_max, 3), v48, v47, v58, v88]);
          const [] = txn4.data;
          const v116 = txn4.value;
          
          const v117 = stdlib.eq(v116, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
          stdlib.assert(v117, {
            at: 'reach standard library:77:16:after expr stmt semicolon',
            fs: ['at ./index.rsh:63:43:application call to "function" (defined at: reach standard library:76:8:function exp)'],
            msg: 'pay amount correct',
            who: 'Bob'
             });
          const v118 = v88;
          const v121 = stdlib.add(v118, v116);
          const v122 = v121;
          sim_r.txns.push({
            amt: v122,
            to: v58
             });
          sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([]), []);
          sim_r.isHalt = true;
          
          return sim_r;
           }));
        const [] = txn4.data;
        const v116 = txn4.value;
        const v117 = stdlib.eq(v116, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
        stdlib.assert(v117, {
          at: 'reach standard library:77:16:after expr stmt semicolon',
          fs: ['at ./index.rsh:63:43:application call to "function" (defined at: reach standard library:76:8:function exp)'],
          msg: 'pay amount correct',
          who: 'Bob'
           });
        const v118 = v88;
        const v121 = stdlib.add(v118, v116);
        const v122 = v121;
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
        const [v112] = txn3.data;
        const v113 = txn3.value;
        const v114 = stdlib.eq(v113, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
        stdlib.assert(v114, {
          at: './index.rsh:63:62:after expr stmt semicolon',
          fs: [],
          msg: 'pay amount correct',
          who: 'Bob'
           });
        const v137 = v88;
        const v140 = stdlib.add(v137, v113);
        const v143 = stdlib.protect(stdlib.T_UInt, await interact.getHand(), {
          at: './index.rsh:68:52:application',
          fs: ['at ./index.rsh:68:59:after expr stmt semicolon call to "function" (defined at: ./index.rsh:67:19:function exp)'],
          msg: 'getHand',
          who: 'Bob'
           });
        const txn4 = await ctc.sendrecv('Bob', 5, 1, [stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt], [v140, v48, v47, v58, v112, v143], stdlib.checkedBigNumberify('./index.rsh:70:62:after expr stmt semicolon', stdlib.UInt_max, 0), [stdlib.T_UInt], stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10), ((txn4) => {
          const sim_r = { txns: [] };
          sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Digest]), [stdlib.checkedBigNumberify('./index.rsh:69:10:dot', stdlib.UInt_max, 4), v140, v48, v47, v58, v112]);
          const [v144] = txn4.data;
          const v145 = txn4.value;
          
          const v146 = stdlib.eq(v145, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
          stdlib.assert(v146, {
            at: './index.rsh:70:62:after expr stmt semicolon',
            fs: [],
            msg: 'pay amount correct',
            who: 'Bob'
             });
          const v169 = v140;
          const v172 = stdlib.add(v169, v145);
          sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:71:17:after expr stmt semicolon', stdlib.UInt_max, 5), v172, v48, v47, v58, v112, v144]);
          sim_r.isHalt = false;
          
          return sim_r;
           }));
        if (txn4.didTimeout) {
          const txn5 = await ctc.recv('Bob', 8, 0, [], false);
          const [] = txn5.data;
          const v148 = txn5.value;
          const v149 = stdlib.eq(v148, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
          stdlib.assert(v149, {
            at: 'reach standard library:77:16:after expr stmt semicolon',
            fs: ['at ./index.rsh:70:43:application call to "function" (defined at: reach standard library:76:8:function exp)'],
            msg: 'pay amount correct',
            who: 'Bob'
             });
          const v150 = v140;
          const v153 = stdlib.add(v150, v148);
          const v154 = v153;
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
          const [v144] = txn4.data;
          const v145 = txn4.value;
          const v146 = stdlib.eq(v145, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
          stdlib.assert(v146, {
            at: './index.rsh:70:62:after expr stmt semicolon',
            fs: [],
            msg: 'pay amount correct',
            who: 'Bob'
             });
          const v169 = v140;
          const v172 = stdlib.add(v169, v145);
          const txn5 = await ctc.recv('Bob', 6, 2, [stdlib.T_UInt, stdlib.T_UInt], stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10));
          if (txn5.didTimeout) {
            const txn6 = await ctc.sendrecv('Bob', 7, 0, [stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt], [v172, v48, v47, v58, v112, v144], stdlib.checkedBigNumberify('reach standard library:77:16:after expr stmt semicolon', stdlib.UInt_max, 0), [], false, ((txn6) => {
              const sim_r = { txns: [] };
              sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt]), [stdlib.checkedBigNumberify('reach standard library:77:6:dot', stdlib.UInt_max, 5), v172, v48, v47, v58, v112, v144]);
              const [] = txn6.data;
              const v180 = txn6.value;
              
              const v181 = stdlib.eq(v180, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
              stdlib.assert(v181, {
                at: 'reach standard library:77:16:after expr stmt semicolon',
                fs: ['at ./index.rsh:76:43:application call to "function" (defined at: reach standard library:76:8:function exp)'],
                msg: 'pay amount correct',
                who: 'Bob'
                 });
              const v182 = v172;
              const v185 = stdlib.add(v182, v180);
              const v186 = v185;
              sim_r.txns.push({
                amt: v186,
                to: v58
                 });
              sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([]), []);
              sim_r.isHalt = true;
              
              return sim_r;
               }));
            const [] = txn6.data;
            const v180 = txn6.value;
            const v181 = stdlib.eq(v180, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v181, {
              at: 'reach standard library:77:16:after expr stmt semicolon',
              fs: ['at ./index.rsh:76:43:application call to "function" (defined at: reach standard library:76:8:function exp)'],
              msg: 'pay amount correct',
              who: 'Bob'
               });
            const v182 = v172;
            const v185 = stdlib.add(v182, v180);
            const v186 = v185;
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
            const [v175, v176] = txn5.data;
            const v177 = txn5.value;
            const v178 = stdlib.eq(v177, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v178, {
              at: './index.rsh:76:62:after expr stmt semicolon',
              fs: [],
              msg: 'pay amount correct',
              who: 'Bob'
               });
            const v201 = v172;
            const v204 = stdlib.add(v201, v177);
            const v206 = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt]), [v175, v176]);
            const v208 = stdlib.digestEq(v112, v206);
            stdlib.assert(v208, {
              at: 'reach standard library:74:17:application',
              fs: ['at ./index.rsh:77:24:application call to "function" (defined at: reach standard library:73:8:function exp)'],
              msg: null,
              who: 'Bob'
               });
            const v213 = stdlib.sub(stdlib.checkedBigNumberify('./index.rsh:7:18:decimal', stdlib.UInt_max, 4), v144);
            const v216 = stdlib.add(v176, v213);
            const v217 = stdlib.mod(v216, stdlib.checkedBigNumberify('./index.rsh:7:32:decimal', stdlib.UInt_max, 3));
            const v219 = v204;
            const cv88 = v219;
            const cv89 = v217;
            
            v88 = cv88;
            v89 = cv89;
            
            continue; }
           }
         }
       }
    const v221 = stdlib.eq(v89, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
    const v227 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:83:16:decimal', stdlib.UInt_max, 2), v47);
    const v232 = v221 ? v48 : v58;
    ;
    stdlib.protect(stdlib.T_Null, await interact.seeOutcome(v89), {
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
        "components": [
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "_last",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a0svs",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v47",
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
                "internalType": "uint256",
                "name": "_last",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v54",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v48",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v47",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a1svs",
            "name": "svs",
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
                "internalType": "uint256",
                "name": "_last",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v54",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v48",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v47",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a1svs",
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
                "internalType": "uint256",
                "name": "_last",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v48",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v47",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v58",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v88",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a3svs",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v112",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a4msg",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct ReachContract.a4",
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
                "internalType": "uint256",
                "name": "_last",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v140",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v48",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v47",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v58",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v112",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a4svs",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v144",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a5msg",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct ReachContract.a5",
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
                "internalType": "uint256",
                "name": "_last",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v172",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v48",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v47",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v58",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v112",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v144",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a5svs",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v175",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v176",
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
                "internalType": "uint256",
                "name": "_last",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v172",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v48",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v47",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v58",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v112",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v144",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a5svs",
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
                "internalType": "uint256",
                "name": "_last",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v140",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v48",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v47",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v58",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v112",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a4svs",
            "name": "svs",
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
                "internalType": "uint256",
                "name": "_last",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v48",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v47",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v58",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v88",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a3svs",
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
                "name": "_last",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a0svs",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v47",
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
                "internalType": "uint256",
                "name": "_last",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v54",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v48",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v47",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a1svs",
            "name": "svs",
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
                "internalType": "uint256",
                "name": "_last",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v54",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v48",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v47",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a1svs",
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
                "internalType": "uint256",
                "name": "_last",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v48",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v47",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v58",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v88",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a3svs",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v112",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a4msg",
            "name": "msg",
            "type": "tuple"
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
            "components": [
              {
                "internalType": "uint256",
                "name": "_last",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v140",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v48",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v47",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v58",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v112",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a4svs",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v144",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a5msg",
            "name": "msg",
            "type": "tuple"
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
            "components": [
              {
                "internalType": "uint256",
                "name": "_last",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v172",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v48",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v47",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v58",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v112",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v144",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a5svs",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v175",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v176",
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
                "internalType": "uint256",
                "name": "_last",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v172",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v48",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v47",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v58",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v112",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v144",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a5svs",
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
                "internalType": "uint256",
                "name": "_last",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v140",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v48",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v47",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v58",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v112",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a4svs",
            "name": "svs",
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
                "internalType": "uint256",
                "name": "_last",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v48",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v47",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v58",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v88",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a3svs",
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
  Bytecode: `0x608060408190527f49ff028a829527a47ec6839c7147b484eccf5a2a94853eddac09cef44d9d4e9e90600090a161003461006c565b43815260405161004b90600090839060200161007f565b60408051601f1981840301815291905280516020909101206000555061008e565b6040518060200160405280600081525090565b91825251602082015260400190565b6114138061009d6000396000f3fe6080604052600436106100865760003560e01c80639eec5f52116100595780639eec5f52146100d9578063cbd05326146100ec578063dc6b87a4146100ff578063f3afda8714610112578063f90ed0a71461012557610086565b80633172e0931461008b5780634db5a838146100a057806378663485146100b35780639532ef01146100c6575b600080fd5b61009e610099366004610f53565b610138565b005b61009e6100ae366004610fc7565b61025d565b61009e6100c1366004610fb5565b610373565b61009e6100d4366004610f6e565b610526565b61009e6100e7366004610f53565b610616565b61009e6100fa366004610f9a565b610718565b61009e61010d366004610f7f565b61082e565b61009e610120366004610f7f565b610987565b61009e610133366004610f9a565b610a9d565b60405161014c90600190839060200161123c565b6040516020818303038152906040528051906020012060001c6000541461017257600080fd5b6101826060820160408301610f32565b6001600160a01b0316336001600160a01b03161461019f57600080fd5b6101ab8135600a610bf6565b43101580156101b8575060015b6101c157600080fd5b34156101cc57600080fd5b6101dc6060820160408301610f32565b6001600160a01b03166108fc6101f6602084013534610bf6565b6040518115909202916000818181858888f1935050505015801561021e573d6000803e3d6000fd5b507f36179c741fc1bb124e0153ce4bcf6da5698255ba865d0a8435ff39a7e38777028160405161024e919061117b565b60405180910390a16000805533ff5b60405161027190600390839060200161128c565b6040516020818303038152906040528051906020012060001c6000541461029757600080fd5b6102a76080820160608301610f32565b6001600160a01b0316336001600160a01b0316146102c457600080fd5b6102d08135600a610bf6565b43101580156102dd575060015b6102e657600080fd5b34156102f157600080fd5b6103016080820160608301610f32565b6001600160a01b03166108fc61031b608084013534610bf6565b6040518115909202916000818181858888f19350505050158015610343573d6000803e3d6000fd5b507fe0668fd27fde340a6c82ca502f93218b3bea78e9d519863afdca65b1c2239a7d8160405161024e919061121f565b604051610387906005908390602001611356565b6040516020818303038152906040528051906020012060001c600054146103ad57600080fd5b6103bd6060820160408301610f32565b6001600160a01b0316336001600160a01b0316146103da57600080fd5b6103e68135600a610bf6565b43106103f157600080fd5b34156103fc57600080fd5b6040516104189060e083013590610100840135906020016113cf565b60408051601f19818403018152919052805160209091012060a08201351461043f57600080fd5b7f52a9fef337031532c4a6a85f6a4dbbf491a4ffabc353ff28ec7e745c4f27e9e18160405161046e91906111d8565b60405180910390a161047e610d82565b61048e6060830160408401610f32565b81516001600160a01b03909116602090910152805160608301356040909101526104be60a0830160808401610f32565b81516001600160a01b039091166060909101526104df602083013534610bf6565b60208201515260036105046101008401356104ff600460c0870135610c28565b610bf6565b8161050b57fe5b068160200151602001818152505061052281610c4b565b5050565b60405161053a90600090839060200161122d565b6040516020818303038152906040528051906020012060001c6000541461056057600080fd5b610568610da7565b3460208301351461057857600080fd5b610583600034610bf6565b81526040517ff2c62eba998811305a23599b2e6d212befbd7ded3a73f4c08bfb9aefe08dc166906105b5908490611189565b60405180910390a16105c5610dba565b438152815160208083019190915233604080840191909152848201356060840152516105f691600191849101611250565b60408051601f198184030181529190528051602090910120600055505050565b60405161062a90600190839060200161123c565b6040516020818303038152906040528051906020012060001c6000541461065057600080fd5b61065c8135600a610bf6565b431061066757600080fd5b3460608201351461067757600080fd5b7f8301f7e9c0c09395781547f8c2ac4de6f0d6a74724cd84af79d6fb3e60faa02e816040516106a6919061117b565b60405180910390a16106b6610d82565b6106c66060830160408401610f32565b81516001600160a01b0390911660209182015281516060808501356040909201919091528251339101526106fd9083013534610bf6565b60208083018051929092529051600191015261052281610c4b565b60405161072c906005908390602001611356565b6040516020818303038152906040528051906020012060001c6000541461075257600080fd5b61076260a0820160808301610f32565b6001600160a01b0316336001600160a01b03161461077f57600080fd5b61078b8135600a610bf6565b4310158015610798575060015b6107a157600080fd5b34156107ac57600080fd5b6107bc60a0820160808301610f32565b6001600160a01b03166108fc6107d6602084013534610bf6565b6040518115909202916000818181858888f193505050501580156107fe573d6000803e3d6000fd5b507f0ee798ffe814494451796dadf731f393e9ad7998023b83a1d012eefbac5c8e668160405161024e9190611203565b60405161084290600390839060200161128c565b6040516020818303038152906040528051906020012060001c6000541461086857600080fd5b610870610da7565b6108806040830160208401610f32565b6001600160a01b0316336001600160a01b03161461089d57600080fd5b6108a98235600a610bf6565b43106108b457600080fd5b34156108bf57600080fd5b6108cd608083013534610bf6565b81526040517fbf257526bd938402a4de90dd31128f02dca2d0d4999a5e316b439d0b1d62d240906108ff9084906111a0565b60405180910390a161090f610deb565b438152815160208083019190915261092d9060408501908501610f32565b6001600160a01b031660408083019190915283013560608083019190915261095b9060808501908501610f32565b6001600160a01b0316608082015260a080840135908201526040516105f69060049083906020016112ff565b60405161099b9060049083906020016112eb565b6040516020818303038152906040528051906020012060001c600054146109c157600080fd5b6109d16060820160408301610f32565b6001600160a01b0316336001600160a01b0316146109ee57600080fd5b6109fa8135600a610bf6565b4310158015610a07575060015b610a1057600080fd5b3415610a1b57600080fd5b610a2b6060820160408301610f32565b6001600160a01b03166108fc610a45602084013534610bf6565b6040518115909202916000818181858888f19350505050158015610a6d573d6000803e3d6000fd5b507f701b2f4e150abb4bd2e9902305eb7ca3a8f2e4c32799ae2c1000d644283642d48160405161024e9190611211565b604051610ab19060049083906020016112eb565b6040516020818303038152906040528051906020012060001c60005414610ad757600080fd5b610adf610da7565b610aef60a0830160808401610f32565b6001600160a01b0316336001600160a01b031614610b0c57600080fd5b610b188235600a610bf6565b4310610b2357600080fd5b3415610b2e57600080fd5b610b3c602083013534610bf6565b81526040517fbd4029075bd81a55d641a01d42be16518b3120211e1c8dd2ce49cfd56055d9a090610b6e9084906111bc565b60405180910390a1610b7e610e33565b43815281516020820152610b986060840160408501610f32565b6001600160a01b0316604082015260608084013590820152610bc060a0840160808501610f32565b6001600160a01b0316608082015260a0808401359082015260c080840135908201526040516105f690600590839060200161136b565b80820182811015610c225760405162461bcd60e51b8152600401610c1990611155565b60405180910390fd5b92915050565b80820382811115610c225760405162461bcd60e51b8152600401610c1990611107565b60018160200151602001511415610cd757610c64610e82565b43815281516020908101516001600160a01b03908116828401528351604090810151818501528451606090810151909216918401919091528184015151608084015251610cb6916003918491016112a0565b60408051601f19818403018152919052805160209091012060005550610d48565b600281602001516020015114610cf257805160600151610cf9565b8051602001515b6001600160a01b03166108fc610d186002846000015160400151610d4b565b6040518115909202916000818181858888f19350505050158015610d40573d6000803e3d6000fd5b506000805533ff5b50565b6000811580610d6657505080820282828281610d6357fe5b04145b610c225760405162461bcd60e51b8152600401610c199061112f565b6040518060400160405280610d95610e82565b8152602001610da2610ec3565b905290565b6040518060200160405280600081525090565b6040518060800160405280600081526020016000815260200160006001600160a01b03168152602001600081525090565b6040518060c00160405280600081526020016000815260200160006001600160a01b031681526020016000815260200160006001600160a01b03168152602001600081525090565b6040518060e00160405280600081526020016000815260200160006001600160a01b031681526020016000815260200160006001600160a01b0316815260200160008152602001600081525090565b6040518060a001604052806000815260200160006001600160a01b031681526020016000815260200160006001600160a01b03168152602001600081525090565b604051806040016040528060008152602001600081525090565b80356001600160a01b0381168114610ef457600080fd5b919050565b600060808284031215610f0a578081fd5b50919050565b600060c08284031215610f0a578081fd5b600060e08284031215610f0a578081fd5b600060208284031215610f43578081fd5b610f4c82610edd565b9392505050565b600060808284031215610f64578081fd5b610f4c8383610ef9565b600060408284031215610f0a578081fd5b600060c08284031215610f90578081fd5b610f4c8383610f10565b600060e08284031215610fab578081fd5b610f4c8383610f21565b60006101208284031215610f0a578081fd5b600060a08284031215610f0a578081fd5b80358252602080820135908301526001600160a01b03610ffa60408301610edd565b166040830152606090810135910152565b8035825261101b60208201610edd565b6001600160a01b038181166020850152604083810135908501528061104260608501610edd565b1660608501525050608090810135910152565b803582526020810135602083015261106f60408201610edd565b6001600160a01b038181166040850152606083810135908501528061109660808501610edd565b166080850152505060a090810135910152565b80358252602081013560208301526110c360408201610edd565b6001600160a01b03818116604085015260608381013590850152806110ea60808501610edd565b166080850152505060a0818101359083015260c090810135910152565b6020808252600e908201526d1cdd58881ddc985c185c9bdd5b9960921b604082015260600190565b6020808252600c908201526b6d756c206f766572666c6f7760a01b604082015260600190565b6020808252600c908201526b616464206f766572666c6f7760a01b604082015260600190565b60808101610c228284610fd8565b813581526020918201359181019190915260400190565b60c081016111ae828461100b565b60a092830135919092015290565b60e081016111ca8284611055565b60c092830135919092015290565b61012081016111e782846110a9565b60e083013560e083015261010080840135818401525092915050565b60e08101610c2282846110a9565b60c08101610c228284611055565b60a08101610c22828461100b565b91825235602082015260400190565b82815260a08101610f4c6020830184610fd8565b91825280516020808401919091528101516040808401919091528101516001600160a01b03166060808401919091520151608082015260a00190565b82815260c08101610f4c602083018461100b565b91825280516020808401919091528101516001600160a01b0390811660408085019190915282015160608085019190915282015116608080840191909152015160a082015260c00190565b82815260e08101610f4c6020830184611055565b91825280516020808401919091528101516040808401919091528101516001600160a01b039081166060808501919091528201516080808501919091528201511660a080840191909152015160c082015260e00190565b8281526101008101610f4c60208301846110a9565b6000610100820190508382528251602083015260208301516040830152604083015160018060a01b038082166060850152606085015160808501528060808601511660a0850152505060a083015160c083015260c083015160e08301529392505050565b91825260208201526040019056fea2646970667358221220ae6ee297fb2aeb1b3c15d2ccb6afb7ca692cdc1c4bc0604c7cdf596e9049536d64736f6c63430007040033`,
  deployMode: `DM_constructor`
   };

export const _Connectors = {
  ALGO: _ALGO,
  ETH: _ETH
   };

