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
            "internalType": "struct ReachContract.a0postsvs",
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
            "internalType": "struct ReachContract.a1postsvs",
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
            "internalType": "struct ReachContract.a3postsvs",
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
            "internalType": "struct ReachContract.a4postsvs",
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
            "internalType": "struct ReachContract.a5postsvs",
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
            "internalType": "struct ReachContract.a5postsvs",
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
            "internalType": "struct ReachContract.a4postsvs",
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
            "internalType": "struct ReachContract.a3postsvs",
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
            "internalType": "struct ReachContract.a0postsvs",
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
            "internalType": "struct ReachContract.a1postsvs",
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
            "internalType": "struct ReachContract.a3postsvs",
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
            "internalType": "struct ReachContract.a4postsvs",
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
            "internalType": "struct ReachContract.a5postsvs",
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
            "internalType": "struct ReachContract.a5postsvs",
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
            "internalType": "struct ReachContract.a4postsvs",
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
            "internalType": "struct ReachContract.a3postsvs",
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
  Bytecode: `0x608060408190527f49ff028a829527a47ec6839c7147b484eccf5a2a94853eddac09cef44d9d4e9e90600090a161003461006c565b43815260405161004b90600090839060200161007f565b60408051601f1981840301815291905280516020909101206000555061008e565b6040518060200160405280600081525090565b91825251602082015260400190565b6114258061009d6000396000f3fe6080604052600436106100865760003560e01c80639eec5f52116100595780639eec5f52146100d9578063cbd05326146100ec578063dc6b87a4146100ff578063f3afda8714610112578063f90ed0a71461012557610086565b80633172e0931461008b5780634db5a838146100a057806378663485146100b35780639532ef01146100c6575b600080fd5b61009e610099366004610f65565b610138565b005b61009e6100ae366004610fd9565b61025d565b61009e6100c1366004610fc7565b610373565b61009e6100d4366004610f80565b610522565b61009e6100e7366004610f65565b610612565b61009e6100fa366004610fac565b61070f565b61009e61010d366004610f91565b610825565b61009e610120366004610f91565b61097e565b61009e610133366004610fac565b610a94565b60405161014c90600190839060200161124e565b6040516020818303038152906040528051906020012060001c6000541461017257600080fd5b6101826060820160408301610f44565b6001600160a01b0316336001600160a01b03161461019f57600080fd5b6101ab8135600a610bed565b43101580156101b8575060015b6101c157600080fd5b34156101cc57600080fd5b6101dc6060820160408301610f44565b6001600160a01b03166108fc6101f6602084013534610bed565b6040518115909202916000818181858888f1935050505015801561021e573d6000803e3d6000fd5b507f36179c741fc1bb124e0153ce4bcf6da5698255ba865d0a8435ff39a7e38777028160405161024e919061118d565b60405180910390a16000805533ff5b60405161027190600390839060200161129e565b6040516020818303038152906040528051906020012060001c6000541461029757600080fd5b6102a76080820160608301610f44565b6001600160a01b0316336001600160a01b0316146102c457600080fd5b6102d08135600a610bed565b43101580156102dd575060015b6102e657600080fd5b34156102f157600080fd5b6103016080820160608301610f44565b6001600160a01b03166108fc61031b608084013534610bed565b6040518115909202916000818181858888f19350505050158015610343573d6000803e3d6000fd5b507fe0668fd27fde340a6c82ca502f93218b3bea78e9d519863afdca65b1c2239a7d8160405161024e9190611231565b604051610387906005908390602001611368565b6040516020818303038152906040528051906020012060001c600054146103ad57600080fd5b6103bd6060820160408301610f44565b6001600160a01b0316336001600160a01b0316146103da57600080fd5b6103e68135600a610bed565b43106103f157600080fd5b34156103fc57600080fd5b6040516104189060e083013590610100840135906020016113e1565b60408051601f19818403018152919052805160209091012060a08201351461043f57600080fd5b7f52a9fef337031532c4a6a85f6a4dbbf491a4ffabc353ff28ec7e745c4f27e9e18160405161046e91906111ea565b60405180910390a161047e610d74565b61048e6060830160408401610f44565b81516001600160a01b039091169052805160608301356020909101526104ba60a0830160808401610f44565b81516001600160a01b039091166040909101526104db602083013534610bed565b60208201515260036105006101008401356104fb600460c0870135610c1f565b610bed565b8161050757fe5b068160200151602001818152505061051e81610c42565b5050565b60405161053690600090839060200161123f565b6040516020818303038152906040528051906020012060001c6000541461055c57600080fd5b610564610d99565b3460208301351461057457600080fd5b61057f600034610bed565b81526040517ff2c62eba998811305a23599b2e6d212befbd7ded3a73f4c08bfb9aefe08dc166906105b190849061119b565b60405180910390a16105c1610dac565b438152815160208083019190915233604080840191909152848201356060840152516105f291600191849101611262565b60408051601f198184030181529190528051602090910120600055505050565b60405161062690600190839060200161124e565b6040516020818303038152906040528051906020012060001c6000541461064c57600080fd5b6106588135600a610bed565b431061066357600080fd5b3460608201351461067357600080fd5b7f8301f7e9c0c09395781547f8c2ac4de6f0d6a74724cd84af79d6fb3e60faa02e816040516106a2919061118d565b60405180910390a16106b2610d74565b6106c26060830160408401610f44565b81516001600160a01b039091169052805160608301356020918201528151336040909101526106f49083013534610bed565b60208083018051929092529051600191015261051e81610c42565b604051610723906005908390602001611368565b6040516020818303038152906040528051906020012060001c6000541461074957600080fd5b61075960a0820160808301610f44565b6001600160a01b0316336001600160a01b03161461077657600080fd5b6107828135600a610bed565b431015801561078f575060015b61079857600080fd5b34156107a357600080fd5b6107b360a0820160808301610f44565b6001600160a01b03166108fc6107cd602084013534610bed565b6040518115909202916000818181858888f193505050501580156107f5573d6000803e3d6000fd5b507f0ee798ffe814494451796dadf731f393e9ad7998023b83a1d012eefbac5c8e668160405161024e9190611215565b60405161083990600390839060200161129e565b6040516020818303038152906040528051906020012060001c6000541461085f57600080fd5b610867610d99565b6108776040830160208401610f44565b6001600160a01b0316336001600160a01b03161461089457600080fd5b6108a08235600a610bed565b43106108ab57600080fd5b34156108b657600080fd5b6108c4608083013534610bed565b81526040517fbf257526bd938402a4de90dd31128f02dca2d0d4999a5e316b439d0b1d62d240906108f69084906111b2565b60405180910390a1610906610ddd565b43815281516020808301919091526109249060408501908501610f44565b6001600160a01b03166040808301919091528301356060808301919091526109529060808501908501610f44565b6001600160a01b0316608082015260a080840135908201526040516105f2906004908390602001611311565b6040516109929060049083906020016112fd565b6040516020818303038152906040528051906020012060001c600054146109b857600080fd5b6109c86060820160408301610f44565b6001600160a01b0316336001600160a01b0316146109e557600080fd5b6109f18135600a610bed565b43101580156109fe575060015b610a0757600080fd5b3415610a1257600080fd5b610a226060820160408301610f44565b6001600160a01b03166108fc610a3c602084013534610bed565b6040518115909202916000818181858888f19350505050158015610a64573d6000803e3d6000fd5b507f701b2f4e150abb4bd2e9902305eb7ca3a8f2e4c32799ae2c1000d644283642d48160405161024e9190611223565b604051610aa89060049083906020016112fd565b6040516020818303038152906040528051906020012060001c60005414610ace57600080fd5b610ad6610d99565b610ae660a0830160808401610f44565b6001600160a01b0316336001600160a01b031614610b0357600080fd5b610b0f8235600a610bed565b4310610b1a57600080fd5b3415610b2557600080fd5b610b33602083013534610bed565b81526040517fbd4029075bd81a55d641a01d42be16518b3120211e1c8dd2ce49cfd56055d9a090610b659084906111ce565b60405180910390a1610b75610e25565b43815281516020820152610b8f6060840160408501610f44565b6001600160a01b0316604082015260608084013590820152610bb760a0840160808501610f44565b6001600160a01b0316608082015260a0808401359082015260c080840135908201526040516105f290600590839060200161137d565b80820182811015610c195760405162461bcd60e51b8152600401610c1090611167565b60405180910390fd5b92915050565b80820382811115610c195760405162461bcd60e51b8152600401610c1090611119565b60018160200151602001511415610ccc57610c5b610e74565b4381528151516001600160a01b03908116602080840191909152835181015160408085019190915284518101519092166060840152808401515160808401529051610cab916003918491016112b2565b60408051601f19818403018152919052805160209091012060005550610d3a565b600281602001516020015114610ce757805160400151610ceb565b8051515b6001600160a01b03166108fc610d0a6002846000015160200151610d3d565b6040518115909202916000818181858888f19350505050158015610d32573d6000803e3d6000fd5b506000805533ff5b50565b6000811580610d5857505080820282828281610d5557fe5b04145b610c195760405162461bcd60e51b8152600401610c1090611141565b6040518060400160405280610d87610eb5565b8152602001610d94610ed5565b905290565b6040518060200160405280600081525090565b6040518060800160405280600081526020016000815260200160006001600160a01b03168152602001600081525090565b6040518060c00160405280600081526020016000815260200160006001600160a01b031681526020016000815260200160006001600160a01b03168152602001600081525090565b6040518060e00160405280600081526020016000815260200160006001600160a01b031681526020016000815260200160006001600160a01b0316815260200160008152602001600081525090565b6040518060a001604052806000815260200160006001600160a01b031681526020016000815260200160006001600160a01b03168152602001600081525090565b604080516060810182526000808252602082018190529181019190915290565b604051806040016040528060008152602001600081525090565b80356001600160a01b0381168114610f0657600080fd5b919050565b600060808284031215610f1c578081fd5b50919050565b600060c08284031215610f1c578081fd5b600060e08284031215610f1c578081fd5b600060208284031215610f55578081fd5b610f5e82610eef565b9392505050565b600060808284031215610f76578081fd5b610f5e8383610f0b565b600060408284031215610f1c578081fd5b600060c08284031215610fa2578081fd5b610f5e8383610f22565b600060e08284031215610fbd578081fd5b610f5e8383610f33565b60006101208284031215610f1c578081fd5b600060a08284031215610f1c578081fd5b80358252602080820135908301526001600160a01b0361100c60408301610eef565b166040830152606090810135910152565b8035825261102d60208201610eef565b6001600160a01b038181166020850152604083810135908501528061105460608501610eef565b1660608501525050608090810135910152565b803582526020810135602083015261108160408201610eef565b6001600160a01b03818116604085015260608381013590850152806110a860808501610eef565b166080850152505060a090810135910152565b80358252602081013560208301526110d560408201610eef565b6001600160a01b03818116604085015260608381013590850152806110fc60808501610eef565b166080850152505060a0818101359083015260c090810135910152565b6020808252600e908201526d1cdd58881ddc985c185c9bdd5b9960921b604082015260600190565b6020808252600c908201526b6d756c206f766572666c6f7760a01b604082015260600190565b6020808252600c908201526b616464206f766572666c6f7760a01b604082015260600190565b60808101610c198284610fea565b813581526020918201359181019190915260400190565b60c081016111c0828461101d565b60a092830135919092015290565b60e081016111dc8284611067565b60c092830135919092015290565b61012081016111f982846110bb565b60e083013560e083015261010080840135818401525092915050565b60e08101610c1982846110bb565b60c08101610c198284611067565b60a08101610c19828461101d565b91825235602082015260400190565b82815260a08101610f5e6020830184610fea565b91825280516020808401919091528101516040808401919091528101516001600160a01b03166060808401919091520151608082015260a00190565b82815260c08101610f5e602083018461101d565b91825280516020808401919091528101516001600160a01b0390811660408085019190915282015160608085019190915282015116608080840191909152015160a082015260c00190565b82815260e08101610f5e6020830184611067565b91825280516020808401919091528101516040808401919091528101516001600160a01b039081166060808501919091528201516080808501919091528201511660a080840191909152015160c082015260e00190565b8281526101008101610f5e60208301846110bb565b6000610100820190508382528251602083015260208301516040830152604083015160018060a01b038082166060850152606085015160808501528060808601511660a0850152505060a083015160c083015260c083015160e08301529392505050565b91825260208201526040019056fea26469706673582212201760afd4eb9da851d1cecd9d78f779e76d7c4371bef04eb0137eeecbcdb9e24864736f6c63430007040033`,
  deployMode: `DM_constructor`
   };

export const _Connectors = {
  ALGO: _ALGO,
  ETH: _ETH
   };

