// Automatically generated with Reach 0.1.2
/* eslint-disable no-unused-vars, no-empty-pattern, no-useless-escape, no-loop-func */
export const _version = '0.1.2';


export async function Alice(ctc, interact) {
  const stdlib = ctc.stdlib;
  const txn1 = await (ctc.sendrecv('Alice', 1, 1, [stdlib.T_UInt], [stdlib.protect(stdlib.T_UInt, interact.wager, null)], stdlib.protect(stdlib.T_UInt, interact.wager, null), [stdlib.T_UInt], true, true, false, ((txn1) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:44:8:dot', stdlib.UInt_max, 0)]);
    const [v48] = txn1.data;
    const v49 = txn1.value;
    const v47 = txn1.from;
    
    stdlib.assert(true, {
      at: './index.rsh:44:8:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Alice'
       });
    const v50 = stdlib.eq(v49, v48);
    stdlib.assert(v50, {
      at: './index.rsh:44:8:dot',
      fs: [],
      msg: 'pay amount correct',
      who: 'Alice'
       });
    const v51 = stdlib.checkedBigNumberify('./index.rsh:compileDApp', stdlib.UInt_max, 0);
    const v54 = stdlib.add(v51, v49);
    sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address]), [stdlib.checkedBigNumberify('./index.rsh:46:15:after expr stmt semicolon', stdlib.UInt_max, 1), v48, v54, v47]);
    sim_r.isHalt = false;
    
    return sim_r;
     })));
  const [v48] = txn1.data;
  const v49 = txn1.value;
  const v47 = txn1.from;
  stdlib.assert(true, {
    at: './index.rsh:44:8:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Alice'
     });
  const v50 = stdlib.eq(v49, v48);
  stdlib.assert(v50, {
    at: './index.rsh:44:8:dot',
    fs: [],
    msg: 'pay amount correct',
    who: 'Alice'
     });
  const v51 = stdlib.checkedBigNumberify('./index.rsh:compileDApp', stdlib.UInt_max, 0);
  const v54 = stdlib.add(v51, v49);
  const txn2 = await (ctc.recv('Alice', 2, 0, [], false, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10)));
  if (txn2.didTimeout) {
    const txn3 = await (ctc.sendrecv('Alice', 10, 0, [stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address], [v48, v54, v47], stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), [], true, true, false, ((txn3) => {
      const sim_r = { txns: [] };
      sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address]), [stdlib.checkedBigNumberify('reach standard library:77:6:dot', stdlib.UInt_max, 1), v48, v54, v47]);
      const [] = txn3.data;
      const v60 = txn3.value;
      const v59 = txn3.from;
      
      const v62 = stdlib.addressEq(v47, v59);
      stdlib.assert(v62, {
        at: 'reach standard library:77:6:dot',
        fs: ['at ./index.rsh:51:41:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
        msg: 'sender correct',
        who: 'Alice'
         });
      const v61 = stdlib.eq(v60, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
      stdlib.assert(v61, {
        at: 'reach standard library:77:6:dot',
        fs: ['at ./index.rsh:51:41:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
        msg: 'pay amount correct',
        who: 'Alice'
         });
      const v63 = v54;
      const v66 = stdlib.add(v63, v60);
      const v67 = v66;
      sim_r.txns.push({
        amt: v67,
        to: v47
         });
      sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([]), []);
      sim_r.isHalt = true;
      
      return sim_r;
       })));
    const [] = txn3.data;
    const v60 = txn3.value;
    const v59 = txn3.from;
    const v62 = stdlib.addressEq(v47, v59);
    stdlib.assert(v62, {
      at: 'reach standard library:77:6:dot',
      fs: ['at ./index.rsh:51:41:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
      msg: 'sender correct',
      who: 'Alice'
       });
    const v61 = stdlib.eq(v60, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
    stdlib.assert(v61, {
      at: 'reach standard library:77:6:dot',
      fs: ['at ./index.rsh:51:41:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
      msg: 'pay amount correct',
      who: 'Alice'
       });
    const v63 = v54;
    const v66 = stdlib.add(v63, v60);
    const v67 = v66;
    ;
    stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
      at: './index.rsh:40:33:application',
      fs: ['at ./index.rsh:40:39:after expr stmt semicolon call to [unknown function] (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:80:8:application call to [unknown function] (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:51:41:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
      msg: 'informTimeout',
      who: 'Alice'
       });
    return;
     }
  else {
    const [] = txn2.data;
    const v83 = txn2.value;
    const v82 = txn2.from;
    stdlib.assert(true, {
      at: './index.rsh:50:8:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Alice'
       });
    const v84 = stdlib.eq(v83, v48);
    stdlib.assert(v84, {
      at: './index.rsh:50:8:dot',
      fs: [],
      msg: 'pay amount correct',
      who: 'Alice'
       });
    const v85 = v54;
    const v88 = stdlib.add(v85, v83);
    const v89 = v88;
    let v90 = v89;
    let v91 = stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1);
    
    while ((() => {
      const v105 = stdlib.eq(v91, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
      
      return v105; })()) {
      const v109 = stdlib.protect(stdlib.T_UInt, await interact.getHand(), {
        at: './index.rsh:59:42:application',
        fs: ['at ./index.rsh:61:51:after expr stmt semicolon call to [unknown function] (defined at: ./index.rsh:58:19:function exp)'],
        msg: 'getHand',
        who: 'Alice'
         });
      const v111 = stdlib.protect(stdlib.T_UInt, await interact.random(), {
        at: 'reach standard library:69:31:application',
        fs: ['at ./index.rsh:60:52:application call to [unknown function] (defined at: reach standard library:68:8:function exp)', 'at ./index.rsh:61:51:after expr stmt semicolon call to [unknown function] (defined at: ./index.rsh:58:19:function exp)'],
        msg: 'random',
        who: 'Alice'
         });
      const v112 = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt]), [v111, v109]);
      const txn3 = await (ctc.sendrecv('Alice', 4, 1, [stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Digest], [v48, v47, v82, v90, v112], stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), [stdlib.T_Digest], true, true, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10), ((txn3) => {
        const sim_r = { txns: [] };
        sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:62:10:dot', stdlib.UInt_max, 3), v48, v47, v82, v90]);
        const [v139] = txn3.data;
        const v140 = txn3.value;
        const v138 = txn3.from;
        
        const v142 = stdlib.addressEq(v47, v138);
        stdlib.assert(v142, {
          at: './index.rsh:62:10:dot',
          fs: [],
          msg: 'sender correct',
          who: 'Alice'
           });
        const v141 = stdlib.eq(v140, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
        stdlib.assert(v141, {
          at: './index.rsh:62:10:dot',
          fs: [],
          msg: 'pay amount correct',
          who: 'Alice'
           });
        const v143 = v90;
        const v146 = stdlib.add(v143, v140);
        sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:64:17:after expr stmt semicolon', stdlib.UInt_max, 4), v48, v47, v82, v139, v146]);
        sim_r.isHalt = false;
        
        return sim_r;
         })));
      if (txn3.didTimeout) {
        const txn4 = await (ctc.recv('Alice', 9, 0, [], false, false));
        const [] = txn4.data;
        const v116 = txn4.value;
        const v115 = txn4.from;
        const v118 = stdlib.addressEq(v82, v115);
        stdlib.assert(v118, {
          at: 'reach standard library:77:6:dot',
          fs: ['at ./index.rsh:63:43:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
          msg: 'sender correct',
          who: 'Alice'
           });
        const v117 = stdlib.eq(v116, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
        stdlib.assert(v117, {
          at: 'reach standard library:77:6:dot',
          fs: ['at ./index.rsh:63:43:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
          msg: 'pay amount correct',
          who: 'Alice'
           });
        const v119 = v90;
        const v122 = stdlib.add(v119, v116);
        const v123 = v122;
        ;
        stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
          at: './index.rsh:40:33:application',
          fs: ['at ./index.rsh:40:39:after expr stmt semicolon call to [unknown function] (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:80:8:application call to [unknown function] (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:63:43:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
          msg: 'informTimeout',
          who: 'Alice'
           });
        return;
         }
      else {
        const [v139] = txn3.data;
        const v140 = txn3.value;
        const v138 = txn3.from;
        const v142 = stdlib.addressEq(v47, v138);
        stdlib.assert(v142, {
          at: './index.rsh:62:10:dot',
          fs: [],
          msg: 'sender correct',
          who: 'Alice'
           });
        const v141 = stdlib.eq(v140, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
        stdlib.assert(v141, {
          at: './index.rsh:62:10:dot',
          fs: [],
          msg: 'pay amount correct',
          who: 'Alice'
           });
        const v143 = v90;
        const v146 = stdlib.add(v143, v140);
        const txn4 = await (ctc.recv('Alice', 5, 1, [stdlib.T_UInt], false, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10)));
        if (txn4.didTimeout) {
          const txn5 = await (ctc.sendrecv('Alice', 8, 0, [stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt], [v48, v47, v82, v139, v146], stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), [], true, true, false, ((txn5) => {
            const sim_r = { txns: [] };
            sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt]), [stdlib.checkedBigNumberify('reach standard library:77:6:dot', stdlib.UInt_max, 4), v48, v47, v82, v139, v146]);
            const [] = txn5.data;
            const v152 = txn5.value;
            const v151 = txn5.from;
            
            const v154 = stdlib.addressEq(v47, v151);
            stdlib.assert(v154, {
              at: 'reach standard library:77:6:dot',
              fs: ['at ./index.rsh:70:43:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
              msg: 'sender correct',
              who: 'Alice'
               });
            const v153 = stdlib.eq(v152, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v153, {
              at: 'reach standard library:77:6:dot',
              fs: ['at ./index.rsh:70:43:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
              msg: 'pay amount correct',
              who: 'Alice'
               });
            const v155 = v146;
            const v158 = stdlib.add(v155, v152);
            const v159 = v158;
            sim_r.txns.push({
              amt: v159,
              to: v47
               });
            sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([]), []);
            sim_r.isHalt = true;
            
            return sim_r;
             })));
          const [] = txn5.data;
          const v152 = txn5.value;
          const v151 = txn5.from;
          const v154 = stdlib.addressEq(v47, v151);
          stdlib.assert(v154, {
            at: 'reach standard library:77:6:dot',
            fs: ['at ./index.rsh:70:43:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
            msg: 'sender correct',
            who: 'Alice'
             });
          const v153 = stdlib.eq(v152, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
          stdlib.assert(v153, {
            at: 'reach standard library:77:6:dot',
            fs: ['at ./index.rsh:70:43:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
            msg: 'pay amount correct',
            who: 'Alice'
             });
          const v155 = v146;
          const v158 = stdlib.add(v155, v152);
          const v159 = v158;
          ;
          stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
            at: './index.rsh:40:33:application',
            fs: ['at ./index.rsh:40:39:after expr stmt semicolon call to [unknown function] (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:80:8:application call to [unknown function] (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:70:43:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
            msg: 'informTimeout',
            who: 'Alice'
             });
          return;
           }
        else {
          const [v175] = txn4.data;
          const v176 = txn4.value;
          const v174 = txn4.from;
          const v178 = stdlib.addressEq(v82, v174);
          stdlib.assert(v178, {
            at: './index.rsh:69:10:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Alice'
             });
          const v177 = stdlib.eq(v176, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
          stdlib.assert(v177, {
            at: './index.rsh:69:10:dot',
            fs: [],
            msg: 'pay amount correct',
            who: 'Alice'
             });
          const v179 = v146;
          const v182 = stdlib.add(v179, v176);
          const txn5 = await (ctc.sendrecv('Alice', 6, 2, [stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt], [v48, v47, v82, v139, v175, v182, v111, v109], stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), [stdlib.T_UInt, stdlib.T_UInt], true, true, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10), ((txn5) => {
            const sim_r = { txns: [] };
            sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:75:10:dot', stdlib.UInt_max, 5), v48, v47, v82, v139, v175, v182]);
            const [v210, v211] = txn5.data;
            const v212 = txn5.value;
            const v209 = txn5.from;
            
            const v214 = stdlib.addressEq(v47, v209);
            stdlib.assert(v214, {
              at: './index.rsh:75:10:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Alice'
               });
            const v213 = stdlib.eq(v212, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v213, {
              at: './index.rsh:75:10:dot',
              fs: [],
              msg: 'pay amount correct',
              who: 'Alice'
               });
            const v215 = v182;
            const v218 = stdlib.add(v215, v212);
            const v220 = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt]), [v210, v211]);
            const v222 = stdlib.digestEq(v139, v220);
            stdlib.assert(v222, {
              at: 'reach standard library:74:17:application',
              fs: ['at ./index.rsh:77:24:application call to [unknown function] (defined at: reach standard library:73:8:function exp)'],
              msg: null,
              who: 'Alice'
               });
            const v227 = stdlib.sub(stdlib.checkedBigNumberify('./index.rsh:7:18:decimal', stdlib.UInt_max, 4), v175);
            const v230 = stdlib.add(v211, v227);
            const v231 = stdlib.mod(v230, stdlib.checkedBigNumberify('./index.rsh:7:32:decimal', stdlib.UInt_max, 3));
            const v233 = v218;
            const cv90 = v233;
            const cv91 = v231;
            
            (() => {
              const v90 = cv90;
              const v91 = cv91;
              
              if ((() => {
                const v105 = stdlib.eq(v91, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
                
                return v105; })()) {
                sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:56:17:after expr stmt semicolon', stdlib.UInt_max, 3), v48, v47, v82, v90]);
                sim_r.isHalt = false;
                 }
              else {
                const v235 = stdlib.eq(v91, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
                const v241 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:83:16:decimal', stdlib.UInt_max, 2), v48);
                const v246 = v235 ? v47 : v82;
                sim_r.txns.push({
                  amt: v241,
                  to: v246
                   });
                sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([]), []);
                sim_r.isHalt = true;
                 } })();
            return sim_r;
             })));
          if (txn5.didTimeout) {
            const txn6 = await (ctc.recv('Alice', 7, 0, [], false, false));
            const [] = txn6.data;
            const v187 = txn6.value;
            const v186 = txn6.from;
            const v189 = stdlib.addressEq(v82, v186);
            stdlib.assert(v189, {
              at: 'reach standard library:77:6:dot',
              fs: ['at ./index.rsh:76:43:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
              msg: 'sender correct',
              who: 'Alice'
               });
            const v188 = stdlib.eq(v187, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v188, {
              at: 'reach standard library:77:6:dot',
              fs: ['at ./index.rsh:76:43:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
              msg: 'pay amount correct',
              who: 'Alice'
               });
            const v190 = v182;
            const v193 = stdlib.add(v190, v187);
            const v194 = v193;
            ;
            stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
              at: './index.rsh:40:33:application',
              fs: ['at ./index.rsh:40:39:after expr stmt semicolon call to [unknown function] (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:80:8:application call to [unknown function] (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:76:43:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
              msg: 'informTimeout',
              who: 'Alice'
               });
            return;
             }
          else {
            const [v210, v211] = txn5.data;
            const v212 = txn5.value;
            const v209 = txn5.from;
            const v214 = stdlib.addressEq(v47, v209);
            stdlib.assert(v214, {
              at: './index.rsh:75:10:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Alice'
               });
            const v213 = stdlib.eq(v212, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v213, {
              at: './index.rsh:75:10:dot',
              fs: [],
              msg: 'pay amount correct',
              who: 'Alice'
               });
            const v215 = v182;
            const v218 = stdlib.add(v215, v212);
            const v220 = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt]), [v210, v211]);
            const v222 = stdlib.digestEq(v139, v220);
            stdlib.assert(v222, {
              at: 'reach standard library:74:17:application',
              fs: ['at ./index.rsh:77:24:application call to [unknown function] (defined at: reach standard library:73:8:function exp)'],
              msg: null,
              who: 'Alice'
               });
            const v227 = stdlib.sub(stdlib.checkedBigNumberify('./index.rsh:7:18:decimal', stdlib.UInt_max, 4), v175);
            const v230 = stdlib.add(v211, v227);
            const v231 = stdlib.mod(v230, stdlib.checkedBigNumberify('./index.rsh:7:32:decimal', stdlib.UInt_max, 3));
            const v233 = v218;
            const cv90 = v233;
            const cv91 = v231;
            
            v90 = cv90;
            v91 = cv91;
            
            continue; }
           }
         }
       }
    const v235 = stdlib.eq(v91, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
    const v241 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:83:16:decimal', stdlib.UInt_max, 2), v48);
    const v246 = v235 ? v47 : v82;
    ;
    stdlib.protect(stdlib.T_Null, await interact.seeOutcome(v91), {
      at: './index.rsh:87:28:application',
      fs: ['at ./index.rsh:87:41:after expr stmt semicolon call to [unknown function] (defined at: ./index.rsh:86:23:function exp)'],
      msg: 'seeOutcome',
      who: 'Alice'
       });
    return; }
  
   }
export async function Bob(ctc, interact) {
  const stdlib = ctc.stdlib;
  const txn1 = await (ctc.recv('Bob', 1, 1, [stdlib.T_UInt], false, false));
  const [v48] = txn1.data;
  const v49 = txn1.value;
  const v47 = txn1.from;
  stdlib.assert(true, {
    at: './index.rsh:44:8:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Bob'
     });
  const v50 = stdlib.eq(v49, v48);
  stdlib.assert(v50, {
    at: './index.rsh:44:8:dot',
    fs: [],
    msg: 'pay amount correct',
    who: 'Bob'
     });
  const v51 = stdlib.checkedBigNumberify('./index.rsh:compileDApp', stdlib.UInt_max, 0);
  const v54 = stdlib.add(v51, v49);
  stdlib.protect(stdlib.T_Null, await interact.acceptWager(v48), {
    at: './index.rsh:49:29:application',
    fs: ['at ./index.rsh:49:40:after expr stmt semicolon call to [unknown function] (defined at: ./index.rsh:48:17:function exp)'],
    msg: 'acceptWager',
    who: 'Bob'
     });
  const txn2 = await (ctc.sendrecv('Bob', 2, 0, [stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address], [v48, v54, v47], v48, [], true, true, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10), ((txn2) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address]), [stdlib.checkedBigNumberify('./index.rsh:50:8:dot', stdlib.UInt_max, 1), v48, v54, v47]);
    const [] = txn2.data;
    const v83 = txn2.value;
    const v82 = txn2.from;
    
    stdlib.assert(true, {
      at: './index.rsh:50:8:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Bob'
       });
    const v84 = stdlib.eq(v83, v48);
    stdlib.assert(v84, {
      at: './index.rsh:50:8:dot',
      fs: [],
      msg: 'pay amount correct',
      who: 'Bob'
       });
    const v85 = v54;
    const v88 = stdlib.add(v85, v83);
    const v89 = v88;
    const v90 = v89;
    const v91 = stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1);
    
    if ((() => {
      const v105 = stdlib.eq(v91, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
      
      return v105; })()) {
      sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:56:17:after expr stmt semicolon', stdlib.UInt_max, 3), v48, v47, v82, v90]);
      sim_r.isHalt = false;
       }
    else {
      const v235 = stdlib.eq(v91, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
      const v241 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:83:16:decimal', stdlib.UInt_max, 2), v48);
      const v246 = v235 ? v47 : v82;
      sim_r.txns.push({
        amt: v241,
        to: v246
         });
      sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([]), []);
      sim_r.isHalt = true;
       }
    return sim_r;
     })));
  if (txn2.didTimeout) {
    const txn3 = await (ctc.recv('Bob', 10, 0, [], false, false));
    const [] = txn3.data;
    const v60 = txn3.value;
    const v59 = txn3.from;
    const v62 = stdlib.addressEq(v47, v59);
    stdlib.assert(v62, {
      at: 'reach standard library:77:6:dot',
      fs: ['at ./index.rsh:51:41:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
      msg: 'sender correct',
      who: 'Bob'
       });
    const v61 = stdlib.eq(v60, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
    stdlib.assert(v61, {
      at: 'reach standard library:77:6:dot',
      fs: ['at ./index.rsh:51:41:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
      msg: 'pay amount correct',
      who: 'Bob'
       });
    const v63 = v54;
    const v66 = stdlib.add(v63, v60);
    const v67 = v66;
    ;
    stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
      at: './index.rsh:40:33:application',
      fs: ['at ./index.rsh:40:39:after expr stmt semicolon call to [unknown function] (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:80:8:application call to [unknown function] (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:51:41:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
      msg: 'informTimeout',
      who: 'Bob'
       });
    return;
     }
  else {
    const [] = txn2.data;
    const v83 = txn2.value;
    const v82 = txn2.from;
    stdlib.assert(true, {
      at: './index.rsh:50:8:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Bob'
       });
    const v84 = stdlib.eq(v83, v48);
    stdlib.assert(v84, {
      at: './index.rsh:50:8:dot',
      fs: [],
      msg: 'pay amount correct',
      who: 'Bob'
       });
    const v85 = v54;
    const v88 = stdlib.add(v85, v83);
    const v89 = v88;
    let v90 = v89;
    let v91 = stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1);
    
    while ((() => {
      const v105 = stdlib.eq(v91, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
      
      return v105; })()) {
      const txn3 = await (ctc.recv('Bob', 4, 1, [stdlib.T_Digest], false, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10)));
      if (txn3.didTimeout) {
        const txn4 = await (ctc.sendrecv('Bob', 9, 0, [stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_UInt], [v48, v47, v82, v90], stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), [], true, true, false, ((txn4) => {
          const sim_r = { txns: [] };
          sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_UInt]), [stdlib.checkedBigNumberify('reach standard library:77:6:dot', stdlib.UInt_max, 3), v48, v47, v82, v90]);
          const [] = txn4.data;
          const v116 = txn4.value;
          const v115 = txn4.from;
          
          const v118 = stdlib.addressEq(v82, v115);
          stdlib.assert(v118, {
            at: 'reach standard library:77:6:dot',
            fs: ['at ./index.rsh:63:43:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
            msg: 'sender correct',
            who: 'Bob'
             });
          const v117 = stdlib.eq(v116, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
          stdlib.assert(v117, {
            at: 'reach standard library:77:6:dot',
            fs: ['at ./index.rsh:63:43:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
            msg: 'pay amount correct',
            who: 'Bob'
             });
          const v119 = v90;
          const v122 = stdlib.add(v119, v116);
          const v123 = v122;
          sim_r.txns.push({
            amt: v123,
            to: v82
             });
          sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([]), []);
          sim_r.isHalt = true;
          
          return sim_r;
           })));
        const [] = txn4.data;
        const v116 = txn4.value;
        const v115 = txn4.from;
        const v118 = stdlib.addressEq(v82, v115);
        stdlib.assert(v118, {
          at: 'reach standard library:77:6:dot',
          fs: ['at ./index.rsh:63:43:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
          msg: 'sender correct',
          who: 'Bob'
           });
        const v117 = stdlib.eq(v116, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
        stdlib.assert(v117, {
          at: 'reach standard library:77:6:dot',
          fs: ['at ./index.rsh:63:43:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
          msg: 'pay amount correct',
          who: 'Bob'
           });
        const v119 = v90;
        const v122 = stdlib.add(v119, v116);
        const v123 = v122;
        ;
        stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
          at: './index.rsh:40:33:application',
          fs: ['at ./index.rsh:40:39:after expr stmt semicolon call to [unknown function] (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:80:8:application call to [unknown function] (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:63:43:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
          msg: 'informTimeout',
          who: 'Bob'
           });
        return;
         }
      else {
        const [v139] = txn3.data;
        const v140 = txn3.value;
        const v138 = txn3.from;
        const v142 = stdlib.addressEq(v47, v138);
        stdlib.assert(v142, {
          at: './index.rsh:62:10:dot',
          fs: [],
          msg: 'sender correct',
          who: 'Bob'
           });
        const v141 = stdlib.eq(v140, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
        stdlib.assert(v141, {
          at: './index.rsh:62:10:dot',
          fs: [],
          msg: 'pay amount correct',
          who: 'Bob'
           });
        const v143 = v90;
        const v146 = stdlib.add(v143, v140);
        const v149 = stdlib.protect(stdlib.T_UInt, await interact.getHand(), {
          at: './index.rsh:68:52:application',
          fs: ['at ./index.rsh:68:59:after expr stmt semicolon call to [unknown function] (defined at: ./index.rsh:67:19:function exp)'],
          msg: 'getHand',
          who: 'Bob'
           });
        const txn4 = await (ctc.sendrecv('Bob', 5, 1, [stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt], [v48, v47, v82, v139, v146, v149], stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), [stdlib.T_UInt], true, true, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10), ((txn4) => {
          const sim_r = { txns: [] };
          sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:69:10:dot', stdlib.UInt_max, 4), v48, v47, v82, v139, v146]);
          const [v175] = txn4.data;
          const v176 = txn4.value;
          const v174 = txn4.from;
          
          const v178 = stdlib.addressEq(v82, v174);
          stdlib.assert(v178, {
            at: './index.rsh:69:10:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Bob'
             });
          const v177 = stdlib.eq(v176, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
          stdlib.assert(v177, {
            at: './index.rsh:69:10:dot',
            fs: [],
            msg: 'pay amount correct',
            who: 'Bob'
             });
          const v179 = v146;
          const v182 = stdlib.add(v179, v176);
          sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:71:17:after expr stmt semicolon', stdlib.UInt_max, 5), v48, v47, v82, v139, v175, v182]);
          sim_r.isHalt = false;
          
          return sim_r;
           })));
        if (txn4.didTimeout) {
          const txn5 = await (ctc.recv('Bob', 8, 0, [], false, false));
          const [] = txn5.data;
          const v152 = txn5.value;
          const v151 = txn5.from;
          const v154 = stdlib.addressEq(v47, v151);
          stdlib.assert(v154, {
            at: 'reach standard library:77:6:dot',
            fs: ['at ./index.rsh:70:43:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
            msg: 'sender correct',
            who: 'Bob'
             });
          const v153 = stdlib.eq(v152, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
          stdlib.assert(v153, {
            at: 'reach standard library:77:6:dot',
            fs: ['at ./index.rsh:70:43:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
            msg: 'pay amount correct',
            who: 'Bob'
             });
          const v155 = v146;
          const v158 = stdlib.add(v155, v152);
          const v159 = v158;
          ;
          stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
            at: './index.rsh:40:33:application',
            fs: ['at ./index.rsh:40:39:after expr stmt semicolon call to [unknown function] (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:80:8:application call to [unknown function] (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:70:43:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
            msg: 'informTimeout',
            who: 'Bob'
             });
          return;
           }
        else {
          const [v175] = txn4.data;
          const v176 = txn4.value;
          const v174 = txn4.from;
          const v178 = stdlib.addressEq(v82, v174);
          stdlib.assert(v178, {
            at: './index.rsh:69:10:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Bob'
             });
          const v177 = stdlib.eq(v176, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
          stdlib.assert(v177, {
            at: './index.rsh:69:10:dot',
            fs: [],
            msg: 'pay amount correct',
            who: 'Bob'
             });
          const v179 = v146;
          const v182 = stdlib.add(v179, v176);
          const txn5 = await (ctc.recv('Bob', 6, 2, [stdlib.T_UInt, stdlib.T_UInt], false, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10)));
          if (txn5.didTimeout) {
            const txn6 = await (ctc.sendrecv('Bob', 7, 0, [stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt], [v48, v47, v82, v139, v175, v182], stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), [], true, true, false, ((txn6) => {
              const sim_r = { txns: [] };
              sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('reach standard library:77:6:dot', stdlib.UInt_max, 5), v48, v47, v82, v139, v175, v182]);
              const [] = txn6.data;
              const v187 = txn6.value;
              const v186 = txn6.from;
              
              const v189 = stdlib.addressEq(v82, v186);
              stdlib.assert(v189, {
                at: 'reach standard library:77:6:dot',
                fs: ['at ./index.rsh:76:43:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
                msg: 'sender correct',
                who: 'Bob'
                 });
              const v188 = stdlib.eq(v187, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
              stdlib.assert(v188, {
                at: 'reach standard library:77:6:dot',
                fs: ['at ./index.rsh:76:43:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
                msg: 'pay amount correct',
                who: 'Bob'
                 });
              const v190 = v182;
              const v193 = stdlib.add(v190, v187);
              const v194 = v193;
              sim_r.txns.push({
                amt: v194,
                to: v82
                 });
              sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([]), []);
              sim_r.isHalt = true;
              
              return sim_r;
               })));
            const [] = txn6.data;
            const v187 = txn6.value;
            const v186 = txn6.from;
            const v189 = stdlib.addressEq(v82, v186);
            stdlib.assert(v189, {
              at: 'reach standard library:77:6:dot',
              fs: ['at ./index.rsh:76:43:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
              msg: 'sender correct',
              who: 'Bob'
               });
            const v188 = stdlib.eq(v187, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v188, {
              at: 'reach standard library:77:6:dot',
              fs: ['at ./index.rsh:76:43:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
              msg: 'pay amount correct',
              who: 'Bob'
               });
            const v190 = v182;
            const v193 = stdlib.add(v190, v187);
            const v194 = v193;
            ;
            stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
              at: './index.rsh:40:33:application',
              fs: ['at ./index.rsh:40:39:after expr stmt semicolon call to [unknown function] (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:80:8:application call to [unknown function] (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:76:43:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
              msg: 'informTimeout',
              who: 'Bob'
               });
            return;
             }
          else {
            const [v210, v211] = txn5.data;
            const v212 = txn5.value;
            const v209 = txn5.from;
            const v214 = stdlib.addressEq(v47, v209);
            stdlib.assert(v214, {
              at: './index.rsh:75:10:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Bob'
               });
            const v213 = stdlib.eq(v212, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v213, {
              at: './index.rsh:75:10:dot',
              fs: [],
              msg: 'pay amount correct',
              who: 'Bob'
               });
            const v215 = v182;
            const v218 = stdlib.add(v215, v212);
            const v220 = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt]), [v210, v211]);
            const v222 = stdlib.digestEq(v139, v220);
            stdlib.assert(v222, {
              at: 'reach standard library:74:17:application',
              fs: ['at ./index.rsh:77:24:application call to [unknown function] (defined at: reach standard library:73:8:function exp)'],
              msg: null,
              who: 'Bob'
               });
            const v227 = stdlib.sub(stdlib.checkedBigNumberify('./index.rsh:7:18:decimal', stdlib.UInt_max, 4), v175);
            const v230 = stdlib.add(v211, v227);
            const v231 = stdlib.mod(v230, stdlib.checkedBigNumberify('./index.rsh:7:32:decimal', stdlib.UInt_max, 3));
            const v233 = v218;
            const cv90 = v233;
            const cv91 = v231;
            
            v90 = cv90;
            v91 = cv91;
            
            continue; }
           }
         }
       }
    const v235 = stdlib.eq(v91, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
    const v241 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:83:16:decimal', stdlib.UInt_max, 2), v48);
    const v246 = v235 ? v47 : v82;
    ;
    stdlib.protect(stdlib.T_Null, await interact.seeOutcome(v91), {
      at: './index.rsh:87:28:application',
      fs: ['at ./index.rsh:87:41:after expr stmt semicolon call to [unknown function] (defined at: ./index.rsh:86:23:function exp)'],
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
// Just "sender correct"
// "./index.rsh:44:8:dot"
// "[]"
int 1
assert
// Just "pay amount correct"
// "./index.rsh:44:8:dot"
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
arg 5
concat
load 255
itob
concat
gtxn 3 Sender
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
// Just "sender correct"
// "./index.rsh:50:8:dot"
// "[]"
int 1
assert
// Just "pay amount correct"
// "./index.rsh:50:8:dot"
// "[]"
gtxn 3 Amount
arg 3
btoi
-
arg 5
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
arg 5
concat
arg 7
concat
gtxn 3 Sender
concat
arg 6
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
arg 7
gtxn 3 Sender
ite
==
assert
gtxn 4 Amount
int 2
arg 5
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
// Just "sender correct"
// "./index.rsh:62:10:dot"
// "[]"
arg 6
gtxn 3 Sender
==
assert
// Just "pay amount correct"
// "./index.rsh:62:10:dot"
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
// Just "sender correct"
// "./index.rsh:69:10:dot"
// "[]"
arg 7
gtxn 3 Sender
==
assert
// Just "pay amount correct"
// "./index.rsh:69:10:dot"
// "[]"
gtxn 3 Amount
arg 3
btoi
-
int 0
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
// Just "sender correct"
// "./index.rsh:75:10:dot"
// "[]"
arg 6
gtxn 3 Sender
==
assert
// Just "pay amount correct"
// "./index.rsh:75:10:dot"
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
// "[at ./index.rsh:77:24:application call to [unknown function] (defined at: reach standard library:73:8:function exp)]"
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
store 255
int 1
==
bz l0
int 3
itob
arg 5
concat
arg 6
concat
arg 7
concat
arg 10
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
arg 7
ite
==
assert
gtxn 4 Amount
int 2
arg 5
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
// Just "sender correct"
// "reach standard library:77:6:dot"
// "[at ./index.rsh:76:43:application call to [unknown function] (defined at: reach standard library:76:8:function exp)]"
arg 7
gtxn 3 Sender
==
assert
// Just "pay amount correct"
// "reach standard library:77:6:dot"
// "[at ./index.rsh:76:43:application call to [unknown function] (defined at: reach standard library:76:8:function exp)]"
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
// Just "sender correct"
// "reach standard library:77:6:dot"
// "[at ./index.rsh:70:43:application call to [unknown function] (defined at: reach standard library:76:8:function exp)]"
arg 6
gtxn 3 Sender
==
assert
// Just "pay amount correct"
// "reach standard library:77:6:dot"
// "[at ./index.rsh:70:43:application call to [unknown function] (defined at: reach standard library:76:8:function exp)]"
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
// Just "sender correct"
// "reach standard library:77:6:dot"
// "[at ./index.rsh:63:43:application call to [unknown function] (defined at: reach standard library:76:8:function exp)]"
arg 7
gtxn 3 Sender
==
assert
// Just "pay amount correct"
// "reach standard library:77:6:dot"
// "[at ./index.rsh:63:43:application call to [unknown function] (defined at: reach standard library:76:8:function exp)]"
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
// Just "sender correct"
// "reach standard library:77:6:dot"
// "[at ./index.rsh:51:41:application call to [unknown function] (defined at: reach standard library:76:8:function exp)]"
arg 7
gtxn 3 Sender
==
assert
// Just "pay amount correct"
// "reach standard library:77:6:dot"
// "[at ./index.rsh:51:41:application call to [unknown function] (defined at: reach standard library:76:8:function exp)]"
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
arg 6
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
                "name": "v48",
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
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v54",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v47",
                "type": "address"
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
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v54",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v47",
                "type": "address"
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
                "internalType": "uint256",
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v47",
                "type": "address"
              },
              {
                "internalType": "address payable",
                "name": "v82",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v90",
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
                "name": "v139",
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
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v47",
                "type": "address"
              },
              {
                "internalType": "address payable",
                "name": "v82",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v139",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v146",
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
                "name": "v175",
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
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v47",
                "type": "address"
              },
              {
                "internalType": "address payable",
                "name": "v82",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v139",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v175",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v182",
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
                "name": "v210",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v211",
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
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v47",
                "type": "address"
              },
              {
                "internalType": "address payable",
                "name": "v82",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v139",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v175",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v182",
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
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v47",
                "type": "address"
              },
              {
                "internalType": "address payable",
                "name": "v82",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v139",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v146",
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
                "internalType": "uint256",
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v47",
                "type": "address"
              },
              {
                "internalType": "address payable",
                "name": "v82",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v90",
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
                "name": "v48",
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
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v54",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v47",
                "type": "address"
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
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v54",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v47",
                "type": "address"
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
                "internalType": "uint256",
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v47",
                "type": "address"
              },
              {
                "internalType": "address payable",
                "name": "v82",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v90",
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
                "name": "v139",
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
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v47",
                "type": "address"
              },
              {
                "internalType": "address payable",
                "name": "v82",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v139",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v146",
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
                "name": "v175",
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
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v47",
                "type": "address"
              },
              {
                "internalType": "address payable",
                "name": "v82",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v139",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v175",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v182",
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
                "name": "v210",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v211",
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
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v47",
                "type": "address"
              },
              {
                "internalType": "address payable",
                "name": "v82",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v139",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v175",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v182",
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
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v47",
                "type": "address"
              },
              {
                "internalType": "address payable",
                "name": "v82",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v139",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v146",
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
                "internalType": "uint256",
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v47",
                "type": "address"
              },
              {
                "internalType": "address payable",
                "name": "v82",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v90",
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
  Bytecode: `0x608060408190527f49ff028a829527a47ec6839c7147b484eccf5a2a94853eddac09cef44d9d4e9e90600090a161003461006c565b43815260405161004b90600090839060200161007f565b60408051601f1981840301815291905280516020909101206000555061008e565b6040518060200160405280600081525090565b91825251602082015260400190565b6113e18061009d6000396000f3fe6080604052600436106100865760003560e01c8063905436cf11610059578063905436cf146100d95780639532ef01146100ec578063b43649a9146100ff578063b984daee14610112578063f64375f01461012557610086565b80631baffe451461008b5780633b8bfd27146100a05780636e132f37146100b3578063860aa3a2146100c6575b600080fd5b61009e610099366004610f8f565b610138565b005b61009e6100ae366004610f47565b610254565b61009e6100c1366004610f62565b6103ba565b61009e6100d4366004610f62565b6104c7565b61009e6100e7366004610f1b565b610617565b61009e6100fa366004610f36565b610718565b61009e61010d366004610f1b565b6107e5565b61009e610120366004610f7d565b6108f2565b61009e610133366004610f47565b610a94565b60405161014c90600390839060200161125a565b6040516020818303038152906040528051906020012060001c6000541461017257600080fd5b61017e8135600a610ba1565b431015801561018b575060015b61019457600080fd5b336101a56080830160608401610efa565b6001600160a01b0316146101b857600080fd5b34156101c357600080fd5b6101d36080820160608301610efa565b6001600160a01b03166108fc6101ed608084013534610ba1565b6040518115909202916000818181858888f19350505050158015610215573d6000803e3d6000fd5b507f5248857ac1e50f3eec315691739ae3f39481296b8c8bb14d387eb910144b8d888160405161024591906111ed565b60405180910390a16000805533ff5b60405161026890600390839060200161125a565b6040516020818303038152906040528051906020012060001c6000541461028e57600080fd5b610296610d2a565b6102a28235600a610ba1565b43106102ad57600080fd5b336102be6060840160408501610efa565b6001600160a01b0316146102d157600080fd5b34156102dc57600080fd5b6102ea608083013534610ba1565b81526040517f64cd544bbf9c0f35b6525f1a29ad79ad82f8f3cf565646ace5148ee3a58bd9a29061031c90849061116e565b60405180910390a161032c610d3d565b438152602080840135908201526103496060840160408501610efa565b6001600160a01b031660408201526103676080840160608501610efa565b6001600160a01b0316606082015260a080840135608083015282519082015260405161039a9060049083906020016112cd565b60408051601f198184030181529190528051602090910120600055505050565b6040516103ce906005908390602001611324565b6040516020818303038152906040528051906020012060001c600054146103f457600080fd5b6104008135600a610ba1565b431015801561040d575060015b61041657600080fd5b336104276080830160608401610efa565b6001600160a01b03161461043a57600080fd5b341561044557600080fd5b6104556080820160608301610efa565b6001600160a01b03166108fc61046f60c084013534610ba1565b6040518115909202916000818181858888f19350505050158015610497573d6000803e3d6000fd5b507fac41b2957e4375f480396ae1fc3fe6b2fde1e46b79d88f9231f97b71af8a5ee98160405161024591906111d1565b6040516104db9060049083906020016112b9565b6040516020818303038152906040528051906020012060001c6000541461050157600080fd5b610509610d2a565b6105158235600a610ba1565b431061052057600080fd5b336105316080840160608501610efa565b6001600160a01b03161461054457600080fd5b341561054f57600080fd5b61055d60a083013534610ba1565b81526040517f0cf662498b758fb6b54ab3575eb782e773b5e657965eab74f0b3887a73b26cce9061058f90849061118a565b60405180910390a161059f610d85565b438152602080840135908201526105bc6060840160408501610efa565b6001600160a01b031660408201526105da6080840160608501610efa565b6001600160a01b031660608201526080808401359082015260c08084013560a083015282519082015260405161039a906005908390602001611339565b60405161062b90600190839060200161120a565b6040516020818303038152906040528051906020012060001c6000541461065157600080fd5b61065d8135600a610ba1565b431061066857600080fd5b3460208201351461067857600080fd5b7f5074eecd0995b3a3f596cc2835f720a588e59c1848a49558a0743c6f9bb251ba816040516106a79190611149565b60405180910390a16106b7610dd4565b8051602083013590526106d06080830160608401610efa565b81516001600160a01b039091166020909101528051336040918201526106f99083013534610ba1565b60208083018051929092529051600191015261071481610bd3565b5050565b60405161072c9060009083906020016111fb565b6040516020818303038152906040528051906020012060001c6000541461075257600080fd5b61075a610d2a565b3460208301351461076a57600080fd5b610775600034610ba1565b81526040517ff2c62eba998811305a23599b2e6d212befbd7ded3a73f4c08bfb9aefe08dc166906107a7908490611157565b60405180910390a16107b7610df9565b4381526020808401358183015282516040808401919091523360608401525161039a9160019184910161121e565b6040516107f990600190839060200161120a565b6040516020818303038152906040528051906020012060001c6000541461081f57600080fd5b61082b8135600a610ba1565b4310158015610838575060015b61084157600080fd5b336108526080830160608401610efa565b6001600160a01b03161461086557600080fd5b341561087057600080fd5b6108806080820160608301610efa565b6001600160a01b03166108fc61089a604084013534610ba1565b6040518115909202916000818181858888f193505050501580156108c2573d6000803e3d6000fd5b507f5891766ff36501b641f062681fd9d274fb08f40cbfbf3fba5e3c907f9b6f320a816040516102459190611149565b604051610906906005908390602001611324565b6040516020818303038152906040528051906020012060001c6000541461092c57600080fd5b6109388135600a610ba1565b431061094357600080fd5b336109546060830160408401610efa565b6001600160a01b03161461096757600080fd5b341561097257600080fd5b60405161098e9060e0830135906101008401359060200161139d565b60408051601f1981840301815291905280516020909101206080820135146109b557600080fd5b7f0805283f75f9efae35b94a7b3c3f5ffbc0c271c41627f7ea11a17fcac2bd8ebe816040516109e491906111a6565b60405180910390a16109f4610dd4565b805160208301359052610a0d6060830160408401610efa565b81516001600160a01b03909116602090910152610a306080830160608401610efa565b81516001600160a01b03909116604090910152610a5160c083013534610ba1565b6020820151526003610a76610100840135610a71600460a0870135610cd0565b610ba1565b81610a7d57fe5b068160200151602001818152505061071481610bd3565b604051610aa89060049083906020016112b9565b6040516020818303038152906040528051906020012060001c60005414610ace57600080fd5b610ada8135600a610ba1565b4310158015610ae7575060015b610af057600080fd5b33610b016060830160408401610efa565b6001600160a01b031614610b1457600080fd5b3415610b1f57600080fd5b610b2f6060820160408301610efa565b6001600160a01b03166108fc610b4960a084013534610ba1565b6040518115909202916000818181858888f19350505050158015610b71573d6000803e3d6000fd5b507f4c4b22217c1d658a4f0d67fe6839de0cea1f266db1ceb1b771d1a8167ff32fb58160405161024591906111df565b80820182811015610bcd5760405162461bcd60e51b8152600401610bc490611123565b60405180910390fd5b92915050565b60018160200151602001511415610c5c57610bec610e2a565b43815281515160208083019190915282518101516001600160a01b03908116604080850191909152845181015190911660608401528184015151608084015251610c3b9160039184910161126e565b60408051601f19818403018152919052805160209091012060005550610ccd565b600281602001516020015114610c7757805160400151610c7e565b8051602001515b6001600160a01b03166108fc610c9d6002846000015160000151610cf3565b6040518115909202916000818181858888f19350505050158015610cc5573d6000803e3d6000fd5b506000805533ff5b50565b80820382811115610bcd5760405162461bcd60e51b8152600401610bc4906110d5565b6000811580610d0e57505080820282828281610d0b57fe5b04145b610bcd5760405162461bcd60e51b8152600401610bc4906110fd565b6040518060200160405280600081525090565b6040518060c00160405280600081526020016000815260200160006001600160a01b0316815260200160006001600160a01b0316815260200160008152602001600081525090565b6040518060e00160405280600081526020016000815260200160006001600160a01b0316815260200160006001600160a01b031681526020016000815260200160008152602001600081525090565b6040518060400160405280610de7610e6b565b8152602001610df4610e8b565b905290565b604051806080016040528060008152602001600081526020016000815260200160006001600160a01b031681525090565b6040518060a00160405280600081526020016000815260200160006001600160a01b0316815260200160006001600160a01b03168152602001600081525090565b604080516060810182526000808252602082018190529181019190915290565b604051806040016040528060008152602001600081525090565b80356001600160a01b0381168114610ebc57600080fd5b919050565b600060808284031215610ed2578081fd5b50919050565b600060c08284031215610ed2578081fd5b600060e08284031215610ed2578081fd5b600060208284031215610f0b578081fd5b610f1482610ea5565b9392505050565b600060808284031215610f2c578081fd5b610f148383610ec1565b600060408284031215610ed2578081fd5b600060c08284031215610f58578081fd5b610f148383610ed8565b600060e08284031215610f73578081fd5b610f148383610ee9565b60006101208284031215610ed2578081fd5b600060a08284031215610ed2578081fd5b8035825260208082013590830152604080820135908301526001600160a01b03610fcc60608301610ea5565b1660608301525050565b8035825260208101356020830152610ff060408201610ea5565b6001600160a01b0381811660408501528061100d60608501610ea5565b1660608501525050608090810135910152565b803582526020810135602083015261103a60408201610ea5565b6001600160a01b0381811660408501528061105760608501610ea5565b16606085015250506080818101359083015260a090810135910152565b803582526020810135602083015261108e60408201610ea5565b6001600160a01b038181166040850152806110ab60608501610ea5565b16606085015250506080810135608083015260a081013560a083015260c081013560c08301525050565b6020808252600e908201526d1cdd58881ddc985c185c9bdd5b9960921b604082015260600190565b6020808252600c908201526b6d756c206f766572666c6f7760a01b604082015260600190565b6020808252600c908201526b616464206f766572666c6f7760a01b604082015260600190565b60808101610bcd8284610fa0565b813581526020918201359181019190915260400190565b60c0810161117c8284610fd6565b60a092830135919092015290565b60e081016111988284611020565b60c092830135919092015290565b61012081016111b58284611074565b60e083013560e083015261010080840135818401525092915050565b60e08101610bcd8284611074565b60c08101610bcd8284611020565b60a08101610bcd8284610fd6565b91825235602082015260400190565b82815260a08101610f146020830184610fa0565b918252805160208084019190915281015160408084019190915281015160608084019190915201516001600160a01b0316608082015260a00190565b82815260c08101610f146020830184610fd6565b91825280516020808401919091528101516040808401919091528101516001600160a01b0390811660608085019190915282015116608080840191909152015160a082015260c00190565b82815260e08101610f146020830184611020565b91825280516020808401919091528101516040808401919091528101516001600160a01b039081166060808501919091528201511660808084019190915281015160a080840191909152015160c082015260e00190565b8281526101008101610f146020830184611074565b6000610100820190508382528251602083015260208301516040830152604083015160018060a01b0380821660608501528060608601511660808501525050608083015160a083015260a083015160c083015260c083015160e08301529392505050565b91825260208201526040019056fea2646970667358221220a1cb277a42b58c2a8d57b57edbe475e108e9a366bca8d43fede170e13fa4509864736f6c63430007050033`,
  deployMode: `DM_constructor`
   };

export const _Connectors = {
  ALGO: _ALGO,
  ETH: _ETH
   };

