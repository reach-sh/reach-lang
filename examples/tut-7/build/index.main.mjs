// Automatically generated with Reach 0.1.2
/* eslint-disable no-unused-vars, no-empty-pattern, no-useless-escape, no-loop-func */
export const _version = '0.1.2';


export async function Alice(ctc, interact) {
  const stdlib = ctc.stdlib;
  const v45 = await ctc.creationTime();
  const txn1 = await (ctc.sendrecv('Alice', 1, 1, stdlib.checkedBigNumberify('./index.rsh:44:9:dot', stdlib.UInt_max, 0), [stdlib.T_UInt, stdlib.T_UInt], [v45, stdlib.protect(stdlib.T_UInt, interact.wager, null)], stdlib.protect(stdlib.T_UInt, interact.wager, null), [stdlib.T_UInt], true, true, false, ((txn1) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:44:9:dot', stdlib.UInt_max, 0), v45]);
    sim_r.prevSt_noPrevTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:44:9:dot', stdlib.UInt_max, 0)]);
    const [v49] = txn1.data;
    const v50 = txn1.value;
    const v56 = txn1.time;
    const v48 = txn1.from;
    
    stdlib.assert(true, {
      at: './index.rsh:44:9:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Alice'
       });
    const v51 = stdlib.eq(v50, v49);
    stdlib.assert(v51, {
      at: './index.rsh:44:9:dot',
      fs: [],
      msg: 'pay amount correct',
      who: 'Alice'
       });
    const v52 = stdlib.checkedBigNumberify('./index.rsh:compileDApp', stdlib.UInt_max, 0);
    const v55 = stdlib.add(v52, v50);
    sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address]), [stdlib.checkedBigNumberify('./index.rsh:46:15:after expr stmt semicolon', stdlib.UInt_max, 1), v56, v49, v55, v48]);
    sim_r.nextSt_noTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address]), [stdlib.checkedBigNumberify('./index.rsh:46:15:after expr stmt semicolon', stdlib.UInt_max, 1), v49, v55, v48]);
    sim_r.isHalt = false;
    
    return sim_r;
     })));
  const [v49] = txn1.data;
  const v50 = txn1.value;
  const v56 = txn1.time;
  const v48 = txn1.from;
  stdlib.assert(true, {
    at: './index.rsh:44:9:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Alice'
     });
  const v51 = stdlib.eq(v50, v49);
  stdlib.assert(v51, {
    at: './index.rsh:44:9:dot',
    fs: [],
    msg: 'pay amount correct',
    who: 'Alice'
     });
  const v52 = stdlib.checkedBigNumberify('./index.rsh:compileDApp', stdlib.UInt_max, 0);
  const v55 = stdlib.add(v52, v50);
  const txn2 = await (ctc.recv('Alice', 2, 0, [], false, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10)));
  if (txn2.didTimeout) {
    const txn3 = await (ctc.sendrecv('Alice', 10, 0, stdlib.checkedBigNumberify('reach standard library:77:7:dot', stdlib.UInt_max, 0), [stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address], [v56, v49, v55, v48], stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), [], true, true, false, ((txn3) => {
      const sim_r = { txns: [] };
      sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address]), [stdlib.checkedBigNumberify('reach standard library:77:7:dot', stdlib.UInt_max, 1), v56, v49, v55, v48]);
      sim_r.prevSt_noPrevTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address]), [stdlib.checkedBigNumberify('reach standard library:77:7:dot', stdlib.UInt_max, 1), v49, v55, v48]);
      const [] = txn3.data;
      const v64 = txn3.value;
      const v71 = txn3.time;
      const v63 = txn3.from;
      
      const v66 = stdlib.addressEq(v48, v63);
      stdlib.assert(v66, {
        at: 'reach standard library:77:7:dot',
        fs: ['at ./index.rsh:51:41:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
        msg: 'sender correct',
        who: 'Alice'
         });
      const v65 = stdlib.eq(v64, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
      stdlib.assert(v65, {
        at: 'reach standard library:77:7:dot',
        fs: ['at ./index.rsh:51:41:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
        msg: 'pay amount correct',
        who: 'Alice'
         });
      const v70 = stdlib.add(v55, v64);
      sim_r.txns.push({
        amt: v70,
        to: v48
         });
      sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([]), []);
      sim_r.nextSt_noTime = stdlib.digest(stdlib.T_Tuple([]), []);
      sim_r.isHalt = true;
      
      return sim_r;
       })));
    const [] = txn3.data;
    const v64 = txn3.value;
    const v71 = txn3.time;
    const v63 = txn3.from;
    const v66 = stdlib.addressEq(v48, v63);
    stdlib.assert(v66, {
      at: 'reach standard library:77:7:dot',
      fs: ['at ./index.rsh:51:41:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
      msg: 'sender correct',
      who: 'Alice'
       });
    const v65 = stdlib.eq(v64, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
    stdlib.assert(v65, {
      at: 'reach standard library:77:7:dot',
      fs: ['at ./index.rsh:51:41:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
      msg: 'pay amount correct',
      who: 'Alice'
       });
    const v70 = stdlib.add(v55, v64);
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
    const v90 = txn2.value;
    const v96 = txn2.time;
    const v89 = txn2.from;
    stdlib.assert(true, {
      at: './index.rsh:50:9:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Alice'
       });
    const v91 = stdlib.eq(v90, v49);
    stdlib.assert(v91, {
      at: './index.rsh:50:9:dot',
      fs: [],
      msg: 'pay amount correct',
      who: 'Alice'
       });
    const v95 = stdlib.add(v55, v90);
    let v287 = v95;
    let v289 = v56;
    let v288 = v96;
    let v98 = stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1);
    
    while ((() => {
      const v112 = stdlib.eq(v98, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
      
      return v112; })()) {
      const v117 = stdlib.protect(stdlib.T_UInt, await interact.getHand(), {
        at: './index.rsh:59:42:application',
        fs: ['at ./index.rsh:61:51:after expr stmt semicolon call to [unknown function] (defined at: ./index.rsh:58:19:function exp)'],
        msg: 'getHand',
        who: 'Alice'
         });
      const v119 = stdlib.protect(stdlib.T_UInt, await interact.random(), {
        at: 'reach standard library:69:31:application',
        fs: ['at ./index.rsh:60:52:application call to [unknown function] (defined at: reach standard library:68:8:function exp)', 'at ./index.rsh:61:51:after expr stmt semicolon call to [unknown function] (defined at: ./index.rsh:58:19:function exp)'],
        msg: 'random',
        who: 'Alice'
         });
      const v120 = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt]), [v119, v117]);
      const txn3 = await (ctc.sendrecv('Alice', 4, 1, stdlib.checkedBigNumberify('./index.rsh:62:11:dot', stdlib.UInt_max, 4), [stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Digest], [v49, v48, v89, v287, v288, v120], stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), [stdlib.T_Digest], true, true, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10), ((txn3) => {
        const sim_r = { txns: [] };
        sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:62:11:dot', stdlib.UInt_max, 3), v49, v48, v89, v287, v288]);
        sim_r.prevSt_noPrevTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:62:11:dot', stdlib.UInt_max, 3), v49, v48, v89, v287]);
        const [v150] = txn3.data;
        const v151 = txn3.value;
        const v158 = txn3.time;
        const v149 = txn3.from;
        
        const v153 = stdlib.addressEq(v48, v149);
        stdlib.assert(v153, {
          at: './index.rsh:62:11:dot',
          fs: [],
          msg: 'sender correct',
          who: 'Alice'
           });
        const v152 = stdlib.eq(v151, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
        stdlib.assert(v152, {
          at: './index.rsh:62:11:dot',
          fs: [],
          msg: 'pay amount correct',
          who: 'Alice'
           });
        const v157 = stdlib.add(v287, v151);
        sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Digest, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:64:17:after expr stmt semicolon', stdlib.UInt_max, 4), v49, v48, v89, v158, v150, v157]);
        sim_r.nextSt_noTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:64:17:after expr stmt semicolon', stdlib.UInt_max, 4), v49, v48, v89, v150, v157]);
        sim_r.isHalt = false;
        
        return sim_r;
         })));
      if (txn3.didTimeout) {
        const txn4 = await (ctc.recv('Alice', 9, 0, [], false, false));
        const [] = txn4.data;
        const v124 = txn4.value;
        const v131 = txn4.time;
        const v123 = txn4.from;
        const v126 = stdlib.addressEq(v89, v123);
        stdlib.assert(v126, {
          at: 'reach standard library:77:7:dot',
          fs: ['at ./index.rsh:63:43:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
          msg: 'sender correct',
          who: 'Alice'
           });
        const v125 = stdlib.eq(v124, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
        stdlib.assert(v125, {
          at: 'reach standard library:77:7:dot',
          fs: ['at ./index.rsh:63:43:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
          msg: 'pay amount correct',
          who: 'Alice'
           });
        const v130 = stdlib.add(v287, v124);
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
        const [v150] = txn3.data;
        const v151 = txn3.value;
        const v158 = txn3.time;
        const v149 = txn3.from;
        const v153 = stdlib.addressEq(v48, v149);
        stdlib.assert(v153, {
          at: './index.rsh:62:11:dot',
          fs: [],
          msg: 'sender correct',
          who: 'Alice'
           });
        const v152 = stdlib.eq(v151, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
        stdlib.assert(v152, {
          at: './index.rsh:62:11:dot',
          fs: [],
          msg: 'pay amount correct',
          who: 'Alice'
           });
        const v157 = stdlib.add(v287, v151);
        const txn4 = await (ctc.recv('Alice', 5, 1, [stdlib.T_UInt], false, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10)));
        if (txn4.didTimeout) {
          const txn5 = await (ctc.sendrecv('Alice', 8, 0, stdlib.checkedBigNumberify('reach standard library:77:7:dot', stdlib.UInt_max, 3), [stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Digest, stdlib.T_UInt], [v49, v48, v89, v158, v150, v157], stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), [], true, true, false, ((txn5) => {
            const sim_r = { txns: [] };
            sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Digest, stdlib.T_UInt]), [stdlib.checkedBigNumberify('reach standard library:77:7:dot', stdlib.UInt_max, 4), v49, v48, v89, v158, v150, v157]);
            sim_r.prevSt_noPrevTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt]), [stdlib.checkedBigNumberify('reach standard library:77:7:dot', stdlib.UInt_max, 4), v49, v48, v89, v150, v157]);
            const [] = txn5.data;
            const v166 = txn5.value;
            const v173 = txn5.time;
            const v165 = txn5.from;
            
            const v168 = stdlib.addressEq(v48, v165);
            stdlib.assert(v168, {
              at: 'reach standard library:77:7:dot',
              fs: ['at ./index.rsh:70:43:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
              msg: 'sender correct',
              who: 'Alice'
               });
            const v167 = stdlib.eq(v166, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v167, {
              at: 'reach standard library:77:7:dot',
              fs: ['at ./index.rsh:70:43:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
              msg: 'pay amount correct',
              who: 'Alice'
               });
            const v172 = stdlib.add(v157, v166);
            sim_r.txns.push({
              amt: v172,
              to: v48
               });
            sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([]), []);
            sim_r.nextSt_noTime = stdlib.digest(stdlib.T_Tuple([]), []);
            sim_r.isHalt = true;
            
            return sim_r;
             })));
          const [] = txn5.data;
          const v166 = txn5.value;
          const v173 = txn5.time;
          const v165 = txn5.from;
          const v168 = stdlib.addressEq(v48, v165);
          stdlib.assert(v168, {
            at: 'reach standard library:77:7:dot',
            fs: ['at ./index.rsh:70:43:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
            msg: 'sender correct',
            who: 'Alice'
             });
          const v167 = stdlib.eq(v166, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
          stdlib.assert(v167, {
            at: 'reach standard library:77:7:dot',
            fs: ['at ./index.rsh:70:43:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
            msg: 'pay amount correct',
            who: 'Alice'
             });
          const v172 = stdlib.add(v157, v166);
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
          const [v192] = txn4.data;
          const v193 = txn4.value;
          const v200 = txn4.time;
          const v191 = txn4.from;
          const v195 = stdlib.addressEq(v89, v191);
          stdlib.assert(v195, {
            at: './index.rsh:69:11:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Alice'
             });
          const v194 = stdlib.eq(v193, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
          stdlib.assert(v194, {
            at: './index.rsh:69:11:dot',
            fs: [],
            msg: 'pay amount correct',
            who: 'Alice'
             });
          const v199 = stdlib.add(v157, v193);
          const txn5 = await (ctc.sendrecv('Alice', 6, 2, stdlib.checkedBigNumberify('./index.rsh:75:11:dot', stdlib.UInt_max, 4), [stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt], [v49, v48, v89, v150, v200, v192, v199, v119, v117], stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), [stdlib.T_UInt, stdlib.T_UInt], true, true, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10), ((txn5) => {
            const sim_r = { txns: [] };
            sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:75:11:dot', stdlib.UInt_max, 5), v49, v48, v89, v150, v200, v192, v199]);
            sim_r.prevSt_noPrevTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:75:11:dot', stdlib.UInt_max, 5), v49, v48, v89, v150, v192, v199]);
            const [v233, v234] = txn5.data;
            const v235 = txn5.value;
            const v242 = txn5.time;
            const v232 = txn5.from;
            
            const v237 = stdlib.addressEq(v48, v232);
            stdlib.assert(v237, {
              at: './index.rsh:75:11:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Alice'
               });
            const v236 = stdlib.eq(v235, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v236, {
              at: './index.rsh:75:11:dot',
              fs: [],
              msg: 'pay amount correct',
              who: 'Alice'
               });
            const v241 = stdlib.add(v199, v235);
            const v245 = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt]), [v233, v234]);
            const v247 = stdlib.digestEq(v150, v245);
            stdlib.assert(v247, {
              at: 'reach standard library:74:17:application',
              fs: ['at ./index.rsh:77:24:application call to [unknown function] (defined at: reach standard library:73:8:function exp)'],
              msg: null,
              who: 'Alice'
               });
            const v252 = stdlib.sub(stdlib.checkedBigNumberify('./index.rsh:7:18:decimal', stdlib.UInt_max, 4), v192);
            const v255 = stdlib.add(v234, v252);
            const v256 = stdlib.mod(v255, stdlib.checkedBigNumberify('./index.rsh:7:32:decimal', stdlib.UInt_max, 3));
            const cv287 = v241;
            const cv289 = v200;
            const cv288 = v242;
            const cv98 = v256;
            
            (() => {
              const v287 = cv287;
              const v289 = cv289;
              const v288 = cv288;
              const v98 = cv98;
              
              if ((() => {
                const v112 = stdlib.eq(v98, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
                
                return v112; })()) {
                sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:56:17:after expr stmt semicolon', stdlib.UInt_max, 3), v49, v48, v89, v287, v288]);
                sim_r.nextSt_noTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:56:17:after expr stmt semicolon', stdlib.UInt_max, 3), v49, v48, v89, v287]);
                sim_r.isHalt = false;
                 }
              else {
                const v259 = stdlib.eq(v98, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
                const v265 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:83:16:decimal', stdlib.UInt_max, 2), v49);
                const v270 = v259 ? v48 : v89;
                sim_r.txns.push({
                  amt: v265,
                  to: v270
                   });
                sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([]), []);
                sim_r.nextSt_noTime = stdlib.digest(stdlib.T_Tuple([]), []);
                sim_r.isHalt = true;
                 } })();
            return sim_r;
             })));
          if (txn5.didTimeout) {
            const txn6 = await (ctc.recv('Alice', 7, 0, [], false, false));
            const [] = txn6.data;
            const v207 = txn6.value;
            const v214 = txn6.time;
            const v206 = txn6.from;
            const v209 = stdlib.addressEq(v89, v206);
            stdlib.assert(v209, {
              at: 'reach standard library:77:7:dot',
              fs: ['at ./index.rsh:76:43:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
              msg: 'sender correct',
              who: 'Alice'
               });
            const v208 = stdlib.eq(v207, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v208, {
              at: 'reach standard library:77:7:dot',
              fs: ['at ./index.rsh:76:43:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
              msg: 'pay amount correct',
              who: 'Alice'
               });
            const v213 = stdlib.add(v199, v207);
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
            const [v233, v234] = txn5.data;
            const v235 = txn5.value;
            const v242 = txn5.time;
            const v232 = txn5.from;
            const v237 = stdlib.addressEq(v48, v232);
            stdlib.assert(v237, {
              at: './index.rsh:75:11:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Alice'
               });
            const v236 = stdlib.eq(v235, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v236, {
              at: './index.rsh:75:11:dot',
              fs: [],
              msg: 'pay amount correct',
              who: 'Alice'
               });
            const v241 = stdlib.add(v199, v235);
            const v245 = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt]), [v233, v234]);
            const v247 = stdlib.digestEq(v150, v245);
            stdlib.assert(v247, {
              at: 'reach standard library:74:17:application',
              fs: ['at ./index.rsh:77:24:application call to [unknown function] (defined at: reach standard library:73:8:function exp)'],
              msg: null,
              who: 'Alice'
               });
            const v252 = stdlib.sub(stdlib.checkedBigNumberify('./index.rsh:7:18:decimal', stdlib.UInt_max, 4), v192);
            const v255 = stdlib.add(v234, v252);
            const v256 = stdlib.mod(v255, stdlib.checkedBigNumberify('./index.rsh:7:32:decimal', stdlib.UInt_max, 3));
            const cv287 = v241;
            const cv289 = v200;
            const cv288 = v242;
            const cv98 = v256;
            
            v287 = cv287;
            v289 = cv289;
            v288 = cv288;
            v98 = cv98;
            
            continue; }
           }
         }
       }
    const v259 = stdlib.eq(v98, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
    const v265 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:83:16:decimal', stdlib.UInt_max, 2), v49);
    const v270 = v259 ? v48 : v89;
    ;
    stdlib.protect(stdlib.T_Null, await interact.seeOutcome(v98), {
      at: './index.rsh:87:28:application',
      fs: ['at ./index.rsh:87:41:after expr stmt semicolon call to [unknown function] (defined at: ./index.rsh:86:23:function exp)'],
      msg: 'seeOutcome',
      who: 'Alice'
       });
    return; }
  
  
   }
export async function Bob(ctc, interact) {
  const stdlib = ctc.stdlib;
  const v45 = await ctc.creationTime();
  const txn1 = await (ctc.recv('Bob', 1, 1, [stdlib.T_UInt], false, false));
  const [v49] = txn1.data;
  const v50 = txn1.value;
  const v56 = txn1.time;
  const v48 = txn1.from;
  stdlib.assert(true, {
    at: './index.rsh:44:9:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Bob'
     });
  const v51 = stdlib.eq(v50, v49);
  stdlib.assert(v51, {
    at: './index.rsh:44:9:dot',
    fs: [],
    msg: 'pay amount correct',
    who: 'Bob'
     });
  const v52 = stdlib.checkedBigNumberify('./index.rsh:compileDApp', stdlib.UInt_max, 0);
  const v55 = stdlib.add(v52, v50);
  stdlib.protect(stdlib.T_Null, await interact.acceptWager(v49), {
    at: './index.rsh:49:29:application',
    fs: ['at ./index.rsh:49:40:after expr stmt semicolon call to [unknown function] (defined at: ./index.rsh:48:17:function exp)'],
    msg: 'acceptWager',
    who: 'Bob'
     });
  const txn2 = await (ctc.sendrecv('Bob', 2, 0, stdlib.checkedBigNumberify('./index.rsh:50:9:dot', stdlib.UInt_max, 0), [stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address], [v56, v49, v55, v48], v49, [], true, true, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10), ((txn2) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address]), [stdlib.checkedBigNumberify('./index.rsh:50:9:dot', stdlib.UInt_max, 1), v56, v49, v55, v48]);
    sim_r.prevSt_noPrevTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address]), [stdlib.checkedBigNumberify('./index.rsh:50:9:dot', stdlib.UInt_max, 1), v49, v55, v48]);
    const [] = txn2.data;
    const v90 = txn2.value;
    const v96 = txn2.time;
    const v89 = txn2.from;
    
    stdlib.assert(true, {
      at: './index.rsh:50:9:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Bob'
       });
    const v91 = stdlib.eq(v90, v49);
    stdlib.assert(v91, {
      at: './index.rsh:50:9:dot',
      fs: [],
      msg: 'pay amount correct',
      who: 'Bob'
       });
    const v95 = stdlib.add(v55, v90);
    const v287 = v95;
    const v289 = v56;
    const v288 = v96;
    const v98 = stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1);
    
    if ((() => {
      const v112 = stdlib.eq(v98, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
      
      return v112; })()) {
      sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:56:17:after expr stmt semicolon', stdlib.UInt_max, 3), v49, v48, v89, v287, v288]);
      sim_r.nextSt_noTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:56:17:after expr stmt semicolon', stdlib.UInt_max, 3), v49, v48, v89, v287]);
      sim_r.isHalt = false;
       }
    else {
      const v259 = stdlib.eq(v98, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
      const v265 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:83:16:decimal', stdlib.UInt_max, 2), v49);
      const v270 = v259 ? v48 : v89;
      sim_r.txns.push({
        amt: v265,
        to: v270
         });
      sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([]), []);
      sim_r.nextSt_noTime = stdlib.digest(stdlib.T_Tuple([]), []);
      sim_r.isHalt = true;
       }
    return sim_r;
     })));
  if (txn2.didTimeout) {
    const txn3 = await (ctc.recv('Bob', 10, 0, [], false, false));
    const [] = txn3.data;
    const v64 = txn3.value;
    const v71 = txn3.time;
    const v63 = txn3.from;
    const v66 = stdlib.addressEq(v48, v63);
    stdlib.assert(v66, {
      at: 'reach standard library:77:7:dot',
      fs: ['at ./index.rsh:51:41:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
      msg: 'sender correct',
      who: 'Bob'
       });
    const v65 = stdlib.eq(v64, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
    stdlib.assert(v65, {
      at: 'reach standard library:77:7:dot',
      fs: ['at ./index.rsh:51:41:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
      msg: 'pay amount correct',
      who: 'Bob'
       });
    const v70 = stdlib.add(v55, v64);
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
    const v90 = txn2.value;
    const v96 = txn2.time;
    const v89 = txn2.from;
    stdlib.assert(true, {
      at: './index.rsh:50:9:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Bob'
       });
    const v91 = stdlib.eq(v90, v49);
    stdlib.assert(v91, {
      at: './index.rsh:50:9:dot',
      fs: [],
      msg: 'pay amount correct',
      who: 'Bob'
       });
    const v95 = stdlib.add(v55, v90);
    let v287 = v95;
    let v289 = v56;
    let v288 = v96;
    let v98 = stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1);
    
    while ((() => {
      const v112 = stdlib.eq(v98, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
      
      return v112; })()) {
      const txn3 = await (ctc.recv('Bob', 4, 1, [stdlib.T_Digest], false, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10)));
      if (txn3.didTimeout) {
        const txn4 = await (ctc.sendrecv('Bob', 9, 0, stdlib.checkedBigNumberify('reach standard library:77:7:dot', stdlib.UInt_max, 4), [stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_UInt, stdlib.T_UInt], [v49, v48, v89, v287, v288], stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), [], true, true, false, ((txn4) => {
          const sim_r = { txns: [] };
          sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('reach standard library:77:7:dot', stdlib.UInt_max, 3), v49, v48, v89, v287, v288]);
          sim_r.prevSt_noPrevTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_UInt]), [stdlib.checkedBigNumberify('reach standard library:77:7:dot', stdlib.UInt_max, 3), v49, v48, v89, v287]);
          const [] = txn4.data;
          const v124 = txn4.value;
          const v131 = txn4.time;
          const v123 = txn4.from;
          
          const v126 = stdlib.addressEq(v89, v123);
          stdlib.assert(v126, {
            at: 'reach standard library:77:7:dot',
            fs: ['at ./index.rsh:63:43:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
            msg: 'sender correct',
            who: 'Bob'
             });
          const v125 = stdlib.eq(v124, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
          stdlib.assert(v125, {
            at: 'reach standard library:77:7:dot',
            fs: ['at ./index.rsh:63:43:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
            msg: 'pay amount correct',
            who: 'Bob'
             });
          const v130 = stdlib.add(v287, v124);
          sim_r.txns.push({
            amt: v130,
            to: v89
             });
          sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([]), []);
          sim_r.nextSt_noTime = stdlib.digest(stdlib.T_Tuple([]), []);
          sim_r.isHalt = true;
          
          return sim_r;
           })));
        const [] = txn4.data;
        const v124 = txn4.value;
        const v131 = txn4.time;
        const v123 = txn4.from;
        const v126 = stdlib.addressEq(v89, v123);
        stdlib.assert(v126, {
          at: 'reach standard library:77:7:dot',
          fs: ['at ./index.rsh:63:43:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
          msg: 'sender correct',
          who: 'Bob'
           });
        const v125 = stdlib.eq(v124, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
        stdlib.assert(v125, {
          at: 'reach standard library:77:7:dot',
          fs: ['at ./index.rsh:63:43:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
          msg: 'pay amount correct',
          who: 'Bob'
           });
        const v130 = stdlib.add(v287, v124);
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
        const [v150] = txn3.data;
        const v151 = txn3.value;
        const v158 = txn3.time;
        const v149 = txn3.from;
        const v153 = stdlib.addressEq(v48, v149);
        stdlib.assert(v153, {
          at: './index.rsh:62:11:dot',
          fs: [],
          msg: 'sender correct',
          who: 'Bob'
           });
        const v152 = stdlib.eq(v151, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
        stdlib.assert(v152, {
          at: './index.rsh:62:11:dot',
          fs: [],
          msg: 'pay amount correct',
          who: 'Bob'
           });
        const v157 = stdlib.add(v287, v151);
        const v163 = stdlib.protect(stdlib.T_UInt, await interact.getHand(), {
          at: './index.rsh:68:52:application',
          fs: ['at ./index.rsh:68:59:after expr stmt semicolon call to [unknown function] (defined at: ./index.rsh:67:19:function exp)'],
          msg: 'getHand',
          who: 'Bob'
           });
        const txn4 = await (ctc.sendrecv('Bob', 5, 1, stdlib.checkedBigNumberify('./index.rsh:69:11:dot', stdlib.UInt_max, 3), [stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt], [v49, v48, v89, v158, v150, v157, v163], stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), [stdlib.T_UInt], true, true, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10), ((txn4) => {
          const sim_r = { txns: [] };
          sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Digest, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:69:11:dot', stdlib.UInt_max, 4), v49, v48, v89, v158, v150, v157]);
          sim_r.prevSt_noPrevTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:69:11:dot', stdlib.UInt_max, 4), v49, v48, v89, v150, v157]);
          const [v192] = txn4.data;
          const v193 = txn4.value;
          const v200 = txn4.time;
          const v191 = txn4.from;
          
          const v195 = stdlib.addressEq(v89, v191);
          stdlib.assert(v195, {
            at: './index.rsh:69:11:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Bob'
             });
          const v194 = stdlib.eq(v193, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
          stdlib.assert(v194, {
            at: './index.rsh:69:11:dot',
            fs: [],
            msg: 'pay amount correct',
            who: 'Bob'
             });
          const v199 = stdlib.add(v157, v193);
          sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:71:17:after expr stmt semicolon', stdlib.UInt_max, 5), v49, v48, v89, v150, v200, v192, v199]);
          sim_r.nextSt_noTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:71:17:after expr stmt semicolon', stdlib.UInt_max, 5), v49, v48, v89, v150, v192, v199]);
          sim_r.isHalt = false;
          
          return sim_r;
           })));
        if (txn4.didTimeout) {
          const txn5 = await (ctc.recv('Bob', 8, 0, [], false, false));
          const [] = txn5.data;
          const v166 = txn5.value;
          const v173 = txn5.time;
          const v165 = txn5.from;
          const v168 = stdlib.addressEq(v48, v165);
          stdlib.assert(v168, {
            at: 'reach standard library:77:7:dot',
            fs: ['at ./index.rsh:70:43:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
            msg: 'sender correct',
            who: 'Bob'
             });
          const v167 = stdlib.eq(v166, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
          stdlib.assert(v167, {
            at: 'reach standard library:77:7:dot',
            fs: ['at ./index.rsh:70:43:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
            msg: 'pay amount correct',
            who: 'Bob'
             });
          const v172 = stdlib.add(v157, v166);
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
          const [v192] = txn4.data;
          const v193 = txn4.value;
          const v200 = txn4.time;
          const v191 = txn4.from;
          const v195 = stdlib.addressEq(v89, v191);
          stdlib.assert(v195, {
            at: './index.rsh:69:11:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Bob'
             });
          const v194 = stdlib.eq(v193, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
          stdlib.assert(v194, {
            at: './index.rsh:69:11:dot',
            fs: [],
            msg: 'pay amount correct',
            who: 'Bob'
             });
          const v199 = stdlib.add(v157, v193);
          const txn5 = await (ctc.recv('Bob', 6, 2, [stdlib.T_UInt, stdlib.T_UInt], false, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10)));
          if (txn5.didTimeout) {
            const txn6 = await (ctc.sendrecv('Bob', 7, 0, stdlib.checkedBigNumberify('reach standard library:77:7:dot', stdlib.UInt_max, 4), [stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt], [v49, v48, v89, v150, v200, v192, v199], stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), [], true, true, false, ((txn6) => {
              const sim_r = { txns: [] };
              sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('reach standard library:77:7:dot', stdlib.UInt_max, 5), v49, v48, v89, v150, v200, v192, v199]);
              sim_r.prevSt_noPrevTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('reach standard library:77:7:dot', stdlib.UInt_max, 5), v49, v48, v89, v150, v192, v199]);
              const [] = txn6.data;
              const v207 = txn6.value;
              const v214 = txn6.time;
              const v206 = txn6.from;
              
              const v209 = stdlib.addressEq(v89, v206);
              stdlib.assert(v209, {
                at: 'reach standard library:77:7:dot',
                fs: ['at ./index.rsh:76:43:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
                msg: 'sender correct',
                who: 'Bob'
                 });
              const v208 = stdlib.eq(v207, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
              stdlib.assert(v208, {
                at: 'reach standard library:77:7:dot',
                fs: ['at ./index.rsh:76:43:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
                msg: 'pay amount correct',
                who: 'Bob'
                 });
              const v213 = stdlib.add(v199, v207);
              sim_r.txns.push({
                amt: v213,
                to: v89
                 });
              sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([]), []);
              sim_r.nextSt_noTime = stdlib.digest(stdlib.T_Tuple([]), []);
              sim_r.isHalt = true;
              
              return sim_r;
               })));
            const [] = txn6.data;
            const v207 = txn6.value;
            const v214 = txn6.time;
            const v206 = txn6.from;
            const v209 = stdlib.addressEq(v89, v206);
            stdlib.assert(v209, {
              at: 'reach standard library:77:7:dot',
              fs: ['at ./index.rsh:76:43:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
              msg: 'sender correct',
              who: 'Bob'
               });
            const v208 = stdlib.eq(v207, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v208, {
              at: 'reach standard library:77:7:dot',
              fs: ['at ./index.rsh:76:43:application call to [unknown function] (defined at: reach standard library:76:8:function exp)'],
              msg: 'pay amount correct',
              who: 'Bob'
               });
            const v213 = stdlib.add(v199, v207);
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
            const [v233, v234] = txn5.data;
            const v235 = txn5.value;
            const v242 = txn5.time;
            const v232 = txn5.from;
            const v237 = stdlib.addressEq(v48, v232);
            stdlib.assert(v237, {
              at: './index.rsh:75:11:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Bob'
               });
            const v236 = stdlib.eq(v235, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v236, {
              at: './index.rsh:75:11:dot',
              fs: [],
              msg: 'pay amount correct',
              who: 'Bob'
               });
            const v241 = stdlib.add(v199, v235);
            const v245 = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt]), [v233, v234]);
            const v247 = stdlib.digestEq(v150, v245);
            stdlib.assert(v247, {
              at: 'reach standard library:74:17:application',
              fs: ['at ./index.rsh:77:24:application call to [unknown function] (defined at: reach standard library:73:8:function exp)'],
              msg: null,
              who: 'Bob'
               });
            const v252 = stdlib.sub(stdlib.checkedBigNumberify('./index.rsh:7:18:decimal', stdlib.UInt_max, 4), v192);
            const v255 = stdlib.add(v234, v252);
            const v256 = stdlib.mod(v255, stdlib.checkedBigNumberify('./index.rsh:7:32:decimal', stdlib.UInt_max, 3));
            const cv287 = v241;
            const cv289 = v200;
            const cv288 = v242;
            const cv98 = v256;
            
            v287 = cv287;
            v289 = cv289;
            v288 = cv288;
            v98 = cv98;
            
            continue; }
           }
         }
       }
    const v259 = stdlib.eq(v98, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
    const v265 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:83:16:decimal', stdlib.UInt_max, 2), v49);
    const v270 = v259 ? v48 : v89;
    ;
    stdlib.protect(stdlib.T_Null, await interact.seeOutcome(v98), {
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
// compute state in HM_Set
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
// compute state in HM_Check 0
int 0
itob
keccak256
arg 0
==
assert
// Run body
// Just "sender correct"
// "./index.rsh:44:9:dot"
// "[]"
int 1
assert
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
int 0
gtxn 3 Amount
arg 3
btoi
-
+
store 255
// compute state in HM_Set
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
// Just "sender correct"
// "./index.rsh:50:9:dot"
// "[]"
int 1
assert
// Just "pay amount correct"
// "./index.rsh:50:9:dot"
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
// compute state in HM_Set
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
// compute state in HM_Check 3
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
// "./index.rsh:62:11:dot"
// "[]"
arg 6
gtxn 3 Sender
==
assert
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
arg 8
btoi
gtxn 3 Amount
arg 3
btoi
-
+
store 255
// compute state in HM_Set
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
arg 9
concat
keccak256
arg 0
==
assert
// Run body
// Just "sender correct"
// "./index.rsh:69:11:dot"
// "[]"
arg 7
gtxn 3 Sender
==
assert
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
arg 9
btoi
gtxn 3 Amount
arg 3
btoi
-
+
store 255
// compute state in HM_Set
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
// compute state in HM_Check 5
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
// "./index.rsh:75:11:dot"
// "[]"
arg 6
gtxn 3 Sender
==
assert
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
// compute state in HM_Set
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
// compute state in HM_Check 5
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
// "reach standard library:77:7:dot"
// "[at ./index.rsh:76:43:application call to [unknown function] (defined at: reach standard library:76:8:function exp)]"
arg 7
gtxn 3 Sender
==
assert
// Just "pay amount correct"
// "reach standard library:77:7:dot"
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
arg 9
concat
keccak256
arg 0
==
assert
// Run body
// Just "sender correct"
// "reach standard library:77:7:dot"
// "[at ./index.rsh:70:43:application call to [unknown function] (defined at: reach standard library:76:8:function exp)]"
arg 6
gtxn 3 Sender
==
assert
// Just "pay amount correct"
// "reach standard library:77:7:dot"
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
// compute state in HM_Check 3
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
// "reach standard library:77:7:dot"
// "[at ./index.rsh:63:43:application call to [unknown function] (defined at: reach standard library:76:8:function exp)]"
arg 7
gtxn 3 Sender
==
assert
// Just "pay amount correct"
// "reach standard library:77:7:dot"
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
// Just "sender correct"
// "reach standard library:77:7:dot"
// "[at ./index.rsh:51:41:application call to [unknown function] (defined at: reach standard library:76:8:function exp)]"
arg 7
gtxn 3 Sender
==
assert
// Just "pay amount correct"
// "reach standard library:77:7:dot"
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
                "name": "v45",
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
                "name": "v49",
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
                "name": "v56",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v55",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v48",
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
                "name": "v56",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v55",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v48",
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
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v48",
                "type": "address"
              },
              {
                "internalType": "address payable",
                "name": "v89",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v287",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v288",
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
                "name": "v150",
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
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v48",
                "type": "address"
              },
              {
                "internalType": "address payable",
                "name": "v89",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v158",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v150",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v157",
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
                "name": "v192",
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
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v48",
                "type": "address"
              },
              {
                "internalType": "address payable",
                "name": "v89",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v150",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v200",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v192",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v199",
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
                "name": "v233",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v234",
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
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v48",
                "type": "address"
              },
              {
                "internalType": "address payable",
                "name": "v89",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v150",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v200",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v192",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v199",
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
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v48",
                "type": "address"
              },
              {
                "internalType": "address payable",
                "name": "v89",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v158",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v150",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v157",
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
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v48",
                "type": "address"
              },
              {
                "internalType": "address payable",
                "name": "v89",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v287",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v288",
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
                "name": "v45",
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
                "name": "v49",
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
                "name": "v56",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v55",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v48",
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
                "name": "v56",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v55",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v48",
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
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v48",
                "type": "address"
              },
              {
                "internalType": "address payable",
                "name": "v89",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v287",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v288",
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
                "name": "v150",
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
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v48",
                "type": "address"
              },
              {
                "internalType": "address payable",
                "name": "v89",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v158",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v150",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v157",
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
                "name": "v192",
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
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v48",
                "type": "address"
              },
              {
                "internalType": "address payable",
                "name": "v89",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v150",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v200",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v192",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v199",
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
                "name": "v233",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v234",
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
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v48",
                "type": "address"
              },
              {
                "internalType": "address payable",
                "name": "v89",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v150",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v200",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v192",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v199",
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
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v48",
                "type": "address"
              },
              {
                "internalType": "address payable",
                "name": "v89",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v158",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v150",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v157",
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
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v48",
                "type": "address"
              },
              {
                "internalType": "address payable",
                "name": "v89",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v287",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v288",
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
  Bytecode: `0x608060408190527f49ff028a829527a47ec6839c7147b484eccf5a2a94853eddac09cef44d9d4e9e90600090a161003461007a565b43815261003f61007a565b8151815260405161005790600090839060200161008d565b60408051601f1981840301815291905280516020909101206000555061009c9050565b6040518060200160405280600081525090565b91825251602082015260400190565b6114e7806100ab6000396000f3fe6080604052600436106100865760003560e01c8063b43649a911610059578063b43649a9146100d9578063b8bc1e39146100ec578063d7f6b4cd146100ff578063e2fc8b4b14610112578063eeda42ac1461012557610086565b80633c1726a31461008b5780633e1037b1146100a0578063905436cf146100b35780639532ef01146100c6575b600080fd5b61009e610099366004610ff0565b610138565b005b61009e6100ae366004610fc3565b610257565b61009e6100c1366004610f7c565b6103c9565b61009e6100d4366004610f97565b6104dc565b61009e6100e7366004610f7c565b6105a9565b61009e6100fa366004610fa8565b6106b6565b61009e61010d366004610fc3565b6107fe565b61009e610120366004610fa8565b61090e565b61009e610133366004610fde565b610a1e565b60405161014c9060039083906020016112be565b6040516020818303038152906040528051906020012060001c6000541461017257600080fd5b6101816080820135600a610bd5565b431015801561018e575060015b61019757600080fd5b336101a86060830160408401610f5b565b6001600160a01b0316146101bb57600080fd5b34156101c657600080fd5b6101d66060820160408301610f5b565b6001600160a01b03166108fc6101f0606084013534610bd5565b6040518115909202916000818181858888f19350505050158015610218573d6000803e3d6000fd5b507f893e8b5e3088e38bfe39d4bd08bb72efde6587ca2c04cfca2f91b997377e7f38816040516102489190611251565b60405180910390a16000805533ff5b60405161026b90600490839060200161131d565b6040516020818303038152906040528051906020012060001c6000541461029157600080fd5b610299610d7d565b6102a86060830135600a610bd5565b43106102b357600080fd5b336102c46060840160408501610f5b565b6001600160a01b0316146102d757600080fd5b34156102e257600080fd5b6102f060a083013534610bd5565b81526040517f8b364c1cec79e889c00d6309a0d6316af59862f7883ed242d587454e9f375b08906103229084906111ee565b60405180910390a1610332610d90565b823581526103466040840160208501610f5b565b6001600160a01b031660208201526103646060840160408501610f5b565b6001600160a01b03166040808301919091526080808501356060840152439083015260c08085013560a0840152835190830152516103a990600590839060200161139d565b60408051601f198184030181529190528051602090910120600055505050565b6040516103dd90600190839060200161126e565b6040516020818303038152906040528051906020012060001c6000541461040357600080fd5b61040f8135600a610bd5565b431061041a57600080fd5b3460208201351461042a57600080fd5b7f5074eecd0995b3a3f596cc2835f720a588e59c1848a49558a0743c6f9bb251ba8160405161045991906111ad565b60405180910390a1610469610ddf565b8051602083013590526104826080830160608401610f5b565b81516001600160a01b039091166020909101528051336040918201526104ab9083013534610bd5565b6020808301805192909252815184359101528051436040909101525160016060909101526104d881610c12565b5050565b6040516104f090600090839060200161125f565b6040516020818303038152906040528051906020012060001c6000541461051657600080fd5b61051e610d7d565b3460208301351461052e57600080fd5b610539600034610bd5565b81526040517ff2c62eba998811305a23599b2e6d212befbd7ded3a73f4c08bfb9aefe08dc1669061056b9084906111bb565b60405180910390a161057b610e04565b438152602080840135818301528251604080840191909152336060840152516103a991600191849101611282565b6040516105bd90600190839060200161126e565b6040516020818303038152906040528051906020012060001c600054146105e357600080fd5b6105ef8135600a610bd5565b43101580156105fc575060015b61060557600080fd5b336106166080830160608401610f5b565b6001600160a01b03161461062957600080fd5b341561063457600080fd5b6106446080820160608301610f5b565b6001600160a01b03166108fc61065e604084013534610bd5565b6040518115909202916000818181858888f19350505050158015610686573d6000803e3d6000fd5b507f5891766ff36501b641f062681fd9d274fb08f40cbfbf3fba5e3c907f9b6f320a8160405161024891906111ad565b6040516106ca9060039083906020016112be565b6040516020818303038152906040528051906020012060001c600054146106f057600080fd5b6106f8610d7d565b6107076080830135600a610bd5565b431061071257600080fd5b336107236040840160208501610f5b565b6001600160a01b03161461073657600080fd5b341561074157600080fd5b61074f606083013534610bd5565b81526040517ff948ef1ad0a1a495fc3532d2393b1845f35c1073faeaa7a785c3a443ee951747906107819084906111d2565b60405180910390a1610791610e35565b823581526107a56040840160208501610f5b565b6001600160a01b031660208201526107c36060840160408501610f5b565b6001600160a01b031660408083019190915243606083015260a0808501356080840152835190830152516103a9906004908390602001611331565b604051610812906005908390602001611388565b6040516020818303038152906040528051906020012060001c6000541461083857600080fd5b6108476080820135600a610bd5565b4310158015610854575060015b61085d57600080fd5b3361086e6060830160408401610f5b565b6001600160a01b03161461088157600080fd5b341561088c57600080fd5b61089c6060820160408301610f5b565b6001600160a01b03166108fc6108b660c084013534610bd5565b6040518115909202916000818181858888f193505050501580156108de573d6000803e3d6000fd5b507feb62f87838dee3686c1f4cf7779f83baab34ca940fc98a1a989530712bea0788816040516102489190611235565b60405161092290600490839060200161131d565b6040516020818303038152906040528051906020012060001c6000541461094857600080fd5b6109576060820135600a610bd5565b4310158015610964575060015b61096d57600080fd5b3361097e6040830160208401610f5b565b6001600160a01b03161461099157600080fd5b341561099c57600080fd5b6109ac6040820160208301610f5b565b6001600160a01b03166108fc6109c660a084013534610bd5565b6040518115909202916000818181858888f193505050501580156109ee573d6000803e3d6000fd5b507f22dfeb9e788c86fb591f8ae2f63009c6f04bff2687696501b1448b747ec9c0af816040516102489190611243565b604051610a32906005908390602001611388565b6040516020818303038152906040528051906020012060001c60005414610a5857600080fd5b610a676080820135600a610bd5565b4310610a7257600080fd5b33610a836040830160208401610f5b565b6001600160a01b031614610a9657600080fd5b3415610aa157600080fd5b604051610abd9060e08301359061010084013590602001611401565b60408051601f198184030181529190528051602090910120606082013514610ae457600080fd5b7f4f661d1e3c80826d8bb1ea490d5b6a2290c8b62fdbe859223656d2a583857a0081604051610b13919061120a565b60405180910390a1610b23610ddf565b805182359052610b396040830160208401610f5b565b81516001600160a01b03909116602090910152610b5c6060830160408401610f5b565b81516001600160a01b03909116604090910152610b7d60c083013534610bd5565b60208083018051929092528151608085013591015251436040909101526003610bb9610100840135610bb4600460a0870135610d0f565b610bd5565b610bc39190611471565b6020820151606001526104d881610c12565b600082610be2838261140f565b9150811015610c0c5760405162461bcd60e51b8152600401610c0390611187565b60405180910390fd5b92915050565b60018160200151606001511415610c9b57610c2b610e7d565b815151815281516020908101516001600160a01b0390811682840152835160409081015190911681840152818401805151606085015251810151608084015251610c7a916003918491016112d2565b60408051601f19818403018152919052805160209091012060005550610d0c565b600281602001516060015114610cb657805160400151610cbd565b8051602001515b6001600160a01b03166108fc610cdc6002846000015160000151610d3d565b6040518115909202916000818181858888f19350505050158015610d04573d6000803e3d6000fd5b506000805533ff5b50565b600082610d1c838261145a565b9150811115610c0c5760405162461bcd60e51b8152600401610c0390611139565b6000811580610d6157508282610d53818361143b565b9250610d5f9083611427565b145b610c0c5760405162461bcd60e51b8152600401610c0390611161565b6040518060200160405280600081525090565b6040518060e001604052806000815260200160006001600160a01b0316815260200160006001600160a01b03168152602001600081526020016000815260200160008152602001600081525090565b6040518060400160405280610df2610ebe565b8152602001610dff610ede565b905290565b604051806080016040528060008152602001600081526020016000815260200160006001600160a01b031681525090565b6040518060c001604052806000815260200160006001600160a01b0316815260200160006001600160a01b031681526020016000815260200160008152602001600081525090565b6040518060a001604052806000815260200160006001600160a01b0316815260200160006001600160a01b0316815260200160008152602001600081525090565b604080516060810182526000808252602082018190529181019190915290565b6040518060800160405280600081526020016000815260200160008152602001600081525090565b80356001600160a01b0381168114610f1d57600080fd5b919050565b600060808284031215610f33578081fd5b50919050565b600060c08284031215610f33578081fd5b600060e08284031215610f33578081fd5b600060208284031215610f6c578081fd5b610f7582610f06565b9392505050565b600060808284031215610f8d578081fd5b610f758383610f22565b600060408284031215610f33578081fd5b600060c08284031215610fb9578081fd5b610f758383610f39565b600060e08284031215610fd4578081fd5b610f758383610f4a565b60006101208284031215610f33578081fd5b600060a08284031215610f33578081fd5b8035825260208082013590830152604080820135908301526001600160a01b0361102d60608301610f06565b1660608301525050565b8035825261104760208201610f06565b6001600160a01b0381811660208501528061106460408501610f06565b166040850152505060608181013590830152608090810135910152565b8035825261109160208201610f06565b6001600160a01b038181166020850152806110ae60408501610f06565b1660408501525050606081013560608301526080810135608083015260a081013560a08301525050565b803582526110e860208201610f06565b6001600160a01b0381811660208501528061110560408501610f06565b1660408501525050606081013560608301526080810135608083015260a081013560a083015260c081013560c08301525050565b6020808252600e908201526d1cdd58881ddc985c185c9bdd5b9960921b604082015260600190565b6020808252600c908201526b6d756c206f766572666c6f7760a01b604082015260600190565b6020808252600c908201526b616464206f766572666c6f7760a01b604082015260600190565b60808101610c0c8284611001565b813581526020918201359181019190915260400190565b60c081016111e08284611037565b60a092830135919092015290565b60e081016111fc8284611081565b60c092830135919092015290565b610120810161121982846110d8565b60e083013560e083015261010080840135818401525092915050565b60e08101610c0c82846110d8565b60c08101610c0c8284611081565b60a08101610c0c8284611037565b91825235602082015260400190565b82815260a08101610f756020830184611001565b918252805160208084019190915281015160408084019190915281015160608084019190915201516001600160a01b0316608082015260a00190565b82815260c08101610f756020830184611037565b91825280516020808401919091528101516001600160a01b0390811660408085019190915282015116606080840191909152810151608080840191909152015160a082015260c00190565b82815260e08101610f756020830184611081565b91825280516020808401919091528101516001600160a01b039081166040808501919091528201511660608084019190915281015160808084019190915281015160a080840191909152015160c082015260e00190565b8281526101008101610f7560208301846110d8565b60006101008201905083825282516020830152602083015160018060a01b038082166040850152806040860151166060850152505060608301516080830152608083015160a083015260a083015160c083015260c083015160e08301529392505050565b918252602082015260400190565b6000821982111561142257611422611485565b500190565b6000826114365761143661149b565b500490565b600081600019048311821515161561145557611455611485565b500290565b60008282101561146c5761146c611485565b500390565b6000826114805761148061149b565b500690565b634e487b7160e01b600052601160045260246000fd5b634e487b7160e01b600052601260045260246000fdfea264697066735822122081ba9dab6b9e40f2dfceb36f95ea460c8144bfa916fa9a0b89dc70d2a17f258764736f6c63430008000033`,
  deployMode: `DM_constructor`
   };

export const _Connectors = {
  ALGO: _ALGO,
  ETH: _ETH
   };

