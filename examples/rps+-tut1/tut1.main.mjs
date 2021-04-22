// Automatically generated with Reach 0.1.2
/* eslint-disable */
export const _version = '0.1.2';


export function getExports(s) {
  const stdlib = s.reachStdlib;
  return {
    };
  };

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
  
  
  const v51 = await ctc.creationTime();
  const v49 = stdlib.protect(ctc0, interact.DEADLINE, null);
  const v50 = stdlib.protect(ctc0, interact.wager, null);
  const v55 = stdlib.protect(ctc0, await interact.getHand(), {
    at: './tut1.rsh:46:45:application',
    fs: ['at ./tut1.rsh:43:13:application call to [unknown function] (defined at: ./tut1.rsh:43:17:function exp)'],
    msg: 'getHand',
    who: 'Alice'
    });
  const v57 = stdlib.protect(ctc0, await interact.random(), {
    at: 'reach standard library:60:31:application',
    fs: ['at ./tut1.rsh:47:72:application call to "makeCommitment" (defined at: reach standard library:59:8:function exp)', 'at ./tut1.rsh:43:13:application call to [unknown function] (defined at: ./tut1.rsh:43:17:function exp)'],
    msg: 'random',
    who: 'Alice'
    });
  const v58 = stdlib.digest(ctc1, [v57, v55]);
  const txn1 = await (ctc.sendrecv(1, 3, stdlib.checkedBigNumberify('./tut1.rsh:51:9:dot', stdlib.UInt_max, 0), [ctc0, ctc0, ctc0, ctc2], [v51, v50, v49, v58], [v50, []], [ctc0, ctc0, ctc2], true, true, false, (async (txn1) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.digest(ctc1, [stdlib.checkedBigNumberify('./tut1.rsh:51:9:dot', stdlib.UInt_max, 0), v51]);
    sim_r.prevSt_noPrevTime = stdlib.digest(ctc15, [stdlib.checkedBigNumberify('./tut1.rsh:51:9:dot', stdlib.UInt_max, 0)]);
    const [v61, v62, v63] = txn1.data;
    const v66 = txn1.time;
    const v60 = txn1.from;
    
    stdlib.assert(true, {
      at: './tut1.rsh:51:9:dot',
      fs: [],
      msg: null,
      who: 'Alice'
      });
    const v65 = stdlib.add(stdlib.checkedBigNumberify('./tut1.rsh:compileDApp', stdlib.UInt_max, 0), v61);
    sim_r.txns.push({
      amt: v61,
      kind: 'to',
      tok: undefined
      });
    stdlib.assert(true, {
      at: './tut1.rsh:51:9:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Alice'
      });
    sim_r.nextSt = stdlib.digest(ctc13, [stdlib.checkedBigNumberify('./tut1.rsh:53:15:after expr stmt semicolon', stdlib.UInt_max, 1), v60, v61, v62, v63, v65, v66]);
    sim_r.nextSt_noTime = stdlib.digest(ctc14, [stdlib.checkedBigNumberify('./tut1.rsh:53:15:after expr stmt semicolon', stdlib.UInt_max, 1), v60, v61, v62, v63, v65]);
    sim_r.isHalt = false;
    
    return sim_r;
    })));
  const [v61, v62, v63] = txn1.data;
  const v66 = txn1.time;
  const v60 = txn1.from;
  stdlib.assert(true, {
    at: './tut1.rsh:51:9:dot',
    fs: [],
    msg: null,
    who: 'Alice'
    });
  const v65 = stdlib.add(stdlib.checkedBigNumberify('./tut1.rsh:compileDApp', stdlib.UInt_max, 0), v61);
  ;
  stdlib.assert(true, {
    at: './tut1.rsh:51:9:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Alice'
    });
  const txn2 = await (ctc.recv(2, 1, [ctc0], false, v62));
  if (txn2.didTimeout) {
    const txn3 = await (ctc.sendrecv(3, 0, stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 5), [ctc4, ctc0, ctc0, ctc2, ctc0, ctc0], [v60, v61, v62, v63, v65, v66], [stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), []], [], true, true, false, (async (txn3) => {
      const sim_r = { txns: [] };
      sim_r.prevSt = stdlib.digest(ctc13, [stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 1), v60, v61, v62, v63, v65, v66]);
      sim_r.prevSt_noPrevTime = stdlib.digest(ctc14, [stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 1), v60, v61, v62, v63, v65]);
      const [] = txn3.data;
      const v252 = txn3.time;
      const v248 = txn3.from;
      
      stdlib.assert(true, {
        at: 'reach standard library:209:7:dot',
        fs: ['at ./tut1.rsh:62:41:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
        msg: null,
        who: 'Alice'
        });
      const v250 = stdlib.add(v65, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
      sim_r.txns.push({
        amt: stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0),
        kind: 'to',
        tok: undefined
        });
      const v251 = stdlib.addressEq(v60, v248);
      stdlib.assert(v251, {
        at: 'reach standard library:209:7:dot',
        fs: ['at ./tut1.rsh:62:41:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
        msg: 'sender correct',
        who: 'Alice'
        });
      sim_r.txns.push({
        amt: v250,
        kind: 'from',
        to: v60,
        tok: undefined
        });
      sim_r.txns.push({
        kind: 'halt',
        tok: undefined
        })
      sim_r.nextSt = stdlib.digest(ctc7, []);
      sim_r.nextSt_noTime = stdlib.digest(ctc7, []);
      sim_r.isHalt = true;
      
      return sim_r;
      })));
    const [] = txn3.data;
    const v252 = txn3.time;
    const v248 = txn3.from;
    stdlib.assert(true, {
      at: 'reach standard library:209:7:dot',
      fs: ['at ./tut1.rsh:62:41:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
      msg: null,
      who: 'Alice'
      });
    const v250 = stdlib.add(v65, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
    ;
    const v251 = stdlib.addressEq(v60, v248);
    stdlib.assert(v251, {
      at: 'reach standard library:209:7:dot',
      fs: ['at ./tut1.rsh:62:41:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
      msg: 'sender correct',
      who: 'Alice'
      });
    ;
    stdlib.protect(ctc3, await interact.informTimeout(), {
      at: './tut1.rsh:41:33:application',
      fs: ['at ./tut1.rsh:40:13:application call to [unknown function] (defined at: ./tut1.rsh:40:25:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./tut1.rsh:39:32:function exp)', 'at ./tut1.rsh:62:41:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
      msg: 'informTimeout',
      who: 'Alice'
      });
    return;
    }
  else {
    const [v72] = txn2.data;
    const v75 = txn2.time;
    const v71 = txn2.from;
    stdlib.assert(true, {
      at: './tut1.rsh:60:9:dot',
      fs: [],
      msg: null,
      who: 'Alice'
      });
    const v74 = stdlib.add(v65, v61);
    ;
    stdlib.assert(true, {
      at: './tut1.rsh:60:9:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Alice'
      });
    const txn3 = await (ctc.sendrecv(4, 2, stdlib.checkedBigNumberify('./tut1.rsh:68:9:dot', stdlib.UInt_max, 7), [ctc4, ctc0, ctc0, ctc2, ctc4, ctc0, ctc0, ctc0, ctc0, ctc0], [v60, v61, v62, v63, v71, v72, v74, v75, v57, v55], [stdlib.checkedBigNumberify('./tut1.rsh:decimal', stdlib.UInt_max, 0), []], [ctc0, ctc0], true, true, v62, (async (txn3) => {
      const sim_r = { txns: [] };
      sim_r.prevSt = stdlib.digest(ctc11, [stdlib.checkedBigNumberify('./tut1.rsh:68:9:dot', stdlib.UInt_max, 2), v60, v61, v62, v63, v71, v72, v74, v75]);
      sim_r.prevSt_noPrevTime = stdlib.digest(ctc12, [stdlib.checkedBigNumberify('./tut1.rsh:68:9:dot', stdlib.UInt_max, 2), v60, v61, v62, v63, v71, v72, v74]);
      const [v80, v81] = txn3.data;
      const v85 = txn3.time;
      const v79 = txn3.from;
      
      stdlib.assert(true, {
        at: './tut1.rsh:68:9:dot',
        fs: [],
        msg: null,
        who: 'Alice'
        });
      const v83 = stdlib.add(v74, stdlib.checkedBigNumberify('./tut1.rsh:decimal', stdlib.UInt_max, 0));
      sim_r.txns.push({
        amt: stdlib.checkedBigNumberify('./tut1.rsh:decimal', stdlib.UInt_max, 0),
        kind: 'to',
        tok: undefined
        });
      const v84 = stdlib.addressEq(v60, v79);
      stdlib.assert(v84, {
        at: './tut1.rsh:68:9:dot',
        fs: [],
        msg: 'sender correct',
        who: 'Alice'
        });
      const v87 = stdlib.digest(ctc1, [v80, v81]);
      const v88 = stdlib.digestEq(v63, v87);
      stdlib.assert(v88, {
        at: 'reach standard library:65:17:application',
        fs: ['at ./tut1.rsh:70:22:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)'],
        msg: null,
        who: 'Alice'
        });
      const v91 = stdlib.sub(stdlib.checkedBigNumberify('./tut1.rsh:7:18:decimal', stdlib.UInt_max, 4), v72);
      const v92 = stdlib.add(v81, v91);
      const v93 = stdlib.mod(v92, stdlib.checkedBigNumberify('./tut1.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v95 = v93;
      const v267 = v85;
      const v269 = v83;
      
      if ((() => {
        const v105 = stdlib.eq(v95, stdlib.checkedBigNumberify('./tut1.rsh:makeEnum', stdlib.UInt_max, 1));
        
        return v105;})()) {
        sim_r.nextSt = stdlib.digest(ctc5, [stdlib.checkedBigNumberify('./tut1.rsh:75:17:after expr stmt semicolon', stdlib.UInt_max, 6), v60, v61, v62, v71, v267, v269]);
        sim_r.nextSt_noTime = stdlib.digest(ctc6, [stdlib.checkedBigNumberify('./tut1.rsh:75:17:after expr stmt semicolon', stdlib.UInt_max, 6), v60, v61, v62, v71, v269]);
        sim_r.isHalt = false;
        }
      else {
        const v209 = stdlib.eq(v95, stdlib.checkedBigNumberify('./tut1.rsh:makeEnum', stdlib.UInt_max, 2));
        const v212 = stdlib.mul(stdlib.checkedBigNumberify('./tut1.rsh:102:16:decimal', stdlib.UInt_max, 2), v61);
        const v214 = v209 ? v60 : v71;
        sim_r.txns.push({
          amt: v212,
          kind: 'from',
          to: v214,
          tok: undefined
          });
        sim_r.txns.push({
          kind: 'halt',
          tok: undefined
          })
        sim_r.nextSt = stdlib.digest(ctc7, []);
        sim_r.nextSt_noTime = stdlib.digest(ctc7, []);
        sim_r.isHalt = true;
        }
      return sim_r;
      })));
    if (txn3.didTimeout) {
      const txn4 = await (ctc.recv(5, 0, [], false, false));
      const [] = txn4.data;
      const v232 = txn4.time;
      const v228 = txn4.from;
      stdlib.assert(true, {
        at: 'reach standard library:209:7:dot',
        fs: ['at ./tut1.rsh:69:40:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
        msg: null,
        who: 'Alice'
        });
      const v230 = stdlib.add(v74, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
      ;
      const v231 = stdlib.addressEq(v71, v228);
      stdlib.assert(v231, {
        at: 'reach standard library:209:7:dot',
        fs: ['at ./tut1.rsh:69:40:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
        msg: 'sender correct',
        who: 'Alice'
        });
      ;
      stdlib.protect(ctc3, await interact.informTimeout(), {
        at: './tut1.rsh:41:33:application',
        fs: ['at ./tut1.rsh:40:13:application call to [unknown function] (defined at: ./tut1.rsh:40:25:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./tut1.rsh:39:32:function exp)', 'at ./tut1.rsh:69:40:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
        msg: 'informTimeout',
        who: 'Alice'
        });
      return;
      }
    else {
      const [v80, v81] = txn3.data;
      const v85 = txn3.time;
      const v79 = txn3.from;
      stdlib.assert(true, {
        at: './tut1.rsh:68:9:dot',
        fs: [],
        msg: null,
        who: 'Alice'
        });
      const v83 = stdlib.add(v74, stdlib.checkedBigNumberify('./tut1.rsh:decimal', stdlib.UInt_max, 0));
      ;
      const v84 = stdlib.addressEq(v60, v79);
      stdlib.assert(v84, {
        at: './tut1.rsh:68:9:dot',
        fs: [],
        msg: 'sender correct',
        who: 'Alice'
        });
      const v87 = stdlib.digest(ctc1, [v80, v81]);
      const v88 = stdlib.digestEq(v63, v87);
      stdlib.assert(v88, {
        at: 'reach standard library:65:17:application',
        fs: ['at ./tut1.rsh:70:22:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)'],
        msg: null,
        who: 'Alice'
        });
      const v91 = stdlib.sub(stdlib.checkedBigNumberify('./tut1.rsh:7:18:decimal', stdlib.UInt_max, 4), v72);
      const v92 = stdlib.add(v81, v91);
      const v93 = stdlib.mod(v92, stdlib.checkedBigNumberify('./tut1.rsh:7:32:decimal', stdlib.UInt_max, 3));
      let v95 = v93;
      let v267 = v85;
      let v269 = v83;
      
      while ((() => {
        const v105 = stdlib.eq(v95, stdlib.checkedBigNumberify('./tut1.rsh:makeEnum', stdlib.UInt_max, 1));
        
        return v105;})()) {
        const v109 = stdlib.protect(ctc0, await interact.getHand(), {
          at: './tut1.rsh:78:42:application',
          fs: ['at ./tut1.rsh:77:15:application call to [unknown function] (defined at: ./tut1.rsh:77:19:function exp)'],
          msg: 'getHand',
          who: 'Alice'
          });
        const v111 = stdlib.protect(ctc0, await interact.random(), {
          at: 'reach standard library:60:31:application',
          fs: ['at ./tut1.rsh:79:52:application call to "makeCommitment" (defined at: reach standard library:59:8:function exp)', 'at ./tut1.rsh:77:15:application call to [unknown function] (defined at: ./tut1.rsh:77:19:function exp)'],
          msg: 'random',
          who: 'Alice'
          });
        const v112 = stdlib.digest(ctc1, [v111, v109]);
        const txn4 = await (ctc.sendrecv(8, 1, stdlib.checkedBigNumberify('./tut1.rsh:81:11:dot', stdlib.UInt_max, 4), [ctc4, ctc0, ctc0, ctc4, ctc0, ctc0, ctc2], [v60, v61, v62, v71, v267, v269, v112], [stdlib.checkedBigNumberify('./tut1.rsh:decimal', stdlib.UInt_max, 0), []], [ctc2], true, true, v62, (async (txn4) => {
          const sim_r = { txns: [] };
          sim_r.prevSt = stdlib.digest(ctc5, [stdlib.checkedBigNumberify('./tut1.rsh:81:11:dot', stdlib.UInt_max, 6), v60, v61, v62, v71, v267, v269]);
          sim_r.prevSt_noPrevTime = stdlib.digest(ctc6, [stdlib.checkedBigNumberify('./tut1.rsh:81:11:dot', stdlib.UInt_max, 6), v60, v61, v62, v71, v269]);
          const [v115] = txn4.data;
          const v119 = txn4.time;
          const v114 = txn4.from;
          
          stdlib.assert(true, {
            at: './tut1.rsh:81:11:dot',
            fs: [],
            msg: null,
            who: 'Alice'
            });
          const v117 = stdlib.add(v269, stdlib.checkedBigNumberify('./tut1.rsh:decimal', stdlib.UInt_max, 0));
          sim_r.txns.push({
            amt: stdlib.checkedBigNumberify('./tut1.rsh:decimal', stdlib.UInt_max, 0),
            kind: 'to',
            tok: undefined
            });
          const v118 = stdlib.addressEq(v60, v114);
          stdlib.assert(v118, {
            at: './tut1.rsh:81:11:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Alice'
            });
          sim_r.nextSt = stdlib.digest(ctc9, [stdlib.checkedBigNumberify('./tut1.rsh:83:17:after expr stmt semicolon', stdlib.UInt_max, 8), v60, v61, v62, v71, v115, v117, v119]);
          sim_r.nextSt_noTime = stdlib.digest(ctc10, [stdlib.checkedBigNumberify('./tut1.rsh:83:17:after expr stmt semicolon', stdlib.UInt_max, 8), v60, v61, v62, v71, v115, v117]);
          sim_r.isHalt = false;
          
          return sim_r;
          })));
        if (txn4.didTimeout) {
          const txn5 = await (ctc.recv(9, 0, [], false, false));
          const [] = txn5.data;
          const v194 = txn5.time;
          const v190 = txn5.from;
          stdlib.assert(true, {
            at: 'reach standard library:209:7:dot',
            fs: ['at ./tut1.rsh:82:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
            msg: null,
            who: 'Alice'
            });
          const v192 = stdlib.add(v269, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
          ;
          const v193 = stdlib.addressEq(v71, v190);
          stdlib.assert(v193, {
            at: 'reach standard library:209:7:dot',
            fs: ['at ./tut1.rsh:82:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
            msg: 'sender correct',
            who: 'Alice'
            });
          ;
          stdlib.protect(ctc3, await interact.informTimeout(), {
            at: './tut1.rsh:41:33:application',
            fs: ['at ./tut1.rsh:40:13:application call to [unknown function] (defined at: ./tut1.rsh:40:25:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./tut1.rsh:39:32:function exp)', 'at ./tut1.rsh:82:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
            msg: 'informTimeout',
            who: 'Alice'
            });
          return;
          }
        else {
          const [v115] = txn4.data;
          const v119 = txn4.time;
          const v114 = txn4.from;
          stdlib.assert(true, {
            at: './tut1.rsh:81:11:dot',
            fs: [],
            msg: null,
            who: 'Alice'
            });
          const v117 = stdlib.add(v269, stdlib.checkedBigNumberify('./tut1.rsh:decimal', stdlib.UInt_max, 0));
          ;
          const v118 = stdlib.addressEq(v60, v114);
          stdlib.assert(v118, {
            at: './tut1.rsh:81:11:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Alice'
            });
          const txn5 = await (ctc.recv(10, 1, [ctc0], false, v62));
          if (txn5.didTimeout) {
            const txn6 = await (ctc.sendrecv(11, 0, stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 6), [ctc4, ctc0, ctc0, ctc4, ctc2, ctc0, ctc0], [v60, v61, v62, v71, v115, v117, v119], [stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), []], [], true, true, false, (async (txn6) => {
              const sim_r = { txns: [] };
              sim_r.prevSt = stdlib.digest(ctc9, [stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 8), v60, v61, v62, v71, v115, v117, v119]);
              sim_r.prevSt_noPrevTime = stdlib.digest(ctc10, [stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 8), v60, v61, v62, v71, v115, v117]);
              const [] = txn6.data;
              const v174 = txn6.time;
              const v170 = txn6.from;
              
              stdlib.assert(true, {
                at: 'reach standard library:209:7:dot',
                fs: ['at ./tut1.rsh:89:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
                msg: null,
                who: 'Alice'
                });
              const v172 = stdlib.add(v117, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
              sim_r.txns.push({
                amt: stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0),
                kind: 'to',
                tok: undefined
                });
              const v173 = stdlib.addressEq(v60, v170);
              stdlib.assert(v173, {
                at: 'reach standard library:209:7:dot',
                fs: ['at ./tut1.rsh:89:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
                msg: 'sender correct',
                who: 'Alice'
                });
              sim_r.txns.push({
                amt: v172,
                kind: 'from',
                to: v60,
                tok: undefined
                });
              sim_r.txns.push({
                kind: 'halt',
                tok: undefined
                })
              sim_r.nextSt = stdlib.digest(ctc7, []);
              sim_r.nextSt_noTime = stdlib.digest(ctc7, []);
              sim_r.isHalt = true;
              
              return sim_r;
              })));
            const [] = txn6.data;
            const v174 = txn6.time;
            const v170 = txn6.from;
            stdlib.assert(true, {
              at: 'reach standard library:209:7:dot',
              fs: ['at ./tut1.rsh:89:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
              msg: null,
              who: 'Alice'
              });
            const v172 = stdlib.add(v117, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
            ;
            const v173 = stdlib.addressEq(v60, v170);
            stdlib.assert(v173, {
              at: 'reach standard library:209:7:dot',
              fs: ['at ./tut1.rsh:89:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
              msg: 'sender correct',
              who: 'Alice'
              });
            ;
            stdlib.protect(ctc3, await interact.informTimeout(), {
              at: './tut1.rsh:41:33:application',
              fs: ['at ./tut1.rsh:40:13:application call to [unknown function] (defined at: ./tut1.rsh:40:25:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./tut1.rsh:39:32:function exp)', 'at ./tut1.rsh:89:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
              msg: 'informTimeout',
              who: 'Alice'
              });
            return;
            }
          else {
            const [v125] = txn5.data;
            const v129 = txn5.time;
            const v124 = txn5.from;
            stdlib.assert(true, {
              at: './tut1.rsh:88:11:dot',
              fs: [],
              msg: null,
              who: 'Alice'
              });
            const v127 = stdlib.add(v117, stdlib.checkedBigNumberify('./tut1.rsh:decimal', stdlib.UInt_max, 0));
            ;
            const v128 = stdlib.addressEq(v71, v124);
            stdlib.assert(v128, {
              at: './tut1.rsh:88:11:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Alice'
              });
            const txn6 = await (ctc.sendrecv(12, 2, stdlib.checkedBigNumberify('./tut1.rsh:94:11:dot', stdlib.UInt_max, 7), [ctc4, ctc0, ctc0, ctc4, ctc2, ctc0, ctc0, ctc0, ctc0, ctc0], [v60, v61, v62, v71, v115, v125, v127, v129, v111, v109], [stdlib.checkedBigNumberify('./tut1.rsh:decimal', stdlib.UInt_max, 0), []], [ctc0, ctc0], true, true, v62, (async (txn6) => {
              const sim_r = { txns: [] };
              sim_r.prevSt = stdlib.digest(ctc8, [stdlib.checkedBigNumberify('./tut1.rsh:94:11:dot', stdlib.UInt_max, 10), v60, v61, v62, v71, v115, v125, v127, v129]);
              sim_r.prevSt_noPrevTime = stdlib.digest(ctc9, [stdlib.checkedBigNumberify('./tut1.rsh:94:11:dot', stdlib.UInt_max, 10), v60, v61, v62, v71, v115, v125, v127]);
              const [v134, v135] = txn6.data;
              const v139 = txn6.time;
              const v133 = txn6.from;
              
              stdlib.assert(true, {
                at: './tut1.rsh:94:11:dot',
                fs: [],
                msg: null,
                who: 'Alice'
                });
              const v137 = stdlib.add(v127, stdlib.checkedBigNumberify('./tut1.rsh:decimal', stdlib.UInt_max, 0));
              sim_r.txns.push({
                amt: stdlib.checkedBigNumberify('./tut1.rsh:decimal', stdlib.UInt_max, 0),
                kind: 'to',
                tok: undefined
                });
              const v138 = stdlib.addressEq(v60, v133);
              stdlib.assert(v138, {
                at: './tut1.rsh:94:11:dot',
                fs: [],
                msg: 'sender correct',
                who: 'Alice'
                });
              const v141 = stdlib.digest(ctc1, [v134, v135]);
              const v142 = stdlib.digestEq(v115, v141);
              stdlib.assert(v142, {
                at: 'reach standard library:65:17:application',
                fs: ['at ./tut1.rsh:96:24:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)'],
                msg: null,
                who: 'Alice'
                });
              const v145 = stdlib.sub(stdlib.checkedBigNumberify('./tut1.rsh:7:18:decimal', stdlib.UInt_max, 4), v125);
              const v146 = stdlib.add(v135, v145);
              const v147 = stdlib.mod(v146, stdlib.checkedBigNumberify('./tut1.rsh:7:32:decimal', stdlib.UInt_max, 3));
              const cv95 = v147;
              const cv267 = v139;
              const cv269 = v137;
              
              (() => {
                const v95 = cv95;
                const v267 = cv267;
                const v269 = cv269;
                
                if ((() => {
                  const v105 = stdlib.eq(v95, stdlib.checkedBigNumberify('./tut1.rsh:makeEnum', stdlib.UInt_max, 1));
                  
                  return v105;})()) {
                  sim_r.nextSt = stdlib.digest(ctc5, [stdlib.checkedBigNumberify('./tut1.rsh:75:17:after expr stmt semicolon', stdlib.UInt_max, 6), v60, v61, v62, v71, v267, v269]);
                  sim_r.nextSt_noTime = stdlib.digest(ctc6, [stdlib.checkedBigNumberify('./tut1.rsh:75:17:after expr stmt semicolon', stdlib.UInt_max, 6), v60, v61, v62, v71, v269]);
                  sim_r.isHalt = false;
                  }
                else {
                  const v209 = stdlib.eq(v95, stdlib.checkedBigNumberify('./tut1.rsh:makeEnum', stdlib.UInt_max, 2));
                  const v212 = stdlib.mul(stdlib.checkedBigNumberify('./tut1.rsh:102:16:decimal', stdlib.UInt_max, 2), v61);
                  const v214 = v209 ? v60 : v71;
                  sim_r.txns.push({
                    amt: v212,
                    kind: 'from',
                    to: v214,
                    tok: undefined
                    });
                  sim_r.txns.push({
                    kind: 'halt',
                    tok: undefined
                    })
                  sim_r.nextSt = stdlib.digest(ctc7, []);
                  sim_r.nextSt_noTime = stdlib.digest(ctc7, []);
                  sim_r.isHalt = true;
                  }})();
              return sim_r;
              })));
            if (txn6.didTimeout) {
              const txn7 = await (ctc.recv(13, 0, [], false, false));
              const [] = txn7.data;
              const v154 = txn7.time;
              const v150 = txn7.from;
              stdlib.assert(true, {
                at: 'reach standard library:209:7:dot',
                fs: ['at ./tut1.rsh:95:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
                msg: null,
                who: 'Alice'
                });
              const v152 = stdlib.add(v127, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
              ;
              const v153 = stdlib.addressEq(v71, v150);
              stdlib.assert(v153, {
                at: 'reach standard library:209:7:dot',
                fs: ['at ./tut1.rsh:95:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
                msg: 'sender correct',
                who: 'Alice'
                });
              ;
              stdlib.protect(ctc3, await interact.informTimeout(), {
                at: './tut1.rsh:41:33:application',
                fs: ['at ./tut1.rsh:40:13:application call to [unknown function] (defined at: ./tut1.rsh:40:25:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./tut1.rsh:39:32:function exp)', 'at ./tut1.rsh:95:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
                msg: 'informTimeout',
                who: 'Alice'
                });
              return;
              }
            else {
              const [v134, v135] = txn6.data;
              const v139 = txn6.time;
              const v133 = txn6.from;
              stdlib.assert(true, {
                at: './tut1.rsh:94:11:dot',
                fs: [],
                msg: null,
                who: 'Alice'
                });
              const v137 = stdlib.add(v127, stdlib.checkedBigNumberify('./tut1.rsh:decimal', stdlib.UInt_max, 0));
              ;
              const v138 = stdlib.addressEq(v60, v133);
              stdlib.assert(v138, {
                at: './tut1.rsh:94:11:dot',
                fs: [],
                msg: 'sender correct',
                who: 'Alice'
                });
              const v141 = stdlib.digest(ctc1, [v134, v135]);
              const v142 = stdlib.digestEq(v115, v141);
              stdlib.assert(v142, {
                at: 'reach standard library:65:17:application',
                fs: ['at ./tut1.rsh:96:24:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)'],
                msg: null,
                who: 'Alice'
                });
              const v145 = stdlib.sub(stdlib.checkedBigNumberify('./tut1.rsh:7:18:decimal', stdlib.UInt_max, 4), v125);
              const v146 = stdlib.add(v135, v145);
              const v147 = stdlib.mod(v146, stdlib.checkedBigNumberify('./tut1.rsh:7:32:decimal', stdlib.UInt_max, 3));
              const cv95 = v147;
              const cv267 = v139;
              const cv269 = v137;
              
              v95 = cv95;
              v267 = cv267;
              v269 = cv269;
              
              continue;}
            }
          }
        }
      const v209 = stdlib.eq(v95, stdlib.checkedBigNumberify('./tut1.rsh:makeEnum', stdlib.UInt_max, 2));
      const v212 = stdlib.mul(stdlib.checkedBigNumberify('./tut1.rsh:102:16:decimal', stdlib.UInt_max, 2), v61);
      const v214 = v209 ? v60 : v71;
      ;
      stdlib.protect(ctc3, await interact.seeOutcome(v95), {
        at: './tut1.rsh:106:28:application',
        fs: ['at ./tut1.rsh:105:11:application call to [unknown function] (defined at: ./tut1.rsh:105:23:function exp)'],
        msg: 'seeOutcome',
        who: 'Alice'
        });
      return;}
    }
  
  
  };
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
  
  
  const v51 = await ctc.creationTime();
  const txn1 = await (ctc.recv(1, 3, [ctc0, ctc0, ctc1], false, false));
  const [v61, v62, v63] = txn1.data;
  const v66 = txn1.time;
  const v60 = txn1.from;
  stdlib.assert(true, {
    at: './tut1.rsh:51:9:dot',
    fs: [],
    msg: null,
    who: 'Bob'
    });
  const v65 = stdlib.add(stdlib.checkedBigNumberify('./tut1.rsh:compileDApp', stdlib.UInt_max, 0), v61);
  ;
  stdlib.assert(true, {
    at: './tut1.rsh:51:9:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Bob'
    });
  stdlib.protect(ctc2, await interact.acceptWager(v61, v62), {
    at: './tut1.rsh:57:29:application',
    fs: ['at ./tut1.rsh:56:13:application call to [unknown function] (defined at: ./tut1.rsh:56:17:function exp)'],
    msg: 'acceptWager',
    who: 'Bob'
    });
  const v70 = stdlib.protect(ctc0, await interact.getHand(), {
    at: './tut1.rsh:58:55:application',
    fs: ['at ./tut1.rsh:56:13:application call to [unknown function] (defined at: ./tut1.rsh:56:17:function exp)'],
    msg: 'getHand',
    who: 'Bob'
    });
  const txn2 = await (ctc.sendrecv(2, 1, stdlib.checkedBigNumberify('./tut1.rsh:60:9:dot', stdlib.UInt_max, 5), [ctc5, ctc0, ctc0, ctc1, ctc0, ctc0, ctc0], [v60, v61, v62, v63, v65, v66, v70], [v61, []], [ctc0], true, true, v62, (async (txn2) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.digest(ctc13, [stdlib.checkedBigNumberify('./tut1.rsh:60:9:dot', stdlib.UInt_max, 1), v60, v61, v62, v63, v65, v66]);
    sim_r.prevSt_noPrevTime = stdlib.digest(ctc14, [stdlib.checkedBigNumberify('./tut1.rsh:60:9:dot', stdlib.UInt_max, 1), v60, v61, v62, v63, v65]);
    const [v72] = txn2.data;
    const v75 = txn2.time;
    const v71 = txn2.from;
    
    stdlib.assert(true, {
      at: './tut1.rsh:60:9:dot',
      fs: [],
      msg: null,
      who: 'Bob'
      });
    const v74 = stdlib.add(v65, v61);
    sim_r.txns.push({
      amt: v61,
      kind: 'to',
      tok: undefined
      });
    stdlib.assert(true, {
      at: './tut1.rsh:60:9:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Bob'
      });
    sim_r.nextSt = stdlib.digest(ctc11, [stdlib.checkedBigNumberify('./tut1.rsh:63:15:after expr stmt semicolon', stdlib.UInt_max, 2), v60, v61, v62, v63, v71, v72, v74, v75]);
    sim_r.nextSt_noTime = stdlib.digest(ctc12, [stdlib.checkedBigNumberify('./tut1.rsh:63:15:after expr stmt semicolon', stdlib.UInt_max, 2), v60, v61, v62, v63, v71, v72, v74]);
    sim_r.isHalt = false;
    
    return sim_r;
    })));
  if (txn2.didTimeout) {
    const txn3 = await (ctc.recv(3, 0, [], false, false));
    const [] = txn3.data;
    const v252 = txn3.time;
    const v248 = txn3.from;
    stdlib.assert(true, {
      at: 'reach standard library:209:7:dot',
      fs: ['at ./tut1.rsh:62:41:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
      msg: null,
      who: 'Bob'
      });
    const v250 = stdlib.add(v65, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
    ;
    const v251 = stdlib.addressEq(v60, v248);
    stdlib.assert(v251, {
      at: 'reach standard library:209:7:dot',
      fs: ['at ./tut1.rsh:62:41:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
      msg: 'sender correct',
      who: 'Bob'
      });
    ;
    stdlib.protect(ctc2, await interact.informTimeout(), {
      at: './tut1.rsh:41:33:application',
      fs: ['at ./tut1.rsh:40:13:application call to [unknown function] (defined at: ./tut1.rsh:40:25:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./tut1.rsh:39:32:function exp)', 'at ./tut1.rsh:62:41:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
      msg: 'informTimeout',
      who: 'Bob'
      });
    return;
    }
  else {
    const [v72] = txn2.data;
    const v75 = txn2.time;
    const v71 = txn2.from;
    stdlib.assert(true, {
      at: './tut1.rsh:60:9:dot',
      fs: [],
      msg: null,
      who: 'Bob'
      });
    const v74 = stdlib.add(v65, v61);
    ;
    stdlib.assert(true, {
      at: './tut1.rsh:60:9:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Bob'
      });
    const txn3 = await (ctc.recv(4, 2, [ctc0, ctc0], false, v62));
    if (txn3.didTimeout) {
      const txn4 = await (ctc.sendrecv(5, 0, stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 7), [ctc5, ctc0, ctc0, ctc1, ctc5, ctc0, ctc0, ctc0], [v60, v61, v62, v63, v71, v72, v74, v75], [stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), []], [], true, true, false, (async (txn4) => {
        const sim_r = { txns: [] };
        sim_r.prevSt = stdlib.digest(ctc11, [stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 2), v60, v61, v62, v63, v71, v72, v74, v75]);
        sim_r.prevSt_noPrevTime = stdlib.digest(ctc12, [stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 2), v60, v61, v62, v63, v71, v72, v74]);
        const [] = txn4.data;
        const v232 = txn4.time;
        const v228 = txn4.from;
        
        stdlib.assert(true, {
          at: 'reach standard library:209:7:dot',
          fs: ['at ./tut1.rsh:69:40:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
          msg: null,
          who: 'Bob'
          });
        const v230 = stdlib.add(v74, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
        sim_r.txns.push({
          amt: stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0),
          kind: 'to',
          tok: undefined
          });
        const v231 = stdlib.addressEq(v71, v228);
        stdlib.assert(v231, {
          at: 'reach standard library:209:7:dot',
          fs: ['at ./tut1.rsh:69:40:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
          msg: 'sender correct',
          who: 'Bob'
          });
        sim_r.txns.push({
          amt: v230,
          kind: 'from',
          to: v71,
          tok: undefined
          });
        sim_r.txns.push({
          kind: 'halt',
          tok: undefined
          })
        sim_r.nextSt = stdlib.digest(ctc4, []);
        sim_r.nextSt_noTime = stdlib.digest(ctc4, []);
        sim_r.isHalt = true;
        
        return sim_r;
        })));
      const [] = txn4.data;
      const v232 = txn4.time;
      const v228 = txn4.from;
      stdlib.assert(true, {
        at: 'reach standard library:209:7:dot',
        fs: ['at ./tut1.rsh:69:40:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
        msg: null,
        who: 'Bob'
        });
      const v230 = stdlib.add(v74, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
      ;
      const v231 = stdlib.addressEq(v71, v228);
      stdlib.assert(v231, {
        at: 'reach standard library:209:7:dot',
        fs: ['at ./tut1.rsh:69:40:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
        msg: 'sender correct',
        who: 'Bob'
        });
      ;
      stdlib.protect(ctc2, await interact.informTimeout(), {
        at: './tut1.rsh:41:33:application',
        fs: ['at ./tut1.rsh:40:13:application call to [unknown function] (defined at: ./tut1.rsh:40:25:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./tut1.rsh:39:32:function exp)', 'at ./tut1.rsh:69:40:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
        msg: 'informTimeout',
        who: 'Bob'
        });
      return;
      }
    else {
      const [v80, v81] = txn3.data;
      const v85 = txn3.time;
      const v79 = txn3.from;
      stdlib.assert(true, {
        at: './tut1.rsh:68:9:dot',
        fs: [],
        msg: null,
        who: 'Bob'
        });
      const v83 = stdlib.add(v74, stdlib.checkedBigNumberify('./tut1.rsh:decimal', stdlib.UInt_max, 0));
      ;
      const v84 = stdlib.addressEq(v60, v79);
      stdlib.assert(v84, {
        at: './tut1.rsh:68:9:dot',
        fs: [],
        msg: 'sender correct',
        who: 'Bob'
        });
      const v87 = stdlib.digest(ctc3, [v80, v81]);
      const v88 = stdlib.digestEq(v63, v87);
      stdlib.assert(v88, {
        at: 'reach standard library:65:17:application',
        fs: ['at ./tut1.rsh:70:22:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)'],
        msg: null,
        who: 'Bob'
        });
      const v91 = stdlib.sub(stdlib.checkedBigNumberify('./tut1.rsh:7:18:decimal', stdlib.UInt_max, 4), v72);
      const v92 = stdlib.add(v81, v91);
      const v93 = stdlib.mod(v92, stdlib.checkedBigNumberify('./tut1.rsh:7:32:decimal', stdlib.UInt_max, 3));
      let v95 = v93;
      let v267 = v85;
      let v269 = v83;
      
      while ((() => {
        const v105 = stdlib.eq(v95, stdlib.checkedBigNumberify('./tut1.rsh:makeEnum', stdlib.UInt_max, 1));
        
        return v105;})()) {
        const txn4 = await (ctc.recv(8, 1, [ctc1], false, v62));
        if (txn4.didTimeout) {
          const txn5 = await (ctc.sendrecv(9, 0, stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 4), [ctc5, ctc0, ctc0, ctc5, ctc0, ctc0], [v60, v61, v62, v71, v267, v269], [stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), []], [], true, true, false, (async (txn5) => {
            const sim_r = { txns: [] };
            sim_r.prevSt = stdlib.digest(ctc9, [stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 6), v60, v61, v62, v71, v267, v269]);
            sim_r.prevSt_noPrevTime = stdlib.digest(ctc10, [stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 6), v60, v61, v62, v71, v269]);
            const [] = txn5.data;
            const v194 = txn5.time;
            const v190 = txn5.from;
            
            stdlib.assert(true, {
              at: 'reach standard library:209:7:dot',
              fs: ['at ./tut1.rsh:82:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
              msg: null,
              who: 'Bob'
              });
            const v192 = stdlib.add(v269, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
            sim_r.txns.push({
              amt: stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0),
              kind: 'to',
              tok: undefined
              });
            const v193 = stdlib.addressEq(v71, v190);
            stdlib.assert(v193, {
              at: 'reach standard library:209:7:dot',
              fs: ['at ./tut1.rsh:82:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
              msg: 'sender correct',
              who: 'Bob'
              });
            sim_r.txns.push({
              amt: v192,
              kind: 'from',
              to: v71,
              tok: undefined
              });
            sim_r.txns.push({
              kind: 'halt',
              tok: undefined
              })
            sim_r.nextSt = stdlib.digest(ctc4, []);
            sim_r.nextSt_noTime = stdlib.digest(ctc4, []);
            sim_r.isHalt = true;
            
            return sim_r;
            })));
          const [] = txn5.data;
          const v194 = txn5.time;
          const v190 = txn5.from;
          stdlib.assert(true, {
            at: 'reach standard library:209:7:dot',
            fs: ['at ./tut1.rsh:82:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
            msg: null,
            who: 'Bob'
            });
          const v192 = stdlib.add(v269, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
          ;
          const v193 = stdlib.addressEq(v71, v190);
          stdlib.assert(v193, {
            at: 'reach standard library:209:7:dot',
            fs: ['at ./tut1.rsh:82:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
            msg: 'sender correct',
            who: 'Bob'
            });
          ;
          stdlib.protect(ctc2, await interact.informTimeout(), {
            at: './tut1.rsh:41:33:application',
            fs: ['at ./tut1.rsh:40:13:application call to [unknown function] (defined at: ./tut1.rsh:40:25:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./tut1.rsh:39:32:function exp)', 'at ./tut1.rsh:82:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
            msg: 'informTimeout',
            who: 'Bob'
            });
          return;
          }
        else {
          const [v115] = txn4.data;
          const v119 = txn4.time;
          const v114 = txn4.from;
          stdlib.assert(true, {
            at: './tut1.rsh:81:11:dot',
            fs: [],
            msg: null,
            who: 'Bob'
            });
          const v117 = stdlib.add(v269, stdlib.checkedBigNumberify('./tut1.rsh:decimal', stdlib.UInt_max, 0));
          ;
          const v118 = stdlib.addressEq(v60, v114);
          stdlib.assert(v118, {
            at: './tut1.rsh:81:11:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Bob'
            });
          const v123 = stdlib.protect(ctc0, await interact.getHand(), {
            at: './tut1.rsh:87:52:application',
            fs: ['at ./tut1.rsh:86:15:application call to [unknown function] (defined at: ./tut1.rsh:86:19:function exp)'],
            msg: 'getHand',
            who: 'Bob'
            });
          const txn5 = await (ctc.sendrecv(10, 1, stdlib.checkedBigNumberify('./tut1.rsh:88:11:dot', stdlib.UInt_max, 6), [ctc5, ctc0, ctc0, ctc5, ctc1, ctc0, ctc0, ctc0], [v60, v61, v62, v71, v115, v117, v119, v123], [stdlib.checkedBigNumberify('./tut1.rsh:decimal', stdlib.UInt_max, 0), []], [ctc0], true, true, v62, (async (txn5) => {
            const sim_r = { txns: [] };
            sim_r.prevSt = stdlib.digest(ctc7, [stdlib.checkedBigNumberify('./tut1.rsh:88:11:dot', stdlib.UInt_max, 8), v60, v61, v62, v71, v115, v117, v119]);
            sim_r.prevSt_noPrevTime = stdlib.digest(ctc8, [stdlib.checkedBigNumberify('./tut1.rsh:88:11:dot', stdlib.UInt_max, 8), v60, v61, v62, v71, v115, v117]);
            const [v125] = txn5.data;
            const v129 = txn5.time;
            const v124 = txn5.from;
            
            stdlib.assert(true, {
              at: './tut1.rsh:88:11:dot',
              fs: [],
              msg: null,
              who: 'Bob'
              });
            const v127 = stdlib.add(v117, stdlib.checkedBigNumberify('./tut1.rsh:decimal', stdlib.UInt_max, 0));
            sim_r.txns.push({
              amt: stdlib.checkedBigNumberify('./tut1.rsh:decimal', stdlib.UInt_max, 0),
              kind: 'to',
              tok: undefined
              });
            const v128 = stdlib.addressEq(v71, v124);
            stdlib.assert(v128, {
              at: './tut1.rsh:88:11:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Bob'
              });
            sim_r.nextSt = stdlib.digest(ctc6, [stdlib.checkedBigNumberify('./tut1.rsh:90:17:after expr stmt semicolon', stdlib.UInt_max, 10), v60, v61, v62, v71, v115, v125, v127, v129]);
            sim_r.nextSt_noTime = stdlib.digest(ctc7, [stdlib.checkedBigNumberify('./tut1.rsh:90:17:after expr stmt semicolon', stdlib.UInt_max, 10), v60, v61, v62, v71, v115, v125, v127]);
            sim_r.isHalt = false;
            
            return sim_r;
            })));
          if (txn5.didTimeout) {
            const txn6 = await (ctc.recv(11, 0, [], false, false));
            const [] = txn6.data;
            const v174 = txn6.time;
            const v170 = txn6.from;
            stdlib.assert(true, {
              at: 'reach standard library:209:7:dot',
              fs: ['at ./tut1.rsh:89:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
              msg: null,
              who: 'Bob'
              });
            const v172 = stdlib.add(v117, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
            ;
            const v173 = stdlib.addressEq(v60, v170);
            stdlib.assert(v173, {
              at: 'reach standard library:209:7:dot',
              fs: ['at ./tut1.rsh:89:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
              msg: 'sender correct',
              who: 'Bob'
              });
            ;
            stdlib.protect(ctc2, await interact.informTimeout(), {
              at: './tut1.rsh:41:33:application',
              fs: ['at ./tut1.rsh:40:13:application call to [unknown function] (defined at: ./tut1.rsh:40:25:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./tut1.rsh:39:32:function exp)', 'at ./tut1.rsh:89:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
              msg: 'informTimeout',
              who: 'Bob'
              });
            return;
            }
          else {
            const [v125] = txn5.data;
            const v129 = txn5.time;
            const v124 = txn5.from;
            stdlib.assert(true, {
              at: './tut1.rsh:88:11:dot',
              fs: [],
              msg: null,
              who: 'Bob'
              });
            const v127 = stdlib.add(v117, stdlib.checkedBigNumberify('./tut1.rsh:decimal', stdlib.UInt_max, 0));
            ;
            const v128 = stdlib.addressEq(v71, v124);
            stdlib.assert(v128, {
              at: './tut1.rsh:88:11:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Bob'
              });
            const txn6 = await (ctc.recv(12, 2, [ctc0, ctc0], false, v62));
            if (txn6.didTimeout) {
              const txn7 = await (ctc.sendrecv(13, 0, stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 7), [ctc5, ctc0, ctc0, ctc5, ctc1, ctc0, ctc0, ctc0], [v60, v61, v62, v71, v115, v125, v127, v129], [stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), []], [], true, true, false, (async (txn7) => {
                const sim_r = { txns: [] };
                sim_r.prevSt = stdlib.digest(ctc6, [stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 10), v60, v61, v62, v71, v115, v125, v127, v129]);
                sim_r.prevSt_noPrevTime = stdlib.digest(ctc7, [stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 10), v60, v61, v62, v71, v115, v125, v127]);
                const [] = txn7.data;
                const v154 = txn7.time;
                const v150 = txn7.from;
                
                stdlib.assert(true, {
                  at: 'reach standard library:209:7:dot',
                  fs: ['at ./tut1.rsh:95:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
                  msg: null,
                  who: 'Bob'
                  });
                const v152 = stdlib.add(v127, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
                sim_r.txns.push({
                  amt: stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0),
                  kind: 'to',
                  tok: undefined
                  });
                const v153 = stdlib.addressEq(v71, v150);
                stdlib.assert(v153, {
                  at: 'reach standard library:209:7:dot',
                  fs: ['at ./tut1.rsh:95:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
                  msg: 'sender correct',
                  who: 'Bob'
                  });
                sim_r.txns.push({
                  amt: v152,
                  kind: 'from',
                  to: v71,
                  tok: undefined
                  });
                sim_r.txns.push({
                  kind: 'halt',
                  tok: undefined
                  })
                sim_r.nextSt = stdlib.digest(ctc4, []);
                sim_r.nextSt_noTime = stdlib.digest(ctc4, []);
                sim_r.isHalt = true;
                
                return sim_r;
                })));
              const [] = txn7.data;
              const v154 = txn7.time;
              const v150 = txn7.from;
              stdlib.assert(true, {
                at: 'reach standard library:209:7:dot',
                fs: ['at ./tut1.rsh:95:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
                msg: null,
                who: 'Bob'
                });
              const v152 = stdlib.add(v127, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
              ;
              const v153 = stdlib.addressEq(v71, v150);
              stdlib.assert(v153, {
                at: 'reach standard library:209:7:dot',
                fs: ['at ./tut1.rsh:95:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
                msg: 'sender correct',
                who: 'Bob'
                });
              ;
              stdlib.protect(ctc2, await interact.informTimeout(), {
                at: './tut1.rsh:41:33:application',
                fs: ['at ./tut1.rsh:40:13:application call to [unknown function] (defined at: ./tut1.rsh:40:25:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./tut1.rsh:39:32:function exp)', 'at ./tut1.rsh:95:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
                msg: 'informTimeout',
                who: 'Bob'
                });
              return;
              }
            else {
              const [v134, v135] = txn6.data;
              const v139 = txn6.time;
              const v133 = txn6.from;
              stdlib.assert(true, {
                at: './tut1.rsh:94:11:dot',
                fs: [],
                msg: null,
                who: 'Bob'
                });
              const v137 = stdlib.add(v127, stdlib.checkedBigNumberify('./tut1.rsh:decimal', stdlib.UInt_max, 0));
              ;
              const v138 = stdlib.addressEq(v60, v133);
              stdlib.assert(v138, {
                at: './tut1.rsh:94:11:dot',
                fs: [],
                msg: 'sender correct',
                who: 'Bob'
                });
              const v141 = stdlib.digest(ctc3, [v134, v135]);
              const v142 = stdlib.digestEq(v115, v141);
              stdlib.assert(v142, {
                at: 'reach standard library:65:17:application',
                fs: ['at ./tut1.rsh:96:24:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)'],
                msg: null,
                who: 'Bob'
                });
              const v145 = stdlib.sub(stdlib.checkedBigNumberify('./tut1.rsh:7:18:decimal', stdlib.UInt_max, 4), v125);
              const v146 = stdlib.add(v135, v145);
              const v147 = stdlib.mod(v146, stdlib.checkedBigNumberify('./tut1.rsh:7:32:decimal', stdlib.UInt_max, 3));
              const cv95 = v147;
              const cv267 = v139;
              const cv269 = v137;
              
              v95 = cv95;
              v267 = cv267;
              v269 = cv269;
              
              continue;}
            }
          }
        }
      const v209 = stdlib.eq(v95, stdlib.checkedBigNumberify('./tut1.rsh:makeEnum', stdlib.UInt_max, 2));
      const v212 = stdlib.mul(stdlib.checkedBigNumberify('./tut1.rsh:102:16:decimal', stdlib.UInt_max, 2), v61);
      const v214 = v209 ? v60 : v71;
      ;
      stdlib.protect(ctc2, await interact.seeOutcome(v95), {
        at: './tut1.rsh:106:28:application',
        fs: ['at ./tut1.rsh:105:11:application call to [unknown function] (defined at: ./tut1.rsh:105:23:function exp)'],
        msg: 'seeOutcome',
        who: 'Bob'
        });
      return;}
    }
  
  
  };

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
int 3
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
int 3
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
int axfer
dup2
==
||
assert
txn RekeyTo
global ZeroAddress
==
assert
txn GroupIndex
int 3
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
// Nothing
// "./tut1.rsh:51:9:dot"
// "[]"
int 1
assert
int 0
gtxna 0 ApplicationArgs 5
btoi
+
store 255
// CheckPay
// "./tut1.rsh:51:9:dot"
// "[]"
gtxn 3 TypeEnum
int pay
==
assert
gtxn 3 Receiver
byte "{{ContractAddr}}"
==
assert
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
gtxna 0 ApplicationArgs 5
btoi
==
assert
// Just "sender correct"
// "./tut1.rsh:51:9:dot"
// "[]"
int 1
assert
// compute state in HM_Set 1
int 1
itob
gtxn 0 Sender
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
// Nothing
// "./tut1.rsh:60:9:dot"
// "[]"
int 1
assert
gtxna 0 ApplicationArgs 9
btoi
gtxna 0 ApplicationArgs 6
btoi
+
store 255
// CheckPay
// "./tut1.rsh:60:9:dot"
// "[]"
gtxn 3 TypeEnum
int pay
==
assert
gtxn 3 Receiver
byte "{{ContractAddr}}"
==
assert
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
gtxna 0 ApplicationArgs 6
btoi
==
assert
// Just "sender correct"
// "./tut1.rsh:60:9:dot"
// "[]"
int 1
assert
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
gtxn 0 Sender
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
// Nothing
// "reach standard library:209:7:dot"
// "[at ./tut1.rsh:62:41:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
int 1
assert
// CheckPay
// "reach standard library:209:7:dot"
// "[at ./tut1.rsh:62:41:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
gtxn 3 TypeEnum
int pay
==
assert
gtxn 3 Receiver
byte "{{ContractAddr}}"
==
assert
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "reach standard library:209:7:dot"
// "[at ./tut1.rsh:62:41:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
gtxna 0 ApplicationArgs 5
gtxn 0 Sender
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
int 0
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
gtxn 5 CloseRemainderTo
byte "{{Deployer}}"
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
gtxn 4 Fee
gtxn 5 Fee
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
// Nothing
// "./tut1.rsh:68:9:dot"
// "[]"
int 1
assert
gtxna 0 ApplicationArgs 11
btoi
int 0
+
store 255
// CheckPay
// "./tut1.rsh:68:9:dot"
// "[]"
gtxn 3 TypeEnum
int pay
==
assert
gtxn 3 Receiver
byte "{{ContractAddr}}"
==
assert
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "./tut1.rsh:68:9:dot"
// "[]"
gtxna 0 ApplicationArgs 5
gtxn 0 Sender
==
assert
// Nothing
// "reach standard library:65:17:application"
// "[at ./tut1.rsh:70:22:application call to \"checkCommitment\" (defined at: reach standard library:64:8:function exp)]"
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
gtxn 5 CloseRemainderTo
byte "{{Deployer}}"
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
gtxn 4 Fee
gtxn 5 Fee
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
// Nothing
// "reach standard library:209:7:dot"
// "[at ./tut1.rsh:69:40:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
int 1
assert
// CheckPay
// "reach standard library:209:7:dot"
// "[at ./tut1.rsh:69:40:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
gtxn 3 TypeEnum
int pay
==
assert
gtxn 3 Receiver
byte "{{ContractAddr}}"
==
assert
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "reach standard library:209:7:dot"
// "[at ./tut1.rsh:69:40:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
gtxna 0 ApplicationArgs 9
gtxn 0 Sender
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
int 0
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
gtxn 5 CloseRemainderTo
byte "{{Deployer}}"
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
gtxn 4 Fee
gtxn 5 Fee
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
// Nothing
// "./tut1.rsh:81:11:dot"
// "[]"
int 1
assert
gtxna 0 ApplicationArgs 9
btoi
int 0
+
store 255
// CheckPay
// "./tut1.rsh:81:11:dot"
// "[]"
gtxn 3 TypeEnum
int pay
==
assert
gtxn 3 Receiver
byte "{{ContractAddr}}"
==
assert
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "./tut1.rsh:81:11:dot"
// "[]"
gtxna 0 ApplicationArgs 5
gtxn 0 Sender
==
assert
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
// Nothing
// "reach standard library:209:7:dot"
// "[at ./tut1.rsh:82:43:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
int 1
assert
// CheckPay
// "reach standard library:209:7:dot"
// "[at ./tut1.rsh:82:43:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
gtxn 3 TypeEnum
int pay
==
assert
gtxn 3 Receiver
byte "{{ContractAddr}}"
==
assert
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "reach standard library:209:7:dot"
// "[at ./tut1.rsh:82:43:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
gtxna 0 ApplicationArgs 8
gtxn 0 Sender
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
int 0
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
gtxn 5 CloseRemainderTo
byte "{{Deployer}}"
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
gtxn 4 Fee
gtxn 5 Fee
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
// Nothing
// "./tut1.rsh:88:11:dot"
// "[]"
int 1
assert
gtxna 0 ApplicationArgs 10
btoi
int 0
+
store 255
// CheckPay
// "./tut1.rsh:88:11:dot"
// "[]"
gtxn 3 TypeEnum
int pay
==
assert
gtxn 3 Receiver
byte "{{ContractAddr}}"
==
assert
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "./tut1.rsh:88:11:dot"
// "[]"
gtxna 0 ApplicationArgs 8
gtxn 0 Sender
==
assert
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
// Nothing
// "reach standard library:209:7:dot"
// "[at ./tut1.rsh:89:43:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
int 1
assert
// CheckPay
// "reach standard library:209:7:dot"
// "[at ./tut1.rsh:89:43:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
gtxn 3 TypeEnum
int pay
==
assert
gtxn 3 Receiver
byte "{{ContractAddr}}"
==
assert
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "reach standard library:209:7:dot"
// "[at ./tut1.rsh:89:43:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
gtxna 0 ApplicationArgs 5
gtxn 0 Sender
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
int 0
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
gtxn 5 CloseRemainderTo
byte "{{Deployer}}"
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
gtxn 4 Fee
gtxn 5 Fee
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
// Nothing
// "./tut1.rsh:94:11:dot"
// "[]"
int 1
assert
gtxna 0 ApplicationArgs 11
btoi
int 0
+
store 255
// CheckPay
// "./tut1.rsh:94:11:dot"
// "[]"
gtxn 3 TypeEnum
int pay
==
assert
gtxn 3 Receiver
byte "{{ContractAddr}}"
==
assert
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "./tut1.rsh:94:11:dot"
// "[]"
gtxna 0 ApplicationArgs 5
gtxn 0 Sender
==
assert
// Nothing
// "reach standard library:65:17:application"
// "[at ./tut1.rsh:96:24:application call to \"checkCommitment\" (defined at: reach standard library:64:8:function exp)]"
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
gtxn 5 CloseRemainderTo
byte "{{Deployer}}"
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
gtxn 4 Fee
gtxn 5 Fee
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
// Nothing
// "reach standard library:209:7:dot"
// "[at ./tut1.rsh:95:43:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
int 1
assert
// CheckPay
// "reach standard library:209:7:dot"
// "[at ./tut1.rsh:95:43:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
gtxn 3 TypeEnum
int pay
==
assert
gtxn 3 Receiver
byte "{{ContractAddr}}"
==
assert
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "reach standard library:209:7:dot"
// "[at ./tut1.rsh:95:43:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
gtxna 0 ApplicationArgs 8
gtxn 0 Sender
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
int 0
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
gtxn 5 CloseRemainderTo
byte "{{Deployer}}"
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
gtxn 4 Fee
gtxn 5 Fee
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
                "name": "v51",
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
                "name": "v61",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v62",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v63",
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
                "name": "v60",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v61",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v62",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v71",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v115",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v117",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v119",
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
                "name": "v125",
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
                "name": "v60",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v61",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v62",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v71",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v115",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v117",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v119",
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
                "name": "v60",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v61",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v62",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v71",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v115",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v125",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v127",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v129",
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
                "name": "v134",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v135",
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
                "name": "v60",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v61",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v62",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v71",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v115",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v125",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v127",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v129",
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
                "name": "v60",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v61",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v62",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v63",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v65",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v66",
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
                "name": "v72",
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
                "name": "v60",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v61",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v62",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v63",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v65",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v66",
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
                "name": "v60",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v61",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v62",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v63",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v71",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v72",
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
            "internalType": "struct T4",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v80",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v81",
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
                "name": "v60",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v61",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v62",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v63",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v71",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v72",
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
                "name": "v60",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v61",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v62",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v71",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v267",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v269",
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
                "name": "v115",
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
                "name": "v60",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v61",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v62",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v71",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v267",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v269",
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
                "name": "v51",
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
                "name": "v61",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v62",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v63",
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
                "name": "v60",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v61",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v62",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v71",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v115",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v117",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v119",
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
                "name": "v125",
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
                "name": "v60",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v61",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v62",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v71",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v115",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v117",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v119",
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
                "name": "v60",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v61",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v62",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v71",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v115",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v125",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v127",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v129",
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
                "name": "v134",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v135",
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
                "name": "v60",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v61",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v62",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v71",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v115",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v125",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v127",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v129",
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
                "name": "v60",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v61",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v62",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v63",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v65",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v66",
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
                "name": "v72",
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
                "name": "v60",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v61",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v62",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v63",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v65",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v66",
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
                "name": "v60",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v61",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v62",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v63",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v71",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v72",
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
            "internalType": "struct T4",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v80",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v81",
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
                "name": "v60",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v61",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v62",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v63",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v71",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v72",
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
                "name": "v60",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v61",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v62",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v71",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v267",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v269",
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
                "name": "v115",
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
                "name": "v60",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v61",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v62",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v71",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v267",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v269",
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
  Bytecode: `0x608060408190527f49ff028a829527a47ec6839c7147b484eccf5a2a94853eddac09cef44d9d4e9e90600090a16040805160208082018352438252825180820184526000808252925181528351808301849052905181850152835180820385018152606090910190935282519201919091209055611900806100826000396000f3fe6080604052600436106100a05760003560e01c80638f71c4b6116100645780638f71c4b61461010d57806392b91a3114610120578063a437860b14610133578063abb65ccc14610146578063d22bdb5814610159578063d34b6dfc1461016c576100a7565b80632bf4f873146100ac5780632bfd6934146100c1578063571d5205146100d457806360fdfb87146100e757806372f6740b146100fa576100a7565b366100a757005b600080fd5b6100bf6100ba3660046114ce565b61017f565b005b6100bf6100cf36600461147b565b610324565b6100bf6100e2366004611497565b61044b565b6100bf6100f536600461145f565b610563565b6100bf6101083660046114b2565b610724565b6100bf61011b366004611497565b610934565b6100bf61012e36600461145f565b610b1c565b6100bf61014136600461147b565b610c6a565b6100bf610154366004611497565b610d82565b6100bf610167366004611497565b610f71565b6100bf61017a3660046114b2565b611083565b60408051600060208201528235918101919091526060016040516020818303038152906040528051906020012060001c600054146101bc57600080fd5b600080805560408051602081019091529081526101de60208301356000611846565b8152346020830135146101f057600080fd5b60408051833581526020808501359082015283820135818301526060808501359082015290517f2bb570a5feee0f446e450005a048c78efd478914692e1f0be1009bac144b11709181900360800190a16102826040518060c0016040528060006001600160a01b0316815260200160008152602001600081526020016000815260200160008152602001600081525090565b338152602083810135818301908152604080860135818501908152606080880135818701908152875160808089019182524360a0808b01918252875160019a81019a909a528a516001600160a01b0316978a019790975296519388019390935292519186019190915251918401919091525160c08301525160e0820152610100015b60408051601f198184030181529190528051602090910120600055505050565b60405161033890600a90839060200161181c565b6040516020818303038152906040528051906020012060001c6000541461035e57600080fd5b60008055610374604082013560e0830135611846565b4310158015610381575060015b61038a57600080fd5b341561039557600080fd5b336103a6608083016060840161143e565b6001600160a01b0316146103b957600080fd5b6103c9608082016060830161143e565b6001600160a01b03166108fc6103e4600060c0850135611846565b6040518115909202916000818181858888f1935050505015801561040c573d6000803e3d6000fd5b507f69dd1b34aedf23239d695825c063b0ccab540698bc04080155ef5bbab022a1ba8160405161043c91906117b4565b60405180910390a16000805533ff5b60405161045f9060069083906020016117df565b6040516020818303038152906040528051906020012060001c6000541461048557600080fd5b6000805561049b60408201356080830135611846565b43101580156104a8575060015b6104b157600080fd5b34156104bc57600080fd5b336104cd608083016060840161143e565b6001600160a01b0316146104e057600080fd5b6104f0608082016060830161143e565b6001600160a01b03166108fc61050b600060a0850135611846565b6040518115909202916000818181858888f19350505050158015610533573d6000803e3d6000fd5b507fca13e6f8d91ff4ca5d5587cef0f51406a1cbd78ee61e6b614471da01a93eeea28160405161043c9190611727565b60405161057790600a90839060200161181c565b6040516020818303038152906040528051906020012060001c6000541461059d57600080fd5b600080556105b3604082013560e0830135611846565b43106105be57600080fd5b34156105c957600080fd5b336105d7602083018361143e565b6001600160a01b0316146105ea57600080fd5b6040805161010083013560208201526101208301359181019190915260600160408051601f19818403018152919052805160209091012060808201351461063057600080fd5b7f7da4df75c439201a8e39c13804f6f50b438ef5089390c42c43a237b942a7c16c8160405161065f91906117a5565b60405180910390a161066f61137c565b61067c602083018361143e565b81516001600160a01b039091169052805160208084013591015280516040808401359101526106b1608083016060840161143e565b81516001600160a01b0390911660609091015260036106d560a0840135600461187d565b6106e490610120850135611846565b6106ee9190611894565b602080830180519290925290514391015261070e600060c0840135611846565b60208201516040015261072081611195565b5050565b6040516107389060089083906020016117f3565b6040516020818303038152906040528051906020012060001c6000541461075e57600080fd5b6000808055604080516020810182529182526107819083013560c0840135611846565b431061078c57600080fd5b61079b600060a0840135611846565b815234156107a857600080fd5b336107b9608084016060850161143e565b6001600160a01b0316146107cc57600080fd5b7fb5c2fe74458044d02de0cf289cfe60daebe99955f43c74bd817aa32ebd30bb49826040516107fb9190611757565b60405180910390a161085d60405180610100016040528060006001600160a01b03168152602001600081526020016000815260200160006001600160a01b03168152602001600081526020016000815260200160008152602001600081525090565b61086a602084018461143e565b6001600160a01b031681526020808401359082015260408084013590820152610899608084016060850161143e565b6001600160a01b03908116606083810191825260808681013581860190815260e08089013560a0808901918252895160c0808b0191825243858c0190815260408051600a6020808301919091528e518e16828401528e01519a81019a909a528c015197890197909752975190981690860152915194840194909452519282019290925291516101008301525161012082015261014001610304565b604051610948906001908390602001611808565b6040516020818303038152906040528051906020012060001c6000541461096e57600080fd5b6000808055604080516020810182529182526109919083013560a0840135611846565b431061099c57600080fd5b6109ae60208301356080840135611846565b8152346020830135146109c057600080fd5b7f9bfbd5986241222572c780ca3c93186365207930439d70d8812e93d77208b9ec826040516109ef91906117c3565b60405180910390a1610a5160405180610100016040528060006001600160a01b0316815260200160008152602001600081526020016000815260200160006001600160a01b031681526020016000815260200160008152602001600081525090565b610a5e602084018461143e565b6001600160a01b0316815260208084013581830152604080850135818401526060808601359084015233608084015260c08086013560a08501528451908401524360e0840152516103049160029184910160006101208201905083825260018060a01b038084511660208401526020840151604084015260408401516060840152606084015160808401528060808501511660a08401525060a083015160c083015260c083015160e083015260e08301516101008301529392505050565b604051610b30906002908390602001611831565b6040516020818303038152906040528051906020012060001c60005414610b5657600080fd5b60008055610b6c604082013560e0830135611846565b4310610b7757600080fd5b3415610b8257600080fd5b33610b90602083018361143e565b6001600160a01b031614610ba357600080fd5b6040805161010083013560208201526101208301359181019190915260600160408051601f198184030181529190528051602090910120606082013514610be957600080fd5b7f94867f4474e5a69dad1ab334e9b419f7aef4a590534cf35772ef55b613c168e981604051610c1891906116ac565b60405180910390a1610c2861137c565b610c35602083018361143e565b81516001600160a01b039091169052805160208084013591015280516040808401359101526106b160a083016080840161143e565b604051610c7e906002908390602001611831565b6040516020818303038152906040528051906020012060001c60005414610ca457600080fd5b60008055610cba604082013560e0830135611846565b4310158015610cc7575060015b610cd057600080fd5b3415610cdb57600080fd5b33610cec60a083016080840161143e565b6001600160a01b031614610cff57600080fd5b610d0f60a082016080830161143e565b6001600160a01b03166108fc610d2a600060c0850135611846565b6040518115909202916000818181858888f19350505050158015610d52573d6000803e3d6000fd5b507fc292a98c9160ad377062bc69de738d1c79919b1aba82516476823d2be3a085a08160405161043c91906116d8565b604051610d969060069083906020016117df565b6040516020818303038152906040528051906020012060001c60005414610dbc57600080fd5b600080805560408051602081018252918252610ddf908301356080840135611846565b4310610dea57600080fd5b610df9600060a0840135611846565b81523415610e0657600080fd5b33610e14602084018461143e565b6001600160a01b031614610e2757600080fd5b7fdaaf5830bac0927d0eb7e607cca3bafc032c01e985c2937f25e78351cc41ccc382604051610e56919061170b565b60405180910390a1610eb06040518060e0016040528060006001600160a01b03168152602001600081526020016000815260200160006001600160a01b031681526020016000815260200160008152602001600081525090565b610ebd602084018461143e565b6001600160a01b031681526020808401359082015260408084013590820152610eec608084016060850161143e565b6001600160a01b03908116606083810191825260c0868101356080808701918252875160a080890191825243858a019081526040805160086020808301919091528c518c16828401528c0151988101989098528a0151938701939093529551909616948401949094525190820152915160e08301525161010082015261012001610304565b604051610f85906001908390602001611808565b6040516020818303038152906040528051906020012060001c60005414610fab57600080fd5b60008055610fc1604082013560a0830135611846565b4310158015610fce575060015b610fd757600080fd5b3415610fe257600080fd5b33610ff0602083018361143e565b6001600160a01b03161461100357600080fd5b611010602082018261143e565b6001600160a01b03166108fc61102b60006080850135611846565b6040518115909202916000818181858888f19350505050158015611053573d6000803e3d6000fd5b507f17040e3ed853a8df776cd092f1357f15488d98d460f66cd5e6b0cb07d5bc8ae38160405161043c91906117d1565b6040516110979060089083906020016117f3565b6040516020818303038152906040528051906020012060001c600054146110bd57600080fd5b600080556110d3604082013560c0830135611846565b43101580156110e0575060015b6110e957600080fd5b34156110f457600080fd5b33611102602083018361143e565b6001600160a01b03161461111557600080fd5b611122602082018261143e565b6001600160a01b03166108fc61113d600060a0850135611846565b6040518115909202916000818181858888f19350505050158015611165573d6000803e3d6000fd5b507f3ed1473309a228b1330dadeda04661997e11292de069751738dd383ff44916b48160405161043c9190611774565b602081015151600114156112a3576111ee6040518060c0016040528060006001600160a01b03168152602001600081526020016000815260200160006001600160a01b0316815260200160008152602001600081525090565b8151516001600160a01b039081168252825160209081015181840190815284516040908101518186019081528651606090810151861681880190815285890180518701516080808b01918252915186015160a0808c0191825287516006818c01529b518b168c8901529751948b01949094529351908901525190951692860192909252905160c0850152915160e08085019190915282518085039091018152610100909301909152815191012060005561130c565b6040805160c081018252600091810182815260608083018481526080840185815260a085018681528486526020808701979097528751516001600160a01b039081169095528751870151909252865190920151909216905291830151519091526107208161130f565b50565b8051606001516002146113275780516040015161132b565b8051515b6001600160a01b03166108fc826000015160200151600261134c919061185e565b6040518115909202916000818181858888f19350505050158015611374573d6000803e3d6000fd5b506000805533ff5b6040805160c0810182526000918101828152606082018390526080820183905260a082019290925290819081526020016113d060405180606001604052806000815260200160008152602001600081525090565b905290565b80356001600160a01b03811681146113ec57600080fd5b919050565b60006101408284031215611403578081fd5b50919050565b60006101208284031215611403578081fd5b600060e08284031215611403578081fd5b60006101008284031215611403578081fd5b60006020828403121561144f578081fd5b611458826113d5565b9392505050565b60006101408284031215611471578081fd5b61145883836113f1565b6000610120828403121561148d578081fd5b6114588383611409565b600060e082840312156114a8578081fd5b611458838361141b565b600061010082840312156114c4578081fd5b611458838361142c565b600060808284031215611403578081fd5b6001600160a01b03806114f1836113d5565b168352602082013560208401526040820135604084015280611515606084016113d5565b166060840152506080818101359083015260a090810135910152565b6001600160a01b0380611543836113d5565b168352602082013560208401526040820135604084015280611567606084016113d5565b166060840152506080810135608083015260a081013560a083015260c081013560c08301525050565b6001600160a01b036115a1826113d5565b1682526020810135602083015260408101356040830152606081013560608301526080810135608083015260a081013560a08301525050565b6001600160a01b03806115ec836113d5565b168352602082013560208401526040820135604084015280611610606084016113d5565b166060840152506080810135608083015260a081013560a083015260c081013560c083015260e081013560e08301525050565b6001600160a01b0380611655836113d5565b16835260208201356020840152604082013560408401526060820135606084015280611683608084016113d5565b1660808401525060a081013560a083015260c081013560c083015260e081013560e08301525050565b61014081016116bb8284611643565b610100838101358382015261012080850135908401525092915050565b61012081016116e78284611643565b610100808401358015158082146116fd57600080fd5b808386015250505092915050565b60e0810161171982846114df565b60c092830135919092015290565b60e0810161173582846114df565b60c083013580151580821461174957600080fd5b8060c0850152505092915050565b61010081016117668284611531565b60e092830135919092015290565b61010081016117838284611531565b60e083013580151580821461179757600080fd5b8060e0850152505092915050565b61014081016116bb82846115da565b61012081016116e782846115da565b60e081016117198284611590565b60e081016117358284611590565b82815260e0810161145860208301846114df565b82815261010081016114586020830184611531565b82815260e081016114586020830184611590565b828152610120810161145860208301846115da565b82815261012081016114586020830184611643565b60008219821115611859576118596118b4565b500190565b6000816000190483118215151615611878576118786118b4565b500290565b60008282101561188f5761188f6118b4565b500390565b6000826118af57634e487b7160e01b81526012600452602481fd5b500690565b634e487b7160e01b600052601160045260246000fdfea264697066735822122041dee6eb6a2d64a66b5a21d0f268ba630554732a40a69c5b3bb38dfb6b410ec964736f6c63430008020033`,
  deployMode: `DM_constructor`
  };

export const _Connectors = {
  ALGO: _ALGO,
  ETH: _ETH
  };

