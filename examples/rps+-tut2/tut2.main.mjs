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
  const ctc1 = stdlib.T_Array(ctc0, stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, 5));
  const ctc2 = stdlib.T_Tuple([ctc0, ctc1]);
  const ctc3 = stdlib.T_Digest;
  const ctc4 = stdlib.T_Null;
  const ctc5 = stdlib.T_Address;
  const ctc6 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc0, ctc5, ctc0, ctc0]);
  const ctc7 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc0, ctc5, ctc0]);
  const ctc8 = stdlib.T_Tuple([]);
  const ctc9 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc0, ctc5, ctc3, ctc1, ctc0, ctc0]);
  const ctc10 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc0, ctc5, ctc3, ctc1, ctc0]);
  const ctc11 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc0, ctc5, ctc3, ctc0, ctc0]);
  const ctc12 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc0, ctc5, ctc3, ctc0]);
  const ctc13 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc0, ctc3, ctc5, ctc1, ctc0, ctc0]);
  const ctc14 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc0, ctc3, ctc5, ctc1, ctc0]);
  const ctc15 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc0, ctc3, ctc0, ctc0]);
  const ctc16 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc0, ctc3, ctc0]);
  const ctc17 = stdlib.T_Tuple([ctc0, ctc0]);
  const ctc18 = stdlib.T_Tuple([ctc0]);
  
  
  const v51 = await ctc.creationTime();
  const v49 = stdlib.protect(ctc0, interact.DEADLINE, null);
  const v50 = stdlib.protect(ctc0, interact.wager, null);
  const v55 = stdlib.protect(ctc1, await interact.getBatch(), {
    at: './tut2.rsh:72:47:application',
    fs: ['at ./tut2.rsh:69:13:application call to [unknown function] (defined at: ./tut2.rsh:69:17:function exp)'],
    msg: 'getBatch',
    who: 'Alice'
    });
  const v57 = stdlib.protect(ctc0, await interact.random(), {
    at: 'reach standard library:60:31:application',
    fs: ['at ./tut2.rsh:73:74:application call to "makeCommitment" (defined at: reach standard library:59:8:function exp)', 'at ./tut2.rsh:69:13:application call to [unknown function] (defined at: ./tut2.rsh:69:17:function exp)'],
    msg: 'random',
    who: 'Alice'
    });
  const v58 = stdlib.digest(ctc2, [v57, v55]);
  const txn1 = await (ctc.sendrecv(1, 3, stdlib.checkedBigNumberify('./tut2.rsh:77:9:dot', stdlib.UInt_max, 0), [ctc0, ctc0, ctc0, ctc3], [v51, v50, v49, v58], [v50, []], [ctc0, ctc0, ctc3], true, true, false, (async (txn1) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.digest(ctc17, [stdlib.checkedBigNumberify('./tut2.rsh:77:9:dot', stdlib.UInt_max, 0), v51]);
    sim_r.prevSt_noPrevTime = stdlib.digest(ctc18, [stdlib.checkedBigNumberify('./tut2.rsh:77:9:dot', stdlib.UInt_max, 0)]);
    const [v61, v62, v63] = txn1.data;
    const v66 = txn1.time;
    const v60 = txn1.from;
    
    stdlib.assert(true, {
      at: './tut2.rsh:77:9:dot',
      fs: [],
      msg: null,
      who: 'Alice'
      });
    const v65 = stdlib.add(stdlib.checkedBigNumberify('./tut2.rsh:compileDApp', stdlib.UInt_max, 0), v61);
    sim_r.txns.push({
      amt: v61,
      kind: 'to',
      tok: undefined
      });
    stdlib.assert(true, {
      at: './tut2.rsh:77:9:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Alice'
      });
    sim_r.nextSt = stdlib.digest(ctc15, [stdlib.checkedBigNumberify('./tut2.rsh:79:15:after expr stmt semicolon', stdlib.UInt_max, 1), v60, v61, v62, v63, v65, v66]);
    sim_r.nextSt_noTime = stdlib.digest(ctc16, [stdlib.checkedBigNumberify('./tut2.rsh:79:15:after expr stmt semicolon', stdlib.UInt_max, 1), v60, v61, v62, v63, v65]);
    sim_r.isHalt = false;
    
    return sim_r;
    })));
  const [v61, v62, v63] = txn1.data;
  const v66 = txn1.time;
  const v60 = txn1.from;
  stdlib.assert(true, {
    at: './tut2.rsh:77:9:dot',
    fs: [],
    msg: null,
    who: 'Alice'
    });
  const v65 = stdlib.add(stdlib.checkedBigNumberify('./tut2.rsh:compileDApp', stdlib.UInt_max, 0), v61);
  ;
  stdlib.assert(true, {
    at: './tut2.rsh:77:9:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Alice'
    });
  const txn2 = await (ctc.recv(2, 1, [ctc1], false, v62));
  if (txn2.didTimeout) {
    const txn3 = await (ctc.sendrecv(3, 0, stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 5), [ctc5, ctc0, ctc0, ctc3, ctc0, ctc0], [v60, v61, v62, v63, v65, v66], [stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), []], [], true, true, false, (async (txn3) => {
      const sim_r = { txns: [] };
      sim_r.prevSt = stdlib.digest(ctc15, [stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 1), v60, v61, v62, v63, v65, v66]);
      sim_r.prevSt_noPrevTime = stdlib.digest(ctc16, [stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 1), v60, v61, v62, v63, v65]);
      const [] = txn3.data;
      const v412 = txn3.time;
      const v408 = txn3.from;
      
      stdlib.assert(true, {
        at: 'reach standard library:209:7:dot',
        fs: ['at ./tut2.rsh:88:41:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
        msg: null,
        who: 'Alice'
        });
      const v410 = stdlib.add(v65, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
      sim_r.txns.push({
        amt: stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0),
        kind: 'to',
        tok: undefined
        });
      const v411 = stdlib.addressEq(v60, v408);
      stdlib.assert(v411, {
        at: 'reach standard library:209:7:dot',
        fs: ['at ./tut2.rsh:88:41:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
        msg: 'sender correct',
        who: 'Alice'
        });
      sim_r.txns.push({
        amt: v410,
        kind: 'from',
        to: v60,
        tok: undefined
        });
      sim_r.txns.push({
        kind: 'halt',
        tok: undefined
        })
      sim_r.nextSt = stdlib.digest(ctc8, []);
      sim_r.nextSt_noTime = stdlib.digest(ctc8, []);
      sim_r.isHalt = true;
      
      return sim_r;
      })));
    const [] = txn3.data;
    const v412 = txn3.time;
    const v408 = txn3.from;
    stdlib.assert(true, {
      at: 'reach standard library:209:7:dot',
      fs: ['at ./tut2.rsh:88:41:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
      msg: null,
      who: 'Alice'
      });
    const v410 = stdlib.add(v65, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
    ;
    const v411 = stdlib.addressEq(v60, v408);
    stdlib.assert(v411, {
      at: 'reach standard library:209:7:dot',
      fs: ['at ./tut2.rsh:88:41:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
      msg: 'sender correct',
      who: 'Alice'
      });
    ;
    stdlib.protect(ctc4, await interact.informTimeout(), {
      at: './tut2.rsh:67:33:application',
      fs: ['at ./tut2.rsh:66:13:application call to [unknown function] (defined at: ./tut2.rsh:66:25:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./tut2.rsh:65:32:function exp)', 'at ./tut2.rsh:88:41:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
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
      at: './tut2.rsh:86:9:dot',
      fs: [],
      msg: null,
      who: 'Alice'
      });
    const v74 = stdlib.add(v65, v61);
    ;
    stdlib.assert(true, {
      at: './tut2.rsh:86:9:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Alice'
      });
    const txn3 = await (ctc.sendrecv(4, 2, stdlib.checkedBigNumberify('./tut2.rsh:94:9:dot', stdlib.UInt_max, 7), [ctc5, ctc0, ctc0, ctc3, ctc5, ctc1, ctc0, ctc0, ctc0, ctc1], [v60, v61, v62, v63, v71, v72, v74, v75, v57, v55], [stdlib.checkedBigNumberify('./tut2.rsh:decimal', stdlib.UInt_max, 0), []], [ctc0, ctc1], true, true, v62, (async (txn3) => {
      const sim_r = { txns: [] };
      sim_r.prevSt = stdlib.digest(ctc13, [stdlib.checkedBigNumberify('./tut2.rsh:94:9:dot', stdlib.UInt_max, 2), v60, v61, v62, v63, v71, v72, v74, v75]);
      sim_r.prevSt_noPrevTime = stdlib.digest(ctc14, [stdlib.checkedBigNumberify('./tut2.rsh:94:9:dot', stdlib.UInt_max, 2), v60, v61, v62, v63, v71, v72, v74]);
      const [v80, v81] = txn3.data;
      const v85 = txn3.time;
      const v79 = txn3.from;
      
      stdlib.assert(true, {
        at: './tut2.rsh:94:9:dot',
        fs: [],
        msg: null,
        who: 'Alice'
        });
      const v83 = stdlib.add(v74, stdlib.checkedBigNumberify('./tut2.rsh:decimal', stdlib.UInt_max, 0));
      sim_r.txns.push({
        amt: stdlib.checkedBigNumberify('./tut2.rsh:decimal', stdlib.UInt_max, 0),
        kind: 'to',
        tok: undefined
        });
      const v84 = stdlib.addressEq(v60, v79);
      stdlib.assert(v84, {
        at: './tut2.rsh:94:9:dot',
        fs: [],
        msg: 'sender correct',
        who: 'Alice'
        });
      const v87 = stdlib.digest(ctc2, [v80, v81]);
      const v88 = stdlib.digestEq(v63, v87);
      stdlib.assert(v88, {
        at: 'reach standard library:65:17:application',
        fs: ['at ./tut2.rsh:96:22:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)'],
        msg: null,
        who: 'Alice'
        });
      const v105 = v81[stdlib.checkedBigNumberify('./tut2.rsh:39:18:array ref', stdlib.UInt_max, 0)];
      const v106 = v72[stdlib.checkedBigNumberify('./tut2.rsh:39:29:array ref', stdlib.UInt_max, 0)];
      const v108 = stdlib.sub(stdlib.checkedBigNumberify('./tut2.rsh:7:18:decimal', stdlib.UInt_max, 4), v106);
      const v109 = stdlib.add(v105, v108);
      const v110 = stdlib.mod(v109, stdlib.checkedBigNumberify('./tut2.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v114 = v81[stdlib.checkedBigNumberify('./tut2.rsh:39:18:array ref', stdlib.UInt_max, 1)];
      const v115 = v72[stdlib.checkedBigNumberify('./tut2.rsh:39:29:array ref', stdlib.UInt_max, 1)];
      const v117 = stdlib.sub(stdlib.checkedBigNumberify('./tut2.rsh:7:18:decimal', stdlib.UInt_max, 4), v115);
      const v118 = stdlib.add(v114, v117);
      const v119 = stdlib.mod(v118, stdlib.checkedBigNumberify('./tut2.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v123 = v81[stdlib.checkedBigNumberify('./tut2.rsh:39:18:array ref', stdlib.UInt_max, 2)];
      const v124 = v72[stdlib.checkedBigNumberify('./tut2.rsh:39:29:array ref', stdlib.UInt_max, 2)];
      const v126 = stdlib.sub(stdlib.checkedBigNumberify('./tut2.rsh:7:18:decimal', stdlib.UInt_max, 4), v124);
      const v127 = stdlib.add(v123, v126);
      const v128 = stdlib.mod(v127, stdlib.checkedBigNumberify('./tut2.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v132 = v81[stdlib.checkedBigNumberify('./tut2.rsh:39:18:array ref', stdlib.UInt_max, 3)];
      const v133 = v72[stdlib.checkedBigNumberify('./tut2.rsh:39:29:array ref', stdlib.UInt_max, 3)];
      const v135 = stdlib.sub(stdlib.checkedBigNumberify('./tut2.rsh:7:18:decimal', stdlib.UInt_max, 4), v133);
      const v136 = stdlib.add(v132, v135);
      const v137 = stdlib.mod(v136, stdlib.checkedBigNumberify('./tut2.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v141 = v81[stdlib.checkedBigNumberify('./tut2.rsh:39:18:array ref', stdlib.UInt_max, 4)];
      const v142 = v72[stdlib.checkedBigNumberify('./tut2.rsh:39:29:array ref', stdlib.UInt_max, 4)];
      const v144 = stdlib.sub(stdlib.checkedBigNumberify('./tut2.rsh:7:18:decimal', stdlib.UInt_max, 4), v142);
      const v145 = stdlib.add(v141, v144);
      const v146 = stdlib.mod(v145, stdlib.checkedBigNumberify('./tut2.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v159 = stdlib.eq(v110, stdlib.checkedBigNumberify('./tut2.rsh:makeEnum', stdlib.UInt_max, 1));
      const v160 = v159 ? v119 : v110;
      const v163 = stdlib.eq(v160, stdlib.checkedBigNumberify('./tut2.rsh:makeEnum', stdlib.UInt_max, 1));
      const v164 = v163 ? v128 : v160;
      const v167 = stdlib.eq(v164, stdlib.checkedBigNumberify('./tut2.rsh:makeEnum', stdlib.UInt_max, 1));
      const v168 = v167 ? v137 : v164;
      const v171 = stdlib.eq(v168, stdlib.checkedBigNumberify('./tut2.rsh:makeEnum', stdlib.UInt_max, 1));
      const v172 = v171 ? v146 : v168;
      const v175 = v172;
      const v427 = v85;
      const v429 = v83;
      
      if ((() => {
        const v185 = stdlib.eq(v175, stdlib.checkedBigNumberify('./tut2.rsh:makeEnum', stdlib.UInt_max, 1));
        
        return v185;})()) {
        sim_r.nextSt = stdlib.digest(ctc6, [stdlib.checkedBigNumberify('./tut2.rsh:101:17:after expr stmt semicolon', stdlib.UInt_max, 6), v60, v61, v62, v71, v427, v429]);
        sim_r.nextSt_noTime = stdlib.digest(ctc7, [stdlib.checkedBigNumberify('./tut2.rsh:101:17:after expr stmt semicolon', stdlib.UInt_max, 6), v60, v61, v62, v71, v429]);
        sim_r.isHalt = false;
        }
      else {
        const v369 = stdlib.eq(v175, stdlib.checkedBigNumberify('./tut2.rsh:makeEnum', stdlib.UInt_max, 2));
        const v372 = stdlib.mul(stdlib.checkedBigNumberify('./tut2.rsh:128:16:decimal', stdlib.UInt_max, 2), v61);
        const v374 = v369 ? v60 : v71;
        sim_r.txns.push({
          amt: v372,
          kind: 'from',
          to: v374,
          tok: undefined
          });
        sim_r.txns.push({
          kind: 'halt',
          tok: undefined
          })
        sim_r.nextSt = stdlib.digest(ctc8, []);
        sim_r.nextSt_noTime = stdlib.digest(ctc8, []);
        sim_r.isHalt = true;
        }
      return sim_r;
      })));
    if (txn3.didTimeout) {
      const txn4 = await (ctc.recv(5, 0, [], false, false));
      const [] = txn4.data;
      const v392 = txn4.time;
      const v388 = txn4.from;
      stdlib.assert(true, {
        at: 'reach standard library:209:7:dot',
        fs: ['at ./tut2.rsh:95:40:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
        msg: null,
        who: 'Alice'
        });
      const v390 = stdlib.add(v74, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
      ;
      const v391 = stdlib.addressEq(v71, v388);
      stdlib.assert(v391, {
        at: 'reach standard library:209:7:dot',
        fs: ['at ./tut2.rsh:95:40:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
        msg: 'sender correct',
        who: 'Alice'
        });
      ;
      stdlib.protect(ctc4, await interact.informTimeout(), {
        at: './tut2.rsh:67:33:application',
        fs: ['at ./tut2.rsh:66:13:application call to [unknown function] (defined at: ./tut2.rsh:66:25:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./tut2.rsh:65:32:function exp)', 'at ./tut2.rsh:95:40:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
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
        at: './tut2.rsh:94:9:dot',
        fs: [],
        msg: null,
        who: 'Alice'
        });
      const v83 = stdlib.add(v74, stdlib.checkedBigNumberify('./tut2.rsh:decimal', stdlib.UInt_max, 0));
      ;
      const v84 = stdlib.addressEq(v60, v79);
      stdlib.assert(v84, {
        at: './tut2.rsh:94:9:dot',
        fs: [],
        msg: 'sender correct',
        who: 'Alice'
        });
      const v87 = stdlib.digest(ctc2, [v80, v81]);
      const v88 = stdlib.digestEq(v63, v87);
      stdlib.assert(v88, {
        at: 'reach standard library:65:17:application',
        fs: ['at ./tut2.rsh:96:22:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)'],
        msg: null,
        who: 'Alice'
        });
      const v105 = v81[stdlib.checkedBigNumberify('./tut2.rsh:39:18:array ref', stdlib.UInt_max, 0)];
      const v106 = v72[stdlib.checkedBigNumberify('./tut2.rsh:39:29:array ref', stdlib.UInt_max, 0)];
      const v108 = stdlib.sub(stdlib.checkedBigNumberify('./tut2.rsh:7:18:decimal', stdlib.UInt_max, 4), v106);
      const v109 = stdlib.add(v105, v108);
      const v110 = stdlib.mod(v109, stdlib.checkedBigNumberify('./tut2.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v114 = v81[stdlib.checkedBigNumberify('./tut2.rsh:39:18:array ref', stdlib.UInt_max, 1)];
      const v115 = v72[stdlib.checkedBigNumberify('./tut2.rsh:39:29:array ref', stdlib.UInt_max, 1)];
      const v117 = stdlib.sub(stdlib.checkedBigNumberify('./tut2.rsh:7:18:decimal', stdlib.UInt_max, 4), v115);
      const v118 = stdlib.add(v114, v117);
      const v119 = stdlib.mod(v118, stdlib.checkedBigNumberify('./tut2.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v123 = v81[stdlib.checkedBigNumberify('./tut2.rsh:39:18:array ref', stdlib.UInt_max, 2)];
      const v124 = v72[stdlib.checkedBigNumberify('./tut2.rsh:39:29:array ref', stdlib.UInt_max, 2)];
      const v126 = stdlib.sub(stdlib.checkedBigNumberify('./tut2.rsh:7:18:decimal', stdlib.UInt_max, 4), v124);
      const v127 = stdlib.add(v123, v126);
      const v128 = stdlib.mod(v127, stdlib.checkedBigNumberify('./tut2.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v132 = v81[stdlib.checkedBigNumberify('./tut2.rsh:39:18:array ref', stdlib.UInt_max, 3)];
      const v133 = v72[stdlib.checkedBigNumberify('./tut2.rsh:39:29:array ref', stdlib.UInt_max, 3)];
      const v135 = stdlib.sub(stdlib.checkedBigNumberify('./tut2.rsh:7:18:decimal', stdlib.UInt_max, 4), v133);
      const v136 = stdlib.add(v132, v135);
      const v137 = stdlib.mod(v136, stdlib.checkedBigNumberify('./tut2.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v141 = v81[stdlib.checkedBigNumberify('./tut2.rsh:39:18:array ref', stdlib.UInt_max, 4)];
      const v142 = v72[stdlib.checkedBigNumberify('./tut2.rsh:39:29:array ref', stdlib.UInt_max, 4)];
      const v144 = stdlib.sub(stdlib.checkedBigNumberify('./tut2.rsh:7:18:decimal', stdlib.UInt_max, 4), v142);
      const v145 = stdlib.add(v141, v144);
      const v146 = stdlib.mod(v145, stdlib.checkedBigNumberify('./tut2.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v159 = stdlib.eq(v110, stdlib.checkedBigNumberify('./tut2.rsh:makeEnum', stdlib.UInt_max, 1));
      const v160 = v159 ? v119 : v110;
      const v163 = stdlib.eq(v160, stdlib.checkedBigNumberify('./tut2.rsh:makeEnum', stdlib.UInt_max, 1));
      const v164 = v163 ? v128 : v160;
      const v167 = stdlib.eq(v164, stdlib.checkedBigNumberify('./tut2.rsh:makeEnum', stdlib.UInt_max, 1));
      const v168 = v167 ? v137 : v164;
      const v171 = stdlib.eq(v168, stdlib.checkedBigNumberify('./tut2.rsh:makeEnum', stdlib.UInt_max, 1));
      const v172 = v171 ? v146 : v168;
      let v175 = v172;
      let v427 = v85;
      let v429 = v83;
      
      while ((() => {
        const v185 = stdlib.eq(v175, stdlib.checkedBigNumberify('./tut2.rsh:makeEnum', stdlib.UInt_max, 1));
        
        return v185;})()) {
        const v189 = stdlib.protect(ctc1, await interact.getBatch(), {
          at: './tut2.rsh:104:44:application',
          fs: ['at ./tut2.rsh:103:15:application call to [unknown function] (defined at: ./tut2.rsh:103:19:function exp)'],
          msg: 'getBatch',
          who: 'Alice'
          });
        const v191 = stdlib.protect(ctc0, await interact.random(), {
          at: 'reach standard library:60:31:application',
          fs: ['at ./tut2.rsh:105:52:application call to "makeCommitment" (defined at: reach standard library:59:8:function exp)', 'at ./tut2.rsh:103:15:application call to [unknown function] (defined at: ./tut2.rsh:103:19:function exp)'],
          msg: 'random',
          who: 'Alice'
          });
        const v192 = stdlib.digest(ctc2, [v191, v189]);
        const txn4 = await (ctc.sendrecv(8, 1, stdlib.checkedBigNumberify('./tut2.rsh:107:11:dot', stdlib.UInt_max, 4), [ctc5, ctc0, ctc0, ctc5, ctc0, ctc0, ctc3], [v60, v61, v62, v71, v427, v429, v192], [stdlib.checkedBigNumberify('./tut2.rsh:decimal', stdlib.UInt_max, 0), []], [ctc3], true, true, v62, (async (txn4) => {
          const sim_r = { txns: [] };
          sim_r.prevSt = stdlib.digest(ctc6, [stdlib.checkedBigNumberify('./tut2.rsh:107:11:dot', stdlib.UInt_max, 6), v60, v61, v62, v71, v427, v429]);
          sim_r.prevSt_noPrevTime = stdlib.digest(ctc7, [stdlib.checkedBigNumberify('./tut2.rsh:107:11:dot', stdlib.UInt_max, 6), v60, v61, v62, v71, v429]);
          const [v195] = txn4.data;
          const v199 = txn4.time;
          const v194 = txn4.from;
          
          stdlib.assert(true, {
            at: './tut2.rsh:107:11:dot',
            fs: [],
            msg: null,
            who: 'Alice'
            });
          const v197 = stdlib.add(v429, stdlib.checkedBigNumberify('./tut2.rsh:decimal', stdlib.UInt_max, 0));
          sim_r.txns.push({
            amt: stdlib.checkedBigNumberify('./tut2.rsh:decimal', stdlib.UInt_max, 0),
            kind: 'to',
            tok: undefined
            });
          const v198 = stdlib.addressEq(v60, v194);
          stdlib.assert(v198, {
            at: './tut2.rsh:107:11:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Alice'
            });
          sim_r.nextSt = stdlib.digest(ctc11, [stdlib.checkedBigNumberify('./tut2.rsh:109:17:after expr stmt semicolon', stdlib.UInt_max, 8), v60, v61, v62, v71, v195, v197, v199]);
          sim_r.nextSt_noTime = stdlib.digest(ctc12, [stdlib.checkedBigNumberify('./tut2.rsh:109:17:after expr stmt semicolon', stdlib.UInt_max, 8), v60, v61, v62, v71, v195, v197]);
          sim_r.isHalt = false;
          
          return sim_r;
          })));
        if (txn4.didTimeout) {
          const txn5 = await (ctc.recv(9, 0, [], false, false));
          const [] = txn5.data;
          const v354 = txn5.time;
          const v350 = txn5.from;
          stdlib.assert(true, {
            at: 'reach standard library:209:7:dot',
            fs: ['at ./tut2.rsh:108:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
            msg: null,
            who: 'Alice'
            });
          const v352 = stdlib.add(v429, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
          ;
          const v353 = stdlib.addressEq(v71, v350);
          stdlib.assert(v353, {
            at: 'reach standard library:209:7:dot',
            fs: ['at ./tut2.rsh:108:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
            msg: 'sender correct',
            who: 'Alice'
            });
          ;
          stdlib.protect(ctc4, await interact.informTimeout(), {
            at: './tut2.rsh:67:33:application',
            fs: ['at ./tut2.rsh:66:13:application call to [unknown function] (defined at: ./tut2.rsh:66:25:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./tut2.rsh:65:32:function exp)', 'at ./tut2.rsh:108:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
            msg: 'informTimeout',
            who: 'Alice'
            });
          return;
          }
        else {
          const [v195] = txn4.data;
          const v199 = txn4.time;
          const v194 = txn4.from;
          stdlib.assert(true, {
            at: './tut2.rsh:107:11:dot',
            fs: [],
            msg: null,
            who: 'Alice'
            });
          const v197 = stdlib.add(v429, stdlib.checkedBigNumberify('./tut2.rsh:decimal', stdlib.UInt_max, 0));
          ;
          const v198 = stdlib.addressEq(v60, v194);
          stdlib.assert(v198, {
            at: './tut2.rsh:107:11:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Alice'
            });
          const txn5 = await (ctc.recv(10, 1, [ctc1], false, v62));
          if (txn5.didTimeout) {
            const txn6 = await (ctc.sendrecv(11, 0, stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 6), [ctc5, ctc0, ctc0, ctc5, ctc3, ctc0, ctc0], [v60, v61, v62, v71, v195, v197, v199], [stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), []], [], true, true, false, (async (txn6) => {
              const sim_r = { txns: [] };
              sim_r.prevSt = stdlib.digest(ctc11, [stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 8), v60, v61, v62, v71, v195, v197, v199]);
              sim_r.prevSt_noPrevTime = stdlib.digest(ctc12, [stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 8), v60, v61, v62, v71, v195, v197]);
              const [] = txn6.data;
              const v334 = txn6.time;
              const v330 = txn6.from;
              
              stdlib.assert(true, {
                at: 'reach standard library:209:7:dot',
                fs: ['at ./tut2.rsh:115:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
                msg: null,
                who: 'Alice'
                });
              const v332 = stdlib.add(v197, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
              sim_r.txns.push({
                amt: stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0),
                kind: 'to',
                tok: undefined
                });
              const v333 = stdlib.addressEq(v60, v330);
              stdlib.assert(v333, {
                at: 'reach standard library:209:7:dot',
                fs: ['at ./tut2.rsh:115:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
                msg: 'sender correct',
                who: 'Alice'
                });
              sim_r.txns.push({
                amt: v332,
                kind: 'from',
                to: v60,
                tok: undefined
                });
              sim_r.txns.push({
                kind: 'halt',
                tok: undefined
                })
              sim_r.nextSt = stdlib.digest(ctc8, []);
              sim_r.nextSt_noTime = stdlib.digest(ctc8, []);
              sim_r.isHalt = true;
              
              return sim_r;
              })));
            const [] = txn6.data;
            const v334 = txn6.time;
            const v330 = txn6.from;
            stdlib.assert(true, {
              at: 'reach standard library:209:7:dot',
              fs: ['at ./tut2.rsh:115:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
              msg: null,
              who: 'Alice'
              });
            const v332 = stdlib.add(v197, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
            ;
            const v333 = stdlib.addressEq(v60, v330);
            stdlib.assert(v333, {
              at: 'reach standard library:209:7:dot',
              fs: ['at ./tut2.rsh:115:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
              msg: 'sender correct',
              who: 'Alice'
              });
            ;
            stdlib.protect(ctc4, await interact.informTimeout(), {
              at: './tut2.rsh:67:33:application',
              fs: ['at ./tut2.rsh:66:13:application call to [unknown function] (defined at: ./tut2.rsh:66:25:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./tut2.rsh:65:32:function exp)', 'at ./tut2.rsh:115:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
              msg: 'informTimeout',
              who: 'Alice'
              });
            return;
            }
          else {
            const [v205] = txn5.data;
            const v209 = txn5.time;
            const v204 = txn5.from;
            stdlib.assert(true, {
              at: './tut2.rsh:114:11:dot',
              fs: [],
              msg: null,
              who: 'Alice'
              });
            const v207 = stdlib.add(v197, stdlib.checkedBigNumberify('./tut2.rsh:decimal', stdlib.UInt_max, 0));
            ;
            const v208 = stdlib.addressEq(v71, v204);
            stdlib.assert(v208, {
              at: './tut2.rsh:114:11:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Alice'
              });
            const txn6 = await (ctc.sendrecv(12, 2, stdlib.checkedBigNumberify('./tut2.rsh:120:11:dot', stdlib.UInt_max, 7), [ctc5, ctc0, ctc0, ctc5, ctc3, ctc1, ctc0, ctc0, ctc0, ctc1], [v60, v61, v62, v71, v195, v205, v207, v209, v191, v189], [stdlib.checkedBigNumberify('./tut2.rsh:decimal', stdlib.UInt_max, 0), []], [ctc0, ctc1], true, true, v62, (async (txn6) => {
              const sim_r = { txns: [] };
              sim_r.prevSt = stdlib.digest(ctc9, [stdlib.checkedBigNumberify('./tut2.rsh:120:11:dot', stdlib.UInt_max, 10), v60, v61, v62, v71, v195, v205, v207, v209]);
              sim_r.prevSt_noPrevTime = stdlib.digest(ctc10, [stdlib.checkedBigNumberify('./tut2.rsh:120:11:dot', stdlib.UInt_max, 10), v60, v61, v62, v71, v195, v205, v207]);
              const [v214, v215] = txn6.data;
              const v219 = txn6.time;
              const v213 = txn6.from;
              
              stdlib.assert(true, {
                at: './tut2.rsh:120:11:dot',
                fs: [],
                msg: null,
                who: 'Alice'
                });
              const v217 = stdlib.add(v207, stdlib.checkedBigNumberify('./tut2.rsh:decimal', stdlib.UInt_max, 0));
              sim_r.txns.push({
                amt: stdlib.checkedBigNumberify('./tut2.rsh:decimal', stdlib.UInt_max, 0),
                kind: 'to',
                tok: undefined
                });
              const v218 = stdlib.addressEq(v60, v213);
              stdlib.assert(v218, {
                at: './tut2.rsh:120:11:dot',
                fs: [],
                msg: 'sender correct',
                who: 'Alice'
                });
              const v221 = stdlib.digest(ctc2, [v214, v215]);
              const v222 = stdlib.digestEq(v195, v221);
              stdlib.assert(v222, {
                at: 'reach standard library:65:17:application',
                fs: ['at ./tut2.rsh:122:24:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)'],
                msg: null,
                who: 'Alice'
                });
              const v239 = v215[stdlib.checkedBigNumberify('./tut2.rsh:39:18:array ref', stdlib.UInt_max, 0)];
              const v240 = v205[stdlib.checkedBigNumberify('./tut2.rsh:39:29:array ref', stdlib.UInt_max, 0)];
              const v242 = stdlib.sub(stdlib.checkedBigNumberify('./tut2.rsh:7:18:decimal', stdlib.UInt_max, 4), v240);
              const v243 = stdlib.add(v239, v242);
              const v244 = stdlib.mod(v243, stdlib.checkedBigNumberify('./tut2.rsh:7:32:decimal', stdlib.UInt_max, 3));
              const v248 = v215[stdlib.checkedBigNumberify('./tut2.rsh:39:18:array ref', stdlib.UInt_max, 1)];
              const v249 = v205[stdlib.checkedBigNumberify('./tut2.rsh:39:29:array ref', stdlib.UInt_max, 1)];
              const v251 = stdlib.sub(stdlib.checkedBigNumberify('./tut2.rsh:7:18:decimal', stdlib.UInt_max, 4), v249);
              const v252 = stdlib.add(v248, v251);
              const v253 = stdlib.mod(v252, stdlib.checkedBigNumberify('./tut2.rsh:7:32:decimal', stdlib.UInt_max, 3));
              const v257 = v215[stdlib.checkedBigNumberify('./tut2.rsh:39:18:array ref', stdlib.UInt_max, 2)];
              const v258 = v205[stdlib.checkedBigNumberify('./tut2.rsh:39:29:array ref', stdlib.UInt_max, 2)];
              const v260 = stdlib.sub(stdlib.checkedBigNumberify('./tut2.rsh:7:18:decimal', stdlib.UInt_max, 4), v258);
              const v261 = stdlib.add(v257, v260);
              const v262 = stdlib.mod(v261, stdlib.checkedBigNumberify('./tut2.rsh:7:32:decimal', stdlib.UInt_max, 3));
              const v266 = v215[stdlib.checkedBigNumberify('./tut2.rsh:39:18:array ref', stdlib.UInt_max, 3)];
              const v267 = v205[stdlib.checkedBigNumberify('./tut2.rsh:39:29:array ref', stdlib.UInt_max, 3)];
              const v269 = stdlib.sub(stdlib.checkedBigNumberify('./tut2.rsh:7:18:decimal', stdlib.UInt_max, 4), v267);
              const v270 = stdlib.add(v266, v269);
              const v271 = stdlib.mod(v270, stdlib.checkedBigNumberify('./tut2.rsh:7:32:decimal', stdlib.UInt_max, 3));
              const v275 = v215[stdlib.checkedBigNumberify('./tut2.rsh:39:18:array ref', stdlib.UInt_max, 4)];
              const v276 = v205[stdlib.checkedBigNumberify('./tut2.rsh:39:29:array ref', stdlib.UInt_max, 4)];
              const v278 = stdlib.sub(stdlib.checkedBigNumberify('./tut2.rsh:7:18:decimal', stdlib.UInt_max, 4), v276);
              const v279 = stdlib.add(v275, v278);
              const v280 = stdlib.mod(v279, stdlib.checkedBigNumberify('./tut2.rsh:7:32:decimal', stdlib.UInt_max, 3));
              const v293 = stdlib.eq(v244, stdlib.checkedBigNumberify('./tut2.rsh:makeEnum', stdlib.UInt_max, 1));
              const v294 = v293 ? v253 : v244;
              const v297 = stdlib.eq(v294, stdlib.checkedBigNumberify('./tut2.rsh:makeEnum', stdlib.UInt_max, 1));
              const v298 = v297 ? v262 : v294;
              const v301 = stdlib.eq(v298, stdlib.checkedBigNumberify('./tut2.rsh:makeEnum', stdlib.UInt_max, 1));
              const v302 = v301 ? v271 : v298;
              const v305 = stdlib.eq(v302, stdlib.checkedBigNumberify('./tut2.rsh:makeEnum', stdlib.UInt_max, 1));
              const v306 = v305 ? v280 : v302;
              const cv175 = v306;
              const cv427 = v219;
              const cv429 = v217;
              
              (() => {
                const v175 = cv175;
                const v427 = cv427;
                const v429 = cv429;
                
                if ((() => {
                  const v185 = stdlib.eq(v175, stdlib.checkedBigNumberify('./tut2.rsh:makeEnum', stdlib.UInt_max, 1));
                  
                  return v185;})()) {
                  sim_r.nextSt = stdlib.digest(ctc6, [stdlib.checkedBigNumberify('./tut2.rsh:101:17:after expr stmt semicolon', stdlib.UInt_max, 6), v60, v61, v62, v71, v427, v429]);
                  sim_r.nextSt_noTime = stdlib.digest(ctc7, [stdlib.checkedBigNumberify('./tut2.rsh:101:17:after expr stmt semicolon', stdlib.UInt_max, 6), v60, v61, v62, v71, v429]);
                  sim_r.isHalt = false;
                  }
                else {
                  const v369 = stdlib.eq(v175, stdlib.checkedBigNumberify('./tut2.rsh:makeEnum', stdlib.UInt_max, 2));
                  const v372 = stdlib.mul(stdlib.checkedBigNumberify('./tut2.rsh:128:16:decimal', stdlib.UInt_max, 2), v61);
                  const v374 = v369 ? v60 : v71;
                  sim_r.txns.push({
                    amt: v372,
                    kind: 'from',
                    to: v374,
                    tok: undefined
                    });
                  sim_r.txns.push({
                    kind: 'halt',
                    tok: undefined
                    })
                  sim_r.nextSt = stdlib.digest(ctc8, []);
                  sim_r.nextSt_noTime = stdlib.digest(ctc8, []);
                  sim_r.isHalt = true;
                  }})();
              return sim_r;
              })));
            if (txn6.didTimeout) {
              const txn7 = await (ctc.recv(13, 0, [], false, false));
              const [] = txn7.data;
              const v314 = txn7.time;
              const v310 = txn7.from;
              stdlib.assert(true, {
                at: 'reach standard library:209:7:dot',
                fs: ['at ./tut2.rsh:121:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
                msg: null,
                who: 'Alice'
                });
              const v312 = stdlib.add(v207, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
              ;
              const v313 = stdlib.addressEq(v71, v310);
              stdlib.assert(v313, {
                at: 'reach standard library:209:7:dot',
                fs: ['at ./tut2.rsh:121:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
                msg: 'sender correct',
                who: 'Alice'
                });
              ;
              stdlib.protect(ctc4, await interact.informTimeout(), {
                at: './tut2.rsh:67:33:application',
                fs: ['at ./tut2.rsh:66:13:application call to [unknown function] (defined at: ./tut2.rsh:66:25:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./tut2.rsh:65:32:function exp)', 'at ./tut2.rsh:121:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
                msg: 'informTimeout',
                who: 'Alice'
                });
              return;
              }
            else {
              const [v214, v215] = txn6.data;
              const v219 = txn6.time;
              const v213 = txn6.from;
              stdlib.assert(true, {
                at: './tut2.rsh:120:11:dot',
                fs: [],
                msg: null,
                who: 'Alice'
                });
              const v217 = stdlib.add(v207, stdlib.checkedBigNumberify('./tut2.rsh:decimal', stdlib.UInt_max, 0));
              ;
              const v218 = stdlib.addressEq(v60, v213);
              stdlib.assert(v218, {
                at: './tut2.rsh:120:11:dot',
                fs: [],
                msg: 'sender correct',
                who: 'Alice'
                });
              const v221 = stdlib.digest(ctc2, [v214, v215]);
              const v222 = stdlib.digestEq(v195, v221);
              stdlib.assert(v222, {
                at: 'reach standard library:65:17:application',
                fs: ['at ./tut2.rsh:122:24:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)'],
                msg: null,
                who: 'Alice'
                });
              const v239 = v215[stdlib.checkedBigNumberify('./tut2.rsh:39:18:array ref', stdlib.UInt_max, 0)];
              const v240 = v205[stdlib.checkedBigNumberify('./tut2.rsh:39:29:array ref', stdlib.UInt_max, 0)];
              const v242 = stdlib.sub(stdlib.checkedBigNumberify('./tut2.rsh:7:18:decimal', stdlib.UInt_max, 4), v240);
              const v243 = stdlib.add(v239, v242);
              const v244 = stdlib.mod(v243, stdlib.checkedBigNumberify('./tut2.rsh:7:32:decimal', stdlib.UInt_max, 3));
              const v248 = v215[stdlib.checkedBigNumberify('./tut2.rsh:39:18:array ref', stdlib.UInt_max, 1)];
              const v249 = v205[stdlib.checkedBigNumberify('./tut2.rsh:39:29:array ref', stdlib.UInt_max, 1)];
              const v251 = stdlib.sub(stdlib.checkedBigNumberify('./tut2.rsh:7:18:decimal', stdlib.UInt_max, 4), v249);
              const v252 = stdlib.add(v248, v251);
              const v253 = stdlib.mod(v252, stdlib.checkedBigNumberify('./tut2.rsh:7:32:decimal', stdlib.UInt_max, 3));
              const v257 = v215[stdlib.checkedBigNumberify('./tut2.rsh:39:18:array ref', stdlib.UInt_max, 2)];
              const v258 = v205[stdlib.checkedBigNumberify('./tut2.rsh:39:29:array ref', stdlib.UInt_max, 2)];
              const v260 = stdlib.sub(stdlib.checkedBigNumberify('./tut2.rsh:7:18:decimal', stdlib.UInt_max, 4), v258);
              const v261 = stdlib.add(v257, v260);
              const v262 = stdlib.mod(v261, stdlib.checkedBigNumberify('./tut2.rsh:7:32:decimal', stdlib.UInt_max, 3));
              const v266 = v215[stdlib.checkedBigNumberify('./tut2.rsh:39:18:array ref', stdlib.UInt_max, 3)];
              const v267 = v205[stdlib.checkedBigNumberify('./tut2.rsh:39:29:array ref', stdlib.UInt_max, 3)];
              const v269 = stdlib.sub(stdlib.checkedBigNumberify('./tut2.rsh:7:18:decimal', stdlib.UInt_max, 4), v267);
              const v270 = stdlib.add(v266, v269);
              const v271 = stdlib.mod(v270, stdlib.checkedBigNumberify('./tut2.rsh:7:32:decimal', stdlib.UInt_max, 3));
              const v275 = v215[stdlib.checkedBigNumberify('./tut2.rsh:39:18:array ref', stdlib.UInt_max, 4)];
              const v276 = v205[stdlib.checkedBigNumberify('./tut2.rsh:39:29:array ref', stdlib.UInt_max, 4)];
              const v278 = stdlib.sub(stdlib.checkedBigNumberify('./tut2.rsh:7:18:decimal', stdlib.UInt_max, 4), v276);
              const v279 = stdlib.add(v275, v278);
              const v280 = stdlib.mod(v279, stdlib.checkedBigNumberify('./tut2.rsh:7:32:decimal', stdlib.UInt_max, 3));
              const v293 = stdlib.eq(v244, stdlib.checkedBigNumberify('./tut2.rsh:makeEnum', stdlib.UInt_max, 1));
              const v294 = v293 ? v253 : v244;
              const v297 = stdlib.eq(v294, stdlib.checkedBigNumberify('./tut2.rsh:makeEnum', stdlib.UInt_max, 1));
              const v298 = v297 ? v262 : v294;
              const v301 = stdlib.eq(v298, stdlib.checkedBigNumberify('./tut2.rsh:makeEnum', stdlib.UInt_max, 1));
              const v302 = v301 ? v271 : v298;
              const v305 = stdlib.eq(v302, stdlib.checkedBigNumberify('./tut2.rsh:makeEnum', stdlib.UInt_max, 1));
              const v306 = v305 ? v280 : v302;
              const cv175 = v306;
              const cv427 = v219;
              const cv429 = v217;
              
              v175 = cv175;
              v427 = cv427;
              v429 = cv429;
              
              continue;}
            }
          }
        }
      const v369 = stdlib.eq(v175, stdlib.checkedBigNumberify('./tut2.rsh:makeEnum', stdlib.UInt_max, 2));
      const v372 = stdlib.mul(stdlib.checkedBigNumberify('./tut2.rsh:128:16:decimal', stdlib.UInt_max, 2), v61);
      const v374 = v369 ? v60 : v71;
      ;
      stdlib.protect(ctc4, await interact.seeOutcome(v175), {
        at: './tut2.rsh:132:28:application',
        fs: ['at ./tut2.rsh:131:11:application call to [unknown function] (defined at: ./tut2.rsh:131:23:function exp)'],
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
  const ctc3 = stdlib.T_Array(ctc0, stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, 5));
  const ctc4 = stdlib.T_Tuple([ctc0, ctc3]);
  const ctc5 = stdlib.T_Tuple([]);
  const ctc6 = stdlib.T_Address;
  const ctc7 = stdlib.T_Tuple([ctc0, ctc6, ctc0, ctc0, ctc6, ctc1, ctc3, ctc0, ctc0]);
  const ctc8 = stdlib.T_Tuple([ctc0, ctc6, ctc0, ctc0, ctc6, ctc1, ctc3, ctc0]);
  const ctc9 = stdlib.T_Tuple([ctc0, ctc6, ctc0, ctc0, ctc6, ctc1, ctc0, ctc0]);
  const ctc10 = stdlib.T_Tuple([ctc0, ctc6, ctc0, ctc0, ctc6, ctc1, ctc0]);
  const ctc11 = stdlib.T_Tuple([ctc0, ctc6, ctc0, ctc0, ctc6, ctc0, ctc0]);
  const ctc12 = stdlib.T_Tuple([ctc0, ctc6, ctc0, ctc0, ctc6, ctc0]);
  const ctc13 = stdlib.T_Tuple([ctc0, ctc6, ctc0, ctc0, ctc1, ctc6, ctc3, ctc0, ctc0]);
  const ctc14 = stdlib.T_Tuple([ctc0, ctc6, ctc0, ctc0, ctc1, ctc6, ctc3, ctc0]);
  const ctc15 = stdlib.T_Tuple([ctc0, ctc6, ctc0, ctc0, ctc1, ctc0, ctc0]);
  const ctc16 = stdlib.T_Tuple([ctc0, ctc6, ctc0, ctc0, ctc1, ctc0]);
  
  
  const v51 = await ctc.creationTime();
  const txn1 = await (ctc.recv(1, 3, [ctc0, ctc0, ctc1], false, false));
  const [v61, v62, v63] = txn1.data;
  const v66 = txn1.time;
  const v60 = txn1.from;
  stdlib.assert(true, {
    at: './tut2.rsh:77:9:dot',
    fs: [],
    msg: null,
    who: 'Bob'
    });
  const v65 = stdlib.add(stdlib.checkedBigNumberify('./tut2.rsh:compileDApp', stdlib.UInt_max, 0), v61);
  ;
  stdlib.assert(true, {
    at: './tut2.rsh:77:9:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Bob'
    });
  stdlib.protect(ctc2, await interact.acceptWager(v61, v62), {
    at: './tut2.rsh:83:29:application',
    fs: ['at ./tut2.rsh:82:13:application call to [unknown function] (defined at: ./tut2.rsh:82:17:function exp)'],
    msg: 'acceptWager',
    who: 'Bob'
    });
  const v70 = stdlib.protect(ctc3, await interact.getBatch(), {
    at: './tut2.rsh:84:57:application',
    fs: ['at ./tut2.rsh:82:13:application call to [unknown function] (defined at: ./tut2.rsh:82:17:function exp)'],
    msg: 'getBatch',
    who: 'Bob'
    });
  const txn2 = await (ctc.sendrecv(2, 1, stdlib.checkedBigNumberify('./tut2.rsh:86:9:dot', stdlib.UInt_max, 5), [ctc6, ctc0, ctc0, ctc1, ctc0, ctc0, ctc3], [v60, v61, v62, v63, v65, v66, v70], [v61, []], [ctc3], true, true, v62, (async (txn2) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.digest(ctc15, [stdlib.checkedBigNumberify('./tut2.rsh:86:9:dot', stdlib.UInt_max, 1), v60, v61, v62, v63, v65, v66]);
    sim_r.prevSt_noPrevTime = stdlib.digest(ctc16, [stdlib.checkedBigNumberify('./tut2.rsh:86:9:dot', stdlib.UInt_max, 1), v60, v61, v62, v63, v65]);
    const [v72] = txn2.data;
    const v75 = txn2.time;
    const v71 = txn2.from;
    
    stdlib.assert(true, {
      at: './tut2.rsh:86:9:dot',
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
      at: './tut2.rsh:86:9:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Bob'
      });
    sim_r.nextSt = stdlib.digest(ctc13, [stdlib.checkedBigNumberify('./tut2.rsh:89:15:after expr stmt semicolon', stdlib.UInt_max, 2), v60, v61, v62, v63, v71, v72, v74, v75]);
    sim_r.nextSt_noTime = stdlib.digest(ctc14, [stdlib.checkedBigNumberify('./tut2.rsh:89:15:after expr stmt semicolon', stdlib.UInt_max, 2), v60, v61, v62, v63, v71, v72, v74]);
    sim_r.isHalt = false;
    
    return sim_r;
    })));
  if (txn2.didTimeout) {
    const txn3 = await (ctc.recv(3, 0, [], false, false));
    const [] = txn3.data;
    const v412 = txn3.time;
    const v408 = txn3.from;
    stdlib.assert(true, {
      at: 'reach standard library:209:7:dot',
      fs: ['at ./tut2.rsh:88:41:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
      msg: null,
      who: 'Bob'
      });
    const v410 = stdlib.add(v65, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
    ;
    const v411 = stdlib.addressEq(v60, v408);
    stdlib.assert(v411, {
      at: 'reach standard library:209:7:dot',
      fs: ['at ./tut2.rsh:88:41:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
      msg: 'sender correct',
      who: 'Bob'
      });
    ;
    stdlib.protect(ctc2, await interact.informTimeout(), {
      at: './tut2.rsh:67:33:application',
      fs: ['at ./tut2.rsh:66:13:application call to [unknown function] (defined at: ./tut2.rsh:66:25:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./tut2.rsh:65:32:function exp)', 'at ./tut2.rsh:88:41:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
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
      at: './tut2.rsh:86:9:dot',
      fs: [],
      msg: null,
      who: 'Bob'
      });
    const v74 = stdlib.add(v65, v61);
    ;
    stdlib.assert(true, {
      at: './tut2.rsh:86:9:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Bob'
      });
    const txn3 = await (ctc.recv(4, 2, [ctc0, ctc3], false, v62));
    if (txn3.didTimeout) {
      const txn4 = await (ctc.sendrecv(5, 0, stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 7), [ctc6, ctc0, ctc0, ctc1, ctc6, ctc3, ctc0, ctc0], [v60, v61, v62, v63, v71, v72, v74, v75], [stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), []], [], true, true, false, (async (txn4) => {
        const sim_r = { txns: [] };
        sim_r.prevSt = stdlib.digest(ctc13, [stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 2), v60, v61, v62, v63, v71, v72, v74, v75]);
        sim_r.prevSt_noPrevTime = stdlib.digest(ctc14, [stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 2), v60, v61, v62, v63, v71, v72, v74]);
        const [] = txn4.data;
        const v392 = txn4.time;
        const v388 = txn4.from;
        
        stdlib.assert(true, {
          at: 'reach standard library:209:7:dot',
          fs: ['at ./tut2.rsh:95:40:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
          msg: null,
          who: 'Bob'
          });
        const v390 = stdlib.add(v74, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
        sim_r.txns.push({
          amt: stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0),
          kind: 'to',
          tok: undefined
          });
        const v391 = stdlib.addressEq(v71, v388);
        stdlib.assert(v391, {
          at: 'reach standard library:209:7:dot',
          fs: ['at ./tut2.rsh:95:40:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
          msg: 'sender correct',
          who: 'Bob'
          });
        sim_r.txns.push({
          amt: v390,
          kind: 'from',
          to: v71,
          tok: undefined
          });
        sim_r.txns.push({
          kind: 'halt',
          tok: undefined
          })
        sim_r.nextSt = stdlib.digest(ctc5, []);
        sim_r.nextSt_noTime = stdlib.digest(ctc5, []);
        sim_r.isHalt = true;
        
        return sim_r;
        })));
      const [] = txn4.data;
      const v392 = txn4.time;
      const v388 = txn4.from;
      stdlib.assert(true, {
        at: 'reach standard library:209:7:dot',
        fs: ['at ./tut2.rsh:95:40:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
        msg: null,
        who: 'Bob'
        });
      const v390 = stdlib.add(v74, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
      ;
      const v391 = stdlib.addressEq(v71, v388);
      stdlib.assert(v391, {
        at: 'reach standard library:209:7:dot',
        fs: ['at ./tut2.rsh:95:40:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
        msg: 'sender correct',
        who: 'Bob'
        });
      ;
      stdlib.protect(ctc2, await interact.informTimeout(), {
        at: './tut2.rsh:67:33:application',
        fs: ['at ./tut2.rsh:66:13:application call to [unknown function] (defined at: ./tut2.rsh:66:25:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./tut2.rsh:65:32:function exp)', 'at ./tut2.rsh:95:40:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
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
        at: './tut2.rsh:94:9:dot',
        fs: [],
        msg: null,
        who: 'Bob'
        });
      const v83 = stdlib.add(v74, stdlib.checkedBigNumberify('./tut2.rsh:decimal', stdlib.UInt_max, 0));
      ;
      const v84 = stdlib.addressEq(v60, v79);
      stdlib.assert(v84, {
        at: './tut2.rsh:94:9:dot',
        fs: [],
        msg: 'sender correct',
        who: 'Bob'
        });
      const v87 = stdlib.digest(ctc4, [v80, v81]);
      const v88 = stdlib.digestEq(v63, v87);
      stdlib.assert(v88, {
        at: 'reach standard library:65:17:application',
        fs: ['at ./tut2.rsh:96:22:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)'],
        msg: null,
        who: 'Bob'
        });
      const v105 = v81[stdlib.checkedBigNumberify('./tut2.rsh:39:18:array ref', stdlib.UInt_max, 0)];
      const v106 = v72[stdlib.checkedBigNumberify('./tut2.rsh:39:29:array ref', stdlib.UInt_max, 0)];
      const v108 = stdlib.sub(stdlib.checkedBigNumberify('./tut2.rsh:7:18:decimal', stdlib.UInt_max, 4), v106);
      const v109 = stdlib.add(v105, v108);
      const v110 = stdlib.mod(v109, stdlib.checkedBigNumberify('./tut2.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v114 = v81[stdlib.checkedBigNumberify('./tut2.rsh:39:18:array ref', stdlib.UInt_max, 1)];
      const v115 = v72[stdlib.checkedBigNumberify('./tut2.rsh:39:29:array ref', stdlib.UInt_max, 1)];
      const v117 = stdlib.sub(stdlib.checkedBigNumberify('./tut2.rsh:7:18:decimal', stdlib.UInt_max, 4), v115);
      const v118 = stdlib.add(v114, v117);
      const v119 = stdlib.mod(v118, stdlib.checkedBigNumberify('./tut2.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v123 = v81[stdlib.checkedBigNumberify('./tut2.rsh:39:18:array ref', stdlib.UInt_max, 2)];
      const v124 = v72[stdlib.checkedBigNumberify('./tut2.rsh:39:29:array ref', stdlib.UInt_max, 2)];
      const v126 = stdlib.sub(stdlib.checkedBigNumberify('./tut2.rsh:7:18:decimal', stdlib.UInt_max, 4), v124);
      const v127 = stdlib.add(v123, v126);
      const v128 = stdlib.mod(v127, stdlib.checkedBigNumberify('./tut2.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v132 = v81[stdlib.checkedBigNumberify('./tut2.rsh:39:18:array ref', stdlib.UInt_max, 3)];
      const v133 = v72[stdlib.checkedBigNumberify('./tut2.rsh:39:29:array ref', stdlib.UInt_max, 3)];
      const v135 = stdlib.sub(stdlib.checkedBigNumberify('./tut2.rsh:7:18:decimal', stdlib.UInt_max, 4), v133);
      const v136 = stdlib.add(v132, v135);
      const v137 = stdlib.mod(v136, stdlib.checkedBigNumberify('./tut2.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v141 = v81[stdlib.checkedBigNumberify('./tut2.rsh:39:18:array ref', stdlib.UInt_max, 4)];
      const v142 = v72[stdlib.checkedBigNumberify('./tut2.rsh:39:29:array ref', stdlib.UInt_max, 4)];
      const v144 = stdlib.sub(stdlib.checkedBigNumberify('./tut2.rsh:7:18:decimal', stdlib.UInt_max, 4), v142);
      const v145 = stdlib.add(v141, v144);
      const v146 = stdlib.mod(v145, stdlib.checkedBigNumberify('./tut2.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v159 = stdlib.eq(v110, stdlib.checkedBigNumberify('./tut2.rsh:makeEnum', stdlib.UInt_max, 1));
      const v160 = v159 ? v119 : v110;
      const v163 = stdlib.eq(v160, stdlib.checkedBigNumberify('./tut2.rsh:makeEnum', stdlib.UInt_max, 1));
      const v164 = v163 ? v128 : v160;
      const v167 = stdlib.eq(v164, stdlib.checkedBigNumberify('./tut2.rsh:makeEnum', stdlib.UInt_max, 1));
      const v168 = v167 ? v137 : v164;
      const v171 = stdlib.eq(v168, stdlib.checkedBigNumberify('./tut2.rsh:makeEnum', stdlib.UInt_max, 1));
      const v172 = v171 ? v146 : v168;
      let v175 = v172;
      let v427 = v85;
      let v429 = v83;
      
      while ((() => {
        const v185 = stdlib.eq(v175, stdlib.checkedBigNumberify('./tut2.rsh:makeEnum', stdlib.UInt_max, 1));
        
        return v185;})()) {
        const txn4 = await (ctc.recv(8, 1, [ctc1], false, v62));
        if (txn4.didTimeout) {
          const txn5 = await (ctc.sendrecv(9, 0, stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 4), [ctc6, ctc0, ctc0, ctc6, ctc0, ctc0], [v60, v61, v62, v71, v427, v429], [stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), []], [], true, true, false, (async (txn5) => {
            const sim_r = { txns: [] };
            sim_r.prevSt = stdlib.digest(ctc11, [stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 6), v60, v61, v62, v71, v427, v429]);
            sim_r.prevSt_noPrevTime = stdlib.digest(ctc12, [stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 6), v60, v61, v62, v71, v429]);
            const [] = txn5.data;
            const v354 = txn5.time;
            const v350 = txn5.from;
            
            stdlib.assert(true, {
              at: 'reach standard library:209:7:dot',
              fs: ['at ./tut2.rsh:108:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
              msg: null,
              who: 'Bob'
              });
            const v352 = stdlib.add(v429, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
            sim_r.txns.push({
              amt: stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0),
              kind: 'to',
              tok: undefined
              });
            const v353 = stdlib.addressEq(v71, v350);
            stdlib.assert(v353, {
              at: 'reach standard library:209:7:dot',
              fs: ['at ./tut2.rsh:108:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
              msg: 'sender correct',
              who: 'Bob'
              });
            sim_r.txns.push({
              amt: v352,
              kind: 'from',
              to: v71,
              tok: undefined
              });
            sim_r.txns.push({
              kind: 'halt',
              tok: undefined
              })
            sim_r.nextSt = stdlib.digest(ctc5, []);
            sim_r.nextSt_noTime = stdlib.digest(ctc5, []);
            sim_r.isHalt = true;
            
            return sim_r;
            })));
          const [] = txn5.data;
          const v354 = txn5.time;
          const v350 = txn5.from;
          stdlib.assert(true, {
            at: 'reach standard library:209:7:dot',
            fs: ['at ./tut2.rsh:108:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
            msg: null,
            who: 'Bob'
            });
          const v352 = stdlib.add(v429, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
          ;
          const v353 = stdlib.addressEq(v71, v350);
          stdlib.assert(v353, {
            at: 'reach standard library:209:7:dot',
            fs: ['at ./tut2.rsh:108:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
            msg: 'sender correct',
            who: 'Bob'
            });
          ;
          stdlib.protect(ctc2, await interact.informTimeout(), {
            at: './tut2.rsh:67:33:application',
            fs: ['at ./tut2.rsh:66:13:application call to [unknown function] (defined at: ./tut2.rsh:66:25:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./tut2.rsh:65:32:function exp)', 'at ./tut2.rsh:108:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
            msg: 'informTimeout',
            who: 'Bob'
            });
          return;
          }
        else {
          const [v195] = txn4.data;
          const v199 = txn4.time;
          const v194 = txn4.from;
          stdlib.assert(true, {
            at: './tut2.rsh:107:11:dot',
            fs: [],
            msg: null,
            who: 'Bob'
            });
          const v197 = stdlib.add(v429, stdlib.checkedBigNumberify('./tut2.rsh:decimal', stdlib.UInt_max, 0));
          ;
          const v198 = stdlib.addressEq(v60, v194);
          stdlib.assert(v198, {
            at: './tut2.rsh:107:11:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Bob'
            });
          const v203 = stdlib.protect(ctc3, await interact.getBatch(), {
            at: './tut2.rsh:113:54:application',
            fs: ['at ./tut2.rsh:112:15:application call to [unknown function] (defined at: ./tut2.rsh:112:19:function exp)'],
            msg: 'getBatch',
            who: 'Bob'
            });
          const txn5 = await (ctc.sendrecv(10, 1, stdlib.checkedBigNumberify('./tut2.rsh:114:11:dot', stdlib.UInt_max, 6), [ctc6, ctc0, ctc0, ctc6, ctc1, ctc0, ctc0, ctc3], [v60, v61, v62, v71, v195, v197, v199, v203], [stdlib.checkedBigNumberify('./tut2.rsh:decimal', stdlib.UInt_max, 0), []], [ctc3], true, true, v62, (async (txn5) => {
            const sim_r = { txns: [] };
            sim_r.prevSt = stdlib.digest(ctc9, [stdlib.checkedBigNumberify('./tut2.rsh:114:11:dot', stdlib.UInt_max, 8), v60, v61, v62, v71, v195, v197, v199]);
            sim_r.prevSt_noPrevTime = stdlib.digest(ctc10, [stdlib.checkedBigNumberify('./tut2.rsh:114:11:dot', stdlib.UInt_max, 8), v60, v61, v62, v71, v195, v197]);
            const [v205] = txn5.data;
            const v209 = txn5.time;
            const v204 = txn5.from;
            
            stdlib.assert(true, {
              at: './tut2.rsh:114:11:dot',
              fs: [],
              msg: null,
              who: 'Bob'
              });
            const v207 = stdlib.add(v197, stdlib.checkedBigNumberify('./tut2.rsh:decimal', stdlib.UInt_max, 0));
            sim_r.txns.push({
              amt: stdlib.checkedBigNumberify('./tut2.rsh:decimal', stdlib.UInt_max, 0),
              kind: 'to',
              tok: undefined
              });
            const v208 = stdlib.addressEq(v71, v204);
            stdlib.assert(v208, {
              at: './tut2.rsh:114:11:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Bob'
              });
            sim_r.nextSt = stdlib.digest(ctc7, [stdlib.checkedBigNumberify('./tut2.rsh:116:17:after expr stmt semicolon', stdlib.UInt_max, 10), v60, v61, v62, v71, v195, v205, v207, v209]);
            sim_r.nextSt_noTime = stdlib.digest(ctc8, [stdlib.checkedBigNumberify('./tut2.rsh:116:17:after expr stmt semicolon', stdlib.UInt_max, 10), v60, v61, v62, v71, v195, v205, v207]);
            sim_r.isHalt = false;
            
            return sim_r;
            })));
          if (txn5.didTimeout) {
            const txn6 = await (ctc.recv(11, 0, [], false, false));
            const [] = txn6.data;
            const v334 = txn6.time;
            const v330 = txn6.from;
            stdlib.assert(true, {
              at: 'reach standard library:209:7:dot',
              fs: ['at ./tut2.rsh:115:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
              msg: null,
              who: 'Bob'
              });
            const v332 = stdlib.add(v197, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
            ;
            const v333 = stdlib.addressEq(v60, v330);
            stdlib.assert(v333, {
              at: 'reach standard library:209:7:dot',
              fs: ['at ./tut2.rsh:115:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
              msg: 'sender correct',
              who: 'Bob'
              });
            ;
            stdlib.protect(ctc2, await interact.informTimeout(), {
              at: './tut2.rsh:67:33:application',
              fs: ['at ./tut2.rsh:66:13:application call to [unknown function] (defined at: ./tut2.rsh:66:25:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./tut2.rsh:65:32:function exp)', 'at ./tut2.rsh:115:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
              msg: 'informTimeout',
              who: 'Bob'
              });
            return;
            }
          else {
            const [v205] = txn5.data;
            const v209 = txn5.time;
            const v204 = txn5.from;
            stdlib.assert(true, {
              at: './tut2.rsh:114:11:dot',
              fs: [],
              msg: null,
              who: 'Bob'
              });
            const v207 = stdlib.add(v197, stdlib.checkedBigNumberify('./tut2.rsh:decimal', stdlib.UInt_max, 0));
            ;
            const v208 = stdlib.addressEq(v71, v204);
            stdlib.assert(v208, {
              at: './tut2.rsh:114:11:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Bob'
              });
            const txn6 = await (ctc.recv(12, 2, [ctc0, ctc3], false, v62));
            if (txn6.didTimeout) {
              const txn7 = await (ctc.sendrecv(13, 0, stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 7), [ctc6, ctc0, ctc0, ctc6, ctc1, ctc3, ctc0, ctc0], [v60, v61, v62, v71, v195, v205, v207, v209], [stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), []], [], true, true, false, (async (txn7) => {
                const sim_r = { txns: [] };
                sim_r.prevSt = stdlib.digest(ctc7, [stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 10), v60, v61, v62, v71, v195, v205, v207, v209]);
                sim_r.prevSt_noPrevTime = stdlib.digest(ctc8, [stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 10), v60, v61, v62, v71, v195, v205, v207]);
                const [] = txn7.data;
                const v314 = txn7.time;
                const v310 = txn7.from;
                
                stdlib.assert(true, {
                  at: 'reach standard library:209:7:dot',
                  fs: ['at ./tut2.rsh:121:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
                  msg: null,
                  who: 'Bob'
                  });
                const v312 = stdlib.add(v207, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
                sim_r.txns.push({
                  amt: stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0),
                  kind: 'to',
                  tok: undefined
                  });
                const v313 = stdlib.addressEq(v71, v310);
                stdlib.assert(v313, {
                  at: 'reach standard library:209:7:dot',
                  fs: ['at ./tut2.rsh:121:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
                  msg: 'sender correct',
                  who: 'Bob'
                  });
                sim_r.txns.push({
                  amt: v312,
                  kind: 'from',
                  to: v71,
                  tok: undefined
                  });
                sim_r.txns.push({
                  kind: 'halt',
                  tok: undefined
                  })
                sim_r.nextSt = stdlib.digest(ctc5, []);
                sim_r.nextSt_noTime = stdlib.digest(ctc5, []);
                sim_r.isHalt = true;
                
                return sim_r;
                })));
              const [] = txn7.data;
              const v314 = txn7.time;
              const v310 = txn7.from;
              stdlib.assert(true, {
                at: 'reach standard library:209:7:dot',
                fs: ['at ./tut2.rsh:121:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
                msg: null,
                who: 'Bob'
                });
              const v312 = stdlib.add(v207, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
              ;
              const v313 = stdlib.addressEq(v71, v310);
              stdlib.assert(v313, {
                at: 'reach standard library:209:7:dot',
                fs: ['at ./tut2.rsh:121:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
                msg: 'sender correct',
                who: 'Bob'
                });
              ;
              stdlib.protect(ctc2, await interact.informTimeout(), {
                at: './tut2.rsh:67:33:application',
                fs: ['at ./tut2.rsh:66:13:application call to [unknown function] (defined at: ./tut2.rsh:66:25:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./tut2.rsh:65:32:function exp)', 'at ./tut2.rsh:121:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
                msg: 'informTimeout',
                who: 'Bob'
                });
              return;
              }
            else {
              const [v214, v215] = txn6.data;
              const v219 = txn6.time;
              const v213 = txn6.from;
              stdlib.assert(true, {
                at: './tut2.rsh:120:11:dot',
                fs: [],
                msg: null,
                who: 'Bob'
                });
              const v217 = stdlib.add(v207, stdlib.checkedBigNumberify('./tut2.rsh:decimal', stdlib.UInt_max, 0));
              ;
              const v218 = stdlib.addressEq(v60, v213);
              stdlib.assert(v218, {
                at: './tut2.rsh:120:11:dot',
                fs: [],
                msg: 'sender correct',
                who: 'Bob'
                });
              const v221 = stdlib.digest(ctc4, [v214, v215]);
              const v222 = stdlib.digestEq(v195, v221);
              stdlib.assert(v222, {
                at: 'reach standard library:65:17:application',
                fs: ['at ./tut2.rsh:122:24:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)'],
                msg: null,
                who: 'Bob'
                });
              const v239 = v215[stdlib.checkedBigNumberify('./tut2.rsh:39:18:array ref', stdlib.UInt_max, 0)];
              const v240 = v205[stdlib.checkedBigNumberify('./tut2.rsh:39:29:array ref', stdlib.UInt_max, 0)];
              const v242 = stdlib.sub(stdlib.checkedBigNumberify('./tut2.rsh:7:18:decimal', stdlib.UInt_max, 4), v240);
              const v243 = stdlib.add(v239, v242);
              const v244 = stdlib.mod(v243, stdlib.checkedBigNumberify('./tut2.rsh:7:32:decimal', stdlib.UInt_max, 3));
              const v248 = v215[stdlib.checkedBigNumberify('./tut2.rsh:39:18:array ref', stdlib.UInt_max, 1)];
              const v249 = v205[stdlib.checkedBigNumberify('./tut2.rsh:39:29:array ref', stdlib.UInt_max, 1)];
              const v251 = stdlib.sub(stdlib.checkedBigNumberify('./tut2.rsh:7:18:decimal', stdlib.UInt_max, 4), v249);
              const v252 = stdlib.add(v248, v251);
              const v253 = stdlib.mod(v252, stdlib.checkedBigNumberify('./tut2.rsh:7:32:decimal', stdlib.UInt_max, 3));
              const v257 = v215[stdlib.checkedBigNumberify('./tut2.rsh:39:18:array ref', stdlib.UInt_max, 2)];
              const v258 = v205[stdlib.checkedBigNumberify('./tut2.rsh:39:29:array ref', stdlib.UInt_max, 2)];
              const v260 = stdlib.sub(stdlib.checkedBigNumberify('./tut2.rsh:7:18:decimal', stdlib.UInt_max, 4), v258);
              const v261 = stdlib.add(v257, v260);
              const v262 = stdlib.mod(v261, stdlib.checkedBigNumberify('./tut2.rsh:7:32:decimal', stdlib.UInt_max, 3));
              const v266 = v215[stdlib.checkedBigNumberify('./tut2.rsh:39:18:array ref', stdlib.UInt_max, 3)];
              const v267 = v205[stdlib.checkedBigNumberify('./tut2.rsh:39:29:array ref', stdlib.UInt_max, 3)];
              const v269 = stdlib.sub(stdlib.checkedBigNumberify('./tut2.rsh:7:18:decimal', stdlib.UInt_max, 4), v267);
              const v270 = stdlib.add(v266, v269);
              const v271 = stdlib.mod(v270, stdlib.checkedBigNumberify('./tut2.rsh:7:32:decimal', stdlib.UInt_max, 3));
              const v275 = v215[stdlib.checkedBigNumberify('./tut2.rsh:39:18:array ref', stdlib.UInt_max, 4)];
              const v276 = v205[stdlib.checkedBigNumberify('./tut2.rsh:39:29:array ref', stdlib.UInt_max, 4)];
              const v278 = stdlib.sub(stdlib.checkedBigNumberify('./tut2.rsh:7:18:decimal', stdlib.UInt_max, 4), v276);
              const v279 = stdlib.add(v275, v278);
              const v280 = stdlib.mod(v279, stdlib.checkedBigNumberify('./tut2.rsh:7:32:decimal', stdlib.UInt_max, 3));
              const v293 = stdlib.eq(v244, stdlib.checkedBigNumberify('./tut2.rsh:makeEnum', stdlib.UInt_max, 1));
              const v294 = v293 ? v253 : v244;
              const v297 = stdlib.eq(v294, stdlib.checkedBigNumberify('./tut2.rsh:makeEnum', stdlib.UInt_max, 1));
              const v298 = v297 ? v262 : v294;
              const v301 = stdlib.eq(v298, stdlib.checkedBigNumberify('./tut2.rsh:makeEnum', stdlib.UInt_max, 1));
              const v302 = v301 ? v271 : v298;
              const v305 = stdlib.eq(v302, stdlib.checkedBigNumberify('./tut2.rsh:makeEnum', stdlib.UInt_max, 1));
              const v306 = v305 ? v280 : v302;
              const cv175 = v306;
              const cv427 = v219;
              const cv429 = v217;
              
              v175 = cv175;
              v427 = cv427;
              v429 = cv429;
              
              continue;}
            }
          }
        }
      const v369 = stdlib.eq(v175, stdlib.checkedBigNumberify('./tut2.rsh:makeEnum', stdlib.UInt_max, 2));
      const v372 = stdlib.mul(stdlib.checkedBigNumberify('./tut2.rsh:128:16:decimal', stdlib.UInt_max, 2), v61);
      const v374 = v369 ? v60 : v71;
      ;
      stdlib.protect(ctc2, await interact.seeOutcome(v175), {
        at: './tut2.rsh:132:28:application',
        fs: ['at ./tut2.rsh:131:11:application call to [unknown function] (defined at: ./tut2.rsh:131:23:function exp)'],
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
  stepargs: [0, 129, 209, 169, 289, 241, 0, 0, 201, 169, 241, 201, 289, 241],
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
// "./tut2.rsh:77:9:dot"
// "[]"
int 1
assert
int 0
gtxna 0 ApplicationArgs 5
btoi
+
store 255
// CheckPay
// "./tut2.rsh:77:9:dot"
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
// "./tut2.rsh:77:9:dot"
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
// "./tut2.rsh:86:9:dot"
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
// "./tut2.rsh:86:9:dot"
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
// "./tut2.rsh:86:9:dot"
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
// "[at ./tut2.rsh:88:41:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
int 1
assert
// CheckPay
// "reach standard library:209:7:dot"
// "[at ./tut2.rsh:88:41:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
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
// "[at ./tut2.rsh:88:41:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
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
// "./tut2.rsh:94:9:dot"
// "[]"
int 1
assert
gtxna 0 ApplicationArgs 11
btoi
int 0
+
store 255
// CheckPay
// "./tut2.rsh:94:9:dot"
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
// "./tut2.rsh:94:9:dot"
// "[]"
gtxna 0 ApplicationArgs 5
gtxn 0 Sender
==
assert
// Nothing
// "reach standard library:65:17:application"
// "[at ./tut2.rsh:96:22:application call to \"checkCommitment\" (defined at: reach standard library:64:8:function exp)]"
gtxna 0 ApplicationArgs 8
gtxna 0 ApplicationArgs 12
gtxna 0 ApplicationArgs 13
concat
keccak256
==
assert
gtxna 0 ApplicationArgs 13
substring 0 8
btoi
int 4
gtxna 0 ApplicationArgs 10
substring 0 8
btoi
-
+
int 3
%
dup
store 254
gtxna 0 ApplicationArgs 13
substring 8 16
btoi
int 4
gtxna 0 ApplicationArgs 10
substring 8 16
btoi
-
+
int 3
%
load 254
int 1
==
select
dup
store 253
gtxna 0 ApplicationArgs 13
substring 16 24
btoi
int 4
gtxna 0 ApplicationArgs 10
substring 16 24
btoi
-
+
int 3
%
load 253
int 1
==
select
dup
store 252
gtxna 0 ApplicationArgs 13
substring 24 32
btoi
int 4
gtxna 0 ApplicationArgs 10
substring 24 32
btoi
-
+
int 3
%
load 252
int 1
==
select
dup
store 251
gtxna 0 ApplicationArgs 13
substring 32 40
btoi
int 4
gtxna 0 ApplicationArgs 10
substring 32 40
btoi
-
+
int 3
%
load 251
int 1
==
select
dup
store 250
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
load 250
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
// "[at ./tut2.rsh:95:40:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
int 1
assert
// CheckPay
// "reach standard library:209:7:dot"
// "[at ./tut2.rsh:95:40:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
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
// "[at ./tut2.rsh:95:40:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
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
// "./tut2.rsh:107:11:dot"
// "[]"
int 1
assert
gtxna 0 ApplicationArgs 9
btoi
int 0
+
store 255
// CheckPay
// "./tut2.rsh:107:11:dot"
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
// "./tut2.rsh:107:11:dot"
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
// "[at ./tut2.rsh:108:43:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
int 1
assert
// CheckPay
// "reach standard library:209:7:dot"
// "[at ./tut2.rsh:108:43:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
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
// "[at ./tut2.rsh:108:43:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
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
// "./tut2.rsh:114:11:dot"
// "[]"
int 1
assert
gtxna 0 ApplicationArgs 10
btoi
int 0
+
store 255
// CheckPay
// "./tut2.rsh:114:11:dot"
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
// "./tut2.rsh:114:11:dot"
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
// "[at ./tut2.rsh:115:43:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
int 1
assert
// CheckPay
// "reach standard library:209:7:dot"
// "[at ./tut2.rsh:115:43:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
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
// "[at ./tut2.rsh:115:43:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
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
// "./tut2.rsh:120:11:dot"
// "[]"
int 1
assert
gtxna 0 ApplicationArgs 11
btoi
int 0
+
store 255
// CheckPay
// "./tut2.rsh:120:11:dot"
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
// "./tut2.rsh:120:11:dot"
// "[]"
gtxna 0 ApplicationArgs 5
gtxn 0 Sender
==
assert
// Nothing
// "reach standard library:65:17:application"
// "[at ./tut2.rsh:122:24:application call to \"checkCommitment\" (defined at: reach standard library:64:8:function exp)]"
gtxna 0 ApplicationArgs 9
gtxna 0 ApplicationArgs 12
gtxna 0 ApplicationArgs 13
concat
keccak256
==
assert
gtxna 0 ApplicationArgs 13
substring 0 8
btoi
int 4
gtxna 0 ApplicationArgs 10
substring 0 8
btoi
-
+
int 3
%
dup
store 254
gtxna 0 ApplicationArgs 13
substring 8 16
btoi
int 4
gtxna 0 ApplicationArgs 10
substring 8 16
btoi
-
+
int 3
%
load 254
int 1
==
select
dup
store 253
gtxna 0 ApplicationArgs 13
substring 16 24
btoi
int 4
gtxna 0 ApplicationArgs 10
substring 16 24
btoi
-
+
int 3
%
load 253
int 1
==
select
dup
store 252
gtxna 0 ApplicationArgs 13
substring 24 32
btoi
int 4
gtxna 0 ApplicationArgs 10
substring 24 32
btoi
-
+
int 3
%
load 252
int 1
==
select
dup
store 251
gtxna 0 ApplicationArgs 13
substring 32 40
btoi
int 4
gtxna 0 ApplicationArgs 10
substring 32 40
btoi
-
+
int 3
%
load 251
int 1
==
select
dup
store 250
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
load 250
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
// "[at ./tut2.rsh:121:43:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
int 1
assert
// CheckPay
// "reach standard library:209:7:dot"
// "[at ./tut2.rsh:121:43:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
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
// "[at ./tut2.rsh:121:43:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
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
                "name": "v195",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v197",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v199",
                "type": "uint256"
              }
            ],
            "internalType": "struct T19",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256[5]",
                "name": "v205",
                "type": "uint256[5]"
              }
            ],
            "internalType": "struct T24",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct T25",
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
                "name": "v195",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v197",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v199",
                "type": "uint256"
              }
            ],
            "internalType": "struct T19",
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
        "internalType": "struct T26",
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
                "name": "v195",
                "type": "uint256"
              },
              {
                "internalType": "uint256[5]",
                "name": "v205",
                "type": "uint256[5]"
              },
              {
                "internalType": "uint256",
                "name": "v207",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v209",
                "type": "uint256"
              }
            ],
            "internalType": "struct T23",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v214",
                "type": "uint256"
              },
              {
                "internalType": "uint256[5]",
                "name": "v215",
                "type": "uint256[5]"
              }
            ],
            "internalType": "struct T27",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct T28",
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
                "name": "v195",
                "type": "uint256"
              },
              {
                "internalType": "uint256[5]",
                "name": "v205",
                "type": "uint256[5]"
              },
              {
                "internalType": "uint256",
                "name": "v207",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v209",
                "type": "uint256"
              }
            ],
            "internalType": "struct T23",
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
        "internalType": "struct T29",
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
                "internalType": "uint256[5]",
                "name": "v72",
                "type": "uint256[5]"
              }
            ],
            "internalType": "struct T6",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct T7",
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
        "internalType": "struct T9",
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
                "internalType": "uint256[5]",
                "name": "v72",
                "type": "uint256[5]"
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
            "internalType": "struct T5",
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
                "internalType": "uint256[5]",
                "name": "v81",
                "type": "uint256[5]"
              }
            ],
            "internalType": "struct T13",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct T14",
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
                "internalType": "uint256[5]",
                "name": "v72",
                "type": "uint256[5]"
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
            "internalType": "struct T5",
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
        "internalType": "struct T15",
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
                "name": "v427",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v429",
                "type": "uint256"
              }
            ],
            "internalType": "struct T16",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v195",
                "type": "uint256"
              }
            ],
            "internalType": "struct T20",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct T21",
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
                "name": "v427",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v429",
                "type": "uint256"
              }
            ],
            "internalType": "struct T16",
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
        "internalType": "struct T22",
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
                "name": "v195",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v197",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v199",
                "type": "uint256"
              }
            ],
            "internalType": "struct T19",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256[5]",
                "name": "v205",
                "type": "uint256[5]"
              }
            ],
            "internalType": "struct T24",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "internalType": "struct T25",
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
                "name": "v195",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v197",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v199",
                "type": "uint256"
              }
            ],
            "internalType": "struct T19",
            "name": "svs",
            "type": "tuple"
          },
          {
            "internalType": "bool",
            "name": "msg",
            "type": "bool"
          }
        ],
        "internalType": "struct T26",
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
                "name": "v195",
                "type": "uint256"
              },
              {
                "internalType": "uint256[5]",
                "name": "v205",
                "type": "uint256[5]"
              },
              {
                "internalType": "uint256",
                "name": "v207",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v209",
                "type": "uint256"
              }
            ],
            "internalType": "struct T23",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v214",
                "type": "uint256"
              },
              {
                "internalType": "uint256[5]",
                "name": "v215",
                "type": "uint256[5]"
              }
            ],
            "internalType": "struct T27",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "internalType": "struct T28",
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
                "name": "v195",
                "type": "uint256"
              },
              {
                "internalType": "uint256[5]",
                "name": "v205",
                "type": "uint256[5]"
              },
              {
                "internalType": "uint256",
                "name": "v207",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v209",
                "type": "uint256"
              }
            ],
            "internalType": "struct T23",
            "name": "svs",
            "type": "tuple"
          },
          {
            "internalType": "bool",
            "name": "msg",
            "type": "bool"
          }
        ],
        "internalType": "struct T29",
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
                "internalType": "uint256[5]",
                "name": "v72",
                "type": "uint256[5]"
              }
            ],
            "internalType": "struct T6",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "internalType": "struct T7",
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
        "internalType": "struct T9",
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
                "internalType": "uint256[5]",
                "name": "v72",
                "type": "uint256[5]"
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
            "internalType": "struct T5",
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
                "internalType": "uint256[5]",
                "name": "v81",
                "type": "uint256[5]"
              }
            ],
            "internalType": "struct T13",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "internalType": "struct T14",
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
                "internalType": "uint256[5]",
                "name": "v72",
                "type": "uint256[5]"
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
            "internalType": "struct T5",
            "name": "svs",
            "type": "tuple"
          },
          {
            "internalType": "bool",
            "name": "msg",
            "type": "bool"
          }
        ],
        "internalType": "struct T15",
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
                "name": "v427",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v429",
                "type": "uint256"
              }
            ],
            "internalType": "struct T16",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v195",
                "type": "uint256"
              }
            ],
            "internalType": "struct T20",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "internalType": "struct T21",
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
                "name": "v427",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v429",
                "type": "uint256"
              }
            ],
            "internalType": "struct T16",
            "name": "svs",
            "type": "tuple"
          },
          {
            "internalType": "bool",
            "name": "msg",
            "type": "bool"
          }
        ],
        "internalType": "struct T22",
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
  Bytecode: `0x608060408190527f49ff028a829527a47ec6839c7147b484eccf5a2a94853eddac09cef44d9d4e9e90600090a16040805160208082018352438252825180820184526000808252925181528351808301849052905181850152835180820385018152606090910190935282519201919091209055611c48806100826000396000f3fe6080604052600436106100a05760003560e01c8063abb65ccc11610064578063abb65ccc1461010d578063b880f86a14610120578063c0730ab414610133578063d10ac92214610146578063d22bdb5814610159578063d34b6dfc1461016c576100a7565b8063253d71be146100ac5780632bf4f873146100c1578063515d2731146100d4578063571d5205146100e75780639b4521e1146100fa576100a7565b366100a757005b600080fd5b6100bf6100ba366004611666565b61017f565b005b6100bf6100cf3660046116c1565b6102a8565b6100bf6100e23660046116d2565b61044d565b6100bf6100f5366004611682565b6105a0565b6100bf61010836600461169d565b6106b8565b6100bf61011b366004611682565b610841565b6100bf61012e366004611666565b610a30565b6100bf61014136600461164a565b610b4a565b6100bf61015436600461164a565b610e3e565b6100bf610167366004611682565b6110a8565b6100bf61017a3660046116af565b6111ba565b604051610193906002908390602001611b20565b6040516020818303038152906040528051906020012060001c600054146101b957600080fd5b600080556101d06040820135610160830135611b8e565b43101580156101dd575060015b6101e657600080fd5b34156101f157600080fd5b3361020260a0830160808401611629565b6001600160a01b03161461021557600080fd5b61022560a0820160808301611629565b6001600160a01b03166108fc6102416000610140850135611b8e565b6040518115909202916000818181858888f19350505050158015610269573d6000803e3d6000fd5b507fc18ab0af979eec50e5539334a3b97c236f4df25f2c0886da35b2f866708b0b13816040516102999190611919565b60405180910390a16000805533ff5b60408051600060208201528235918101919091526060016040516020818303038152906040528051906020012060001c600054146102e557600080fd5b6000808055604080516020810190915290815261030760208301356000611b8e565b81523460208301351461031957600080fd5b60408051833581526020808501359082015283820135818301526060808501359082015290517f2bb570a5feee0f446e450005a048c78efd478914692e1f0be1009bac144b11709181900360800190a16103ab6040518060c0016040528060006001600160a01b0316815260200160008152602001600081526020016000815260200160008152602001600081525090565b338152602083810135818301908152604080860135818501908152606080880135818701908152875160808089019182524360a0808b01918252875160019a81019a909a528a516001600160a01b0316978a019790975296519388019390935292519186019190915251918401919091525160c08301525160e0820152610100015b60408051601f198184030181529190528051602090910120600055505050565b604051610461906001908390602001611a80565b6040516020818303038152906040528051906020012060001c6000541461048757600080fd5b6000808055604080516020810182529182526104aa9083013560a0840135611b8e565b43106104b557600080fd5b6104c760208301356080840135611b8e565b8152346020830135146104d957600080fd5b7f128ceb6e462bfebf9ef10870b6d9ae608efb33f1cdefac45d94895b5f28af76e826040516105089190611a0b565b60405180910390a16105186114b5565b6105256020840184611629565b6001600160a01b03168152602080840135908201526040808401358183015260608085013590830152336080830152805160a08181019092529060c08501906005908390839080828437600092019190915250505060a0820152815160c08201524360e082015260405161042d906002908390602001611b35565b6040516105b4906006908390602001611a57565b6040516020818303038152906040528051906020012060001c600054146105da57600080fd5b600080556105f060408201356080830135611b8e565b43101580156105fd575060015b61060657600080fd5b341561061157600080fd5b336106226080830160608401611629565b6001600160a01b03161461063557600080fd5b6106456080820160608301611629565b6001600160a01b03166108fc610660600060a0850135611b8e565b6040518115909202916000818181858888f19350505050158015610688573d6000803e3d6000fd5b507fca13e6f8d91ff4ca5d5587cef0f51406a1cbd78ee61e6b614471da01a93eeea2816040516102999190611968565b6040516106cc906008908390602001611a6b565b6040516020818303038152906040528051906020012060001c600054146106f257600080fd5b6000808055604080516020810182529182526107159083013560c0840135611b8e565b431061072057600080fd5b61072f600060a0840135611b8e565b8152341561073c57600080fd5b3361074d6080840160608501611629565b6001600160a01b03161461076057600080fd5b7fb5bc6a299ec3e0af4dedca1415eb651d5d6c912c04d8f7040f94c46574d04f088260405161078f9190611998565b60405180910390a161079f611512565b6107ac6020840184611629565b6001600160a01b0316815260208084013590820152604080840135908201526107db6080840160608501611629565b6001600160a01b03166060820152608080840135908201526040805160a08181019092529060e08501906005908390839080828437600092019190915250505060a0820152815160c08201524360e082015260405161042d90600a908390602001611aa9565b604051610855906006908390602001611a57565b6040516020818303038152906040528051906020012060001c6000541461087b57600080fd5b60008080556040805160208101825291825261089e908301356080840135611b8e565b43106108a957600080fd5b6108b8600060a0840135611b8e565b815234156108c557600080fd5b336108d36020840184611629565b6001600160a01b0316146108e657600080fd5b7fdaaf5830bac0927d0eb7e607cca3bafc032c01e985c2937f25e78351cc41ccc382604051610915919061194c565b60405180910390a161096f6040518060e0016040528060006001600160a01b03168152602001600081526020016000815260200160006001600160a01b031681526020016000815260200160008152602001600081525090565b61097c6020840184611629565b6001600160a01b0316815260208084013590820152604080840135908201526109ab6080840160608501611629565b6001600160a01b03908116606083810191825260c0868101356080808701918252875160a080890191825243858a019081526040805160086020808301919091528c518c16828401528c0151988101989098528a0151938701939093529551909616948401949094525190820152915160e0830152516101008201526101200161042d565b604051610a4490600a908390602001611a94565b6040516020818303038152906040528051906020012060001c60005414610a6a57600080fd5b60008055610a816040820135610160830135611b8e565b4310158015610a8e575060015b610a9757600080fd5b3415610aa257600080fd5b33610ab36080830160608401611629565b6001600160a01b031614610ac657600080fd5b610ad66080820160608301611629565b6001600160a01b03166108fc610af26000610140850135611b8e565b6040518115909202916000818181858888f19350505050158015610b1a573d6000803e3d6000fd5b507fa214578b700dfe990c18bb84f49e9ec1851731446c25e7eff7a0e358bf72e2818160405161029991906119fc565b604051610b5e90600a908390602001611a94565b6040516020818303038152906040528051906020012060001c60005414610b8457600080fd5b60008081905550610bb66040518060800160405280600081526020016000815260200160008152602001600081525090565b610bc96040830135610160840135611b8e565b4310610bd457600080fd5b3415610bdf57600080fd5b33610bed6020840184611629565b6001600160a01b031614610c0057600080fd5b604051610c1c90610180840135906101a0850190602001611a3d565b60408051601f198184030181529190528051602090910120608083013514610c4357600080fd5b6003610c5460a08401356004611bc5565b610c63906101a0850135611b8e565b610c6d9190611bdc565b808252600114610c7e578051610ca8565b6003610c8f60c08401356004611bc5565b610c9e906101c0850135611b8e565b610ca89190611bdc565b60208201819052600114610cc0578060200151610cea565b6003610cd160e08401356004611bc5565b610ce0906101e0850135611b8e565b610cea9190611bdc565b60408201819052600114610d02578060400151610d2d565b6003610d146101008401356004611bc5565b610d2390610200850135611b8e565b610d2d9190611bdc565b60608201526040517fa21d5f6c6dd0bba4371417c9f50787e3571c79cf8341a5d0cf5327d081a8e4d290610d629084906119ed565b60405180910390a1610d7261155b565b610d7f6020840184611629565b81516001600160a01b03909116905280516020808501359101528051604080850135910152610db46080840160608501611629565b81516001600160a01b03909116606091820152820151600114610ddb578160600151610e06565b6003610ded6101208501356004611bc5565b610dfc90610220860135611b8e565b610e069190611bdc565b6020808301805192909252905143910152610e276000610140850135611b8e565b602082015160400152610e39816112cc565b505050565b604051610e52906002908390602001611b20565b6040516020818303038152906040528051906020012060001c60005414610e7857600080fd5b60008081905550610eaa6040518060800160405280600081526020016000815260200160008152602001600081525090565b610ebd6040830135610160840135611b8e565b4310610ec857600080fd5b3415610ed357600080fd5b33610ee16020840184611629565b6001600160a01b031614610ef457600080fd5b604051610f1090610180840135906101a0850190602001611a3d565b60408051601f198184030181529190528051602090910120606083013514610f3757600080fd5b6003610f4860a08401356004611bc5565b610f57906101a0850135611b8e565b610f619190611bdc565b808252600114610f72578051610f9c565b6003610f8360c08401356004611bc5565b610f92906101c0850135611b8e565b610f9c9190611bdc565b60208201819052600114610fb4578060200151610fde565b6003610fc560e08401356004611bc5565b610fd4906101e0850135611b8e565b610fde9190611bdc565b60408201819052600114610ff6578060400151611021565b60036110086101008401356004611bc5565b61101790610200850135611b8e565b6110219190611bdc565b60608201526040517f85b984f93e23bb278dcf4c426e3ebada3df03a830c17f29b763cc686eeff6030906110569084906118f2565b60405180910390a161106661155b565b6110736020840184611629565b81516001600160a01b03909116905280516020808501359101528051604080850135910152610db460a0840160808501611629565b6040516110bc906001908390602001611a80565b6040516020818303038152906040528051906020012060001c600054146110e257600080fd5b600080556110f8604082013560a0830135611b8e565b4310158015611105575060015b61110e57600080fd5b341561111957600080fd5b336111276020830183611629565b6001600160a01b03161461113a57600080fd5b6111476020820182611629565b6001600160a01b03166108fc61116260006080850135611b8e565b6040518115909202916000818181858888f1935050505015801561118a573d6000803e3d6000fd5b507f17040e3ed853a8df776cd092f1357f15488d98d460f66cd5e6b0cb07d5bc8ae3816040516102999190611a2f565b6040516111ce906008908390602001611a6b565b6040516020818303038152906040528051906020012060001c600054146111f457600080fd5b6000805561120a604082013560c0830135611b8e565b4310158015611217575060015b61122057600080fd5b341561122b57600080fd5b336112396020830183611629565b6001600160a01b03161461124c57600080fd5b6112596020820182611629565b6001600160a01b03166108fc611274600060a0850135611b8e565b6040518115909202916000818181858888f1935050505015801561129c573d6000803e3d6000fd5b507f3ed1473309a228b1330dadeda04661997e11292de069751738dd383ff44916b48160405161029991906119bc565b602081015151600114156113da576113256040518060c0016040528060006001600160a01b03168152602001600081526020016000815260200160006001600160a01b0316815260200160008152602001600081525090565b8151516001600160a01b039081168252825160209081015181840190815284516040908101518186019081528651606090810151861681880190815285890180518701516080808b01918252915186015160a0808c0191825287516006818c01529b518b168c8901529751948b01949094529351908901525190951692860192909252905160c0850152915160e080850191909152825180850390910181526101009093019091528151910120600055611445565b6040805160c081018252600091810182815260608083018481526080840185815260a085018681528486526020808701979097528751516001600160a01b0390811690955287518701519092528651909201519092169052918301515190915261144381611448565b505b50565b80516060015160021461146057805160400151611464565b8051515b6001600160a01b03166108fc82600001516020015160026114859190611ba6565b6040518115909202916000818181858888f193505050501580156114ad573d6000803e3d6000fd5b506000805533ff5b60405180610100016040528060006001600160a01b0316815260200160008152602001600081526020016000815260200160006001600160a01b031681526020016114fe6115b4565b815260200160008152602001600081525090565b60405180610100016040528060006001600160a01b03168152602001600081526020016000815260200160006001600160a01b03168152602001600081526020016114fe6115b4565b6040805160c0810182526000918101828152606082018390526080820183905260a082019290925290819081526020016115af60405180606001604052806000815260200160008152602001600081525090565b905290565b6040518060a001604052806005906020820280368337509192915050565b80356001600160a01b03811681146115e957600080fd5b919050565b60006102408284031215611600578081fd5b50919050565b60006101a08284031215611600578081fd5b600060e08284031215611600578081fd5b60006020828403121561163a578081fd5b611643826115d2565b9392505050565b6000610240828403121561165c578081fd5b61164383836115ee565b60006101a08284031215611678578081fd5b6116438383611606565b600060e08284031215611693578081fd5b6116438383611618565b60006101808284031215611600578081fd5b60006101008284031215611600578081fd5b600060808284031215611600578081fd5b60006101608284031215611600578081fd5b8060005b60058110156117075781518452602093840193909101906001016116e8565b50505050565b8035825260a0602082016020840137600060c08301525050565b6001600160a01b0380611739836115d2565b16835260208201356020840152604082013560408401528061175d606084016115d2565b166060840152506080818101359083015260a090810135910152565b6001600160a01b038061178b836115d2565b1683526020820135602084015260408201356040840152806117af606084016115d2565b166060840152506080810135608083015260a081013560a083015260c081013560c08301525050565b6001600160a01b036117e9826115d2565b1682526020810135602083015260408101356040830152606081013560608301526080810135608083015260a081013560a08301525050565b6001600160a01b0380611834836115d2565b168352602082013560208401526040820135604084015280611858606084016115d2565b166060840152506080810135608083015260a080820160a0840137610140818101359083015261016090810135910152565b6001600160a01b038061189c836115d2565b168352602082013560208401526040820135604084015260608201356060840152806118ca608084016115d2565b1660808401525060a080820160a0840137610140818101359083015261016090810135910152565b6102408101611901828461188a565b61018061191281840182860161170d565b5092915050565b6101a08101611928828461188a565b6101808084013580151580821461193e57600080fd5b808386015250505092915050565b60e0810161195a8284611727565b60c092830135919092015290565b60e081016119768284611727565b60c083013580151580821461198a57600080fd5b8060c0850152505092915050565b61018081016119a78284611779565b60a060e0840160e08401376000815292915050565b61010081016119cb8284611779565b60e08301358015158082146119df57600080fd5b8060e0850152505092915050565b61024081016119018284611822565b6101a081016119288284611822565b6101608101611a1a82846117d8565b60a060c0840160c08401376000815292915050565b60e0810161197682846117d8565b82815260c0810160a0836020840137600081529392505050565b82815260e081016116436020830184611727565b82815261010081016116436020830184611779565b82815260e0810161164360208301846117d8565b8281526101a081016116436020830184611822565b60006101a08201905083825260018060a01b03808451166020840152602084015160408401526040840151606084015280606085015116608084015250608083015160a083015260a0830151611b0260c08401826116e4565b5060c083015161016083015260e08301516101808301529392505050565b8281526101a08101611643602083018461188a565b60006101a08201905083825260018060a01b038084511660208401526020840151604084015260408401516060840152606084015160808401528060808501511660a08401525060a0830151611b0260c08401826116e4565b60008219821115611ba157611ba1611bfc565b500190565b6000816000190483118215151615611bc057611bc0611bfc565b500290565b600082821015611bd757611bd7611bfc565b500390565b600082611bf757634e487b7160e01b81526012600452602481fd5b500690565b634e487b7160e01b600052601160045260246000fdfea26469706673582212203391866afb5856fe06bcbadcbab28e191fb41c27bd9f39255a7f0d7117de674664736f6c63430008020033`,
  deployMode: `DM_constructor`
  };

export const _Connectors = {
  ALGO: _ALGO,
  ETH: _ETH
  };

