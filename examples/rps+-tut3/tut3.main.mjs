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
  const ctc5 = stdlib.T_Tuple([]);
  const ctc6 = stdlib.T_Address;
  const ctc7 = stdlib.T_Tuple([ctc0, ctc6, ctc0, ctc0, ctc6, ctc0, ctc3, ctc1, ctc0, ctc0]);
  const ctc8 = stdlib.T_Tuple([ctc0, ctc6, ctc0, ctc0, ctc6, ctc0, ctc3, ctc1, ctc0]);
  const ctc9 = stdlib.T_Tuple([ctc0, ctc6, ctc0, ctc0, ctc6, ctc0, ctc3, ctc0, ctc0]);
  const ctc10 = stdlib.T_Tuple([ctc0, ctc6, ctc0, ctc0, ctc6, ctc0, ctc3, ctc0]);
  const ctc11 = stdlib.T_Tuple([ctc0, ctc6, ctc0, ctc0, ctc6, ctc0, ctc0, ctc0]);
  const ctc12 = stdlib.T_Tuple([ctc0, ctc6, ctc0, ctc0, ctc6, ctc0, ctc0]);
  const ctc13 = stdlib.T_Tuple([ctc0, ctc6, ctc0, ctc0, ctc3, ctc6, ctc1, ctc0, ctc0]);
  const ctc14 = stdlib.T_Tuple([ctc0, ctc6, ctc0, ctc0, ctc3, ctc6, ctc1, ctc0]);
  const ctc15 = stdlib.T_Tuple([ctc0, ctc6, ctc0, ctc0, ctc3, ctc0, ctc0]);
  const ctc16 = stdlib.T_Tuple([ctc0, ctc6, ctc0, ctc0, ctc3, ctc0]);
  const ctc17 = stdlib.T_Tuple([ctc0, ctc0]);
  const ctc18 = stdlib.T_Tuple([ctc0]);
  
  
  const v51 = await ctc.creationTime();
  const v49 = stdlib.protect(ctc0, interact.DEADLINE, null);
  const v50 = stdlib.protect(ctc0, interact.wager, null);
  const v55 = stdlib.protect(ctc1, await interact.getBatch(), {
    at: './tut3.rsh:71:47:application',
    fs: ['at ./tut3.rsh:68:13:application call to [unknown function] (defined at: ./tut3.rsh:68:17:function exp)'],
    msg: 'getBatch',
    who: 'Alice'
    });
  const v57 = stdlib.protect(ctc0, await interact.random(), {
    at: 'reach standard library:60:31:application',
    fs: ['at ./tut3.rsh:72:74:application call to "makeCommitment" (defined at: reach standard library:59:8:function exp)', 'at ./tut3.rsh:68:13:application call to [unknown function] (defined at: ./tut3.rsh:68:17:function exp)'],
    msg: 'random',
    who: 'Alice'
    });
  const v58 = stdlib.digest(ctc2, [v57, v55]);
  const txn1 = await (ctc.sendrecv(1, 3, stdlib.checkedBigNumberify('./tut3.rsh:76:9:dot', stdlib.UInt_max, 0), [ctc0, ctc0, ctc0, ctc3], [v51, v50, v49, v58], [v50, []], [ctc0, ctc0, ctc3], true, true, false, (async (txn1) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.digest(ctc17, [stdlib.checkedBigNumberify('./tut3.rsh:76:9:dot', stdlib.UInt_max, 0), v51]);
    sim_r.prevSt_noPrevTime = stdlib.digest(ctc18, [stdlib.checkedBigNumberify('./tut3.rsh:76:9:dot', stdlib.UInt_max, 0)]);
    const [v61, v62, v63] = txn1.data;
    const v66 = txn1.time;
    const v60 = txn1.from;
    
    stdlib.assert(true, {
      at: './tut3.rsh:76:9:dot',
      fs: [],
      msg: null,
      who: 'Alice'
      });
    const v65 = stdlib.add(stdlib.checkedBigNumberify('./tut3.rsh:compileDApp', stdlib.UInt_max, 0), v61);
    sim_r.txns.push({
      amt: v61,
      kind: 'to',
      tok: undefined
      });
    stdlib.assert(true, {
      at: './tut3.rsh:76:9:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Alice'
      });
    sim_r.nextSt = stdlib.digest(ctc15, [stdlib.checkedBigNumberify('./tut3.rsh:78:15:after expr stmt semicolon', stdlib.UInt_max, 1), v60, v61, v62, v63, v65, v66]);
    sim_r.nextSt_noTime = stdlib.digest(ctc16, [stdlib.checkedBigNumberify('./tut3.rsh:78:15:after expr stmt semicolon', stdlib.UInt_max, 1), v60, v61, v62, v63, v65]);
    sim_r.isHalt = false;
    
    return sim_r;
    })));
  const [v61, v62, v63] = txn1.data;
  const v66 = txn1.time;
  const v60 = txn1.from;
  stdlib.assert(true, {
    at: './tut3.rsh:76:9:dot',
    fs: [],
    msg: null,
    who: 'Alice'
    });
  const v65 = stdlib.add(stdlib.checkedBigNumberify('./tut3.rsh:compileDApp', stdlib.UInt_max, 0), v61);
  ;
  stdlib.assert(true, {
    at: './tut3.rsh:76:9:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Alice'
    });
  const txn2 = await (ctc.recv(2, 1, [ctc1], false, v62));
  if (txn2.didTimeout) {
    const txn3 = await (ctc.sendrecv(3, 0, stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 5), [ctc6, ctc0, ctc0, ctc3, ctc0, ctc0], [v60, v61, v62, v63, v65, v66], [stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), []], [], true, true, false, (async (txn3) => {
      const sim_r = { txns: [] };
      sim_r.prevSt = stdlib.digest(ctc15, [stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 1), v60, v61, v62, v63, v65, v66]);
      sim_r.prevSt_noPrevTime = stdlib.digest(ctc16, [stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 1), v60, v61, v62, v63, v65]);
      const [] = txn3.data;
      const v604 = txn3.time;
      const v600 = txn3.from;
      
      stdlib.assert(true, {
        at: 'reach standard library:209:7:dot',
        fs: ['at ./tut3.rsh:87:41:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
        msg: null,
        who: 'Alice'
        });
      const v602 = stdlib.add(v65, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
      sim_r.txns.push({
        amt: stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0),
        kind: 'to',
        tok: undefined
        });
      const v603 = stdlib.addressEq(v60, v600);
      stdlib.assert(v603, {
        at: 'reach standard library:209:7:dot',
        fs: ['at ./tut3.rsh:87:41:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
        msg: 'sender correct',
        who: 'Alice'
        });
      sim_r.txns.push({
        amt: v602,
        kind: 'from',
        to: v60,
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
    const [] = txn3.data;
    const v604 = txn3.time;
    const v600 = txn3.from;
    stdlib.assert(true, {
      at: 'reach standard library:209:7:dot',
      fs: ['at ./tut3.rsh:87:41:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
      msg: null,
      who: 'Alice'
      });
    const v602 = stdlib.add(v65, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
    ;
    const v603 = stdlib.addressEq(v60, v600);
    stdlib.assert(v603, {
      at: 'reach standard library:209:7:dot',
      fs: ['at ./tut3.rsh:87:41:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
      msg: 'sender correct',
      who: 'Alice'
      });
    ;
    stdlib.protect(ctc4, await interact.informTimeout(), {
      at: './tut3.rsh:66:33:application',
      fs: ['at ./tut3.rsh:65:13:application call to [unknown function] (defined at: ./tut3.rsh:65:25:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./tut3.rsh:64:32:function exp)', 'at ./tut3.rsh:87:41:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
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
      at: './tut3.rsh:85:9:dot',
      fs: [],
      msg: null,
      who: 'Alice'
      });
    const v74 = stdlib.add(v65, v61);
    ;
    stdlib.assert(true, {
      at: './tut3.rsh:85:9:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Alice'
      });
    const txn3 = await (ctc.sendrecv(4, 2, stdlib.checkedBigNumberify('./tut3.rsh:93:9:dot', stdlib.UInt_max, 7), [ctc6, ctc0, ctc0, ctc3, ctc6, ctc1, ctc0, ctc0, ctc0, ctc1], [v60, v61, v62, v63, v71, v72, v74, v75, v57, v55], [stdlib.checkedBigNumberify('./tut3.rsh:decimal', stdlib.UInt_max, 0), []], [ctc0, ctc1], true, true, v62, (async (txn3) => {
      const sim_r = { txns: [] };
      sim_r.prevSt = stdlib.digest(ctc13, [stdlib.checkedBigNumberify('./tut3.rsh:93:9:dot', stdlib.UInt_max, 2), v60, v61, v62, v63, v71, v72, v74, v75]);
      sim_r.prevSt_noPrevTime = stdlib.digest(ctc14, [stdlib.checkedBigNumberify('./tut3.rsh:93:9:dot', stdlib.UInt_max, 2), v60, v61, v62, v63, v71, v72, v74]);
      const [v80, v81] = txn3.data;
      const v85 = txn3.time;
      const v79 = txn3.from;
      
      stdlib.assert(true, {
        at: './tut3.rsh:93:9:dot',
        fs: [],
        msg: null,
        who: 'Alice'
        });
      const v83 = stdlib.add(v74, stdlib.checkedBigNumberify('./tut3.rsh:decimal', stdlib.UInt_max, 0));
      sim_r.txns.push({
        amt: stdlib.checkedBigNumberify('./tut3.rsh:decimal', stdlib.UInt_max, 0),
        kind: 'to',
        tok: undefined
        });
      const v84 = stdlib.addressEq(v60, v79);
      stdlib.assert(v84, {
        at: './tut3.rsh:93:9:dot',
        fs: [],
        msg: 'sender correct',
        who: 'Alice'
        });
      const v87 = stdlib.digest(ctc2, [v80, v81]);
      const v88 = stdlib.digestEq(v63, v87);
      stdlib.assert(v88, {
        at: 'reach standard library:65:17:application',
        fs: ['at ./tut3.rsh:95:22:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)'],
        msg: null,
        who: 'Alice'
        });
      const v105 = v81[stdlib.checkedBigNumberify('./tut3.rsh:39:18:array ref', stdlib.UInt_max, 0)];
      const v106 = v72[stdlib.checkedBigNumberify('./tut3.rsh:39:29:array ref', stdlib.UInt_max, 0)];
      const v108 = stdlib.sub(stdlib.checkedBigNumberify('./tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v106);
      const v109 = stdlib.add(v105, v108);
      const v110 = stdlib.mod(v109, stdlib.checkedBigNumberify('./tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v114 = v81[stdlib.checkedBigNumberify('./tut3.rsh:39:18:array ref', stdlib.UInt_max, 1)];
      const v115 = v72[stdlib.checkedBigNumberify('./tut3.rsh:39:29:array ref', stdlib.UInt_max, 1)];
      const v117 = stdlib.sub(stdlib.checkedBigNumberify('./tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v115);
      const v118 = stdlib.add(v114, v117);
      const v119 = stdlib.mod(v118, stdlib.checkedBigNumberify('./tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v123 = v81[stdlib.checkedBigNumberify('./tut3.rsh:39:18:array ref', stdlib.UInt_max, 2)];
      const v124 = v72[stdlib.checkedBigNumberify('./tut3.rsh:39:29:array ref', stdlib.UInt_max, 2)];
      const v126 = stdlib.sub(stdlib.checkedBigNumberify('./tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v124);
      const v127 = stdlib.add(v123, v126);
      const v128 = stdlib.mod(v127, stdlib.checkedBigNumberify('./tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v132 = v81[stdlib.checkedBigNumberify('./tut3.rsh:39:18:array ref', stdlib.UInt_max, 3)];
      const v133 = v72[stdlib.checkedBigNumberify('./tut3.rsh:39:29:array ref', stdlib.UInt_max, 3)];
      const v135 = stdlib.sub(stdlib.checkedBigNumberify('./tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v133);
      const v136 = stdlib.add(v132, v135);
      const v137 = stdlib.mod(v136, stdlib.checkedBigNumberify('./tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v141 = v81[stdlib.checkedBigNumberify('./tut3.rsh:39:18:array ref', stdlib.UInt_max, 4)];
      const v142 = v72[stdlib.checkedBigNumberify('./tut3.rsh:39:29:array ref', stdlib.UInt_max, 4)];
      const v144 = stdlib.sub(stdlib.checkedBigNumberify('./tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v142);
      const v145 = stdlib.add(v141, v144);
      const v146 = stdlib.mod(v145, stdlib.checkedBigNumberify('./tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v159 = stdlib.eq(v110, stdlib.checkedBigNumberify('./tut3.rsh:makeEnum', stdlib.UInt_max, 1));
      const v160 = v159 ? v119 : v110;
      const v163 = stdlib.eq(v160, stdlib.checkedBigNumberify('./tut3.rsh:makeEnum', stdlib.UInt_max, 1));
      const v164 = v163 ? v128 : v160;
      const v167 = stdlib.eq(v164, stdlib.checkedBigNumberify('./tut3.rsh:makeEnum', stdlib.UInt_max, 1));
      const v168 = v167 ? v137 : v164;
      const v171 = stdlib.eq(v168, stdlib.checkedBigNumberify('./tut3.rsh:makeEnum', stdlib.UInt_max, 1));
      const v172 = v171 ? v146 : v168;
      const v175 = v172;
      const v176 = stdlib.checkedBigNumberify('./tut3.rsh:97:70:decimal', stdlib.UInt_max, 0);
      const v619 = v85;
      const v621 = v83;
      
      if ((() => {
        const v186 = stdlib.eq(v175, stdlib.checkedBigNumberify('./tut3.rsh:makeEnum', stdlib.UInt_max, 1));
        
        return v186;})()) {
        const v187 = stdlib.mod(v176, stdlib.checkedBigNumberify('./tut3.rsh:126:21:decimal', stdlib.UInt_max, 2));
        const v188 = stdlib.eq(v187, stdlib.checkedBigNumberify('./tut3.rsh:126:26:decimal', stdlib.UInt_max, 0));
        if (v188) {
          sim_r.nextSt = stdlib.digest(ctc11, [stdlib.checkedBigNumberify('./tut3.rsh:101:19:after expr stmt semicolon', stdlib.UInt_max, 6), v60, v61, v62, v71, v176, v619, v621]);
          sim_r.nextSt_noTime = stdlib.digest(ctc12, [stdlib.checkedBigNumberify('./tut3.rsh:101:19:after expr stmt semicolon', stdlib.UInt_max, 6), v60, v61, v62, v71, v176, v621]);
          sim_r.isHalt = false;
          }
        else {
          sim_r.nextSt = stdlib.digest(ctc11, [stdlib.checkedBigNumberify('./tut3.rsh:101:19:after expr stmt semicolon', stdlib.UInt_max, 6), v60, v61, v62, v71, v176, v619, v621]);
          sim_r.nextSt_noTime = stdlib.digest(ctc12, [stdlib.checkedBigNumberify('./tut3.rsh:101:19:after expr stmt semicolon', stdlib.UInt_max, 6), v60, v61, v62, v71, v176, v621]);
          sim_r.isHalt = false;
          }}
      else {
        const v561 = stdlib.eq(v175, stdlib.checkedBigNumberify('./tut3.rsh:makeEnum', stdlib.UInt_max, 2));
        const v564 = stdlib.mul(stdlib.checkedBigNumberify('./tut3.rsh:138:16:decimal', stdlib.UInt_max, 2), v61);
        const v566 = v561 ? v60 : v71;
        sim_r.txns.push({
          amt: v564,
          kind: 'from',
          to: v566,
          tok: undefined
          });
        sim_r.txns.push({
          kind: 'halt',
          tok: undefined
          })
        sim_r.nextSt = stdlib.digest(ctc5, []);
        sim_r.nextSt_noTime = stdlib.digest(ctc5, []);
        sim_r.isHalt = true;
        }
      return sim_r;
      })));
    if (txn3.didTimeout) {
      const txn4 = await (ctc.recv(5, 0, [], false, false));
      const [] = txn4.data;
      const v584 = txn4.time;
      const v580 = txn4.from;
      stdlib.assert(true, {
        at: 'reach standard library:209:7:dot',
        fs: ['at ./tut3.rsh:94:40:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
        msg: null,
        who: 'Alice'
        });
      const v582 = stdlib.add(v74, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
      ;
      const v583 = stdlib.addressEq(v71, v580);
      stdlib.assert(v583, {
        at: 'reach standard library:209:7:dot',
        fs: ['at ./tut3.rsh:94:40:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
        msg: 'sender correct',
        who: 'Alice'
        });
      ;
      stdlib.protect(ctc4, await interact.informTimeout(), {
        at: './tut3.rsh:66:33:application',
        fs: ['at ./tut3.rsh:65:13:application call to [unknown function] (defined at: ./tut3.rsh:65:25:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./tut3.rsh:64:32:function exp)', 'at ./tut3.rsh:94:40:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
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
        at: './tut3.rsh:93:9:dot',
        fs: [],
        msg: null,
        who: 'Alice'
        });
      const v83 = stdlib.add(v74, stdlib.checkedBigNumberify('./tut3.rsh:decimal', stdlib.UInt_max, 0));
      ;
      const v84 = stdlib.addressEq(v60, v79);
      stdlib.assert(v84, {
        at: './tut3.rsh:93:9:dot',
        fs: [],
        msg: 'sender correct',
        who: 'Alice'
        });
      const v87 = stdlib.digest(ctc2, [v80, v81]);
      const v88 = stdlib.digestEq(v63, v87);
      stdlib.assert(v88, {
        at: 'reach standard library:65:17:application',
        fs: ['at ./tut3.rsh:95:22:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)'],
        msg: null,
        who: 'Alice'
        });
      const v105 = v81[stdlib.checkedBigNumberify('./tut3.rsh:39:18:array ref', stdlib.UInt_max, 0)];
      const v106 = v72[stdlib.checkedBigNumberify('./tut3.rsh:39:29:array ref', stdlib.UInt_max, 0)];
      const v108 = stdlib.sub(stdlib.checkedBigNumberify('./tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v106);
      const v109 = stdlib.add(v105, v108);
      const v110 = stdlib.mod(v109, stdlib.checkedBigNumberify('./tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v114 = v81[stdlib.checkedBigNumberify('./tut3.rsh:39:18:array ref', stdlib.UInt_max, 1)];
      const v115 = v72[stdlib.checkedBigNumberify('./tut3.rsh:39:29:array ref', stdlib.UInt_max, 1)];
      const v117 = stdlib.sub(stdlib.checkedBigNumberify('./tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v115);
      const v118 = stdlib.add(v114, v117);
      const v119 = stdlib.mod(v118, stdlib.checkedBigNumberify('./tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v123 = v81[stdlib.checkedBigNumberify('./tut3.rsh:39:18:array ref', stdlib.UInt_max, 2)];
      const v124 = v72[stdlib.checkedBigNumberify('./tut3.rsh:39:29:array ref', stdlib.UInt_max, 2)];
      const v126 = stdlib.sub(stdlib.checkedBigNumberify('./tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v124);
      const v127 = stdlib.add(v123, v126);
      const v128 = stdlib.mod(v127, stdlib.checkedBigNumberify('./tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v132 = v81[stdlib.checkedBigNumberify('./tut3.rsh:39:18:array ref', stdlib.UInt_max, 3)];
      const v133 = v72[stdlib.checkedBigNumberify('./tut3.rsh:39:29:array ref', stdlib.UInt_max, 3)];
      const v135 = stdlib.sub(stdlib.checkedBigNumberify('./tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v133);
      const v136 = stdlib.add(v132, v135);
      const v137 = stdlib.mod(v136, stdlib.checkedBigNumberify('./tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v141 = v81[stdlib.checkedBigNumberify('./tut3.rsh:39:18:array ref', stdlib.UInt_max, 4)];
      const v142 = v72[stdlib.checkedBigNumberify('./tut3.rsh:39:29:array ref', stdlib.UInt_max, 4)];
      const v144 = stdlib.sub(stdlib.checkedBigNumberify('./tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v142);
      const v145 = stdlib.add(v141, v144);
      const v146 = stdlib.mod(v145, stdlib.checkedBigNumberify('./tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v159 = stdlib.eq(v110, stdlib.checkedBigNumberify('./tut3.rsh:makeEnum', stdlib.UInt_max, 1));
      const v160 = v159 ? v119 : v110;
      const v163 = stdlib.eq(v160, stdlib.checkedBigNumberify('./tut3.rsh:makeEnum', stdlib.UInt_max, 1));
      const v164 = v163 ? v128 : v160;
      const v167 = stdlib.eq(v164, stdlib.checkedBigNumberify('./tut3.rsh:makeEnum', stdlib.UInt_max, 1));
      const v168 = v167 ? v137 : v164;
      const v171 = stdlib.eq(v168, stdlib.checkedBigNumberify('./tut3.rsh:makeEnum', stdlib.UInt_max, 1));
      const v172 = v171 ? v146 : v168;
      let v175 = v172;
      let v176 = stdlib.checkedBigNumberify('./tut3.rsh:97:70:decimal', stdlib.UInt_max, 0);
      let v619 = v85;
      let v621 = v83;
      
      while ((() => {
        const v186 = stdlib.eq(v175, stdlib.checkedBigNumberify('./tut3.rsh:makeEnum', stdlib.UInt_max, 1));
        
        return v186;})()) {
        const v187 = stdlib.mod(v176, stdlib.checkedBigNumberify('./tut3.rsh:126:21:decimal', stdlib.UInt_max, 2));
        const v188 = stdlib.eq(v187, stdlib.checkedBigNumberify('./tut3.rsh:126:26:decimal', stdlib.UInt_max, 0));
        if (v188) {
          const txn4 = await (ctc.recv(8, 1, [ctc3], false, v62));
          if (txn4.didTimeout) {
            const txn5 = await (ctc.sendrecv(9, 0, stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 5), [ctc6, ctc0, ctc0, ctc6, ctc0, ctc0, ctc0], [v60, v61, v62, v71, v176, v619, v621], [stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), []], [], true, true, false, (async (txn5) => {
              const sim_r = { txns: [] };
              sim_r.prevSt = stdlib.digest(ctc11, [stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 6), v60, v61, v62, v71, v176, v619, v621]);
              sim_r.prevSt_noPrevTime = stdlib.digest(ctc12, [stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 6), v60, v61, v62, v71, v176, v621]);
              const [] = txn5.data;
              const v274 = txn5.time;
              const v270 = txn5.from;
              
              stdlib.assert(true, {
                at: 'reach standard library:209:7:dot',
                fs: ['at ./tut3.rsh:108:45:application call to "closeTo" (defined at: reach standard library:207:8:function exp)', 'at ./tut3.rsh:127:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                msg: null,
                who: 'Alice'
                });
              const v272 = stdlib.add(v621, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
              sim_r.txns.push({
                amt: stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0),
                kind: 'to',
                tok: undefined
                });
              const v273 = stdlib.addressEq(v60, v270);
              stdlib.assert(v273, {
                at: 'reach standard library:209:7:dot',
                fs: ['at ./tut3.rsh:108:45:application call to "closeTo" (defined at: reach standard library:207:8:function exp)', 'at ./tut3.rsh:127:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                msg: 'sender correct',
                who: 'Alice'
                });
              sim_r.txns.push({
                amt: v272,
                kind: 'from',
                to: v60,
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
            const v274 = txn5.time;
            const v270 = txn5.from;
            stdlib.assert(true, {
              at: 'reach standard library:209:7:dot',
              fs: ['at ./tut3.rsh:108:45:application call to "closeTo" (defined at: reach standard library:207:8:function exp)', 'at ./tut3.rsh:127:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
              msg: null,
              who: 'Alice'
              });
            const v272 = stdlib.add(v621, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
            ;
            const v273 = stdlib.addressEq(v60, v270);
            stdlib.assert(v273, {
              at: 'reach standard library:209:7:dot',
              fs: ['at ./tut3.rsh:108:45:application call to "closeTo" (defined at: reach standard library:207:8:function exp)', 'at ./tut3.rsh:127:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
              msg: 'sender correct',
              who: 'Alice'
              });
            ;
            stdlib.protect(ctc4, await interact.informTimeout(), {
              at: './tut3.rsh:66:33:application',
              fs: ['at ./tut3.rsh:65:13:application call to [unknown function] (defined at: ./tut3.rsh:65:25:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./tut3.rsh:64:32:function exp)', 'at ./tut3.rsh:108:45:application call to "closeTo" (defined at: reach standard library:207:8:function exp)', 'at ./tut3.rsh:127:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
              msg: 'informTimeout',
              who: 'Alice'
              });
            return;
            }
          else {
            const [v199] = txn4.data;
            const v203 = txn4.time;
            const v198 = txn4.from;
            stdlib.assert(true, {
              at: './tut3.rsh:107:17:dot',
              fs: ['at ./tut3.rsh:127:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
              msg: null,
              who: 'Alice'
              });
            const v201 = stdlib.add(v621, stdlib.checkedBigNumberify('./tut3.rsh:decimal', stdlib.UInt_max, 0));
            ;
            const v202 = stdlib.addressEq(v71, v198);
            stdlib.assert(v202, {
              at: './tut3.rsh:107:17:dot',
              fs: ['at ./tut3.rsh:127:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
              msg: 'sender correct',
              who: 'Alice'
              });
            const v207 = stdlib.protect(ctc1, await interact.getBatch(), {
              at: './tut3.rsh:113:61:application',
              fs: ['at ./tut3.rsh:112:22:application call to [unknown function] (defined at: ./tut3.rsh:112:26:function exp)', 'at ./tut3.rsh:127:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
              msg: 'getBatch',
              who: 'Alice'
              });
            const txn5 = await (ctc.sendrecv(10, 1, stdlib.checkedBigNumberify('./tut3.rsh:114:18:dot', stdlib.UInt_max, 7), [ctc6, ctc0, ctc0, ctc6, ctc0, ctc3, ctc0, ctc0, ctc1], [v60, v61, v62, v71, v176, v199, v201, v203, v207], [stdlib.checkedBigNumberify('./tut3.rsh:decimal', stdlib.UInt_max, 0), []], [ctc1], true, true, v62, (async (txn5) => {
              const sim_r = { txns: [] };
              sim_r.prevSt = stdlib.digest(ctc9, [stdlib.checkedBigNumberify('./tut3.rsh:114:18:dot', stdlib.UInt_max, 8), v60, v61, v62, v71, v176, v199, v201, v203]);
              sim_r.prevSt_noPrevTime = stdlib.digest(ctc10, [stdlib.checkedBigNumberify('./tut3.rsh:114:18:dot', stdlib.UInt_max, 8), v60, v61, v62, v71, v176, v199, v201]);
              const [v209] = txn5.data;
              const v213 = txn5.time;
              const v208 = txn5.from;
              
              stdlib.assert(true, {
                at: './tut3.rsh:114:18:dot',
                fs: ['at ./tut3.rsh:127:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                msg: null,
                who: 'Alice'
                });
              const v211 = stdlib.add(v201, stdlib.checkedBigNumberify('./tut3.rsh:decimal', stdlib.UInt_max, 0));
              sim_r.txns.push({
                amt: stdlib.checkedBigNumberify('./tut3.rsh:decimal', stdlib.UInt_max, 0),
                kind: 'to',
                tok: undefined
                });
              const v212 = stdlib.addressEq(v60, v208);
              stdlib.assert(v212, {
                at: './tut3.rsh:114:18:dot',
                fs: ['at ./tut3.rsh:127:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                msg: 'sender correct',
                who: 'Alice'
                });
              sim_r.nextSt = stdlib.digest(ctc7, [stdlib.checkedBigNumberify('./tut3.rsh:116:19:after expr stmt semicolon', stdlib.UInt_max, 10), v60, v61, v62, v71, v176, v199, v209, v211, v213]);
              sim_r.nextSt_noTime = stdlib.digest(ctc8, [stdlib.checkedBigNumberify('./tut3.rsh:116:19:after expr stmt semicolon', stdlib.UInt_max, 10), v60, v61, v62, v71, v176, v199, v209, v211]);
              sim_r.isHalt = false;
              
              return sim_r;
              })));
            if (txn5.didTimeout) {
              const txn6 = await (ctc.recv(11, 0, [], false, false));
              const [] = txn6.data;
              const v254 = txn6.time;
              const v250 = txn6.from;
              stdlib.assert(true, {
                at: 'reach standard library:209:7:dot',
                fs: ['at ./tut3.rsh:115:45:application call to "closeTo" (defined at: reach standard library:207:8:function exp)', 'at ./tut3.rsh:127:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                msg: null,
                who: 'Alice'
                });
              const v252 = stdlib.add(v201, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
              ;
              const v253 = stdlib.addressEq(v71, v250);
              stdlib.assert(v253, {
                at: 'reach standard library:209:7:dot',
                fs: ['at ./tut3.rsh:115:45:application call to "closeTo" (defined at: reach standard library:207:8:function exp)', 'at ./tut3.rsh:127:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                msg: 'sender correct',
                who: 'Alice'
                });
              ;
              stdlib.protect(ctc4, await interact.informTimeout(), {
                at: './tut3.rsh:66:33:application',
                fs: ['at ./tut3.rsh:65:13:application call to [unknown function] (defined at: ./tut3.rsh:65:25:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./tut3.rsh:64:32:function exp)', 'at ./tut3.rsh:115:45:application call to "closeTo" (defined at: reach standard library:207:8:function exp)', 'at ./tut3.rsh:127:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                msg: 'informTimeout',
                who: 'Alice'
                });
              return;
              }
            else {
              const [v209] = txn5.data;
              const v213 = txn5.time;
              const v208 = txn5.from;
              stdlib.assert(true, {
                at: './tut3.rsh:114:18:dot',
                fs: ['at ./tut3.rsh:127:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                msg: null,
                who: 'Alice'
                });
              const v211 = stdlib.add(v201, stdlib.checkedBigNumberify('./tut3.rsh:decimal', stdlib.UInt_max, 0));
              ;
              const v212 = stdlib.addressEq(v60, v208);
              stdlib.assert(v212, {
                at: './tut3.rsh:114:18:dot',
                fs: ['at ./tut3.rsh:127:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                msg: 'sender correct',
                who: 'Alice'
                });
              const txn6 = await (ctc.recv(12, 2, [ctc0, ctc1], false, v62));
              if (txn6.didTimeout) {
                const txn7 = await (ctc.sendrecv(13, 0, stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 8), [ctc6, ctc0, ctc0, ctc6, ctc0, ctc3, ctc1, ctc0, ctc0], [v60, v61, v62, v71, v176, v199, v209, v211, v213], [stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), []], [], true, true, false, (async (txn7) => {
                  const sim_r = { txns: [] };
                  sim_r.prevSt = stdlib.digest(ctc7, [stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 10), v60, v61, v62, v71, v176, v199, v209, v211, v213]);
                  sim_r.prevSt_noPrevTime = stdlib.digest(ctc8, [stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 10), v60, v61, v62, v71, v176, v199, v209, v211]);
                  const [] = txn7.data;
                  const v234 = txn7.time;
                  const v230 = txn7.from;
                  
                  stdlib.assert(true, {
                    at: 'reach standard library:209:7:dot',
                    fs: ['at ./tut3.rsh:121:45:application call to "closeTo" (defined at: reach standard library:207:8:function exp)', 'at ./tut3.rsh:127:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                    msg: null,
                    who: 'Alice'
                    });
                  const v232 = stdlib.add(v211, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
                  sim_r.txns.push({
                    amt: stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0),
                    kind: 'to',
                    tok: undefined
                    });
                  const v233 = stdlib.addressEq(v60, v230);
                  stdlib.assert(v233, {
                    at: 'reach standard library:209:7:dot',
                    fs: ['at ./tut3.rsh:121:45:application call to "closeTo" (defined at: reach standard library:207:8:function exp)', 'at ./tut3.rsh:127:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                    msg: 'sender correct',
                    who: 'Alice'
                    });
                  sim_r.txns.push({
                    amt: v232,
                    kind: 'from',
                    to: v60,
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
                const v234 = txn7.time;
                const v230 = txn7.from;
                stdlib.assert(true, {
                  at: 'reach standard library:209:7:dot',
                  fs: ['at ./tut3.rsh:121:45:application call to "closeTo" (defined at: reach standard library:207:8:function exp)', 'at ./tut3.rsh:127:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                  msg: null,
                  who: 'Alice'
                  });
                const v232 = stdlib.add(v211, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
                ;
                const v233 = stdlib.addressEq(v60, v230);
                stdlib.assert(v233, {
                  at: 'reach standard library:209:7:dot',
                  fs: ['at ./tut3.rsh:121:45:application call to "closeTo" (defined at: reach standard library:207:8:function exp)', 'at ./tut3.rsh:127:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                  msg: 'sender correct',
                  who: 'Alice'
                  });
                ;
                stdlib.protect(ctc4, await interact.informTimeout(), {
                  at: './tut3.rsh:66:33:application',
                  fs: ['at ./tut3.rsh:65:13:application call to [unknown function] (defined at: ./tut3.rsh:65:25:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./tut3.rsh:64:32:function exp)', 'at ./tut3.rsh:121:45:application call to "closeTo" (defined at: reach standard library:207:8:function exp)', 'at ./tut3.rsh:127:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                  msg: 'informTimeout',
                  who: 'Alice'
                  });
                return;
                }
              else {
                const [v218, v219] = txn6.data;
                const v223 = txn6.time;
                const v217 = txn6.from;
                stdlib.assert(true, {
                  at: './tut3.rsh:120:17:dot',
                  fs: ['at ./tut3.rsh:127:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                  msg: null,
                  who: 'Alice'
                  });
                const v221 = stdlib.add(v211, stdlib.checkedBigNumberify('./tut3.rsh:decimal', stdlib.UInt_max, 0));
                ;
                const v222 = stdlib.addressEq(v71, v217);
                stdlib.assert(v222, {
                  at: './tut3.rsh:120:17:dot',
                  fs: ['at ./tut3.rsh:127:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                  msg: 'sender correct',
                  who: 'Alice'
                  });
                const v225 = stdlib.digest(ctc2, [v218, v219]);
                const v226 = stdlib.digestEq(v199, v225);
                stdlib.assert(v226, {
                  at: 'reach standard library:65:17:application',
                  fs: ['at ./tut3.rsh:122:26:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)', 'at ./tut3.rsh:127:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                  msg: null,
                  who: 'Alice'
                  });
                const v304 = v209[stdlib.checkedBigNumberify('./tut3.rsh:39:18:array ref', stdlib.UInt_max, 0)];
                const v305 = v219[stdlib.checkedBigNumberify('./tut3.rsh:39:29:array ref', stdlib.UInt_max, 0)];
                const v307 = stdlib.sub(stdlib.checkedBigNumberify('./tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v305);
                const v308 = stdlib.add(v304, v307);
                const v309 = stdlib.mod(v308, stdlib.checkedBigNumberify('./tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v313 = v209[stdlib.checkedBigNumberify('./tut3.rsh:39:18:array ref', stdlib.UInt_max, 1)];
                const v314 = v219[stdlib.checkedBigNumberify('./tut3.rsh:39:29:array ref', stdlib.UInt_max, 1)];
                const v316 = stdlib.sub(stdlib.checkedBigNumberify('./tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v314);
                const v317 = stdlib.add(v313, v316);
                const v318 = stdlib.mod(v317, stdlib.checkedBigNumberify('./tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v322 = v209[stdlib.checkedBigNumberify('./tut3.rsh:39:18:array ref', stdlib.UInt_max, 2)];
                const v323 = v219[stdlib.checkedBigNumberify('./tut3.rsh:39:29:array ref', stdlib.UInt_max, 2)];
                const v325 = stdlib.sub(stdlib.checkedBigNumberify('./tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v323);
                const v326 = stdlib.add(v322, v325);
                const v327 = stdlib.mod(v326, stdlib.checkedBigNumberify('./tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v331 = v209[stdlib.checkedBigNumberify('./tut3.rsh:39:18:array ref', stdlib.UInt_max, 3)];
                const v332 = v219[stdlib.checkedBigNumberify('./tut3.rsh:39:29:array ref', stdlib.UInt_max, 3)];
                const v334 = stdlib.sub(stdlib.checkedBigNumberify('./tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v332);
                const v335 = stdlib.add(v331, v334);
                const v336 = stdlib.mod(v335, stdlib.checkedBigNumberify('./tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v340 = v209[stdlib.checkedBigNumberify('./tut3.rsh:39:18:array ref', stdlib.UInt_max, 4)];
                const v341 = v219[stdlib.checkedBigNumberify('./tut3.rsh:39:29:array ref', stdlib.UInt_max, 4)];
                const v343 = stdlib.sub(stdlib.checkedBigNumberify('./tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v341);
                const v344 = stdlib.add(v340, v343);
                const v345 = stdlib.mod(v344, stdlib.checkedBigNumberify('./tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v358 = stdlib.eq(v309, stdlib.checkedBigNumberify('./tut3.rsh:makeEnum', stdlib.UInt_max, 1));
                const v359 = v358 ? v318 : v309;
                const v362 = stdlib.eq(v359, stdlib.checkedBigNumberify('./tut3.rsh:makeEnum', stdlib.UInt_max, 1));
                const v363 = v362 ? v327 : v359;
                const v366 = stdlib.eq(v363, stdlib.checkedBigNumberify('./tut3.rsh:makeEnum', stdlib.UInt_max, 1));
                const v367 = v366 ? v336 : v363;
                const v370 = stdlib.eq(v367, stdlib.checkedBigNumberify('./tut3.rsh:makeEnum', stdlib.UInt_max, 1));
                const v371 = v370 ? v345 : v367;
                const v374 = stdlib.add(v176, stdlib.checkedBigNumberify('./tut3.rsh:128:67:decimal', stdlib.UInt_max, 1));
                const cv175 = v371;
                const cv176 = v374;
                const cv619 = v223;
                const cv621 = v221;
                
                v175 = cv175;
                v176 = cv176;
                v619 = cv619;
                v621 = cv621;
                
                continue;}
              }
            }
          }
        else {
          const v379 = stdlib.protect(ctc1, await interact.getBatch(), {
            at: './tut3.rsh:104:50:application',
            fs: ['at ./tut3.rsh:103:21:application call to [unknown function] (defined at: ./tut3.rsh:103:25:function exp)', 'at ./tut3.rsh:131:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
            msg: 'getBatch',
            who: 'Alice'
            });
          const v381 = stdlib.protect(ctc0, await interact.random(), {
            at: 'reach standard library:60:31:application',
            fs: ['at ./tut3.rsh:105:62:application call to "makeCommitment" (defined at: reach standard library:59:8:function exp)', 'at ./tut3.rsh:103:21:application call to [unknown function] (defined at: ./tut3.rsh:103:25:function exp)', 'at ./tut3.rsh:131:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
            msg: 'random',
            who: 'Alice'
            });
          const v382 = stdlib.digest(ctc2, [v381, v379]);
          const txn4 = await (ctc.sendrecv(14, 1, stdlib.checkedBigNumberify('./tut3.rsh:107:17:dot', stdlib.UInt_max, 5), [ctc6, ctc0, ctc0, ctc6, ctc0, ctc0, ctc0, ctc3], [v60, v61, v62, v71, v176, v619, v621, v382], [stdlib.checkedBigNumberify('./tut3.rsh:decimal', stdlib.UInt_max, 0), []], [ctc3], true, true, v62, (async (txn4) => {
            const sim_r = { txns: [] };
            sim_r.prevSt = stdlib.digest(ctc11, [stdlib.checkedBigNumberify('./tut3.rsh:107:17:dot', stdlib.UInt_max, 6), v60, v61, v62, v71, v176, v619, v621]);
            sim_r.prevSt_noPrevTime = stdlib.digest(ctc12, [stdlib.checkedBigNumberify('./tut3.rsh:107:17:dot', stdlib.UInt_max, 6), v60, v61, v62, v71, v176, v621]);
            const [v385] = txn4.data;
            const v389 = txn4.time;
            const v384 = txn4.from;
            
            stdlib.assert(true, {
              at: './tut3.rsh:107:17:dot',
              fs: ['at ./tut3.rsh:131:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
              msg: null,
              who: 'Alice'
              });
            const v387 = stdlib.add(v621, stdlib.checkedBigNumberify('./tut3.rsh:decimal', stdlib.UInt_max, 0));
            sim_r.txns.push({
              amt: stdlib.checkedBigNumberify('./tut3.rsh:decimal', stdlib.UInt_max, 0),
              kind: 'to',
              tok: undefined
              });
            const v388 = stdlib.addressEq(v60, v384);
            stdlib.assert(v388, {
              at: './tut3.rsh:107:17:dot',
              fs: ['at ./tut3.rsh:131:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
              msg: 'sender correct',
              who: 'Alice'
              });
            sim_r.nextSt = stdlib.digest(ctc9, [stdlib.checkedBigNumberify('./tut3.rsh:109:19:after expr stmt semicolon', stdlib.UInt_max, 14), v60, v61, v62, v71, v176, v385, v387, v389]);
            sim_r.nextSt_noTime = stdlib.digest(ctc10, [stdlib.checkedBigNumberify('./tut3.rsh:109:19:after expr stmt semicolon', stdlib.UInt_max, 14), v60, v61, v62, v71, v176, v385, v387]);
            sim_r.isHalt = false;
            
            return sim_r;
            })));
          if (txn4.didTimeout) {
            const txn5 = await (ctc.recv(15, 0, [], false, false));
            const [] = txn5.data;
            const v460 = txn5.time;
            const v456 = txn5.from;
            stdlib.assert(true, {
              at: 'reach standard library:209:7:dot',
              fs: ['at ./tut3.rsh:108:45:application call to "closeTo" (defined at: reach standard library:207:8:function exp)', 'at ./tut3.rsh:131:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
              msg: null,
              who: 'Alice'
              });
            const v458 = stdlib.add(v621, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
            ;
            const v459 = stdlib.addressEq(v71, v456);
            stdlib.assert(v459, {
              at: 'reach standard library:209:7:dot',
              fs: ['at ./tut3.rsh:108:45:application call to "closeTo" (defined at: reach standard library:207:8:function exp)', 'at ./tut3.rsh:131:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
              msg: 'sender correct',
              who: 'Alice'
              });
            ;
            stdlib.protect(ctc4, await interact.informTimeout(), {
              at: './tut3.rsh:66:33:application',
              fs: ['at ./tut3.rsh:65:13:application call to [unknown function] (defined at: ./tut3.rsh:65:25:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./tut3.rsh:64:32:function exp)', 'at ./tut3.rsh:108:45:application call to "closeTo" (defined at: reach standard library:207:8:function exp)', 'at ./tut3.rsh:131:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
              msg: 'informTimeout',
              who: 'Alice'
              });
            return;
            }
          else {
            const [v385] = txn4.data;
            const v389 = txn4.time;
            const v384 = txn4.from;
            stdlib.assert(true, {
              at: './tut3.rsh:107:17:dot',
              fs: ['at ./tut3.rsh:131:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
              msg: null,
              who: 'Alice'
              });
            const v387 = stdlib.add(v621, stdlib.checkedBigNumberify('./tut3.rsh:decimal', stdlib.UInt_max, 0));
            ;
            const v388 = stdlib.addressEq(v60, v384);
            stdlib.assert(v388, {
              at: './tut3.rsh:107:17:dot',
              fs: ['at ./tut3.rsh:131:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
              msg: 'sender correct',
              who: 'Alice'
              });
            const txn5 = await (ctc.recv(16, 1, [ctc1], false, v62));
            if (txn5.didTimeout) {
              const txn6 = await (ctc.sendrecv(17, 0, stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 7), [ctc6, ctc0, ctc0, ctc6, ctc0, ctc3, ctc0, ctc0], [v60, v61, v62, v71, v176, v385, v387, v389], [stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), []], [], true, true, false, (async (txn6) => {
                const sim_r = { txns: [] };
                sim_r.prevSt = stdlib.digest(ctc9, [stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 14), v60, v61, v62, v71, v176, v385, v387, v389]);
                sim_r.prevSt_noPrevTime = stdlib.digest(ctc10, [stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 14), v60, v61, v62, v71, v176, v385, v387]);
                const [] = txn6.data;
                const v440 = txn6.time;
                const v436 = txn6.from;
                
                stdlib.assert(true, {
                  at: 'reach standard library:209:7:dot',
                  fs: ['at ./tut3.rsh:115:45:application call to "closeTo" (defined at: reach standard library:207:8:function exp)', 'at ./tut3.rsh:131:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                  msg: null,
                  who: 'Alice'
                  });
                const v438 = stdlib.add(v387, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
                sim_r.txns.push({
                  amt: stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0),
                  kind: 'to',
                  tok: undefined
                  });
                const v439 = stdlib.addressEq(v60, v436);
                stdlib.assert(v439, {
                  at: 'reach standard library:209:7:dot',
                  fs: ['at ./tut3.rsh:115:45:application call to "closeTo" (defined at: reach standard library:207:8:function exp)', 'at ./tut3.rsh:131:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                  msg: 'sender correct',
                  who: 'Alice'
                  });
                sim_r.txns.push({
                  amt: v438,
                  kind: 'from',
                  to: v60,
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
              const [] = txn6.data;
              const v440 = txn6.time;
              const v436 = txn6.from;
              stdlib.assert(true, {
                at: 'reach standard library:209:7:dot',
                fs: ['at ./tut3.rsh:115:45:application call to "closeTo" (defined at: reach standard library:207:8:function exp)', 'at ./tut3.rsh:131:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                msg: null,
                who: 'Alice'
                });
              const v438 = stdlib.add(v387, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
              ;
              const v439 = stdlib.addressEq(v60, v436);
              stdlib.assert(v439, {
                at: 'reach standard library:209:7:dot',
                fs: ['at ./tut3.rsh:115:45:application call to "closeTo" (defined at: reach standard library:207:8:function exp)', 'at ./tut3.rsh:131:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                msg: 'sender correct',
                who: 'Alice'
                });
              ;
              stdlib.protect(ctc4, await interact.informTimeout(), {
                at: './tut3.rsh:66:33:application',
                fs: ['at ./tut3.rsh:65:13:application call to [unknown function] (defined at: ./tut3.rsh:65:25:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./tut3.rsh:64:32:function exp)', 'at ./tut3.rsh:115:45:application call to "closeTo" (defined at: reach standard library:207:8:function exp)', 'at ./tut3.rsh:131:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                msg: 'informTimeout',
                who: 'Alice'
                });
              return;
              }
            else {
              const [v395] = txn5.data;
              const v399 = txn5.time;
              const v394 = txn5.from;
              stdlib.assert(true, {
                at: './tut3.rsh:114:18:dot',
                fs: ['at ./tut3.rsh:131:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                msg: null,
                who: 'Alice'
                });
              const v397 = stdlib.add(v387, stdlib.checkedBigNumberify('./tut3.rsh:decimal', stdlib.UInt_max, 0));
              ;
              const v398 = stdlib.addressEq(v71, v394);
              stdlib.assert(v398, {
                at: './tut3.rsh:114:18:dot',
                fs: ['at ./tut3.rsh:131:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                msg: 'sender correct',
                who: 'Alice'
                });
              const txn6 = await (ctc.sendrecv(18, 2, stdlib.checkedBigNumberify('./tut3.rsh:120:17:dot', stdlib.UInt_max, 8), [ctc6, ctc0, ctc0, ctc6, ctc0, ctc3, ctc1, ctc0, ctc0, ctc0, ctc1], [v60, v61, v62, v71, v176, v385, v395, v397, v399, v381, v379], [stdlib.checkedBigNumberify('./tut3.rsh:decimal', stdlib.UInt_max, 0), []], [ctc0, ctc1], true, true, v62, (async (txn6) => {
                const sim_r = { txns: [] };
                sim_r.prevSt = stdlib.digest(ctc7, [stdlib.checkedBigNumberify('./tut3.rsh:120:17:dot', stdlib.UInt_max, 16), v60, v61, v62, v71, v176, v385, v395, v397, v399]);
                sim_r.prevSt_noPrevTime = stdlib.digest(ctc8, [stdlib.checkedBigNumberify('./tut3.rsh:120:17:dot', stdlib.UInt_max, 16), v60, v61, v62, v71, v176, v385, v395, v397]);
                const [v404, v405] = txn6.data;
                const v409 = txn6.time;
                const v403 = txn6.from;
                
                stdlib.assert(true, {
                  at: './tut3.rsh:120:17:dot',
                  fs: ['at ./tut3.rsh:131:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                  msg: null,
                  who: 'Alice'
                  });
                const v407 = stdlib.add(v397, stdlib.checkedBigNumberify('./tut3.rsh:decimal', stdlib.UInt_max, 0));
                sim_r.txns.push({
                  amt: stdlib.checkedBigNumberify('./tut3.rsh:decimal', stdlib.UInt_max, 0),
                  kind: 'to',
                  tok: undefined
                  });
                const v408 = stdlib.addressEq(v60, v403);
                stdlib.assert(v408, {
                  at: './tut3.rsh:120:17:dot',
                  fs: ['at ./tut3.rsh:131:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                  msg: 'sender correct',
                  who: 'Alice'
                  });
                const v411 = stdlib.digest(ctc2, [v404, v405]);
                const v412 = stdlib.digestEq(v385, v411);
                stdlib.assert(v412, {
                  at: 'reach standard library:65:17:application',
                  fs: ['at ./tut3.rsh:122:26:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)', 'at ./tut3.rsh:131:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                  msg: null,
                  who: 'Alice'
                  });
                const v490 = v405[stdlib.checkedBigNumberify('./tut3.rsh:39:18:array ref', stdlib.UInt_max, 0)];
                const v491 = v395[stdlib.checkedBigNumberify('./tut3.rsh:39:29:array ref', stdlib.UInt_max, 0)];
                const v493 = stdlib.sub(stdlib.checkedBigNumberify('./tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v491);
                const v494 = stdlib.add(v490, v493);
                const v495 = stdlib.mod(v494, stdlib.checkedBigNumberify('./tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v499 = v405[stdlib.checkedBigNumberify('./tut3.rsh:39:18:array ref', stdlib.UInt_max, 1)];
                const v500 = v395[stdlib.checkedBigNumberify('./tut3.rsh:39:29:array ref', stdlib.UInt_max, 1)];
                const v502 = stdlib.sub(stdlib.checkedBigNumberify('./tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v500);
                const v503 = stdlib.add(v499, v502);
                const v504 = stdlib.mod(v503, stdlib.checkedBigNumberify('./tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v508 = v405[stdlib.checkedBigNumberify('./tut3.rsh:39:18:array ref', stdlib.UInt_max, 2)];
                const v509 = v395[stdlib.checkedBigNumberify('./tut3.rsh:39:29:array ref', stdlib.UInt_max, 2)];
                const v511 = stdlib.sub(stdlib.checkedBigNumberify('./tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v509);
                const v512 = stdlib.add(v508, v511);
                const v513 = stdlib.mod(v512, stdlib.checkedBigNumberify('./tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v517 = v405[stdlib.checkedBigNumberify('./tut3.rsh:39:18:array ref', stdlib.UInt_max, 3)];
                const v518 = v395[stdlib.checkedBigNumberify('./tut3.rsh:39:29:array ref', stdlib.UInt_max, 3)];
                const v520 = stdlib.sub(stdlib.checkedBigNumberify('./tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v518);
                const v521 = stdlib.add(v517, v520);
                const v522 = stdlib.mod(v521, stdlib.checkedBigNumberify('./tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v526 = v405[stdlib.checkedBigNumberify('./tut3.rsh:39:18:array ref', stdlib.UInt_max, 4)];
                const v527 = v395[stdlib.checkedBigNumberify('./tut3.rsh:39:29:array ref', stdlib.UInt_max, 4)];
                const v529 = stdlib.sub(stdlib.checkedBigNumberify('./tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v527);
                const v530 = stdlib.add(v526, v529);
                const v531 = stdlib.mod(v530, stdlib.checkedBigNumberify('./tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v544 = stdlib.eq(v495, stdlib.checkedBigNumberify('./tut3.rsh:makeEnum', stdlib.UInt_max, 1));
                const v545 = v544 ? v504 : v495;
                const v548 = stdlib.eq(v545, stdlib.checkedBigNumberify('./tut3.rsh:makeEnum', stdlib.UInt_max, 1));
                const v549 = v548 ? v513 : v545;
                const v552 = stdlib.eq(v549, stdlib.checkedBigNumberify('./tut3.rsh:makeEnum', stdlib.UInt_max, 1));
                const v553 = v552 ? v522 : v549;
                const v556 = stdlib.eq(v553, stdlib.checkedBigNumberify('./tut3.rsh:makeEnum', stdlib.UInt_max, 1));
                const v557 = v556 ? v531 : v553;
                const v560 = stdlib.add(v176, stdlib.checkedBigNumberify('./tut3.rsh:132:67:decimal', stdlib.UInt_max, 1));
                const cv175 = v557;
                const cv176 = v560;
                const cv619 = v409;
                const cv621 = v407;
                
                (() => {
                  const v175 = cv175;
                  const v176 = cv176;
                  const v619 = cv619;
                  const v621 = cv621;
                  
                  if ((() => {
                    const v186 = stdlib.eq(v175, stdlib.checkedBigNumberify('./tut3.rsh:makeEnum', stdlib.UInt_max, 1));
                    
                    return v186;})()) {
                    const v187 = stdlib.mod(v176, stdlib.checkedBigNumberify('./tut3.rsh:126:21:decimal', stdlib.UInt_max, 2));
                    const v188 = stdlib.eq(v187, stdlib.checkedBigNumberify('./tut3.rsh:126:26:decimal', stdlib.UInt_max, 0));
                    if (v188) {
                      sim_r.nextSt = stdlib.digest(ctc11, [stdlib.checkedBigNumberify('./tut3.rsh:101:19:after expr stmt semicolon', stdlib.UInt_max, 6), v60, v61, v62, v71, v176, v619, v621]);
                      sim_r.nextSt_noTime = stdlib.digest(ctc12, [stdlib.checkedBigNumberify('./tut3.rsh:101:19:after expr stmt semicolon', stdlib.UInt_max, 6), v60, v61, v62, v71, v176, v621]);
                      sim_r.isHalt = false;
                      }
                    else {
                      sim_r.nextSt = stdlib.digest(ctc11, [stdlib.checkedBigNumberify('./tut3.rsh:101:19:after expr stmt semicolon', stdlib.UInt_max, 6), v60, v61, v62, v71, v176, v619, v621]);
                      sim_r.nextSt_noTime = stdlib.digest(ctc12, [stdlib.checkedBigNumberify('./tut3.rsh:101:19:after expr stmt semicolon', stdlib.UInt_max, 6), v60, v61, v62, v71, v176, v621]);
                      sim_r.isHalt = false;
                      }}
                  else {
                    const v561 = stdlib.eq(v175, stdlib.checkedBigNumberify('./tut3.rsh:makeEnum', stdlib.UInt_max, 2));
                    const v564 = stdlib.mul(stdlib.checkedBigNumberify('./tut3.rsh:138:16:decimal', stdlib.UInt_max, 2), v61);
                    const v566 = v561 ? v60 : v71;
                    sim_r.txns.push({
                      amt: v564,
                      kind: 'from',
                      to: v566,
                      tok: undefined
                      });
                    sim_r.txns.push({
                      kind: 'halt',
                      tok: undefined
                      })
                    sim_r.nextSt = stdlib.digest(ctc5, []);
                    sim_r.nextSt_noTime = stdlib.digest(ctc5, []);
                    sim_r.isHalt = true;
                    }})();
                return sim_r;
                })));
              if (txn6.didTimeout) {
                const txn7 = await (ctc.recv(19, 0, [], false, false));
                const [] = txn7.data;
                const v420 = txn7.time;
                const v416 = txn7.from;
                stdlib.assert(true, {
                  at: 'reach standard library:209:7:dot',
                  fs: ['at ./tut3.rsh:121:45:application call to "closeTo" (defined at: reach standard library:207:8:function exp)', 'at ./tut3.rsh:131:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                  msg: null,
                  who: 'Alice'
                  });
                const v418 = stdlib.add(v397, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
                ;
                const v419 = stdlib.addressEq(v71, v416);
                stdlib.assert(v419, {
                  at: 'reach standard library:209:7:dot',
                  fs: ['at ./tut3.rsh:121:45:application call to "closeTo" (defined at: reach standard library:207:8:function exp)', 'at ./tut3.rsh:131:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                  msg: 'sender correct',
                  who: 'Alice'
                  });
                ;
                stdlib.protect(ctc4, await interact.informTimeout(), {
                  at: './tut3.rsh:66:33:application',
                  fs: ['at ./tut3.rsh:65:13:application call to [unknown function] (defined at: ./tut3.rsh:65:25:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./tut3.rsh:64:32:function exp)', 'at ./tut3.rsh:121:45:application call to "closeTo" (defined at: reach standard library:207:8:function exp)', 'at ./tut3.rsh:131:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                  msg: 'informTimeout',
                  who: 'Alice'
                  });
                return;
                }
              else {
                const [v404, v405] = txn6.data;
                const v409 = txn6.time;
                const v403 = txn6.from;
                stdlib.assert(true, {
                  at: './tut3.rsh:120:17:dot',
                  fs: ['at ./tut3.rsh:131:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                  msg: null,
                  who: 'Alice'
                  });
                const v407 = stdlib.add(v397, stdlib.checkedBigNumberify('./tut3.rsh:decimal', stdlib.UInt_max, 0));
                ;
                const v408 = stdlib.addressEq(v60, v403);
                stdlib.assert(v408, {
                  at: './tut3.rsh:120:17:dot',
                  fs: ['at ./tut3.rsh:131:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                  msg: 'sender correct',
                  who: 'Alice'
                  });
                const v411 = stdlib.digest(ctc2, [v404, v405]);
                const v412 = stdlib.digestEq(v385, v411);
                stdlib.assert(v412, {
                  at: 'reach standard library:65:17:application',
                  fs: ['at ./tut3.rsh:122:26:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)', 'at ./tut3.rsh:131:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                  msg: null,
                  who: 'Alice'
                  });
                const v490 = v405[stdlib.checkedBigNumberify('./tut3.rsh:39:18:array ref', stdlib.UInt_max, 0)];
                const v491 = v395[stdlib.checkedBigNumberify('./tut3.rsh:39:29:array ref', stdlib.UInt_max, 0)];
                const v493 = stdlib.sub(stdlib.checkedBigNumberify('./tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v491);
                const v494 = stdlib.add(v490, v493);
                const v495 = stdlib.mod(v494, stdlib.checkedBigNumberify('./tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v499 = v405[stdlib.checkedBigNumberify('./tut3.rsh:39:18:array ref', stdlib.UInt_max, 1)];
                const v500 = v395[stdlib.checkedBigNumberify('./tut3.rsh:39:29:array ref', stdlib.UInt_max, 1)];
                const v502 = stdlib.sub(stdlib.checkedBigNumberify('./tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v500);
                const v503 = stdlib.add(v499, v502);
                const v504 = stdlib.mod(v503, stdlib.checkedBigNumberify('./tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v508 = v405[stdlib.checkedBigNumberify('./tut3.rsh:39:18:array ref', stdlib.UInt_max, 2)];
                const v509 = v395[stdlib.checkedBigNumberify('./tut3.rsh:39:29:array ref', stdlib.UInt_max, 2)];
                const v511 = stdlib.sub(stdlib.checkedBigNumberify('./tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v509);
                const v512 = stdlib.add(v508, v511);
                const v513 = stdlib.mod(v512, stdlib.checkedBigNumberify('./tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v517 = v405[stdlib.checkedBigNumberify('./tut3.rsh:39:18:array ref', stdlib.UInt_max, 3)];
                const v518 = v395[stdlib.checkedBigNumberify('./tut3.rsh:39:29:array ref', stdlib.UInt_max, 3)];
                const v520 = stdlib.sub(stdlib.checkedBigNumberify('./tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v518);
                const v521 = stdlib.add(v517, v520);
                const v522 = stdlib.mod(v521, stdlib.checkedBigNumberify('./tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v526 = v405[stdlib.checkedBigNumberify('./tut3.rsh:39:18:array ref', stdlib.UInt_max, 4)];
                const v527 = v395[stdlib.checkedBigNumberify('./tut3.rsh:39:29:array ref', stdlib.UInt_max, 4)];
                const v529 = stdlib.sub(stdlib.checkedBigNumberify('./tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v527);
                const v530 = stdlib.add(v526, v529);
                const v531 = stdlib.mod(v530, stdlib.checkedBigNumberify('./tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v544 = stdlib.eq(v495, stdlib.checkedBigNumberify('./tut3.rsh:makeEnum', stdlib.UInt_max, 1));
                const v545 = v544 ? v504 : v495;
                const v548 = stdlib.eq(v545, stdlib.checkedBigNumberify('./tut3.rsh:makeEnum', stdlib.UInt_max, 1));
                const v549 = v548 ? v513 : v545;
                const v552 = stdlib.eq(v549, stdlib.checkedBigNumberify('./tut3.rsh:makeEnum', stdlib.UInt_max, 1));
                const v553 = v552 ? v522 : v549;
                const v556 = stdlib.eq(v553, stdlib.checkedBigNumberify('./tut3.rsh:makeEnum', stdlib.UInt_max, 1));
                const v557 = v556 ? v531 : v553;
                const v560 = stdlib.add(v176, stdlib.checkedBigNumberify('./tut3.rsh:132:67:decimal', stdlib.UInt_max, 1));
                const cv175 = v557;
                const cv176 = v560;
                const cv619 = v409;
                const cv621 = v407;
                
                v175 = cv175;
                v176 = cv176;
                v619 = cv619;
                v621 = cv621;
                
                continue;}
              }
            }
          }}
      const v561 = stdlib.eq(v175, stdlib.checkedBigNumberify('./tut3.rsh:makeEnum', stdlib.UInt_max, 2));
      const v564 = stdlib.mul(stdlib.checkedBigNumberify('./tut3.rsh:138:16:decimal', stdlib.UInt_max, 2), v61);
      const v566 = v561 ? v60 : v71;
      ;
      stdlib.protect(ctc4, await interact.seeOutcome(v175), {
        at: './tut3.rsh:142:28:application',
        fs: ['at ./tut3.rsh:141:11:application call to [unknown function] (defined at: ./tut3.rsh:141:23:function exp)'],
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
  const ctc5 = stdlib.T_Address;
  const ctc6 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc0, ctc5, ctc0, ctc0, ctc0]);
  const ctc7 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc0, ctc5, ctc0, ctc0]);
  const ctc8 = stdlib.T_Tuple([]);
  const ctc9 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc0, ctc5, ctc0, ctc1, ctc3, ctc0, ctc0]);
  const ctc10 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc0, ctc5, ctc0, ctc1, ctc3, ctc0]);
  const ctc11 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc0, ctc5, ctc0, ctc1, ctc0, ctc0]);
  const ctc12 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc0, ctc5, ctc0, ctc1, ctc0]);
  const ctc13 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc0, ctc1, ctc5, ctc3, ctc0, ctc0]);
  const ctc14 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc0, ctc1, ctc5, ctc3, ctc0]);
  const ctc15 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc0, ctc1, ctc0, ctc0]);
  const ctc16 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc0, ctc1, ctc0]);
  
  
  const v51 = await ctc.creationTime();
  const txn1 = await (ctc.recv(1, 3, [ctc0, ctc0, ctc1], false, false));
  const [v61, v62, v63] = txn1.data;
  const v66 = txn1.time;
  const v60 = txn1.from;
  stdlib.assert(true, {
    at: './tut3.rsh:76:9:dot',
    fs: [],
    msg: null,
    who: 'Bob'
    });
  const v65 = stdlib.add(stdlib.checkedBigNumberify('./tut3.rsh:compileDApp', stdlib.UInt_max, 0), v61);
  ;
  stdlib.assert(true, {
    at: './tut3.rsh:76:9:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Bob'
    });
  stdlib.protect(ctc2, await interact.acceptWager(v61, v62), {
    at: './tut3.rsh:82:29:application',
    fs: ['at ./tut3.rsh:81:13:application call to [unknown function] (defined at: ./tut3.rsh:81:17:function exp)'],
    msg: 'acceptWager',
    who: 'Bob'
    });
  const v70 = stdlib.protect(ctc3, await interact.getBatch(), {
    at: './tut3.rsh:83:57:application',
    fs: ['at ./tut3.rsh:81:13:application call to [unknown function] (defined at: ./tut3.rsh:81:17:function exp)'],
    msg: 'getBatch',
    who: 'Bob'
    });
  const txn2 = await (ctc.sendrecv(2, 1, stdlib.checkedBigNumberify('./tut3.rsh:85:9:dot', stdlib.UInt_max, 5), [ctc5, ctc0, ctc0, ctc1, ctc0, ctc0, ctc3], [v60, v61, v62, v63, v65, v66, v70], [v61, []], [ctc3], true, true, v62, (async (txn2) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.digest(ctc15, [stdlib.checkedBigNumberify('./tut3.rsh:85:9:dot', stdlib.UInt_max, 1), v60, v61, v62, v63, v65, v66]);
    sim_r.prevSt_noPrevTime = stdlib.digest(ctc16, [stdlib.checkedBigNumberify('./tut3.rsh:85:9:dot', stdlib.UInt_max, 1), v60, v61, v62, v63, v65]);
    const [v72] = txn2.data;
    const v75 = txn2.time;
    const v71 = txn2.from;
    
    stdlib.assert(true, {
      at: './tut3.rsh:85:9:dot',
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
      at: './tut3.rsh:85:9:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Bob'
      });
    sim_r.nextSt = stdlib.digest(ctc13, [stdlib.checkedBigNumberify('./tut3.rsh:88:15:after expr stmt semicolon', stdlib.UInt_max, 2), v60, v61, v62, v63, v71, v72, v74, v75]);
    sim_r.nextSt_noTime = stdlib.digest(ctc14, [stdlib.checkedBigNumberify('./tut3.rsh:88:15:after expr stmt semicolon', stdlib.UInt_max, 2), v60, v61, v62, v63, v71, v72, v74]);
    sim_r.isHalt = false;
    
    return sim_r;
    })));
  if (txn2.didTimeout) {
    const txn3 = await (ctc.recv(3, 0, [], false, false));
    const [] = txn3.data;
    const v604 = txn3.time;
    const v600 = txn3.from;
    stdlib.assert(true, {
      at: 'reach standard library:209:7:dot',
      fs: ['at ./tut3.rsh:87:41:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
      msg: null,
      who: 'Bob'
      });
    const v602 = stdlib.add(v65, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
    ;
    const v603 = stdlib.addressEq(v60, v600);
    stdlib.assert(v603, {
      at: 'reach standard library:209:7:dot',
      fs: ['at ./tut3.rsh:87:41:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
      msg: 'sender correct',
      who: 'Bob'
      });
    ;
    stdlib.protect(ctc2, await interact.informTimeout(), {
      at: './tut3.rsh:66:33:application',
      fs: ['at ./tut3.rsh:65:13:application call to [unknown function] (defined at: ./tut3.rsh:65:25:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./tut3.rsh:64:32:function exp)', 'at ./tut3.rsh:87:41:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
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
      at: './tut3.rsh:85:9:dot',
      fs: [],
      msg: null,
      who: 'Bob'
      });
    const v74 = stdlib.add(v65, v61);
    ;
    stdlib.assert(true, {
      at: './tut3.rsh:85:9:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Bob'
      });
    const txn3 = await (ctc.recv(4, 2, [ctc0, ctc3], false, v62));
    if (txn3.didTimeout) {
      const txn4 = await (ctc.sendrecv(5, 0, stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 7), [ctc5, ctc0, ctc0, ctc1, ctc5, ctc3, ctc0, ctc0], [v60, v61, v62, v63, v71, v72, v74, v75], [stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), []], [], true, true, false, (async (txn4) => {
        const sim_r = { txns: [] };
        sim_r.prevSt = stdlib.digest(ctc13, [stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 2), v60, v61, v62, v63, v71, v72, v74, v75]);
        sim_r.prevSt_noPrevTime = stdlib.digest(ctc14, [stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 2), v60, v61, v62, v63, v71, v72, v74]);
        const [] = txn4.data;
        const v584 = txn4.time;
        const v580 = txn4.from;
        
        stdlib.assert(true, {
          at: 'reach standard library:209:7:dot',
          fs: ['at ./tut3.rsh:94:40:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
          msg: null,
          who: 'Bob'
          });
        const v582 = stdlib.add(v74, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
        sim_r.txns.push({
          amt: stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0),
          kind: 'to',
          tok: undefined
          });
        const v583 = stdlib.addressEq(v71, v580);
        stdlib.assert(v583, {
          at: 'reach standard library:209:7:dot',
          fs: ['at ./tut3.rsh:94:40:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
          msg: 'sender correct',
          who: 'Bob'
          });
        sim_r.txns.push({
          amt: v582,
          kind: 'from',
          to: v71,
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
      const [] = txn4.data;
      const v584 = txn4.time;
      const v580 = txn4.from;
      stdlib.assert(true, {
        at: 'reach standard library:209:7:dot',
        fs: ['at ./tut3.rsh:94:40:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
        msg: null,
        who: 'Bob'
        });
      const v582 = stdlib.add(v74, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
      ;
      const v583 = stdlib.addressEq(v71, v580);
      stdlib.assert(v583, {
        at: 'reach standard library:209:7:dot',
        fs: ['at ./tut3.rsh:94:40:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
        msg: 'sender correct',
        who: 'Bob'
        });
      ;
      stdlib.protect(ctc2, await interact.informTimeout(), {
        at: './tut3.rsh:66:33:application',
        fs: ['at ./tut3.rsh:65:13:application call to [unknown function] (defined at: ./tut3.rsh:65:25:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./tut3.rsh:64:32:function exp)', 'at ./tut3.rsh:94:40:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
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
        at: './tut3.rsh:93:9:dot',
        fs: [],
        msg: null,
        who: 'Bob'
        });
      const v83 = stdlib.add(v74, stdlib.checkedBigNumberify('./tut3.rsh:decimal', stdlib.UInt_max, 0));
      ;
      const v84 = stdlib.addressEq(v60, v79);
      stdlib.assert(v84, {
        at: './tut3.rsh:93:9:dot',
        fs: [],
        msg: 'sender correct',
        who: 'Bob'
        });
      const v87 = stdlib.digest(ctc4, [v80, v81]);
      const v88 = stdlib.digestEq(v63, v87);
      stdlib.assert(v88, {
        at: 'reach standard library:65:17:application',
        fs: ['at ./tut3.rsh:95:22:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)'],
        msg: null,
        who: 'Bob'
        });
      const v105 = v81[stdlib.checkedBigNumberify('./tut3.rsh:39:18:array ref', stdlib.UInt_max, 0)];
      const v106 = v72[stdlib.checkedBigNumberify('./tut3.rsh:39:29:array ref', stdlib.UInt_max, 0)];
      const v108 = stdlib.sub(stdlib.checkedBigNumberify('./tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v106);
      const v109 = stdlib.add(v105, v108);
      const v110 = stdlib.mod(v109, stdlib.checkedBigNumberify('./tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v114 = v81[stdlib.checkedBigNumberify('./tut3.rsh:39:18:array ref', stdlib.UInt_max, 1)];
      const v115 = v72[stdlib.checkedBigNumberify('./tut3.rsh:39:29:array ref', stdlib.UInt_max, 1)];
      const v117 = stdlib.sub(stdlib.checkedBigNumberify('./tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v115);
      const v118 = stdlib.add(v114, v117);
      const v119 = stdlib.mod(v118, stdlib.checkedBigNumberify('./tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v123 = v81[stdlib.checkedBigNumberify('./tut3.rsh:39:18:array ref', stdlib.UInt_max, 2)];
      const v124 = v72[stdlib.checkedBigNumberify('./tut3.rsh:39:29:array ref', stdlib.UInt_max, 2)];
      const v126 = stdlib.sub(stdlib.checkedBigNumberify('./tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v124);
      const v127 = stdlib.add(v123, v126);
      const v128 = stdlib.mod(v127, stdlib.checkedBigNumberify('./tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v132 = v81[stdlib.checkedBigNumberify('./tut3.rsh:39:18:array ref', stdlib.UInt_max, 3)];
      const v133 = v72[stdlib.checkedBigNumberify('./tut3.rsh:39:29:array ref', stdlib.UInt_max, 3)];
      const v135 = stdlib.sub(stdlib.checkedBigNumberify('./tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v133);
      const v136 = stdlib.add(v132, v135);
      const v137 = stdlib.mod(v136, stdlib.checkedBigNumberify('./tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v141 = v81[stdlib.checkedBigNumberify('./tut3.rsh:39:18:array ref', stdlib.UInt_max, 4)];
      const v142 = v72[stdlib.checkedBigNumberify('./tut3.rsh:39:29:array ref', stdlib.UInt_max, 4)];
      const v144 = stdlib.sub(stdlib.checkedBigNumberify('./tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v142);
      const v145 = stdlib.add(v141, v144);
      const v146 = stdlib.mod(v145, stdlib.checkedBigNumberify('./tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v159 = stdlib.eq(v110, stdlib.checkedBigNumberify('./tut3.rsh:makeEnum', stdlib.UInt_max, 1));
      const v160 = v159 ? v119 : v110;
      const v163 = stdlib.eq(v160, stdlib.checkedBigNumberify('./tut3.rsh:makeEnum', stdlib.UInt_max, 1));
      const v164 = v163 ? v128 : v160;
      const v167 = stdlib.eq(v164, stdlib.checkedBigNumberify('./tut3.rsh:makeEnum', stdlib.UInt_max, 1));
      const v168 = v167 ? v137 : v164;
      const v171 = stdlib.eq(v168, stdlib.checkedBigNumberify('./tut3.rsh:makeEnum', stdlib.UInt_max, 1));
      const v172 = v171 ? v146 : v168;
      let v175 = v172;
      let v176 = stdlib.checkedBigNumberify('./tut3.rsh:97:70:decimal', stdlib.UInt_max, 0);
      let v619 = v85;
      let v621 = v83;
      
      while ((() => {
        const v186 = stdlib.eq(v175, stdlib.checkedBigNumberify('./tut3.rsh:makeEnum', stdlib.UInt_max, 1));
        
        return v186;})()) {
        const v187 = stdlib.mod(v176, stdlib.checkedBigNumberify('./tut3.rsh:126:21:decimal', stdlib.UInt_max, 2));
        const v188 = stdlib.eq(v187, stdlib.checkedBigNumberify('./tut3.rsh:126:26:decimal', stdlib.UInt_max, 0));
        if (v188) {
          const v193 = stdlib.protect(ctc3, await interact.getBatch(), {
            at: './tut3.rsh:104:50:application',
            fs: ['at ./tut3.rsh:103:21:application call to [unknown function] (defined at: ./tut3.rsh:103:25:function exp)', 'at ./tut3.rsh:127:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
            msg: 'getBatch',
            who: 'Bob'
            });
          const v195 = stdlib.protect(ctc0, await interact.random(), {
            at: 'reach standard library:60:31:application',
            fs: ['at ./tut3.rsh:105:62:application call to "makeCommitment" (defined at: reach standard library:59:8:function exp)', 'at ./tut3.rsh:103:21:application call to [unknown function] (defined at: ./tut3.rsh:103:25:function exp)', 'at ./tut3.rsh:127:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
            msg: 'random',
            who: 'Bob'
            });
          const v196 = stdlib.digest(ctc4, [v195, v193]);
          const txn4 = await (ctc.sendrecv(8, 1, stdlib.checkedBigNumberify('./tut3.rsh:107:17:dot', stdlib.UInt_max, 5), [ctc5, ctc0, ctc0, ctc5, ctc0, ctc0, ctc0, ctc1], [v60, v61, v62, v71, v176, v619, v621, v196], [stdlib.checkedBigNumberify('./tut3.rsh:decimal', stdlib.UInt_max, 0), []], [ctc1], true, true, v62, (async (txn4) => {
            const sim_r = { txns: [] };
            sim_r.prevSt = stdlib.digest(ctc6, [stdlib.checkedBigNumberify('./tut3.rsh:107:17:dot', stdlib.UInt_max, 6), v60, v61, v62, v71, v176, v619, v621]);
            sim_r.prevSt_noPrevTime = stdlib.digest(ctc7, [stdlib.checkedBigNumberify('./tut3.rsh:107:17:dot', stdlib.UInt_max, 6), v60, v61, v62, v71, v176, v621]);
            const [v199] = txn4.data;
            const v203 = txn4.time;
            const v198 = txn4.from;
            
            stdlib.assert(true, {
              at: './tut3.rsh:107:17:dot',
              fs: ['at ./tut3.rsh:127:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
              msg: null,
              who: 'Bob'
              });
            const v201 = stdlib.add(v621, stdlib.checkedBigNumberify('./tut3.rsh:decimal', stdlib.UInt_max, 0));
            sim_r.txns.push({
              amt: stdlib.checkedBigNumberify('./tut3.rsh:decimal', stdlib.UInt_max, 0),
              kind: 'to',
              tok: undefined
              });
            const v202 = stdlib.addressEq(v71, v198);
            stdlib.assert(v202, {
              at: './tut3.rsh:107:17:dot',
              fs: ['at ./tut3.rsh:127:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
              msg: 'sender correct',
              who: 'Bob'
              });
            sim_r.nextSt = stdlib.digest(ctc11, [stdlib.checkedBigNumberify('./tut3.rsh:109:19:after expr stmt semicolon', stdlib.UInt_max, 8), v60, v61, v62, v71, v176, v199, v201, v203]);
            sim_r.nextSt_noTime = stdlib.digest(ctc12, [stdlib.checkedBigNumberify('./tut3.rsh:109:19:after expr stmt semicolon', stdlib.UInt_max, 8), v60, v61, v62, v71, v176, v199, v201]);
            sim_r.isHalt = false;
            
            return sim_r;
            })));
          if (txn4.didTimeout) {
            const txn5 = await (ctc.recv(9, 0, [], false, false));
            const [] = txn5.data;
            const v274 = txn5.time;
            const v270 = txn5.from;
            stdlib.assert(true, {
              at: 'reach standard library:209:7:dot',
              fs: ['at ./tut3.rsh:108:45:application call to "closeTo" (defined at: reach standard library:207:8:function exp)', 'at ./tut3.rsh:127:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
              msg: null,
              who: 'Bob'
              });
            const v272 = stdlib.add(v621, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
            ;
            const v273 = stdlib.addressEq(v60, v270);
            stdlib.assert(v273, {
              at: 'reach standard library:209:7:dot',
              fs: ['at ./tut3.rsh:108:45:application call to "closeTo" (defined at: reach standard library:207:8:function exp)', 'at ./tut3.rsh:127:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
              msg: 'sender correct',
              who: 'Bob'
              });
            ;
            stdlib.protect(ctc2, await interact.informTimeout(), {
              at: './tut3.rsh:66:33:application',
              fs: ['at ./tut3.rsh:65:13:application call to [unknown function] (defined at: ./tut3.rsh:65:25:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./tut3.rsh:64:32:function exp)', 'at ./tut3.rsh:108:45:application call to "closeTo" (defined at: reach standard library:207:8:function exp)', 'at ./tut3.rsh:127:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
              msg: 'informTimeout',
              who: 'Bob'
              });
            return;
            }
          else {
            const [v199] = txn4.data;
            const v203 = txn4.time;
            const v198 = txn4.from;
            stdlib.assert(true, {
              at: './tut3.rsh:107:17:dot',
              fs: ['at ./tut3.rsh:127:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
              msg: null,
              who: 'Bob'
              });
            const v201 = stdlib.add(v621, stdlib.checkedBigNumberify('./tut3.rsh:decimal', stdlib.UInt_max, 0));
            ;
            const v202 = stdlib.addressEq(v71, v198);
            stdlib.assert(v202, {
              at: './tut3.rsh:107:17:dot',
              fs: ['at ./tut3.rsh:127:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
              msg: 'sender correct',
              who: 'Bob'
              });
            const txn5 = await (ctc.recv(10, 1, [ctc3], false, v62));
            if (txn5.didTimeout) {
              const txn6 = await (ctc.sendrecv(11, 0, stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 7), [ctc5, ctc0, ctc0, ctc5, ctc0, ctc1, ctc0, ctc0], [v60, v61, v62, v71, v176, v199, v201, v203], [stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), []], [], true, true, false, (async (txn6) => {
                const sim_r = { txns: [] };
                sim_r.prevSt = stdlib.digest(ctc11, [stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 8), v60, v61, v62, v71, v176, v199, v201, v203]);
                sim_r.prevSt_noPrevTime = stdlib.digest(ctc12, [stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 8), v60, v61, v62, v71, v176, v199, v201]);
                const [] = txn6.data;
                const v254 = txn6.time;
                const v250 = txn6.from;
                
                stdlib.assert(true, {
                  at: 'reach standard library:209:7:dot',
                  fs: ['at ./tut3.rsh:115:45:application call to "closeTo" (defined at: reach standard library:207:8:function exp)', 'at ./tut3.rsh:127:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                  msg: null,
                  who: 'Bob'
                  });
                const v252 = stdlib.add(v201, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
                sim_r.txns.push({
                  amt: stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0),
                  kind: 'to',
                  tok: undefined
                  });
                const v253 = stdlib.addressEq(v71, v250);
                stdlib.assert(v253, {
                  at: 'reach standard library:209:7:dot',
                  fs: ['at ./tut3.rsh:115:45:application call to "closeTo" (defined at: reach standard library:207:8:function exp)', 'at ./tut3.rsh:127:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                  msg: 'sender correct',
                  who: 'Bob'
                  });
                sim_r.txns.push({
                  amt: v252,
                  kind: 'from',
                  to: v71,
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
              const v254 = txn6.time;
              const v250 = txn6.from;
              stdlib.assert(true, {
                at: 'reach standard library:209:7:dot',
                fs: ['at ./tut3.rsh:115:45:application call to "closeTo" (defined at: reach standard library:207:8:function exp)', 'at ./tut3.rsh:127:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                msg: null,
                who: 'Bob'
                });
              const v252 = stdlib.add(v201, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
              ;
              const v253 = stdlib.addressEq(v71, v250);
              stdlib.assert(v253, {
                at: 'reach standard library:209:7:dot',
                fs: ['at ./tut3.rsh:115:45:application call to "closeTo" (defined at: reach standard library:207:8:function exp)', 'at ./tut3.rsh:127:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                msg: 'sender correct',
                who: 'Bob'
                });
              ;
              stdlib.protect(ctc2, await interact.informTimeout(), {
                at: './tut3.rsh:66:33:application',
                fs: ['at ./tut3.rsh:65:13:application call to [unknown function] (defined at: ./tut3.rsh:65:25:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./tut3.rsh:64:32:function exp)', 'at ./tut3.rsh:115:45:application call to "closeTo" (defined at: reach standard library:207:8:function exp)', 'at ./tut3.rsh:127:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                msg: 'informTimeout',
                who: 'Bob'
                });
              return;
              }
            else {
              const [v209] = txn5.data;
              const v213 = txn5.time;
              const v208 = txn5.from;
              stdlib.assert(true, {
                at: './tut3.rsh:114:18:dot',
                fs: ['at ./tut3.rsh:127:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                msg: null,
                who: 'Bob'
                });
              const v211 = stdlib.add(v201, stdlib.checkedBigNumberify('./tut3.rsh:decimal', stdlib.UInt_max, 0));
              ;
              const v212 = stdlib.addressEq(v60, v208);
              stdlib.assert(v212, {
                at: './tut3.rsh:114:18:dot',
                fs: ['at ./tut3.rsh:127:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                msg: 'sender correct',
                who: 'Bob'
                });
              const txn6 = await (ctc.sendrecv(12, 2, stdlib.checkedBigNumberify('./tut3.rsh:120:17:dot', stdlib.UInt_max, 8), [ctc5, ctc0, ctc0, ctc5, ctc0, ctc1, ctc3, ctc0, ctc0, ctc0, ctc3], [v60, v61, v62, v71, v176, v199, v209, v211, v213, v195, v193], [stdlib.checkedBigNumberify('./tut3.rsh:decimal', stdlib.UInt_max, 0), []], [ctc0, ctc3], true, true, v62, (async (txn6) => {
                const sim_r = { txns: [] };
                sim_r.prevSt = stdlib.digest(ctc9, [stdlib.checkedBigNumberify('./tut3.rsh:120:17:dot', stdlib.UInt_max, 10), v60, v61, v62, v71, v176, v199, v209, v211, v213]);
                sim_r.prevSt_noPrevTime = stdlib.digest(ctc10, [stdlib.checkedBigNumberify('./tut3.rsh:120:17:dot', stdlib.UInt_max, 10), v60, v61, v62, v71, v176, v199, v209, v211]);
                const [v218, v219] = txn6.data;
                const v223 = txn6.time;
                const v217 = txn6.from;
                
                stdlib.assert(true, {
                  at: './tut3.rsh:120:17:dot',
                  fs: ['at ./tut3.rsh:127:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                  msg: null,
                  who: 'Bob'
                  });
                const v221 = stdlib.add(v211, stdlib.checkedBigNumberify('./tut3.rsh:decimal', stdlib.UInt_max, 0));
                sim_r.txns.push({
                  amt: stdlib.checkedBigNumberify('./tut3.rsh:decimal', stdlib.UInt_max, 0),
                  kind: 'to',
                  tok: undefined
                  });
                const v222 = stdlib.addressEq(v71, v217);
                stdlib.assert(v222, {
                  at: './tut3.rsh:120:17:dot',
                  fs: ['at ./tut3.rsh:127:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                  msg: 'sender correct',
                  who: 'Bob'
                  });
                const v225 = stdlib.digest(ctc4, [v218, v219]);
                const v226 = stdlib.digestEq(v199, v225);
                stdlib.assert(v226, {
                  at: 'reach standard library:65:17:application',
                  fs: ['at ./tut3.rsh:122:26:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)', 'at ./tut3.rsh:127:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                  msg: null,
                  who: 'Bob'
                  });
                const v304 = v209[stdlib.checkedBigNumberify('./tut3.rsh:39:18:array ref', stdlib.UInt_max, 0)];
                const v305 = v219[stdlib.checkedBigNumberify('./tut3.rsh:39:29:array ref', stdlib.UInt_max, 0)];
                const v307 = stdlib.sub(stdlib.checkedBigNumberify('./tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v305);
                const v308 = stdlib.add(v304, v307);
                const v309 = stdlib.mod(v308, stdlib.checkedBigNumberify('./tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v313 = v209[stdlib.checkedBigNumberify('./tut3.rsh:39:18:array ref', stdlib.UInt_max, 1)];
                const v314 = v219[stdlib.checkedBigNumberify('./tut3.rsh:39:29:array ref', stdlib.UInt_max, 1)];
                const v316 = stdlib.sub(stdlib.checkedBigNumberify('./tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v314);
                const v317 = stdlib.add(v313, v316);
                const v318 = stdlib.mod(v317, stdlib.checkedBigNumberify('./tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v322 = v209[stdlib.checkedBigNumberify('./tut3.rsh:39:18:array ref', stdlib.UInt_max, 2)];
                const v323 = v219[stdlib.checkedBigNumberify('./tut3.rsh:39:29:array ref', stdlib.UInt_max, 2)];
                const v325 = stdlib.sub(stdlib.checkedBigNumberify('./tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v323);
                const v326 = stdlib.add(v322, v325);
                const v327 = stdlib.mod(v326, stdlib.checkedBigNumberify('./tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v331 = v209[stdlib.checkedBigNumberify('./tut3.rsh:39:18:array ref', stdlib.UInt_max, 3)];
                const v332 = v219[stdlib.checkedBigNumberify('./tut3.rsh:39:29:array ref', stdlib.UInt_max, 3)];
                const v334 = stdlib.sub(stdlib.checkedBigNumberify('./tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v332);
                const v335 = stdlib.add(v331, v334);
                const v336 = stdlib.mod(v335, stdlib.checkedBigNumberify('./tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v340 = v209[stdlib.checkedBigNumberify('./tut3.rsh:39:18:array ref', stdlib.UInt_max, 4)];
                const v341 = v219[stdlib.checkedBigNumberify('./tut3.rsh:39:29:array ref', stdlib.UInt_max, 4)];
                const v343 = stdlib.sub(stdlib.checkedBigNumberify('./tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v341);
                const v344 = stdlib.add(v340, v343);
                const v345 = stdlib.mod(v344, stdlib.checkedBigNumberify('./tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v358 = stdlib.eq(v309, stdlib.checkedBigNumberify('./tut3.rsh:makeEnum', stdlib.UInt_max, 1));
                const v359 = v358 ? v318 : v309;
                const v362 = stdlib.eq(v359, stdlib.checkedBigNumberify('./tut3.rsh:makeEnum', stdlib.UInt_max, 1));
                const v363 = v362 ? v327 : v359;
                const v366 = stdlib.eq(v363, stdlib.checkedBigNumberify('./tut3.rsh:makeEnum', stdlib.UInt_max, 1));
                const v367 = v366 ? v336 : v363;
                const v370 = stdlib.eq(v367, stdlib.checkedBigNumberify('./tut3.rsh:makeEnum', stdlib.UInt_max, 1));
                const v371 = v370 ? v345 : v367;
                const v374 = stdlib.add(v176, stdlib.checkedBigNumberify('./tut3.rsh:128:67:decimal', stdlib.UInt_max, 1));
                const cv175 = v371;
                const cv176 = v374;
                const cv619 = v223;
                const cv621 = v221;
                
                (() => {
                  const v175 = cv175;
                  const v176 = cv176;
                  const v619 = cv619;
                  const v621 = cv621;
                  
                  if ((() => {
                    const v186 = stdlib.eq(v175, stdlib.checkedBigNumberify('./tut3.rsh:makeEnum', stdlib.UInt_max, 1));
                    
                    return v186;})()) {
                    const v187 = stdlib.mod(v176, stdlib.checkedBigNumberify('./tut3.rsh:126:21:decimal', stdlib.UInt_max, 2));
                    const v188 = stdlib.eq(v187, stdlib.checkedBigNumberify('./tut3.rsh:126:26:decimal', stdlib.UInt_max, 0));
                    if (v188) {
                      sim_r.nextSt = stdlib.digest(ctc6, [stdlib.checkedBigNumberify('./tut3.rsh:101:19:after expr stmt semicolon', stdlib.UInt_max, 6), v60, v61, v62, v71, v176, v619, v621]);
                      sim_r.nextSt_noTime = stdlib.digest(ctc7, [stdlib.checkedBigNumberify('./tut3.rsh:101:19:after expr stmt semicolon', stdlib.UInt_max, 6), v60, v61, v62, v71, v176, v621]);
                      sim_r.isHalt = false;
                      }
                    else {
                      sim_r.nextSt = stdlib.digest(ctc6, [stdlib.checkedBigNumberify('./tut3.rsh:101:19:after expr stmt semicolon', stdlib.UInt_max, 6), v60, v61, v62, v71, v176, v619, v621]);
                      sim_r.nextSt_noTime = stdlib.digest(ctc7, [stdlib.checkedBigNumberify('./tut3.rsh:101:19:after expr stmt semicolon', stdlib.UInt_max, 6), v60, v61, v62, v71, v176, v621]);
                      sim_r.isHalt = false;
                      }}
                  else {
                    const v561 = stdlib.eq(v175, stdlib.checkedBigNumberify('./tut3.rsh:makeEnum', stdlib.UInt_max, 2));
                    const v564 = stdlib.mul(stdlib.checkedBigNumberify('./tut3.rsh:138:16:decimal', stdlib.UInt_max, 2), v61);
                    const v566 = v561 ? v60 : v71;
                    sim_r.txns.push({
                      amt: v564,
                      kind: 'from',
                      to: v566,
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
                const v234 = txn7.time;
                const v230 = txn7.from;
                stdlib.assert(true, {
                  at: 'reach standard library:209:7:dot',
                  fs: ['at ./tut3.rsh:121:45:application call to "closeTo" (defined at: reach standard library:207:8:function exp)', 'at ./tut3.rsh:127:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                  msg: null,
                  who: 'Bob'
                  });
                const v232 = stdlib.add(v211, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
                ;
                const v233 = stdlib.addressEq(v60, v230);
                stdlib.assert(v233, {
                  at: 'reach standard library:209:7:dot',
                  fs: ['at ./tut3.rsh:121:45:application call to "closeTo" (defined at: reach standard library:207:8:function exp)', 'at ./tut3.rsh:127:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                  msg: 'sender correct',
                  who: 'Bob'
                  });
                ;
                stdlib.protect(ctc2, await interact.informTimeout(), {
                  at: './tut3.rsh:66:33:application',
                  fs: ['at ./tut3.rsh:65:13:application call to [unknown function] (defined at: ./tut3.rsh:65:25:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./tut3.rsh:64:32:function exp)', 'at ./tut3.rsh:121:45:application call to "closeTo" (defined at: reach standard library:207:8:function exp)', 'at ./tut3.rsh:127:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                  msg: 'informTimeout',
                  who: 'Bob'
                  });
                return;
                }
              else {
                const [v218, v219] = txn6.data;
                const v223 = txn6.time;
                const v217 = txn6.from;
                stdlib.assert(true, {
                  at: './tut3.rsh:120:17:dot',
                  fs: ['at ./tut3.rsh:127:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                  msg: null,
                  who: 'Bob'
                  });
                const v221 = stdlib.add(v211, stdlib.checkedBigNumberify('./tut3.rsh:decimal', stdlib.UInt_max, 0));
                ;
                const v222 = stdlib.addressEq(v71, v217);
                stdlib.assert(v222, {
                  at: './tut3.rsh:120:17:dot',
                  fs: ['at ./tut3.rsh:127:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                  msg: 'sender correct',
                  who: 'Bob'
                  });
                const v225 = stdlib.digest(ctc4, [v218, v219]);
                const v226 = stdlib.digestEq(v199, v225);
                stdlib.assert(v226, {
                  at: 'reach standard library:65:17:application',
                  fs: ['at ./tut3.rsh:122:26:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)', 'at ./tut3.rsh:127:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                  msg: null,
                  who: 'Bob'
                  });
                const v304 = v209[stdlib.checkedBigNumberify('./tut3.rsh:39:18:array ref', stdlib.UInt_max, 0)];
                const v305 = v219[stdlib.checkedBigNumberify('./tut3.rsh:39:29:array ref', stdlib.UInt_max, 0)];
                const v307 = stdlib.sub(stdlib.checkedBigNumberify('./tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v305);
                const v308 = stdlib.add(v304, v307);
                const v309 = stdlib.mod(v308, stdlib.checkedBigNumberify('./tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v313 = v209[stdlib.checkedBigNumberify('./tut3.rsh:39:18:array ref', stdlib.UInt_max, 1)];
                const v314 = v219[stdlib.checkedBigNumberify('./tut3.rsh:39:29:array ref', stdlib.UInt_max, 1)];
                const v316 = stdlib.sub(stdlib.checkedBigNumberify('./tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v314);
                const v317 = stdlib.add(v313, v316);
                const v318 = stdlib.mod(v317, stdlib.checkedBigNumberify('./tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v322 = v209[stdlib.checkedBigNumberify('./tut3.rsh:39:18:array ref', stdlib.UInt_max, 2)];
                const v323 = v219[stdlib.checkedBigNumberify('./tut3.rsh:39:29:array ref', stdlib.UInt_max, 2)];
                const v325 = stdlib.sub(stdlib.checkedBigNumberify('./tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v323);
                const v326 = stdlib.add(v322, v325);
                const v327 = stdlib.mod(v326, stdlib.checkedBigNumberify('./tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v331 = v209[stdlib.checkedBigNumberify('./tut3.rsh:39:18:array ref', stdlib.UInt_max, 3)];
                const v332 = v219[stdlib.checkedBigNumberify('./tut3.rsh:39:29:array ref', stdlib.UInt_max, 3)];
                const v334 = stdlib.sub(stdlib.checkedBigNumberify('./tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v332);
                const v335 = stdlib.add(v331, v334);
                const v336 = stdlib.mod(v335, stdlib.checkedBigNumberify('./tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v340 = v209[stdlib.checkedBigNumberify('./tut3.rsh:39:18:array ref', stdlib.UInt_max, 4)];
                const v341 = v219[stdlib.checkedBigNumberify('./tut3.rsh:39:29:array ref', stdlib.UInt_max, 4)];
                const v343 = stdlib.sub(stdlib.checkedBigNumberify('./tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v341);
                const v344 = stdlib.add(v340, v343);
                const v345 = stdlib.mod(v344, stdlib.checkedBigNumberify('./tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v358 = stdlib.eq(v309, stdlib.checkedBigNumberify('./tut3.rsh:makeEnum', stdlib.UInt_max, 1));
                const v359 = v358 ? v318 : v309;
                const v362 = stdlib.eq(v359, stdlib.checkedBigNumberify('./tut3.rsh:makeEnum', stdlib.UInt_max, 1));
                const v363 = v362 ? v327 : v359;
                const v366 = stdlib.eq(v363, stdlib.checkedBigNumberify('./tut3.rsh:makeEnum', stdlib.UInt_max, 1));
                const v367 = v366 ? v336 : v363;
                const v370 = stdlib.eq(v367, stdlib.checkedBigNumberify('./tut3.rsh:makeEnum', stdlib.UInt_max, 1));
                const v371 = v370 ? v345 : v367;
                const v374 = stdlib.add(v176, stdlib.checkedBigNumberify('./tut3.rsh:128:67:decimal', stdlib.UInt_max, 1));
                const cv175 = v371;
                const cv176 = v374;
                const cv619 = v223;
                const cv621 = v221;
                
                v175 = cv175;
                v176 = cv176;
                v619 = cv619;
                v621 = cv621;
                
                continue;}
              }
            }
          }
        else {
          const txn4 = await (ctc.recv(14, 1, [ctc1], false, v62));
          if (txn4.didTimeout) {
            const txn5 = await (ctc.sendrecv(15, 0, stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 5), [ctc5, ctc0, ctc0, ctc5, ctc0, ctc0, ctc0], [v60, v61, v62, v71, v176, v619, v621], [stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), []], [], true, true, false, (async (txn5) => {
              const sim_r = { txns: [] };
              sim_r.prevSt = stdlib.digest(ctc6, [stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 6), v60, v61, v62, v71, v176, v619, v621]);
              sim_r.prevSt_noPrevTime = stdlib.digest(ctc7, [stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 6), v60, v61, v62, v71, v176, v621]);
              const [] = txn5.data;
              const v460 = txn5.time;
              const v456 = txn5.from;
              
              stdlib.assert(true, {
                at: 'reach standard library:209:7:dot',
                fs: ['at ./tut3.rsh:108:45:application call to "closeTo" (defined at: reach standard library:207:8:function exp)', 'at ./tut3.rsh:131:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                msg: null,
                who: 'Bob'
                });
              const v458 = stdlib.add(v621, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
              sim_r.txns.push({
                amt: stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0),
                kind: 'to',
                tok: undefined
                });
              const v459 = stdlib.addressEq(v71, v456);
              stdlib.assert(v459, {
                at: 'reach standard library:209:7:dot',
                fs: ['at ./tut3.rsh:108:45:application call to "closeTo" (defined at: reach standard library:207:8:function exp)', 'at ./tut3.rsh:131:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                msg: 'sender correct',
                who: 'Bob'
                });
              sim_r.txns.push({
                amt: v458,
                kind: 'from',
                to: v71,
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
            const [] = txn5.data;
            const v460 = txn5.time;
            const v456 = txn5.from;
            stdlib.assert(true, {
              at: 'reach standard library:209:7:dot',
              fs: ['at ./tut3.rsh:108:45:application call to "closeTo" (defined at: reach standard library:207:8:function exp)', 'at ./tut3.rsh:131:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
              msg: null,
              who: 'Bob'
              });
            const v458 = stdlib.add(v621, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
            ;
            const v459 = stdlib.addressEq(v71, v456);
            stdlib.assert(v459, {
              at: 'reach standard library:209:7:dot',
              fs: ['at ./tut3.rsh:108:45:application call to "closeTo" (defined at: reach standard library:207:8:function exp)', 'at ./tut3.rsh:131:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
              msg: 'sender correct',
              who: 'Bob'
              });
            ;
            stdlib.protect(ctc2, await interact.informTimeout(), {
              at: './tut3.rsh:66:33:application',
              fs: ['at ./tut3.rsh:65:13:application call to [unknown function] (defined at: ./tut3.rsh:65:25:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./tut3.rsh:64:32:function exp)', 'at ./tut3.rsh:108:45:application call to "closeTo" (defined at: reach standard library:207:8:function exp)', 'at ./tut3.rsh:131:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
              msg: 'informTimeout',
              who: 'Bob'
              });
            return;
            }
          else {
            const [v385] = txn4.data;
            const v389 = txn4.time;
            const v384 = txn4.from;
            stdlib.assert(true, {
              at: './tut3.rsh:107:17:dot',
              fs: ['at ./tut3.rsh:131:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
              msg: null,
              who: 'Bob'
              });
            const v387 = stdlib.add(v621, stdlib.checkedBigNumberify('./tut3.rsh:decimal', stdlib.UInt_max, 0));
            ;
            const v388 = stdlib.addressEq(v60, v384);
            stdlib.assert(v388, {
              at: './tut3.rsh:107:17:dot',
              fs: ['at ./tut3.rsh:131:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
              msg: 'sender correct',
              who: 'Bob'
              });
            const v393 = stdlib.protect(ctc3, await interact.getBatch(), {
              at: './tut3.rsh:113:61:application',
              fs: ['at ./tut3.rsh:112:22:application call to [unknown function] (defined at: ./tut3.rsh:112:26:function exp)', 'at ./tut3.rsh:131:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
              msg: 'getBatch',
              who: 'Bob'
              });
            const txn5 = await (ctc.sendrecv(16, 1, stdlib.checkedBigNumberify('./tut3.rsh:114:18:dot', stdlib.UInt_max, 7), [ctc5, ctc0, ctc0, ctc5, ctc0, ctc1, ctc0, ctc0, ctc3], [v60, v61, v62, v71, v176, v385, v387, v389, v393], [stdlib.checkedBigNumberify('./tut3.rsh:decimal', stdlib.UInt_max, 0), []], [ctc3], true, true, v62, (async (txn5) => {
              const sim_r = { txns: [] };
              sim_r.prevSt = stdlib.digest(ctc11, [stdlib.checkedBigNumberify('./tut3.rsh:114:18:dot', stdlib.UInt_max, 14), v60, v61, v62, v71, v176, v385, v387, v389]);
              sim_r.prevSt_noPrevTime = stdlib.digest(ctc12, [stdlib.checkedBigNumberify('./tut3.rsh:114:18:dot', stdlib.UInt_max, 14), v60, v61, v62, v71, v176, v385, v387]);
              const [v395] = txn5.data;
              const v399 = txn5.time;
              const v394 = txn5.from;
              
              stdlib.assert(true, {
                at: './tut3.rsh:114:18:dot',
                fs: ['at ./tut3.rsh:131:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                msg: null,
                who: 'Bob'
                });
              const v397 = stdlib.add(v387, stdlib.checkedBigNumberify('./tut3.rsh:decimal', stdlib.UInt_max, 0));
              sim_r.txns.push({
                amt: stdlib.checkedBigNumberify('./tut3.rsh:decimal', stdlib.UInt_max, 0),
                kind: 'to',
                tok: undefined
                });
              const v398 = stdlib.addressEq(v71, v394);
              stdlib.assert(v398, {
                at: './tut3.rsh:114:18:dot',
                fs: ['at ./tut3.rsh:131:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                msg: 'sender correct',
                who: 'Bob'
                });
              sim_r.nextSt = stdlib.digest(ctc9, [stdlib.checkedBigNumberify('./tut3.rsh:116:19:after expr stmt semicolon', stdlib.UInt_max, 16), v60, v61, v62, v71, v176, v385, v395, v397, v399]);
              sim_r.nextSt_noTime = stdlib.digest(ctc10, [stdlib.checkedBigNumberify('./tut3.rsh:116:19:after expr stmt semicolon', stdlib.UInt_max, 16), v60, v61, v62, v71, v176, v385, v395, v397]);
              sim_r.isHalt = false;
              
              return sim_r;
              })));
            if (txn5.didTimeout) {
              const txn6 = await (ctc.recv(17, 0, [], false, false));
              const [] = txn6.data;
              const v440 = txn6.time;
              const v436 = txn6.from;
              stdlib.assert(true, {
                at: 'reach standard library:209:7:dot',
                fs: ['at ./tut3.rsh:115:45:application call to "closeTo" (defined at: reach standard library:207:8:function exp)', 'at ./tut3.rsh:131:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                msg: null,
                who: 'Bob'
                });
              const v438 = stdlib.add(v387, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
              ;
              const v439 = stdlib.addressEq(v60, v436);
              stdlib.assert(v439, {
                at: 'reach standard library:209:7:dot',
                fs: ['at ./tut3.rsh:115:45:application call to "closeTo" (defined at: reach standard library:207:8:function exp)', 'at ./tut3.rsh:131:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                msg: 'sender correct',
                who: 'Bob'
                });
              ;
              stdlib.protect(ctc2, await interact.informTimeout(), {
                at: './tut3.rsh:66:33:application',
                fs: ['at ./tut3.rsh:65:13:application call to [unknown function] (defined at: ./tut3.rsh:65:25:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./tut3.rsh:64:32:function exp)', 'at ./tut3.rsh:115:45:application call to "closeTo" (defined at: reach standard library:207:8:function exp)', 'at ./tut3.rsh:131:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                msg: 'informTimeout',
                who: 'Bob'
                });
              return;
              }
            else {
              const [v395] = txn5.data;
              const v399 = txn5.time;
              const v394 = txn5.from;
              stdlib.assert(true, {
                at: './tut3.rsh:114:18:dot',
                fs: ['at ./tut3.rsh:131:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                msg: null,
                who: 'Bob'
                });
              const v397 = stdlib.add(v387, stdlib.checkedBigNumberify('./tut3.rsh:decimal', stdlib.UInt_max, 0));
              ;
              const v398 = stdlib.addressEq(v71, v394);
              stdlib.assert(v398, {
                at: './tut3.rsh:114:18:dot',
                fs: ['at ./tut3.rsh:131:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                msg: 'sender correct',
                who: 'Bob'
                });
              const txn6 = await (ctc.recv(18, 2, [ctc0, ctc3], false, v62));
              if (txn6.didTimeout) {
                const txn7 = await (ctc.sendrecv(19, 0, stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 8), [ctc5, ctc0, ctc0, ctc5, ctc0, ctc1, ctc3, ctc0, ctc0], [v60, v61, v62, v71, v176, v385, v395, v397, v399], [stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), []], [], true, true, false, (async (txn7) => {
                  const sim_r = { txns: [] };
                  sim_r.prevSt = stdlib.digest(ctc9, [stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 16), v60, v61, v62, v71, v176, v385, v395, v397, v399]);
                  sim_r.prevSt_noPrevTime = stdlib.digest(ctc10, [stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 16), v60, v61, v62, v71, v176, v385, v395, v397]);
                  const [] = txn7.data;
                  const v420 = txn7.time;
                  const v416 = txn7.from;
                  
                  stdlib.assert(true, {
                    at: 'reach standard library:209:7:dot',
                    fs: ['at ./tut3.rsh:121:45:application call to "closeTo" (defined at: reach standard library:207:8:function exp)', 'at ./tut3.rsh:131:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                    msg: null,
                    who: 'Bob'
                    });
                  const v418 = stdlib.add(v397, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
                  sim_r.txns.push({
                    amt: stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0),
                    kind: 'to',
                    tok: undefined
                    });
                  const v419 = stdlib.addressEq(v71, v416);
                  stdlib.assert(v419, {
                    at: 'reach standard library:209:7:dot',
                    fs: ['at ./tut3.rsh:121:45:application call to "closeTo" (defined at: reach standard library:207:8:function exp)', 'at ./tut3.rsh:131:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                    msg: 'sender correct',
                    who: 'Bob'
                    });
                  sim_r.txns.push({
                    amt: v418,
                    kind: 'from',
                    to: v71,
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
                const [] = txn7.data;
                const v420 = txn7.time;
                const v416 = txn7.from;
                stdlib.assert(true, {
                  at: 'reach standard library:209:7:dot',
                  fs: ['at ./tut3.rsh:121:45:application call to "closeTo" (defined at: reach standard library:207:8:function exp)', 'at ./tut3.rsh:131:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                  msg: null,
                  who: 'Bob'
                  });
                const v418 = stdlib.add(v397, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
                ;
                const v419 = stdlib.addressEq(v71, v416);
                stdlib.assert(v419, {
                  at: 'reach standard library:209:7:dot',
                  fs: ['at ./tut3.rsh:121:45:application call to "closeTo" (defined at: reach standard library:207:8:function exp)', 'at ./tut3.rsh:131:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                  msg: 'sender correct',
                  who: 'Bob'
                  });
                ;
                stdlib.protect(ctc2, await interact.informTimeout(), {
                  at: './tut3.rsh:66:33:application',
                  fs: ['at ./tut3.rsh:65:13:application call to [unknown function] (defined at: ./tut3.rsh:65:25:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./tut3.rsh:64:32:function exp)', 'at ./tut3.rsh:121:45:application call to "closeTo" (defined at: reach standard library:207:8:function exp)', 'at ./tut3.rsh:131:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                  msg: 'informTimeout',
                  who: 'Bob'
                  });
                return;
                }
              else {
                const [v404, v405] = txn6.data;
                const v409 = txn6.time;
                const v403 = txn6.from;
                stdlib.assert(true, {
                  at: './tut3.rsh:120:17:dot',
                  fs: ['at ./tut3.rsh:131:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                  msg: null,
                  who: 'Bob'
                  });
                const v407 = stdlib.add(v397, stdlib.checkedBigNumberify('./tut3.rsh:decimal', stdlib.UInt_max, 0));
                ;
                const v408 = stdlib.addressEq(v60, v403);
                stdlib.assert(v408, {
                  at: './tut3.rsh:120:17:dot',
                  fs: ['at ./tut3.rsh:131:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                  msg: 'sender correct',
                  who: 'Bob'
                  });
                const v411 = stdlib.digest(ctc4, [v404, v405]);
                const v412 = stdlib.digestEq(v385, v411);
                stdlib.assert(v412, {
                  at: 'reach standard library:65:17:application',
                  fs: ['at ./tut3.rsh:122:26:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)', 'at ./tut3.rsh:131:42:application call to "doRound" (defined at: ./tut3.rsh:100:54:function exp)'],
                  msg: null,
                  who: 'Bob'
                  });
                const v490 = v405[stdlib.checkedBigNumberify('./tut3.rsh:39:18:array ref', stdlib.UInt_max, 0)];
                const v491 = v395[stdlib.checkedBigNumberify('./tut3.rsh:39:29:array ref', stdlib.UInt_max, 0)];
                const v493 = stdlib.sub(stdlib.checkedBigNumberify('./tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v491);
                const v494 = stdlib.add(v490, v493);
                const v495 = stdlib.mod(v494, stdlib.checkedBigNumberify('./tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v499 = v405[stdlib.checkedBigNumberify('./tut3.rsh:39:18:array ref', stdlib.UInt_max, 1)];
                const v500 = v395[stdlib.checkedBigNumberify('./tut3.rsh:39:29:array ref', stdlib.UInt_max, 1)];
                const v502 = stdlib.sub(stdlib.checkedBigNumberify('./tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v500);
                const v503 = stdlib.add(v499, v502);
                const v504 = stdlib.mod(v503, stdlib.checkedBigNumberify('./tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v508 = v405[stdlib.checkedBigNumberify('./tut3.rsh:39:18:array ref', stdlib.UInt_max, 2)];
                const v509 = v395[stdlib.checkedBigNumberify('./tut3.rsh:39:29:array ref', stdlib.UInt_max, 2)];
                const v511 = stdlib.sub(stdlib.checkedBigNumberify('./tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v509);
                const v512 = stdlib.add(v508, v511);
                const v513 = stdlib.mod(v512, stdlib.checkedBigNumberify('./tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v517 = v405[stdlib.checkedBigNumberify('./tut3.rsh:39:18:array ref', stdlib.UInt_max, 3)];
                const v518 = v395[stdlib.checkedBigNumberify('./tut3.rsh:39:29:array ref', stdlib.UInt_max, 3)];
                const v520 = stdlib.sub(stdlib.checkedBigNumberify('./tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v518);
                const v521 = stdlib.add(v517, v520);
                const v522 = stdlib.mod(v521, stdlib.checkedBigNumberify('./tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v526 = v405[stdlib.checkedBigNumberify('./tut3.rsh:39:18:array ref', stdlib.UInt_max, 4)];
                const v527 = v395[stdlib.checkedBigNumberify('./tut3.rsh:39:29:array ref', stdlib.UInt_max, 4)];
                const v529 = stdlib.sub(stdlib.checkedBigNumberify('./tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v527);
                const v530 = stdlib.add(v526, v529);
                const v531 = stdlib.mod(v530, stdlib.checkedBigNumberify('./tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v544 = stdlib.eq(v495, stdlib.checkedBigNumberify('./tut3.rsh:makeEnum', stdlib.UInt_max, 1));
                const v545 = v544 ? v504 : v495;
                const v548 = stdlib.eq(v545, stdlib.checkedBigNumberify('./tut3.rsh:makeEnum', stdlib.UInt_max, 1));
                const v549 = v548 ? v513 : v545;
                const v552 = stdlib.eq(v549, stdlib.checkedBigNumberify('./tut3.rsh:makeEnum', stdlib.UInt_max, 1));
                const v553 = v552 ? v522 : v549;
                const v556 = stdlib.eq(v553, stdlib.checkedBigNumberify('./tut3.rsh:makeEnum', stdlib.UInt_max, 1));
                const v557 = v556 ? v531 : v553;
                const v560 = stdlib.add(v176, stdlib.checkedBigNumberify('./tut3.rsh:132:67:decimal', stdlib.UInt_max, 1));
                const cv175 = v557;
                const cv176 = v560;
                const cv619 = v409;
                const cv621 = v407;
                
                v175 = cv175;
                v176 = cv176;
                v619 = cv619;
                v621 = cv621;
                
                continue;}
              }
            }
          }}
      const v561 = stdlib.eq(v175, stdlib.checkedBigNumberify('./tut3.rsh:makeEnum', stdlib.UInt_max, 2));
      const v564 = stdlib.mul(stdlib.checkedBigNumberify('./tut3.rsh:138:16:decimal', stdlib.UInt_max, 2), v61);
      const v566 = v561 ? v60 : v71;
      ;
      stdlib.protect(ctc2, await interact.seeOutcome(v175), {
        at: './tut3.rsh:142:28:application',
        fs: ['at ./tut3.rsh:141:11:application call to [unknown function] (defined at: ./tut3.rsh:141:23:function exp)'],
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
gtxn 2 Sender
byte "{{m14}}"
==
||
gtxn 2 Sender
byte "{{m15}}"
==
||
gtxn 2 Sender
byte "{{m16}}"
==
||
gtxn 2 Sender
byte "{{m17}}"
==
||
gtxn 2 Sender
byte "{{m18}}"
==
||
gtxn 2 Sender
byte "{{m19}}"
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
  stepargs: [0, 129, 209, 169, 289, 241, 0, 0, 209, 177, 249, 209, 297, 249, 209, 177, 249, 209, 297, 249],
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
// "./tut3.rsh:76:9:dot"
// "[]"
int 1
assert
int 0
gtxna 0 ApplicationArgs 5
btoi
+
store 255
// CheckPay
// "./tut3.rsh:76:9:dot"
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
// "./tut3.rsh:76:9:dot"
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
// "./tut3.rsh:85:9:dot"
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
// "./tut3.rsh:85:9:dot"
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
// "./tut3.rsh:85:9:dot"
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
// "[at ./tut3.rsh:87:41:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
int 1
assert
// CheckPay
// "reach standard library:209:7:dot"
// "[at ./tut3.rsh:87:41:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
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
// "[at ./tut3.rsh:87:41:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
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
// "./tut3.rsh:93:9:dot"
// "[]"
int 1
assert
gtxna 0 ApplicationArgs 11
btoi
int 0
+
store 255
// CheckPay
// "./tut3.rsh:93:9:dot"
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
// "./tut3.rsh:93:9:dot"
// "[]"
gtxna 0 ApplicationArgs 5
gtxn 0 Sender
==
assert
// Nothing
// "reach standard library:65:17:application"
// "[at ./tut3.rsh:95:22:application call to \"checkCommitment\" (defined at: reach standard library:64:8:function exp)]"
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
int 0
itob
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
// "[at ./tut3.rsh:94:40:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
int 1
assert
// CheckPay
// "reach standard library:209:7:dot"
// "[at ./tut3.rsh:94:40:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
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
// "[at ./tut3.rsh:94:40:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
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
// "./tut3.rsh:107:17:dot"
// "[at ./tut3.rsh:127:42:application call to \"doRound\" (defined at: ./tut3.rsh:100:54:function exp)]"
int 1
assert
gtxna 0 ApplicationArgs 10
btoi
int 0
+
store 255
// CheckPay
// "./tut3.rsh:107:17:dot"
// "[at ./tut3.rsh:127:42:application call to \"doRound\" (defined at: ./tut3.rsh:100:54:function exp)]"
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
// "./tut3.rsh:107:17:dot"
// "[at ./tut3.rsh:127:42:application call to \"doRound\" (defined at: ./tut3.rsh:100:54:function exp)]"
gtxna 0 ApplicationArgs 8
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
// "[at ./tut3.rsh:108:45:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp),at ./tut3.rsh:127:42:application call to \"doRound\" (defined at: ./tut3.rsh:100:54:function exp)]"
int 1
assert
// CheckPay
// "reach standard library:209:7:dot"
// "[at ./tut3.rsh:108:45:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp),at ./tut3.rsh:127:42:application call to \"doRound\" (defined at: ./tut3.rsh:100:54:function exp)]"
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
// "[at ./tut3.rsh:108:45:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp),at ./tut3.rsh:127:42:application call to \"doRound\" (defined at: ./tut3.rsh:100:54:function exp)]"
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
// "./tut3.rsh:114:18:dot"
// "[at ./tut3.rsh:127:42:application call to \"doRound\" (defined at: ./tut3.rsh:100:54:function exp)]"
int 1
assert
gtxna 0 ApplicationArgs 11
btoi
int 0
+
store 255
// CheckPay
// "./tut3.rsh:114:18:dot"
// "[at ./tut3.rsh:127:42:application call to \"doRound\" (defined at: ./tut3.rsh:100:54:function exp)]"
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
// "./tut3.rsh:114:18:dot"
// "[at ./tut3.rsh:127:42:application call to \"doRound\" (defined at: ./tut3.rsh:100:54:function exp)]"
gtxna 0 ApplicationArgs 5
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
gtxna 0 ApplicationArgs 10
concat
gtxna 0 ApplicationArgs 12
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
// "[at ./tut3.rsh:115:45:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp),at ./tut3.rsh:127:42:application call to \"doRound\" (defined at: ./tut3.rsh:100:54:function exp)]"
int 1
assert
// CheckPay
// "reach standard library:209:7:dot"
// "[at ./tut3.rsh:115:45:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp),at ./tut3.rsh:127:42:application call to \"doRound\" (defined at: ./tut3.rsh:100:54:function exp)]"
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
// "[at ./tut3.rsh:115:45:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp),at ./tut3.rsh:127:42:application call to \"doRound\" (defined at: ./tut3.rsh:100:54:function exp)]"
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
int 15
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
gtxna 0 ApplicationArgs 12
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
// "./tut3.rsh:120:17:dot"
// "[at ./tut3.rsh:127:42:application call to \"doRound\" (defined at: ./tut3.rsh:100:54:function exp)]"
int 1
assert
gtxna 0 ApplicationArgs 12
btoi
int 0
+
store 255
// CheckPay
// "./tut3.rsh:120:17:dot"
// "[at ./tut3.rsh:127:42:application call to \"doRound\" (defined at: ./tut3.rsh:100:54:function exp)]"
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
// "./tut3.rsh:120:17:dot"
// "[at ./tut3.rsh:127:42:application call to \"doRound\" (defined at: ./tut3.rsh:100:54:function exp)]"
gtxna 0 ApplicationArgs 8
gtxn 0 Sender
==
assert
// Nothing
// "reach standard library:65:17:application"
// "[at ./tut3.rsh:122:26:application call to \"checkCommitment\" (defined at: reach standard library:64:8:function exp),at ./tut3.rsh:127:42:application call to \"doRound\" (defined at: ./tut3.rsh:100:54:function exp)]"
gtxna 0 ApplicationArgs 10
gtxna 0 ApplicationArgs 13
gtxna 0 ApplicationArgs 14
concat
keccak256
==
assert
gtxna 0 ApplicationArgs 11
substring 0 8
btoi
int 4
gtxna 0 ApplicationArgs 14
substring 0 8
btoi
-
+
int 3
%
dup
store 254
gtxna 0 ApplicationArgs 11
substring 8 16
btoi
int 4
gtxna 0 ApplicationArgs 14
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
gtxna 0 ApplicationArgs 11
substring 16 24
btoi
int 4
gtxna 0 ApplicationArgs 14
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
gtxna 0 ApplicationArgs 11
substring 24 32
btoi
int 4
gtxna 0 ApplicationArgs 14
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
gtxna 0 ApplicationArgs 11
substring 32 40
btoi
int 4
gtxna 0 ApplicationArgs 14
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
store 250
gtxna 0 ApplicationArgs 9
btoi
int 1
+
store 249
load 250
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
load 249
itob
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
gtxna 0 ApplicationArgs 12
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
// "[at ./tut3.rsh:121:45:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp),at ./tut3.rsh:127:42:application call to \"doRound\" (defined at: ./tut3.rsh:100:54:function exp)]"
int 1
assert
// CheckPay
// "reach standard library:209:7:dot"
// "[at ./tut3.rsh:121:45:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp),at ./tut3.rsh:127:42:application call to \"doRound\" (defined at: ./tut3.rsh:100:54:function exp)]"
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
// "[at ./tut3.rsh:121:45:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp),at ./tut3.rsh:127:42:application call to \"doRound\" (defined at: ./tut3.rsh:100:54:function exp)]"
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
gtxna 0 ApplicationArgs 12
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
// Handler 14
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
// "./tut3.rsh:107:17:dot"
// "[at ./tut3.rsh:131:42:application call to \"doRound\" (defined at: ./tut3.rsh:100:54:function exp)]"
int 1
assert
gtxna 0 ApplicationArgs 10
btoi
int 0
+
store 255
// CheckPay
// "./tut3.rsh:107:17:dot"
// "[at ./tut3.rsh:131:42:application call to \"doRound\" (defined at: ./tut3.rsh:100:54:function exp)]"
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
// "./tut3.rsh:107:17:dot"
// "[at ./tut3.rsh:131:42:application call to \"doRound\" (defined at: ./tut3.rsh:100:54:function exp)]"
gtxna 0 ApplicationArgs 5
gtxn 0 Sender
==
assert
// compute state in HM_Set 14
int 14
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
// Handler 15
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
// "[at ./tut3.rsh:108:45:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp),at ./tut3.rsh:131:42:application call to \"doRound\" (defined at: ./tut3.rsh:100:54:function exp)]"
int 1
assert
// CheckPay
// "reach standard library:209:7:dot"
// "[at ./tut3.rsh:108:45:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp),at ./tut3.rsh:131:42:application call to \"doRound\" (defined at: ./tut3.rsh:100:54:function exp)]"
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
// "[at ./tut3.rsh:108:45:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp),at ./tut3.rsh:131:42:application call to \"doRound\" (defined at: ./tut3.rsh:100:54:function exp)]"
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
// Handler 16
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
// compute state in HM_Check 14
int 14
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
// "./tut3.rsh:114:18:dot"
// "[at ./tut3.rsh:131:42:application call to \"doRound\" (defined at: ./tut3.rsh:100:54:function exp)]"
int 1
assert
gtxna 0 ApplicationArgs 11
btoi
int 0
+
store 255
// CheckPay
// "./tut3.rsh:114:18:dot"
// "[at ./tut3.rsh:131:42:application call to \"doRound\" (defined at: ./tut3.rsh:100:54:function exp)]"
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
// "./tut3.rsh:114:18:dot"
// "[at ./tut3.rsh:131:42:application call to \"doRound\" (defined at: ./tut3.rsh:100:54:function exp)]"
gtxna 0 ApplicationArgs 8
gtxn 0 Sender
==
assert
// compute state in HM_Set 16
int 16
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
gtxna 0 ApplicationArgs 12
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
// Handler 17
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
// compute state in HM_Check 14
int 14
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
// "[at ./tut3.rsh:115:45:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp),at ./tut3.rsh:131:42:application call to \"doRound\" (defined at: ./tut3.rsh:100:54:function exp)]"
int 1
assert
// CheckPay
// "reach standard library:209:7:dot"
// "[at ./tut3.rsh:115:45:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp),at ./tut3.rsh:131:42:application call to \"doRound\" (defined at: ./tut3.rsh:100:54:function exp)]"
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
// "[at ./tut3.rsh:115:45:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp),at ./tut3.rsh:131:42:application call to \"doRound\" (defined at: ./tut3.rsh:100:54:function exp)]"
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
`, `#pragma version 3
// Handler 18
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
int 15
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
// compute state in HM_Check 16
int 16
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
gtxna 0 ApplicationArgs 12
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
// "./tut3.rsh:120:17:dot"
// "[at ./tut3.rsh:131:42:application call to \"doRound\" (defined at: ./tut3.rsh:100:54:function exp)]"
int 1
assert
gtxna 0 ApplicationArgs 12
btoi
int 0
+
store 255
// CheckPay
// "./tut3.rsh:120:17:dot"
// "[at ./tut3.rsh:131:42:application call to \"doRound\" (defined at: ./tut3.rsh:100:54:function exp)]"
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
// "./tut3.rsh:120:17:dot"
// "[at ./tut3.rsh:131:42:application call to \"doRound\" (defined at: ./tut3.rsh:100:54:function exp)]"
gtxna 0 ApplicationArgs 5
gtxn 0 Sender
==
assert
// Nothing
// "reach standard library:65:17:application"
// "[at ./tut3.rsh:122:26:application call to \"checkCommitment\" (defined at: reach standard library:64:8:function exp),at ./tut3.rsh:131:42:application call to \"doRound\" (defined at: ./tut3.rsh:100:54:function exp)]"
gtxna 0 ApplicationArgs 10
gtxna 0 ApplicationArgs 13
gtxna 0 ApplicationArgs 14
concat
keccak256
==
assert
gtxna 0 ApplicationArgs 14
substring 0 8
btoi
int 4
gtxna 0 ApplicationArgs 11
substring 0 8
btoi
-
+
int 3
%
dup
store 254
gtxna 0 ApplicationArgs 14
substring 8 16
btoi
int 4
gtxna 0 ApplicationArgs 11
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
gtxna 0 ApplicationArgs 14
substring 16 24
btoi
int 4
gtxna 0 ApplicationArgs 11
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
gtxna 0 ApplicationArgs 14
substring 24 32
btoi
int 4
gtxna 0 ApplicationArgs 11
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
gtxna 0 ApplicationArgs 14
substring 32 40
btoi
int 4
gtxna 0 ApplicationArgs 11
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
store 250
gtxna 0 ApplicationArgs 9
btoi
int 1
+
store 249
load 250
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
load 249
itob
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
// Handler 19
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
// compute state in HM_Check 16
int 16
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
gtxna 0 ApplicationArgs 12
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
// "[at ./tut3.rsh:121:45:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp),at ./tut3.rsh:131:42:application call to \"doRound\" (defined at: ./tut3.rsh:100:54:function exp)]"
int 1
assert
// CheckPay
// "reach standard library:209:7:dot"
// "[at ./tut3.rsh:121:45:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp),at ./tut3.rsh:131:42:application call to \"doRound\" (defined at: ./tut3.rsh:100:54:function exp)]"
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
// "[at ./tut3.rsh:121:45:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp),at ./tut3.rsh:131:42:application call to \"doRound\" (defined at: ./tut3.rsh:100:54:function exp)]"
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
gtxna 0 ApplicationArgs 12
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
                "name": "v176",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v199",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v201",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v203",
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
                "name": "v209",
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
                "name": "v176",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v199",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v201",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v203",
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
                "name": "v176",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v199",
                "type": "uint256"
              },
              {
                "internalType": "uint256[5]",
                "name": "v209",
                "type": "uint256[5]"
              },
              {
                "internalType": "uint256",
                "name": "v211",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v213",
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
                "name": "v218",
                "type": "uint256"
              },
              {
                "internalType": "uint256[5]",
                "name": "v219",
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
                "name": "v176",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v199",
                "type": "uint256"
              },
              {
                "internalType": "uint256[5]",
                "name": "v209",
                "type": "uint256[5]"
              },
              {
                "internalType": "uint256",
                "name": "v211",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v213",
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
                "internalType": "address payable",
                "name": "v71",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v176",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v619",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v621",
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
                "name": "v385",
                "type": "uint256"
              }
            ],
            "internalType": "struct T31",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct T32",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "e14",
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
                "name": "v176",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v619",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v621",
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
    "name": "e15",
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
                "name": "v176",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v385",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v387",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v389",
                "type": "uint256"
              }
            ],
            "internalType": "struct T30",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256[5]",
                "name": "v395",
                "type": "uint256[5]"
              }
            ],
            "internalType": "struct T34",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct T35",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "e16",
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
                "name": "v176",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v385",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v387",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v389",
                "type": "uint256"
              }
            ],
            "internalType": "struct T30",
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
        "internalType": "struct T36",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "e17",
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
                "name": "v176",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v385",
                "type": "uint256"
              },
              {
                "internalType": "uint256[5]",
                "name": "v395",
                "type": "uint256[5]"
              },
              {
                "internalType": "uint256",
                "name": "v397",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v399",
                "type": "uint256"
              }
            ],
            "internalType": "struct T33",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v404",
                "type": "uint256"
              },
              {
                "internalType": "uint256[5]",
                "name": "v405",
                "type": "uint256[5]"
              }
            ],
            "internalType": "struct T37",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct T38",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "e18",
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
                "name": "v176",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v385",
                "type": "uint256"
              },
              {
                "internalType": "uint256[5]",
                "name": "v395",
                "type": "uint256[5]"
              },
              {
                "internalType": "uint256",
                "name": "v397",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v399",
                "type": "uint256"
              }
            ],
            "internalType": "struct T33",
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
        "internalType": "struct T39",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "e19",
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
                "name": "v176",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v619",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v621",
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
                "name": "v199",
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
                "name": "v176",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v619",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v621",
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
                "name": "v176",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v199",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v201",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v203",
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
                "name": "v209",
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
                "name": "v176",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v199",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v201",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v203",
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
                "name": "v176",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v199",
                "type": "uint256"
              },
              {
                "internalType": "uint256[5]",
                "name": "v209",
                "type": "uint256[5]"
              },
              {
                "internalType": "uint256",
                "name": "v211",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v213",
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
                "name": "v218",
                "type": "uint256"
              },
              {
                "internalType": "uint256[5]",
                "name": "v219",
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
                "name": "v176",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v199",
                "type": "uint256"
              },
              {
                "internalType": "uint256[5]",
                "name": "v209",
                "type": "uint256[5]"
              },
              {
                "internalType": "uint256",
                "name": "v211",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v213",
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
                "internalType": "address payable",
                "name": "v71",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v176",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v619",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v621",
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
                "name": "v385",
                "type": "uint256"
              }
            ],
            "internalType": "struct T31",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "internalType": "struct T32",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "m14",
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
                "name": "v176",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v619",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v621",
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
    "name": "m15",
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
                "name": "v176",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v385",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v387",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v389",
                "type": "uint256"
              }
            ],
            "internalType": "struct T30",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256[5]",
                "name": "v395",
                "type": "uint256[5]"
              }
            ],
            "internalType": "struct T34",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "internalType": "struct T35",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "m16",
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
                "name": "v176",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v385",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v387",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v389",
                "type": "uint256"
              }
            ],
            "internalType": "struct T30",
            "name": "svs",
            "type": "tuple"
          },
          {
            "internalType": "bool",
            "name": "msg",
            "type": "bool"
          }
        ],
        "internalType": "struct T36",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "m17",
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
                "name": "v176",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v385",
                "type": "uint256"
              },
              {
                "internalType": "uint256[5]",
                "name": "v395",
                "type": "uint256[5]"
              },
              {
                "internalType": "uint256",
                "name": "v397",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v399",
                "type": "uint256"
              }
            ],
            "internalType": "struct T33",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v404",
                "type": "uint256"
              },
              {
                "internalType": "uint256[5]",
                "name": "v405",
                "type": "uint256[5]"
              }
            ],
            "internalType": "struct T37",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "internalType": "struct T38",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "m18",
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
                "name": "v176",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v385",
                "type": "uint256"
              },
              {
                "internalType": "uint256[5]",
                "name": "v395",
                "type": "uint256[5]"
              },
              {
                "internalType": "uint256",
                "name": "v397",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v399",
                "type": "uint256"
              }
            ],
            "internalType": "struct T33",
            "name": "svs",
            "type": "tuple"
          },
          {
            "internalType": "bool",
            "name": "msg",
            "type": "bool"
          }
        ],
        "internalType": "struct T39",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "m19",
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
                "name": "v176",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v619",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v621",
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
                "name": "v199",
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
                "name": "v176",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v619",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v621",
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
  Bytecode: `0x608060408190527f49ff028a829527a47ec6839c7147b484eccf5a2a94853eddac09cef44d9d4e9e90600090a1604080516020808201835243825282518082018452600080825292518152835180830184905290518185015283518082038501815260609091019093528251920191909120905561295a806100826000396000f3fe6080604052600436106101025760003560e01c8063515d273111610095578063b2dc9b3c11610064578063b2dc9b3c146101f4578063b830ed6e14610207578063c74327551461021a578063d10ac9221461022d578063d22bdb581461024057610109565b8063515d2731146101a857806355b09c29146101bb5780636dac4fdc146101ce578063a701f0cc146101e157610109565b80633aa258ab116100d15780633aa258ab1461015c5780633df7a7891461016f578063497ccc6a146101825780634f4e2b061461019557610109565b8063253d71be1461010e5780632bf4f873146101235780632d8ee29e1461013657806338015b161461014957610109565b3661010957005b600080fd5b61012161011c36600461219d565b610253565b005b610121610131366004612229565b61037c565b61012161014436600461220d565b610521565b6101216101573660046121d5565b61063b565b61012161016a3660046121d5565b610753565b61012161017d3660046121b9565b610865565b6101216101903660046121b9565b610a1d565b6101216101a336600461219d565b610b2f565b6101216101b636600461223a565b610cc2565b6101216101c93660046121f1565b610e15565b6101216101dc3660046121b9565b61112c565b6101216101ef3660046121f1565b611244565b61012161020236600461219d565b6114f6565b61012161021536600461220d565b611686565b6101216102283660046121b9565b61179a565b61012161023b36600461218b565b61194f565b61012161024e36600461224c565b611c37565b604051610267906002908390602001612814565b6040516020818303038152906040528051906020012060001c6000541461028d57600080fd5b600080556102a460408201356101608301356128a0565b43101580156102b1575060015b6102ba57600080fd5b34156102c557600080fd5b336102d660a083016080840161216a565b6001600160a01b0316146102e957600080fd5b6102f960a082016080830161216a565b6001600160a01b03166108fc61031560006101408501356128a0565b6040518115909202916000818181858888f1935050505015801561033d573d6000803e3d6000fd5b507fc18ab0af979eec50e5539334a3b97c236f4df25f2c0886da35b2f866708b0b138160405161036d919061259e565b60405180910390a16000805533ff5b60408051600060208201528235918101919091526060016040516020818303038152906040528051906020012060001c600054146103b957600080fd5b600080805560408051602081019091529081526103db602083013560006128a0565b8152346020830135146103ed57600080fd5b60408051833581526020808501359082015283820135818301526060808501359082015290517f2bb570a5feee0f446e450005a048c78efd478914692e1f0be1009bac144b11709181900360800190a161047f6040518060c0016040528060006001600160a01b0316815260200160008152602001600081526020016000815260200160008152602001600081525090565b338152602083810135818301908152604080860135818501908152606080880135818701908152875160808089019182524360a0808b01918252875160019a81019a909a528a516001600160a01b0316978a019790975296519388019390935292519186019190915251918401919091525160c08301525160e0820152610100015b60408051601f198184030181529190528051602090910120600055505050565b6040516105359060109083906020016127ea565b6040516020818303038152906040528051906020012060001c6000541461055b57600080fd5b6000805561057260408201356101808301356128a0565b431015801561057f575060015b61058857600080fd5b341561059357600080fd5b336105a4608083016060840161216a565b6001600160a01b0316146105b757600080fd5b6105c7608082016060830161216a565b6001600160a01b03166108fc6105e360006101608501356128a0565b6040518115909202916000818181858888f1935050505015801561060b573d6000803e3d6000fd5b507fd6fbb3cbca99542c0c2ad4a426f274a3e1b0990f868dfa7680cc48800395e0fd8160405161036d9190612663565b60405161064f906008908390602001612757565b6040516020818303038152906040528051906020012060001c6000541461067557600080fd5b6000805561068b604082013560e08301356128a0565b4310158015610698575060015b6106a157600080fd5b34156106ac57600080fd5b336106bd608083016060840161216a565b6001600160a01b0316146106d057600080fd5b6106e0608082016060830161216a565b6001600160a01b03166108fc6106fb600060c08501356128a0565b6040518115909202916000818181858888f19350505050158015610723573d6000803e3d6000fd5b507fb9d135d4afaa7938b4616c3637968c0e71f66413b54043da6188989930963f898160405161036d9190612634565b60405161076790600e908390602001612757565b6040516020818303038152906040528051906020012060001c6000541461078d57600080fd5b600080556107a3604082013560e08301356128a0565b43101580156107b0575060015b6107b957600080fd5b34156107c457600080fd5b336107d2602083018361216a565b6001600160a01b0316146107e557600080fd5b6107f2602082018261216a565b6001600160a01b03166108fc61080d600060c08501356128a0565b6040518115909202916000818181858888f19350505050158015610835573d6000803e3d6000fd5b507fe592bdad3da2420c74b7bd9841a064dd887014772397104fb0e8f350ee4ab2c68160405161036d9190612634565b6040516108799060069083906020016126e0565b6040516020818303038152906040528051906020012060001c6000541461089f57600080fd5b6000808055604080516020810182529182526108c29083013560a08401356128a0565b43106108cd57600080fd5b6108dc600060c08401356128a0565b815234156108e957600080fd5b336108fa608084016060850161216a565b6001600160a01b03161461090d57600080fd5b7f6739bb4acbb3812eea51c48895245b154776bf02dd61571666f6abba93266fec8260405161093c91906125d1565b60405180910390a161099e60405180610100016040528060006001600160a01b03168152602001600081526020016000815260200160006001600160a01b03168152602001600081526020016000815260200160008152602001600081525090565b6109ab602084018461216a565b6001600160a01b0316815260208084013590820152604080840135908201526109da608084016060850161216a565b6001600160a01b031660608201526080808401359082015260e08084013560a0830152825160c0830152439082015260405161050190600890839060200161276c565b604051610a319060069083906020016126e0565b6040516020818303038152906040528051906020012060001c60005414610a5757600080fd5b60008055610a6d604082013560a08301356128a0565b4310158015610a7a575060015b610a8357600080fd5b3415610a8e57600080fd5b33610a9c602083018361216a565b6001600160a01b031614610aaf57600080fd5b610abc602082018261216a565b6001600160a01b03166108fc610ad7600060c08501356128a0565b6040518115909202916000818181858888f19350505050158015610aff573d6000803e3d6000fd5b507fcdae4cbd433f8c4039f23f2632824e3ab0089e9b8c2050e8e87e4d6e0a3df09b8160405161036d91906125ee565b604051610b4390600e908390602001612757565b6040516020818303038152906040528051906020012060001c60005414610b6957600080fd5b600080805560408051602081018252918252610b8c9083013560e08401356128a0565b4310610b9757600080fd5b610ba6600060c08401356128a0565b81523415610bb357600080fd5b33610bc4608084016060850161216a565b6001600160a01b031614610bd757600080fd5b7f1ce2536efdc067dd1e872bca971868b9750c7f8faa8b4eece19fa68af9cb89c082604051610c06919061261f565b60405180910390a1610c16611f74565b610c23602084018461216a565b6001600160a01b031681526020808401359082015260408084013590820152610c52608084016060850161216a565b6001600160a01b031660608201526080808401359082015260a0808401358183015260408051808301909152906101008501906005908390839080828437600092019190915250505060c0820152815160e0820152436101008201526040516105019060109083906020016127ff565b604051610cd69060019083906020016127d6565b6040516020818303038152906040528051906020012060001c60005414610cfc57600080fd5b600080805560408051602081018252918252610d1f9083013560a08401356128a0565b4310610d2a57600080fd5b610d3c602083013560808401356128a0565b815234602083013514610d4e57600080fd5b7f128ceb6e462bfebf9ef10870b6d9ae608efb33f1cdefac45d94895b5f28af76e82604051610d7d9190612672565b60405180910390a1610d8d611fd8565b610d9a602084018461216a565b6001600160a01b03168152602080840135908201526040808401358183015260608085013590830152336080830152805160a08181019092529060c08501906005908390839080828437600092019190915250505060a0820152815160c08201524360e0820152604051610501906002908390602001612829565b604051610e299060109083906020016127ea565b6040516020818303038152906040528051906020012060001c60005414610e4f57600080fd5b60008081905550610e816040518060800160405280600081526020016000815260200160008152602001600081525090565b610e9460408301356101808401356128a0565b4310610e9f57600080fd5b3415610eaa57600080fd5b33610eb8602084018461216a565b6001600160a01b031614610ecb57600080fd5b604051610ee7906101a0840135906101c08501906020016126c6565b60408051601f19818403018152919052805160209091012060a083013514610f0e57600080fd5b6003610f1f60c084013560046128d7565b610f2e906101c08501356128a0565b610f3891906128ee565b808252600114610f49578051610f73565b6003610f5a60e084013560046128d7565b610f69906101e08501356128a0565b610f7391906128ee565b60208201819052600114610f8b578060200151610fb6565b6003610f9d61010084013560046128d7565b610fac906102008501356128a0565b610fb691906128ee565b60408201819052600114610fce578060400151610ff9565b6003610fe061012084013560046128d7565b610fef906102208501356128a0565b610ff991906128ee565b60608201526040517f523e44f49032f50206e5ffcdb88da3392c2e81e3e5608b43c05b289ebe2c3b9e9061102e908490612643565b60405180910390a161103e612021565b61104b602084018461216a565b81516001600160a01b03909116905280516020808501359101528051604080850135910152611080608084016060850161216a565b81516001600160a01b039091166060918201528201516001146110a75781606001516110da565b60036110b961014085013560046128d7565b6101c0850160045b60200201356110d091906128a0565b6110da91906128ee565b6020820151526110ef600160808501356128a0565b6020808301805190910191909152514360409091015261111560006101608501356128a0565b60208201516060015261112781611d49565b505050565b6040516111409060069083906020016126e0565b6040516020818303038152906040528051906020012060001c6000541461116657600080fd5b6000805561117c604082013560a08301356128a0565b4310158015611189575060015b61119257600080fd5b341561119d57600080fd5b336111ae608083016060840161216a565b6001600160a01b0316146111c157600080fd5b6111d1608082016060830161216a565b6001600160a01b03166108fc6111ec600060c08501356128a0565b6040518115909202916000818181858888f19350505050158015611214573d6000803e3d6000fd5b507fe9b2c2b8b894a17634956282f47287e65395bf5f12d38d24114f9df9d064cb938160405161036d91906125ee565b60405161125890600a9083906020016127ea565b6040516020818303038152906040528051906020012060001c6000541461127e57600080fd5b600080819055506112b06040518060800160405280600081526020016000815260200160008152602001600081525090565b6112c360408301356101808401356128a0565b43106112ce57600080fd5b34156112d957600080fd5b336112ea608084016060850161216a565b6001600160a01b0316146112fd57600080fd5b604051611319906101a0840135906101c08501906020016126c6565b60408051601f19818403018152919052805160209091012060a08301351461134057600080fd5b60036113526101c084013560046128d7565b6113609060c08501356128a0565b61136a91906128ee565b80825260011461137b5780516113a5565b600361138d6101e084013560046128d7565b61139b9060e08501356128a0565b6113a591906128ee565b602082018190526001146113bd5780602001516113e8565b60036113cf61020084013560046128d7565b6113de906101008501356128a0565b6113e891906128ee565b6040820181905260011461140057806040015161142b565b600361141261022084013560046128d7565b611421906101208501356128a0565b61142b91906128ee565b60608201526040517f6ee71c1c32e4cf4bb84253364ba7d5a7f07dc28f60fbebebac51aae83206b87290611460908490612643565b60405180910390a1611470612021565b61147d602084018461216a565b81516001600160a01b039091169052805160208085013591015280516040808501359101526114b2608084016060850161216a565b81516001600160a01b039091166060918201528201516001146114d95781606001516110da565b60036114eb61024085013560046128d7565b60c0850160046110c1565b60405161150a906008908390602001612757565b6040516020818303038152906040528051906020012060001c6000541461153057600080fd5b6000808055604080516020810182529182526115539083013560e08401356128a0565b431061155e57600080fd5b61156d600060c08401356128a0565b8152341561157a57600080fd5b33611588602084018461216a565b6001600160a01b03161461159b57600080fd5b7f4b39e6355207f2c77bb98c443fb3e5e02a82c850a077da7e0938cd671b364487826040516115ca919061261f565b60405180910390a16115da611f74565b6115e7602084018461216a565b6001600160a01b031681526020808401359082015260408084013590820152611616608084016060850161216a565b6001600160a01b031660608201526080808401359082015260a0808401358183015260408051808301909152906101008501906005908390839080828437600092019190915250505060c0820152815160e08201524361010082015260405161050190600a9083906020016127ff565b60405161169a90600a9083906020016127ea565b6040516020818303038152906040528051906020012060001c600054146116c057600080fd5b600080556116d760408201356101808301356128a0565b43101580156116e4575060015b6116ed57600080fd5b34156116f857600080fd5b33611706602083018361216a565b6001600160a01b03161461171957600080fd5b611726602082018261216a565b6001600160a01b03166108fc61174260006101608501356128a0565b6040518115909202916000818181858888f1935050505015801561176a573d6000803e3d6000fd5b507ff145fb2384258d0f3c54ba74b14d362f8412b221ff369ece9d48eb2ad8e034cf8160405161036d9190612663565b6040516117ae9060069083906020016126e0565b6040516020818303038152906040528051906020012060001c600054146117d457600080fd5b6000808055604080516020810182529182526117f79083013560a08401356128a0565b431061180257600080fd5b611811600060c08401356128a0565b8152341561181e57600080fd5b3361182c602084018461216a565b6001600160a01b03161461183f57600080fd5b7f8034f7b80d60c125999e5b76eb5d44b464ae8dc58c5f3258d4e1e4d9055978988260405161186e91906125d1565b60405180910390a16118d060405180610100016040528060006001600160a01b03168152602001600081526020016000815260200160006001600160a01b03168152602001600081526020016000815260200160008152602001600081525090565b6118dd602084018461216a565b6001600160a01b03168152602080840135908201526040808401359082015261190c608084016060850161216a565b6001600160a01b031660608201526080808401359082015260e08084013560a0830152825160c0830152439082015260405161050190600e90839060200161276c565b604051611963906002908390602001612814565b6040516020818303038152906040528051906020012060001c6000541461198957600080fd5b600080819055506119bb6040518060800160405280600081526020016000815260200160008152602001600081525090565b6119ce60408301356101608401356128a0565b43106119d957600080fd5b34156119e457600080fd5b336119f2602084018461216a565b6001600160a01b031614611a0557600080fd5b604051611a2190610180840135906101a08501906020016126c6565b60408051601f198184030181529190528051602090910120606083013514611a4857600080fd5b6003611a5960a084013560046128d7565b611a68906101a08501356128a0565b611a7291906128ee565b808252600114611a83578051611aad565b6003611a9460c084013560046128d7565b611aa3906101c08501356128a0565b611aad91906128ee565b60208201819052600114611ac5578060200151611aef565b6003611ad660e084013560046128d7565b611ae5906101e08501356128a0565b611aef91906128ee565b60408201819052600114611b07578060400151611b32565b6003611b1961010084013560046128d7565b611b28906102008501356128a0565b611b3291906128ee565b60608201526040517f85b984f93e23bb278dcf4c426e3ebada3df03a830c17f29b763cc686eeff603090611b67908490612577565b60405180910390a1611b77612021565b611b84602084018461216a565b81516001600160a01b03909116905280516020808501359101528051604080850135910152611bb960a084016080850161216a565b81516001600160a01b03909116606091820152820151600114611be0578160600151611c0b565b6003611bf261012085013560046128d7565b611c01906102208601356128a0565b611c0b91906128ee565b6020808301805192909252815160009101819052905143604090910152611115906101408501356128a0565b604051611c4b9060019083906020016127d6565b6040516020818303038152906040528051906020012060001c60005414611c7157600080fd5b60008055611c87604082013560a08301356128a0565b4310158015611c94575060015b611c9d57600080fd5b3415611ca857600080fd5b33611cb6602083018361216a565b6001600160a01b031614611cc957600080fd5b611cd6602082018261216a565b6001600160a01b03166108fc611cf1600060808501356128a0565b6040518115909202916000818181858888f19350505050158015611d19573d6000803e3d6000fd5b507f17040e3ed853a8df776cd092f1357f15488d98d460f66cd5e6b0cb07d5bc8ae38160405161036d9190612696565b60208101515160011415611e995760006002826020015160200151611d6e91906128ee565b1415611e0657611d7c612081565b8151516001600160a01b03908116825282516020908101518184015283516040908101518185015284516060908101519093168385015281850180518301516080860152805182015160a0860152519092015160c08401529051611de5916006918491016126f5565b60408051601f19818403018152919052805160209091012060005550611e94565b611e0e612081565b8151516001600160a01b03908116825282516020908101518184015283516040908101518185015284516060908101519093168385015281850180518301516080860152805182015160a0860152519092015160c08401529051611e77916006918491016126f5565b60408051601f198184030181529190528051602090910120600055505b611f04565b6040805160c081018252600091810182815260608083018481526080840185815260a085018681528486526020808701979097528751516001600160a01b03908116909552875187015190925286519092015190921690529183015151909152611f0281611f07565b505b50565b805160600151600214611f1f57805160400151611f23565b8051515b6001600160a01b03166108fc8260000151602001516002611f4491906128b8565b6040518115909202916000818181858888f19350505050158015611f6c573d6000803e3d6000fd5b506000805533ff5b60405180610120016040528060006001600160a01b03168152602001600081526020016000815260200160006001600160a01b031681526020016000815260200160008152602001611fc46120d0565b815260200160008152602001600081525090565b60405180610100016040528060006001600160a01b0316815260200160008152602001600081526020016000815260200160006001600160a01b03168152602001611fc46120d0565b6040805160c0810182526000918101828152606082018390526080820183905260a0820192909252908190815260200161207c6040518060800160405280600081526020016000815260200160008152602001600081525090565b905290565b6040518060e0016040528060006001600160a01b03168152602001600081526020016000815260200160006001600160a01b031681526020016000815260200160008152602001600081525090565b6040518060a001604052806005906020820280368337509192915050565b80356001600160a01b038116811461210557600080fd5b919050565b60006101a0828403121561211c578081fd5b50919050565b6000610100828403121561211c578081fd5b6000610120828403121561211c578081fd5b6000610260828403121561211c578081fd5b60006101c0828403121561211c578081fd5b60006020828403121561217b578081fd5b612184826120ee565b9392505050565b6000610240828403121561211c578081fd5b60006101a082840312156121af578081fd5b612184838361210a565b600061010082840312156121cb578081fd5b6121848383612122565b600061012082840312156121e7578081fd5b6121848383612134565b60006102608284031215612203578081fd5b6121848383612146565b60006101c0828403121561221f578081fd5b6121848383612158565b60006080828403121561211c578081fd5b6000610160828403121561211c578081fd5b600060e0828403121561211c578081fd5b8060005b6005811015612280578151845260209384019390910190600101612261565b50505050565b8035825260a0602082016020840137600060c08301525050565b6001600160a01b03806122b2836120ee565b1683526020820135602084015260408201356040840152806122d6606084016120ee565b166060840152506080810135608083015260a081013560a083015260c081013560c08301525050565b6001600160a01b0380612311836120ee565b168352602082013560208401526040820135604084015280612335606084016120ee565b166060840152506080810135608083015260a081013560a083015260c081013560c083015260e081013560e08301525050565b6001600160a01b03612379826120ee565b1682526020810135602083015260408101356040830152606081013560608301526080810135608083015260a081013560a08301525050565b6001600160a01b03806123c4836120ee565b1683526020820135602084015260408201356040840152806123e8606084016120ee565b166060840152506080810135608083015260a081013560a083015260a060c0820160c0840137610160818101359083015261018090810135910152565b60018060a01b0381511682526020810151602083015260408101516040830152606081015161245f60608401826001600160a01b03169052565b506080810151608083015260a081015160a083015260c081015161248660c084018261225d565b5060e0810151610160830152610100015161018090910152565b6124aa82826122ff565b61010060a0818301828501375060006101a08301525050565b6124cd82826122ff565b610100808201358015158082146124e357600080fd5b80838601525050505050565b6124f982826123b2565b6101a0808201358015158082146124e357600080fd5b6001600160a01b0380612521836120ee565b1683526020820135602084015260408201356040840152606082013560608401528061254f608084016120ee565b1660808401525060a080820160a0840137610140818101359083015261016090810135910152565b6102408101612586828461250f565b610180612597818401828601612286565b5092915050565b6101a081016125ad828461250f565b610180808401358015158082146125c357600080fd5b808386015250505092915050565b61010081016125e082846122a0565b60e092830135919092015290565b61010081016125fd82846122a0565b60e083013580151580821461261157600080fd5b8060e0850152505092915050565b6101a0810161262e82846124a0565b92915050565b610120810161262e82846124c3565b610260810161265282846123b2565b6101a0612597818401828601612286565b6101c0810161262e82846124ef565b61016081016126818284612368565b60a060c0840160c08401376000815292915050565b60e081016126a48284612368565b60c08301358015158082146126b857600080fd5b8060c0850152505092915050565b82815260c0810160a0836020840137600081529392505050565b828152610100810161218460208301846122a0565b60006101008201905083825260018060a01b03808451166020840152602084015160408401526040840151606084015280606085015116608084015250608083015160a083015260a083015160c083015260c083015160e08301529392505050565b828152610120810161218460208301846122ff565b8281526101208101612184602083018460018060a01b038082511683526020820151602084015260408201516040840152806060830151166060840152506080810151608083015260a081015160a083015260c081015160c083015260e081015160e08301525050565b82815260e081016121846020830184612368565b8281526101c0810161218460208301846123b2565b8281526101c081016121846020830184612425565b8281526101a08101612184602083018461250f565b60006101a08201905083825260018060a01b038084511660208401526020840151604084015260408401516060840152606084015160808401528060808501511660a08401525060a083015161288260c084018261225d565b5060c083015161016083015260e08301516101808301529392505050565b600082198211156128b3576128b361290e565b500190565b60008160001904831182151516156128d2576128d261290e565b500290565b6000828210156128e9576128e961290e565b500390565b60008261290957634e487b7160e01b81526012600452602481fd5b500690565b634e487b7160e01b600052601160045260246000fdfea26469706673582212204347d39dd6193e358fa1106d0a9b64f770d6766cd64231922ff4ca94cb01a38864736f6c63430008020033`,
  deployMode: `DM_constructor`
  };

export const _Connectors = {
  ALGO: _ALGO,
  ETH: _ETH
  };

