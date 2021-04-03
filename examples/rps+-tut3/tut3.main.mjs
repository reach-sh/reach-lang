// Automatically generated with Reach 0.1.2
/* eslint-disable */
export const _version = '0.1.2';


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
  
  
  const v39 = await ctc.creationTime();
  const v37 = stdlib.protect(ctc0, interact.DEADLINE, null);
  const v38 = stdlib.protect(ctc0, interact.wager, null);
  const v42 = stdlib.protect(ctc1, await interact.getBatch(), {
    at: './tut3/tut3.rsh:73:47:application',
    fs: ['at ./tut3/tut3.rsh:70:13:application call to [unknown function] (defined at: ./tut3/tut3.rsh:70:17:function exp)'],
    msg: 'getBatch',
    who: 'Alice'
     });
  const v44 = stdlib.protect(ctc0, await interact.random(), {
    at: 'reach standard library:60:31:application',
    fs: ['at ./tut3/tut3.rsh:74:74:application call to "makeCommitment" (defined at: reach standard library:59:8:function exp)', 'at ./tut3/tut3.rsh:70:13:application call to [unknown function] (defined at: ./tut3/tut3.rsh:70:17:function exp)'],
    msg: 'random',
    who: 'Alice'
     });
  const v45 = stdlib.digest(ctc2, [v44, v42]);
  const txn1 = await (ctc.sendrecv('Alice', 1, 3, stdlib.checkedBigNumberify('./tut3/tut3.rsh:78:9:dot', stdlib.UInt_max, 0), [ctc0, ctc0, ctc0, ctc3], [v39, v38, v37, v45], v38, [ctc0, ctc0, ctc3], true, true, false, (async (txn1) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.digest(ctc17, [stdlib.checkedBigNumberify('./tut3/tut3.rsh:78:9:dot', stdlib.UInt_max, 0), v39]);
    sim_r.prevSt_noPrevTime = stdlib.digest(ctc18, [stdlib.checkedBigNumberify('./tut3/tut3.rsh:78:9:dot', stdlib.UInt_max, 0)]);
    const [v48, v49, v50] = txn1.data;
    const v51 = txn1.value;
    const v55 = txn1.time;
    const v47 = txn1.from;
    
    const v52 = stdlib.eq(v51, v48);
    stdlib.assert(v52, {
      at: './tut3/tut3.rsh:78:9:dot',
      fs: [],
      msg: 'pay amount correct',
      who: 'Alice'
       });
    stdlib.assert(true, {
      at: './tut3/tut3.rsh:78:9:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Alice'
       });
    const v54 = stdlib.add(stdlib.checkedBigNumberify('./tut3/tut3.rsh:compileDApp', stdlib.UInt_max, 0), v51);
    sim_r.nextSt = stdlib.digest(ctc15, [stdlib.checkedBigNumberify('./tut3/tut3.rsh:80:15:after expr stmt semicolon', stdlib.UInt_max, 1), v47, v48, v49, v50, v54, v55]);
    sim_r.nextSt_noTime = stdlib.digest(ctc16, [stdlib.checkedBigNumberify('./tut3/tut3.rsh:80:15:after expr stmt semicolon', stdlib.UInt_max, 1), v47, v48, v49, v50, v54]);
    sim_r.isHalt = false;
    
    return sim_r;
     })));
  const [v48, v49, v50] = txn1.data;
  const v51 = txn1.value;
  const v55 = txn1.time;
  const v47 = txn1.from;
  const v52 = stdlib.eq(v51, v48);
  stdlib.assert(v52, {
    at: './tut3/tut3.rsh:78:9:dot',
    fs: [],
    msg: 'pay amount correct',
    who: 'Alice'
     });
  stdlib.assert(true, {
    at: './tut3/tut3.rsh:78:9:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Alice'
     });
  const v54 = stdlib.add(stdlib.checkedBigNumberify('./tut3/tut3.rsh:compileDApp', stdlib.UInt_max, 0), v51);
  const txn2 = await (ctc.recv('Alice', 2, 1, [ctc1], false, v49));
  if (txn2.didTimeout) {
    const txn3 = await (ctc.sendrecv('Alice', 3, 0, stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 5), [ctc6, ctc0, ctc0, ctc3, ctc0, ctc0], [v47, v48, v49, v50, v54, v55], stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), [], true, true, false, (async (txn3) => {
      const sim_r = { txns: [] };
      sim_r.prevSt = stdlib.digest(ctc15, [stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 1), v47, v48, v49, v50, v54, v55]);
      sim_r.prevSt_noPrevTime = stdlib.digest(ctc16, [stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 1), v47, v48, v49, v50, v54]);
      const [] = txn3.data;
      const v594 = txn3.value;
      const v599 = txn3.time;
      const v593 = txn3.from;
      
      const v595 = stdlib.eq(v594, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
      stdlib.assert(v595, {
        at: 'reach standard library:68:7:dot',
        fs: ['at ./tut3/tut3.rsh:89:41:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
        msg: 'pay amount correct',
        who: 'Alice'
         });
      const v596 = stdlib.addressEq(v47, v593);
      stdlib.assert(v596, {
        at: 'reach standard library:68:7:dot',
        fs: ['at ./tut3/tut3.rsh:89:41:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
        msg: 'sender correct',
        who: 'Alice'
         });
      const v598 = stdlib.add(v54, v594);
      sim_r.txns.push({
        amt: v598,
        to: v47
         });
      sim_r.nextSt = stdlib.digest(ctc5, []);
      sim_r.nextSt_noTime = stdlib.digest(ctc5, []);
      sim_r.isHalt = true;
      
      return sim_r;
       })));
    const [] = txn3.data;
    const v594 = txn3.value;
    const v599 = txn3.time;
    const v593 = txn3.from;
    const v595 = stdlib.eq(v594, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
    stdlib.assert(v595, {
      at: 'reach standard library:68:7:dot',
      fs: ['at ./tut3/tut3.rsh:89:41:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
      msg: 'pay amount correct',
      who: 'Alice'
       });
    const v596 = stdlib.addressEq(v47, v593);
    stdlib.assert(v596, {
      at: 'reach standard library:68:7:dot',
      fs: ['at ./tut3/tut3.rsh:89:41:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
      msg: 'sender correct',
      who: 'Alice'
       });
    const v598 = stdlib.add(v54, v594);
    ;
    stdlib.protect(ctc4, await interact.informTimeout(), {
      at: './tut3/tut3.rsh:68:33:application',
      fs: ['at ./tut3/tut3.rsh:67:13:application call to [unknown function] (defined at: ./tut3/tut3.rsh:67:25:function exp)', 'at reach standard library:71:8:application call to "after" (defined at: ./tut3/tut3.rsh:66:32:function exp)', 'at ./tut3/tut3.rsh:89:41:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
      msg: 'informTimeout',
      who: 'Alice'
       });
    return;
     }
  else {
    const [v60] = txn2.data;
    const v61 = txn2.value;
    const v65 = txn2.time;
    const v59 = txn2.from;
    const v62 = stdlib.eq(v61, v48);
    stdlib.assert(v62, {
      at: './tut3/tut3.rsh:87:9:dot',
      fs: [],
      msg: 'pay amount correct',
      who: 'Alice'
       });
    stdlib.assert(true, {
      at: './tut3/tut3.rsh:87:9:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Alice'
       });
    const v64 = stdlib.add(v54, v61);
    const txn3 = await (ctc.sendrecv('Alice', 4, 2, stdlib.checkedBigNumberify('./tut3/tut3.rsh:95:9:dot', stdlib.UInt_max, 7), [ctc6, ctc0, ctc0, ctc3, ctc6, ctc1, ctc0, ctc0, ctc0, ctc1], [v47, v48, v49, v50, v59, v60, v64, v65, v44, v42], stdlib.checkedBigNumberify('./tut3/tut3.rsh:decimal', stdlib.UInt_max, 0), [ctc0, ctc1], true, true, v49, (async (txn3) => {
      const sim_r = { txns: [] };
      sim_r.prevSt = stdlib.digest(ctc13, [stdlib.checkedBigNumberify('./tut3/tut3.rsh:95:9:dot', stdlib.UInt_max, 2), v47, v48, v49, v50, v59, v60, v64, v65]);
      sim_r.prevSt_noPrevTime = stdlib.digest(ctc14, [stdlib.checkedBigNumberify('./tut3/tut3.rsh:95:9:dot', stdlib.UInt_max, 2), v47, v48, v49, v50, v59, v60, v64]);
      const [v69, v70] = txn3.data;
      const v71 = txn3.value;
      const v76 = txn3.time;
      const v68 = txn3.from;
      
      const v72 = stdlib.eq(v71, stdlib.checkedBigNumberify('./tut3/tut3.rsh:decimal', stdlib.UInt_max, 0));
      stdlib.assert(v72, {
        at: './tut3/tut3.rsh:95:9:dot',
        fs: [],
        msg: 'pay amount correct',
        who: 'Alice'
         });
      const v73 = stdlib.addressEq(v47, v68);
      stdlib.assert(v73, {
        at: './tut3/tut3.rsh:95:9:dot',
        fs: [],
        msg: 'sender correct',
        who: 'Alice'
         });
      const v75 = stdlib.add(v64, v71);
      const v78 = stdlib.digest(ctc2, [v69, v70]);
      const v79 = stdlib.eq(v50, v78);
      stdlib.assert(v79, {
        at: 'reach standard library:65:17:application',
        fs: ['at ./tut3/tut3.rsh:97:22:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)'],
        msg: null,
        who: 'Alice'
         });
      const v96 = v70[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:18:array ref', stdlib.UInt_max, 0)];
      const v97 = v60[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:29:array ref', stdlib.UInt_max, 0)];
      const v99 = stdlib.sub(stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v97);
      const v100 = stdlib.add(v96, v99);
      const v101 = stdlib.mod(v100, stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v105 = v70[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:18:array ref', stdlib.UInt_max, 1)];
      const v106 = v60[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:29:array ref', stdlib.UInt_max, 1)];
      const v108 = stdlib.sub(stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v106);
      const v109 = stdlib.add(v105, v108);
      const v110 = stdlib.mod(v109, stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v114 = v70[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:18:array ref', stdlib.UInt_max, 2)];
      const v115 = v60[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:29:array ref', stdlib.UInt_max, 2)];
      const v117 = stdlib.sub(stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v115);
      const v118 = stdlib.add(v114, v117);
      const v119 = stdlib.mod(v118, stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v123 = v70[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:18:array ref', stdlib.UInt_max, 3)];
      const v124 = v60[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:29:array ref', stdlib.UInt_max, 3)];
      const v126 = stdlib.sub(stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v124);
      const v127 = stdlib.add(v123, v126);
      const v128 = stdlib.mod(v127, stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v132 = v70[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:18:array ref', stdlib.UInt_max, 4)];
      const v133 = v60[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:29:array ref', stdlib.UInt_max, 4)];
      const v135 = stdlib.sub(stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v133);
      const v136 = stdlib.add(v132, v135);
      const v137 = stdlib.mod(v136, stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v150 = stdlib.eq(v101, stdlib.checkedBigNumberify('./tut3/tut3.rsh:makeEnum', stdlib.UInt_max, 1));
      const v151 = v150 ? v110 : v101;
      const v154 = stdlib.eq(v151, stdlib.checkedBigNumberify('./tut3/tut3.rsh:makeEnum', stdlib.UInt_max, 1));
      const v155 = v154 ? v119 : v151;
      const v158 = stdlib.eq(v155, stdlib.checkedBigNumberify('./tut3/tut3.rsh:makeEnum', stdlib.UInt_max, 1));
      const v159 = v158 ? v128 : v155;
      const v162 = stdlib.eq(v159, stdlib.checkedBigNumberify('./tut3/tut3.rsh:makeEnum', stdlib.UInt_max, 1));
      const v163 = v162 ? v137 : v159;
      const v166 = v163;
      const v167 = stdlib.checkedBigNumberify('./tut3/tut3.rsh:99:70:decimal', stdlib.UInt_max, 0);
      const v612 = v75;
      const v613 = v76;
      
      if ((() => {
        const v177 = stdlib.eq(v166, stdlib.checkedBigNumberify('./tut3/tut3.rsh:makeEnum', stdlib.UInt_max, 1));
        
        return v177; })()) {
        const v178 = stdlib.mod(v167, stdlib.checkedBigNumberify('./tut3/tut3.rsh:128:21:decimal', stdlib.UInt_max, 2));
        const v179 = stdlib.eq(v178, stdlib.checkedBigNumberify('./tut3/tut3.rsh:128:26:decimal', stdlib.UInt_max, 0));
        if (v179) {
          sim_r.nextSt = stdlib.digest(ctc11, [stdlib.checkedBigNumberify('./tut3/tut3.rsh:103:19:after expr stmt semicolon', stdlib.UInt_max, 6), v47, v48, v49, v59, v167, v612, v613]);
          sim_r.nextSt_noTime = stdlib.digest(ctc12, [stdlib.checkedBigNumberify('./tut3/tut3.rsh:103:19:after expr stmt semicolon', stdlib.UInt_max, 6), v47, v48, v49, v59, v167, v612]);
          sim_r.isHalt = false;
           }
        else {
          sim_r.nextSt = stdlib.digest(ctc11, [stdlib.checkedBigNumberify('./tut3/tut3.rsh:103:19:after expr stmt semicolon', stdlib.UInt_max, 6), v47, v48, v49, v59, v167, v612, v613]);
          sim_r.nextSt_noTime = stdlib.digest(ctc12, [stdlib.checkedBigNumberify('./tut3/tut3.rsh:103:19:after expr stmt semicolon', stdlib.UInt_max, 6), v47, v48, v49, v59, v167, v612]);
          sim_r.isHalt = false;
           } }
      else {
        const v556 = stdlib.eq(v166, stdlib.checkedBigNumberify('./tut3/tut3.rsh:makeEnum', stdlib.UInt_max, 2));
        const v559 = stdlib.mul(stdlib.checkedBigNumberify('./tut3/tut3.rsh:140:16:decimal', stdlib.UInt_max, 2), v48);
        const v561 = v556 ? v47 : v59;
        sim_r.txns.push({
          amt: v559,
          to: v561
           });
        sim_r.nextSt = stdlib.digest(ctc5, []);
        sim_r.nextSt_noTime = stdlib.digest(ctc5, []);
        sim_r.isHalt = true;
         }
      return sim_r;
       })));
    if (txn3.didTimeout) {
      const txn4 = await (ctc.recv('Alice', 5, 0, [], false, false));
      const [] = txn4.data;
      const v574 = txn4.value;
      const v579 = txn4.time;
      const v573 = txn4.from;
      const v575 = stdlib.eq(v574, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
      stdlib.assert(v575, {
        at: 'reach standard library:68:7:dot',
        fs: ['at ./tut3/tut3.rsh:96:40:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
        msg: 'pay amount correct',
        who: 'Alice'
         });
      const v576 = stdlib.addressEq(v59, v573);
      stdlib.assert(v576, {
        at: 'reach standard library:68:7:dot',
        fs: ['at ./tut3/tut3.rsh:96:40:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
        msg: 'sender correct',
        who: 'Alice'
         });
      const v578 = stdlib.add(v64, v574);
      ;
      stdlib.protect(ctc4, await interact.informTimeout(), {
        at: './tut3/tut3.rsh:68:33:application',
        fs: ['at ./tut3/tut3.rsh:67:13:application call to [unknown function] (defined at: ./tut3/tut3.rsh:67:25:function exp)', 'at reach standard library:71:8:application call to "after" (defined at: ./tut3/tut3.rsh:66:32:function exp)', 'at ./tut3/tut3.rsh:96:40:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
        msg: 'informTimeout',
        who: 'Alice'
         });
      return;
       }
    else {
      const [v69, v70] = txn3.data;
      const v71 = txn3.value;
      const v76 = txn3.time;
      const v68 = txn3.from;
      const v72 = stdlib.eq(v71, stdlib.checkedBigNumberify('./tut3/tut3.rsh:decimal', stdlib.UInt_max, 0));
      stdlib.assert(v72, {
        at: './tut3/tut3.rsh:95:9:dot',
        fs: [],
        msg: 'pay amount correct',
        who: 'Alice'
         });
      const v73 = stdlib.addressEq(v47, v68);
      stdlib.assert(v73, {
        at: './tut3/tut3.rsh:95:9:dot',
        fs: [],
        msg: 'sender correct',
        who: 'Alice'
         });
      const v75 = stdlib.add(v64, v71);
      const v78 = stdlib.digest(ctc2, [v69, v70]);
      const v79 = stdlib.eq(v50, v78);
      stdlib.assert(v79, {
        at: 'reach standard library:65:17:application',
        fs: ['at ./tut3/tut3.rsh:97:22:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)'],
        msg: null,
        who: 'Alice'
         });
      const v96 = v70[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:18:array ref', stdlib.UInt_max, 0)];
      const v97 = v60[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:29:array ref', stdlib.UInt_max, 0)];
      const v99 = stdlib.sub(stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v97);
      const v100 = stdlib.add(v96, v99);
      const v101 = stdlib.mod(v100, stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v105 = v70[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:18:array ref', stdlib.UInt_max, 1)];
      const v106 = v60[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:29:array ref', stdlib.UInt_max, 1)];
      const v108 = stdlib.sub(stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v106);
      const v109 = stdlib.add(v105, v108);
      const v110 = stdlib.mod(v109, stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v114 = v70[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:18:array ref', stdlib.UInt_max, 2)];
      const v115 = v60[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:29:array ref', stdlib.UInt_max, 2)];
      const v117 = stdlib.sub(stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v115);
      const v118 = stdlib.add(v114, v117);
      const v119 = stdlib.mod(v118, stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v123 = v70[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:18:array ref', stdlib.UInt_max, 3)];
      const v124 = v60[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:29:array ref', stdlib.UInt_max, 3)];
      const v126 = stdlib.sub(stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v124);
      const v127 = stdlib.add(v123, v126);
      const v128 = stdlib.mod(v127, stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v132 = v70[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:18:array ref', stdlib.UInt_max, 4)];
      const v133 = v60[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:29:array ref', stdlib.UInt_max, 4)];
      const v135 = stdlib.sub(stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v133);
      const v136 = stdlib.add(v132, v135);
      const v137 = stdlib.mod(v136, stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v150 = stdlib.eq(v101, stdlib.checkedBigNumberify('./tut3/tut3.rsh:makeEnum', stdlib.UInt_max, 1));
      const v151 = v150 ? v110 : v101;
      const v154 = stdlib.eq(v151, stdlib.checkedBigNumberify('./tut3/tut3.rsh:makeEnum', stdlib.UInt_max, 1));
      const v155 = v154 ? v119 : v151;
      const v158 = stdlib.eq(v155, stdlib.checkedBigNumberify('./tut3/tut3.rsh:makeEnum', stdlib.UInt_max, 1));
      const v159 = v158 ? v128 : v155;
      const v162 = stdlib.eq(v159, stdlib.checkedBigNumberify('./tut3/tut3.rsh:makeEnum', stdlib.UInt_max, 1));
      const v163 = v162 ? v137 : v159;
      let v166 = v163;
      let v167 = stdlib.checkedBigNumberify('./tut3/tut3.rsh:99:70:decimal', stdlib.UInt_max, 0);
      let v612 = v75;
      let v613 = v76;
      
      while ((() => {
        const v177 = stdlib.eq(v166, stdlib.checkedBigNumberify('./tut3/tut3.rsh:makeEnum', stdlib.UInt_max, 1));
        
        return v177; })()) {
        const v178 = stdlib.mod(v167, stdlib.checkedBigNumberify('./tut3/tut3.rsh:128:21:decimal', stdlib.UInt_max, 2));
        const v179 = stdlib.eq(v178, stdlib.checkedBigNumberify('./tut3/tut3.rsh:128:26:decimal', stdlib.UInt_max, 0));
        if (v179) {
          const txn4 = await (ctc.recv('Alice', 8, 1, [ctc3], false, v49));
          if (txn4.didTimeout) {
            const txn5 = await (ctc.sendrecv('Alice', 9, 0, stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 6), [ctc6, ctc0, ctc0, ctc6, ctc0, ctc0, ctc0], [v47, v48, v49, v59, v167, v612, v613], stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), [], true, true, false, (async (txn5) => {
              const sim_r = { txns: [] };
              sim_r.prevSt = stdlib.digest(ctc11, [stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 6), v47, v48, v49, v59, v167, v612, v613]);
              sim_r.prevSt_noPrevTime = stdlib.digest(ctc12, [stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 6), v47, v48, v49, v59, v167, v612]);
              const [] = txn5.data;
              const v265 = txn5.value;
              const v270 = txn5.time;
              const v264 = txn5.from;
              
              const v266 = stdlib.eq(v265, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
              stdlib.assert(v266, {
                at: 'reach standard library:68:7:dot',
                fs: ['at ./tut3/tut3.rsh:110:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./tut3/tut3.rsh:129:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                msg: 'pay amount correct',
                who: 'Alice'
                 });
              const v267 = stdlib.addressEq(v47, v264);
              stdlib.assert(v267, {
                at: 'reach standard library:68:7:dot',
                fs: ['at ./tut3/tut3.rsh:110:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./tut3/tut3.rsh:129:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                msg: 'sender correct',
                who: 'Alice'
                 });
              const v269 = stdlib.add(v612, v265);
              sim_r.txns.push({
                amt: v269,
                to: v47
                 });
              sim_r.nextSt = stdlib.digest(ctc5, []);
              sim_r.nextSt_noTime = stdlib.digest(ctc5, []);
              sim_r.isHalt = true;
              
              return sim_r;
               })));
            const [] = txn5.data;
            const v265 = txn5.value;
            const v270 = txn5.time;
            const v264 = txn5.from;
            const v266 = stdlib.eq(v265, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v266, {
              at: 'reach standard library:68:7:dot',
              fs: ['at ./tut3/tut3.rsh:110:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./tut3/tut3.rsh:129:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
              msg: 'pay amount correct',
              who: 'Alice'
               });
            const v267 = stdlib.addressEq(v47, v264);
            stdlib.assert(v267, {
              at: 'reach standard library:68:7:dot',
              fs: ['at ./tut3/tut3.rsh:110:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./tut3/tut3.rsh:129:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
              msg: 'sender correct',
              who: 'Alice'
               });
            const v269 = stdlib.add(v612, v265);
            ;
            stdlib.protect(ctc4, await interact.informTimeout(), {
              at: './tut3/tut3.rsh:68:33:application',
              fs: ['at ./tut3/tut3.rsh:67:13:application call to [unknown function] (defined at: ./tut3/tut3.rsh:67:25:function exp)', 'at reach standard library:71:8:application call to "after" (defined at: ./tut3/tut3.rsh:66:32:function exp)', 'at ./tut3/tut3.rsh:110:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./tut3/tut3.rsh:129:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
              msg: 'informTimeout',
              who: 'Alice'
               });
            return;
             }
          else {
            const [v189] = txn4.data;
            const v190 = txn4.value;
            const v195 = txn4.time;
            const v188 = txn4.from;
            const v191 = stdlib.eq(v190, stdlib.checkedBigNumberify('./tut3/tut3.rsh:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v191, {
              at: './tut3/tut3.rsh:109:17:dot',
              fs: ['at ./tut3/tut3.rsh:129:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
              msg: 'pay amount correct',
              who: 'Alice'
               });
            const v192 = stdlib.addressEq(v59, v188);
            stdlib.assert(v192, {
              at: './tut3/tut3.rsh:109:17:dot',
              fs: ['at ./tut3/tut3.rsh:129:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
              msg: 'sender correct',
              who: 'Alice'
               });
            const v194 = stdlib.add(v612, v190);
            const v198 = stdlib.protect(ctc1, await interact.getBatch(), {
              at: './tut3/tut3.rsh:115:61:application',
              fs: ['at ./tut3/tut3.rsh:114:22:application call to [unknown function] (defined at: ./tut3/tut3.rsh:114:26:function exp)', 'at ./tut3/tut3.rsh:129:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
              msg: 'getBatch',
              who: 'Alice'
               });
            const txn5 = await (ctc.sendrecv('Alice', 10, 1, stdlib.checkedBigNumberify('./tut3/tut3.rsh:116:18:dot', stdlib.UInt_max, 7), [ctc6, ctc0, ctc0, ctc6, ctc0, ctc3, ctc0, ctc0, ctc1], [v47, v48, v49, v59, v167, v189, v194, v195, v198], stdlib.checkedBigNumberify('./tut3/tut3.rsh:decimal', stdlib.UInt_max, 0), [ctc1], true, true, v49, (async (txn5) => {
              const sim_r = { txns: [] };
              sim_r.prevSt = stdlib.digest(ctc9, [stdlib.checkedBigNumberify('./tut3/tut3.rsh:116:18:dot', stdlib.UInt_max, 8), v47, v48, v49, v59, v167, v189, v194, v195]);
              sim_r.prevSt_noPrevTime = stdlib.digest(ctc10, [stdlib.checkedBigNumberify('./tut3/tut3.rsh:116:18:dot', stdlib.UInt_max, 8), v47, v48, v49, v59, v167, v189, v194]);
              const [v200] = txn5.data;
              const v201 = txn5.value;
              const v206 = txn5.time;
              const v199 = txn5.from;
              
              const v202 = stdlib.eq(v201, stdlib.checkedBigNumberify('./tut3/tut3.rsh:decimal', stdlib.UInt_max, 0));
              stdlib.assert(v202, {
                at: './tut3/tut3.rsh:116:18:dot',
                fs: ['at ./tut3/tut3.rsh:129:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                msg: 'pay amount correct',
                who: 'Alice'
                 });
              const v203 = stdlib.addressEq(v47, v199);
              stdlib.assert(v203, {
                at: './tut3/tut3.rsh:116:18:dot',
                fs: ['at ./tut3/tut3.rsh:129:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                msg: 'sender correct',
                who: 'Alice'
                 });
              const v205 = stdlib.add(v194, v201);
              sim_r.nextSt = stdlib.digest(ctc7, [stdlib.checkedBigNumberify('./tut3/tut3.rsh:118:19:after expr stmt semicolon', stdlib.UInt_max, 10), v47, v48, v49, v59, v167, v189, v200, v205, v206]);
              sim_r.nextSt_noTime = stdlib.digest(ctc8, [stdlib.checkedBigNumberify('./tut3/tut3.rsh:118:19:after expr stmt semicolon', stdlib.UInt_max, 10), v47, v48, v49, v59, v167, v189, v200, v205]);
              sim_r.isHalt = false;
              
              return sim_r;
               })));
            if (txn5.didTimeout) {
              const txn6 = await (ctc.recv('Alice', 11, 0, [], false, false));
              const [] = txn6.data;
              const v245 = txn6.value;
              const v250 = txn6.time;
              const v244 = txn6.from;
              const v246 = stdlib.eq(v245, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
              stdlib.assert(v246, {
                at: 'reach standard library:68:7:dot',
                fs: ['at ./tut3/tut3.rsh:117:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./tut3/tut3.rsh:129:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                msg: 'pay amount correct',
                who: 'Alice'
                 });
              const v247 = stdlib.addressEq(v59, v244);
              stdlib.assert(v247, {
                at: 'reach standard library:68:7:dot',
                fs: ['at ./tut3/tut3.rsh:117:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./tut3/tut3.rsh:129:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                msg: 'sender correct',
                who: 'Alice'
                 });
              const v249 = stdlib.add(v194, v245);
              ;
              stdlib.protect(ctc4, await interact.informTimeout(), {
                at: './tut3/tut3.rsh:68:33:application',
                fs: ['at ./tut3/tut3.rsh:67:13:application call to [unknown function] (defined at: ./tut3/tut3.rsh:67:25:function exp)', 'at reach standard library:71:8:application call to "after" (defined at: ./tut3/tut3.rsh:66:32:function exp)', 'at ./tut3/tut3.rsh:117:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./tut3/tut3.rsh:129:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                msg: 'informTimeout',
                who: 'Alice'
                 });
              return;
               }
            else {
              const [v200] = txn5.data;
              const v201 = txn5.value;
              const v206 = txn5.time;
              const v199 = txn5.from;
              const v202 = stdlib.eq(v201, stdlib.checkedBigNumberify('./tut3/tut3.rsh:decimal', stdlib.UInt_max, 0));
              stdlib.assert(v202, {
                at: './tut3/tut3.rsh:116:18:dot',
                fs: ['at ./tut3/tut3.rsh:129:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                msg: 'pay amount correct',
                who: 'Alice'
                 });
              const v203 = stdlib.addressEq(v47, v199);
              stdlib.assert(v203, {
                at: './tut3/tut3.rsh:116:18:dot',
                fs: ['at ./tut3/tut3.rsh:129:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                msg: 'sender correct',
                who: 'Alice'
                 });
              const v205 = stdlib.add(v194, v201);
              const txn6 = await (ctc.recv('Alice', 12, 2, [ctc0, ctc1], false, v49));
              if (txn6.didTimeout) {
                const txn7 = await (ctc.sendrecv('Alice', 13, 0, stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 8), [ctc6, ctc0, ctc0, ctc6, ctc0, ctc3, ctc1, ctc0, ctc0], [v47, v48, v49, v59, v167, v189, v200, v205, v206], stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), [], true, true, false, (async (txn7) => {
                  const sim_r = { txns: [] };
                  sim_r.prevSt = stdlib.digest(ctc7, [stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 10), v47, v48, v49, v59, v167, v189, v200, v205, v206]);
                  sim_r.prevSt_noPrevTime = stdlib.digest(ctc8, [stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 10), v47, v48, v49, v59, v167, v189, v200, v205]);
                  const [] = txn7.data;
                  const v225 = txn7.value;
                  const v230 = txn7.time;
                  const v224 = txn7.from;
                  
                  const v226 = stdlib.eq(v225, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
                  stdlib.assert(v226, {
                    at: 'reach standard library:68:7:dot',
                    fs: ['at ./tut3/tut3.rsh:123:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./tut3/tut3.rsh:129:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                    msg: 'pay amount correct',
                    who: 'Alice'
                     });
                  const v227 = stdlib.addressEq(v47, v224);
                  stdlib.assert(v227, {
                    at: 'reach standard library:68:7:dot',
                    fs: ['at ./tut3/tut3.rsh:123:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./tut3/tut3.rsh:129:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                    msg: 'sender correct',
                    who: 'Alice'
                     });
                  const v229 = stdlib.add(v205, v225);
                  sim_r.txns.push({
                    amt: v229,
                    to: v47
                     });
                  sim_r.nextSt = stdlib.digest(ctc5, []);
                  sim_r.nextSt_noTime = stdlib.digest(ctc5, []);
                  sim_r.isHalt = true;
                  
                  return sim_r;
                   })));
                const [] = txn7.data;
                const v225 = txn7.value;
                const v230 = txn7.time;
                const v224 = txn7.from;
                const v226 = stdlib.eq(v225, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
                stdlib.assert(v226, {
                  at: 'reach standard library:68:7:dot',
                  fs: ['at ./tut3/tut3.rsh:123:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./tut3/tut3.rsh:129:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                  msg: 'pay amount correct',
                  who: 'Alice'
                   });
                const v227 = stdlib.addressEq(v47, v224);
                stdlib.assert(v227, {
                  at: 'reach standard library:68:7:dot',
                  fs: ['at ./tut3/tut3.rsh:123:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./tut3/tut3.rsh:129:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                  msg: 'sender correct',
                  who: 'Alice'
                   });
                const v229 = stdlib.add(v205, v225);
                ;
                stdlib.protect(ctc4, await interact.informTimeout(), {
                  at: './tut3/tut3.rsh:68:33:application',
                  fs: ['at ./tut3/tut3.rsh:67:13:application call to [unknown function] (defined at: ./tut3/tut3.rsh:67:25:function exp)', 'at reach standard library:71:8:application call to "after" (defined at: ./tut3/tut3.rsh:66:32:function exp)', 'at ./tut3/tut3.rsh:123:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./tut3/tut3.rsh:129:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                  msg: 'informTimeout',
                  who: 'Alice'
                   });
                return;
                 }
              else {
                const [v210, v211] = txn6.data;
                const v212 = txn6.value;
                const v217 = txn6.time;
                const v209 = txn6.from;
                const v213 = stdlib.eq(v212, stdlib.checkedBigNumberify('./tut3/tut3.rsh:decimal', stdlib.UInt_max, 0));
                stdlib.assert(v213, {
                  at: './tut3/tut3.rsh:122:17:dot',
                  fs: ['at ./tut3/tut3.rsh:129:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                  msg: 'pay amount correct',
                  who: 'Alice'
                   });
                const v214 = stdlib.addressEq(v59, v209);
                stdlib.assert(v214, {
                  at: './tut3/tut3.rsh:122:17:dot',
                  fs: ['at ./tut3/tut3.rsh:129:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                  msg: 'sender correct',
                  who: 'Alice'
                   });
                const v216 = stdlib.add(v205, v212);
                const v219 = stdlib.digest(ctc2, [v210, v211]);
                const v220 = stdlib.eq(v189, v219);
                stdlib.assert(v220, {
                  at: 'reach standard library:65:17:application',
                  fs: ['at ./tut3/tut3.rsh:124:26:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)', 'at ./tut3/tut3.rsh:129:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                  msg: null,
                  who: 'Alice'
                   });
                const v298 = v200[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:18:array ref', stdlib.UInt_max, 0)];
                const v299 = v211[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:29:array ref', stdlib.UInt_max, 0)];
                const v301 = stdlib.sub(stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v299);
                const v302 = stdlib.add(v298, v301);
                const v303 = stdlib.mod(v302, stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v307 = v200[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:18:array ref', stdlib.UInt_max, 1)];
                const v308 = v211[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:29:array ref', stdlib.UInt_max, 1)];
                const v310 = stdlib.sub(stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v308);
                const v311 = stdlib.add(v307, v310);
                const v312 = stdlib.mod(v311, stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v316 = v200[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:18:array ref', stdlib.UInt_max, 2)];
                const v317 = v211[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:29:array ref', stdlib.UInt_max, 2)];
                const v319 = stdlib.sub(stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v317);
                const v320 = stdlib.add(v316, v319);
                const v321 = stdlib.mod(v320, stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v325 = v200[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:18:array ref', stdlib.UInt_max, 3)];
                const v326 = v211[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:29:array ref', stdlib.UInt_max, 3)];
                const v328 = stdlib.sub(stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v326);
                const v329 = stdlib.add(v325, v328);
                const v330 = stdlib.mod(v329, stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v334 = v200[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:18:array ref', stdlib.UInt_max, 4)];
                const v335 = v211[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:29:array ref', stdlib.UInt_max, 4)];
                const v337 = stdlib.sub(stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v335);
                const v338 = stdlib.add(v334, v337);
                const v339 = stdlib.mod(v338, stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v352 = stdlib.eq(v303, stdlib.checkedBigNumberify('./tut3/tut3.rsh:makeEnum', stdlib.UInt_max, 1));
                const v353 = v352 ? v312 : v303;
                const v356 = stdlib.eq(v353, stdlib.checkedBigNumberify('./tut3/tut3.rsh:makeEnum', stdlib.UInt_max, 1));
                const v357 = v356 ? v321 : v353;
                const v360 = stdlib.eq(v357, stdlib.checkedBigNumberify('./tut3/tut3.rsh:makeEnum', stdlib.UInt_max, 1));
                const v361 = v360 ? v330 : v357;
                const v364 = stdlib.eq(v361, stdlib.checkedBigNumberify('./tut3/tut3.rsh:makeEnum', stdlib.UInt_max, 1));
                const v365 = v364 ? v339 : v361;
                const cv166 = v365;
                const cv167 = v167;
                const cv612 = v216;
                const cv613 = v217;
                
                v166 = cv166;
                v167 = cv167;
                v612 = cv612;
                v613 = cv613;
                
                continue; }
               }
             }
           }
        else {
          const txn4 = await (ctc.recv('Alice', 14, 1, [ctc3], false, v49));
          if (txn4.didTimeout) {
            const txn5 = await (ctc.sendrecv('Alice', 15, 0, stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 6), [ctc6, ctc0, ctc0, ctc6, ctc0, ctc0, ctc0], [v47, v48, v49, v59, v167, v612, v613], stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), [], true, true, false, (async (txn5) => {
              const sim_r = { txns: [] };
              sim_r.prevSt = stdlib.digest(ctc11, [stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 6), v47, v48, v49, v59, v167, v612, v613]);
              sim_r.prevSt_noPrevTime = stdlib.digest(ctc12, [stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 6), v47, v48, v49, v59, v167, v612]);
              const [] = txn5.data;
              const v453 = txn5.value;
              const v458 = txn5.time;
              const v452 = txn5.from;
              
              const v454 = stdlib.eq(v453, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
              stdlib.assert(v454, {
                at: 'reach standard library:68:7:dot',
                fs: ['at ./tut3/tut3.rsh:110:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./tut3/tut3.rsh:133:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                msg: 'pay amount correct',
                who: 'Alice'
                 });
              const v455 = stdlib.addressEq(v47, v452);
              stdlib.assert(v455, {
                at: 'reach standard library:68:7:dot',
                fs: ['at ./tut3/tut3.rsh:110:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./tut3/tut3.rsh:133:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                msg: 'sender correct',
                who: 'Alice'
                 });
              const v457 = stdlib.add(v612, v453);
              sim_r.txns.push({
                amt: v457,
                to: v47
                 });
              sim_r.nextSt = stdlib.digest(ctc5, []);
              sim_r.nextSt_noTime = stdlib.digest(ctc5, []);
              sim_r.isHalt = true;
              
              return sim_r;
               })));
            const [] = txn5.data;
            const v453 = txn5.value;
            const v458 = txn5.time;
            const v452 = txn5.from;
            const v454 = stdlib.eq(v453, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v454, {
              at: 'reach standard library:68:7:dot',
              fs: ['at ./tut3/tut3.rsh:110:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./tut3/tut3.rsh:133:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
              msg: 'pay amount correct',
              who: 'Alice'
               });
            const v455 = stdlib.addressEq(v47, v452);
            stdlib.assert(v455, {
              at: 'reach standard library:68:7:dot',
              fs: ['at ./tut3/tut3.rsh:110:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./tut3/tut3.rsh:133:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
              msg: 'sender correct',
              who: 'Alice'
               });
            const v457 = stdlib.add(v612, v453);
            ;
            stdlib.protect(ctc4, await interact.informTimeout(), {
              at: './tut3/tut3.rsh:68:33:application',
              fs: ['at ./tut3/tut3.rsh:67:13:application call to [unknown function] (defined at: ./tut3/tut3.rsh:67:25:function exp)', 'at reach standard library:71:8:application call to "after" (defined at: ./tut3/tut3.rsh:66:32:function exp)', 'at ./tut3/tut3.rsh:110:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./tut3/tut3.rsh:133:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
              msg: 'informTimeout',
              who: 'Alice'
               });
            return;
             }
          else {
            const [v377] = txn4.data;
            const v378 = txn4.value;
            const v383 = txn4.time;
            const v376 = txn4.from;
            const v379 = stdlib.eq(v378, stdlib.checkedBigNumberify('./tut3/tut3.rsh:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v379, {
              at: './tut3/tut3.rsh:109:17:dot',
              fs: ['at ./tut3/tut3.rsh:133:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
              msg: 'pay amount correct',
              who: 'Alice'
               });
            const v380 = stdlib.addressEq(v59, v376);
            stdlib.assert(v380, {
              at: './tut3/tut3.rsh:109:17:dot',
              fs: ['at ./tut3/tut3.rsh:133:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
              msg: 'sender correct',
              who: 'Alice'
               });
            const v382 = stdlib.add(v612, v378);
            const v386 = stdlib.protect(ctc1, await interact.getBatch(), {
              at: './tut3/tut3.rsh:115:61:application',
              fs: ['at ./tut3/tut3.rsh:114:22:application call to [unknown function] (defined at: ./tut3/tut3.rsh:114:26:function exp)', 'at ./tut3/tut3.rsh:133:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
              msg: 'getBatch',
              who: 'Alice'
               });
            const txn5 = await (ctc.sendrecv('Alice', 16, 1, stdlib.checkedBigNumberify('./tut3/tut3.rsh:116:18:dot', stdlib.UInt_max, 7), [ctc6, ctc0, ctc0, ctc6, ctc0, ctc3, ctc0, ctc0, ctc1], [v47, v48, v49, v59, v167, v377, v382, v383, v386], stdlib.checkedBigNumberify('./tut3/tut3.rsh:decimal', stdlib.UInt_max, 0), [ctc1], true, true, v49, (async (txn5) => {
              const sim_r = { txns: [] };
              sim_r.prevSt = stdlib.digest(ctc9, [stdlib.checkedBigNumberify('./tut3/tut3.rsh:116:18:dot', stdlib.UInt_max, 14), v47, v48, v49, v59, v167, v377, v382, v383]);
              sim_r.prevSt_noPrevTime = stdlib.digest(ctc10, [stdlib.checkedBigNumberify('./tut3/tut3.rsh:116:18:dot', stdlib.UInt_max, 14), v47, v48, v49, v59, v167, v377, v382]);
              const [v388] = txn5.data;
              const v389 = txn5.value;
              const v394 = txn5.time;
              const v387 = txn5.from;
              
              const v390 = stdlib.eq(v389, stdlib.checkedBigNumberify('./tut3/tut3.rsh:decimal', stdlib.UInt_max, 0));
              stdlib.assert(v390, {
                at: './tut3/tut3.rsh:116:18:dot',
                fs: ['at ./tut3/tut3.rsh:133:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                msg: 'pay amount correct',
                who: 'Alice'
                 });
              const v391 = stdlib.addressEq(v47, v387);
              stdlib.assert(v391, {
                at: './tut3/tut3.rsh:116:18:dot',
                fs: ['at ./tut3/tut3.rsh:133:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                msg: 'sender correct',
                who: 'Alice'
                 });
              const v393 = stdlib.add(v382, v389);
              sim_r.nextSt = stdlib.digest(ctc7, [stdlib.checkedBigNumberify('./tut3/tut3.rsh:118:19:after expr stmt semicolon', stdlib.UInt_max, 16), v47, v48, v49, v59, v167, v377, v388, v393, v394]);
              sim_r.nextSt_noTime = stdlib.digest(ctc8, [stdlib.checkedBigNumberify('./tut3/tut3.rsh:118:19:after expr stmt semicolon', stdlib.UInt_max, 16), v47, v48, v49, v59, v167, v377, v388, v393]);
              sim_r.isHalt = false;
              
              return sim_r;
               })));
            if (txn5.didTimeout) {
              const txn6 = await (ctc.recv('Alice', 17, 0, [], false, false));
              const [] = txn6.data;
              const v433 = txn6.value;
              const v438 = txn6.time;
              const v432 = txn6.from;
              const v434 = stdlib.eq(v433, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
              stdlib.assert(v434, {
                at: 'reach standard library:68:7:dot',
                fs: ['at ./tut3/tut3.rsh:117:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./tut3/tut3.rsh:133:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                msg: 'pay amount correct',
                who: 'Alice'
                 });
              const v435 = stdlib.addressEq(v59, v432);
              stdlib.assert(v435, {
                at: 'reach standard library:68:7:dot',
                fs: ['at ./tut3/tut3.rsh:117:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./tut3/tut3.rsh:133:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                msg: 'sender correct',
                who: 'Alice'
                 });
              const v437 = stdlib.add(v382, v433);
              ;
              stdlib.protect(ctc4, await interact.informTimeout(), {
                at: './tut3/tut3.rsh:68:33:application',
                fs: ['at ./tut3/tut3.rsh:67:13:application call to [unknown function] (defined at: ./tut3/tut3.rsh:67:25:function exp)', 'at reach standard library:71:8:application call to "after" (defined at: ./tut3/tut3.rsh:66:32:function exp)', 'at ./tut3/tut3.rsh:117:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./tut3/tut3.rsh:133:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                msg: 'informTimeout',
                who: 'Alice'
                 });
              return;
               }
            else {
              const [v388] = txn5.data;
              const v389 = txn5.value;
              const v394 = txn5.time;
              const v387 = txn5.from;
              const v390 = stdlib.eq(v389, stdlib.checkedBigNumberify('./tut3/tut3.rsh:decimal', stdlib.UInt_max, 0));
              stdlib.assert(v390, {
                at: './tut3/tut3.rsh:116:18:dot',
                fs: ['at ./tut3/tut3.rsh:133:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                msg: 'pay amount correct',
                who: 'Alice'
                 });
              const v391 = stdlib.addressEq(v47, v387);
              stdlib.assert(v391, {
                at: './tut3/tut3.rsh:116:18:dot',
                fs: ['at ./tut3/tut3.rsh:133:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                msg: 'sender correct',
                who: 'Alice'
                 });
              const v393 = stdlib.add(v382, v389);
              const txn6 = await (ctc.recv('Alice', 18, 2, [ctc0, ctc1], false, v49));
              if (txn6.didTimeout) {
                const txn7 = await (ctc.sendrecv('Alice', 19, 0, stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 8), [ctc6, ctc0, ctc0, ctc6, ctc0, ctc3, ctc1, ctc0, ctc0], [v47, v48, v49, v59, v167, v377, v388, v393, v394], stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), [], true, true, false, (async (txn7) => {
                  const sim_r = { txns: [] };
                  sim_r.prevSt = stdlib.digest(ctc7, [stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 16), v47, v48, v49, v59, v167, v377, v388, v393, v394]);
                  sim_r.prevSt_noPrevTime = stdlib.digest(ctc8, [stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 16), v47, v48, v49, v59, v167, v377, v388, v393]);
                  const [] = txn7.data;
                  const v413 = txn7.value;
                  const v418 = txn7.time;
                  const v412 = txn7.from;
                  
                  const v414 = stdlib.eq(v413, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
                  stdlib.assert(v414, {
                    at: 'reach standard library:68:7:dot',
                    fs: ['at ./tut3/tut3.rsh:123:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./tut3/tut3.rsh:133:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                    msg: 'pay amount correct',
                    who: 'Alice'
                     });
                  const v415 = stdlib.addressEq(v47, v412);
                  stdlib.assert(v415, {
                    at: 'reach standard library:68:7:dot',
                    fs: ['at ./tut3/tut3.rsh:123:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./tut3/tut3.rsh:133:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                    msg: 'sender correct',
                    who: 'Alice'
                     });
                  const v417 = stdlib.add(v393, v413);
                  sim_r.txns.push({
                    amt: v417,
                    to: v47
                     });
                  sim_r.nextSt = stdlib.digest(ctc5, []);
                  sim_r.nextSt_noTime = stdlib.digest(ctc5, []);
                  sim_r.isHalt = true;
                  
                  return sim_r;
                   })));
                const [] = txn7.data;
                const v413 = txn7.value;
                const v418 = txn7.time;
                const v412 = txn7.from;
                const v414 = stdlib.eq(v413, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
                stdlib.assert(v414, {
                  at: 'reach standard library:68:7:dot',
                  fs: ['at ./tut3/tut3.rsh:123:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./tut3/tut3.rsh:133:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                  msg: 'pay amount correct',
                  who: 'Alice'
                   });
                const v415 = stdlib.addressEq(v47, v412);
                stdlib.assert(v415, {
                  at: 'reach standard library:68:7:dot',
                  fs: ['at ./tut3/tut3.rsh:123:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./tut3/tut3.rsh:133:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                  msg: 'sender correct',
                  who: 'Alice'
                   });
                const v417 = stdlib.add(v393, v413);
                ;
                stdlib.protect(ctc4, await interact.informTimeout(), {
                  at: './tut3/tut3.rsh:68:33:application',
                  fs: ['at ./tut3/tut3.rsh:67:13:application call to [unknown function] (defined at: ./tut3/tut3.rsh:67:25:function exp)', 'at reach standard library:71:8:application call to "after" (defined at: ./tut3/tut3.rsh:66:32:function exp)', 'at ./tut3/tut3.rsh:123:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./tut3/tut3.rsh:133:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                  msg: 'informTimeout',
                  who: 'Alice'
                   });
                return;
                 }
              else {
                const [v398, v399] = txn6.data;
                const v400 = txn6.value;
                const v405 = txn6.time;
                const v397 = txn6.from;
                const v401 = stdlib.eq(v400, stdlib.checkedBigNumberify('./tut3/tut3.rsh:decimal', stdlib.UInt_max, 0));
                stdlib.assert(v401, {
                  at: './tut3/tut3.rsh:122:17:dot',
                  fs: ['at ./tut3/tut3.rsh:133:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                  msg: 'pay amount correct',
                  who: 'Alice'
                   });
                const v402 = stdlib.addressEq(v59, v397);
                stdlib.assert(v402, {
                  at: './tut3/tut3.rsh:122:17:dot',
                  fs: ['at ./tut3/tut3.rsh:133:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                  msg: 'sender correct',
                  who: 'Alice'
                   });
                const v404 = stdlib.add(v393, v400);
                const v407 = stdlib.digest(ctc2, [v398, v399]);
                const v408 = stdlib.eq(v377, v407);
                stdlib.assert(v408, {
                  at: 'reach standard library:65:17:application',
                  fs: ['at ./tut3/tut3.rsh:124:26:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)', 'at ./tut3/tut3.rsh:133:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                  msg: null,
                  who: 'Alice'
                   });
                const v486 = v399[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:18:array ref', stdlib.UInt_max, 0)];
                const v487 = v388[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:29:array ref', stdlib.UInt_max, 0)];
                const v489 = stdlib.sub(stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v487);
                const v490 = stdlib.add(v486, v489);
                const v491 = stdlib.mod(v490, stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v495 = v399[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:18:array ref', stdlib.UInt_max, 1)];
                const v496 = v388[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:29:array ref', stdlib.UInt_max, 1)];
                const v498 = stdlib.sub(stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v496);
                const v499 = stdlib.add(v495, v498);
                const v500 = stdlib.mod(v499, stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v504 = v399[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:18:array ref', stdlib.UInt_max, 2)];
                const v505 = v388[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:29:array ref', stdlib.UInt_max, 2)];
                const v507 = stdlib.sub(stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v505);
                const v508 = stdlib.add(v504, v507);
                const v509 = stdlib.mod(v508, stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v513 = v399[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:18:array ref', stdlib.UInt_max, 3)];
                const v514 = v388[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:29:array ref', stdlib.UInt_max, 3)];
                const v516 = stdlib.sub(stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v514);
                const v517 = stdlib.add(v513, v516);
                const v518 = stdlib.mod(v517, stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v522 = v399[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:18:array ref', stdlib.UInt_max, 4)];
                const v523 = v388[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:29:array ref', stdlib.UInt_max, 4)];
                const v525 = stdlib.sub(stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v523);
                const v526 = stdlib.add(v522, v525);
                const v527 = stdlib.mod(v526, stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v540 = stdlib.eq(v491, stdlib.checkedBigNumberify('./tut3/tut3.rsh:makeEnum', stdlib.UInt_max, 1));
                const v541 = v540 ? v500 : v491;
                const v544 = stdlib.eq(v541, stdlib.checkedBigNumberify('./tut3/tut3.rsh:makeEnum', stdlib.UInt_max, 1));
                const v545 = v544 ? v509 : v541;
                const v548 = stdlib.eq(v545, stdlib.checkedBigNumberify('./tut3/tut3.rsh:makeEnum', stdlib.UInt_max, 1));
                const v549 = v548 ? v518 : v545;
                const v552 = stdlib.eq(v549, stdlib.checkedBigNumberify('./tut3/tut3.rsh:makeEnum', stdlib.UInt_max, 1));
                const v553 = v552 ? v527 : v549;
                const cv166 = v553;
                const cv167 = v167;
                const cv612 = v404;
                const cv613 = v405;
                
                v166 = cv166;
                v167 = cv167;
                v612 = cv612;
                v613 = cv613;
                
                continue; }
               }
             }
           } }
      const v556 = stdlib.eq(v166, stdlib.checkedBigNumberify('./tut3/tut3.rsh:makeEnum', stdlib.UInt_max, 2));
      const v559 = stdlib.mul(stdlib.checkedBigNumberify('./tut3/tut3.rsh:140:16:decimal', stdlib.UInt_max, 2), v48);
      const v561 = v556 ? v47 : v59;
      ;
      stdlib.protect(ctc4, await interact.seeOutcome(v166), {
        at: './tut3/tut3.rsh:144:28:application',
        fs: ['at ./tut3/tut3.rsh:143:11:application call to [unknown function] (defined at: ./tut3/tut3.rsh:143:23:function exp)'],
        msg: 'seeOutcome',
        who: 'Alice'
         });
      return; }
     }
  
  
   }
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
  
  
  const v39 = await ctc.creationTime();
  const txn1 = await (ctc.recv('Bob', 1, 3, [ctc0, ctc0, ctc1], false, false));
  const [v48, v49, v50] = txn1.data;
  const v51 = txn1.value;
  const v55 = txn1.time;
  const v47 = txn1.from;
  const v52 = stdlib.eq(v51, v48);
  stdlib.assert(v52, {
    at: './tut3/tut3.rsh:78:9:dot',
    fs: [],
    msg: 'pay amount correct',
    who: 'Bob'
     });
  stdlib.assert(true, {
    at: './tut3/tut3.rsh:78:9:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Bob'
     });
  const v54 = stdlib.add(stdlib.checkedBigNumberify('./tut3/tut3.rsh:compileDApp', stdlib.UInt_max, 0), v51);
  stdlib.protect(ctc2, await interact.acceptWager(v48, v49), {
    at: './tut3/tut3.rsh:84:29:application',
    fs: ['at ./tut3/tut3.rsh:83:13:application call to [unknown function] (defined at: ./tut3/tut3.rsh:83:17:function exp)'],
    msg: 'acceptWager',
    who: 'Bob'
     });
  const v58 = stdlib.protect(ctc3, await interact.getBatch(), {
    at: './tut3/tut3.rsh:85:57:application',
    fs: ['at ./tut3/tut3.rsh:83:13:application call to [unknown function] (defined at: ./tut3/tut3.rsh:83:17:function exp)'],
    msg: 'getBatch',
    who: 'Bob'
     });
  const txn2 = await (ctc.sendrecv('Bob', 2, 1, stdlib.checkedBigNumberify('./tut3/tut3.rsh:87:9:dot', stdlib.UInt_max, 5), [ctc5, ctc0, ctc0, ctc1, ctc0, ctc0, ctc3], [v47, v48, v49, v50, v54, v55, v58], v48, [ctc3], true, true, v49, (async (txn2) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.digest(ctc15, [stdlib.checkedBigNumberify('./tut3/tut3.rsh:87:9:dot', stdlib.UInt_max, 1), v47, v48, v49, v50, v54, v55]);
    sim_r.prevSt_noPrevTime = stdlib.digest(ctc16, [stdlib.checkedBigNumberify('./tut3/tut3.rsh:87:9:dot', stdlib.UInt_max, 1), v47, v48, v49, v50, v54]);
    const [v60] = txn2.data;
    const v61 = txn2.value;
    const v65 = txn2.time;
    const v59 = txn2.from;
    
    const v62 = stdlib.eq(v61, v48);
    stdlib.assert(v62, {
      at: './tut3/tut3.rsh:87:9:dot',
      fs: [],
      msg: 'pay amount correct',
      who: 'Bob'
       });
    stdlib.assert(true, {
      at: './tut3/tut3.rsh:87:9:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Bob'
       });
    const v64 = stdlib.add(v54, v61);
    sim_r.nextSt = stdlib.digest(ctc13, [stdlib.checkedBigNumberify('./tut3/tut3.rsh:90:15:after expr stmt semicolon', stdlib.UInt_max, 2), v47, v48, v49, v50, v59, v60, v64, v65]);
    sim_r.nextSt_noTime = stdlib.digest(ctc14, [stdlib.checkedBigNumberify('./tut3/tut3.rsh:90:15:after expr stmt semicolon', stdlib.UInt_max, 2), v47, v48, v49, v50, v59, v60, v64]);
    sim_r.isHalt = false;
    
    return sim_r;
     })));
  if (txn2.didTimeout) {
    const txn3 = await (ctc.recv('Bob', 3, 0, [], false, false));
    const [] = txn3.data;
    const v594 = txn3.value;
    const v599 = txn3.time;
    const v593 = txn3.from;
    const v595 = stdlib.eq(v594, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
    stdlib.assert(v595, {
      at: 'reach standard library:68:7:dot',
      fs: ['at ./tut3/tut3.rsh:89:41:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
      msg: 'pay amount correct',
      who: 'Bob'
       });
    const v596 = stdlib.addressEq(v47, v593);
    stdlib.assert(v596, {
      at: 'reach standard library:68:7:dot',
      fs: ['at ./tut3/tut3.rsh:89:41:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
      msg: 'sender correct',
      who: 'Bob'
       });
    const v598 = stdlib.add(v54, v594);
    ;
    stdlib.protect(ctc2, await interact.informTimeout(), {
      at: './tut3/tut3.rsh:68:33:application',
      fs: ['at ./tut3/tut3.rsh:67:13:application call to [unknown function] (defined at: ./tut3/tut3.rsh:67:25:function exp)', 'at reach standard library:71:8:application call to "after" (defined at: ./tut3/tut3.rsh:66:32:function exp)', 'at ./tut3/tut3.rsh:89:41:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
      msg: 'informTimeout',
      who: 'Bob'
       });
    return;
     }
  else {
    const [v60] = txn2.data;
    const v61 = txn2.value;
    const v65 = txn2.time;
    const v59 = txn2.from;
    const v62 = stdlib.eq(v61, v48);
    stdlib.assert(v62, {
      at: './tut3/tut3.rsh:87:9:dot',
      fs: [],
      msg: 'pay amount correct',
      who: 'Bob'
       });
    stdlib.assert(true, {
      at: './tut3/tut3.rsh:87:9:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Bob'
       });
    const v64 = stdlib.add(v54, v61);
    const txn3 = await (ctc.recv('Bob', 4, 2, [ctc0, ctc3], false, v49));
    if (txn3.didTimeout) {
      const txn4 = await (ctc.sendrecv('Bob', 5, 0, stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 7), [ctc5, ctc0, ctc0, ctc1, ctc5, ctc3, ctc0, ctc0], [v47, v48, v49, v50, v59, v60, v64, v65], stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), [], true, true, false, (async (txn4) => {
        const sim_r = { txns: [] };
        sim_r.prevSt = stdlib.digest(ctc13, [stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 2), v47, v48, v49, v50, v59, v60, v64, v65]);
        sim_r.prevSt_noPrevTime = stdlib.digest(ctc14, [stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 2), v47, v48, v49, v50, v59, v60, v64]);
        const [] = txn4.data;
        const v574 = txn4.value;
        const v579 = txn4.time;
        const v573 = txn4.from;
        
        const v575 = stdlib.eq(v574, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
        stdlib.assert(v575, {
          at: 'reach standard library:68:7:dot',
          fs: ['at ./tut3/tut3.rsh:96:40:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
          msg: 'pay amount correct',
          who: 'Bob'
           });
        const v576 = stdlib.addressEq(v59, v573);
        stdlib.assert(v576, {
          at: 'reach standard library:68:7:dot',
          fs: ['at ./tut3/tut3.rsh:96:40:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
          msg: 'sender correct',
          who: 'Bob'
           });
        const v578 = stdlib.add(v64, v574);
        sim_r.txns.push({
          amt: v578,
          to: v59
           });
        sim_r.nextSt = stdlib.digest(ctc8, []);
        sim_r.nextSt_noTime = stdlib.digest(ctc8, []);
        sim_r.isHalt = true;
        
        return sim_r;
         })));
      const [] = txn4.data;
      const v574 = txn4.value;
      const v579 = txn4.time;
      const v573 = txn4.from;
      const v575 = stdlib.eq(v574, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
      stdlib.assert(v575, {
        at: 'reach standard library:68:7:dot',
        fs: ['at ./tut3/tut3.rsh:96:40:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
        msg: 'pay amount correct',
        who: 'Bob'
         });
      const v576 = stdlib.addressEq(v59, v573);
      stdlib.assert(v576, {
        at: 'reach standard library:68:7:dot',
        fs: ['at ./tut3/tut3.rsh:96:40:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
        msg: 'sender correct',
        who: 'Bob'
         });
      const v578 = stdlib.add(v64, v574);
      ;
      stdlib.protect(ctc2, await interact.informTimeout(), {
        at: './tut3/tut3.rsh:68:33:application',
        fs: ['at ./tut3/tut3.rsh:67:13:application call to [unknown function] (defined at: ./tut3/tut3.rsh:67:25:function exp)', 'at reach standard library:71:8:application call to "after" (defined at: ./tut3/tut3.rsh:66:32:function exp)', 'at ./tut3/tut3.rsh:96:40:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
        msg: 'informTimeout',
        who: 'Bob'
         });
      return;
       }
    else {
      const [v69, v70] = txn3.data;
      const v71 = txn3.value;
      const v76 = txn3.time;
      const v68 = txn3.from;
      const v72 = stdlib.eq(v71, stdlib.checkedBigNumberify('./tut3/tut3.rsh:decimal', stdlib.UInt_max, 0));
      stdlib.assert(v72, {
        at: './tut3/tut3.rsh:95:9:dot',
        fs: [],
        msg: 'pay amount correct',
        who: 'Bob'
         });
      const v73 = stdlib.addressEq(v47, v68);
      stdlib.assert(v73, {
        at: './tut3/tut3.rsh:95:9:dot',
        fs: [],
        msg: 'sender correct',
        who: 'Bob'
         });
      const v75 = stdlib.add(v64, v71);
      const v78 = stdlib.digest(ctc4, [v69, v70]);
      const v79 = stdlib.eq(v50, v78);
      stdlib.assert(v79, {
        at: 'reach standard library:65:17:application',
        fs: ['at ./tut3/tut3.rsh:97:22:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)'],
        msg: null,
        who: 'Bob'
         });
      const v96 = v70[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:18:array ref', stdlib.UInt_max, 0)];
      const v97 = v60[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:29:array ref', stdlib.UInt_max, 0)];
      const v99 = stdlib.sub(stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v97);
      const v100 = stdlib.add(v96, v99);
      const v101 = stdlib.mod(v100, stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v105 = v70[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:18:array ref', stdlib.UInt_max, 1)];
      const v106 = v60[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:29:array ref', stdlib.UInt_max, 1)];
      const v108 = stdlib.sub(stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v106);
      const v109 = stdlib.add(v105, v108);
      const v110 = stdlib.mod(v109, stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v114 = v70[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:18:array ref', stdlib.UInt_max, 2)];
      const v115 = v60[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:29:array ref', stdlib.UInt_max, 2)];
      const v117 = stdlib.sub(stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v115);
      const v118 = stdlib.add(v114, v117);
      const v119 = stdlib.mod(v118, stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v123 = v70[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:18:array ref', stdlib.UInt_max, 3)];
      const v124 = v60[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:29:array ref', stdlib.UInt_max, 3)];
      const v126 = stdlib.sub(stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v124);
      const v127 = stdlib.add(v123, v126);
      const v128 = stdlib.mod(v127, stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v132 = v70[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:18:array ref', stdlib.UInt_max, 4)];
      const v133 = v60[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:29:array ref', stdlib.UInt_max, 4)];
      const v135 = stdlib.sub(stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v133);
      const v136 = stdlib.add(v132, v135);
      const v137 = stdlib.mod(v136, stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v150 = stdlib.eq(v101, stdlib.checkedBigNumberify('./tut3/tut3.rsh:makeEnum', stdlib.UInt_max, 1));
      const v151 = v150 ? v110 : v101;
      const v154 = stdlib.eq(v151, stdlib.checkedBigNumberify('./tut3/tut3.rsh:makeEnum', stdlib.UInt_max, 1));
      const v155 = v154 ? v119 : v151;
      const v158 = stdlib.eq(v155, stdlib.checkedBigNumberify('./tut3/tut3.rsh:makeEnum', stdlib.UInt_max, 1));
      const v159 = v158 ? v128 : v155;
      const v162 = stdlib.eq(v159, stdlib.checkedBigNumberify('./tut3/tut3.rsh:makeEnum', stdlib.UInt_max, 1));
      const v163 = v162 ? v137 : v159;
      let v166 = v163;
      let v167 = stdlib.checkedBigNumberify('./tut3/tut3.rsh:99:70:decimal', stdlib.UInt_max, 0);
      let v612 = v75;
      let v613 = v76;
      
      while ((() => {
        const v177 = stdlib.eq(v166, stdlib.checkedBigNumberify('./tut3/tut3.rsh:makeEnum', stdlib.UInt_max, 1));
        
        return v177; })()) {
        const v178 = stdlib.mod(v167, stdlib.checkedBigNumberify('./tut3/tut3.rsh:128:21:decimal', stdlib.UInt_max, 2));
        const v179 = stdlib.eq(v178, stdlib.checkedBigNumberify('./tut3/tut3.rsh:128:26:decimal', stdlib.UInt_max, 0));
        if (v179) {
          const v183 = stdlib.protect(ctc3, await interact.getBatch(), {
            at: './tut3/tut3.rsh:106:50:application',
            fs: ['at ./tut3/tut3.rsh:105:21:application call to [unknown function] (defined at: ./tut3/tut3.rsh:105:25:function exp)', 'at ./tut3/tut3.rsh:129:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
            msg: 'getBatch',
            who: 'Bob'
             });
          const v185 = stdlib.protect(ctc0, await interact.random(), {
            at: 'reach standard library:60:31:application',
            fs: ['at ./tut3/tut3.rsh:107:62:application call to "makeCommitment" (defined at: reach standard library:59:8:function exp)', 'at ./tut3/tut3.rsh:105:21:application call to [unknown function] (defined at: ./tut3/tut3.rsh:105:25:function exp)', 'at ./tut3/tut3.rsh:129:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
            msg: 'random',
            who: 'Bob'
             });
          const v186 = stdlib.digest(ctc4, [v185, v183]);
          const txn4 = await (ctc.sendrecv('Bob', 8, 1, stdlib.checkedBigNumberify('./tut3/tut3.rsh:109:17:dot', stdlib.UInt_max, 6), [ctc5, ctc0, ctc0, ctc5, ctc0, ctc0, ctc0, ctc1], [v47, v48, v49, v59, v167, v612, v613, v186], stdlib.checkedBigNumberify('./tut3/tut3.rsh:decimal', stdlib.UInt_max, 0), [ctc1], true, true, v49, (async (txn4) => {
            const sim_r = { txns: [] };
            sim_r.prevSt = stdlib.digest(ctc6, [stdlib.checkedBigNumberify('./tut3/tut3.rsh:109:17:dot', stdlib.UInt_max, 6), v47, v48, v49, v59, v167, v612, v613]);
            sim_r.prevSt_noPrevTime = stdlib.digest(ctc7, [stdlib.checkedBigNumberify('./tut3/tut3.rsh:109:17:dot', stdlib.UInt_max, 6), v47, v48, v49, v59, v167, v612]);
            const [v189] = txn4.data;
            const v190 = txn4.value;
            const v195 = txn4.time;
            const v188 = txn4.from;
            
            const v191 = stdlib.eq(v190, stdlib.checkedBigNumberify('./tut3/tut3.rsh:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v191, {
              at: './tut3/tut3.rsh:109:17:dot',
              fs: ['at ./tut3/tut3.rsh:129:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
              msg: 'pay amount correct',
              who: 'Bob'
               });
            const v192 = stdlib.addressEq(v59, v188);
            stdlib.assert(v192, {
              at: './tut3/tut3.rsh:109:17:dot',
              fs: ['at ./tut3/tut3.rsh:129:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
              msg: 'sender correct',
              who: 'Bob'
               });
            const v194 = stdlib.add(v612, v190);
            sim_r.nextSt = stdlib.digest(ctc11, [stdlib.checkedBigNumberify('./tut3/tut3.rsh:111:19:after expr stmt semicolon', stdlib.UInt_max, 8), v47, v48, v49, v59, v167, v189, v194, v195]);
            sim_r.nextSt_noTime = stdlib.digest(ctc12, [stdlib.checkedBigNumberify('./tut3/tut3.rsh:111:19:after expr stmt semicolon', stdlib.UInt_max, 8), v47, v48, v49, v59, v167, v189, v194]);
            sim_r.isHalt = false;
            
            return sim_r;
             })));
          if (txn4.didTimeout) {
            const txn5 = await (ctc.recv('Bob', 9, 0, [], false, false));
            const [] = txn5.data;
            const v265 = txn5.value;
            const v270 = txn5.time;
            const v264 = txn5.from;
            const v266 = stdlib.eq(v265, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v266, {
              at: 'reach standard library:68:7:dot',
              fs: ['at ./tut3/tut3.rsh:110:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./tut3/tut3.rsh:129:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
              msg: 'pay amount correct',
              who: 'Bob'
               });
            const v267 = stdlib.addressEq(v47, v264);
            stdlib.assert(v267, {
              at: 'reach standard library:68:7:dot',
              fs: ['at ./tut3/tut3.rsh:110:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./tut3/tut3.rsh:129:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
              msg: 'sender correct',
              who: 'Bob'
               });
            const v269 = stdlib.add(v612, v265);
            ;
            stdlib.protect(ctc2, await interact.informTimeout(), {
              at: './tut3/tut3.rsh:68:33:application',
              fs: ['at ./tut3/tut3.rsh:67:13:application call to [unknown function] (defined at: ./tut3/tut3.rsh:67:25:function exp)', 'at reach standard library:71:8:application call to "after" (defined at: ./tut3/tut3.rsh:66:32:function exp)', 'at ./tut3/tut3.rsh:110:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./tut3/tut3.rsh:129:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
              msg: 'informTimeout',
              who: 'Bob'
               });
            return;
             }
          else {
            const [v189] = txn4.data;
            const v190 = txn4.value;
            const v195 = txn4.time;
            const v188 = txn4.from;
            const v191 = stdlib.eq(v190, stdlib.checkedBigNumberify('./tut3/tut3.rsh:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v191, {
              at: './tut3/tut3.rsh:109:17:dot',
              fs: ['at ./tut3/tut3.rsh:129:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
              msg: 'pay amount correct',
              who: 'Bob'
               });
            const v192 = stdlib.addressEq(v59, v188);
            stdlib.assert(v192, {
              at: './tut3/tut3.rsh:109:17:dot',
              fs: ['at ./tut3/tut3.rsh:129:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
              msg: 'sender correct',
              who: 'Bob'
               });
            const v194 = stdlib.add(v612, v190);
            const txn5 = await (ctc.recv('Bob', 10, 1, [ctc3], false, v49));
            if (txn5.didTimeout) {
              const txn6 = await (ctc.sendrecv('Bob', 11, 0, stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 7), [ctc5, ctc0, ctc0, ctc5, ctc0, ctc1, ctc0, ctc0], [v47, v48, v49, v59, v167, v189, v194, v195], stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), [], true, true, false, (async (txn6) => {
                const sim_r = { txns: [] };
                sim_r.prevSt = stdlib.digest(ctc11, [stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 8), v47, v48, v49, v59, v167, v189, v194, v195]);
                sim_r.prevSt_noPrevTime = stdlib.digest(ctc12, [stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 8), v47, v48, v49, v59, v167, v189, v194]);
                const [] = txn6.data;
                const v245 = txn6.value;
                const v250 = txn6.time;
                const v244 = txn6.from;
                
                const v246 = stdlib.eq(v245, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
                stdlib.assert(v246, {
                  at: 'reach standard library:68:7:dot',
                  fs: ['at ./tut3/tut3.rsh:117:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./tut3/tut3.rsh:129:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                  msg: 'pay amount correct',
                  who: 'Bob'
                   });
                const v247 = stdlib.addressEq(v59, v244);
                stdlib.assert(v247, {
                  at: 'reach standard library:68:7:dot',
                  fs: ['at ./tut3/tut3.rsh:117:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./tut3/tut3.rsh:129:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                  msg: 'sender correct',
                  who: 'Bob'
                   });
                const v249 = stdlib.add(v194, v245);
                sim_r.txns.push({
                  amt: v249,
                  to: v59
                   });
                sim_r.nextSt = stdlib.digest(ctc8, []);
                sim_r.nextSt_noTime = stdlib.digest(ctc8, []);
                sim_r.isHalt = true;
                
                return sim_r;
                 })));
              const [] = txn6.data;
              const v245 = txn6.value;
              const v250 = txn6.time;
              const v244 = txn6.from;
              const v246 = stdlib.eq(v245, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
              stdlib.assert(v246, {
                at: 'reach standard library:68:7:dot',
                fs: ['at ./tut3/tut3.rsh:117:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./tut3/tut3.rsh:129:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                msg: 'pay amount correct',
                who: 'Bob'
                 });
              const v247 = stdlib.addressEq(v59, v244);
              stdlib.assert(v247, {
                at: 'reach standard library:68:7:dot',
                fs: ['at ./tut3/tut3.rsh:117:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./tut3/tut3.rsh:129:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                msg: 'sender correct',
                who: 'Bob'
                 });
              const v249 = stdlib.add(v194, v245);
              ;
              stdlib.protect(ctc2, await interact.informTimeout(), {
                at: './tut3/tut3.rsh:68:33:application',
                fs: ['at ./tut3/tut3.rsh:67:13:application call to [unknown function] (defined at: ./tut3/tut3.rsh:67:25:function exp)', 'at reach standard library:71:8:application call to "after" (defined at: ./tut3/tut3.rsh:66:32:function exp)', 'at ./tut3/tut3.rsh:117:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./tut3/tut3.rsh:129:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                msg: 'informTimeout',
                who: 'Bob'
                 });
              return;
               }
            else {
              const [v200] = txn5.data;
              const v201 = txn5.value;
              const v206 = txn5.time;
              const v199 = txn5.from;
              const v202 = stdlib.eq(v201, stdlib.checkedBigNumberify('./tut3/tut3.rsh:decimal', stdlib.UInt_max, 0));
              stdlib.assert(v202, {
                at: './tut3/tut3.rsh:116:18:dot',
                fs: ['at ./tut3/tut3.rsh:129:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                msg: 'pay amount correct',
                who: 'Bob'
                 });
              const v203 = stdlib.addressEq(v47, v199);
              stdlib.assert(v203, {
                at: './tut3/tut3.rsh:116:18:dot',
                fs: ['at ./tut3/tut3.rsh:129:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                msg: 'sender correct',
                who: 'Bob'
                 });
              const v205 = stdlib.add(v194, v201);
              const txn6 = await (ctc.sendrecv('Bob', 12, 2, stdlib.checkedBigNumberify('./tut3/tut3.rsh:122:17:dot', stdlib.UInt_max, 8), [ctc5, ctc0, ctc0, ctc5, ctc0, ctc1, ctc3, ctc0, ctc0, ctc0, ctc3], [v47, v48, v49, v59, v167, v189, v200, v205, v206, v185, v183], stdlib.checkedBigNumberify('./tut3/tut3.rsh:decimal', stdlib.UInt_max, 0), [ctc0, ctc3], true, true, v49, (async (txn6) => {
                const sim_r = { txns: [] };
                sim_r.prevSt = stdlib.digest(ctc9, [stdlib.checkedBigNumberify('./tut3/tut3.rsh:122:17:dot', stdlib.UInt_max, 10), v47, v48, v49, v59, v167, v189, v200, v205, v206]);
                sim_r.prevSt_noPrevTime = stdlib.digest(ctc10, [stdlib.checkedBigNumberify('./tut3/tut3.rsh:122:17:dot', stdlib.UInt_max, 10), v47, v48, v49, v59, v167, v189, v200, v205]);
                const [v210, v211] = txn6.data;
                const v212 = txn6.value;
                const v217 = txn6.time;
                const v209 = txn6.from;
                
                const v213 = stdlib.eq(v212, stdlib.checkedBigNumberify('./tut3/tut3.rsh:decimal', stdlib.UInt_max, 0));
                stdlib.assert(v213, {
                  at: './tut3/tut3.rsh:122:17:dot',
                  fs: ['at ./tut3/tut3.rsh:129:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                  msg: 'pay amount correct',
                  who: 'Bob'
                   });
                const v214 = stdlib.addressEq(v59, v209);
                stdlib.assert(v214, {
                  at: './tut3/tut3.rsh:122:17:dot',
                  fs: ['at ./tut3/tut3.rsh:129:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                  msg: 'sender correct',
                  who: 'Bob'
                   });
                const v216 = stdlib.add(v205, v212);
                const v219 = stdlib.digest(ctc4, [v210, v211]);
                const v220 = stdlib.eq(v189, v219);
                stdlib.assert(v220, {
                  at: 'reach standard library:65:17:application',
                  fs: ['at ./tut3/tut3.rsh:124:26:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)', 'at ./tut3/tut3.rsh:129:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                  msg: null,
                  who: 'Bob'
                   });
                const v298 = v200[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:18:array ref', stdlib.UInt_max, 0)];
                const v299 = v211[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:29:array ref', stdlib.UInt_max, 0)];
                const v301 = stdlib.sub(stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v299);
                const v302 = stdlib.add(v298, v301);
                const v303 = stdlib.mod(v302, stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v307 = v200[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:18:array ref', stdlib.UInt_max, 1)];
                const v308 = v211[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:29:array ref', stdlib.UInt_max, 1)];
                const v310 = stdlib.sub(stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v308);
                const v311 = stdlib.add(v307, v310);
                const v312 = stdlib.mod(v311, stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v316 = v200[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:18:array ref', stdlib.UInt_max, 2)];
                const v317 = v211[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:29:array ref', stdlib.UInt_max, 2)];
                const v319 = stdlib.sub(stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v317);
                const v320 = stdlib.add(v316, v319);
                const v321 = stdlib.mod(v320, stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v325 = v200[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:18:array ref', stdlib.UInt_max, 3)];
                const v326 = v211[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:29:array ref', stdlib.UInt_max, 3)];
                const v328 = stdlib.sub(stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v326);
                const v329 = stdlib.add(v325, v328);
                const v330 = stdlib.mod(v329, stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v334 = v200[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:18:array ref', stdlib.UInt_max, 4)];
                const v335 = v211[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:29:array ref', stdlib.UInt_max, 4)];
                const v337 = stdlib.sub(stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v335);
                const v338 = stdlib.add(v334, v337);
                const v339 = stdlib.mod(v338, stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v352 = stdlib.eq(v303, stdlib.checkedBigNumberify('./tut3/tut3.rsh:makeEnum', stdlib.UInt_max, 1));
                const v353 = v352 ? v312 : v303;
                const v356 = stdlib.eq(v353, stdlib.checkedBigNumberify('./tut3/tut3.rsh:makeEnum', stdlib.UInt_max, 1));
                const v357 = v356 ? v321 : v353;
                const v360 = stdlib.eq(v357, stdlib.checkedBigNumberify('./tut3/tut3.rsh:makeEnum', stdlib.UInt_max, 1));
                const v361 = v360 ? v330 : v357;
                const v364 = stdlib.eq(v361, stdlib.checkedBigNumberify('./tut3/tut3.rsh:makeEnum', stdlib.UInt_max, 1));
                const v365 = v364 ? v339 : v361;
                const cv166 = v365;
                const cv167 = v167;
                const cv612 = v216;
                const cv613 = v217;
                
                (() => {
                  const v166 = cv166;
                  const v167 = cv167;
                  const v612 = cv612;
                  const v613 = cv613;
                  
                  if ((() => {
                    const v177 = stdlib.eq(v166, stdlib.checkedBigNumberify('./tut3/tut3.rsh:makeEnum', stdlib.UInt_max, 1));
                    
                    return v177; })()) {
                    const v178 = stdlib.mod(v167, stdlib.checkedBigNumberify('./tut3/tut3.rsh:128:21:decimal', stdlib.UInt_max, 2));
                    const v179 = stdlib.eq(v178, stdlib.checkedBigNumberify('./tut3/tut3.rsh:128:26:decimal', stdlib.UInt_max, 0));
                    if (v179) {
                      sim_r.nextSt = stdlib.digest(ctc6, [stdlib.checkedBigNumberify('./tut3/tut3.rsh:103:19:after expr stmt semicolon', stdlib.UInt_max, 6), v47, v48, v49, v59, v167, v612, v613]);
                      sim_r.nextSt_noTime = stdlib.digest(ctc7, [stdlib.checkedBigNumberify('./tut3/tut3.rsh:103:19:after expr stmt semicolon', stdlib.UInt_max, 6), v47, v48, v49, v59, v167, v612]);
                      sim_r.isHalt = false;
                       }
                    else {
                      sim_r.nextSt = stdlib.digest(ctc6, [stdlib.checkedBigNumberify('./tut3/tut3.rsh:103:19:after expr stmt semicolon', stdlib.UInt_max, 6), v47, v48, v49, v59, v167, v612, v613]);
                      sim_r.nextSt_noTime = stdlib.digest(ctc7, [stdlib.checkedBigNumberify('./tut3/tut3.rsh:103:19:after expr stmt semicolon', stdlib.UInt_max, 6), v47, v48, v49, v59, v167, v612]);
                      sim_r.isHalt = false;
                       } }
                  else {
                    const v556 = stdlib.eq(v166, stdlib.checkedBigNumberify('./tut3/tut3.rsh:makeEnum', stdlib.UInt_max, 2));
                    const v559 = stdlib.mul(stdlib.checkedBigNumberify('./tut3/tut3.rsh:140:16:decimal', stdlib.UInt_max, 2), v48);
                    const v561 = v556 ? v47 : v59;
                    sim_r.txns.push({
                      amt: v559,
                      to: v561
                       });
                    sim_r.nextSt = stdlib.digest(ctc8, []);
                    sim_r.nextSt_noTime = stdlib.digest(ctc8, []);
                    sim_r.isHalt = true;
                     } })();
                return sim_r;
                 })));
              if (txn6.didTimeout) {
                const txn7 = await (ctc.recv('Bob', 13, 0, [], false, false));
                const [] = txn7.data;
                const v225 = txn7.value;
                const v230 = txn7.time;
                const v224 = txn7.from;
                const v226 = stdlib.eq(v225, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
                stdlib.assert(v226, {
                  at: 'reach standard library:68:7:dot',
                  fs: ['at ./tut3/tut3.rsh:123:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./tut3/tut3.rsh:129:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                  msg: 'pay amount correct',
                  who: 'Bob'
                   });
                const v227 = stdlib.addressEq(v47, v224);
                stdlib.assert(v227, {
                  at: 'reach standard library:68:7:dot',
                  fs: ['at ./tut3/tut3.rsh:123:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./tut3/tut3.rsh:129:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                  msg: 'sender correct',
                  who: 'Bob'
                   });
                const v229 = stdlib.add(v205, v225);
                ;
                stdlib.protect(ctc2, await interact.informTimeout(), {
                  at: './tut3/tut3.rsh:68:33:application',
                  fs: ['at ./tut3/tut3.rsh:67:13:application call to [unknown function] (defined at: ./tut3/tut3.rsh:67:25:function exp)', 'at reach standard library:71:8:application call to "after" (defined at: ./tut3/tut3.rsh:66:32:function exp)', 'at ./tut3/tut3.rsh:123:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./tut3/tut3.rsh:129:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                  msg: 'informTimeout',
                  who: 'Bob'
                   });
                return;
                 }
              else {
                const [v210, v211] = txn6.data;
                const v212 = txn6.value;
                const v217 = txn6.time;
                const v209 = txn6.from;
                const v213 = stdlib.eq(v212, stdlib.checkedBigNumberify('./tut3/tut3.rsh:decimal', stdlib.UInt_max, 0));
                stdlib.assert(v213, {
                  at: './tut3/tut3.rsh:122:17:dot',
                  fs: ['at ./tut3/tut3.rsh:129:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                  msg: 'pay amount correct',
                  who: 'Bob'
                   });
                const v214 = stdlib.addressEq(v59, v209);
                stdlib.assert(v214, {
                  at: './tut3/tut3.rsh:122:17:dot',
                  fs: ['at ./tut3/tut3.rsh:129:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                  msg: 'sender correct',
                  who: 'Bob'
                   });
                const v216 = stdlib.add(v205, v212);
                const v219 = stdlib.digest(ctc4, [v210, v211]);
                const v220 = stdlib.eq(v189, v219);
                stdlib.assert(v220, {
                  at: 'reach standard library:65:17:application',
                  fs: ['at ./tut3/tut3.rsh:124:26:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)', 'at ./tut3/tut3.rsh:129:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                  msg: null,
                  who: 'Bob'
                   });
                const v298 = v200[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:18:array ref', stdlib.UInt_max, 0)];
                const v299 = v211[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:29:array ref', stdlib.UInt_max, 0)];
                const v301 = stdlib.sub(stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v299);
                const v302 = stdlib.add(v298, v301);
                const v303 = stdlib.mod(v302, stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v307 = v200[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:18:array ref', stdlib.UInt_max, 1)];
                const v308 = v211[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:29:array ref', stdlib.UInt_max, 1)];
                const v310 = stdlib.sub(stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v308);
                const v311 = stdlib.add(v307, v310);
                const v312 = stdlib.mod(v311, stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v316 = v200[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:18:array ref', stdlib.UInt_max, 2)];
                const v317 = v211[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:29:array ref', stdlib.UInt_max, 2)];
                const v319 = stdlib.sub(stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v317);
                const v320 = stdlib.add(v316, v319);
                const v321 = stdlib.mod(v320, stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v325 = v200[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:18:array ref', stdlib.UInt_max, 3)];
                const v326 = v211[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:29:array ref', stdlib.UInt_max, 3)];
                const v328 = stdlib.sub(stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v326);
                const v329 = stdlib.add(v325, v328);
                const v330 = stdlib.mod(v329, stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v334 = v200[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:18:array ref', stdlib.UInt_max, 4)];
                const v335 = v211[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:29:array ref', stdlib.UInt_max, 4)];
                const v337 = stdlib.sub(stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v335);
                const v338 = stdlib.add(v334, v337);
                const v339 = stdlib.mod(v338, stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v352 = stdlib.eq(v303, stdlib.checkedBigNumberify('./tut3/tut3.rsh:makeEnum', stdlib.UInt_max, 1));
                const v353 = v352 ? v312 : v303;
                const v356 = stdlib.eq(v353, stdlib.checkedBigNumberify('./tut3/tut3.rsh:makeEnum', stdlib.UInt_max, 1));
                const v357 = v356 ? v321 : v353;
                const v360 = stdlib.eq(v357, stdlib.checkedBigNumberify('./tut3/tut3.rsh:makeEnum', stdlib.UInt_max, 1));
                const v361 = v360 ? v330 : v357;
                const v364 = stdlib.eq(v361, stdlib.checkedBigNumberify('./tut3/tut3.rsh:makeEnum', stdlib.UInt_max, 1));
                const v365 = v364 ? v339 : v361;
                const cv166 = v365;
                const cv167 = v167;
                const cv612 = v216;
                const cv613 = v217;
                
                v166 = cv166;
                v167 = cv167;
                v612 = cv612;
                v613 = cv613;
                
                continue; }
               }
             }
           }
        else {
          const v371 = stdlib.protect(ctc3, await interact.getBatch(), {
            at: './tut3/tut3.rsh:106:50:application',
            fs: ['at ./tut3/tut3.rsh:105:21:application call to [unknown function] (defined at: ./tut3/tut3.rsh:105:25:function exp)', 'at ./tut3/tut3.rsh:133:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
            msg: 'getBatch',
            who: 'Bob'
             });
          const v373 = stdlib.protect(ctc0, await interact.random(), {
            at: 'reach standard library:60:31:application',
            fs: ['at ./tut3/tut3.rsh:107:62:application call to "makeCommitment" (defined at: reach standard library:59:8:function exp)', 'at ./tut3/tut3.rsh:105:21:application call to [unknown function] (defined at: ./tut3/tut3.rsh:105:25:function exp)', 'at ./tut3/tut3.rsh:133:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
            msg: 'random',
            who: 'Bob'
             });
          const v374 = stdlib.digest(ctc4, [v373, v371]);
          const txn4 = await (ctc.sendrecv('Bob', 14, 1, stdlib.checkedBigNumberify('./tut3/tut3.rsh:109:17:dot', stdlib.UInt_max, 6), [ctc5, ctc0, ctc0, ctc5, ctc0, ctc0, ctc0, ctc1], [v47, v48, v49, v59, v167, v612, v613, v374], stdlib.checkedBigNumberify('./tut3/tut3.rsh:decimal', stdlib.UInt_max, 0), [ctc1], true, true, v49, (async (txn4) => {
            const sim_r = { txns: [] };
            sim_r.prevSt = stdlib.digest(ctc6, [stdlib.checkedBigNumberify('./tut3/tut3.rsh:109:17:dot', stdlib.UInt_max, 6), v47, v48, v49, v59, v167, v612, v613]);
            sim_r.prevSt_noPrevTime = stdlib.digest(ctc7, [stdlib.checkedBigNumberify('./tut3/tut3.rsh:109:17:dot', stdlib.UInt_max, 6), v47, v48, v49, v59, v167, v612]);
            const [v377] = txn4.data;
            const v378 = txn4.value;
            const v383 = txn4.time;
            const v376 = txn4.from;
            
            const v379 = stdlib.eq(v378, stdlib.checkedBigNumberify('./tut3/tut3.rsh:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v379, {
              at: './tut3/tut3.rsh:109:17:dot',
              fs: ['at ./tut3/tut3.rsh:133:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
              msg: 'pay amount correct',
              who: 'Bob'
               });
            const v380 = stdlib.addressEq(v59, v376);
            stdlib.assert(v380, {
              at: './tut3/tut3.rsh:109:17:dot',
              fs: ['at ./tut3/tut3.rsh:133:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
              msg: 'sender correct',
              who: 'Bob'
               });
            const v382 = stdlib.add(v612, v378);
            sim_r.nextSt = stdlib.digest(ctc11, [stdlib.checkedBigNumberify('./tut3/tut3.rsh:111:19:after expr stmt semicolon', stdlib.UInt_max, 14), v47, v48, v49, v59, v167, v377, v382, v383]);
            sim_r.nextSt_noTime = stdlib.digest(ctc12, [stdlib.checkedBigNumberify('./tut3/tut3.rsh:111:19:after expr stmt semicolon', stdlib.UInt_max, 14), v47, v48, v49, v59, v167, v377, v382]);
            sim_r.isHalt = false;
            
            return sim_r;
             })));
          if (txn4.didTimeout) {
            const txn5 = await (ctc.recv('Bob', 15, 0, [], false, false));
            const [] = txn5.data;
            const v453 = txn5.value;
            const v458 = txn5.time;
            const v452 = txn5.from;
            const v454 = stdlib.eq(v453, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v454, {
              at: 'reach standard library:68:7:dot',
              fs: ['at ./tut3/tut3.rsh:110:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./tut3/tut3.rsh:133:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
              msg: 'pay amount correct',
              who: 'Bob'
               });
            const v455 = stdlib.addressEq(v47, v452);
            stdlib.assert(v455, {
              at: 'reach standard library:68:7:dot',
              fs: ['at ./tut3/tut3.rsh:110:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./tut3/tut3.rsh:133:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
              msg: 'sender correct',
              who: 'Bob'
               });
            const v457 = stdlib.add(v612, v453);
            ;
            stdlib.protect(ctc2, await interact.informTimeout(), {
              at: './tut3/tut3.rsh:68:33:application',
              fs: ['at ./tut3/tut3.rsh:67:13:application call to [unknown function] (defined at: ./tut3/tut3.rsh:67:25:function exp)', 'at reach standard library:71:8:application call to "after" (defined at: ./tut3/tut3.rsh:66:32:function exp)', 'at ./tut3/tut3.rsh:110:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./tut3/tut3.rsh:133:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
              msg: 'informTimeout',
              who: 'Bob'
               });
            return;
             }
          else {
            const [v377] = txn4.data;
            const v378 = txn4.value;
            const v383 = txn4.time;
            const v376 = txn4.from;
            const v379 = stdlib.eq(v378, stdlib.checkedBigNumberify('./tut3/tut3.rsh:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v379, {
              at: './tut3/tut3.rsh:109:17:dot',
              fs: ['at ./tut3/tut3.rsh:133:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
              msg: 'pay amount correct',
              who: 'Bob'
               });
            const v380 = stdlib.addressEq(v59, v376);
            stdlib.assert(v380, {
              at: './tut3/tut3.rsh:109:17:dot',
              fs: ['at ./tut3/tut3.rsh:133:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
              msg: 'sender correct',
              who: 'Bob'
               });
            const v382 = stdlib.add(v612, v378);
            const txn5 = await (ctc.recv('Bob', 16, 1, [ctc3], false, v49));
            if (txn5.didTimeout) {
              const txn6 = await (ctc.sendrecv('Bob', 17, 0, stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 7), [ctc5, ctc0, ctc0, ctc5, ctc0, ctc1, ctc0, ctc0], [v47, v48, v49, v59, v167, v377, v382, v383], stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), [], true, true, false, (async (txn6) => {
                const sim_r = { txns: [] };
                sim_r.prevSt = stdlib.digest(ctc11, [stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 14), v47, v48, v49, v59, v167, v377, v382, v383]);
                sim_r.prevSt_noPrevTime = stdlib.digest(ctc12, [stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 14), v47, v48, v49, v59, v167, v377, v382]);
                const [] = txn6.data;
                const v433 = txn6.value;
                const v438 = txn6.time;
                const v432 = txn6.from;
                
                const v434 = stdlib.eq(v433, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
                stdlib.assert(v434, {
                  at: 'reach standard library:68:7:dot',
                  fs: ['at ./tut3/tut3.rsh:117:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./tut3/tut3.rsh:133:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                  msg: 'pay amount correct',
                  who: 'Bob'
                   });
                const v435 = stdlib.addressEq(v59, v432);
                stdlib.assert(v435, {
                  at: 'reach standard library:68:7:dot',
                  fs: ['at ./tut3/tut3.rsh:117:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./tut3/tut3.rsh:133:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                  msg: 'sender correct',
                  who: 'Bob'
                   });
                const v437 = stdlib.add(v382, v433);
                sim_r.txns.push({
                  amt: v437,
                  to: v59
                   });
                sim_r.nextSt = stdlib.digest(ctc8, []);
                sim_r.nextSt_noTime = stdlib.digest(ctc8, []);
                sim_r.isHalt = true;
                
                return sim_r;
                 })));
              const [] = txn6.data;
              const v433 = txn6.value;
              const v438 = txn6.time;
              const v432 = txn6.from;
              const v434 = stdlib.eq(v433, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
              stdlib.assert(v434, {
                at: 'reach standard library:68:7:dot',
                fs: ['at ./tut3/tut3.rsh:117:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./tut3/tut3.rsh:133:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                msg: 'pay amount correct',
                who: 'Bob'
                 });
              const v435 = stdlib.addressEq(v59, v432);
              stdlib.assert(v435, {
                at: 'reach standard library:68:7:dot',
                fs: ['at ./tut3/tut3.rsh:117:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./tut3/tut3.rsh:133:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                msg: 'sender correct',
                who: 'Bob'
                 });
              const v437 = stdlib.add(v382, v433);
              ;
              stdlib.protect(ctc2, await interact.informTimeout(), {
                at: './tut3/tut3.rsh:68:33:application',
                fs: ['at ./tut3/tut3.rsh:67:13:application call to [unknown function] (defined at: ./tut3/tut3.rsh:67:25:function exp)', 'at reach standard library:71:8:application call to "after" (defined at: ./tut3/tut3.rsh:66:32:function exp)', 'at ./tut3/tut3.rsh:117:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./tut3/tut3.rsh:133:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                msg: 'informTimeout',
                who: 'Bob'
                 });
              return;
               }
            else {
              const [v388] = txn5.data;
              const v389 = txn5.value;
              const v394 = txn5.time;
              const v387 = txn5.from;
              const v390 = stdlib.eq(v389, stdlib.checkedBigNumberify('./tut3/tut3.rsh:decimal', stdlib.UInt_max, 0));
              stdlib.assert(v390, {
                at: './tut3/tut3.rsh:116:18:dot',
                fs: ['at ./tut3/tut3.rsh:133:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                msg: 'pay amount correct',
                who: 'Bob'
                 });
              const v391 = stdlib.addressEq(v47, v387);
              stdlib.assert(v391, {
                at: './tut3/tut3.rsh:116:18:dot',
                fs: ['at ./tut3/tut3.rsh:133:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                msg: 'sender correct',
                who: 'Bob'
                 });
              const v393 = stdlib.add(v382, v389);
              const txn6 = await (ctc.sendrecv('Bob', 18, 2, stdlib.checkedBigNumberify('./tut3/tut3.rsh:122:17:dot', stdlib.UInt_max, 8), [ctc5, ctc0, ctc0, ctc5, ctc0, ctc1, ctc3, ctc0, ctc0, ctc0, ctc3], [v47, v48, v49, v59, v167, v377, v388, v393, v394, v373, v371], stdlib.checkedBigNumberify('./tut3/tut3.rsh:decimal', stdlib.UInt_max, 0), [ctc0, ctc3], true, true, v49, (async (txn6) => {
                const sim_r = { txns: [] };
                sim_r.prevSt = stdlib.digest(ctc9, [stdlib.checkedBigNumberify('./tut3/tut3.rsh:122:17:dot', stdlib.UInt_max, 16), v47, v48, v49, v59, v167, v377, v388, v393, v394]);
                sim_r.prevSt_noPrevTime = stdlib.digest(ctc10, [stdlib.checkedBigNumberify('./tut3/tut3.rsh:122:17:dot', stdlib.UInt_max, 16), v47, v48, v49, v59, v167, v377, v388, v393]);
                const [v398, v399] = txn6.data;
                const v400 = txn6.value;
                const v405 = txn6.time;
                const v397 = txn6.from;
                
                const v401 = stdlib.eq(v400, stdlib.checkedBigNumberify('./tut3/tut3.rsh:decimal', stdlib.UInt_max, 0));
                stdlib.assert(v401, {
                  at: './tut3/tut3.rsh:122:17:dot',
                  fs: ['at ./tut3/tut3.rsh:133:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                  msg: 'pay amount correct',
                  who: 'Bob'
                   });
                const v402 = stdlib.addressEq(v59, v397);
                stdlib.assert(v402, {
                  at: './tut3/tut3.rsh:122:17:dot',
                  fs: ['at ./tut3/tut3.rsh:133:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                  msg: 'sender correct',
                  who: 'Bob'
                   });
                const v404 = stdlib.add(v393, v400);
                const v407 = stdlib.digest(ctc4, [v398, v399]);
                const v408 = stdlib.eq(v377, v407);
                stdlib.assert(v408, {
                  at: 'reach standard library:65:17:application',
                  fs: ['at ./tut3/tut3.rsh:124:26:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)', 'at ./tut3/tut3.rsh:133:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                  msg: null,
                  who: 'Bob'
                   });
                const v486 = v399[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:18:array ref', stdlib.UInt_max, 0)];
                const v487 = v388[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:29:array ref', stdlib.UInt_max, 0)];
                const v489 = stdlib.sub(stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v487);
                const v490 = stdlib.add(v486, v489);
                const v491 = stdlib.mod(v490, stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v495 = v399[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:18:array ref', stdlib.UInt_max, 1)];
                const v496 = v388[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:29:array ref', stdlib.UInt_max, 1)];
                const v498 = stdlib.sub(stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v496);
                const v499 = stdlib.add(v495, v498);
                const v500 = stdlib.mod(v499, stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v504 = v399[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:18:array ref', stdlib.UInt_max, 2)];
                const v505 = v388[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:29:array ref', stdlib.UInt_max, 2)];
                const v507 = stdlib.sub(stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v505);
                const v508 = stdlib.add(v504, v507);
                const v509 = stdlib.mod(v508, stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v513 = v399[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:18:array ref', stdlib.UInt_max, 3)];
                const v514 = v388[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:29:array ref', stdlib.UInt_max, 3)];
                const v516 = stdlib.sub(stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v514);
                const v517 = stdlib.add(v513, v516);
                const v518 = stdlib.mod(v517, stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v522 = v399[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:18:array ref', stdlib.UInt_max, 4)];
                const v523 = v388[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:29:array ref', stdlib.UInt_max, 4)];
                const v525 = stdlib.sub(stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v523);
                const v526 = stdlib.add(v522, v525);
                const v527 = stdlib.mod(v526, stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v540 = stdlib.eq(v491, stdlib.checkedBigNumberify('./tut3/tut3.rsh:makeEnum', stdlib.UInt_max, 1));
                const v541 = v540 ? v500 : v491;
                const v544 = stdlib.eq(v541, stdlib.checkedBigNumberify('./tut3/tut3.rsh:makeEnum', stdlib.UInt_max, 1));
                const v545 = v544 ? v509 : v541;
                const v548 = stdlib.eq(v545, stdlib.checkedBigNumberify('./tut3/tut3.rsh:makeEnum', stdlib.UInt_max, 1));
                const v549 = v548 ? v518 : v545;
                const v552 = stdlib.eq(v549, stdlib.checkedBigNumberify('./tut3/tut3.rsh:makeEnum', stdlib.UInt_max, 1));
                const v553 = v552 ? v527 : v549;
                const cv166 = v553;
                const cv167 = v167;
                const cv612 = v404;
                const cv613 = v405;
                
                (() => {
                  const v166 = cv166;
                  const v167 = cv167;
                  const v612 = cv612;
                  const v613 = cv613;
                  
                  if ((() => {
                    const v177 = stdlib.eq(v166, stdlib.checkedBigNumberify('./tut3/tut3.rsh:makeEnum', stdlib.UInt_max, 1));
                    
                    return v177; })()) {
                    const v178 = stdlib.mod(v167, stdlib.checkedBigNumberify('./tut3/tut3.rsh:128:21:decimal', stdlib.UInt_max, 2));
                    const v179 = stdlib.eq(v178, stdlib.checkedBigNumberify('./tut3/tut3.rsh:128:26:decimal', stdlib.UInt_max, 0));
                    if (v179) {
                      sim_r.nextSt = stdlib.digest(ctc6, [stdlib.checkedBigNumberify('./tut3/tut3.rsh:103:19:after expr stmt semicolon', stdlib.UInt_max, 6), v47, v48, v49, v59, v167, v612, v613]);
                      sim_r.nextSt_noTime = stdlib.digest(ctc7, [stdlib.checkedBigNumberify('./tut3/tut3.rsh:103:19:after expr stmt semicolon', stdlib.UInt_max, 6), v47, v48, v49, v59, v167, v612]);
                      sim_r.isHalt = false;
                       }
                    else {
                      sim_r.nextSt = stdlib.digest(ctc6, [stdlib.checkedBigNumberify('./tut3/tut3.rsh:103:19:after expr stmt semicolon', stdlib.UInt_max, 6), v47, v48, v49, v59, v167, v612, v613]);
                      sim_r.nextSt_noTime = stdlib.digest(ctc7, [stdlib.checkedBigNumberify('./tut3/tut3.rsh:103:19:after expr stmt semicolon', stdlib.UInt_max, 6), v47, v48, v49, v59, v167, v612]);
                      sim_r.isHalt = false;
                       } }
                  else {
                    const v556 = stdlib.eq(v166, stdlib.checkedBigNumberify('./tut3/tut3.rsh:makeEnum', stdlib.UInt_max, 2));
                    const v559 = stdlib.mul(stdlib.checkedBigNumberify('./tut3/tut3.rsh:140:16:decimal', stdlib.UInt_max, 2), v48);
                    const v561 = v556 ? v47 : v59;
                    sim_r.txns.push({
                      amt: v559,
                      to: v561
                       });
                    sim_r.nextSt = stdlib.digest(ctc8, []);
                    sim_r.nextSt_noTime = stdlib.digest(ctc8, []);
                    sim_r.isHalt = true;
                     } })();
                return sim_r;
                 })));
              if (txn6.didTimeout) {
                const txn7 = await (ctc.recv('Bob', 19, 0, [], false, false));
                const [] = txn7.data;
                const v413 = txn7.value;
                const v418 = txn7.time;
                const v412 = txn7.from;
                const v414 = stdlib.eq(v413, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
                stdlib.assert(v414, {
                  at: 'reach standard library:68:7:dot',
                  fs: ['at ./tut3/tut3.rsh:123:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./tut3/tut3.rsh:133:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                  msg: 'pay amount correct',
                  who: 'Bob'
                   });
                const v415 = stdlib.addressEq(v47, v412);
                stdlib.assert(v415, {
                  at: 'reach standard library:68:7:dot',
                  fs: ['at ./tut3/tut3.rsh:123:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./tut3/tut3.rsh:133:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                  msg: 'sender correct',
                  who: 'Bob'
                   });
                const v417 = stdlib.add(v393, v413);
                ;
                stdlib.protect(ctc2, await interact.informTimeout(), {
                  at: './tut3/tut3.rsh:68:33:application',
                  fs: ['at ./tut3/tut3.rsh:67:13:application call to [unknown function] (defined at: ./tut3/tut3.rsh:67:25:function exp)', 'at reach standard library:71:8:application call to "after" (defined at: ./tut3/tut3.rsh:66:32:function exp)', 'at ./tut3/tut3.rsh:123:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./tut3/tut3.rsh:133:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                  msg: 'informTimeout',
                  who: 'Bob'
                   });
                return;
                 }
              else {
                const [v398, v399] = txn6.data;
                const v400 = txn6.value;
                const v405 = txn6.time;
                const v397 = txn6.from;
                const v401 = stdlib.eq(v400, stdlib.checkedBigNumberify('./tut3/tut3.rsh:decimal', stdlib.UInt_max, 0));
                stdlib.assert(v401, {
                  at: './tut3/tut3.rsh:122:17:dot',
                  fs: ['at ./tut3/tut3.rsh:133:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                  msg: 'pay amount correct',
                  who: 'Bob'
                   });
                const v402 = stdlib.addressEq(v59, v397);
                stdlib.assert(v402, {
                  at: './tut3/tut3.rsh:122:17:dot',
                  fs: ['at ./tut3/tut3.rsh:133:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                  msg: 'sender correct',
                  who: 'Bob'
                   });
                const v404 = stdlib.add(v393, v400);
                const v407 = stdlib.digest(ctc4, [v398, v399]);
                const v408 = stdlib.eq(v377, v407);
                stdlib.assert(v408, {
                  at: 'reach standard library:65:17:application',
                  fs: ['at ./tut3/tut3.rsh:124:26:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)', 'at ./tut3/tut3.rsh:133:42:application call to "doRound" (defined at: ./tut3/tut3.rsh:102:54:function exp)'],
                  msg: null,
                  who: 'Bob'
                   });
                const v486 = v399[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:18:array ref', stdlib.UInt_max, 0)];
                const v487 = v388[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:29:array ref', stdlib.UInt_max, 0)];
                const v489 = stdlib.sub(stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v487);
                const v490 = stdlib.add(v486, v489);
                const v491 = stdlib.mod(v490, stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v495 = v399[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:18:array ref', stdlib.UInt_max, 1)];
                const v496 = v388[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:29:array ref', stdlib.UInt_max, 1)];
                const v498 = stdlib.sub(stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v496);
                const v499 = stdlib.add(v495, v498);
                const v500 = stdlib.mod(v499, stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v504 = v399[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:18:array ref', stdlib.UInt_max, 2)];
                const v505 = v388[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:29:array ref', stdlib.UInt_max, 2)];
                const v507 = stdlib.sub(stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v505);
                const v508 = stdlib.add(v504, v507);
                const v509 = stdlib.mod(v508, stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v513 = v399[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:18:array ref', stdlib.UInt_max, 3)];
                const v514 = v388[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:29:array ref', stdlib.UInt_max, 3)];
                const v516 = stdlib.sub(stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v514);
                const v517 = stdlib.add(v513, v516);
                const v518 = stdlib.mod(v517, stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v522 = v399[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:18:array ref', stdlib.UInt_max, 4)];
                const v523 = v388[stdlib.checkedBigNumberify('./tut3/tut3.rsh:39:29:array ref', stdlib.UInt_max, 4)];
                const v525 = stdlib.sub(stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:18:decimal', stdlib.UInt_max, 4), v523);
                const v526 = stdlib.add(v522, v525);
                const v527 = stdlib.mod(v526, stdlib.checkedBigNumberify('./tut3/tut3.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v540 = stdlib.eq(v491, stdlib.checkedBigNumberify('./tut3/tut3.rsh:makeEnum', stdlib.UInt_max, 1));
                const v541 = v540 ? v500 : v491;
                const v544 = stdlib.eq(v541, stdlib.checkedBigNumberify('./tut3/tut3.rsh:makeEnum', stdlib.UInt_max, 1));
                const v545 = v544 ? v509 : v541;
                const v548 = stdlib.eq(v545, stdlib.checkedBigNumberify('./tut3/tut3.rsh:makeEnum', stdlib.UInt_max, 1));
                const v549 = v548 ? v518 : v545;
                const v552 = stdlib.eq(v549, stdlib.checkedBigNumberify('./tut3/tut3.rsh:makeEnum', stdlib.UInt_max, 1));
                const v553 = v552 ? v527 : v549;
                const cv166 = v553;
                const cv167 = v167;
                const cv612 = v404;
                const cv613 = v405;
                
                v166 = cv166;
                v167 = cv167;
                v612 = cv612;
                v613 = cv613;
                
                continue; }
               }
             }
           } }
      const v556 = stdlib.eq(v166, stdlib.checkedBigNumberify('./tut3/tut3.rsh:makeEnum', stdlib.UInt_max, 2));
      const v559 = stdlib.mul(stdlib.checkedBigNumberify('./tut3/tut3.rsh:140:16:decimal', stdlib.UInt_max, 2), v48);
      const v561 = v556 ? v47 : v59;
      ;
      stdlib.protect(ctc2, await interact.seeOutcome(v166), {
        at: './tut3/tut3.rsh:144:28:application',
        fs: ['at ./tut3/tut3.rsh:143:11:application call to [unknown function] (defined at: ./tut3/tut3.rsh:143:23:function exp)'],
        msg: 'seeOutcome',
        who: 'Bob'
         });
      return; }
     }
  
  
   }

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
global ZeroAddress
byte "{{Deployer}}"
global GroupSize
int 1
-
txn GroupIndex
==
gtxna 0 ApplicationArgs 2
btoi
&&
select
txn CloseRemainderTo
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
txn CloseRemainderTo
gtxn 1 Sender
==
assert
// Run body
// Just "pay amount correct"
// "./tut3/tut3.rsh:78:9:dot"
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
// "./tut3/tut3.rsh:78:9:dot"
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
// Just "pay amount correct"
// "./tut3/tut3.rsh:87:9:dot"
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
// "./tut3/tut3.rsh:87:9:dot"
// "[]"
int 1
assert
gtxna 0 ApplicationArgs 9
btoi
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
+
store 255
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
gtxn 3 Sender
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
// Just "pay amount correct"
// "reach standard library:68:7:dot"
// "[at ./tut3/tut3.rsh:89:41:application call to \"closeTo\" (defined at: reach standard library:67:8:function exp)]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "reach standard library:68:7:dot"
// "[at ./tut3/tut3.rsh:89:41:application call to \"closeTo\" (defined at: reach standard library:67:8:function exp)]"
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
gtxn 5 Fee
gtxn 4 Fee
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
// Just "pay amount correct"
// "./tut3/tut3.rsh:95:9:dot"
// "[]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "./tut3/tut3.rsh:95:9:dot"
// "[]"
gtxna 0 ApplicationArgs 5
gtxn 3 Sender
==
assert
gtxna 0 ApplicationArgs 11
btoi
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
+
store 255
// Nothing
// "reach standard library:65:17:application"
// "[at ./tut3/tut3.rsh:97:22:application call to \"checkCommitment\" (defined at: reach standard library:64:8:function exp)]"
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
gtxn 5 Fee
gtxn 4 Fee
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
// Just "pay amount correct"
// "reach standard library:68:7:dot"
// "[at ./tut3/tut3.rsh:96:40:application call to \"closeTo\" (defined at: reach standard library:67:8:function exp)]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "reach standard library:68:7:dot"
// "[at ./tut3/tut3.rsh:96:40:application call to \"closeTo\" (defined at: reach standard library:67:8:function exp)]"
gtxna 0 ApplicationArgs 9
gtxn 3 Sender
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
gtxn 5 Fee
gtxn 4 Fee
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
// Just "pay amount correct"
// "./tut3/tut3.rsh:109:17:dot"
// "[at ./tut3/tut3.rsh:129:42:application call to \"doRound\" (defined at: ./tut3/tut3.rsh:102:54:function exp)]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "./tut3/tut3.rsh:109:17:dot"
// "[at ./tut3/tut3.rsh:129:42:application call to \"doRound\" (defined at: ./tut3/tut3.rsh:102:54:function exp)]"
gtxna 0 ApplicationArgs 8
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
// Just "pay amount correct"
// "reach standard library:68:7:dot"
// "[at ./tut3/tut3.rsh:110:45:application call to \"closeTo\" (defined at: reach standard library:67:8:function exp),at ./tut3/tut3.rsh:129:42:application call to \"doRound\" (defined at: ./tut3/tut3.rsh:102:54:function exp)]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "reach standard library:68:7:dot"
// "[at ./tut3/tut3.rsh:110:45:application call to \"closeTo\" (defined at: reach standard library:67:8:function exp),at ./tut3/tut3.rsh:129:42:application call to \"doRound\" (defined at: ./tut3/tut3.rsh:102:54:function exp)]"
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
gtxn 5 Fee
gtxn 4 Fee
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
// Just "pay amount correct"
// "./tut3/tut3.rsh:116:18:dot"
// "[at ./tut3/tut3.rsh:129:42:application call to \"doRound\" (defined at: ./tut3/tut3.rsh:102:54:function exp)]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "./tut3/tut3.rsh:116:18:dot"
// "[at ./tut3/tut3.rsh:129:42:application call to \"doRound\" (defined at: ./tut3/tut3.rsh:102:54:function exp)]"
gtxna 0 ApplicationArgs 5
gtxn 3 Sender
==
assert
gtxna 0 ApplicationArgs 11
btoi
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
+
store 255
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
// Just "pay amount correct"
// "reach standard library:68:7:dot"
// "[at ./tut3/tut3.rsh:117:45:application call to \"closeTo\" (defined at: reach standard library:67:8:function exp),at ./tut3/tut3.rsh:129:42:application call to \"doRound\" (defined at: ./tut3/tut3.rsh:102:54:function exp)]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "reach standard library:68:7:dot"
// "[at ./tut3/tut3.rsh:117:45:application call to \"closeTo\" (defined at: reach standard library:67:8:function exp),at ./tut3/tut3.rsh:129:42:application call to \"doRound\" (defined at: ./tut3/tut3.rsh:102:54:function exp)]"
gtxna 0 ApplicationArgs 8
gtxn 3 Sender
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
gtxn 5 Fee
gtxn 4 Fee
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
// Just "pay amount correct"
// "./tut3/tut3.rsh:122:17:dot"
// "[at ./tut3/tut3.rsh:129:42:application call to \"doRound\" (defined at: ./tut3/tut3.rsh:102:54:function exp)]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "./tut3/tut3.rsh:122:17:dot"
// "[at ./tut3/tut3.rsh:129:42:application call to \"doRound\" (defined at: ./tut3/tut3.rsh:102:54:function exp)]"
gtxna 0 ApplicationArgs 8
gtxn 3 Sender
==
assert
gtxna 0 ApplicationArgs 12
btoi
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
+
store 255
// Nothing
// "reach standard library:65:17:application"
// "[at ./tut3/tut3.rsh:124:26:application call to \"checkCommitment\" (defined at: reach standard library:64:8:function exp),at ./tut3/tut3.rsh:129:42:application call to \"doRound\" (defined at: ./tut3/tut3.rsh:102:54:function exp)]"
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
gtxn 5 Fee
gtxn 4 Fee
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
// Just "pay amount correct"
// "reach standard library:68:7:dot"
// "[at ./tut3/tut3.rsh:123:45:application call to \"closeTo\" (defined at: reach standard library:67:8:function exp),at ./tut3/tut3.rsh:129:42:application call to \"doRound\" (defined at: ./tut3/tut3.rsh:102:54:function exp)]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "reach standard library:68:7:dot"
// "[at ./tut3/tut3.rsh:123:45:application call to \"closeTo\" (defined at: reach standard library:67:8:function exp),at ./tut3/tut3.rsh:129:42:application call to \"doRound\" (defined at: ./tut3/tut3.rsh:102:54:function exp)]"
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
gtxna 0 ApplicationArgs 12
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
gtxn 5 Fee
gtxn 4 Fee
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
// Just "pay amount correct"
// "./tut3/tut3.rsh:109:17:dot"
// "[at ./tut3/tut3.rsh:133:42:application call to \"doRound\" (defined at: ./tut3/tut3.rsh:102:54:function exp)]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "./tut3/tut3.rsh:109:17:dot"
// "[at ./tut3/tut3.rsh:133:42:application call to \"doRound\" (defined at: ./tut3/tut3.rsh:102:54:function exp)]"
gtxna 0 ApplicationArgs 8
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
// Just "pay amount correct"
// "reach standard library:68:7:dot"
// "[at ./tut3/tut3.rsh:110:45:application call to \"closeTo\" (defined at: reach standard library:67:8:function exp),at ./tut3/tut3.rsh:133:42:application call to \"doRound\" (defined at: ./tut3/tut3.rsh:102:54:function exp)]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "reach standard library:68:7:dot"
// "[at ./tut3/tut3.rsh:110:45:application call to \"closeTo\" (defined at: reach standard library:67:8:function exp),at ./tut3/tut3.rsh:133:42:application call to \"doRound\" (defined at: ./tut3/tut3.rsh:102:54:function exp)]"
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
gtxn 5 Fee
gtxn 4 Fee
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
// Just "pay amount correct"
// "./tut3/tut3.rsh:116:18:dot"
// "[at ./tut3/tut3.rsh:133:42:application call to \"doRound\" (defined at: ./tut3/tut3.rsh:102:54:function exp)]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "./tut3/tut3.rsh:116:18:dot"
// "[at ./tut3/tut3.rsh:133:42:application call to \"doRound\" (defined at: ./tut3/tut3.rsh:102:54:function exp)]"
gtxna 0 ApplicationArgs 5
gtxn 3 Sender
==
assert
gtxna 0 ApplicationArgs 11
btoi
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
+
store 255
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
// Just "pay amount correct"
// "reach standard library:68:7:dot"
// "[at ./tut3/tut3.rsh:117:45:application call to \"closeTo\" (defined at: reach standard library:67:8:function exp),at ./tut3/tut3.rsh:133:42:application call to \"doRound\" (defined at: ./tut3/tut3.rsh:102:54:function exp)]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "reach standard library:68:7:dot"
// "[at ./tut3/tut3.rsh:117:45:application call to \"closeTo\" (defined at: reach standard library:67:8:function exp),at ./tut3/tut3.rsh:133:42:application call to \"doRound\" (defined at: ./tut3/tut3.rsh:102:54:function exp)]"
gtxna 0 ApplicationArgs 8
gtxn 3 Sender
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
gtxn 5 Fee
gtxn 4 Fee
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
// Just "pay amount correct"
// "./tut3/tut3.rsh:122:17:dot"
// "[at ./tut3/tut3.rsh:133:42:application call to \"doRound\" (defined at: ./tut3/tut3.rsh:102:54:function exp)]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "./tut3/tut3.rsh:122:17:dot"
// "[at ./tut3/tut3.rsh:133:42:application call to \"doRound\" (defined at: ./tut3/tut3.rsh:102:54:function exp)]"
gtxna 0 ApplicationArgs 8
gtxn 3 Sender
==
assert
gtxna 0 ApplicationArgs 12
btoi
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
+
store 255
// Nothing
// "reach standard library:65:17:application"
// "[at ./tut3/tut3.rsh:124:26:application call to \"checkCommitment\" (defined at: reach standard library:64:8:function exp),at ./tut3/tut3.rsh:133:42:application call to \"doRound\" (defined at: ./tut3/tut3.rsh:102:54:function exp)]"
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
gtxn 5 Fee
gtxn 4 Fee
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
// Just "pay amount correct"
// "reach standard library:68:7:dot"
// "[at ./tut3/tut3.rsh:123:45:application call to \"closeTo\" (defined at: reach standard library:67:8:function exp),at ./tut3/tut3.rsh:133:42:application call to \"doRound\" (defined at: ./tut3/tut3.rsh:102:54:function exp)]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "reach standard library:68:7:dot"
// "[at ./tut3/tut3.rsh:123:45:application call to \"closeTo\" (defined at: reach standard library:67:8:function exp),at ./tut3/tut3.rsh:133:42:application call to \"doRound\" (defined at: ./tut3/tut3.rsh:102:54:function exp)]"
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
gtxna 0 ApplicationArgs 12
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
gtxn 5 Fee
gtxn 4 Fee
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
                "name": "v39",
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
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v50",
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
                "name": "v47",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v59",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v167",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v189",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v194",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v195",
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
                "name": "v200",
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
                "name": "v47",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v59",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v167",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v189",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v194",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v195",
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
                "name": "v47",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v59",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v167",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v189",
                "type": "uint256"
              },
              {
                "internalType": "uint256[5]",
                "name": "v200",
                "type": "uint256[5]"
              },
              {
                "internalType": "uint256",
                "name": "v205",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v206",
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
                "name": "v210",
                "type": "uint256"
              },
              {
                "internalType": "uint256[5]",
                "name": "v211",
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
                "name": "v47",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v59",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v167",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v189",
                "type": "uint256"
              },
              {
                "internalType": "uint256[5]",
                "name": "v200",
                "type": "uint256[5]"
              },
              {
                "internalType": "uint256",
                "name": "v205",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v206",
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
                "name": "v47",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v59",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v167",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v612",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v613",
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
                "name": "v377",
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
                "name": "v47",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v59",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v167",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v612",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v613",
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
                "name": "v47",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v59",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v167",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v377",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v382",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v383",
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
                "name": "v388",
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
                "name": "v47",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v59",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v167",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v377",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v382",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v383",
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
                "name": "v47",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v59",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v167",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v377",
                "type": "uint256"
              },
              {
                "internalType": "uint256[5]",
                "name": "v388",
                "type": "uint256[5]"
              },
              {
                "internalType": "uint256",
                "name": "v393",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v394",
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
                "name": "v398",
                "type": "uint256"
              },
              {
                "internalType": "uint256[5]",
                "name": "v399",
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
                "name": "v47",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v59",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v167",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v377",
                "type": "uint256"
              },
              {
                "internalType": "uint256[5]",
                "name": "v388",
                "type": "uint256[5]"
              },
              {
                "internalType": "uint256",
                "name": "v393",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v394",
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
                "name": "v47",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v50",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v54",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v55",
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
                "name": "v60",
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
                "name": "v47",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v50",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v54",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v55",
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
                "name": "v47",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v50",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v59",
                "type": "address"
              },
              {
                "internalType": "uint256[5]",
                "name": "v60",
                "type": "uint256[5]"
              },
              {
                "internalType": "uint256",
                "name": "v64",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v65",
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
                "name": "v69",
                "type": "uint256"
              },
              {
                "internalType": "uint256[5]",
                "name": "v70",
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
                "name": "v47",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v50",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v59",
                "type": "address"
              },
              {
                "internalType": "uint256[5]",
                "name": "v60",
                "type": "uint256[5]"
              },
              {
                "internalType": "uint256",
                "name": "v64",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v65",
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
                "name": "v47",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v59",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v167",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v612",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v613",
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
                "name": "v189",
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
                "name": "v47",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v59",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v167",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v612",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v613",
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
                "name": "v39",
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
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v50",
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
                "name": "v47",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v59",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v167",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v189",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v194",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v195",
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
                "name": "v200",
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
                "name": "v47",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v59",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v167",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v189",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v194",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v195",
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
                "name": "v47",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v59",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v167",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v189",
                "type": "uint256"
              },
              {
                "internalType": "uint256[5]",
                "name": "v200",
                "type": "uint256[5]"
              },
              {
                "internalType": "uint256",
                "name": "v205",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v206",
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
                "name": "v210",
                "type": "uint256"
              },
              {
                "internalType": "uint256[5]",
                "name": "v211",
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
                "name": "v47",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v59",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v167",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v189",
                "type": "uint256"
              },
              {
                "internalType": "uint256[5]",
                "name": "v200",
                "type": "uint256[5]"
              },
              {
                "internalType": "uint256",
                "name": "v205",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v206",
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
                "name": "v47",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v59",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v167",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v612",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v613",
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
                "name": "v377",
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
                "name": "v47",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v59",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v167",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v612",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v613",
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
                "name": "v47",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v59",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v167",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v377",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v382",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v383",
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
                "name": "v388",
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
                "name": "v47",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v59",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v167",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v377",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v382",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v383",
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
                "name": "v47",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v59",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v167",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v377",
                "type": "uint256"
              },
              {
                "internalType": "uint256[5]",
                "name": "v388",
                "type": "uint256[5]"
              },
              {
                "internalType": "uint256",
                "name": "v393",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v394",
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
                "name": "v398",
                "type": "uint256"
              },
              {
                "internalType": "uint256[5]",
                "name": "v399",
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
                "name": "v47",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v59",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v167",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v377",
                "type": "uint256"
              },
              {
                "internalType": "uint256[5]",
                "name": "v388",
                "type": "uint256[5]"
              },
              {
                "internalType": "uint256",
                "name": "v393",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v394",
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
                "name": "v47",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v50",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v54",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v55",
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
                "name": "v60",
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
                "name": "v47",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v50",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v54",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v55",
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
                "name": "v47",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v50",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v59",
                "type": "address"
              },
              {
                "internalType": "uint256[5]",
                "name": "v60",
                "type": "uint256[5]"
              },
              {
                "internalType": "uint256",
                "name": "v64",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v65",
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
                "name": "v69",
                "type": "uint256"
              },
              {
                "internalType": "uint256[5]",
                "name": "v70",
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
                "name": "v47",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v50",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v59",
                "type": "address"
              },
              {
                "internalType": "uint256[5]",
                "name": "v60",
                "type": "uint256[5]"
              },
              {
                "internalType": "uint256",
                "name": "v64",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v65",
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
                "name": "v47",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v59",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v167",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v612",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v613",
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
                "name": "v189",
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
                "name": "v47",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v49",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v59",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v167",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v612",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v613",
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
  Bytecode: `0x608060408190527f49ff028a829527a47ec6839c7147b484eccf5a2a94853eddac09cef44d9d4e9e90600090a16040805160208082018352438252825180820184526000808252925181528351808301849052905181850152835180820385018152606090910190935282519201919091209055612932806100826000396000f3fe6080604052600436106101025760003560e01c8063515d273111610095578063b2dc9b3c11610064578063b2dc9b3c146101f4578063b830ed6e14610207578063c74327551461021a578063d10ac9221461022d578063d22bdb581461024057610109565b8063515d2731146101a857806355b09c29146101bb5780636dac4fdc146101ce578063a701f0cc146101e157610109565b80633aa258ab116100d15780633aa258ab1461015c5780633df7a7891461016f578063497ccc6a146101825780634f4e2b061461019557610109565b8063253d71be1461010e5780632bf4f873146101235780632d8ee29e1461013657806338015b161461014957610109565b3661010957005b600080fd5b61012161011c366004612175565b610253565b005b610121610131366004612201565b61037b565b6101216101443660046121e5565b61051c565b6101216101573660046121ad565b61062f565b61012161016a3660046121ad565b610746565b61012161017d366004612191565b61085d565b610121610190366004612191565b610a15565b6101216101a3366004612175565b610b26565b6101216101b6366004612212565b610cb6565b6101216101c93660046121c9565b610e06565b6101216101dc366004612191565b611115565b6101216101ef3660046121c9565b611226565b610121610202366004612175565b6114d8565b6101216102153660046121e5565b611668565b610121610228366004612191565b61177b565b61012161023b366004612163565b611933565b61012161024e366004612224565b611c10565b6040516102679060029083906020016127ec565b6040516020818303038152906040528051906020012060001c6000541461028d57600080fd5b600080556102a46040820135610160830135612878565b43101580156102b1575060015b6102ba57600080fd5b34156102c557600080fd5b336102d660a0830160808401612142565b6001600160a01b0316146102e957600080fd5b6102f960a0820160808301612142565b6001600160a01b03166108fc61031434610140850135612878565b6040518115909202916000818181858888f1935050505015801561033c573d6000803e3d6000fd5b507fc18ab0af979eec50e5539334a3b97c236f4df25f2c0886da35b2f866708b0b138160405161036c9190612576565b60405180910390a16000805533ff5b60408051600060208201528235918101919091526060016040516020818303038152906040528051906020012060001c600054146103b857600080fd5b60008080556040805160208101909152908152346020830135146103db57600080fd5b6103e6346000612878565b815260408051833581526020808501359082015283820135818301526060808501359082015290517f2bb570a5feee0f446e450005a048c78efd478914692e1f0be1009bac144b11709181900360800190a161047a6040518060c0016040528060006001600160a01b0316815260200160008152602001600081526020016000815260200160008152602001600081525090565b338152602083810135818301908152604080860135818501908152606080880135818701908152875160808089019182524360a0808b01918252875160019a81019a909a528a516001600160a01b0316978a019790975296519388019390935292519186019190915251918401919091525160c08301525160e0820152610100015b60408051601f198184030181529190528051602090910120600055505050565b6040516105309060109083906020016127c2565b6040516020818303038152906040528051906020012060001c6000541461055657600080fd5b6000805561056d6040820135610180830135612878565b431015801561057a575060015b61058357600080fd5b341561058e57600080fd5b3361059c6020830183612142565b6001600160a01b0316146105af57600080fd5b6105bc6020820182612142565b6001600160a01b03166108fc6105d734610160850135612878565b6040518115909202916000818181858888f193505050501580156105ff573d6000803e3d6000fd5b507fd6fbb3cbca99542c0c2ad4a426f274a3e1b0990f868dfa7680cc48800395e0fd8160405161036c919061263b565b60405161064390600890839060200161272f565b6040516020818303038152906040528051906020012060001c6000541461066957600080fd5b6000805561067f604082013560e0830135612878565b431015801561068c575060015b61069557600080fd5b34156106a057600080fd5b336106b16080830160608401612142565b6001600160a01b0316146106c457600080fd5b6106d46080820160608301612142565b6001600160a01b03166108fc6106ee3460c0850135612878565b6040518115909202916000818181858888f19350505050158015610716573d6000803e3d6000fd5b507fb9d135d4afaa7938b4616c3637968c0e71f66413b54043da6188989930963f898160405161036c919061260c565b60405161075a90600e90839060200161272f565b6040516020818303038152906040528051906020012060001c6000541461078057600080fd5b60008055610796604082013560e0830135612878565b43101580156107a3575060015b6107ac57600080fd5b34156107b757600080fd5b336107c86080830160608401612142565b6001600160a01b0316146107db57600080fd5b6107eb6080820160608301612142565b6001600160a01b03166108fc6108053460c0850135612878565b6040518115909202916000818181858888f1935050505015801561082d573d6000803e3d6000fd5b507fe592bdad3da2420c74b7bd9841a064dd887014772397104fb0e8f350ee4ab2c68160405161036c919061260c565b6040516108719060069083906020016126b8565b6040516020818303038152906040528051906020012060001c6000541461089757600080fd5b6000808055604080516020810182529182526108ba9083013560c0840135612878565b43106108c557600080fd5b34156108d057600080fd5b336108e16080840160608501612142565b6001600160a01b0316146108f457600080fd5b6109023460a0840135612878565b81526040517f6739bb4acbb3812eea51c48895245b154776bf02dd61571666f6abba93266fec906109349084906125a9565b60405180910390a161099660405180610100016040528060006001600160a01b03168152602001600081526020016000815260200160006001600160a01b03168152602001600081526020016000815260200160008152602001600081525090565b6109a36020840184612142565b6001600160a01b0316815260208084013590820152604080840135908201526109d26080840160608501612142565b6001600160a01b031660608201526080808401359082015260e08084013560a0830152825160c083015243908201526040516104fc906008908390602001612744565b604051610a299060069083906020016126b8565b6040516020818303038152906040528051906020012060001c60005414610a4f57600080fd5b60008055610a65604082013560c0830135612878565b4310158015610a72575060015b610a7b57600080fd5b3415610a8657600080fd5b33610a946020830183612142565b6001600160a01b031614610aa757600080fd5b610ab46020820182612142565b6001600160a01b03166108fc610ace3460a0850135612878565b6040518115909202916000818181858888f19350505050158015610af6573d6000803e3d6000fd5b507fcdae4cbd433f8c4039f23f2632824e3ab0089e9b8c2050e8e87e4d6e0a3df09b8160405161036c91906125c6565b604051610b3a90600e90839060200161272f565b6040516020818303038152906040528051906020012060001c60005414610b6057600080fd5b600080805560408051602081018252918252610b839083013560e0840135612878565b4310610b8e57600080fd5b3415610b9957600080fd5b33610ba76020840184612142565b6001600160a01b031614610bba57600080fd5b610bc83460c0840135612878565b81526040517f1ce2536efdc067dd1e872bca971868b9750c7f8faa8b4eece19fa68af9cb89c090610bfa9084906125f7565b60405180910390a1610c0a611f4c565b610c176020840184612142565b6001600160a01b031681526020808401359082015260408084013590820152610c466080840160608501612142565b6001600160a01b031660608201526080808401359082015260a0808401358183015260408051808301909152906101008501906005908390839080828437600092019190915250505060c0820152815160e0820152436101008201526040516104fc9060109083906020016127d7565b604051610cca9060019083906020016127ae565b6040516020818303038152906040528051906020012060001c60005414610cf057600080fd5b600080805560408051602081018252918252610d139083013560a0840135612878565b4310610d1e57600080fd5b34602083013514610d2e57600080fd5b610d3c346080840135612878565b81526040517f128ceb6e462bfebf9ef10870b6d9ae608efb33f1cdefac45d94895b5f28af76e90610d6e90849061264a565b60405180910390a1610d7e611fb0565b610d8b6020840184612142565b6001600160a01b03168152602080840135908201526040808401358183015260608085013590830152336080830152805160a08181019092529060c08501906005908390839080828437600092019190915250505060a0820152815160c08201524360e08201526040516104fc906002908390602001612801565b604051610e1a9060109083906020016127c2565b6040516020818303038152906040528051906020012060001c60005414610e4057600080fd5b60008081905550610e726040518060800160405280600081526020016000815260200160008152602001600081525090565b610e856040830135610180840135612878565b4310610e9057600080fd5b3415610e9b57600080fd5b33610eac6080840160608501612142565b6001600160a01b031614610ebf57600080fd5b604051610edb906101a0840135906101c085019060200161269e565b60408051601f19818403018152919052805160209091012060a083013514610f0257600080fd5b6003610f1360c084013560046128af565b610f22906101c0850135612878565b610f2c91906128c6565b808252600114610f3d578051610f67565b6003610f4e60e084013560046128af565b610f5d906101e0850135612878565b610f6791906128c6565b60208201819052600114610f7f578060200151610faa565b6003610f9161010084013560046128af565b610fa090610200850135612878565b610faa91906128c6565b60408201819052600114610fc2578060400151610fed565b6003610fd461012084013560046128af565b610fe390610220850135612878565b610fed91906128c6565b60608201526040517f523e44f49032f50206e5ffcdb88da3392c2e81e3e5608b43c05b289ebe2c3b9e9061102290849061261b565b60405180910390a1611032611ff9565b61103f6020840184612142565b81516001600160a01b039091169052805160208085013591015280516040808501359101526110746080840160608501612142565b81516001600160a01b0390911660609182015282015160011461109b5781606001516110ce565b60036110ad61014085013560046128af565b6101c0850160045b60200201356110c49190612878565b6110ce91906128c6565b6020808301805192909252905160808501359101526110f234610160850135612878565b60208201805160400191909152514360609091015261111081611d21565b505050565b6040516111299060069083906020016126b8565b6040516020818303038152906040528051906020012060001c6000541461114f57600080fd5b60008055611165604082013560c0830135612878565b4310158015611172575060015b61117b57600080fd5b341561118657600080fd5b336111946020830183612142565b6001600160a01b0316146111a757600080fd5b6111b46020820182612142565b6001600160a01b03166108fc6111ce3460a0850135612878565b6040518115909202916000818181858888f193505050501580156111f6573d6000803e3d6000fd5b507fe9b2c2b8b894a17634956282f47287e65395bf5f12d38d24114f9df9d064cb938160405161036c91906125c6565b60405161123a90600a9083906020016127c2565b6040516020818303038152906040528051906020012060001c6000541461126057600080fd5b600080819055506112926040518060800160405280600081526020016000815260200160008152602001600081525090565b6112a56040830135610180840135612878565b43106112b057600080fd5b34156112bb57600080fd5b336112cc6080840160608501612142565b6001600160a01b0316146112df57600080fd5b6040516112fb906101a0840135906101c085019060200161269e565b60408051601f19818403018152919052805160209091012060a08301351461132257600080fd5b60036113346101c084013560046128af565b6113429060c0850135612878565b61134c91906128c6565b80825260011461135d578051611387565b600361136f6101e084013560046128af565b61137d9060e0850135612878565b61138791906128c6565b6020820181905260011461139f5780602001516113ca565b60036113b161020084013560046128af565b6113c090610100850135612878565b6113ca91906128c6565b604082018190526001146113e257806040015161140d565b60036113f461022084013560046128af565b61140390610120850135612878565b61140d91906128c6565b60608201526040517f6ee71c1c32e4cf4bb84253364ba7d5a7f07dc28f60fbebebac51aae83206b8729061144290849061261b565b60405180910390a1611452611ff9565b61145f6020840184612142565b81516001600160a01b039091169052805160208085013591015280516040808501359101526114946080840160608501612142565b81516001600160a01b039091166060918201528201516001146114bb5781606001516110ce565b60036114cd61024085013560046128af565b60c0850160046110b5565b6040516114ec90600890839060200161272f565b6040516020818303038152906040528051906020012060001c6000541461151257600080fd5b6000808055604080516020810182529182526115359083013560e0840135612878565b431061154057600080fd5b341561154b57600080fd5b336115596020840184612142565b6001600160a01b03161461156c57600080fd5b61157a3460c0840135612878565b81526040517f4b39e6355207f2c77bb98c443fb3e5e02a82c850a077da7e0938cd671b364487906115ac9084906125f7565b60405180910390a16115bc611f4c565b6115c96020840184612142565b6001600160a01b0316815260208084013590820152604080840135908201526115f86080840160608501612142565b6001600160a01b031660608201526080808401359082015260a0808401358183015260408051808301909152906101008501906005908390839080828437600092019190915250505060c0820152815160e0820152436101008201526040516104fc90600a9083906020016127d7565b60405161167c90600a9083906020016127c2565b6040516020818303038152906040528051906020012060001c600054146116a257600080fd5b600080556116b96040820135610180830135612878565b43101580156116c6575060015b6116cf57600080fd5b34156116da57600080fd5b336116e86020830183612142565b6001600160a01b0316146116fb57600080fd5b6117086020820182612142565b6001600160a01b03166108fc61172334610160850135612878565b6040518115909202916000818181858888f1935050505015801561174b573d6000803e3d6000fd5b507ff145fb2384258d0f3c54ba74b14d362f8412b221ff369ece9d48eb2ad8e034cf8160405161036c919061263b565b60405161178f9060069083906020016126b8565b6040516020818303038152906040528051906020012060001c600054146117b557600080fd5b6000808055604080516020810182529182526117d89083013560c0840135612878565b43106117e357600080fd5b34156117ee57600080fd5b336117ff6080840160608501612142565b6001600160a01b03161461181257600080fd5b6118203460a0840135612878565b81526040517f8034f7b80d60c125999e5b76eb5d44b464ae8dc58c5f3258d4e1e4d905597898906118529084906125a9565b60405180910390a16118b460405180610100016040528060006001600160a01b03168152602001600081526020016000815260200160006001600160a01b03168152602001600081526020016000815260200160008152602001600081525090565b6118c16020840184612142565b6001600160a01b0316815260208084013590820152604080840135908201526118f06080840160608501612142565b6001600160a01b031660608201526080808401359082015260e08084013560a0830152825160c083015243908201526040516104fc90600e908390602001612744565b6040516119479060029083906020016127ec565b6040516020818303038152906040528051906020012060001c6000541461196d57600080fd5b6000808190555061199f6040518060800160405280600081526020016000815260200160008152602001600081525090565b6119b26040830135610160840135612878565b43106119bd57600080fd5b34156119c857600080fd5b336119d66020840184612142565b6001600160a01b0316146119e957600080fd5b604051611a0590610180840135906101a085019060200161269e565b60408051601f198184030181529190528051602090910120606083013514611a2c57600080fd5b6003611a3d60a084013560046128af565b611a4c906101a0850135612878565b611a5691906128c6565b808252600114611a67578051611a91565b6003611a7860c084013560046128af565b611a87906101c0850135612878565b611a9191906128c6565b60208201819052600114611aa9578060200151611ad3565b6003611aba60e084013560046128af565b611ac9906101e0850135612878565b611ad391906128c6565b60408201819052600114611aeb578060400151611b16565b6003611afd61010084013560046128af565b611b0c90610200850135612878565b611b1691906128c6565b60608201526040517f85b984f93e23bb278dcf4c426e3ebada3df03a830c17f29b763cc686eeff603090611b4b90849061254f565b60405180910390a1611b5b611ff9565b611b686020840184612142565b81516001600160a01b03909116905280516020808501359101528051604080850135910152611b9d60a0840160808501612142565b81516001600160a01b03909116606091820152820151600114611bc4578160600151611bef565b6003611bd661012085013560046128af565b611be590610220860135612878565b611bef91906128c6565b6020808301805192909252905160009101526110f234610140850135612878565b604051611c249060019083906020016127ae565b6040516020818303038152906040528051906020012060001c60005414611c4a57600080fd5b60008055611c60604082013560a0830135612878565b4310158015611c6d575060015b611c7657600080fd5b3415611c8157600080fd5b33611c8f6020830183612142565b6001600160a01b031614611ca257600080fd5b611caf6020820182612142565b6001600160a01b03166108fc611cc9346080850135612878565b6040518115909202916000818181858888f19350505050158015611cf1573d6000803e3d6000fd5b507f17040e3ed853a8df776cd092f1357f15488d98d460f66cd5e6b0cb07d5bc8ae38160405161036c919061266e565b60208101515160011415611e715760006002826020015160200151611d4691906128c6565b1415611dde57611d54612059565b8151516001600160a01b03908116825282516020908101518184015283516040908101518185015284516060908101519093168385015281850180518301516080860152805182015160a0860152519092015160c08401529051611dbd916006918491016126cd565b60408051601f19818403018152919052805160209091012060005550611e6c565b611de6612059565b8151516001600160a01b03908116825282516020908101518184015283516040908101518185015284516060908101519093168385015281850180518301516080860152805182015160a0860152519092015160c08401529051611e4f916006918491016126cd565b60408051601f198184030181529190528051602090910120600055505b611edc565b6040805160c081018252600091810182815260608083018481526080840185815260a085018681528486526020808701979097528751516001600160a01b03908116909552875187015190925286519092015190921690529183015151909152611eda81611edf565b505b50565b805160600151600214611ef757805160400151611efb565b8051515b6001600160a01b03166108fc8260000151602001516002611f1c9190612890565b6040518115909202916000818181858888f19350505050158015611f44573d6000803e3d6000fd5b506000805533ff5b60405180610120016040528060006001600160a01b03168152602001600081526020016000815260200160006001600160a01b031681526020016000815260200160008152602001611f9c6120a8565b815260200160008152602001600081525090565b60405180610100016040528060006001600160a01b0316815260200160008152602001600081526020016000815260200160006001600160a01b03168152602001611f9c6120a8565b6040805160c0810182526000918101828152606082018390526080820183905260a082019290925290819081526020016120546040518060800160405280600081526020016000815260200160008152602001600081525090565b905290565b6040518060e0016040528060006001600160a01b03168152602001600081526020016000815260200160006001600160a01b031681526020016000815260200160008152602001600081525090565b6040518060a001604052806005906020820280368337509192915050565b80356001600160a01b03811681146120dd57600080fd5b919050565b60006101a082840312156120f4578081fd5b50919050565b600061010082840312156120f4578081fd5b600061012082840312156120f4578081fd5b600061026082840312156120f4578081fd5b60006101c082840312156120f4578081fd5b600060208284031215612153578081fd5b61215c826120c6565b9392505050565b600061024082840312156120f4578081fd5b60006101a08284031215612187578081fd5b61215c83836120e2565b600061010082840312156121a3578081fd5b61215c83836120fa565b600061012082840312156121bf578081fd5b61215c838361210c565b600061026082840312156121db578081fd5b61215c838361211e565b60006101c082840312156121f7578081fd5b61215c8383612130565b6000608082840312156120f4578081fd5b600061016082840312156120f4578081fd5b600060e082840312156120f4578081fd5b8060005b6005811015612258578151845260209384019390910190600101612239565b50505050565b8035825260a0602082016020840137600060c08301525050565b6001600160a01b038061228a836120c6565b1683526020820135602084015260408201356040840152806122ae606084016120c6565b166060840152506080810135608083015260a081013560a083015260c081013560c08301525050565b6001600160a01b03806122e9836120c6565b16835260208201356020840152604082013560408401528061230d606084016120c6565b166060840152506080810135608083015260a081013560a083015260c081013560c083015260e081013560e08301525050565b6001600160a01b03612351826120c6565b1682526020810135602083015260408101356040830152606081013560608301526080810135608083015260a081013560a08301525050565b6001600160a01b038061239c836120c6565b1683526020820135602084015260408201356040840152806123c0606084016120c6565b166060840152506080810135608083015260a081013560a083015260a060c0820160c0840137610160818101359083015261018090810135910152565b60018060a01b0381511682526020810151602083015260408101516040830152606081015161243760608401826001600160a01b03169052565b506080810151608083015260a081015160a083015260c081015161245e60c0840182612235565b5060e0810151610160830152610100015161018090910152565b61248282826122d7565b61010060a0818301828501375060006101a08301525050565b6124a582826122d7565b610100808201358015158082146124bb57600080fd5b80838601525050505050565b6124d1828261238a565b6101a0808201358015158082146124bb57600080fd5b6001600160a01b03806124f9836120c6565b16835260208201356020840152604082013560408401526060820135606084015280612527608084016120c6565b1660808401525060a080820160a0840137610140818101359083015261016090810135910152565b610240810161255e82846124e7565b61018061256f81840182860161225e565b5092915050565b6101a0810161258582846124e7565b6101808084013580151580821461259b57600080fd5b808386015250505092915050565b61010081016125b88284612278565b60e092830135919092015290565b61010081016125d58284612278565b60e08301358015158082146125e957600080fd5b8060e0850152505092915050565b6101a081016126068284612478565b92915050565b6101208101612606828461249b565b610260810161262a828461238a565b6101a061256f81840182860161225e565b6101c0810161260682846124c7565b61016081016126598284612340565b60a060c0840160c08401376000815292915050565b60e0810161267c8284612340565b60c083013580151580821461269057600080fd5b8060c0850152505092915050565b82815260c0810160a0836020840137600081529392505050565b828152610100810161215c6020830184612278565b60006101008201905083825260018060a01b03808451166020840152602084015160408401526040840151606084015280606085015116608084015250608083015160a083015260a083015160c083015260c083015160e08301529392505050565b828152610120810161215c60208301846122d7565b828152610120810161215c602083018460018060a01b038082511683526020820151602084015260408201516040840152806060830151166060840152506080810151608083015260a081015160a083015260c081015160c083015260e081015160e08301525050565b82815260e0810161215c6020830184612340565b8281526101c0810161215c602083018461238a565b8281526101c0810161215c60208301846123fd565b8281526101a0810161215c60208301846124e7565b60006101a08201905083825260018060a01b038084511660208401526020840151604084015260408401516060840152606084015160808401528060808501511660a08401525060a083015161285a60c0840182612235565b5060c083015161016083015260e08301516101808301529392505050565b6000821982111561288b5761288b6128e6565b500190565b60008160001904831182151516156128aa576128aa6128e6565b500290565b6000828210156128c1576128c16128e6565b500390565b6000826128e157634e487b7160e01b81526012600452602481fd5b500690565b634e487b7160e01b600052601160045260246000fdfea2646970667358221220260e3cd87624fdaf134326219ebd1c18f3de64a64864573ef469df49dcb23f8b64736f6c63430008020033`,
  deployMode: `DM_constructor`
   };

export const _Connectors = {
  ALGO: _ALGO,
  ETH: _ETH
   };

