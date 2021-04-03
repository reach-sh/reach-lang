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
  const ctc5 = stdlib.T_Address;
  const ctc6 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc0, ctc5, ctc0, ctc0, ctc0]);
  const ctc7 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc0, ctc5, ctc0, ctc0]);
  const ctc8 = stdlib.T_Tuple([]);
  const ctc9 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc0, ctc5, ctc0, ctc3, ctc0, ctc0]);
  const ctc10 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc0, ctc5, ctc0, ctc3, ctc0]);
  const ctc11 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc0, ctc3, ctc5, ctc1, ctc0, ctc0]);
  const ctc12 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc0, ctc3, ctc5, ctc1, ctc0]);
  const ctc13 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc0, ctc3, ctc0, ctc0]);
  const ctc14 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc0, ctc3, ctc0]);
  const ctc15 = stdlib.T_Tuple([ctc0, ctc0]);
  const ctc16 = stdlib.T_Tuple([ctc0]);
  
  
  const v41 = await ctc.creationTime();
  const v37 = stdlib.protect(ctc0, interact.DEADLINE, null);
  const v38 = stdlib.protect(ctc1, interact.firstBatch, null);
  const v39 = stdlib.protect(ctc0, interact.wager, null);
  const v45 = stdlib.protect(ctc0, await interact.random(), {
    at: 'reach standard library:60:31:application',
    fs: ['at ./tut2/tut2.rsh:74:74:application call to "makeCommitment" (defined at: reach standard library:59:8:function exp)', 'at ./tut2/tut2.rsh:70:13:application call to [unknown function] (defined at: ./tut2/tut2.rsh:70:17:function exp)'],
    msg: 'random',
    who: 'Alice'
     });
  const v46 = stdlib.digest(ctc2, [v45, v38]);
  const txn1 = await (ctc.sendrecv('Alice', 1, 3, stdlib.checkedBigNumberify('./tut2/tut2.rsh:78:9:dot', stdlib.UInt_max, 0), [ctc0, ctc0, ctc0, ctc3], [v41, v39, v37, v46], v39, [ctc0, ctc0, ctc3], true, true, false, (async (txn1) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.digest(ctc15, [stdlib.checkedBigNumberify('./tut2/tut2.rsh:78:9:dot', stdlib.UInt_max, 0), v41]);
    sim_r.prevSt_noPrevTime = stdlib.digest(ctc16, [stdlib.checkedBigNumberify('./tut2/tut2.rsh:78:9:dot', stdlib.UInt_max, 0)]);
    const [v49, v50, v51] = txn1.data;
    const v52 = txn1.value;
    const v56 = txn1.time;
    const v48 = txn1.from;
    
    const v53 = stdlib.eq(v52, v49);
    stdlib.assert(v53, {
      at: './tut2/tut2.rsh:78:9:dot',
      fs: [],
      msg: 'pay amount correct',
      who: 'Alice'
       });
    stdlib.assert(true, {
      at: './tut2/tut2.rsh:78:9:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Alice'
       });
    const v55 = stdlib.add(stdlib.checkedBigNumberify('./tut2/tut2.rsh:compileDApp', stdlib.UInt_max, 0), v52);
    sim_r.nextSt = stdlib.digest(ctc13, [stdlib.checkedBigNumberify('./tut2/tut2.rsh:80:15:after expr stmt semicolon', stdlib.UInt_max, 1), v48, v49, v50, v51, v55, v56]);
    sim_r.nextSt_noTime = stdlib.digest(ctc14, [stdlib.checkedBigNumberify('./tut2/tut2.rsh:80:15:after expr stmt semicolon', stdlib.UInt_max, 1), v48, v49, v50, v51, v55]);
    sim_r.isHalt = false;
    
    return sim_r;
     })));
  const [v49, v50, v51] = txn1.data;
  const v52 = txn1.value;
  const v56 = txn1.time;
  const v48 = txn1.from;
  const v53 = stdlib.eq(v52, v49);
  stdlib.assert(v53, {
    at: './tut2/tut2.rsh:78:9:dot',
    fs: [],
    msg: 'pay amount correct',
    who: 'Alice'
     });
  stdlib.assert(true, {
    at: './tut2/tut2.rsh:78:9:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Alice'
     });
  const v55 = stdlib.add(stdlib.checkedBigNumberify('./tut2/tut2.rsh:compileDApp', stdlib.UInt_max, 0), v52);
  const txn2 = await (ctc.recv('Alice', 2, 1, [ctc1], false, v50));
  if (txn2.didTimeout) {
    const txn3 = await (ctc.sendrecv('Alice', 3, 0, stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 5), [ctc5, ctc0, ctc0, ctc3, ctc0, ctc0], [v48, v49, v50, v51, v55, v56], stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), [], true, true, false, (async (txn3) => {
      const sim_r = { txns: [] };
      sim_r.prevSt = stdlib.digest(ctc13, [stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 1), v48, v49, v50, v51, v55, v56]);
      sim_r.prevSt_noPrevTime = stdlib.digest(ctc14, [stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 1), v48, v49, v50, v51, v55]);
      const [] = txn3.data;
      const v401 = txn3.value;
      const v406 = txn3.time;
      const v400 = txn3.from;
      
      const v402 = stdlib.eq(v401, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
      stdlib.assert(v402, {
        at: 'reach standard library:68:7:dot',
        fs: ['at ./tut2/tut2.rsh:89:41:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
        msg: 'pay amount correct',
        who: 'Alice'
         });
      const v403 = stdlib.addressEq(v48, v400);
      stdlib.assert(v403, {
        at: 'reach standard library:68:7:dot',
        fs: ['at ./tut2/tut2.rsh:89:41:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
        msg: 'sender correct',
        who: 'Alice'
         });
      const v405 = stdlib.add(v55, v401);
      sim_r.txns.push({
        amt: v405,
        to: v48
         });
      sim_r.nextSt = stdlib.digest(ctc8, []);
      sim_r.nextSt_noTime = stdlib.digest(ctc8, []);
      sim_r.isHalt = true;
      
      return sim_r;
       })));
    const [] = txn3.data;
    const v401 = txn3.value;
    const v406 = txn3.time;
    const v400 = txn3.from;
    const v402 = stdlib.eq(v401, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
    stdlib.assert(v402, {
      at: 'reach standard library:68:7:dot',
      fs: ['at ./tut2/tut2.rsh:89:41:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
      msg: 'pay amount correct',
      who: 'Alice'
       });
    const v403 = stdlib.addressEq(v48, v400);
    stdlib.assert(v403, {
      at: 'reach standard library:68:7:dot',
      fs: ['at ./tut2/tut2.rsh:89:41:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
      msg: 'sender correct',
      who: 'Alice'
       });
    const v405 = stdlib.add(v55, v401);
    ;
    stdlib.protect(ctc4, await interact.informTimeout(), {
      at: './tut2/tut2.rsh:68:33:application',
      fs: ['at ./tut2/tut2.rsh:67:13:application call to [unknown function] (defined at: ./tut2/tut2.rsh:67:25:function exp)', 'at reach standard library:71:8:application call to "after" (defined at: ./tut2/tut2.rsh:66:32:function exp)', 'at ./tut2/tut2.rsh:89:41:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
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
    const v62 = stdlib.eq(v61, v49);
    stdlib.assert(v62, {
      at: './tut2/tut2.rsh:87:9:dot',
      fs: [],
      msg: 'pay amount correct',
      who: 'Alice'
       });
    stdlib.assert(true, {
      at: './tut2/tut2.rsh:87:9:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Alice'
       });
    const v64 = stdlib.add(v55, v61);
    const txn3 = await (ctc.sendrecv('Alice', 4, 2, stdlib.checkedBigNumberify('./tut2/tut2.rsh:95:9:dot', stdlib.UInt_max, 7), [ctc5, ctc0, ctc0, ctc3, ctc5, ctc1, ctc0, ctc0, ctc0, ctc1], [v48, v49, v50, v51, v59, v60, v64, v65, v45, v38], stdlib.checkedBigNumberify('./tut2/tut2.rsh:decimal', stdlib.UInt_max, 0), [ctc0, ctc1], true, true, v50, (async (txn3) => {
      const sim_r = { txns: [] };
      sim_r.prevSt = stdlib.digest(ctc11, [stdlib.checkedBigNumberify('./tut2/tut2.rsh:95:9:dot', stdlib.UInt_max, 2), v48, v49, v50, v51, v59, v60, v64, v65]);
      sim_r.prevSt_noPrevTime = stdlib.digest(ctc12, [stdlib.checkedBigNumberify('./tut2/tut2.rsh:95:9:dot', stdlib.UInt_max, 2), v48, v49, v50, v51, v59, v60, v64]);
      const [v69, v70] = txn3.data;
      const v71 = txn3.value;
      const v76 = txn3.time;
      const v68 = txn3.from;
      
      const v72 = stdlib.eq(v71, stdlib.checkedBigNumberify('./tut2/tut2.rsh:decimal', stdlib.UInt_max, 0));
      stdlib.assert(v72, {
        at: './tut2/tut2.rsh:95:9:dot',
        fs: [],
        msg: 'pay amount correct',
        who: 'Alice'
         });
      const v73 = stdlib.addressEq(v48, v68);
      stdlib.assert(v73, {
        at: './tut2/tut2.rsh:95:9:dot',
        fs: [],
        msg: 'sender correct',
        who: 'Alice'
         });
      const v75 = stdlib.add(v64, v71);
      const v78 = stdlib.digest(ctc2, [v69, v70]);
      const v79 = stdlib.eq(v51, v78);
      stdlib.assert(v79, {
        at: 'reach standard library:65:17:application',
        fs: ['at ./tut2/tut2.rsh:97:22:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)'],
        msg: null,
        who: 'Alice'
         });
      const v96 = v70[stdlib.checkedBigNumberify('./tut2/tut2.rsh:39:18:array ref', stdlib.UInt_max, 0)];
      const v97 = v60[stdlib.checkedBigNumberify('./tut2/tut2.rsh:39:29:array ref', stdlib.UInt_max, 0)];
      const v99 = stdlib.sub(stdlib.checkedBigNumberify('./tut2/tut2.rsh:7:18:decimal', stdlib.UInt_max, 4), v97);
      const v100 = stdlib.add(v96, v99);
      const v101 = stdlib.mod(v100, stdlib.checkedBigNumberify('./tut2/tut2.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v105 = v70[stdlib.checkedBigNumberify('./tut2/tut2.rsh:39:18:array ref', stdlib.UInt_max, 1)];
      const v106 = v60[stdlib.checkedBigNumberify('./tut2/tut2.rsh:39:29:array ref', stdlib.UInt_max, 1)];
      const v108 = stdlib.sub(stdlib.checkedBigNumberify('./tut2/tut2.rsh:7:18:decimal', stdlib.UInt_max, 4), v106);
      const v109 = stdlib.add(v105, v108);
      const v110 = stdlib.mod(v109, stdlib.checkedBigNumberify('./tut2/tut2.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v114 = v70[stdlib.checkedBigNumberify('./tut2/tut2.rsh:39:18:array ref', stdlib.UInt_max, 2)];
      const v115 = v60[stdlib.checkedBigNumberify('./tut2/tut2.rsh:39:29:array ref', stdlib.UInt_max, 2)];
      const v117 = stdlib.sub(stdlib.checkedBigNumberify('./tut2/tut2.rsh:7:18:decimal', stdlib.UInt_max, 4), v115);
      const v118 = stdlib.add(v114, v117);
      const v119 = stdlib.mod(v118, stdlib.checkedBigNumberify('./tut2/tut2.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v123 = v70[stdlib.checkedBigNumberify('./tut2/tut2.rsh:39:18:array ref', stdlib.UInt_max, 3)];
      const v124 = v60[stdlib.checkedBigNumberify('./tut2/tut2.rsh:39:29:array ref', stdlib.UInt_max, 3)];
      const v126 = stdlib.sub(stdlib.checkedBigNumberify('./tut2/tut2.rsh:7:18:decimal', stdlib.UInt_max, 4), v124);
      const v127 = stdlib.add(v123, v126);
      const v128 = stdlib.mod(v127, stdlib.checkedBigNumberify('./tut2/tut2.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v132 = v70[stdlib.checkedBigNumberify('./tut2/tut2.rsh:39:18:array ref', stdlib.UInt_max, 4)];
      const v133 = v60[stdlib.checkedBigNumberify('./tut2/tut2.rsh:39:29:array ref', stdlib.UInt_max, 4)];
      const v135 = stdlib.sub(stdlib.checkedBigNumberify('./tut2/tut2.rsh:7:18:decimal', stdlib.UInt_max, 4), v133);
      const v136 = stdlib.add(v132, v135);
      const v137 = stdlib.mod(v136, stdlib.checkedBigNumberify('./tut2/tut2.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v150 = stdlib.eq(v101, stdlib.checkedBigNumberify('./tut2/tut2.rsh:makeEnum', stdlib.UInt_max, 1));
      const v151 = v150 ? v110 : v101;
      const v154 = stdlib.eq(v151, stdlib.checkedBigNumberify('./tut2/tut2.rsh:makeEnum', stdlib.UInt_max, 1));
      const v155 = v154 ? v119 : v151;
      const v158 = stdlib.eq(v155, stdlib.checkedBigNumberify('./tut2/tut2.rsh:makeEnum', stdlib.UInt_max, 1));
      const v159 = v158 ? v128 : v155;
      const v162 = stdlib.eq(v159, stdlib.checkedBigNumberify('./tut2/tut2.rsh:makeEnum', stdlib.UInt_max, 1));
      const v163 = v162 ? v137 : v159;
      const v166 = v163;
      const v419 = v75;
      const v420 = v76;
      
      if ((() => {
        const v176 = stdlib.eq(v166, stdlib.checkedBigNumberify('./tut2/tut2.rsh:makeEnum', stdlib.UInt_max, 1));
        
        return v176; })()) {
        sim_r.nextSt = stdlib.digest(ctc6, [stdlib.checkedBigNumberify('./tut2/tut2.rsh:102:17:after expr stmt semicolon', stdlib.UInt_max, 6), v48, v49, v50, v59, v163, v419, v420]);
        sim_r.nextSt_noTime = stdlib.digest(ctc7, [stdlib.checkedBigNumberify('./tut2/tut2.rsh:102:17:after expr stmt semicolon', stdlib.UInt_max, 6), v48, v49, v50, v59, v163, v419]);
        sim_r.isHalt = false;
         }
      else {
        const v363 = stdlib.eq(v166, stdlib.checkedBigNumberify('./tut2/tut2.rsh:makeEnum', stdlib.UInt_max, 2));
        const v366 = stdlib.mul(stdlib.checkedBigNumberify('./tut2/tut2.rsh:129:16:decimal', stdlib.UInt_max, 2), v49);
        const v368 = v363 ? v48 : v59;
        sim_r.txns.push({
          amt: v366,
          to: v368
           });
        sim_r.nextSt = stdlib.digest(ctc8, []);
        sim_r.nextSt_noTime = stdlib.digest(ctc8, []);
        sim_r.isHalt = true;
         }
      return sim_r;
       })));
    if (txn3.didTimeout) {
      const txn4 = await (ctc.recv('Alice', 5, 0, [], false, false));
      const [] = txn4.data;
      const v381 = txn4.value;
      const v386 = txn4.time;
      const v380 = txn4.from;
      const v382 = stdlib.eq(v381, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
      stdlib.assert(v382, {
        at: 'reach standard library:68:7:dot',
        fs: ['at ./tut2/tut2.rsh:96:40:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
        msg: 'pay amount correct',
        who: 'Alice'
         });
      const v383 = stdlib.addressEq(v59, v380);
      stdlib.assert(v383, {
        at: 'reach standard library:68:7:dot',
        fs: ['at ./tut2/tut2.rsh:96:40:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
        msg: 'sender correct',
        who: 'Alice'
         });
      const v385 = stdlib.add(v64, v381);
      ;
      stdlib.protect(ctc4, await interact.informTimeout(), {
        at: './tut2/tut2.rsh:68:33:application',
        fs: ['at ./tut2/tut2.rsh:67:13:application call to [unknown function] (defined at: ./tut2/tut2.rsh:67:25:function exp)', 'at reach standard library:71:8:application call to "after" (defined at: ./tut2/tut2.rsh:66:32:function exp)', 'at ./tut2/tut2.rsh:96:40:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
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
      const v72 = stdlib.eq(v71, stdlib.checkedBigNumberify('./tut2/tut2.rsh:decimal', stdlib.UInt_max, 0));
      stdlib.assert(v72, {
        at: './tut2/tut2.rsh:95:9:dot',
        fs: [],
        msg: 'pay amount correct',
        who: 'Alice'
         });
      const v73 = stdlib.addressEq(v48, v68);
      stdlib.assert(v73, {
        at: './tut2/tut2.rsh:95:9:dot',
        fs: [],
        msg: 'sender correct',
        who: 'Alice'
         });
      const v75 = stdlib.add(v64, v71);
      const v78 = stdlib.digest(ctc2, [v69, v70]);
      const v79 = stdlib.eq(v51, v78);
      stdlib.assert(v79, {
        at: 'reach standard library:65:17:application',
        fs: ['at ./tut2/tut2.rsh:97:22:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)'],
        msg: null,
        who: 'Alice'
         });
      const v96 = v70[stdlib.checkedBigNumberify('./tut2/tut2.rsh:39:18:array ref', stdlib.UInt_max, 0)];
      const v97 = v60[stdlib.checkedBigNumberify('./tut2/tut2.rsh:39:29:array ref', stdlib.UInt_max, 0)];
      const v99 = stdlib.sub(stdlib.checkedBigNumberify('./tut2/tut2.rsh:7:18:decimal', stdlib.UInt_max, 4), v97);
      const v100 = stdlib.add(v96, v99);
      const v101 = stdlib.mod(v100, stdlib.checkedBigNumberify('./tut2/tut2.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v105 = v70[stdlib.checkedBigNumberify('./tut2/tut2.rsh:39:18:array ref', stdlib.UInt_max, 1)];
      const v106 = v60[stdlib.checkedBigNumberify('./tut2/tut2.rsh:39:29:array ref', stdlib.UInt_max, 1)];
      const v108 = stdlib.sub(stdlib.checkedBigNumberify('./tut2/tut2.rsh:7:18:decimal', stdlib.UInt_max, 4), v106);
      const v109 = stdlib.add(v105, v108);
      const v110 = stdlib.mod(v109, stdlib.checkedBigNumberify('./tut2/tut2.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v114 = v70[stdlib.checkedBigNumberify('./tut2/tut2.rsh:39:18:array ref', stdlib.UInt_max, 2)];
      const v115 = v60[stdlib.checkedBigNumberify('./tut2/tut2.rsh:39:29:array ref', stdlib.UInt_max, 2)];
      const v117 = stdlib.sub(stdlib.checkedBigNumberify('./tut2/tut2.rsh:7:18:decimal', stdlib.UInt_max, 4), v115);
      const v118 = stdlib.add(v114, v117);
      const v119 = stdlib.mod(v118, stdlib.checkedBigNumberify('./tut2/tut2.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v123 = v70[stdlib.checkedBigNumberify('./tut2/tut2.rsh:39:18:array ref', stdlib.UInt_max, 3)];
      const v124 = v60[stdlib.checkedBigNumberify('./tut2/tut2.rsh:39:29:array ref', stdlib.UInt_max, 3)];
      const v126 = stdlib.sub(stdlib.checkedBigNumberify('./tut2/tut2.rsh:7:18:decimal', stdlib.UInt_max, 4), v124);
      const v127 = stdlib.add(v123, v126);
      const v128 = stdlib.mod(v127, stdlib.checkedBigNumberify('./tut2/tut2.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v132 = v70[stdlib.checkedBigNumberify('./tut2/tut2.rsh:39:18:array ref', stdlib.UInt_max, 4)];
      const v133 = v60[stdlib.checkedBigNumberify('./tut2/tut2.rsh:39:29:array ref', stdlib.UInt_max, 4)];
      const v135 = stdlib.sub(stdlib.checkedBigNumberify('./tut2/tut2.rsh:7:18:decimal', stdlib.UInt_max, 4), v133);
      const v136 = stdlib.add(v132, v135);
      const v137 = stdlib.mod(v136, stdlib.checkedBigNumberify('./tut2/tut2.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v150 = stdlib.eq(v101, stdlib.checkedBigNumberify('./tut2/tut2.rsh:makeEnum', stdlib.UInt_max, 1));
      const v151 = v150 ? v110 : v101;
      const v154 = stdlib.eq(v151, stdlib.checkedBigNumberify('./tut2/tut2.rsh:makeEnum', stdlib.UInt_max, 1));
      const v155 = v154 ? v119 : v151;
      const v158 = stdlib.eq(v155, stdlib.checkedBigNumberify('./tut2/tut2.rsh:makeEnum', stdlib.UInt_max, 1));
      const v159 = v158 ? v128 : v155;
      const v162 = stdlib.eq(v159, stdlib.checkedBigNumberify('./tut2/tut2.rsh:makeEnum', stdlib.UInt_max, 1));
      const v163 = v162 ? v137 : v159;
      let v166 = v163;
      let v419 = v75;
      let v420 = v76;
      
      while ((() => {
        const v176 = stdlib.eq(v166, stdlib.checkedBigNumberify('./tut2/tut2.rsh:makeEnum', stdlib.UInt_max, 1));
        
        return v176; })()) {
        const v179 = stdlib.protect(ctc1, await interact.getBatch(), {
          at: './tut2/tut2.rsh:105:44:application',
          fs: ['at ./tut2/tut2.rsh:104:15:application call to [unknown function] (defined at: ./tut2/tut2.rsh:104:19:function exp)'],
          msg: 'getBatch',
          who: 'Alice'
           });
        const v181 = stdlib.protect(ctc0, await interact.random(), {
          at: 'reach standard library:60:31:application',
          fs: ['at ./tut2/tut2.rsh:106:52:application call to "makeCommitment" (defined at: reach standard library:59:8:function exp)', 'at ./tut2/tut2.rsh:104:15:application call to [unknown function] (defined at: ./tut2/tut2.rsh:104:19:function exp)'],
          msg: 'random',
          who: 'Alice'
           });
        const v182 = stdlib.digest(ctc2, [v181, v179]);
        const txn4 = await (ctc.sendrecv('Alice', 8, 1, stdlib.checkedBigNumberify('./tut2/tut2.rsh:108:11:dot', stdlib.UInt_max, 6), [ctc5, ctc0, ctc0, ctc5, ctc0, ctc0, ctc0, ctc3], [v48, v49, v50, v59, v163, v419, v420, v182], stdlib.checkedBigNumberify('./tut2/tut2.rsh:decimal', stdlib.UInt_max, 0), [ctc3], true, true, v50, (async (txn4) => {
          const sim_r = { txns: [] };
          sim_r.prevSt = stdlib.digest(ctc6, [stdlib.checkedBigNumberify('./tut2/tut2.rsh:108:11:dot', stdlib.UInt_max, 6), v48, v49, v50, v59, v163, v419, v420]);
          sim_r.prevSt_noPrevTime = stdlib.digest(ctc7, [stdlib.checkedBigNumberify('./tut2/tut2.rsh:108:11:dot', stdlib.UInt_max, 6), v48, v49, v50, v59, v163, v419]);
          const [v185] = txn4.data;
          const v186 = txn4.value;
          const v191 = txn4.time;
          const v184 = txn4.from;
          
          const v187 = stdlib.eq(v186, stdlib.checkedBigNumberify('./tut2/tut2.rsh:decimal', stdlib.UInt_max, 0));
          stdlib.assert(v187, {
            at: './tut2/tut2.rsh:108:11:dot',
            fs: [],
            msg: 'pay amount correct',
            who: 'Alice'
             });
          const v188 = stdlib.addressEq(v48, v184);
          stdlib.assert(v188, {
            at: './tut2/tut2.rsh:108:11:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Alice'
             });
          const v190 = stdlib.add(v419, v186);
          sim_r.nextSt = stdlib.digest(ctc9, [stdlib.checkedBigNumberify('./tut2/tut2.rsh:110:17:after expr stmt semicolon', stdlib.UInt_max, 8), v48, v49, v50, v59, v163, v185, v190, v191]);
          sim_r.nextSt_noTime = stdlib.digest(ctc10, [stdlib.checkedBigNumberify('./tut2/tut2.rsh:110:17:after expr stmt semicolon', stdlib.UInt_max, 8), v48, v49, v50, v59, v163, v185, v190]);
          sim_r.isHalt = false;
          
          return sim_r;
           })));
        if (txn4.didTimeout) {
          const txn5 = await (ctc.recv('Alice', 9, 0, [], false, false));
          const [] = txn5.data;
          const v345 = txn5.value;
          const v350 = txn5.time;
          const v344 = txn5.from;
          const v346 = stdlib.eq(v345, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
          stdlib.assert(v346, {
            at: 'reach standard library:68:7:dot',
            fs: ['at ./tut2/tut2.rsh:109:43:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
            msg: 'pay amount correct',
            who: 'Alice'
             });
          const v347 = stdlib.addressEq(v59, v344);
          stdlib.assert(v347, {
            at: 'reach standard library:68:7:dot',
            fs: ['at ./tut2/tut2.rsh:109:43:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
            msg: 'sender correct',
            who: 'Alice'
             });
          const v349 = stdlib.add(v419, v345);
          ;
          stdlib.protect(ctc4, await interact.informTimeout(), {
            at: './tut2/tut2.rsh:68:33:application',
            fs: ['at ./tut2/tut2.rsh:67:13:application call to [unknown function] (defined at: ./tut2/tut2.rsh:67:25:function exp)', 'at reach standard library:71:8:application call to "after" (defined at: ./tut2/tut2.rsh:66:32:function exp)', 'at ./tut2/tut2.rsh:109:43:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
            msg: 'informTimeout',
            who: 'Alice'
             });
          return;
           }
        else {
          const [v185] = txn4.data;
          const v186 = txn4.value;
          const v191 = txn4.time;
          const v184 = txn4.from;
          const v187 = stdlib.eq(v186, stdlib.checkedBigNumberify('./tut2/tut2.rsh:decimal', stdlib.UInt_max, 0));
          stdlib.assert(v187, {
            at: './tut2/tut2.rsh:108:11:dot',
            fs: [],
            msg: 'pay amount correct',
            who: 'Alice'
             });
          const v188 = stdlib.addressEq(v48, v184);
          stdlib.assert(v188, {
            at: './tut2/tut2.rsh:108:11:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Alice'
             });
          const v190 = stdlib.add(v419, v186);
          const txn5 = await (ctc.recv('Alice', 10, 1, [ctc1], false, v50));
          if (txn5.didTimeout) {
            const txn6 = await (ctc.sendrecv('Alice', 11, 0, stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 7), [ctc5, ctc0, ctc0, ctc5, ctc0, ctc3, ctc0, ctc0], [v48, v49, v50, v59, v163, v185, v190, v191], stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), [], true, true, false, (async (txn6) => {
              const sim_r = { txns: [] };
              sim_r.prevSt = stdlib.digest(ctc9, [stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 8), v48, v49, v50, v59, v163, v185, v190, v191]);
              sim_r.prevSt_noPrevTime = stdlib.digest(ctc10, [stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 8), v48, v49, v50, v59, v163, v185, v190]);
              const [] = txn6.data;
              const v325 = txn6.value;
              const v330 = txn6.time;
              const v324 = txn6.from;
              
              const v326 = stdlib.eq(v325, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
              stdlib.assert(v326, {
                at: 'reach standard library:68:7:dot',
                fs: ['at ./tut2/tut2.rsh:116:43:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
                msg: 'pay amount correct',
                who: 'Alice'
                 });
              const v327 = stdlib.addressEq(v48, v324);
              stdlib.assert(v327, {
                at: 'reach standard library:68:7:dot',
                fs: ['at ./tut2/tut2.rsh:116:43:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
                msg: 'sender correct',
                who: 'Alice'
                 });
              const v329 = stdlib.add(v190, v325);
              sim_r.txns.push({
                amt: v329,
                to: v48
                 });
              sim_r.nextSt = stdlib.digest(ctc8, []);
              sim_r.nextSt_noTime = stdlib.digest(ctc8, []);
              sim_r.isHalt = true;
              
              return sim_r;
               })));
            const [] = txn6.data;
            const v325 = txn6.value;
            const v330 = txn6.time;
            const v324 = txn6.from;
            const v326 = stdlib.eq(v325, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v326, {
              at: 'reach standard library:68:7:dot',
              fs: ['at ./tut2/tut2.rsh:116:43:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
              msg: 'pay amount correct',
              who: 'Alice'
               });
            const v327 = stdlib.addressEq(v48, v324);
            stdlib.assert(v327, {
              at: 'reach standard library:68:7:dot',
              fs: ['at ./tut2/tut2.rsh:116:43:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
              msg: 'sender correct',
              who: 'Alice'
               });
            const v329 = stdlib.add(v190, v325);
            ;
            stdlib.protect(ctc4, await interact.informTimeout(), {
              at: './tut2/tut2.rsh:68:33:application',
              fs: ['at ./tut2/tut2.rsh:67:13:application call to [unknown function] (defined at: ./tut2/tut2.rsh:67:25:function exp)', 'at reach standard library:71:8:application call to "after" (defined at: ./tut2/tut2.rsh:66:32:function exp)', 'at ./tut2/tut2.rsh:116:43:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
              msg: 'informTimeout',
              who: 'Alice'
               });
            return;
             }
          else {
            const [v196] = txn5.data;
            const v197 = txn5.value;
            const v202 = txn5.time;
            const v195 = txn5.from;
            const v198 = stdlib.eq(v197, stdlib.checkedBigNumberify('./tut2/tut2.rsh:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v198, {
              at: './tut2/tut2.rsh:115:11:dot',
              fs: [],
              msg: 'pay amount correct',
              who: 'Alice'
               });
            const v199 = stdlib.addressEq(v59, v195);
            stdlib.assert(v199, {
              at: './tut2/tut2.rsh:115:11:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Alice'
               });
            const v201 = stdlib.add(v190, v197);
            const txn6 = await (ctc.sendrecv('Alice', 12, 2, stdlib.checkedBigNumberify('./tut2/tut2.rsh:121:11:dot', stdlib.UInt_max, 7), [ctc5, ctc0, ctc0, ctc5, ctc0, ctc3, ctc0, ctc0, ctc0, ctc1], [v48, v49, v50, v59, v163, v185, v201, v202, v181, v179], stdlib.checkedBigNumberify('./tut2/tut2.rsh:decimal', stdlib.UInt_max, 0), [ctc0, ctc1], true, true, v50, (async (txn6) => {
              const sim_r = { txns: [] };
              sim_r.prevSt = stdlib.digest(ctc9, [stdlib.checkedBigNumberify('./tut2/tut2.rsh:121:11:dot', stdlib.UInt_max, 10), v48, v49, v50, v59, v163, v185, v201, v202]);
              sim_r.prevSt_noPrevTime = stdlib.digest(ctc10, [stdlib.checkedBigNumberify('./tut2/tut2.rsh:121:11:dot', stdlib.UInt_max, 10), v48, v49, v50, v59, v163, v185, v201]);
              const [v206, v207] = txn6.data;
              const v208 = txn6.value;
              const v213 = txn6.time;
              const v205 = txn6.from;
              
              const v209 = stdlib.eq(v208, stdlib.checkedBigNumberify('./tut2/tut2.rsh:decimal', stdlib.UInt_max, 0));
              stdlib.assert(v209, {
                at: './tut2/tut2.rsh:121:11:dot',
                fs: [],
                msg: 'pay amount correct',
                who: 'Alice'
                 });
              const v210 = stdlib.addressEq(v48, v205);
              stdlib.assert(v210, {
                at: './tut2/tut2.rsh:121:11:dot',
                fs: [],
                msg: 'sender correct',
                who: 'Alice'
                 });
              const v212 = stdlib.add(v201, v208);
              const v215 = stdlib.digest(ctc2, [v206, v207]);
              const v216 = stdlib.eq(v185, v215);
              stdlib.assert(v216, {
                at: 'reach standard library:65:17:application',
                fs: ['at ./tut2/tut2.rsh:123:24:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)'],
                msg: null,
                who: 'Alice'
                 });
              const cv166 = v163;
              const cv419 = v212;
              const cv420 = v213;
              
              (() => {
                const v166 = cv166;
                const v419 = cv419;
                const v420 = cv420;
                
                if ((() => {
                  const v176 = stdlib.eq(v166, stdlib.checkedBigNumberify('./tut2/tut2.rsh:makeEnum', stdlib.UInt_max, 1));
                  
                  return v176; })()) {
                  sim_r.nextSt = stdlib.digest(ctc6, [stdlib.checkedBigNumberify('./tut2/tut2.rsh:102:17:after expr stmt semicolon', stdlib.UInt_max, 6), v48, v49, v50, v59, v163, v419, v420]);
                  sim_r.nextSt_noTime = stdlib.digest(ctc7, [stdlib.checkedBigNumberify('./tut2/tut2.rsh:102:17:after expr stmt semicolon', stdlib.UInt_max, 6), v48, v49, v50, v59, v163, v419]);
                  sim_r.isHalt = false;
                   }
                else {
                  const v363 = stdlib.eq(v166, stdlib.checkedBigNumberify('./tut2/tut2.rsh:makeEnum', stdlib.UInt_max, 2));
                  const v366 = stdlib.mul(stdlib.checkedBigNumberify('./tut2/tut2.rsh:129:16:decimal', stdlib.UInt_max, 2), v49);
                  const v368 = v363 ? v48 : v59;
                  sim_r.txns.push({
                    amt: v366,
                    to: v368
                     });
                  sim_r.nextSt = stdlib.digest(ctc8, []);
                  sim_r.nextSt_noTime = stdlib.digest(ctc8, []);
                  sim_r.isHalt = true;
                   } })();
              return sim_r;
               })));
            if (txn6.didTimeout) {
              const txn7 = await (ctc.recv('Alice', 13, 0, [], false, false));
              const [] = txn7.data;
              const v305 = txn7.value;
              const v310 = txn7.time;
              const v304 = txn7.from;
              const v306 = stdlib.eq(v305, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
              stdlib.assert(v306, {
                at: 'reach standard library:68:7:dot',
                fs: ['at ./tut2/tut2.rsh:122:43:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
                msg: 'pay amount correct',
                who: 'Alice'
                 });
              const v307 = stdlib.addressEq(v59, v304);
              stdlib.assert(v307, {
                at: 'reach standard library:68:7:dot',
                fs: ['at ./tut2/tut2.rsh:122:43:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
                msg: 'sender correct',
                who: 'Alice'
                 });
              const v309 = stdlib.add(v201, v305);
              ;
              stdlib.protect(ctc4, await interact.informTimeout(), {
                at: './tut2/tut2.rsh:68:33:application',
                fs: ['at ./tut2/tut2.rsh:67:13:application call to [unknown function] (defined at: ./tut2/tut2.rsh:67:25:function exp)', 'at reach standard library:71:8:application call to "after" (defined at: ./tut2/tut2.rsh:66:32:function exp)', 'at ./tut2/tut2.rsh:122:43:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
                msg: 'informTimeout',
                who: 'Alice'
                 });
              return;
               }
            else {
              const [v206, v207] = txn6.data;
              const v208 = txn6.value;
              const v213 = txn6.time;
              const v205 = txn6.from;
              const v209 = stdlib.eq(v208, stdlib.checkedBigNumberify('./tut2/tut2.rsh:decimal', stdlib.UInt_max, 0));
              stdlib.assert(v209, {
                at: './tut2/tut2.rsh:121:11:dot',
                fs: [],
                msg: 'pay amount correct',
                who: 'Alice'
                 });
              const v210 = stdlib.addressEq(v48, v205);
              stdlib.assert(v210, {
                at: './tut2/tut2.rsh:121:11:dot',
                fs: [],
                msg: 'sender correct',
                who: 'Alice'
                 });
              const v212 = stdlib.add(v201, v208);
              const v215 = stdlib.digest(ctc2, [v206, v207]);
              const v216 = stdlib.eq(v185, v215);
              stdlib.assert(v216, {
                at: 'reach standard library:65:17:application',
                fs: ['at ./tut2/tut2.rsh:123:24:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)'],
                msg: null,
                who: 'Alice'
                 });
              const cv166 = v163;
              const cv419 = v212;
              const cv420 = v213;
              
              v166 = cv166;
              v419 = cv419;
              v420 = cv420;
              
              continue; }
             }
           }
         }
      const v363 = stdlib.eq(v166, stdlib.checkedBigNumberify('./tut2/tut2.rsh:makeEnum', stdlib.UInt_max, 2));
      const v366 = stdlib.mul(stdlib.checkedBigNumberify('./tut2/tut2.rsh:129:16:decimal', stdlib.UInt_max, 2), v49);
      const v368 = v363 ? v48 : v59;
      ;
      stdlib.protect(ctc4, await interact.seeOutcome(v166), {
        at: './tut2/tut2.rsh:133:28:application',
        fs: ['at ./tut2/tut2.rsh:132:11:application call to [unknown function] (defined at: ./tut2/tut2.rsh:132:23:function exp)'],
        msg: 'seeOutcome',
        who: 'Alice'
         });
      return; }
     }
  
  
   }
export async function Bob(ctc, interact) {
  const stdlib = ctc.stdlib;
  const ctc0 = stdlib.T_UInt;
  const ctc1 = stdlib.T_Array(ctc0, stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, 5));
  const ctc2 = stdlib.T_Digest;
  const ctc3 = stdlib.T_Null;
  const ctc4 = stdlib.T_Tuple([ctc0, ctc1]);
  const ctc5 = stdlib.T_Tuple([]);
  const ctc6 = stdlib.T_Address;
  const ctc7 = stdlib.T_Tuple([ctc0, ctc6, ctc0, ctc0, ctc6, ctc0, ctc2, ctc0, ctc0]);
  const ctc8 = stdlib.T_Tuple([ctc0, ctc6, ctc0, ctc0, ctc6, ctc0, ctc2, ctc0]);
  const ctc9 = stdlib.T_Tuple([ctc0, ctc6, ctc0, ctc0, ctc6, ctc0, ctc0, ctc0]);
  const ctc10 = stdlib.T_Tuple([ctc0, ctc6, ctc0, ctc0, ctc6, ctc0, ctc0]);
  const ctc11 = stdlib.T_Tuple([ctc0, ctc6, ctc0, ctc0, ctc2, ctc6, ctc1, ctc0, ctc0]);
  const ctc12 = stdlib.T_Tuple([ctc0, ctc6, ctc0, ctc0, ctc2, ctc6, ctc1, ctc0]);
  const ctc13 = stdlib.T_Tuple([ctc0, ctc6, ctc0, ctc0, ctc2, ctc0, ctc0]);
  const ctc14 = stdlib.T_Tuple([ctc0, ctc6, ctc0, ctc0, ctc2, ctc0]);
  
  
  const v41 = await ctc.creationTime();
  const v40 = stdlib.protect(ctc1, interact.firstBatch, null);
  const txn1 = await (ctc.recv('Bob', 1, 3, [ctc0, ctc0, ctc2], false, false));
  const [v49, v50, v51] = txn1.data;
  const v52 = txn1.value;
  const v56 = txn1.time;
  const v48 = txn1.from;
  const v53 = stdlib.eq(v52, v49);
  stdlib.assert(v53, {
    at: './tut2/tut2.rsh:78:9:dot',
    fs: [],
    msg: 'pay amount correct',
    who: 'Bob'
     });
  stdlib.assert(true, {
    at: './tut2/tut2.rsh:78:9:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Bob'
     });
  const v55 = stdlib.add(stdlib.checkedBigNumberify('./tut2/tut2.rsh:compileDApp', stdlib.UInt_max, 0), v52);
  stdlib.protect(ctc3, await interact.acceptWager(v49), {
    at: './tut2/tut2.rsh:84:29:application',
    fs: ['at ./tut2/tut2.rsh:83:13:application call to [unknown function] (defined at: ./tut2/tut2.rsh:83:17:function exp)'],
    msg: 'acceptWager',
    who: 'Bob'
     });
  const txn2 = await (ctc.sendrecv('Bob', 2, 1, stdlib.checkedBigNumberify('./tut2/tut2.rsh:87:9:dot', stdlib.UInt_max, 5), [ctc6, ctc0, ctc0, ctc2, ctc0, ctc0, ctc1], [v48, v49, v50, v51, v55, v56, v40], v49, [ctc1], true, true, v50, (async (txn2) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.digest(ctc13, [stdlib.checkedBigNumberify('./tut2/tut2.rsh:87:9:dot', stdlib.UInt_max, 1), v48, v49, v50, v51, v55, v56]);
    sim_r.prevSt_noPrevTime = stdlib.digest(ctc14, [stdlib.checkedBigNumberify('./tut2/tut2.rsh:87:9:dot', stdlib.UInt_max, 1), v48, v49, v50, v51, v55]);
    const [v60] = txn2.data;
    const v61 = txn2.value;
    const v65 = txn2.time;
    const v59 = txn2.from;
    
    const v62 = stdlib.eq(v61, v49);
    stdlib.assert(v62, {
      at: './tut2/tut2.rsh:87:9:dot',
      fs: [],
      msg: 'pay amount correct',
      who: 'Bob'
       });
    stdlib.assert(true, {
      at: './tut2/tut2.rsh:87:9:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Bob'
       });
    const v64 = stdlib.add(v55, v61);
    sim_r.nextSt = stdlib.digest(ctc11, [stdlib.checkedBigNumberify('./tut2/tut2.rsh:90:15:after expr stmt semicolon', stdlib.UInt_max, 2), v48, v49, v50, v51, v59, v60, v64, v65]);
    sim_r.nextSt_noTime = stdlib.digest(ctc12, [stdlib.checkedBigNumberify('./tut2/tut2.rsh:90:15:after expr stmt semicolon', stdlib.UInt_max, 2), v48, v49, v50, v51, v59, v60, v64]);
    sim_r.isHalt = false;
    
    return sim_r;
     })));
  if (txn2.didTimeout) {
    const txn3 = await (ctc.recv('Bob', 3, 0, [], false, false));
    const [] = txn3.data;
    const v401 = txn3.value;
    const v406 = txn3.time;
    const v400 = txn3.from;
    const v402 = stdlib.eq(v401, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
    stdlib.assert(v402, {
      at: 'reach standard library:68:7:dot',
      fs: ['at ./tut2/tut2.rsh:89:41:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
      msg: 'pay amount correct',
      who: 'Bob'
       });
    const v403 = stdlib.addressEq(v48, v400);
    stdlib.assert(v403, {
      at: 'reach standard library:68:7:dot',
      fs: ['at ./tut2/tut2.rsh:89:41:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
      msg: 'sender correct',
      who: 'Bob'
       });
    const v405 = stdlib.add(v55, v401);
    ;
    stdlib.protect(ctc3, await interact.informTimeout(), {
      at: './tut2/tut2.rsh:68:33:application',
      fs: ['at ./tut2/tut2.rsh:67:13:application call to [unknown function] (defined at: ./tut2/tut2.rsh:67:25:function exp)', 'at reach standard library:71:8:application call to "after" (defined at: ./tut2/tut2.rsh:66:32:function exp)', 'at ./tut2/tut2.rsh:89:41:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
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
    const v62 = stdlib.eq(v61, v49);
    stdlib.assert(v62, {
      at: './tut2/tut2.rsh:87:9:dot',
      fs: [],
      msg: 'pay amount correct',
      who: 'Bob'
       });
    stdlib.assert(true, {
      at: './tut2/tut2.rsh:87:9:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Bob'
       });
    const v64 = stdlib.add(v55, v61);
    const txn3 = await (ctc.recv('Bob', 4, 2, [ctc0, ctc1], false, v50));
    if (txn3.didTimeout) {
      const txn4 = await (ctc.sendrecv('Bob', 5, 0, stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 7), [ctc6, ctc0, ctc0, ctc2, ctc6, ctc1, ctc0, ctc0], [v48, v49, v50, v51, v59, v60, v64, v65], stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), [], true, true, false, (async (txn4) => {
        const sim_r = { txns: [] };
        sim_r.prevSt = stdlib.digest(ctc11, [stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 2), v48, v49, v50, v51, v59, v60, v64, v65]);
        sim_r.prevSt_noPrevTime = stdlib.digest(ctc12, [stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 2), v48, v49, v50, v51, v59, v60, v64]);
        const [] = txn4.data;
        const v381 = txn4.value;
        const v386 = txn4.time;
        const v380 = txn4.from;
        
        const v382 = stdlib.eq(v381, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
        stdlib.assert(v382, {
          at: 'reach standard library:68:7:dot',
          fs: ['at ./tut2/tut2.rsh:96:40:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
          msg: 'pay amount correct',
          who: 'Bob'
           });
        const v383 = stdlib.addressEq(v59, v380);
        stdlib.assert(v383, {
          at: 'reach standard library:68:7:dot',
          fs: ['at ./tut2/tut2.rsh:96:40:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
          msg: 'sender correct',
          who: 'Bob'
           });
        const v385 = stdlib.add(v64, v381);
        sim_r.txns.push({
          amt: v385,
          to: v59
           });
        sim_r.nextSt = stdlib.digest(ctc5, []);
        sim_r.nextSt_noTime = stdlib.digest(ctc5, []);
        sim_r.isHalt = true;
        
        return sim_r;
         })));
      const [] = txn4.data;
      const v381 = txn4.value;
      const v386 = txn4.time;
      const v380 = txn4.from;
      const v382 = stdlib.eq(v381, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
      stdlib.assert(v382, {
        at: 'reach standard library:68:7:dot',
        fs: ['at ./tut2/tut2.rsh:96:40:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
        msg: 'pay amount correct',
        who: 'Bob'
         });
      const v383 = stdlib.addressEq(v59, v380);
      stdlib.assert(v383, {
        at: 'reach standard library:68:7:dot',
        fs: ['at ./tut2/tut2.rsh:96:40:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
        msg: 'sender correct',
        who: 'Bob'
         });
      const v385 = stdlib.add(v64, v381);
      ;
      stdlib.protect(ctc3, await interact.informTimeout(), {
        at: './tut2/tut2.rsh:68:33:application',
        fs: ['at ./tut2/tut2.rsh:67:13:application call to [unknown function] (defined at: ./tut2/tut2.rsh:67:25:function exp)', 'at reach standard library:71:8:application call to "after" (defined at: ./tut2/tut2.rsh:66:32:function exp)', 'at ./tut2/tut2.rsh:96:40:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
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
      const v72 = stdlib.eq(v71, stdlib.checkedBigNumberify('./tut2/tut2.rsh:decimal', stdlib.UInt_max, 0));
      stdlib.assert(v72, {
        at: './tut2/tut2.rsh:95:9:dot',
        fs: [],
        msg: 'pay amount correct',
        who: 'Bob'
         });
      const v73 = stdlib.addressEq(v48, v68);
      stdlib.assert(v73, {
        at: './tut2/tut2.rsh:95:9:dot',
        fs: [],
        msg: 'sender correct',
        who: 'Bob'
         });
      const v75 = stdlib.add(v64, v71);
      const v78 = stdlib.digest(ctc4, [v69, v70]);
      const v79 = stdlib.eq(v51, v78);
      stdlib.assert(v79, {
        at: 'reach standard library:65:17:application',
        fs: ['at ./tut2/tut2.rsh:97:22:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)'],
        msg: null,
        who: 'Bob'
         });
      const v96 = v70[stdlib.checkedBigNumberify('./tut2/tut2.rsh:39:18:array ref', stdlib.UInt_max, 0)];
      const v97 = v60[stdlib.checkedBigNumberify('./tut2/tut2.rsh:39:29:array ref', stdlib.UInt_max, 0)];
      const v99 = stdlib.sub(stdlib.checkedBigNumberify('./tut2/tut2.rsh:7:18:decimal', stdlib.UInt_max, 4), v97);
      const v100 = stdlib.add(v96, v99);
      const v101 = stdlib.mod(v100, stdlib.checkedBigNumberify('./tut2/tut2.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v105 = v70[stdlib.checkedBigNumberify('./tut2/tut2.rsh:39:18:array ref', stdlib.UInt_max, 1)];
      const v106 = v60[stdlib.checkedBigNumberify('./tut2/tut2.rsh:39:29:array ref', stdlib.UInt_max, 1)];
      const v108 = stdlib.sub(stdlib.checkedBigNumberify('./tut2/tut2.rsh:7:18:decimal', stdlib.UInt_max, 4), v106);
      const v109 = stdlib.add(v105, v108);
      const v110 = stdlib.mod(v109, stdlib.checkedBigNumberify('./tut2/tut2.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v114 = v70[stdlib.checkedBigNumberify('./tut2/tut2.rsh:39:18:array ref', stdlib.UInt_max, 2)];
      const v115 = v60[stdlib.checkedBigNumberify('./tut2/tut2.rsh:39:29:array ref', stdlib.UInt_max, 2)];
      const v117 = stdlib.sub(stdlib.checkedBigNumberify('./tut2/tut2.rsh:7:18:decimal', stdlib.UInt_max, 4), v115);
      const v118 = stdlib.add(v114, v117);
      const v119 = stdlib.mod(v118, stdlib.checkedBigNumberify('./tut2/tut2.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v123 = v70[stdlib.checkedBigNumberify('./tut2/tut2.rsh:39:18:array ref', stdlib.UInt_max, 3)];
      const v124 = v60[stdlib.checkedBigNumberify('./tut2/tut2.rsh:39:29:array ref', stdlib.UInt_max, 3)];
      const v126 = stdlib.sub(stdlib.checkedBigNumberify('./tut2/tut2.rsh:7:18:decimal', stdlib.UInt_max, 4), v124);
      const v127 = stdlib.add(v123, v126);
      const v128 = stdlib.mod(v127, stdlib.checkedBigNumberify('./tut2/tut2.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v132 = v70[stdlib.checkedBigNumberify('./tut2/tut2.rsh:39:18:array ref', stdlib.UInt_max, 4)];
      const v133 = v60[stdlib.checkedBigNumberify('./tut2/tut2.rsh:39:29:array ref', stdlib.UInt_max, 4)];
      const v135 = stdlib.sub(stdlib.checkedBigNumberify('./tut2/tut2.rsh:7:18:decimal', stdlib.UInt_max, 4), v133);
      const v136 = stdlib.add(v132, v135);
      const v137 = stdlib.mod(v136, stdlib.checkedBigNumberify('./tut2/tut2.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v150 = stdlib.eq(v101, stdlib.checkedBigNumberify('./tut2/tut2.rsh:makeEnum', stdlib.UInt_max, 1));
      const v151 = v150 ? v110 : v101;
      const v154 = stdlib.eq(v151, stdlib.checkedBigNumberify('./tut2/tut2.rsh:makeEnum', stdlib.UInt_max, 1));
      const v155 = v154 ? v119 : v151;
      const v158 = stdlib.eq(v155, stdlib.checkedBigNumberify('./tut2/tut2.rsh:makeEnum', stdlib.UInt_max, 1));
      const v159 = v158 ? v128 : v155;
      const v162 = stdlib.eq(v159, stdlib.checkedBigNumberify('./tut2/tut2.rsh:makeEnum', stdlib.UInt_max, 1));
      const v163 = v162 ? v137 : v159;
      let v166 = v163;
      let v419 = v75;
      let v420 = v76;
      
      while ((() => {
        const v176 = stdlib.eq(v166, stdlib.checkedBigNumberify('./tut2/tut2.rsh:makeEnum', stdlib.UInt_max, 1));
        
        return v176; })()) {
        const txn4 = await (ctc.recv('Bob', 8, 1, [ctc2], false, v50));
        if (txn4.didTimeout) {
          const txn5 = await (ctc.sendrecv('Bob', 9, 0, stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 6), [ctc6, ctc0, ctc0, ctc6, ctc0, ctc0, ctc0], [v48, v49, v50, v59, v163, v419, v420], stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), [], true, true, false, (async (txn5) => {
            const sim_r = { txns: [] };
            sim_r.prevSt = stdlib.digest(ctc9, [stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 6), v48, v49, v50, v59, v163, v419, v420]);
            sim_r.prevSt_noPrevTime = stdlib.digest(ctc10, [stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 6), v48, v49, v50, v59, v163, v419]);
            const [] = txn5.data;
            const v345 = txn5.value;
            const v350 = txn5.time;
            const v344 = txn5.from;
            
            const v346 = stdlib.eq(v345, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v346, {
              at: 'reach standard library:68:7:dot',
              fs: ['at ./tut2/tut2.rsh:109:43:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
              msg: 'pay amount correct',
              who: 'Bob'
               });
            const v347 = stdlib.addressEq(v59, v344);
            stdlib.assert(v347, {
              at: 'reach standard library:68:7:dot',
              fs: ['at ./tut2/tut2.rsh:109:43:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
              msg: 'sender correct',
              who: 'Bob'
               });
            const v349 = stdlib.add(v419, v345);
            sim_r.txns.push({
              amt: v349,
              to: v59
               });
            sim_r.nextSt = stdlib.digest(ctc5, []);
            sim_r.nextSt_noTime = stdlib.digest(ctc5, []);
            sim_r.isHalt = true;
            
            return sim_r;
             })));
          const [] = txn5.data;
          const v345 = txn5.value;
          const v350 = txn5.time;
          const v344 = txn5.from;
          const v346 = stdlib.eq(v345, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
          stdlib.assert(v346, {
            at: 'reach standard library:68:7:dot',
            fs: ['at ./tut2/tut2.rsh:109:43:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
            msg: 'pay amount correct',
            who: 'Bob'
             });
          const v347 = stdlib.addressEq(v59, v344);
          stdlib.assert(v347, {
            at: 'reach standard library:68:7:dot',
            fs: ['at ./tut2/tut2.rsh:109:43:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
            msg: 'sender correct',
            who: 'Bob'
             });
          const v349 = stdlib.add(v419, v345);
          ;
          stdlib.protect(ctc3, await interact.informTimeout(), {
            at: './tut2/tut2.rsh:68:33:application',
            fs: ['at ./tut2/tut2.rsh:67:13:application call to [unknown function] (defined at: ./tut2/tut2.rsh:67:25:function exp)', 'at reach standard library:71:8:application call to "after" (defined at: ./tut2/tut2.rsh:66:32:function exp)', 'at ./tut2/tut2.rsh:109:43:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
            msg: 'informTimeout',
            who: 'Bob'
             });
          return;
           }
        else {
          const [v185] = txn4.data;
          const v186 = txn4.value;
          const v191 = txn4.time;
          const v184 = txn4.from;
          const v187 = stdlib.eq(v186, stdlib.checkedBigNumberify('./tut2/tut2.rsh:decimal', stdlib.UInt_max, 0));
          stdlib.assert(v187, {
            at: './tut2/tut2.rsh:108:11:dot',
            fs: [],
            msg: 'pay amount correct',
            who: 'Bob'
             });
          const v188 = stdlib.addressEq(v48, v184);
          stdlib.assert(v188, {
            at: './tut2/tut2.rsh:108:11:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Bob'
             });
          const v190 = stdlib.add(v419, v186);
          const v194 = stdlib.protect(ctc1, await interact.getBatch(), {
            at: './tut2/tut2.rsh:114:54:application',
            fs: ['at ./tut2/tut2.rsh:113:15:application call to [unknown function] (defined at: ./tut2/tut2.rsh:113:19:function exp)'],
            msg: 'getBatch',
            who: 'Bob'
             });
          const txn5 = await (ctc.sendrecv('Bob', 10, 1, stdlib.checkedBigNumberify('./tut2/tut2.rsh:115:11:dot', stdlib.UInt_max, 7), [ctc6, ctc0, ctc0, ctc6, ctc0, ctc2, ctc0, ctc0, ctc1], [v48, v49, v50, v59, v163, v185, v190, v191, v194], stdlib.checkedBigNumberify('./tut2/tut2.rsh:decimal', stdlib.UInt_max, 0), [ctc1], true, true, v50, (async (txn5) => {
            const sim_r = { txns: [] };
            sim_r.prevSt = stdlib.digest(ctc7, [stdlib.checkedBigNumberify('./tut2/tut2.rsh:115:11:dot', stdlib.UInt_max, 8), v48, v49, v50, v59, v163, v185, v190, v191]);
            sim_r.prevSt_noPrevTime = stdlib.digest(ctc8, [stdlib.checkedBigNumberify('./tut2/tut2.rsh:115:11:dot', stdlib.UInt_max, 8), v48, v49, v50, v59, v163, v185, v190]);
            const [v196] = txn5.data;
            const v197 = txn5.value;
            const v202 = txn5.time;
            const v195 = txn5.from;
            
            const v198 = stdlib.eq(v197, stdlib.checkedBigNumberify('./tut2/tut2.rsh:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v198, {
              at: './tut2/tut2.rsh:115:11:dot',
              fs: [],
              msg: 'pay amount correct',
              who: 'Bob'
               });
            const v199 = stdlib.addressEq(v59, v195);
            stdlib.assert(v199, {
              at: './tut2/tut2.rsh:115:11:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Bob'
               });
            const v201 = stdlib.add(v190, v197);
            sim_r.nextSt = stdlib.digest(ctc7, [stdlib.checkedBigNumberify('./tut2/tut2.rsh:117:17:after expr stmt semicolon', stdlib.UInt_max, 10), v48, v49, v50, v59, v163, v185, v201, v202]);
            sim_r.nextSt_noTime = stdlib.digest(ctc8, [stdlib.checkedBigNumberify('./tut2/tut2.rsh:117:17:after expr stmt semicolon', stdlib.UInt_max, 10), v48, v49, v50, v59, v163, v185, v201]);
            sim_r.isHalt = false;
            
            return sim_r;
             })));
          if (txn5.didTimeout) {
            const txn6 = await (ctc.recv('Bob', 11, 0, [], false, false));
            const [] = txn6.data;
            const v325 = txn6.value;
            const v330 = txn6.time;
            const v324 = txn6.from;
            const v326 = stdlib.eq(v325, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v326, {
              at: 'reach standard library:68:7:dot',
              fs: ['at ./tut2/tut2.rsh:116:43:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
              msg: 'pay amount correct',
              who: 'Bob'
               });
            const v327 = stdlib.addressEq(v48, v324);
            stdlib.assert(v327, {
              at: 'reach standard library:68:7:dot',
              fs: ['at ./tut2/tut2.rsh:116:43:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
              msg: 'sender correct',
              who: 'Bob'
               });
            const v329 = stdlib.add(v190, v325);
            ;
            stdlib.protect(ctc3, await interact.informTimeout(), {
              at: './tut2/tut2.rsh:68:33:application',
              fs: ['at ./tut2/tut2.rsh:67:13:application call to [unknown function] (defined at: ./tut2/tut2.rsh:67:25:function exp)', 'at reach standard library:71:8:application call to "after" (defined at: ./tut2/tut2.rsh:66:32:function exp)', 'at ./tut2/tut2.rsh:116:43:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
              msg: 'informTimeout',
              who: 'Bob'
               });
            return;
             }
          else {
            const [v196] = txn5.data;
            const v197 = txn5.value;
            const v202 = txn5.time;
            const v195 = txn5.from;
            const v198 = stdlib.eq(v197, stdlib.checkedBigNumberify('./tut2/tut2.rsh:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v198, {
              at: './tut2/tut2.rsh:115:11:dot',
              fs: [],
              msg: 'pay amount correct',
              who: 'Bob'
               });
            const v199 = stdlib.addressEq(v59, v195);
            stdlib.assert(v199, {
              at: './tut2/tut2.rsh:115:11:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Bob'
               });
            const v201 = stdlib.add(v190, v197);
            const txn6 = await (ctc.recv('Bob', 12, 2, [ctc0, ctc1], false, v50));
            if (txn6.didTimeout) {
              const txn7 = await (ctc.sendrecv('Bob', 13, 0, stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 7), [ctc6, ctc0, ctc0, ctc6, ctc0, ctc2, ctc0, ctc0], [v48, v49, v50, v59, v163, v185, v201, v202], stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), [], true, true, false, (async (txn7) => {
                const sim_r = { txns: [] };
                sim_r.prevSt = stdlib.digest(ctc7, [stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 10), v48, v49, v50, v59, v163, v185, v201, v202]);
                sim_r.prevSt_noPrevTime = stdlib.digest(ctc8, [stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 10), v48, v49, v50, v59, v163, v185, v201]);
                const [] = txn7.data;
                const v305 = txn7.value;
                const v310 = txn7.time;
                const v304 = txn7.from;
                
                const v306 = stdlib.eq(v305, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
                stdlib.assert(v306, {
                  at: 'reach standard library:68:7:dot',
                  fs: ['at ./tut2/tut2.rsh:122:43:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
                  msg: 'pay amount correct',
                  who: 'Bob'
                   });
                const v307 = stdlib.addressEq(v59, v304);
                stdlib.assert(v307, {
                  at: 'reach standard library:68:7:dot',
                  fs: ['at ./tut2/tut2.rsh:122:43:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
                  msg: 'sender correct',
                  who: 'Bob'
                   });
                const v309 = stdlib.add(v201, v305);
                sim_r.txns.push({
                  amt: v309,
                  to: v59
                   });
                sim_r.nextSt = stdlib.digest(ctc5, []);
                sim_r.nextSt_noTime = stdlib.digest(ctc5, []);
                sim_r.isHalt = true;
                
                return sim_r;
                 })));
              const [] = txn7.data;
              const v305 = txn7.value;
              const v310 = txn7.time;
              const v304 = txn7.from;
              const v306 = stdlib.eq(v305, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
              stdlib.assert(v306, {
                at: 'reach standard library:68:7:dot',
                fs: ['at ./tut2/tut2.rsh:122:43:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
                msg: 'pay amount correct',
                who: 'Bob'
                 });
              const v307 = stdlib.addressEq(v59, v304);
              stdlib.assert(v307, {
                at: 'reach standard library:68:7:dot',
                fs: ['at ./tut2/tut2.rsh:122:43:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
                msg: 'sender correct',
                who: 'Bob'
                 });
              const v309 = stdlib.add(v201, v305);
              ;
              stdlib.protect(ctc3, await interact.informTimeout(), {
                at: './tut2/tut2.rsh:68:33:application',
                fs: ['at ./tut2/tut2.rsh:67:13:application call to [unknown function] (defined at: ./tut2/tut2.rsh:67:25:function exp)', 'at reach standard library:71:8:application call to "after" (defined at: ./tut2/tut2.rsh:66:32:function exp)', 'at ./tut2/tut2.rsh:122:43:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
                msg: 'informTimeout',
                who: 'Bob'
                 });
              return;
               }
            else {
              const [v206, v207] = txn6.data;
              const v208 = txn6.value;
              const v213 = txn6.time;
              const v205 = txn6.from;
              const v209 = stdlib.eq(v208, stdlib.checkedBigNumberify('./tut2/tut2.rsh:decimal', stdlib.UInt_max, 0));
              stdlib.assert(v209, {
                at: './tut2/tut2.rsh:121:11:dot',
                fs: [],
                msg: 'pay amount correct',
                who: 'Bob'
                 });
              const v210 = stdlib.addressEq(v48, v205);
              stdlib.assert(v210, {
                at: './tut2/tut2.rsh:121:11:dot',
                fs: [],
                msg: 'sender correct',
                who: 'Bob'
                 });
              const v212 = stdlib.add(v201, v208);
              const v215 = stdlib.digest(ctc4, [v206, v207]);
              const v216 = stdlib.eq(v185, v215);
              stdlib.assert(v216, {
                at: 'reach standard library:65:17:application',
                fs: ['at ./tut2/tut2.rsh:123:24:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)'],
                msg: null,
                who: 'Bob'
                 });
              const cv166 = v163;
              const cv419 = v212;
              const cv420 = v213;
              
              v166 = cv166;
              v419 = cv419;
              v420 = cv420;
              
              continue; }
             }
           }
         }
      const v363 = stdlib.eq(v166, stdlib.checkedBigNumberify('./tut2/tut2.rsh:makeEnum', stdlib.UInt_max, 2));
      const v366 = stdlib.mul(stdlib.checkedBigNumberify('./tut2/tut2.rsh:129:16:decimal', stdlib.UInt_max, 2), v49);
      const v368 = v363 ? v48 : v59;
      ;
      stdlib.protect(ctc3, await interact.seeOutcome(v166), {
        at: './tut2/tut2.rsh:133:28:application',
        fs: ['at ./tut2/tut2.rsh:132:11:application call to [unknown function] (defined at: ./tut2/tut2.rsh:132:23:function exp)'],
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
  stepargs: [0, 129, 209, 169, 289, 241, 0, 0, 209, 177, 249, 209, 257, 209],
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
// "./tut2/tut2.rsh:78:9:dot"
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
// "./tut2/tut2.rsh:78:9:dot"
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
// "./tut2/tut2.rsh:87:9:dot"
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
// "./tut2/tut2.rsh:87:9:dot"
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
// "[at ./tut2/tut2.rsh:89:41:application call to \"closeTo\" (defined at: reach standard library:67:8:function exp)]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "reach standard library:68:7:dot"
// "[at ./tut2/tut2.rsh:89:41:application call to \"closeTo\" (defined at: reach standard library:67:8:function exp)]"
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
// "./tut2/tut2.rsh:95:9:dot"
// "[]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "./tut2/tut2.rsh:95:9:dot"
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
// "[at ./tut2/tut2.rsh:97:22:application call to \"checkCommitment\" (defined at: reach standard library:64:8:function exp)]"
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
load 250
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
// "[at ./tut2/tut2.rsh:96:40:application call to \"closeTo\" (defined at: reach standard library:67:8:function exp)]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "reach standard library:68:7:dot"
// "[at ./tut2/tut2.rsh:96:40:application call to \"closeTo\" (defined at: reach standard library:67:8:function exp)]"
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
// "./tut2/tut2.rsh:108:11:dot"
// "[]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "./tut2/tut2.rsh:108:11:dot"
// "[]"
gtxna 0 ApplicationArgs 5
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
// "[at ./tut2/tut2.rsh:109:43:application call to \"closeTo\" (defined at: reach standard library:67:8:function exp)]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "reach standard library:68:7:dot"
// "[at ./tut2/tut2.rsh:109:43:application call to \"closeTo\" (defined at: reach standard library:67:8:function exp)]"
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
// "./tut2/tut2.rsh:115:11:dot"
// "[]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "./tut2/tut2.rsh:115:11:dot"
// "[]"
gtxna 0 ApplicationArgs 8
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
// "[at ./tut2/tut2.rsh:116:43:application call to \"closeTo\" (defined at: reach standard library:67:8:function exp)]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "reach standard library:68:7:dot"
// "[at ./tut2/tut2.rsh:116:43:application call to \"closeTo\" (defined at: reach standard library:67:8:function exp)]"
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
// Just "pay amount correct"
// "./tut2/tut2.rsh:121:11:dot"
// "[]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "./tut2/tut2.rsh:121:11:dot"
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
// "[at ./tut2/tut2.rsh:123:24:application call to \"checkCommitment\" (defined at: reach standard library:64:8:function exp)]"
gtxna 0 ApplicationArgs 10
gtxna 0 ApplicationArgs 12
gtxna 0 ApplicationArgs 13
concat
keccak256
==
assert
gtxna 0 ApplicationArgs 9
btoi
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
gtxna 0 ApplicationArgs 9
btoi
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
// Just "pay amount correct"
// "reach standard library:68:7:dot"
// "[at ./tut2/tut2.rsh:122:43:application call to \"closeTo\" (defined at: reach standard library:67:8:function exp)]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "reach standard library:68:7:dot"
// "[at ./tut2/tut2.rsh:122:43:application call to \"closeTo\" (defined at: reach standard library:67:8:function exp)]"
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
                "name": "v41",
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
                "name": "v51",
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
                "name": "v48",
                "type": "address"
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
                "internalType": "uint256",
                "name": "v163",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v185",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v190",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v191",
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
                "name": "v196",
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
                "name": "v48",
                "type": "address"
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
                "internalType": "uint256",
                "name": "v163",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v185",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v190",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v191",
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
                "name": "v48",
                "type": "address"
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
                "internalType": "uint256",
                "name": "v163",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v185",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v201",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v202",
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
                "name": "v206",
                "type": "uint256"
              },
              {
                "internalType": "uint256[5]",
                "name": "v207",
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
                "name": "v48",
                "type": "address"
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
                "internalType": "uint256",
                "name": "v163",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v185",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v201",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v202",
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
                "name": "v48",
                "type": "address"
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
                "name": "v51",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v55",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v56",
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
                "name": "v48",
                "type": "address"
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
                "name": "v51",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v55",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v56",
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
                "name": "v48",
                "type": "address"
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
                "name": "v51",
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
                "name": "v48",
                "type": "address"
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
                "name": "v51",
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
                "name": "v48",
                "type": "address"
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
                "internalType": "uint256",
                "name": "v163",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v419",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v420",
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
                "name": "v185",
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
                "name": "v48",
                "type": "address"
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
                "internalType": "uint256",
                "name": "v163",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v419",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v420",
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
                "name": "v41",
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
                "name": "v51",
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
                "name": "v48",
                "type": "address"
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
                "internalType": "uint256",
                "name": "v163",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v185",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v190",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v191",
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
                "name": "v196",
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
                "name": "v48",
                "type": "address"
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
                "internalType": "uint256",
                "name": "v163",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v185",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v190",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v191",
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
                "name": "v48",
                "type": "address"
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
                "internalType": "uint256",
                "name": "v163",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v185",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v201",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v202",
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
                "name": "v206",
                "type": "uint256"
              },
              {
                "internalType": "uint256[5]",
                "name": "v207",
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
                "name": "v48",
                "type": "address"
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
                "internalType": "uint256",
                "name": "v163",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v185",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v201",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v202",
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
                "name": "v48",
                "type": "address"
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
                "name": "v51",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v55",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v56",
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
                "name": "v48",
                "type": "address"
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
                "name": "v51",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v55",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v56",
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
                "name": "v48",
                "type": "address"
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
                "name": "v51",
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
                "name": "v48",
                "type": "address"
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
                "name": "v51",
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
                "name": "v48",
                "type": "address"
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
                "internalType": "uint256",
                "name": "v163",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v419",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v420",
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
                "name": "v185",
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
                "name": "v48",
                "type": "address"
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
                "internalType": "uint256",
                "name": "v163",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v419",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v420",
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
  Bytecode: `0x608060408190527f49ff028a829527a47ec6839c7147b484eccf5a2a94853eddac09cef44d9d4e9e90600090a16040805160208082018352438252825180820184526000808252925181528351808301849052905181850152835180820385018152606090910190935282519201919091209055611b5d806100826000396000f3fe6080604052600436106100a05760003560e01c8063497ccc6a11610064578063497ccc6a1461010d578063515d2731146101205780639d1d6c8a14610133578063b2dc9b3c14610146578063d10ac92214610159578063d22bdb581461016c576100a7565b8063253d71be146100ac5780632bf4f873146100c15780632bfd6934146100d457806338015b16146100e75780633df7a789146100fa576100a7565b366100a757005b600080fd5b6100bf6100ba36600461157b565b61017f565b005b6100bf6100cf3660046115e1565b6102a7565b6100bf6100e23660046115b3565b610448565b6100bf6100f53660046115b3565b61055f565b6100bf610108366004611597565b610670565b6100bf61011b366004611597565b610825565b6100bf61012e3660046115f2565b61093c565b6100bf6101413660046115cf565b610a8c565b6100bf61015436600461157b565b610c2d565b6100bf610167366004611569565b610de5565b6100bf61017a366004611604565b6110f6565b604051610193906002908390602001611a17565b6040516020818303038152906040528051906020012060001c600054146101b957600080fd5b600080556101d06040820135610160830135611aa3565b43101580156101dd575060015b6101e657600080fd5b34156101f157600080fd5b3361020260a0830160808401611548565b6001600160a01b03161461021557600080fd5b61022560a0820160808301611548565b6001600160a01b03166108fc61024034610140850135611aa3565b6040518115909202916000818181858888f19350505050158015610268573d6000803e3d6000fd5b507fc18ab0af979eec50e5539334a3b97c236f4df25f2c0886da35b2f866708b0b13816040516102989190611825565b60405180910390a16000805533ff5b60408051600060208201528235918101919091526060016040516020818303038152906040528051906020012060001c600054146102e457600080fd5b600080805560408051602081019091529081523460208301351461030757600080fd5b610312346000611aa3565b815260408051833581526020808501359082015283820135818301526060808501359082015290517f2bb570a5feee0f446e450005a048c78efd478914692e1f0be1009bac144b11709181900360800190a16103a66040518060c0016040528060006001600160a01b0316815260200160008152602001600081526020016000815260200160008152602001600081525090565b338152602083810135818301908152604080860135818501908152606080880135818701908152875160808089019182524360a0808b01918252875160019a81019a909a528a516001600160a01b0316978a019790975296519388019390935292519186019190915251918401919091525160c08301525160e0820152610100015b60408051601f198184030181529190528051602090910120600055505050565b60405161045c90600a908390602001611984565b6040516020818303038152906040528051906020012060001c6000541461048257600080fd5b60008055610498604082013560e0830135611aa3565b43101580156104a5575060015b6104ae57600080fd5b34156104b957600080fd5b336104ca6080830160608401611548565b6001600160a01b0316146104dd57600080fd5b6104ed6080820160608301611548565b6001600160a01b03166108fc6105073460c0850135611aa3565b6040518115909202916000818181858888f1935050505015801561052f573d6000803e3d6000fd5b507f69dd1b34aedf23239d695825c063b0ccab540698bc04080155ef5bbab022a1ba8160405161029891906118cc565b604051610573906008908390602001611984565b6040516020818303038152906040528051906020012060001c6000541461059957600080fd5b600080556105af604082013560e0830135611aa3565b43101580156105bc575060015b6105c557600080fd5b34156105d057600080fd5b336105de6020830183611548565b6001600160a01b0316146105f157600080fd5b6105fe6020820182611548565b6001600160a01b03166108fc6106183460c0850135611aa3565b6040518115909202916000818181858888f19350505050158015610640573d6000803e3d6000fd5b507fb9d135d4afaa7938b4616c3637968c0e71f66413b54043da6188989930963f898160405161029891906118cc565b60405161068490600690839060200161196f565b6040516020818303038152906040528051906020012060001c600054146106aa57600080fd5b6000808055604080516020810182529182526106cd9083013560c0840135611aa3565b43106106d857600080fd5b34156106e357600080fd5b336106f16020840184611548565b6001600160a01b03161461070457600080fd5b6107123460a0840135611aa3565b81526040517f6739bb4acbb3812eea51c48895245b154776bf02dd61571666f6abba93266fec90610744908490611858565b60405180910390a16107a660405180610100016040528060006001600160a01b03168152602001600081526020016000815260200160006001600160a01b03168152602001600081526020016000815260200160008152602001600081525090565b6107b36020840184611548565b6001600160a01b0316815260208084013590820152604080840135908201526107e26080840160608501611548565b6001600160a01b031660608201526080808401359082015260e08084013560a0830152825160c08301524390820152604051610428906008908390602001611999565b60405161083990600690839060200161196f565b6040516020818303038152906040528051906020012060001c6000541461085f57600080fd5b60008055610875604082013560c0830135611aa3565b4310158015610882575060015b61088b57600080fd5b341561089657600080fd5b336108a76080830160608401611548565b6001600160a01b0316146108ba57600080fd5b6108ca6080820160608301611548565b6001600160a01b03166108fc6108e43460a0850135611aa3565b6040518115909202916000818181858888f1935050505015801561090c573d6000803e3d6000fd5b507fcdae4cbd433f8c4039f23f2632824e3ab0089e9b8c2050e8e87e4d6e0a3df09b816040516102989190611875565b604051610950906001908390602001611a03565b6040516020818303038152906040528051906020012060001c6000541461097657600080fd5b6000808055604080516020810182529182526109999083013560a0840135611aa3565b43106109a457600080fd5b346020830135146109b457600080fd5b6109c2346080840135611aa3565b81526040517f128ceb6e462bfebf9ef10870b6d9ae608efb33f1cdefac45d94895b5f28af76e906109f4908490611901565b60405180910390a1610a04611415565b610a116020840184611548565b6001600160a01b03168152602080840135908201526040808401358183015260608085013590830152336080830152805160a08181019092529060c08501906005908390839080828437600092019190915250505060a0820152815160c08201524360e0820152604051610428906002908390602001611a2c565b604051610aa090600a908390602001611984565b6040516020818303038152906040528051906020012060001c60005414610ac657600080fd5b60008055610adc604082013560e0830135611aa3565b4310610ae757600080fd5b3415610af257600080fd5b33610b006020830183611548565b6001600160a01b031614610b1357600080fd5b604051610b2f9061010083013590610120840190602001611955565b60408051601f19818403018152919052805160209091012060a082013514610b5657600080fd5b7e5461e7e4d5391a041fd64bd44e15e016f9dae1ace2fc3ef579b196ede298e881604051610b8491906118e1565b60405180910390a1610b94611472565b610ba16020830183611548565b81516001600160a01b03909116905280516020808401359101528051604080840135910152610bd66080830160608401611548565b81516001600160a01b0390911660609091015280516080808401359101819052602082015152610c0a3460c0840135611aa3565b60208083018051909101919091525143604090910152610c2981611207565b5050565b604051610c41906008908390602001611984565b6040516020818303038152906040528051906020012060001c60005414610c6757600080fd5b600080805560408051602081018252918252610c8a9083013560e0840135611aa3565b4310610c9557600080fd5b3415610ca057600080fd5b33610cb16080840160608501611548565b6001600160a01b031614610cc457600080fd5b610cd23460c0840135611aa3565b81526040517f4b39e6355207f2c77bb98c443fb3e5e02a82c850a077da7e0938cd671b36448790610d049084906118a6565b60405180910390a1610d6660405180610100016040528060006001600160a01b03168152602001600081526020016000815260200160006001600160a01b03168152602001600081526020016000815260200160008152602001600081525090565b610d736020840184611548565b6001600160a01b031681526020808401359082015260408084013590820152610da26080840160608501611548565b6001600160a01b031660608201526080808401359082015260a08084013590820152815160c08201524360e082015260405161042890600a908390602001611999565b604051610df9906002908390602001611a17565b6040516020818303038152906040528051906020012060001c60005414610e1f57600080fd5b60008081905550610e586040518060a0016040528060008152602001600081526020016000815260200160008152602001600081525090565b610e6b6040830135610160840135611aa3565b4310610e7657600080fd5b3415610e8157600080fd5b33610e8f6020840184611548565b6001600160a01b031614610ea257600080fd5b604051610ebe90610180840135906101a0850190602001611955565b60408051601f198184030181529190528051602090910120606083013514610ee557600080fd5b6003610ef660a08401356004611ada565b610f05906101a0850135611aa3565b610f0f9190611af1565b808252600114610f20578051610f4a565b6003610f3160c08401356004611ada565b610f40906101c0850135611aa3565b610f4a9190611af1565b60208201819052600114610f62578060200151610f8c565b6003610f7360e08401356004611ada565b610f82906101e0850135611aa3565b610f8c9190611af1565b60408201819052600114610fa4578060400151610fcf565b6003610fb66101008401356004611ada565b610fc590610200850135611aa3565b610fcf9190611af1565b60608201819052600114610fe7578060600151611012565b6003610ff96101208401356004611ada565b61100890610220850135611aa3565b6110129190611af1565b60808201526040517f85b984f93e23bb278dcf4c426e3ebada3df03a830c17f29b763cc686eeff6030906110479084906117fe565b60405180910390a1611057611472565b6110646020840184611548565b81516001600160a01b0390911690528051602080850135910152805160408085013591015261109960a0840160808501611548565b81516001600160a01b0390911660609091015260808083018051835190920191909152516020820151526110d234610140850135611aa3565b602080830180519091019190915251436040909101526110f181611207565b505050565b60405161110a906001908390602001611a03565b6040516020818303038152906040528051906020012060001c6000541461113057600080fd5b60008055611146604082013560a0830135611aa3565b4310158015611153575060015b61115c57600080fd5b341561116757600080fd5b336111756020830183611548565b6001600160a01b03161461118857600080fd5b6111956020820182611548565b6001600160a01b03166108fc6111af346080850135611aa3565b6040518115909202916000818181858888f193505050501580156111d7573d6000803e3d6000fd5b507f17040e3ed853a8df776cd092f1357f15488d98d460f66cd5e6b0cb07d5bc8ae3816040516102989190611925565b6020810151516001141561133c576112676040518060e0016040528060006001600160a01b03168152602001600081526020016000815260200160006001600160a01b031681526020016000815260200160008152602001600081525090565b8151516001600160a01b03908116825282516020908101518184019081528451604090810151818601908152865160609081015186168188019081528851608090810151818a01908152878b01805189015160a0808d01918252915188015160c0808e01918252895160069c81019c909c528d518d16998c01999099529851958a0195909552945191880191909152905190961691850191909152935190830152915160e082015290516101008201526101200160408051601f198184030181529190528051602090910120600055506113a5565b6040805160c081018252600091810182815260608083018481526080840185815260a085018681528486526020808701979097528751516001600160a01b03908116909552875187015190925286519092015190921690529183015151909152610c29816113a8565b50565b8051606001516002146113c0578051604001516113c4565b8051515b6001600160a01b03166108fc82600001516020015160026113e59190611abb565b6040518115909202916000818181858888f1935050505015801561140d573d6000803e3d6000fd5b506000805533ff5b60405180610100016040528060006001600160a01b0316815260200160008152602001600081526020016000815260200160006001600160a01b0316815260200161145e6114d2565b815260200160008152602001600081525090565b6040805160e0810182526000918101828152606082018390526080820183905260a0820183905260c082019290925290819081526020016114cd60405180606001604052806000815260200160008152602001600081525090565b905290565b6040518060a001604052806005906020820280368337509192915050565b80356001600160a01b038116811461150757600080fd5b919050565b60006101a0828403121561151e578081fd5b50919050565b6000610100828403121561151e578081fd5b6000610120828403121561151e578081fd5b600060208284031215611559578081fd5b611562826114f0565b9392505050565b6000610240828403121561151e578081fd5b60006101a0828403121561158d578081fd5b611562838361150c565b600061010082840312156115a9578081fd5b6115628383611524565b600061012082840312156115c5578081fd5b6115628383611536565b60006101c0828403121561151e578081fd5b60006080828403121561151e578081fd5b6000610160828403121561151e578081fd5b600060e0828403121561151e578081fd5b8060005b6005811015611638578151845260209384019390910190600101611619565b50505050565b8035825260a0602082016020840137600060c08301525050565b6001600160a01b038061166a836114f0565b16835260208201356020840152604082013560408401528061168e606084016114f0565b166060840152506080810135608083015260a081013560a083015260c081013560c08301525050565b6001600160a01b03806116c9836114f0565b1683526020820135602084015260408201356040840152806116ed606084016114f0565b166060840152506080810135608083015260a081013560a083015260c081013560c083015260e081013560e08301525050565b6001600160a01b03611731826114f0565b1682526020810135602083015260408101356040830152606081013560608301526080810135608083015260a081013560a08301525050565b61177482826116b7565b6101008082013580151580821461178a57600080fd5b80838601525050505050565b6001600160a01b03806117a8836114f0565b168352602082013560208401526040820135604084015260608201356060840152806117d6608084016114f0565b1660808401525060a080820160a0840137610140818101359083015261016090810135910152565b610240810161180d8284611796565b61018061181e81840182860161163e565b5092915050565b6101a081016118348284611796565b6101808084013580151580821461184a57600080fd5b808386015250505092915050565b61010081016118678284611658565b60e092830135919092015290565b61010081016118848284611658565b60e083013580151580821461189857600080fd5b8060e0850152505092915050565b6101a081016118b582846116b7565b61010060a081850182850137506000815292915050565b61012081016118db828461176a565b92915050565b6101c081016118f082846116b7565b61010061181e81840182860161163e565b61016081016119108284611720565b60a060c0840160c08401376000815292915050565b60e081016119338284611720565b60c083013580151580821461194757600080fd5b8060c0850152505092915050565b82815260c0810160a0836020840137600081529392505050565b82815261010081016115626020830184611658565b828152610120810161156260208301846116b7565b8281526101208101611562602083018460018060a01b038082511683526020820151602084015260408201516040840152806060830151166060840152506080810151608083015260a081015160a083015260c081015160c083015260e081015160e08301525050565b82815260e081016115626020830184611720565b8281526101a081016115626020830184611796565b60006101a08201905083825260018060a01b038084511660208401526020840151604084015260408401516060840152606084015160808401528060808501511660a08401525060a0830151611a8560c0840182611615565b5060c083015161016083015260e08301516101808301529392505050565b60008219821115611ab657611ab6611b11565b500190565b6000816000190483118215151615611ad557611ad5611b11565b500290565b600082821015611aec57611aec611b11565b500390565b600082611b0c57634e487b7160e01b81526012600452602481fd5b500690565b634e487b7160e01b600052601160045260246000fdfea2646970667358221220547c9ff0a5810784b1d234bf4136255dfc1326072e5f2c361568b15062497d5764736f6c63430008020033`,
  deployMode: `DM_constructor`
   };

export const _Connectors = {
  ALGO: _ALGO,
  ETH: _ETH
   };

