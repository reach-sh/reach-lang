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
    at: './workshop.rsh:60:47:application',
    fs: ['at ./workshop.rsh:57:13:application call to [unknown function] (defined at: ./workshop.rsh:57:17:function exp)'],
    msg: 'getBatch',
    who: 'Alice'
     });
  const v44 = stdlib.protect(ctc0, await interact.random(), {
    at: 'reach standard library:60:31:application',
    fs: ['at ./workshop.rsh:61:74:application call to "makeCommitment" (defined at: reach standard library:59:8:function exp)', 'at ./workshop.rsh:57:13:application call to [unknown function] (defined at: ./workshop.rsh:57:17:function exp)'],
    msg: 'random',
    who: 'Alice'
     });
  const v45 = stdlib.digest(ctc2, [v44, v42]);
  const txn1 = await (ctc.sendrecv('Alice', 1, 3, stdlib.checkedBigNumberify('./workshop.rsh:65:9:dot', stdlib.UInt_max, 0), [ctc0, ctc0, ctc0, ctc3], [v39, v38, v37, v45], v38, [ctc0, ctc0, ctc3], true, true, false, (async (txn1) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.digest(ctc17, [stdlib.checkedBigNumberify('./workshop.rsh:65:9:dot', stdlib.UInt_max, 0), v39]);
    sim_r.prevSt_noPrevTime = stdlib.digest(ctc18, [stdlib.checkedBigNumberify('./workshop.rsh:65:9:dot', stdlib.UInt_max, 0)]);
    const [v48, v49, v50] = txn1.data;
    const v51 = txn1.value;
    const v55 = txn1.time;
    const v47 = txn1.from;
    
    const v52 = stdlib.eq(v51, v48);
    stdlib.assert(v52, {
      at: './workshop.rsh:65:9:dot',
      fs: [],
      msg: 'pay amount correct',
      who: 'Alice'
       });
    stdlib.assert(true, {
      at: './workshop.rsh:65:9:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Alice'
       });
    const v54 = stdlib.add(stdlib.checkedBigNumberify('./workshop.rsh:compileDApp', stdlib.UInt_max, 0), v51);
    sim_r.nextSt = stdlib.digest(ctc15, [stdlib.checkedBigNumberify('./workshop.rsh:67:15:after expr stmt semicolon', stdlib.UInt_max, 1), v47, v48, v49, v50, v54, v55]);
    sim_r.nextSt_noTime = stdlib.digest(ctc16, [stdlib.checkedBigNumberify('./workshop.rsh:67:15:after expr stmt semicolon', stdlib.UInt_max, 1), v47, v48, v49, v50, v54]);
    sim_r.isHalt = false;
    
    return sim_r;
     })));
  const [v48, v49, v50] = txn1.data;
  const v51 = txn1.value;
  const v55 = txn1.time;
  const v47 = txn1.from;
  const v52 = stdlib.eq(v51, v48);
  stdlib.assert(v52, {
    at: './workshop.rsh:65:9:dot',
    fs: [],
    msg: 'pay amount correct',
    who: 'Alice'
     });
  stdlib.assert(true, {
    at: './workshop.rsh:65:9:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Alice'
     });
  const v54 = stdlib.add(stdlib.checkedBigNumberify('./workshop.rsh:compileDApp', stdlib.UInt_max, 0), v51);
  const txn2 = await (ctc.recv('Alice', 2, 1, [ctc1], false, v49));
  if (txn2.didTimeout) {
    const txn3 = await (ctc.sendrecv('Alice', 3, 0, stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 5), [ctc6, ctc0, ctc0, ctc3, ctc0, ctc0], [v47, v48, v49, v50, v54, v55], stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), [], true, true, false, (async (txn3) => {
      const sim_r = { txns: [] };
      sim_r.prevSt = stdlib.digest(ctc15, [stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 1), v47, v48, v49, v50, v54, v55]);
      sim_r.prevSt_noPrevTime = stdlib.digest(ctc16, [stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 1), v47, v48, v49, v50, v54]);
      const [] = txn3.data;
      const v607 = txn3.value;
      const v612 = txn3.time;
      const v606 = txn3.from;
      
      const v608 = stdlib.eq(v607, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
      stdlib.assert(v608, {
        at: 'reach standard library:68:7:dot',
        fs: ['at ./workshop.rsh:76:41:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
        msg: 'pay amount correct',
        who: 'Alice'
         });
      const v609 = stdlib.addressEq(v47, v606);
      stdlib.assert(v609, {
        at: 'reach standard library:68:7:dot',
        fs: ['at ./workshop.rsh:76:41:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
        msg: 'sender correct',
        who: 'Alice'
         });
      const v611 = stdlib.add(v54, v607);
      sim_r.txns.push({
        amt: v611,
        to: v47
         });
      sim_r.nextSt = stdlib.digest(ctc5, []);
      sim_r.nextSt_noTime = stdlib.digest(ctc5, []);
      sim_r.isHalt = true;
      
      return sim_r;
       })));
    const [] = txn3.data;
    const v607 = txn3.value;
    const v612 = txn3.time;
    const v606 = txn3.from;
    const v608 = stdlib.eq(v607, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
    stdlib.assert(v608, {
      at: 'reach standard library:68:7:dot',
      fs: ['at ./workshop.rsh:76:41:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
      msg: 'pay amount correct',
      who: 'Alice'
       });
    const v609 = stdlib.addressEq(v47, v606);
    stdlib.assert(v609, {
      at: 'reach standard library:68:7:dot',
      fs: ['at ./workshop.rsh:76:41:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
      msg: 'sender correct',
      who: 'Alice'
       });
    const v611 = stdlib.add(v54, v607);
    ;
    stdlib.protect(ctc4, await interact.informTimeout(stdlib.checkedBigNumberify('./workshop.rsh:76:66:decimal', stdlib.UInt_max, 1)), {
      at: './workshop.rsh:55:33:application',
      fs: ['at ./workshop.rsh:54:13:application call to [unknown function] (defined at: ./workshop.rsh:54:25:function exp)', 'at ./workshop.rsh:76:65:application call to "informTimeout" (defined at: ./workshop.rsh:53:35:function exp)', 'at reach standard library:71:8:application call to "after" (defined at: ./workshop.rsh:76:48:function exp)', 'at ./workshop.rsh:76:41:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
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
      at: './workshop.rsh:74:9:dot',
      fs: [],
      msg: 'pay amount correct',
      who: 'Alice'
       });
    stdlib.assert(true, {
      at: './workshop.rsh:74:9:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Alice'
       });
    const v64 = stdlib.add(v54, v61);
    const txn3 = await (ctc.sendrecv('Alice', 4, 2, stdlib.checkedBigNumberify('./workshop.rsh:82:9:dot', stdlib.UInt_max, 7), [ctc6, ctc0, ctc0, ctc3, ctc6, ctc1, ctc0, ctc0, ctc0, ctc1], [v47, v48, v49, v50, v59, v60, v64, v65, v44, v42], stdlib.checkedBigNumberify('./workshop.rsh:decimal', stdlib.UInt_max, 0), [ctc0, ctc1], true, true, v49, (async (txn3) => {
      const sim_r = { txns: [] };
      sim_r.prevSt = stdlib.digest(ctc13, [stdlib.checkedBigNumberify('./workshop.rsh:82:9:dot', stdlib.UInt_max, 2), v47, v48, v49, v50, v59, v60, v64, v65]);
      sim_r.prevSt_noPrevTime = stdlib.digest(ctc14, [stdlib.checkedBigNumberify('./workshop.rsh:82:9:dot', stdlib.UInt_max, 2), v47, v48, v49, v50, v59, v60, v64]);
      const [v69, v70] = txn3.data;
      const v71 = txn3.value;
      const v76 = txn3.time;
      const v68 = txn3.from;
      
      const v72 = stdlib.eq(v71, stdlib.checkedBigNumberify('./workshop.rsh:decimal', stdlib.UInt_max, 0));
      stdlib.assert(v72, {
        at: './workshop.rsh:82:9:dot',
        fs: [],
        msg: 'pay amount correct',
        who: 'Alice'
         });
      const v73 = stdlib.addressEq(v47, v68);
      stdlib.assert(v73, {
        at: './workshop.rsh:82:9:dot',
        fs: [],
        msg: 'sender correct',
        who: 'Alice'
         });
      const v75 = stdlib.add(v64, v71);
      const v78 = stdlib.digest(ctc2, [v69, v70]);
      const v79 = stdlib.eq(v50, v78);
      stdlib.assert(v79, {
        at: 'reach standard library:65:17:application',
        fs: ['at ./workshop.rsh:84:22:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)'],
        msg: null,
        who: 'Alice'
         });
      const v82 = [stdlib.checkedBigNumberify('./workshop.rsh:27:13:application', stdlib.UInt_max, 0), stdlib.checkedBigNumberify('./workshop.rsh:27:13:application', stdlib.UInt_max, 1), stdlib.checkedBigNumberify('./workshop.rsh:27:13:application', stdlib.UInt_max, 2), stdlib.checkedBigNumberify('./workshop.rsh:27:13:application', stdlib.UInt_max, 3), stdlib.checkedBigNumberify('./workshop.rsh:27:13:application', stdlib.UInt_max, 4)];
      const v96 = v70[stdlib.checkedBigNumberify('./workshop.rsh:28:18:array ref', stdlib.UInt_max, 0)];
      const v97 = v60[stdlib.checkedBigNumberify('./workshop.rsh:28:29:array ref', stdlib.UInt_max, 0)];
      const v99 = stdlib.sub(stdlib.checkedBigNumberify('./workshop.rsh:7:18:decimal', stdlib.UInt_max, 4), v97);
      const v100 = stdlib.add(v96, v99);
      const v101 = stdlib.mod(v100, stdlib.checkedBigNumberify('./workshop.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v105 = v70[stdlib.checkedBigNumberify('./workshop.rsh:28:18:array ref', stdlib.UInt_max, 1)];
      const v106 = v60[stdlib.checkedBigNumberify('./workshop.rsh:28:29:array ref', stdlib.UInt_max, 1)];
      const v108 = stdlib.sub(stdlib.checkedBigNumberify('./workshop.rsh:7:18:decimal', stdlib.UInt_max, 4), v106);
      const v109 = stdlib.add(v105, v108);
      const v110 = stdlib.mod(v109, stdlib.checkedBigNumberify('./workshop.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v114 = v70[stdlib.checkedBigNumberify('./workshop.rsh:28:18:array ref', stdlib.UInt_max, 2)];
      const v115 = v60[stdlib.checkedBigNumberify('./workshop.rsh:28:29:array ref', stdlib.UInt_max, 2)];
      const v117 = stdlib.sub(stdlib.checkedBigNumberify('./workshop.rsh:7:18:decimal', stdlib.UInt_max, 4), v115);
      const v118 = stdlib.add(v114, v117);
      const v119 = stdlib.mod(v118, stdlib.checkedBigNumberify('./workshop.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v123 = v70[stdlib.checkedBigNumberify('./workshop.rsh:28:18:array ref', stdlib.UInt_max, 3)];
      const v124 = v60[stdlib.checkedBigNumberify('./workshop.rsh:28:29:array ref', stdlib.UInt_max, 3)];
      const v126 = stdlib.sub(stdlib.checkedBigNumberify('./workshop.rsh:7:18:decimal', stdlib.UInt_max, 4), v124);
      const v127 = stdlib.add(v123, v126);
      const v128 = stdlib.mod(v127, stdlib.checkedBigNumberify('./workshop.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v132 = v70[stdlib.checkedBigNumberify('./workshop.rsh:28:18:array ref', stdlib.UInt_max, 4)];
      const v133 = v60[stdlib.checkedBigNumberify('./workshop.rsh:28:29:array ref', stdlib.UInt_max, 4)];
      const v135 = stdlib.sub(stdlib.checkedBigNumberify('./workshop.rsh:7:18:decimal', stdlib.UInt_max, 4), v133);
      const v136 = stdlib.add(v132, v135);
      const v137 = stdlib.mod(v136, stdlib.checkedBigNumberify('./workshop.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v150 = stdlib.eq(v101, stdlib.checkedBigNumberify('./workshop.rsh:makeEnum', stdlib.UInt_max, 1));
      const v151 = v150 ? v110 : v101;
      const v154 = stdlib.eq(v151, stdlib.checkedBigNumberify('./workshop.rsh:makeEnum', stdlib.UInt_max, 1));
      const v155 = v154 ? v119 : v151;
      const v158 = stdlib.eq(v155, stdlib.checkedBigNumberify('./workshop.rsh:makeEnum', stdlib.UInt_max, 1));
      const v159 = v158 ? v128 : v155;
      const v162 = stdlib.eq(v159, stdlib.checkedBigNumberify('./workshop.rsh:makeEnum', stdlib.UInt_max, 1));
      const v163 = v162 ? v137 : v159;
      const v166 = v82;
      const v167 = v82;
      const v168 = v163;
      const v169 = stdlib.checkedBigNumberify('./workshop.rsh:86:86:decimal', stdlib.UInt_max, 0);
      const v626 = v75;
      const v627 = v76;
      
      if ((() => {
        const v181 = stdlib.eq(v168, stdlib.checkedBigNumberify('./workshop.rsh:makeEnum', stdlib.UInt_max, 1));
        
        return v181; })()) {
        const v182 = stdlib.mod(v169, stdlib.checkedBigNumberify('./workshop.rsh:115:21:decimal', stdlib.UInt_max, 2));
        const v183 = stdlib.eq(v182, stdlib.checkedBigNumberify('./workshop.rsh:115:26:decimal', stdlib.UInt_max, 0));
        if (v183) {
          sim_r.nextSt = stdlib.digest(ctc11, [stdlib.checkedBigNumberify('./workshop.rsh:90:19:after expr stmt semicolon', stdlib.UInt_max, 6), v47, v48, v49, v59, v169, v626, v627]);
          sim_r.nextSt_noTime = stdlib.digest(ctc12, [stdlib.checkedBigNumberify('./workshop.rsh:90:19:after expr stmt semicolon', stdlib.UInt_max, 6), v47, v48, v49, v59, v169, v626]);
          sim_r.isHalt = false;
           }
        else {
          sim_r.nextSt = stdlib.digest(ctc11, [stdlib.checkedBigNumberify('./workshop.rsh:90:19:after expr stmt semicolon', stdlib.UInt_max, 6), v47, v48, v49, v59, v169, v626, v627]);
          sim_r.nextSt_noTime = stdlib.digest(ctc12, [stdlib.checkedBigNumberify('./workshop.rsh:90:19:after expr stmt semicolon', stdlib.UInt_max, 6), v47, v48, v49, v59, v169, v626]);
          sim_r.isHalt = false;
           } }
      else {
        const v568 = stdlib.eq(v168, stdlib.checkedBigNumberify('./workshop.rsh:makeEnum', stdlib.UInt_max, 2));
        const v571 = stdlib.mul(stdlib.checkedBigNumberify('./workshop.rsh:127:16:decimal', stdlib.UInt_max, 2), v48);
        const v573 = v568 ? v47 : v59;
        sim_r.txns.push({
          amt: v571,
          to: v573
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
      const v586 = txn4.value;
      const v591 = txn4.time;
      const v585 = txn4.from;
      const v587 = stdlib.eq(v586, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
      stdlib.assert(v587, {
        at: 'reach standard library:68:7:dot',
        fs: ['at ./workshop.rsh:83:40:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
        msg: 'pay amount correct',
        who: 'Alice'
         });
      const v588 = stdlib.addressEq(v59, v585);
      stdlib.assert(v588, {
        at: 'reach standard library:68:7:dot',
        fs: ['at ./workshop.rsh:83:40:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
        msg: 'sender correct',
        who: 'Alice'
         });
      const v590 = stdlib.add(v64, v586);
      ;
      stdlib.protect(ctc4, await interact.informTimeout(stdlib.checkedBigNumberify('./workshop.rsh:83:65:decimal', stdlib.UInt_max, 0)), {
        at: './workshop.rsh:55:33:application',
        fs: ['at ./workshop.rsh:54:13:application call to [unknown function] (defined at: ./workshop.rsh:54:25:function exp)', 'at ./workshop.rsh:83:64:application call to "informTimeout" (defined at: ./workshop.rsh:53:35:function exp)', 'at reach standard library:71:8:application call to "after" (defined at: ./workshop.rsh:83:47:function exp)', 'at ./workshop.rsh:83:40:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
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
      const v72 = stdlib.eq(v71, stdlib.checkedBigNumberify('./workshop.rsh:decimal', stdlib.UInt_max, 0));
      stdlib.assert(v72, {
        at: './workshop.rsh:82:9:dot',
        fs: [],
        msg: 'pay amount correct',
        who: 'Alice'
         });
      const v73 = stdlib.addressEq(v47, v68);
      stdlib.assert(v73, {
        at: './workshop.rsh:82:9:dot',
        fs: [],
        msg: 'sender correct',
        who: 'Alice'
         });
      const v75 = stdlib.add(v64, v71);
      const v78 = stdlib.digest(ctc2, [v69, v70]);
      const v79 = stdlib.eq(v50, v78);
      stdlib.assert(v79, {
        at: 'reach standard library:65:17:application',
        fs: ['at ./workshop.rsh:84:22:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)'],
        msg: null,
        who: 'Alice'
         });
      const v82 = [stdlib.checkedBigNumberify('./workshop.rsh:27:13:application', stdlib.UInt_max, 0), stdlib.checkedBigNumberify('./workshop.rsh:27:13:application', stdlib.UInt_max, 1), stdlib.checkedBigNumberify('./workshop.rsh:27:13:application', stdlib.UInt_max, 2), stdlib.checkedBigNumberify('./workshop.rsh:27:13:application', stdlib.UInt_max, 3), stdlib.checkedBigNumberify('./workshop.rsh:27:13:application', stdlib.UInt_max, 4)];
      const v96 = v70[stdlib.checkedBigNumberify('./workshop.rsh:28:18:array ref', stdlib.UInt_max, 0)];
      const v97 = v60[stdlib.checkedBigNumberify('./workshop.rsh:28:29:array ref', stdlib.UInt_max, 0)];
      const v99 = stdlib.sub(stdlib.checkedBigNumberify('./workshop.rsh:7:18:decimal', stdlib.UInt_max, 4), v97);
      const v100 = stdlib.add(v96, v99);
      const v101 = stdlib.mod(v100, stdlib.checkedBigNumberify('./workshop.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v105 = v70[stdlib.checkedBigNumberify('./workshop.rsh:28:18:array ref', stdlib.UInt_max, 1)];
      const v106 = v60[stdlib.checkedBigNumberify('./workshop.rsh:28:29:array ref', stdlib.UInt_max, 1)];
      const v108 = stdlib.sub(stdlib.checkedBigNumberify('./workshop.rsh:7:18:decimal', stdlib.UInt_max, 4), v106);
      const v109 = stdlib.add(v105, v108);
      const v110 = stdlib.mod(v109, stdlib.checkedBigNumberify('./workshop.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v114 = v70[stdlib.checkedBigNumberify('./workshop.rsh:28:18:array ref', stdlib.UInt_max, 2)];
      const v115 = v60[stdlib.checkedBigNumberify('./workshop.rsh:28:29:array ref', stdlib.UInt_max, 2)];
      const v117 = stdlib.sub(stdlib.checkedBigNumberify('./workshop.rsh:7:18:decimal', stdlib.UInt_max, 4), v115);
      const v118 = stdlib.add(v114, v117);
      const v119 = stdlib.mod(v118, stdlib.checkedBigNumberify('./workshop.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v123 = v70[stdlib.checkedBigNumberify('./workshop.rsh:28:18:array ref', stdlib.UInt_max, 3)];
      const v124 = v60[stdlib.checkedBigNumberify('./workshop.rsh:28:29:array ref', stdlib.UInt_max, 3)];
      const v126 = stdlib.sub(stdlib.checkedBigNumberify('./workshop.rsh:7:18:decimal', stdlib.UInt_max, 4), v124);
      const v127 = stdlib.add(v123, v126);
      const v128 = stdlib.mod(v127, stdlib.checkedBigNumberify('./workshop.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v132 = v70[stdlib.checkedBigNumberify('./workshop.rsh:28:18:array ref', stdlib.UInt_max, 4)];
      const v133 = v60[stdlib.checkedBigNumberify('./workshop.rsh:28:29:array ref', stdlib.UInt_max, 4)];
      const v135 = stdlib.sub(stdlib.checkedBigNumberify('./workshop.rsh:7:18:decimal', stdlib.UInt_max, 4), v133);
      const v136 = stdlib.add(v132, v135);
      const v137 = stdlib.mod(v136, stdlib.checkedBigNumberify('./workshop.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v150 = stdlib.eq(v101, stdlib.checkedBigNumberify('./workshop.rsh:makeEnum', stdlib.UInt_max, 1));
      const v151 = v150 ? v110 : v101;
      const v154 = stdlib.eq(v151, stdlib.checkedBigNumberify('./workshop.rsh:makeEnum', stdlib.UInt_max, 1));
      const v155 = v154 ? v119 : v151;
      const v158 = stdlib.eq(v155, stdlib.checkedBigNumberify('./workshop.rsh:makeEnum', stdlib.UInt_max, 1));
      const v159 = v158 ? v128 : v155;
      const v162 = stdlib.eq(v159, stdlib.checkedBigNumberify('./workshop.rsh:makeEnum', stdlib.UInt_max, 1));
      const v163 = v162 ? v137 : v159;
      let v166 = v82;
      let v167 = v82;
      let v168 = v163;
      let v169 = stdlib.checkedBigNumberify('./workshop.rsh:86:86:decimal', stdlib.UInt_max, 0);
      let v626 = v75;
      let v627 = v76;
      
      while ((() => {
        const v181 = stdlib.eq(v168, stdlib.checkedBigNumberify('./workshop.rsh:makeEnum', stdlib.UInt_max, 1));
        
        return v181; })()) {
        const v182 = stdlib.mod(v169, stdlib.checkedBigNumberify('./workshop.rsh:115:21:decimal', stdlib.UInt_max, 2));
        const v183 = stdlib.eq(v182, stdlib.checkedBigNumberify('./workshop.rsh:115:26:decimal', stdlib.UInt_max, 0));
        if (v183) {
          const txn4 = await (ctc.recv('Alice', 8, 1, [ctc3], false, v49));
          if (txn4.didTimeout) {
            const txn5 = await (ctc.sendrecv('Alice', 9, 0, stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 6), [ctc6, ctc0, ctc0, ctc6, ctc0, ctc0, ctc0], [v47, v48, v49, v59, v169, v626, v627], stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), [], true, true, false, (async (txn5) => {
              const sim_r = { txns: [] };
              sim_r.prevSt = stdlib.digest(ctc11, [stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 6), v47, v48, v49, v59, v169, v626, v627]);
              sim_r.prevSt_noPrevTime = stdlib.digest(ctc12, [stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 6), v47, v48, v49, v59, v169, v626]);
              const [] = txn5.data;
              const v271 = txn5.value;
              const v276 = txn5.time;
              const v270 = txn5.from;
              
              const v272 = stdlib.eq(v271, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
              stdlib.assert(v272, {
                at: 'reach standard library:68:7:dot',
                fs: ['at ./workshop.rsh:97:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./workshop.rsh:116:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                msg: 'pay amount correct',
                who: 'Alice'
                 });
              const v273 = stdlib.addressEq(v47, v270);
              stdlib.assert(v273, {
                at: 'reach standard library:68:7:dot',
                fs: ['at ./workshop.rsh:97:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./workshop.rsh:116:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                msg: 'sender correct',
                who: 'Alice'
                 });
              const v275 = stdlib.add(v626, v271);
              sim_r.txns.push({
                amt: v275,
                to: v47
                 });
              sim_r.nextSt = stdlib.digest(ctc5, []);
              sim_r.nextSt_noTime = stdlib.digest(ctc5, []);
              sim_r.isHalt = true;
              
              return sim_r;
               })));
            const [] = txn5.data;
            const v271 = txn5.value;
            const v276 = txn5.time;
            const v270 = txn5.from;
            const v272 = stdlib.eq(v271, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v272, {
              at: 'reach standard library:68:7:dot',
              fs: ['at ./workshop.rsh:97:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./workshop.rsh:116:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
              msg: 'pay amount correct',
              who: 'Alice'
               });
            const v273 = stdlib.addressEq(v47, v270);
            stdlib.assert(v273, {
              at: 'reach standard library:68:7:dot',
              fs: ['at ./workshop.rsh:97:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./workshop.rsh:116:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
              msg: 'sender correct',
              who: 'Alice'
               });
            const v275 = stdlib.add(v626, v271);
            ;
            stdlib.protect(ctc4, await interact.informTimeout(stdlib.checkedBigNumberify('./workshop.rsh:116:47:decimal', stdlib.UInt_max, 1)), {
              at: './workshop.rsh:55:33:application',
              fs: ['at ./workshop.rsh:54:13:application call to [unknown function] (defined at: ./workshop.rsh:54:25:function exp)', 'at ./workshop.rsh:97:74:application call to "informTimeout" (defined at: ./workshop.rsh:53:35:function exp)', 'at reach standard library:71:8:application call to "after" (defined at: ./workshop.rsh:97:57:function exp)', 'at ./workshop.rsh:97:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./workshop.rsh:116:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
              msg: 'informTimeout',
              who: 'Alice'
               });
            return;
             }
          else {
            const [v193] = txn4.data;
            const v194 = txn4.value;
            const v199 = txn4.time;
            const v192 = txn4.from;
            const v195 = stdlib.eq(v194, stdlib.checkedBigNumberify('./workshop.rsh:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v195, {
              at: './workshop.rsh:96:17:dot',
              fs: ['at ./workshop.rsh:116:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
              msg: 'pay amount correct',
              who: 'Alice'
               });
            const v196 = stdlib.addressEq(v59, v192);
            stdlib.assert(v196, {
              at: './workshop.rsh:96:17:dot',
              fs: ['at ./workshop.rsh:116:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
              msg: 'sender correct',
              who: 'Alice'
               });
            const v198 = stdlib.add(v626, v194);
            const v202 = stdlib.protect(ctc1, await interact.getBatch(), {
              at: './workshop.rsh:102:61:application',
              fs: ['at ./workshop.rsh:101:22:application call to [unknown function] (defined at: ./workshop.rsh:101:26:function exp)', 'at ./workshop.rsh:116:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
              msg: 'getBatch',
              who: 'Alice'
               });
            const txn5 = await (ctc.sendrecv('Alice', 10, 1, stdlib.checkedBigNumberify('./workshop.rsh:103:18:dot', stdlib.UInt_max, 7), [ctc6, ctc0, ctc0, ctc6, ctc0, ctc3, ctc0, ctc0, ctc1], [v47, v48, v49, v59, v169, v193, v198, v199, v202], stdlib.checkedBigNumberify('./workshop.rsh:decimal', stdlib.UInt_max, 0), [ctc1], true, true, v49, (async (txn5) => {
              const sim_r = { txns: [] };
              sim_r.prevSt = stdlib.digest(ctc9, [stdlib.checkedBigNumberify('./workshop.rsh:103:18:dot', stdlib.UInt_max, 8), v47, v48, v49, v59, v169, v193, v198, v199]);
              sim_r.prevSt_noPrevTime = stdlib.digest(ctc10, [stdlib.checkedBigNumberify('./workshop.rsh:103:18:dot', stdlib.UInt_max, 8), v47, v48, v49, v59, v169, v193, v198]);
              const [v204] = txn5.data;
              const v205 = txn5.value;
              const v210 = txn5.time;
              const v203 = txn5.from;
              
              const v206 = stdlib.eq(v205, stdlib.checkedBigNumberify('./workshop.rsh:decimal', stdlib.UInt_max, 0));
              stdlib.assert(v206, {
                at: './workshop.rsh:103:18:dot',
                fs: ['at ./workshop.rsh:116:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                msg: 'pay amount correct',
                who: 'Alice'
                 });
              const v207 = stdlib.addressEq(v47, v203);
              stdlib.assert(v207, {
                at: './workshop.rsh:103:18:dot',
                fs: ['at ./workshop.rsh:116:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                msg: 'sender correct',
                who: 'Alice'
                 });
              const v209 = stdlib.add(v198, v205);
              sim_r.nextSt = stdlib.digest(ctc7, [stdlib.checkedBigNumberify('./workshop.rsh:105:19:after expr stmt semicolon', stdlib.UInt_max, 10), v47, v48, v49, v59, v169, v193, v204, v209, v210]);
              sim_r.nextSt_noTime = stdlib.digest(ctc8, [stdlib.checkedBigNumberify('./workshop.rsh:105:19:after expr stmt semicolon', stdlib.UInt_max, 10), v47, v48, v49, v59, v169, v193, v204, v209]);
              sim_r.isHalt = false;
              
              return sim_r;
               })));
            if (txn5.didTimeout) {
              const txn6 = await (ctc.recv('Alice', 11, 0, [], false, false));
              const [] = txn6.data;
              const v250 = txn6.value;
              const v255 = txn6.time;
              const v249 = txn6.from;
              const v251 = stdlib.eq(v250, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
              stdlib.assert(v251, {
                at: 'reach standard library:68:7:dot',
                fs: ['at ./workshop.rsh:104:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./workshop.rsh:116:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                msg: 'pay amount correct',
                who: 'Alice'
                 });
              const v252 = stdlib.addressEq(v59, v249);
              stdlib.assert(v252, {
                at: 'reach standard library:68:7:dot',
                fs: ['at ./workshop.rsh:104:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./workshop.rsh:116:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                msg: 'sender correct',
                who: 'Alice'
                 });
              const v254 = stdlib.add(v198, v250);
              ;
              stdlib.protect(ctc4, await interact.informTimeout(stdlib.checkedBigNumberify('./workshop.rsh:116:49:decimal', stdlib.UInt_max, 0)), {
                at: './workshop.rsh:55:33:application',
                fs: ['at ./workshop.rsh:54:13:application call to [unknown function] (defined at: ./workshop.rsh:54:25:function exp)', 'at ./workshop.rsh:104:73:application call to "informTimeout" (defined at: ./workshop.rsh:53:35:function exp)', 'at reach standard library:71:8:application call to "after" (defined at: ./workshop.rsh:104:56:function exp)', 'at ./workshop.rsh:104:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./workshop.rsh:116:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                msg: 'informTimeout',
                who: 'Alice'
                 });
              return;
               }
            else {
              const [v204] = txn5.data;
              const v205 = txn5.value;
              const v210 = txn5.time;
              const v203 = txn5.from;
              const v206 = stdlib.eq(v205, stdlib.checkedBigNumberify('./workshop.rsh:decimal', stdlib.UInt_max, 0));
              stdlib.assert(v206, {
                at: './workshop.rsh:103:18:dot',
                fs: ['at ./workshop.rsh:116:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                msg: 'pay amount correct',
                who: 'Alice'
                 });
              const v207 = stdlib.addressEq(v47, v203);
              stdlib.assert(v207, {
                at: './workshop.rsh:103:18:dot',
                fs: ['at ./workshop.rsh:116:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                msg: 'sender correct',
                who: 'Alice'
                 });
              const v209 = stdlib.add(v198, v205);
              const txn6 = await (ctc.recv('Alice', 12, 2, [ctc0, ctc1], false, v49));
              if (txn6.didTimeout) {
                const txn7 = await (ctc.sendrecv('Alice', 13, 0, stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 8), [ctc6, ctc0, ctc0, ctc6, ctc0, ctc3, ctc1, ctc0, ctc0], [v47, v48, v49, v59, v169, v193, v204, v209, v210], stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), [], true, true, false, (async (txn7) => {
                  const sim_r = { txns: [] };
                  sim_r.prevSt = stdlib.digest(ctc7, [stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 10), v47, v48, v49, v59, v169, v193, v204, v209, v210]);
                  sim_r.prevSt_noPrevTime = stdlib.digest(ctc8, [stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 10), v47, v48, v49, v59, v169, v193, v204, v209]);
                  const [] = txn7.data;
                  const v229 = txn7.value;
                  const v234 = txn7.time;
                  const v228 = txn7.from;
                  
                  const v230 = stdlib.eq(v229, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
                  stdlib.assert(v230, {
                    at: 'reach standard library:68:7:dot',
                    fs: ['at ./workshop.rsh:110:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./workshop.rsh:116:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                    msg: 'pay amount correct',
                    who: 'Alice'
                     });
                  const v231 = stdlib.addressEq(v47, v228);
                  stdlib.assert(v231, {
                    at: 'reach standard library:68:7:dot',
                    fs: ['at ./workshop.rsh:110:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./workshop.rsh:116:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                    msg: 'sender correct',
                    who: 'Alice'
                     });
                  const v233 = stdlib.add(v209, v229);
                  sim_r.txns.push({
                    amt: v233,
                    to: v47
                     });
                  sim_r.nextSt = stdlib.digest(ctc5, []);
                  sim_r.nextSt_noTime = stdlib.digest(ctc5, []);
                  sim_r.isHalt = true;
                  
                  return sim_r;
                   })));
                const [] = txn7.data;
                const v229 = txn7.value;
                const v234 = txn7.time;
                const v228 = txn7.from;
                const v230 = stdlib.eq(v229, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
                stdlib.assert(v230, {
                  at: 'reach standard library:68:7:dot',
                  fs: ['at ./workshop.rsh:110:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./workshop.rsh:116:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                  msg: 'pay amount correct',
                  who: 'Alice'
                   });
                const v231 = stdlib.addressEq(v47, v228);
                stdlib.assert(v231, {
                  at: 'reach standard library:68:7:dot',
                  fs: ['at ./workshop.rsh:110:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./workshop.rsh:116:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                  msg: 'sender correct',
                  who: 'Alice'
                   });
                const v233 = stdlib.add(v209, v229);
                ;
                stdlib.protect(ctc4, await interact.informTimeout(stdlib.checkedBigNumberify('./workshop.rsh:116:47:decimal', stdlib.UInt_max, 1)), {
                  at: './workshop.rsh:55:33:application',
                  fs: ['at ./workshop.rsh:54:13:application call to [unknown function] (defined at: ./workshop.rsh:54:25:function exp)', 'at ./workshop.rsh:110:74:application call to "informTimeout" (defined at: ./workshop.rsh:53:35:function exp)', 'at reach standard library:71:8:application call to "after" (defined at: ./workshop.rsh:110:57:function exp)', 'at ./workshop.rsh:110:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./workshop.rsh:116:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                  msg: 'informTimeout',
                  who: 'Alice'
                   });
                return;
                 }
              else {
                const [v214, v215] = txn6.data;
                const v216 = txn6.value;
                const v221 = txn6.time;
                const v213 = txn6.from;
                const v217 = stdlib.eq(v216, stdlib.checkedBigNumberify('./workshop.rsh:decimal', stdlib.UInt_max, 0));
                stdlib.assert(v217, {
                  at: './workshop.rsh:109:17:dot',
                  fs: ['at ./workshop.rsh:116:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                  msg: 'pay amount correct',
                  who: 'Alice'
                   });
                const v218 = stdlib.addressEq(v59, v213);
                stdlib.assert(v218, {
                  at: './workshop.rsh:109:17:dot',
                  fs: ['at ./workshop.rsh:116:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                  msg: 'sender correct',
                  who: 'Alice'
                   });
                const v220 = stdlib.add(v209, v216);
                const v223 = stdlib.digest(ctc2, [v214, v215]);
                const v224 = stdlib.eq(v193, v223);
                stdlib.assert(v224, {
                  at: 'reach standard library:65:17:application',
                  fs: ['at ./workshop.rsh:111:26:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)', 'at ./workshop.rsh:116:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                  msg: null,
                  who: 'Alice'
                   });
                const v305 = v204[stdlib.checkedBigNumberify('./workshop.rsh:28:18:array ref', stdlib.UInt_max, 0)];
                const v306 = v215[stdlib.checkedBigNumberify('./workshop.rsh:28:29:array ref', stdlib.UInt_max, 0)];
                const v308 = stdlib.sub(stdlib.checkedBigNumberify('./workshop.rsh:7:18:decimal', stdlib.UInt_max, 4), v306);
                const v309 = stdlib.add(v305, v308);
                const v310 = stdlib.mod(v309, stdlib.checkedBigNumberify('./workshop.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v314 = v204[stdlib.checkedBigNumberify('./workshop.rsh:28:18:array ref', stdlib.UInt_max, 1)];
                const v315 = v215[stdlib.checkedBigNumberify('./workshop.rsh:28:29:array ref', stdlib.UInt_max, 1)];
                const v317 = stdlib.sub(stdlib.checkedBigNumberify('./workshop.rsh:7:18:decimal', stdlib.UInt_max, 4), v315);
                const v318 = stdlib.add(v314, v317);
                const v319 = stdlib.mod(v318, stdlib.checkedBigNumberify('./workshop.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v323 = v204[stdlib.checkedBigNumberify('./workshop.rsh:28:18:array ref', stdlib.UInt_max, 2)];
                const v324 = v215[stdlib.checkedBigNumberify('./workshop.rsh:28:29:array ref', stdlib.UInt_max, 2)];
                const v326 = stdlib.sub(stdlib.checkedBigNumberify('./workshop.rsh:7:18:decimal', stdlib.UInt_max, 4), v324);
                const v327 = stdlib.add(v323, v326);
                const v328 = stdlib.mod(v327, stdlib.checkedBigNumberify('./workshop.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v332 = v204[stdlib.checkedBigNumberify('./workshop.rsh:28:18:array ref', stdlib.UInt_max, 3)];
                const v333 = v215[stdlib.checkedBigNumberify('./workshop.rsh:28:29:array ref', stdlib.UInt_max, 3)];
                const v335 = stdlib.sub(stdlib.checkedBigNumberify('./workshop.rsh:7:18:decimal', stdlib.UInt_max, 4), v333);
                const v336 = stdlib.add(v332, v335);
                const v337 = stdlib.mod(v336, stdlib.checkedBigNumberify('./workshop.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v341 = v204[stdlib.checkedBigNumberify('./workshop.rsh:28:18:array ref', stdlib.UInt_max, 4)];
                const v342 = v215[stdlib.checkedBigNumberify('./workshop.rsh:28:29:array ref', stdlib.UInt_max, 4)];
                const v344 = stdlib.sub(stdlib.checkedBigNumberify('./workshop.rsh:7:18:decimal', stdlib.UInt_max, 4), v342);
                const v345 = stdlib.add(v341, v344);
                const v346 = stdlib.mod(v345, stdlib.checkedBigNumberify('./workshop.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v359 = stdlib.eq(v310, stdlib.checkedBigNumberify('./workshop.rsh:makeEnum', stdlib.UInt_max, 1));
                const v360 = v359 ? v319 : v310;
                const v363 = stdlib.eq(v360, stdlib.checkedBigNumberify('./workshop.rsh:makeEnum', stdlib.UInt_max, 1));
                const v364 = v363 ? v328 : v360;
                const v367 = stdlib.eq(v364, stdlib.checkedBigNumberify('./workshop.rsh:makeEnum', stdlib.UInt_max, 1));
                const v368 = v367 ? v337 : v364;
                const v371 = stdlib.eq(v368, stdlib.checkedBigNumberify('./workshop.rsh:makeEnum', stdlib.UInt_max, 1));
                const v372 = v371 ? v346 : v368;
                const v375 = stdlib.add(v169, stdlib.checkedBigNumberify('./workshop.rsh:117:83:decimal', stdlib.UInt_max, 1));
                const cv166 = v204;
                const cv167 = v215;
                const cv168 = v372;
                const cv169 = v375;
                const cv626 = v220;
                const cv627 = v221;
                
                v166 = cv166;
                v167 = cv167;
                v168 = cv168;
                v169 = cv169;
                v626 = cv626;
                v627 = cv627;
                
                continue; }
               }
             }
           }
        else {
          const v379 = stdlib.protect(ctc1, await interact.getBatch(), {
            at: './workshop.rsh:93:50:application',
            fs: ['at ./workshop.rsh:92:21:application call to [unknown function] (defined at: ./workshop.rsh:92:25:function exp)', 'at ./workshop.rsh:120:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
            msg: 'getBatch',
            who: 'Alice'
             });
          const v381 = stdlib.protect(ctc0, await interact.random(), {
            at: 'reach standard library:60:31:application',
            fs: ['at ./workshop.rsh:94:62:application call to "makeCommitment" (defined at: reach standard library:59:8:function exp)', 'at ./workshop.rsh:92:21:application call to [unknown function] (defined at: ./workshop.rsh:92:25:function exp)', 'at ./workshop.rsh:120:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
            msg: 'random',
            who: 'Alice'
             });
          const v382 = stdlib.digest(ctc2, [v381, v379]);
          const txn4 = await (ctc.sendrecv('Alice', 14, 1, stdlib.checkedBigNumberify('./workshop.rsh:96:17:dot', stdlib.UInt_max, 6), [ctc6, ctc0, ctc0, ctc6, ctc0, ctc0, ctc0, ctc3], [v47, v48, v49, v59, v169, v626, v627, v382], stdlib.checkedBigNumberify('./workshop.rsh:decimal', stdlib.UInt_max, 0), [ctc3], true, true, v49, (async (txn4) => {
            const sim_r = { txns: [] };
            sim_r.prevSt = stdlib.digest(ctc11, [stdlib.checkedBigNumberify('./workshop.rsh:96:17:dot', stdlib.UInt_max, 6), v47, v48, v49, v59, v169, v626, v627]);
            sim_r.prevSt_noPrevTime = stdlib.digest(ctc12, [stdlib.checkedBigNumberify('./workshop.rsh:96:17:dot', stdlib.UInt_max, 6), v47, v48, v49, v59, v169, v626]);
            const [v385] = txn4.data;
            const v386 = txn4.value;
            const v391 = txn4.time;
            const v384 = txn4.from;
            
            const v387 = stdlib.eq(v386, stdlib.checkedBigNumberify('./workshop.rsh:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v387, {
              at: './workshop.rsh:96:17:dot',
              fs: ['at ./workshop.rsh:120:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
              msg: 'pay amount correct',
              who: 'Alice'
               });
            const v388 = stdlib.addressEq(v47, v384);
            stdlib.assert(v388, {
              at: './workshop.rsh:96:17:dot',
              fs: ['at ./workshop.rsh:120:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
              msg: 'sender correct',
              who: 'Alice'
               });
            const v390 = stdlib.add(v626, v386);
            sim_r.nextSt = stdlib.digest(ctc9, [stdlib.checkedBigNumberify('./workshop.rsh:98:19:after expr stmt semicolon', stdlib.UInt_max, 14), v47, v48, v49, v59, v169, v385, v390, v391]);
            sim_r.nextSt_noTime = stdlib.digest(ctc10, [stdlib.checkedBigNumberify('./workshop.rsh:98:19:after expr stmt semicolon', stdlib.UInt_max, 14), v47, v48, v49, v59, v169, v385, v390]);
            sim_r.isHalt = false;
            
            return sim_r;
             })));
          if (txn4.didTimeout) {
            const txn5 = await (ctc.recv('Alice', 15, 0, [], false, false));
            const [] = txn5.data;
            const v463 = txn5.value;
            const v468 = txn5.time;
            const v462 = txn5.from;
            const v464 = stdlib.eq(v463, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v464, {
              at: 'reach standard library:68:7:dot',
              fs: ['at ./workshop.rsh:97:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./workshop.rsh:120:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
              msg: 'pay amount correct',
              who: 'Alice'
               });
            const v465 = stdlib.addressEq(v59, v462);
            stdlib.assert(v465, {
              at: 'reach standard library:68:7:dot',
              fs: ['at ./workshop.rsh:97:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./workshop.rsh:120:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
              msg: 'sender correct',
              who: 'Alice'
               });
            const v467 = stdlib.add(v626, v463);
            ;
            stdlib.protect(ctc4, await interact.informTimeout(stdlib.checkedBigNumberify('./workshop.rsh:120:47:decimal', stdlib.UInt_max, 0)), {
              at: './workshop.rsh:55:33:application',
              fs: ['at ./workshop.rsh:54:13:application call to [unknown function] (defined at: ./workshop.rsh:54:25:function exp)', 'at ./workshop.rsh:97:74:application call to "informTimeout" (defined at: ./workshop.rsh:53:35:function exp)', 'at reach standard library:71:8:application call to "after" (defined at: ./workshop.rsh:97:57:function exp)', 'at ./workshop.rsh:97:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./workshop.rsh:120:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
              msg: 'informTimeout',
              who: 'Alice'
               });
            return;
             }
          else {
            const [v385] = txn4.data;
            const v386 = txn4.value;
            const v391 = txn4.time;
            const v384 = txn4.from;
            const v387 = stdlib.eq(v386, stdlib.checkedBigNumberify('./workshop.rsh:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v387, {
              at: './workshop.rsh:96:17:dot',
              fs: ['at ./workshop.rsh:120:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
              msg: 'pay amount correct',
              who: 'Alice'
               });
            const v388 = stdlib.addressEq(v47, v384);
            stdlib.assert(v388, {
              at: './workshop.rsh:96:17:dot',
              fs: ['at ./workshop.rsh:120:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
              msg: 'sender correct',
              who: 'Alice'
               });
            const v390 = stdlib.add(v626, v386);
            const txn5 = await (ctc.recv('Alice', 16, 1, [ctc1], false, v49));
            if (txn5.didTimeout) {
              const txn6 = await (ctc.sendrecv('Alice', 17, 0, stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 7), [ctc6, ctc0, ctc0, ctc6, ctc0, ctc3, ctc0, ctc0], [v47, v48, v49, v59, v169, v385, v390, v391], stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), [], true, true, false, (async (txn6) => {
                const sim_r = { txns: [] };
                sim_r.prevSt = stdlib.digest(ctc9, [stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 14), v47, v48, v49, v59, v169, v385, v390, v391]);
                sim_r.prevSt_noPrevTime = stdlib.digest(ctc10, [stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 14), v47, v48, v49, v59, v169, v385, v390]);
                const [] = txn6.data;
                const v442 = txn6.value;
                const v447 = txn6.time;
                const v441 = txn6.from;
                
                const v443 = stdlib.eq(v442, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
                stdlib.assert(v443, {
                  at: 'reach standard library:68:7:dot',
                  fs: ['at ./workshop.rsh:104:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./workshop.rsh:120:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                  msg: 'pay amount correct',
                  who: 'Alice'
                   });
                const v444 = stdlib.addressEq(v47, v441);
                stdlib.assert(v444, {
                  at: 'reach standard library:68:7:dot',
                  fs: ['at ./workshop.rsh:104:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./workshop.rsh:120:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                  msg: 'sender correct',
                  who: 'Alice'
                   });
                const v446 = stdlib.add(v390, v442);
                sim_r.txns.push({
                  amt: v446,
                  to: v47
                   });
                sim_r.nextSt = stdlib.digest(ctc5, []);
                sim_r.nextSt_noTime = stdlib.digest(ctc5, []);
                sim_r.isHalt = true;
                
                return sim_r;
                 })));
              const [] = txn6.data;
              const v442 = txn6.value;
              const v447 = txn6.time;
              const v441 = txn6.from;
              const v443 = stdlib.eq(v442, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
              stdlib.assert(v443, {
                at: 'reach standard library:68:7:dot',
                fs: ['at ./workshop.rsh:104:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./workshop.rsh:120:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                msg: 'pay amount correct',
                who: 'Alice'
                 });
              const v444 = stdlib.addressEq(v47, v441);
              stdlib.assert(v444, {
                at: 'reach standard library:68:7:dot',
                fs: ['at ./workshop.rsh:104:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./workshop.rsh:120:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                msg: 'sender correct',
                who: 'Alice'
                 });
              const v446 = stdlib.add(v390, v442);
              ;
              stdlib.protect(ctc4, await interact.informTimeout(stdlib.checkedBigNumberify('./workshop.rsh:120:49:decimal', stdlib.UInt_max, 1)), {
                at: './workshop.rsh:55:33:application',
                fs: ['at ./workshop.rsh:54:13:application call to [unknown function] (defined at: ./workshop.rsh:54:25:function exp)', 'at ./workshop.rsh:104:73:application call to "informTimeout" (defined at: ./workshop.rsh:53:35:function exp)', 'at reach standard library:71:8:application call to "after" (defined at: ./workshop.rsh:104:56:function exp)', 'at ./workshop.rsh:104:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./workshop.rsh:120:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                msg: 'informTimeout',
                who: 'Alice'
                 });
              return;
               }
            else {
              const [v396] = txn5.data;
              const v397 = txn5.value;
              const v402 = txn5.time;
              const v395 = txn5.from;
              const v398 = stdlib.eq(v397, stdlib.checkedBigNumberify('./workshop.rsh:decimal', stdlib.UInt_max, 0));
              stdlib.assert(v398, {
                at: './workshop.rsh:103:18:dot',
                fs: ['at ./workshop.rsh:120:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                msg: 'pay amount correct',
                who: 'Alice'
                 });
              const v399 = stdlib.addressEq(v59, v395);
              stdlib.assert(v399, {
                at: './workshop.rsh:103:18:dot',
                fs: ['at ./workshop.rsh:120:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                msg: 'sender correct',
                who: 'Alice'
                 });
              const v401 = stdlib.add(v390, v397);
              const txn6 = await (ctc.sendrecv('Alice', 18, 2, stdlib.checkedBigNumberify('./workshop.rsh:109:17:dot', stdlib.UInt_max, 8), [ctc6, ctc0, ctc0, ctc6, ctc0, ctc3, ctc1, ctc0, ctc0, ctc0, ctc1], [v47, v48, v49, v59, v169, v385, v396, v401, v402, v381, v379], stdlib.checkedBigNumberify('./workshop.rsh:decimal', stdlib.UInt_max, 0), [ctc0, ctc1], true, true, v49, (async (txn6) => {
                const sim_r = { txns: [] };
                sim_r.prevSt = stdlib.digest(ctc7, [stdlib.checkedBigNumberify('./workshop.rsh:109:17:dot', stdlib.UInt_max, 16), v47, v48, v49, v59, v169, v385, v396, v401, v402]);
                sim_r.prevSt_noPrevTime = stdlib.digest(ctc8, [stdlib.checkedBigNumberify('./workshop.rsh:109:17:dot', stdlib.UInt_max, 16), v47, v48, v49, v59, v169, v385, v396, v401]);
                const [v406, v407] = txn6.data;
                const v408 = txn6.value;
                const v413 = txn6.time;
                const v405 = txn6.from;
                
                const v409 = stdlib.eq(v408, stdlib.checkedBigNumberify('./workshop.rsh:decimal', stdlib.UInt_max, 0));
                stdlib.assert(v409, {
                  at: './workshop.rsh:109:17:dot',
                  fs: ['at ./workshop.rsh:120:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                  msg: 'pay amount correct',
                  who: 'Alice'
                   });
                const v410 = stdlib.addressEq(v47, v405);
                stdlib.assert(v410, {
                  at: './workshop.rsh:109:17:dot',
                  fs: ['at ./workshop.rsh:120:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                  msg: 'sender correct',
                  who: 'Alice'
                   });
                const v412 = stdlib.add(v401, v408);
                const v415 = stdlib.digest(ctc2, [v406, v407]);
                const v416 = stdlib.eq(v385, v415);
                stdlib.assert(v416, {
                  at: 'reach standard library:65:17:application',
                  fs: ['at ./workshop.rsh:111:26:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)', 'at ./workshop.rsh:120:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                  msg: null,
                  who: 'Alice'
                   });
                const v497 = v407[stdlib.checkedBigNumberify('./workshop.rsh:28:18:array ref', stdlib.UInt_max, 0)];
                const v498 = v396[stdlib.checkedBigNumberify('./workshop.rsh:28:29:array ref', stdlib.UInt_max, 0)];
                const v500 = stdlib.sub(stdlib.checkedBigNumberify('./workshop.rsh:7:18:decimal', stdlib.UInt_max, 4), v498);
                const v501 = stdlib.add(v497, v500);
                const v502 = stdlib.mod(v501, stdlib.checkedBigNumberify('./workshop.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v506 = v407[stdlib.checkedBigNumberify('./workshop.rsh:28:18:array ref', stdlib.UInt_max, 1)];
                const v507 = v396[stdlib.checkedBigNumberify('./workshop.rsh:28:29:array ref', stdlib.UInt_max, 1)];
                const v509 = stdlib.sub(stdlib.checkedBigNumberify('./workshop.rsh:7:18:decimal', stdlib.UInt_max, 4), v507);
                const v510 = stdlib.add(v506, v509);
                const v511 = stdlib.mod(v510, stdlib.checkedBigNumberify('./workshop.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v515 = v407[stdlib.checkedBigNumberify('./workshop.rsh:28:18:array ref', stdlib.UInt_max, 2)];
                const v516 = v396[stdlib.checkedBigNumberify('./workshop.rsh:28:29:array ref', stdlib.UInt_max, 2)];
                const v518 = stdlib.sub(stdlib.checkedBigNumberify('./workshop.rsh:7:18:decimal', stdlib.UInt_max, 4), v516);
                const v519 = stdlib.add(v515, v518);
                const v520 = stdlib.mod(v519, stdlib.checkedBigNumberify('./workshop.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v524 = v407[stdlib.checkedBigNumberify('./workshop.rsh:28:18:array ref', stdlib.UInt_max, 3)];
                const v525 = v396[stdlib.checkedBigNumberify('./workshop.rsh:28:29:array ref', stdlib.UInt_max, 3)];
                const v527 = stdlib.sub(stdlib.checkedBigNumberify('./workshop.rsh:7:18:decimal', stdlib.UInt_max, 4), v525);
                const v528 = stdlib.add(v524, v527);
                const v529 = stdlib.mod(v528, stdlib.checkedBigNumberify('./workshop.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v533 = v407[stdlib.checkedBigNumberify('./workshop.rsh:28:18:array ref', stdlib.UInt_max, 4)];
                const v534 = v396[stdlib.checkedBigNumberify('./workshop.rsh:28:29:array ref', stdlib.UInt_max, 4)];
                const v536 = stdlib.sub(stdlib.checkedBigNumberify('./workshop.rsh:7:18:decimal', stdlib.UInt_max, 4), v534);
                const v537 = stdlib.add(v533, v536);
                const v538 = stdlib.mod(v537, stdlib.checkedBigNumberify('./workshop.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v551 = stdlib.eq(v502, stdlib.checkedBigNumberify('./workshop.rsh:makeEnum', stdlib.UInt_max, 1));
                const v552 = v551 ? v511 : v502;
                const v555 = stdlib.eq(v552, stdlib.checkedBigNumberify('./workshop.rsh:makeEnum', stdlib.UInt_max, 1));
                const v556 = v555 ? v520 : v552;
                const v559 = stdlib.eq(v556, stdlib.checkedBigNumberify('./workshop.rsh:makeEnum', stdlib.UInt_max, 1));
                const v560 = v559 ? v529 : v556;
                const v563 = stdlib.eq(v560, stdlib.checkedBigNumberify('./workshop.rsh:makeEnum', stdlib.UInt_max, 1));
                const v564 = v563 ? v538 : v560;
                const v567 = stdlib.add(v169, stdlib.checkedBigNumberify('./workshop.rsh:121:83:decimal', stdlib.UInt_max, 1));
                const cv166 = v407;
                const cv167 = v396;
                const cv168 = v564;
                const cv169 = v567;
                const cv626 = v412;
                const cv627 = v413;
                
                (() => {
                  const v166 = cv166;
                  const v167 = cv167;
                  const v168 = cv168;
                  const v169 = cv169;
                  const v626 = cv626;
                  const v627 = cv627;
                  
                  if ((() => {
                    const v181 = stdlib.eq(v168, stdlib.checkedBigNumberify('./workshop.rsh:makeEnum', stdlib.UInt_max, 1));
                    
                    return v181; })()) {
                    const v182 = stdlib.mod(v169, stdlib.checkedBigNumberify('./workshop.rsh:115:21:decimal', stdlib.UInt_max, 2));
                    const v183 = stdlib.eq(v182, stdlib.checkedBigNumberify('./workshop.rsh:115:26:decimal', stdlib.UInt_max, 0));
                    if (v183) {
                      sim_r.nextSt = stdlib.digest(ctc11, [stdlib.checkedBigNumberify('./workshop.rsh:90:19:after expr stmt semicolon', stdlib.UInt_max, 6), v47, v48, v49, v59, v169, v626, v627]);
                      sim_r.nextSt_noTime = stdlib.digest(ctc12, [stdlib.checkedBigNumberify('./workshop.rsh:90:19:after expr stmt semicolon', stdlib.UInt_max, 6), v47, v48, v49, v59, v169, v626]);
                      sim_r.isHalt = false;
                       }
                    else {
                      sim_r.nextSt = stdlib.digest(ctc11, [stdlib.checkedBigNumberify('./workshop.rsh:90:19:after expr stmt semicolon', stdlib.UInt_max, 6), v47, v48, v49, v59, v169, v626, v627]);
                      sim_r.nextSt_noTime = stdlib.digest(ctc12, [stdlib.checkedBigNumberify('./workshop.rsh:90:19:after expr stmt semicolon', stdlib.UInt_max, 6), v47, v48, v49, v59, v169, v626]);
                      sim_r.isHalt = false;
                       } }
                  else {
                    const v568 = stdlib.eq(v168, stdlib.checkedBigNumberify('./workshop.rsh:makeEnum', stdlib.UInt_max, 2));
                    const v571 = stdlib.mul(stdlib.checkedBigNumberify('./workshop.rsh:127:16:decimal', stdlib.UInt_max, 2), v48);
                    const v573 = v568 ? v47 : v59;
                    sim_r.txns.push({
                      amt: v571,
                      to: v573
                       });
                    sim_r.nextSt = stdlib.digest(ctc5, []);
                    sim_r.nextSt_noTime = stdlib.digest(ctc5, []);
                    sim_r.isHalt = true;
                     } })();
                return sim_r;
                 })));
              if (txn6.didTimeout) {
                const txn7 = await (ctc.recv('Alice', 19, 0, [], false, false));
                const [] = txn7.data;
                const v421 = txn7.value;
                const v426 = txn7.time;
                const v420 = txn7.from;
                const v422 = stdlib.eq(v421, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
                stdlib.assert(v422, {
                  at: 'reach standard library:68:7:dot',
                  fs: ['at ./workshop.rsh:110:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./workshop.rsh:120:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                  msg: 'pay amount correct',
                  who: 'Alice'
                   });
                const v423 = stdlib.addressEq(v59, v420);
                stdlib.assert(v423, {
                  at: 'reach standard library:68:7:dot',
                  fs: ['at ./workshop.rsh:110:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./workshop.rsh:120:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                  msg: 'sender correct',
                  who: 'Alice'
                   });
                const v425 = stdlib.add(v401, v421);
                ;
                stdlib.protect(ctc4, await interact.informTimeout(stdlib.checkedBigNumberify('./workshop.rsh:120:47:decimal', stdlib.UInt_max, 0)), {
                  at: './workshop.rsh:55:33:application',
                  fs: ['at ./workshop.rsh:54:13:application call to [unknown function] (defined at: ./workshop.rsh:54:25:function exp)', 'at ./workshop.rsh:110:74:application call to "informTimeout" (defined at: ./workshop.rsh:53:35:function exp)', 'at reach standard library:71:8:application call to "after" (defined at: ./workshop.rsh:110:57:function exp)', 'at ./workshop.rsh:110:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./workshop.rsh:120:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                  msg: 'informTimeout',
                  who: 'Alice'
                   });
                return;
                 }
              else {
                const [v406, v407] = txn6.data;
                const v408 = txn6.value;
                const v413 = txn6.time;
                const v405 = txn6.from;
                const v409 = stdlib.eq(v408, stdlib.checkedBigNumberify('./workshop.rsh:decimal', stdlib.UInt_max, 0));
                stdlib.assert(v409, {
                  at: './workshop.rsh:109:17:dot',
                  fs: ['at ./workshop.rsh:120:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                  msg: 'pay amount correct',
                  who: 'Alice'
                   });
                const v410 = stdlib.addressEq(v47, v405);
                stdlib.assert(v410, {
                  at: './workshop.rsh:109:17:dot',
                  fs: ['at ./workshop.rsh:120:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                  msg: 'sender correct',
                  who: 'Alice'
                   });
                const v412 = stdlib.add(v401, v408);
                const v415 = stdlib.digest(ctc2, [v406, v407]);
                const v416 = stdlib.eq(v385, v415);
                stdlib.assert(v416, {
                  at: 'reach standard library:65:17:application',
                  fs: ['at ./workshop.rsh:111:26:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)', 'at ./workshop.rsh:120:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                  msg: null,
                  who: 'Alice'
                   });
                const v497 = v407[stdlib.checkedBigNumberify('./workshop.rsh:28:18:array ref', stdlib.UInt_max, 0)];
                const v498 = v396[stdlib.checkedBigNumberify('./workshop.rsh:28:29:array ref', stdlib.UInt_max, 0)];
                const v500 = stdlib.sub(stdlib.checkedBigNumberify('./workshop.rsh:7:18:decimal', stdlib.UInt_max, 4), v498);
                const v501 = stdlib.add(v497, v500);
                const v502 = stdlib.mod(v501, stdlib.checkedBigNumberify('./workshop.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v506 = v407[stdlib.checkedBigNumberify('./workshop.rsh:28:18:array ref', stdlib.UInt_max, 1)];
                const v507 = v396[stdlib.checkedBigNumberify('./workshop.rsh:28:29:array ref', stdlib.UInt_max, 1)];
                const v509 = stdlib.sub(stdlib.checkedBigNumberify('./workshop.rsh:7:18:decimal', stdlib.UInt_max, 4), v507);
                const v510 = stdlib.add(v506, v509);
                const v511 = stdlib.mod(v510, stdlib.checkedBigNumberify('./workshop.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v515 = v407[stdlib.checkedBigNumberify('./workshop.rsh:28:18:array ref', stdlib.UInt_max, 2)];
                const v516 = v396[stdlib.checkedBigNumberify('./workshop.rsh:28:29:array ref', stdlib.UInt_max, 2)];
                const v518 = stdlib.sub(stdlib.checkedBigNumberify('./workshop.rsh:7:18:decimal', stdlib.UInt_max, 4), v516);
                const v519 = stdlib.add(v515, v518);
                const v520 = stdlib.mod(v519, stdlib.checkedBigNumberify('./workshop.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v524 = v407[stdlib.checkedBigNumberify('./workshop.rsh:28:18:array ref', stdlib.UInt_max, 3)];
                const v525 = v396[stdlib.checkedBigNumberify('./workshop.rsh:28:29:array ref', stdlib.UInt_max, 3)];
                const v527 = stdlib.sub(stdlib.checkedBigNumberify('./workshop.rsh:7:18:decimal', stdlib.UInt_max, 4), v525);
                const v528 = stdlib.add(v524, v527);
                const v529 = stdlib.mod(v528, stdlib.checkedBigNumberify('./workshop.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v533 = v407[stdlib.checkedBigNumberify('./workshop.rsh:28:18:array ref', stdlib.UInt_max, 4)];
                const v534 = v396[stdlib.checkedBigNumberify('./workshop.rsh:28:29:array ref', stdlib.UInt_max, 4)];
                const v536 = stdlib.sub(stdlib.checkedBigNumberify('./workshop.rsh:7:18:decimal', stdlib.UInt_max, 4), v534);
                const v537 = stdlib.add(v533, v536);
                const v538 = stdlib.mod(v537, stdlib.checkedBigNumberify('./workshop.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v551 = stdlib.eq(v502, stdlib.checkedBigNumberify('./workshop.rsh:makeEnum', stdlib.UInt_max, 1));
                const v552 = v551 ? v511 : v502;
                const v555 = stdlib.eq(v552, stdlib.checkedBigNumberify('./workshop.rsh:makeEnum', stdlib.UInt_max, 1));
                const v556 = v555 ? v520 : v552;
                const v559 = stdlib.eq(v556, stdlib.checkedBigNumberify('./workshop.rsh:makeEnum', stdlib.UInt_max, 1));
                const v560 = v559 ? v529 : v556;
                const v563 = stdlib.eq(v560, stdlib.checkedBigNumberify('./workshop.rsh:makeEnum', stdlib.UInt_max, 1));
                const v564 = v563 ? v538 : v560;
                const v567 = stdlib.add(v169, stdlib.checkedBigNumberify('./workshop.rsh:121:83:decimal', stdlib.UInt_max, 1));
                const cv166 = v407;
                const cv167 = v396;
                const cv168 = v564;
                const cv169 = v567;
                const cv626 = v412;
                const cv627 = v413;
                
                v166 = cv166;
                v167 = cv167;
                v168 = cv168;
                v169 = cv169;
                v626 = cv626;
                v627 = cv627;
                
                continue; }
               }
             }
           } }
      const v568 = stdlib.eq(v168, stdlib.checkedBigNumberify('./workshop.rsh:makeEnum', stdlib.UInt_max, 2));
      const v571 = stdlib.mul(stdlib.checkedBigNumberify('./workshop.rsh:127:16:decimal', stdlib.UInt_max, 2), v48);
      const v573 = v568 ? v47 : v59;
      ;
      stdlib.protect(ctc4, await interact.seeOutcome(v168, v166, v167), {
        at: './workshop.rsh:131:28:application',
        fs: ['at ./workshop.rsh:130:11:application call to [unknown function] (defined at: ./workshop.rsh:130:23:function exp)'],
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
    at: './workshop.rsh:65:9:dot',
    fs: [],
    msg: 'pay amount correct',
    who: 'Bob'
     });
  stdlib.assert(true, {
    at: './workshop.rsh:65:9:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Bob'
     });
  const v54 = stdlib.add(stdlib.checkedBigNumberify('./workshop.rsh:compileDApp', stdlib.UInt_max, 0), v51);
  stdlib.protect(ctc2, await interact.acceptWager(v48, v49), {
    at: './workshop.rsh:71:29:application',
    fs: ['at ./workshop.rsh:70:13:application call to [unknown function] (defined at: ./workshop.rsh:70:17:function exp)'],
    msg: 'acceptWager',
    who: 'Bob'
     });
  const v58 = stdlib.protect(ctc3, await interact.getBatch(), {
    at: './workshop.rsh:72:57:application',
    fs: ['at ./workshop.rsh:70:13:application call to [unknown function] (defined at: ./workshop.rsh:70:17:function exp)'],
    msg: 'getBatch',
    who: 'Bob'
     });
  const txn2 = await (ctc.sendrecv('Bob', 2, 1, stdlib.checkedBigNumberify('./workshop.rsh:74:9:dot', stdlib.UInt_max, 5), [ctc5, ctc0, ctc0, ctc1, ctc0, ctc0, ctc3], [v47, v48, v49, v50, v54, v55, v58], v48, [ctc3], true, true, v49, (async (txn2) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.digest(ctc15, [stdlib.checkedBigNumberify('./workshop.rsh:74:9:dot', stdlib.UInt_max, 1), v47, v48, v49, v50, v54, v55]);
    sim_r.prevSt_noPrevTime = stdlib.digest(ctc16, [stdlib.checkedBigNumberify('./workshop.rsh:74:9:dot', stdlib.UInt_max, 1), v47, v48, v49, v50, v54]);
    const [v60] = txn2.data;
    const v61 = txn2.value;
    const v65 = txn2.time;
    const v59 = txn2.from;
    
    const v62 = stdlib.eq(v61, v48);
    stdlib.assert(v62, {
      at: './workshop.rsh:74:9:dot',
      fs: [],
      msg: 'pay amount correct',
      who: 'Bob'
       });
    stdlib.assert(true, {
      at: './workshop.rsh:74:9:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Bob'
       });
    const v64 = stdlib.add(v54, v61);
    sim_r.nextSt = stdlib.digest(ctc13, [stdlib.checkedBigNumberify('./workshop.rsh:77:15:after expr stmt semicolon', stdlib.UInt_max, 2), v47, v48, v49, v50, v59, v60, v64, v65]);
    sim_r.nextSt_noTime = stdlib.digest(ctc14, [stdlib.checkedBigNumberify('./workshop.rsh:77:15:after expr stmt semicolon', stdlib.UInt_max, 2), v47, v48, v49, v50, v59, v60, v64]);
    sim_r.isHalt = false;
    
    return sim_r;
     })));
  if (txn2.didTimeout) {
    const txn3 = await (ctc.recv('Bob', 3, 0, [], false, false));
    const [] = txn3.data;
    const v607 = txn3.value;
    const v612 = txn3.time;
    const v606 = txn3.from;
    const v608 = stdlib.eq(v607, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
    stdlib.assert(v608, {
      at: 'reach standard library:68:7:dot',
      fs: ['at ./workshop.rsh:76:41:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
      msg: 'pay amount correct',
      who: 'Bob'
       });
    const v609 = stdlib.addressEq(v47, v606);
    stdlib.assert(v609, {
      at: 'reach standard library:68:7:dot',
      fs: ['at ./workshop.rsh:76:41:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
      msg: 'sender correct',
      who: 'Bob'
       });
    const v611 = stdlib.add(v54, v607);
    ;
    stdlib.protect(ctc2, await interact.informTimeout(stdlib.checkedBigNumberify('./workshop.rsh:76:66:decimal', stdlib.UInt_max, 1)), {
      at: './workshop.rsh:55:33:application',
      fs: ['at ./workshop.rsh:54:13:application call to [unknown function] (defined at: ./workshop.rsh:54:25:function exp)', 'at ./workshop.rsh:76:65:application call to "informTimeout" (defined at: ./workshop.rsh:53:35:function exp)', 'at reach standard library:71:8:application call to "after" (defined at: ./workshop.rsh:76:48:function exp)', 'at ./workshop.rsh:76:41:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
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
      at: './workshop.rsh:74:9:dot',
      fs: [],
      msg: 'pay amount correct',
      who: 'Bob'
       });
    stdlib.assert(true, {
      at: './workshop.rsh:74:9:dot',
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
        const v586 = txn4.value;
        const v591 = txn4.time;
        const v585 = txn4.from;
        
        const v587 = stdlib.eq(v586, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
        stdlib.assert(v587, {
          at: 'reach standard library:68:7:dot',
          fs: ['at ./workshop.rsh:83:40:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
          msg: 'pay amount correct',
          who: 'Bob'
           });
        const v588 = stdlib.addressEq(v59, v585);
        stdlib.assert(v588, {
          at: 'reach standard library:68:7:dot',
          fs: ['at ./workshop.rsh:83:40:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
          msg: 'sender correct',
          who: 'Bob'
           });
        const v590 = stdlib.add(v64, v586);
        sim_r.txns.push({
          amt: v590,
          to: v59
           });
        sim_r.nextSt = stdlib.digest(ctc8, []);
        sim_r.nextSt_noTime = stdlib.digest(ctc8, []);
        sim_r.isHalt = true;
        
        return sim_r;
         })));
      const [] = txn4.data;
      const v586 = txn4.value;
      const v591 = txn4.time;
      const v585 = txn4.from;
      const v587 = stdlib.eq(v586, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
      stdlib.assert(v587, {
        at: 'reach standard library:68:7:dot',
        fs: ['at ./workshop.rsh:83:40:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
        msg: 'pay amount correct',
        who: 'Bob'
         });
      const v588 = stdlib.addressEq(v59, v585);
      stdlib.assert(v588, {
        at: 'reach standard library:68:7:dot',
        fs: ['at ./workshop.rsh:83:40:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
        msg: 'sender correct',
        who: 'Bob'
         });
      const v590 = stdlib.add(v64, v586);
      ;
      stdlib.protect(ctc2, await interact.informTimeout(stdlib.checkedBigNumberify('./workshop.rsh:83:65:decimal', stdlib.UInt_max, 0)), {
        at: './workshop.rsh:55:33:application',
        fs: ['at ./workshop.rsh:54:13:application call to [unknown function] (defined at: ./workshop.rsh:54:25:function exp)', 'at ./workshop.rsh:83:64:application call to "informTimeout" (defined at: ./workshop.rsh:53:35:function exp)', 'at reach standard library:71:8:application call to "after" (defined at: ./workshop.rsh:83:47:function exp)', 'at ./workshop.rsh:83:40:application call to "closeTo" (defined at: reach standard library:67:8:function exp)'],
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
      const v72 = stdlib.eq(v71, stdlib.checkedBigNumberify('./workshop.rsh:decimal', stdlib.UInt_max, 0));
      stdlib.assert(v72, {
        at: './workshop.rsh:82:9:dot',
        fs: [],
        msg: 'pay amount correct',
        who: 'Bob'
         });
      const v73 = stdlib.addressEq(v47, v68);
      stdlib.assert(v73, {
        at: './workshop.rsh:82:9:dot',
        fs: [],
        msg: 'sender correct',
        who: 'Bob'
         });
      const v75 = stdlib.add(v64, v71);
      const v78 = stdlib.digest(ctc4, [v69, v70]);
      const v79 = stdlib.eq(v50, v78);
      stdlib.assert(v79, {
        at: 'reach standard library:65:17:application',
        fs: ['at ./workshop.rsh:84:22:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)'],
        msg: null,
        who: 'Bob'
         });
      const v82 = [stdlib.checkedBigNumberify('./workshop.rsh:27:13:application', stdlib.UInt_max, 0), stdlib.checkedBigNumberify('./workshop.rsh:27:13:application', stdlib.UInt_max, 1), stdlib.checkedBigNumberify('./workshop.rsh:27:13:application', stdlib.UInt_max, 2), stdlib.checkedBigNumberify('./workshop.rsh:27:13:application', stdlib.UInt_max, 3), stdlib.checkedBigNumberify('./workshop.rsh:27:13:application', stdlib.UInt_max, 4)];
      const v96 = v70[stdlib.checkedBigNumberify('./workshop.rsh:28:18:array ref', stdlib.UInt_max, 0)];
      const v97 = v60[stdlib.checkedBigNumberify('./workshop.rsh:28:29:array ref', stdlib.UInt_max, 0)];
      const v99 = stdlib.sub(stdlib.checkedBigNumberify('./workshop.rsh:7:18:decimal', stdlib.UInt_max, 4), v97);
      const v100 = stdlib.add(v96, v99);
      const v101 = stdlib.mod(v100, stdlib.checkedBigNumberify('./workshop.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v105 = v70[stdlib.checkedBigNumberify('./workshop.rsh:28:18:array ref', stdlib.UInt_max, 1)];
      const v106 = v60[stdlib.checkedBigNumberify('./workshop.rsh:28:29:array ref', stdlib.UInt_max, 1)];
      const v108 = stdlib.sub(stdlib.checkedBigNumberify('./workshop.rsh:7:18:decimal', stdlib.UInt_max, 4), v106);
      const v109 = stdlib.add(v105, v108);
      const v110 = stdlib.mod(v109, stdlib.checkedBigNumberify('./workshop.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v114 = v70[stdlib.checkedBigNumberify('./workshop.rsh:28:18:array ref', stdlib.UInt_max, 2)];
      const v115 = v60[stdlib.checkedBigNumberify('./workshop.rsh:28:29:array ref', stdlib.UInt_max, 2)];
      const v117 = stdlib.sub(stdlib.checkedBigNumberify('./workshop.rsh:7:18:decimal', stdlib.UInt_max, 4), v115);
      const v118 = stdlib.add(v114, v117);
      const v119 = stdlib.mod(v118, stdlib.checkedBigNumberify('./workshop.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v123 = v70[stdlib.checkedBigNumberify('./workshop.rsh:28:18:array ref', stdlib.UInt_max, 3)];
      const v124 = v60[stdlib.checkedBigNumberify('./workshop.rsh:28:29:array ref', stdlib.UInt_max, 3)];
      const v126 = stdlib.sub(stdlib.checkedBigNumberify('./workshop.rsh:7:18:decimal', stdlib.UInt_max, 4), v124);
      const v127 = stdlib.add(v123, v126);
      const v128 = stdlib.mod(v127, stdlib.checkedBigNumberify('./workshop.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v132 = v70[stdlib.checkedBigNumberify('./workshop.rsh:28:18:array ref', stdlib.UInt_max, 4)];
      const v133 = v60[stdlib.checkedBigNumberify('./workshop.rsh:28:29:array ref', stdlib.UInt_max, 4)];
      const v135 = stdlib.sub(stdlib.checkedBigNumberify('./workshop.rsh:7:18:decimal', stdlib.UInt_max, 4), v133);
      const v136 = stdlib.add(v132, v135);
      const v137 = stdlib.mod(v136, stdlib.checkedBigNumberify('./workshop.rsh:7:32:decimal', stdlib.UInt_max, 3));
      const v150 = stdlib.eq(v101, stdlib.checkedBigNumberify('./workshop.rsh:makeEnum', stdlib.UInt_max, 1));
      const v151 = v150 ? v110 : v101;
      const v154 = stdlib.eq(v151, stdlib.checkedBigNumberify('./workshop.rsh:makeEnum', stdlib.UInt_max, 1));
      const v155 = v154 ? v119 : v151;
      const v158 = stdlib.eq(v155, stdlib.checkedBigNumberify('./workshop.rsh:makeEnum', stdlib.UInt_max, 1));
      const v159 = v158 ? v128 : v155;
      const v162 = stdlib.eq(v159, stdlib.checkedBigNumberify('./workshop.rsh:makeEnum', stdlib.UInt_max, 1));
      const v163 = v162 ? v137 : v159;
      let v166 = v82;
      let v167 = v82;
      let v168 = v163;
      let v169 = stdlib.checkedBigNumberify('./workshop.rsh:86:86:decimal', stdlib.UInt_max, 0);
      let v626 = v75;
      let v627 = v76;
      
      while ((() => {
        const v181 = stdlib.eq(v168, stdlib.checkedBigNumberify('./workshop.rsh:makeEnum', stdlib.UInt_max, 1));
        
        return v181; })()) {
        const v182 = stdlib.mod(v169, stdlib.checkedBigNumberify('./workshop.rsh:115:21:decimal', stdlib.UInt_max, 2));
        const v183 = stdlib.eq(v182, stdlib.checkedBigNumberify('./workshop.rsh:115:26:decimal', stdlib.UInt_max, 0));
        if (v183) {
          const v187 = stdlib.protect(ctc3, await interact.getBatch(), {
            at: './workshop.rsh:93:50:application',
            fs: ['at ./workshop.rsh:92:21:application call to [unknown function] (defined at: ./workshop.rsh:92:25:function exp)', 'at ./workshop.rsh:116:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
            msg: 'getBatch',
            who: 'Bob'
             });
          const v189 = stdlib.protect(ctc0, await interact.random(), {
            at: 'reach standard library:60:31:application',
            fs: ['at ./workshop.rsh:94:62:application call to "makeCommitment" (defined at: reach standard library:59:8:function exp)', 'at ./workshop.rsh:92:21:application call to [unknown function] (defined at: ./workshop.rsh:92:25:function exp)', 'at ./workshop.rsh:116:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
            msg: 'random',
            who: 'Bob'
             });
          const v190 = stdlib.digest(ctc4, [v189, v187]);
          const txn4 = await (ctc.sendrecv('Bob', 8, 1, stdlib.checkedBigNumberify('./workshop.rsh:96:17:dot', stdlib.UInt_max, 6), [ctc5, ctc0, ctc0, ctc5, ctc0, ctc0, ctc0, ctc1], [v47, v48, v49, v59, v169, v626, v627, v190], stdlib.checkedBigNumberify('./workshop.rsh:decimal', stdlib.UInt_max, 0), [ctc1], true, true, v49, (async (txn4) => {
            const sim_r = { txns: [] };
            sim_r.prevSt = stdlib.digest(ctc6, [stdlib.checkedBigNumberify('./workshop.rsh:96:17:dot', stdlib.UInt_max, 6), v47, v48, v49, v59, v169, v626, v627]);
            sim_r.prevSt_noPrevTime = stdlib.digest(ctc7, [stdlib.checkedBigNumberify('./workshop.rsh:96:17:dot', stdlib.UInt_max, 6), v47, v48, v49, v59, v169, v626]);
            const [v193] = txn4.data;
            const v194 = txn4.value;
            const v199 = txn4.time;
            const v192 = txn4.from;
            
            const v195 = stdlib.eq(v194, stdlib.checkedBigNumberify('./workshop.rsh:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v195, {
              at: './workshop.rsh:96:17:dot',
              fs: ['at ./workshop.rsh:116:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
              msg: 'pay amount correct',
              who: 'Bob'
               });
            const v196 = stdlib.addressEq(v59, v192);
            stdlib.assert(v196, {
              at: './workshop.rsh:96:17:dot',
              fs: ['at ./workshop.rsh:116:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
              msg: 'sender correct',
              who: 'Bob'
               });
            const v198 = stdlib.add(v626, v194);
            sim_r.nextSt = stdlib.digest(ctc11, [stdlib.checkedBigNumberify('./workshop.rsh:98:19:after expr stmt semicolon', stdlib.UInt_max, 8), v47, v48, v49, v59, v169, v193, v198, v199]);
            sim_r.nextSt_noTime = stdlib.digest(ctc12, [stdlib.checkedBigNumberify('./workshop.rsh:98:19:after expr stmt semicolon', stdlib.UInt_max, 8), v47, v48, v49, v59, v169, v193, v198]);
            sim_r.isHalt = false;
            
            return sim_r;
             })));
          if (txn4.didTimeout) {
            const txn5 = await (ctc.recv('Bob', 9, 0, [], false, false));
            const [] = txn5.data;
            const v271 = txn5.value;
            const v276 = txn5.time;
            const v270 = txn5.from;
            const v272 = stdlib.eq(v271, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v272, {
              at: 'reach standard library:68:7:dot',
              fs: ['at ./workshop.rsh:97:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./workshop.rsh:116:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
              msg: 'pay amount correct',
              who: 'Bob'
               });
            const v273 = stdlib.addressEq(v47, v270);
            stdlib.assert(v273, {
              at: 'reach standard library:68:7:dot',
              fs: ['at ./workshop.rsh:97:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./workshop.rsh:116:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
              msg: 'sender correct',
              who: 'Bob'
               });
            const v275 = stdlib.add(v626, v271);
            ;
            stdlib.protect(ctc2, await interact.informTimeout(stdlib.checkedBigNumberify('./workshop.rsh:116:47:decimal', stdlib.UInt_max, 1)), {
              at: './workshop.rsh:55:33:application',
              fs: ['at ./workshop.rsh:54:13:application call to [unknown function] (defined at: ./workshop.rsh:54:25:function exp)', 'at ./workshop.rsh:97:74:application call to "informTimeout" (defined at: ./workshop.rsh:53:35:function exp)', 'at reach standard library:71:8:application call to "after" (defined at: ./workshop.rsh:97:57:function exp)', 'at ./workshop.rsh:97:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./workshop.rsh:116:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
              msg: 'informTimeout',
              who: 'Bob'
               });
            return;
             }
          else {
            const [v193] = txn4.data;
            const v194 = txn4.value;
            const v199 = txn4.time;
            const v192 = txn4.from;
            const v195 = stdlib.eq(v194, stdlib.checkedBigNumberify('./workshop.rsh:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v195, {
              at: './workshop.rsh:96:17:dot',
              fs: ['at ./workshop.rsh:116:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
              msg: 'pay amount correct',
              who: 'Bob'
               });
            const v196 = stdlib.addressEq(v59, v192);
            stdlib.assert(v196, {
              at: './workshop.rsh:96:17:dot',
              fs: ['at ./workshop.rsh:116:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
              msg: 'sender correct',
              who: 'Bob'
               });
            const v198 = stdlib.add(v626, v194);
            const txn5 = await (ctc.recv('Bob', 10, 1, [ctc3], false, v49));
            if (txn5.didTimeout) {
              const txn6 = await (ctc.sendrecv('Bob', 11, 0, stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 7), [ctc5, ctc0, ctc0, ctc5, ctc0, ctc1, ctc0, ctc0], [v47, v48, v49, v59, v169, v193, v198, v199], stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), [], true, true, false, (async (txn6) => {
                const sim_r = { txns: [] };
                sim_r.prevSt = stdlib.digest(ctc11, [stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 8), v47, v48, v49, v59, v169, v193, v198, v199]);
                sim_r.prevSt_noPrevTime = stdlib.digest(ctc12, [stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 8), v47, v48, v49, v59, v169, v193, v198]);
                const [] = txn6.data;
                const v250 = txn6.value;
                const v255 = txn6.time;
                const v249 = txn6.from;
                
                const v251 = stdlib.eq(v250, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
                stdlib.assert(v251, {
                  at: 'reach standard library:68:7:dot',
                  fs: ['at ./workshop.rsh:104:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./workshop.rsh:116:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                  msg: 'pay amount correct',
                  who: 'Bob'
                   });
                const v252 = stdlib.addressEq(v59, v249);
                stdlib.assert(v252, {
                  at: 'reach standard library:68:7:dot',
                  fs: ['at ./workshop.rsh:104:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./workshop.rsh:116:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                  msg: 'sender correct',
                  who: 'Bob'
                   });
                const v254 = stdlib.add(v198, v250);
                sim_r.txns.push({
                  amt: v254,
                  to: v59
                   });
                sim_r.nextSt = stdlib.digest(ctc8, []);
                sim_r.nextSt_noTime = stdlib.digest(ctc8, []);
                sim_r.isHalt = true;
                
                return sim_r;
                 })));
              const [] = txn6.data;
              const v250 = txn6.value;
              const v255 = txn6.time;
              const v249 = txn6.from;
              const v251 = stdlib.eq(v250, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
              stdlib.assert(v251, {
                at: 'reach standard library:68:7:dot',
                fs: ['at ./workshop.rsh:104:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./workshop.rsh:116:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                msg: 'pay amount correct',
                who: 'Bob'
                 });
              const v252 = stdlib.addressEq(v59, v249);
              stdlib.assert(v252, {
                at: 'reach standard library:68:7:dot',
                fs: ['at ./workshop.rsh:104:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./workshop.rsh:116:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                msg: 'sender correct',
                who: 'Bob'
                 });
              const v254 = stdlib.add(v198, v250);
              ;
              stdlib.protect(ctc2, await interact.informTimeout(stdlib.checkedBigNumberify('./workshop.rsh:116:49:decimal', stdlib.UInt_max, 0)), {
                at: './workshop.rsh:55:33:application',
                fs: ['at ./workshop.rsh:54:13:application call to [unknown function] (defined at: ./workshop.rsh:54:25:function exp)', 'at ./workshop.rsh:104:73:application call to "informTimeout" (defined at: ./workshop.rsh:53:35:function exp)', 'at reach standard library:71:8:application call to "after" (defined at: ./workshop.rsh:104:56:function exp)', 'at ./workshop.rsh:104:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./workshop.rsh:116:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                msg: 'informTimeout',
                who: 'Bob'
                 });
              return;
               }
            else {
              const [v204] = txn5.data;
              const v205 = txn5.value;
              const v210 = txn5.time;
              const v203 = txn5.from;
              const v206 = stdlib.eq(v205, stdlib.checkedBigNumberify('./workshop.rsh:decimal', stdlib.UInt_max, 0));
              stdlib.assert(v206, {
                at: './workshop.rsh:103:18:dot',
                fs: ['at ./workshop.rsh:116:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                msg: 'pay amount correct',
                who: 'Bob'
                 });
              const v207 = stdlib.addressEq(v47, v203);
              stdlib.assert(v207, {
                at: './workshop.rsh:103:18:dot',
                fs: ['at ./workshop.rsh:116:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                msg: 'sender correct',
                who: 'Bob'
                 });
              const v209 = stdlib.add(v198, v205);
              const txn6 = await (ctc.sendrecv('Bob', 12, 2, stdlib.checkedBigNumberify('./workshop.rsh:109:17:dot', stdlib.UInt_max, 8), [ctc5, ctc0, ctc0, ctc5, ctc0, ctc1, ctc3, ctc0, ctc0, ctc0, ctc3], [v47, v48, v49, v59, v169, v193, v204, v209, v210, v189, v187], stdlib.checkedBigNumberify('./workshop.rsh:decimal', stdlib.UInt_max, 0), [ctc0, ctc3], true, true, v49, (async (txn6) => {
                const sim_r = { txns: [] };
                sim_r.prevSt = stdlib.digest(ctc9, [stdlib.checkedBigNumberify('./workshop.rsh:109:17:dot', stdlib.UInt_max, 10), v47, v48, v49, v59, v169, v193, v204, v209, v210]);
                sim_r.prevSt_noPrevTime = stdlib.digest(ctc10, [stdlib.checkedBigNumberify('./workshop.rsh:109:17:dot', stdlib.UInt_max, 10), v47, v48, v49, v59, v169, v193, v204, v209]);
                const [v214, v215] = txn6.data;
                const v216 = txn6.value;
                const v221 = txn6.time;
                const v213 = txn6.from;
                
                const v217 = stdlib.eq(v216, stdlib.checkedBigNumberify('./workshop.rsh:decimal', stdlib.UInt_max, 0));
                stdlib.assert(v217, {
                  at: './workshop.rsh:109:17:dot',
                  fs: ['at ./workshop.rsh:116:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                  msg: 'pay amount correct',
                  who: 'Bob'
                   });
                const v218 = stdlib.addressEq(v59, v213);
                stdlib.assert(v218, {
                  at: './workshop.rsh:109:17:dot',
                  fs: ['at ./workshop.rsh:116:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                  msg: 'sender correct',
                  who: 'Bob'
                   });
                const v220 = stdlib.add(v209, v216);
                const v223 = stdlib.digest(ctc4, [v214, v215]);
                const v224 = stdlib.eq(v193, v223);
                stdlib.assert(v224, {
                  at: 'reach standard library:65:17:application',
                  fs: ['at ./workshop.rsh:111:26:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)', 'at ./workshop.rsh:116:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                  msg: null,
                  who: 'Bob'
                   });
                const v305 = v204[stdlib.checkedBigNumberify('./workshop.rsh:28:18:array ref', stdlib.UInt_max, 0)];
                const v306 = v215[stdlib.checkedBigNumberify('./workshop.rsh:28:29:array ref', stdlib.UInt_max, 0)];
                const v308 = stdlib.sub(stdlib.checkedBigNumberify('./workshop.rsh:7:18:decimal', stdlib.UInt_max, 4), v306);
                const v309 = stdlib.add(v305, v308);
                const v310 = stdlib.mod(v309, stdlib.checkedBigNumberify('./workshop.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v314 = v204[stdlib.checkedBigNumberify('./workshop.rsh:28:18:array ref', stdlib.UInt_max, 1)];
                const v315 = v215[stdlib.checkedBigNumberify('./workshop.rsh:28:29:array ref', stdlib.UInt_max, 1)];
                const v317 = stdlib.sub(stdlib.checkedBigNumberify('./workshop.rsh:7:18:decimal', stdlib.UInt_max, 4), v315);
                const v318 = stdlib.add(v314, v317);
                const v319 = stdlib.mod(v318, stdlib.checkedBigNumberify('./workshop.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v323 = v204[stdlib.checkedBigNumberify('./workshop.rsh:28:18:array ref', stdlib.UInt_max, 2)];
                const v324 = v215[stdlib.checkedBigNumberify('./workshop.rsh:28:29:array ref', stdlib.UInt_max, 2)];
                const v326 = stdlib.sub(stdlib.checkedBigNumberify('./workshop.rsh:7:18:decimal', stdlib.UInt_max, 4), v324);
                const v327 = stdlib.add(v323, v326);
                const v328 = stdlib.mod(v327, stdlib.checkedBigNumberify('./workshop.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v332 = v204[stdlib.checkedBigNumberify('./workshop.rsh:28:18:array ref', stdlib.UInt_max, 3)];
                const v333 = v215[stdlib.checkedBigNumberify('./workshop.rsh:28:29:array ref', stdlib.UInt_max, 3)];
                const v335 = stdlib.sub(stdlib.checkedBigNumberify('./workshop.rsh:7:18:decimal', stdlib.UInt_max, 4), v333);
                const v336 = stdlib.add(v332, v335);
                const v337 = stdlib.mod(v336, stdlib.checkedBigNumberify('./workshop.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v341 = v204[stdlib.checkedBigNumberify('./workshop.rsh:28:18:array ref', stdlib.UInt_max, 4)];
                const v342 = v215[stdlib.checkedBigNumberify('./workshop.rsh:28:29:array ref', stdlib.UInt_max, 4)];
                const v344 = stdlib.sub(stdlib.checkedBigNumberify('./workshop.rsh:7:18:decimal', stdlib.UInt_max, 4), v342);
                const v345 = stdlib.add(v341, v344);
                const v346 = stdlib.mod(v345, stdlib.checkedBigNumberify('./workshop.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v359 = stdlib.eq(v310, stdlib.checkedBigNumberify('./workshop.rsh:makeEnum', stdlib.UInt_max, 1));
                const v360 = v359 ? v319 : v310;
                const v363 = stdlib.eq(v360, stdlib.checkedBigNumberify('./workshop.rsh:makeEnum', stdlib.UInt_max, 1));
                const v364 = v363 ? v328 : v360;
                const v367 = stdlib.eq(v364, stdlib.checkedBigNumberify('./workshop.rsh:makeEnum', stdlib.UInt_max, 1));
                const v368 = v367 ? v337 : v364;
                const v371 = stdlib.eq(v368, stdlib.checkedBigNumberify('./workshop.rsh:makeEnum', stdlib.UInt_max, 1));
                const v372 = v371 ? v346 : v368;
                const v375 = stdlib.add(v169, stdlib.checkedBigNumberify('./workshop.rsh:117:83:decimal', stdlib.UInt_max, 1));
                const cv166 = v204;
                const cv167 = v215;
                const cv168 = v372;
                const cv169 = v375;
                const cv626 = v220;
                const cv627 = v221;
                
                (() => {
                  const v166 = cv166;
                  const v167 = cv167;
                  const v168 = cv168;
                  const v169 = cv169;
                  const v626 = cv626;
                  const v627 = cv627;
                  
                  if ((() => {
                    const v181 = stdlib.eq(v168, stdlib.checkedBigNumberify('./workshop.rsh:makeEnum', stdlib.UInt_max, 1));
                    
                    return v181; })()) {
                    const v182 = stdlib.mod(v169, stdlib.checkedBigNumberify('./workshop.rsh:115:21:decimal', stdlib.UInt_max, 2));
                    const v183 = stdlib.eq(v182, stdlib.checkedBigNumberify('./workshop.rsh:115:26:decimal', stdlib.UInt_max, 0));
                    if (v183) {
                      sim_r.nextSt = stdlib.digest(ctc6, [stdlib.checkedBigNumberify('./workshop.rsh:90:19:after expr stmt semicolon', stdlib.UInt_max, 6), v47, v48, v49, v59, v169, v626, v627]);
                      sim_r.nextSt_noTime = stdlib.digest(ctc7, [stdlib.checkedBigNumberify('./workshop.rsh:90:19:after expr stmt semicolon', stdlib.UInt_max, 6), v47, v48, v49, v59, v169, v626]);
                      sim_r.isHalt = false;
                       }
                    else {
                      sim_r.nextSt = stdlib.digest(ctc6, [stdlib.checkedBigNumberify('./workshop.rsh:90:19:after expr stmt semicolon', stdlib.UInt_max, 6), v47, v48, v49, v59, v169, v626, v627]);
                      sim_r.nextSt_noTime = stdlib.digest(ctc7, [stdlib.checkedBigNumberify('./workshop.rsh:90:19:after expr stmt semicolon', stdlib.UInt_max, 6), v47, v48, v49, v59, v169, v626]);
                      sim_r.isHalt = false;
                       } }
                  else {
                    const v568 = stdlib.eq(v168, stdlib.checkedBigNumberify('./workshop.rsh:makeEnum', stdlib.UInt_max, 2));
                    const v571 = stdlib.mul(stdlib.checkedBigNumberify('./workshop.rsh:127:16:decimal', stdlib.UInt_max, 2), v48);
                    const v573 = v568 ? v47 : v59;
                    sim_r.txns.push({
                      amt: v571,
                      to: v573
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
                const v229 = txn7.value;
                const v234 = txn7.time;
                const v228 = txn7.from;
                const v230 = stdlib.eq(v229, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
                stdlib.assert(v230, {
                  at: 'reach standard library:68:7:dot',
                  fs: ['at ./workshop.rsh:110:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./workshop.rsh:116:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                  msg: 'pay amount correct',
                  who: 'Bob'
                   });
                const v231 = stdlib.addressEq(v47, v228);
                stdlib.assert(v231, {
                  at: 'reach standard library:68:7:dot',
                  fs: ['at ./workshop.rsh:110:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./workshop.rsh:116:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                  msg: 'sender correct',
                  who: 'Bob'
                   });
                const v233 = stdlib.add(v209, v229);
                ;
                stdlib.protect(ctc2, await interact.informTimeout(stdlib.checkedBigNumberify('./workshop.rsh:116:47:decimal', stdlib.UInt_max, 1)), {
                  at: './workshop.rsh:55:33:application',
                  fs: ['at ./workshop.rsh:54:13:application call to [unknown function] (defined at: ./workshop.rsh:54:25:function exp)', 'at ./workshop.rsh:110:74:application call to "informTimeout" (defined at: ./workshop.rsh:53:35:function exp)', 'at reach standard library:71:8:application call to "after" (defined at: ./workshop.rsh:110:57:function exp)', 'at ./workshop.rsh:110:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./workshop.rsh:116:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                  msg: 'informTimeout',
                  who: 'Bob'
                   });
                return;
                 }
              else {
                const [v214, v215] = txn6.data;
                const v216 = txn6.value;
                const v221 = txn6.time;
                const v213 = txn6.from;
                const v217 = stdlib.eq(v216, stdlib.checkedBigNumberify('./workshop.rsh:decimal', stdlib.UInt_max, 0));
                stdlib.assert(v217, {
                  at: './workshop.rsh:109:17:dot',
                  fs: ['at ./workshop.rsh:116:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                  msg: 'pay amount correct',
                  who: 'Bob'
                   });
                const v218 = stdlib.addressEq(v59, v213);
                stdlib.assert(v218, {
                  at: './workshop.rsh:109:17:dot',
                  fs: ['at ./workshop.rsh:116:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                  msg: 'sender correct',
                  who: 'Bob'
                   });
                const v220 = stdlib.add(v209, v216);
                const v223 = stdlib.digest(ctc4, [v214, v215]);
                const v224 = stdlib.eq(v193, v223);
                stdlib.assert(v224, {
                  at: 'reach standard library:65:17:application',
                  fs: ['at ./workshop.rsh:111:26:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)', 'at ./workshop.rsh:116:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                  msg: null,
                  who: 'Bob'
                   });
                const v305 = v204[stdlib.checkedBigNumberify('./workshop.rsh:28:18:array ref', stdlib.UInt_max, 0)];
                const v306 = v215[stdlib.checkedBigNumberify('./workshop.rsh:28:29:array ref', stdlib.UInt_max, 0)];
                const v308 = stdlib.sub(stdlib.checkedBigNumberify('./workshop.rsh:7:18:decimal', stdlib.UInt_max, 4), v306);
                const v309 = stdlib.add(v305, v308);
                const v310 = stdlib.mod(v309, stdlib.checkedBigNumberify('./workshop.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v314 = v204[stdlib.checkedBigNumberify('./workshop.rsh:28:18:array ref', stdlib.UInt_max, 1)];
                const v315 = v215[stdlib.checkedBigNumberify('./workshop.rsh:28:29:array ref', stdlib.UInt_max, 1)];
                const v317 = stdlib.sub(stdlib.checkedBigNumberify('./workshop.rsh:7:18:decimal', stdlib.UInt_max, 4), v315);
                const v318 = stdlib.add(v314, v317);
                const v319 = stdlib.mod(v318, stdlib.checkedBigNumberify('./workshop.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v323 = v204[stdlib.checkedBigNumberify('./workshop.rsh:28:18:array ref', stdlib.UInt_max, 2)];
                const v324 = v215[stdlib.checkedBigNumberify('./workshop.rsh:28:29:array ref', stdlib.UInt_max, 2)];
                const v326 = stdlib.sub(stdlib.checkedBigNumberify('./workshop.rsh:7:18:decimal', stdlib.UInt_max, 4), v324);
                const v327 = stdlib.add(v323, v326);
                const v328 = stdlib.mod(v327, stdlib.checkedBigNumberify('./workshop.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v332 = v204[stdlib.checkedBigNumberify('./workshop.rsh:28:18:array ref', stdlib.UInt_max, 3)];
                const v333 = v215[stdlib.checkedBigNumberify('./workshop.rsh:28:29:array ref', stdlib.UInt_max, 3)];
                const v335 = stdlib.sub(stdlib.checkedBigNumberify('./workshop.rsh:7:18:decimal', stdlib.UInt_max, 4), v333);
                const v336 = stdlib.add(v332, v335);
                const v337 = stdlib.mod(v336, stdlib.checkedBigNumberify('./workshop.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v341 = v204[stdlib.checkedBigNumberify('./workshop.rsh:28:18:array ref', stdlib.UInt_max, 4)];
                const v342 = v215[stdlib.checkedBigNumberify('./workshop.rsh:28:29:array ref', stdlib.UInt_max, 4)];
                const v344 = stdlib.sub(stdlib.checkedBigNumberify('./workshop.rsh:7:18:decimal', stdlib.UInt_max, 4), v342);
                const v345 = stdlib.add(v341, v344);
                const v346 = stdlib.mod(v345, stdlib.checkedBigNumberify('./workshop.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v359 = stdlib.eq(v310, stdlib.checkedBigNumberify('./workshop.rsh:makeEnum', stdlib.UInt_max, 1));
                const v360 = v359 ? v319 : v310;
                const v363 = stdlib.eq(v360, stdlib.checkedBigNumberify('./workshop.rsh:makeEnum', stdlib.UInt_max, 1));
                const v364 = v363 ? v328 : v360;
                const v367 = stdlib.eq(v364, stdlib.checkedBigNumberify('./workshop.rsh:makeEnum', stdlib.UInt_max, 1));
                const v368 = v367 ? v337 : v364;
                const v371 = stdlib.eq(v368, stdlib.checkedBigNumberify('./workshop.rsh:makeEnum', stdlib.UInt_max, 1));
                const v372 = v371 ? v346 : v368;
                const v375 = stdlib.add(v169, stdlib.checkedBigNumberify('./workshop.rsh:117:83:decimal', stdlib.UInt_max, 1));
                const cv166 = v204;
                const cv167 = v215;
                const cv168 = v372;
                const cv169 = v375;
                const cv626 = v220;
                const cv627 = v221;
                
                v166 = cv166;
                v167 = cv167;
                v168 = cv168;
                v169 = cv169;
                v626 = cv626;
                v627 = cv627;
                
                continue; }
               }
             }
           }
        else {
          const txn4 = await (ctc.recv('Bob', 14, 1, [ctc1], false, v49));
          if (txn4.didTimeout) {
            const txn5 = await (ctc.sendrecv('Bob', 15, 0, stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 6), [ctc5, ctc0, ctc0, ctc5, ctc0, ctc0, ctc0], [v47, v48, v49, v59, v169, v626, v627], stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), [], true, true, false, (async (txn5) => {
              const sim_r = { txns: [] };
              sim_r.prevSt = stdlib.digest(ctc6, [stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 6), v47, v48, v49, v59, v169, v626, v627]);
              sim_r.prevSt_noPrevTime = stdlib.digest(ctc7, [stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 6), v47, v48, v49, v59, v169, v626]);
              const [] = txn5.data;
              const v463 = txn5.value;
              const v468 = txn5.time;
              const v462 = txn5.from;
              
              const v464 = stdlib.eq(v463, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
              stdlib.assert(v464, {
                at: 'reach standard library:68:7:dot',
                fs: ['at ./workshop.rsh:97:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./workshop.rsh:120:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                msg: 'pay amount correct',
                who: 'Bob'
                 });
              const v465 = stdlib.addressEq(v59, v462);
              stdlib.assert(v465, {
                at: 'reach standard library:68:7:dot',
                fs: ['at ./workshop.rsh:97:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./workshop.rsh:120:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                msg: 'sender correct',
                who: 'Bob'
                 });
              const v467 = stdlib.add(v626, v463);
              sim_r.txns.push({
                amt: v467,
                to: v59
                 });
              sim_r.nextSt = stdlib.digest(ctc8, []);
              sim_r.nextSt_noTime = stdlib.digest(ctc8, []);
              sim_r.isHalt = true;
              
              return sim_r;
               })));
            const [] = txn5.data;
            const v463 = txn5.value;
            const v468 = txn5.time;
            const v462 = txn5.from;
            const v464 = stdlib.eq(v463, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v464, {
              at: 'reach standard library:68:7:dot',
              fs: ['at ./workshop.rsh:97:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./workshop.rsh:120:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
              msg: 'pay amount correct',
              who: 'Bob'
               });
            const v465 = stdlib.addressEq(v59, v462);
            stdlib.assert(v465, {
              at: 'reach standard library:68:7:dot',
              fs: ['at ./workshop.rsh:97:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./workshop.rsh:120:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
              msg: 'sender correct',
              who: 'Bob'
               });
            const v467 = stdlib.add(v626, v463);
            ;
            stdlib.protect(ctc2, await interact.informTimeout(stdlib.checkedBigNumberify('./workshop.rsh:120:47:decimal', stdlib.UInt_max, 0)), {
              at: './workshop.rsh:55:33:application',
              fs: ['at ./workshop.rsh:54:13:application call to [unknown function] (defined at: ./workshop.rsh:54:25:function exp)', 'at ./workshop.rsh:97:74:application call to "informTimeout" (defined at: ./workshop.rsh:53:35:function exp)', 'at reach standard library:71:8:application call to "after" (defined at: ./workshop.rsh:97:57:function exp)', 'at ./workshop.rsh:97:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./workshop.rsh:120:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
              msg: 'informTimeout',
              who: 'Bob'
               });
            return;
             }
          else {
            const [v385] = txn4.data;
            const v386 = txn4.value;
            const v391 = txn4.time;
            const v384 = txn4.from;
            const v387 = stdlib.eq(v386, stdlib.checkedBigNumberify('./workshop.rsh:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v387, {
              at: './workshop.rsh:96:17:dot',
              fs: ['at ./workshop.rsh:120:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
              msg: 'pay amount correct',
              who: 'Bob'
               });
            const v388 = stdlib.addressEq(v47, v384);
            stdlib.assert(v388, {
              at: './workshop.rsh:96:17:dot',
              fs: ['at ./workshop.rsh:120:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
              msg: 'sender correct',
              who: 'Bob'
               });
            const v390 = stdlib.add(v626, v386);
            const v394 = stdlib.protect(ctc3, await interact.getBatch(), {
              at: './workshop.rsh:102:61:application',
              fs: ['at ./workshop.rsh:101:22:application call to [unknown function] (defined at: ./workshop.rsh:101:26:function exp)', 'at ./workshop.rsh:120:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
              msg: 'getBatch',
              who: 'Bob'
               });
            const txn5 = await (ctc.sendrecv('Bob', 16, 1, stdlib.checkedBigNumberify('./workshop.rsh:103:18:dot', stdlib.UInt_max, 7), [ctc5, ctc0, ctc0, ctc5, ctc0, ctc1, ctc0, ctc0, ctc3], [v47, v48, v49, v59, v169, v385, v390, v391, v394], stdlib.checkedBigNumberify('./workshop.rsh:decimal', stdlib.UInt_max, 0), [ctc3], true, true, v49, (async (txn5) => {
              const sim_r = { txns: [] };
              sim_r.prevSt = stdlib.digest(ctc11, [stdlib.checkedBigNumberify('./workshop.rsh:103:18:dot', stdlib.UInt_max, 14), v47, v48, v49, v59, v169, v385, v390, v391]);
              sim_r.prevSt_noPrevTime = stdlib.digest(ctc12, [stdlib.checkedBigNumberify('./workshop.rsh:103:18:dot', stdlib.UInt_max, 14), v47, v48, v49, v59, v169, v385, v390]);
              const [v396] = txn5.data;
              const v397 = txn5.value;
              const v402 = txn5.time;
              const v395 = txn5.from;
              
              const v398 = stdlib.eq(v397, stdlib.checkedBigNumberify('./workshop.rsh:decimal', stdlib.UInt_max, 0));
              stdlib.assert(v398, {
                at: './workshop.rsh:103:18:dot',
                fs: ['at ./workshop.rsh:120:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                msg: 'pay amount correct',
                who: 'Bob'
                 });
              const v399 = stdlib.addressEq(v59, v395);
              stdlib.assert(v399, {
                at: './workshop.rsh:103:18:dot',
                fs: ['at ./workshop.rsh:120:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                msg: 'sender correct',
                who: 'Bob'
                 });
              const v401 = stdlib.add(v390, v397);
              sim_r.nextSt = stdlib.digest(ctc9, [stdlib.checkedBigNumberify('./workshop.rsh:105:19:after expr stmt semicolon', stdlib.UInt_max, 16), v47, v48, v49, v59, v169, v385, v396, v401, v402]);
              sim_r.nextSt_noTime = stdlib.digest(ctc10, [stdlib.checkedBigNumberify('./workshop.rsh:105:19:after expr stmt semicolon', stdlib.UInt_max, 16), v47, v48, v49, v59, v169, v385, v396, v401]);
              sim_r.isHalt = false;
              
              return sim_r;
               })));
            if (txn5.didTimeout) {
              const txn6 = await (ctc.recv('Bob', 17, 0, [], false, false));
              const [] = txn6.data;
              const v442 = txn6.value;
              const v447 = txn6.time;
              const v441 = txn6.from;
              const v443 = stdlib.eq(v442, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
              stdlib.assert(v443, {
                at: 'reach standard library:68:7:dot',
                fs: ['at ./workshop.rsh:104:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./workshop.rsh:120:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                msg: 'pay amount correct',
                who: 'Bob'
                 });
              const v444 = stdlib.addressEq(v47, v441);
              stdlib.assert(v444, {
                at: 'reach standard library:68:7:dot',
                fs: ['at ./workshop.rsh:104:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./workshop.rsh:120:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                msg: 'sender correct',
                who: 'Bob'
                 });
              const v446 = stdlib.add(v390, v442);
              ;
              stdlib.protect(ctc2, await interact.informTimeout(stdlib.checkedBigNumberify('./workshop.rsh:120:49:decimal', stdlib.UInt_max, 1)), {
                at: './workshop.rsh:55:33:application',
                fs: ['at ./workshop.rsh:54:13:application call to [unknown function] (defined at: ./workshop.rsh:54:25:function exp)', 'at ./workshop.rsh:104:73:application call to "informTimeout" (defined at: ./workshop.rsh:53:35:function exp)', 'at reach standard library:71:8:application call to "after" (defined at: ./workshop.rsh:104:56:function exp)', 'at ./workshop.rsh:104:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./workshop.rsh:120:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                msg: 'informTimeout',
                who: 'Bob'
                 });
              return;
               }
            else {
              const [v396] = txn5.data;
              const v397 = txn5.value;
              const v402 = txn5.time;
              const v395 = txn5.from;
              const v398 = stdlib.eq(v397, stdlib.checkedBigNumberify('./workshop.rsh:decimal', stdlib.UInt_max, 0));
              stdlib.assert(v398, {
                at: './workshop.rsh:103:18:dot',
                fs: ['at ./workshop.rsh:120:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                msg: 'pay amount correct',
                who: 'Bob'
                 });
              const v399 = stdlib.addressEq(v59, v395);
              stdlib.assert(v399, {
                at: './workshop.rsh:103:18:dot',
                fs: ['at ./workshop.rsh:120:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                msg: 'sender correct',
                who: 'Bob'
                 });
              const v401 = stdlib.add(v390, v397);
              const txn6 = await (ctc.recv('Bob', 18, 2, [ctc0, ctc3], false, v49));
              if (txn6.didTimeout) {
                const txn7 = await (ctc.sendrecv('Bob', 19, 0, stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 8), [ctc5, ctc0, ctc0, ctc5, ctc0, ctc1, ctc3, ctc0, ctc0], [v47, v48, v49, v59, v169, v385, v396, v401, v402], stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), [], true, true, false, (async (txn7) => {
                  const sim_r = { txns: [] };
                  sim_r.prevSt = stdlib.digest(ctc9, [stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 16), v47, v48, v49, v59, v169, v385, v396, v401, v402]);
                  sim_r.prevSt_noPrevTime = stdlib.digest(ctc10, [stdlib.checkedBigNumberify('reach standard library:68:7:dot', stdlib.UInt_max, 16), v47, v48, v49, v59, v169, v385, v396, v401]);
                  const [] = txn7.data;
                  const v421 = txn7.value;
                  const v426 = txn7.time;
                  const v420 = txn7.from;
                  
                  const v422 = stdlib.eq(v421, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
                  stdlib.assert(v422, {
                    at: 'reach standard library:68:7:dot',
                    fs: ['at ./workshop.rsh:110:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./workshop.rsh:120:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                    msg: 'pay amount correct',
                    who: 'Bob'
                     });
                  const v423 = stdlib.addressEq(v59, v420);
                  stdlib.assert(v423, {
                    at: 'reach standard library:68:7:dot',
                    fs: ['at ./workshop.rsh:110:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./workshop.rsh:120:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                    msg: 'sender correct',
                    who: 'Bob'
                     });
                  const v425 = stdlib.add(v401, v421);
                  sim_r.txns.push({
                    amt: v425,
                    to: v59
                     });
                  sim_r.nextSt = stdlib.digest(ctc8, []);
                  sim_r.nextSt_noTime = stdlib.digest(ctc8, []);
                  sim_r.isHalt = true;
                  
                  return sim_r;
                   })));
                const [] = txn7.data;
                const v421 = txn7.value;
                const v426 = txn7.time;
                const v420 = txn7.from;
                const v422 = stdlib.eq(v421, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
                stdlib.assert(v422, {
                  at: 'reach standard library:68:7:dot',
                  fs: ['at ./workshop.rsh:110:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./workshop.rsh:120:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                  msg: 'pay amount correct',
                  who: 'Bob'
                   });
                const v423 = stdlib.addressEq(v59, v420);
                stdlib.assert(v423, {
                  at: 'reach standard library:68:7:dot',
                  fs: ['at ./workshop.rsh:110:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./workshop.rsh:120:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                  msg: 'sender correct',
                  who: 'Bob'
                   });
                const v425 = stdlib.add(v401, v421);
                ;
                stdlib.protect(ctc2, await interact.informTimeout(stdlib.checkedBigNumberify('./workshop.rsh:120:47:decimal', stdlib.UInt_max, 0)), {
                  at: './workshop.rsh:55:33:application',
                  fs: ['at ./workshop.rsh:54:13:application call to [unknown function] (defined at: ./workshop.rsh:54:25:function exp)', 'at ./workshop.rsh:110:74:application call to "informTimeout" (defined at: ./workshop.rsh:53:35:function exp)', 'at reach standard library:71:8:application call to "after" (defined at: ./workshop.rsh:110:57:function exp)', 'at ./workshop.rsh:110:45:application call to "closeTo" (defined at: reach standard library:67:8:function exp)', 'at ./workshop.rsh:120:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                  msg: 'informTimeout',
                  who: 'Bob'
                   });
                return;
                 }
              else {
                const [v406, v407] = txn6.data;
                const v408 = txn6.value;
                const v413 = txn6.time;
                const v405 = txn6.from;
                const v409 = stdlib.eq(v408, stdlib.checkedBigNumberify('./workshop.rsh:decimal', stdlib.UInt_max, 0));
                stdlib.assert(v409, {
                  at: './workshop.rsh:109:17:dot',
                  fs: ['at ./workshop.rsh:120:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                  msg: 'pay amount correct',
                  who: 'Bob'
                   });
                const v410 = stdlib.addressEq(v47, v405);
                stdlib.assert(v410, {
                  at: './workshop.rsh:109:17:dot',
                  fs: ['at ./workshop.rsh:120:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                  msg: 'sender correct',
                  who: 'Bob'
                   });
                const v412 = stdlib.add(v401, v408);
                const v415 = stdlib.digest(ctc4, [v406, v407]);
                const v416 = stdlib.eq(v385, v415);
                stdlib.assert(v416, {
                  at: 'reach standard library:65:17:application',
                  fs: ['at ./workshop.rsh:111:26:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)', 'at ./workshop.rsh:120:42:application call to "doRound" (defined at: ./workshop.rsh:89:53:function exp)'],
                  msg: null,
                  who: 'Bob'
                   });
                const v497 = v407[stdlib.checkedBigNumberify('./workshop.rsh:28:18:array ref', stdlib.UInt_max, 0)];
                const v498 = v396[stdlib.checkedBigNumberify('./workshop.rsh:28:29:array ref', stdlib.UInt_max, 0)];
                const v500 = stdlib.sub(stdlib.checkedBigNumberify('./workshop.rsh:7:18:decimal', stdlib.UInt_max, 4), v498);
                const v501 = stdlib.add(v497, v500);
                const v502 = stdlib.mod(v501, stdlib.checkedBigNumberify('./workshop.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v506 = v407[stdlib.checkedBigNumberify('./workshop.rsh:28:18:array ref', stdlib.UInt_max, 1)];
                const v507 = v396[stdlib.checkedBigNumberify('./workshop.rsh:28:29:array ref', stdlib.UInt_max, 1)];
                const v509 = stdlib.sub(stdlib.checkedBigNumberify('./workshop.rsh:7:18:decimal', stdlib.UInt_max, 4), v507);
                const v510 = stdlib.add(v506, v509);
                const v511 = stdlib.mod(v510, stdlib.checkedBigNumberify('./workshop.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v515 = v407[stdlib.checkedBigNumberify('./workshop.rsh:28:18:array ref', stdlib.UInt_max, 2)];
                const v516 = v396[stdlib.checkedBigNumberify('./workshop.rsh:28:29:array ref', stdlib.UInt_max, 2)];
                const v518 = stdlib.sub(stdlib.checkedBigNumberify('./workshop.rsh:7:18:decimal', stdlib.UInt_max, 4), v516);
                const v519 = stdlib.add(v515, v518);
                const v520 = stdlib.mod(v519, stdlib.checkedBigNumberify('./workshop.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v524 = v407[stdlib.checkedBigNumberify('./workshop.rsh:28:18:array ref', stdlib.UInt_max, 3)];
                const v525 = v396[stdlib.checkedBigNumberify('./workshop.rsh:28:29:array ref', stdlib.UInt_max, 3)];
                const v527 = stdlib.sub(stdlib.checkedBigNumberify('./workshop.rsh:7:18:decimal', stdlib.UInt_max, 4), v525);
                const v528 = stdlib.add(v524, v527);
                const v529 = stdlib.mod(v528, stdlib.checkedBigNumberify('./workshop.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v533 = v407[stdlib.checkedBigNumberify('./workshop.rsh:28:18:array ref', stdlib.UInt_max, 4)];
                const v534 = v396[stdlib.checkedBigNumberify('./workshop.rsh:28:29:array ref', stdlib.UInt_max, 4)];
                const v536 = stdlib.sub(stdlib.checkedBigNumberify('./workshop.rsh:7:18:decimal', stdlib.UInt_max, 4), v534);
                const v537 = stdlib.add(v533, v536);
                const v538 = stdlib.mod(v537, stdlib.checkedBigNumberify('./workshop.rsh:7:32:decimal', stdlib.UInt_max, 3));
                const v551 = stdlib.eq(v502, stdlib.checkedBigNumberify('./workshop.rsh:makeEnum', stdlib.UInt_max, 1));
                const v552 = v551 ? v511 : v502;
                const v555 = stdlib.eq(v552, stdlib.checkedBigNumberify('./workshop.rsh:makeEnum', stdlib.UInt_max, 1));
                const v556 = v555 ? v520 : v552;
                const v559 = stdlib.eq(v556, stdlib.checkedBigNumberify('./workshop.rsh:makeEnum', stdlib.UInt_max, 1));
                const v560 = v559 ? v529 : v556;
                const v563 = stdlib.eq(v560, stdlib.checkedBigNumberify('./workshop.rsh:makeEnum', stdlib.UInt_max, 1));
                const v564 = v563 ? v538 : v560;
                const v567 = stdlib.add(v169, stdlib.checkedBigNumberify('./workshop.rsh:121:83:decimal', stdlib.UInt_max, 1));
                const cv166 = v407;
                const cv167 = v396;
                const cv168 = v564;
                const cv169 = v567;
                const cv626 = v412;
                const cv627 = v413;
                
                v166 = cv166;
                v167 = cv167;
                v168 = cv168;
                v169 = cv169;
                v626 = cv626;
                v627 = cv627;
                
                continue; }
               }
             }
           } }
      const v568 = stdlib.eq(v168, stdlib.checkedBigNumberify('./workshop.rsh:makeEnum', stdlib.UInt_max, 2));
      const v571 = stdlib.mul(stdlib.checkedBigNumberify('./workshop.rsh:127:16:decimal', stdlib.UInt_max, 2), v48);
      const v573 = v568 ? v47 : v59;
      ;
      stdlib.protect(ctc2, await interact.seeOutcome(v168, v166, v167), {
        at: './workshop.rsh:131:28:application',
        fs: ['at ./workshop.rsh:130:11:application call to [unknown function] (defined at: ./workshop.rsh:130:23:function exp)'],
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
// "./workshop.rsh:65:9:dot"
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
// "./workshop.rsh:65:9:dot"
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
// "./workshop.rsh:74:9:dot"
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
// "./workshop.rsh:74:9:dot"
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
// "[at ./workshop.rsh:76:41:application call to \"closeTo\" (defined at: reach standard library:67:8:function exp)]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "reach standard library:68:7:dot"
// "[at ./workshop.rsh:76:41:application call to \"closeTo\" (defined at: reach standard library:67:8:function exp)]"
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
// "./workshop.rsh:82:9:dot"
// "[]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "./workshop.rsh:82:9:dot"
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
// "[at ./workshop.rsh:84:22:application call to \"checkCommitment\" (defined at: reach standard library:64:8:function exp)]"
gtxna 0 ApplicationArgs 8
gtxna 0 ApplicationArgs 12
gtxna 0 ApplicationArgs 13
concat
keccak256
==
assert
int 0
itob
int 1
itob
concat
int 2
itob
concat
int 3
itob
concat
int 4
itob
concat
store 254
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
store 253
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
load 253
int 1
==
select
dup
store 252
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
load 252
int 1
==
select
dup
store 251
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
load 251
int 1
==
select
dup
store 250
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
load 250
int 1
==
select
dup
store 249
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
load 249
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
// "[at ./workshop.rsh:83:40:application call to \"closeTo\" (defined at: reach standard library:67:8:function exp)]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "reach standard library:68:7:dot"
// "[at ./workshop.rsh:83:40:application call to \"closeTo\" (defined at: reach standard library:67:8:function exp)]"
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
// "./workshop.rsh:96:17:dot"
// "[at ./workshop.rsh:116:42:application call to \"doRound\" (defined at: ./workshop.rsh:89:53:function exp)]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "./workshop.rsh:96:17:dot"
// "[at ./workshop.rsh:116:42:application call to \"doRound\" (defined at: ./workshop.rsh:89:53:function exp)]"
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
// "[at ./workshop.rsh:97:45:application call to \"closeTo\" (defined at: reach standard library:67:8:function exp),at ./workshop.rsh:116:42:application call to \"doRound\" (defined at: ./workshop.rsh:89:53:function exp)]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "reach standard library:68:7:dot"
// "[at ./workshop.rsh:97:45:application call to \"closeTo\" (defined at: reach standard library:67:8:function exp),at ./workshop.rsh:116:42:application call to \"doRound\" (defined at: ./workshop.rsh:89:53:function exp)]"
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
// "./workshop.rsh:103:18:dot"
// "[at ./workshop.rsh:116:42:application call to \"doRound\" (defined at: ./workshop.rsh:89:53:function exp)]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "./workshop.rsh:103:18:dot"
// "[at ./workshop.rsh:116:42:application call to \"doRound\" (defined at: ./workshop.rsh:89:53:function exp)]"
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
// "[at ./workshop.rsh:104:45:application call to \"closeTo\" (defined at: reach standard library:67:8:function exp),at ./workshop.rsh:116:42:application call to \"doRound\" (defined at: ./workshop.rsh:89:53:function exp)]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "reach standard library:68:7:dot"
// "[at ./workshop.rsh:104:45:application call to \"closeTo\" (defined at: reach standard library:67:8:function exp),at ./workshop.rsh:116:42:application call to \"doRound\" (defined at: ./workshop.rsh:89:53:function exp)]"
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
// "./workshop.rsh:109:17:dot"
// "[at ./workshop.rsh:116:42:application call to \"doRound\" (defined at: ./workshop.rsh:89:53:function exp)]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "./workshop.rsh:109:17:dot"
// "[at ./workshop.rsh:116:42:application call to \"doRound\" (defined at: ./workshop.rsh:89:53:function exp)]"
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
// "[at ./workshop.rsh:111:26:application call to \"checkCommitment\" (defined at: reach standard library:64:8:function exp),at ./workshop.rsh:116:42:application call to \"doRound\" (defined at: ./workshop.rsh:89:53:function exp)]"
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
// "[at ./workshop.rsh:110:45:application call to \"closeTo\" (defined at: reach standard library:67:8:function exp),at ./workshop.rsh:116:42:application call to \"doRound\" (defined at: ./workshop.rsh:89:53:function exp)]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "reach standard library:68:7:dot"
// "[at ./workshop.rsh:110:45:application call to \"closeTo\" (defined at: reach standard library:67:8:function exp),at ./workshop.rsh:116:42:application call to \"doRound\" (defined at: ./workshop.rsh:89:53:function exp)]"
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
// "./workshop.rsh:96:17:dot"
// "[at ./workshop.rsh:120:42:application call to \"doRound\" (defined at: ./workshop.rsh:89:53:function exp)]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "./workshop.rsh:96:17:dot"
// "[at ./workshop.rsh:120:42:application call to \"doRound\" (defined at: ./workshop.rsh:89:53:function exp)]"
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
// "[at ./workshop.rsh:97:45:application call to \"closeTo\" (defined at: reach standard library:67:8:function exp),at ./workshop.rsh:120:42:application call to \"doRound\" (defined at: ./workshop.rsh:89:53:function exp)]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "reach standard library:68:7:dot"
// "[at ./workshop.rsh:97:45:application call to \"closeTo\" (defined at: reach standard library:67:8:function exp),at ./workshop.rsh:120:42:application call to \"doRound\" (defined at: ./workshop.rsh:89:53:function exp)]"
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
// "./workshop.rsh:103:18:dot"
// "[at ./workshop.rsh:120:42:application call to \"doRound\" (defined at: ./workshop.rsh:89:53:function exp)]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "./workshop.rsh:103:18:dot"
// "[at ./workshop.rsh:120:42:application call to \"doRound\" (defined at: ./workshop.rsh:89:53:function exp)]"
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
// "[at ./workshop.rsh:104:45:application call to \"closeTo\" (defined at: reach standard library:67:8:function exp),at ./workshop.rsh:120:42:application call to \"doRound\" (defined at: ./workshop.rsh:89:53:function exp)]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "reach standard library:68:7:dot"
// "[at ./workshop.rsh:104:45:application call to \"closeTo\" (defined at: reach standard library:67:8:function exp),at ./workshop.rsh:120:42:application call to \"doRound\" (defined at: ./workshop.rsh:89:53:function exp)]"
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
// "./workshop.rsh:109:17:dot"
// "[at ./workshop.rsh:120:42:application call to \"doRound\" (defined at: ./workshop.rsh:89:53:function exp)]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "./workshop.rsh:109:17:dot"
// "[at ./workshop.rsh:120:42:application call to \"doRound\" (defined at: ./workshop.rsh:89:53:function exp)]"
gtxna 0 ApplicationArgs 5
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
// "[at ./workshop.rsh:111:26:application call to \"checkCommitment\" (defined at: reach standard library:64:8:function exp),at ./workshop.rsh:120:42:application call to \"doRound\" (defined at: ./workshop.rsh:89:53:function exp)]"
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
// "[at ./workshop.rsh:110:45:application call to \"closeTo\" (defined at: reach standard library:67:8:function exp),at ./workshop.rsh:120:42:application call to \"doRound\" (defined at: ./workshop.rsh:89:53:function exp)]"
gtxn 3 Amount
gtxna 0 ApplicationArgs 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "reach standard library:68:7:dot"
// "[at ./workshop.rsh:110:45:application call to \"closeTo\" (defined at: reach standard library:67:8:function exp),at ./workshop.rsh:120:42:application call to \"doRound\" (defined at: ./workshop.rsh:89:53:function exp)]"
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
                "name": "v169",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v193",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v198",
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
                "name": "v204",
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
                "name": "v169",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v193",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v198",
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
                "name": "v169",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v193",
                "type": "uint256"
              },
              {
                "internalType": "uint256[5]",
                "name": "v204",
                "type": "uint256[5]"
              },
              {
                "internalType": "uint256",
                "name": "v209",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v210",
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
                "name": "v169",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v193",
                "type": "uint256"
              },
              {
                "internalType": "uint256[5]",
                "name": "v204",
                "type": "uint256[5]"
              },
              {
                "internalType": "uint256",
                "name": "v209",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v210",
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
                "name": "v169",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v626",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v627",
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
                "name": "v169",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v626",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v627",
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
                "name": "v169",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v385",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v390",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v391",
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
                "name": "v396",
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
                "name": "v169",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v385",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v390",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v391",
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
                "name": "v169",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v385",
                "type": "uint256"
              },
              {
                "internalType": "uint256[5]",
                "name": "v396",
                "type": "uint256[5]"
              },
              {
                "internalType": "uint256",
                "name": "v401",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v402",
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
                "name": "v406",
                "type": "uint256"
              },
              {
                "internalType": "uint256[5]",
                "name": "v407",
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
                "name": "v169",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v385",
                "type": "uint256"
              },
              {
                "internalType": "uint256[5]",
                "name": "v396",
                "type": "uint256[5]"
              },
              {
                "internalType": "uint256",
                "name": "v401",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v402",
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
                "name": "v169",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v626",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v627",
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
                "name": "v193",
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
                "name": "v169",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v626",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v627",
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
                "name": "v169",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v193",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v198",
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
                "name": "v204",
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
                "name": "v169",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v193",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v198",
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
                "name": "v169",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v193",
                "type": "uint256"
              },
              {
                "internalType": "uint256[5]",
                "name": "v204",
                "type": "uint256[5]"
              },
              {
                "internalType": "uint256",
                "name": "v209",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v210",
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
                "name": "v169",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v193",
                "type": "uint256"
              },
              {
                "internalType": "uint256[5]",
                "name": "v204",
                "type": "uint256[5]"
              },
              {
                "internalType": "uint256",
                "name": "v209",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v210",
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
                "name": "v169",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v626",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v627",
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
                "name": "v169",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v626",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v627",
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
                "name": "v169",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v385",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v390",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v391",
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
                "name": "v396",
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
                "name": "v169",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v385",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v390",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v391",
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
                "name": "v169",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v385",
                "type": "uint256"
              },
              {
                "internalType": "uint256[5]",
                "name": "v396",
                "type": "uint256[5]"
              },
              {
                "internalType": "uint256",
                "name": "v401",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v402",
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
                "name": "v406",
                "type": "uint256"
              },
              {
                "internalType": "uint256[5]",
                "name": "v407",
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
                "name": "v169",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v385",
                "type": "uint256"
              },
              {
                "internalType": "uint256[5]",
                "name": "v396",
                "type": "uint256[5]"
              },
              {
                "internalType": "uint256",
                "name": "v401",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v402",
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
                "name": "v169",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v626",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v627",
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
                "name": "v193",
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
                "name": "v169",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v626",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v627",
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
  Bytecode: `0x608060408190527f49ff028a829527a47ec6839c7147b484eccf5a2a94853eddac09cef44d9d4e9e90600090a16040805160208082018352438252825180820184526000808252925181528351808301849052905181850152835180820385018152606090910190935282519201919091209055612a76806100826000396000f3fe6080604052600436106101025760003560e01c8063515d273111610095578063b2dc9b3c11610064578063b2dc9b3c146101f4578063b830ed6e14610207578063c74327551461021a578063d10ac9221461022d578063d22bdb581461024057610109565b8063515d2731146101a857806355b09c29146101bb5780636dac4fdc146101ce578063a701f0cc146101e157610109565b80633aa258ab116100d15780633aa258ab1461015c5780633df7a7891461016f578063497ccc6a146101825780634f4e2b061461019557610109565b8063253d71be1461010e5780632bf4f873146101235780632d8ee29e1461013657806338015b161461014957610109565b3661010957005b600080fd5b61012161011c3660046122b9565b610253565b005b610121610131366004612345565b61037b565b610121610144366004612329565b61051c565b6101216101573660046122f1565b610635565b61012161016a3660046122f1565b61074c565b61012161017d3660046122d5565b61085d565b6101216101903660046122d5565b610a15565b6101216101a33660046122b9565b610b26565b6101216101b6366004612356565b610cb9565b6101216101c936600461230d565b610e09565b6101216101dc3660046122d5565b611182565b6101216101ef36600461230d565b611299565b6101216102023660046122b9565b6115ac565b610121610215366004612329565b61173c565b6101216102283660046122d5565b61184f565b61012161023b3660046122a7565b611a04565b61012161024e366004612368565b611d0b565b604051610267906002908390602001612930565b6040516020818303038152906040528051906020012060001c6000541461028d57600080fd5b600080556102a460408201356101608301356129bc565b43101580156102b1575060015b6102ba57600080fd5b34156102c557600080fd5b336102d660a0830160808401612286565b6001600160a01b0316146102e957600080fd5b6102f960a0820160808301612286565b6001600160a01b03166108fc610314346101408501356129bc565b6040518115909202916000818181858888f1935050505015801561033c573d6000803e3d6000fd5b507fc18ab0af979eec50e5539334a3b97c236f4df25f2c0886da35b2f866708b0b138160405161036c91906126ba565b60405180910390a16000805533ff5b60408051600060208201528235918101919091526060016040516020818303038152906040528051906020012060001c600054146103b857600080fd5b60008080556040805160208101909152908152346020830135146103db57600080fd5b6103e63460006129bc565b815260408051833581526020808501359082015283820135818301526060808501359082015290517f2bb570a5feee0f446e450005a048c78efd478914692e1f0be1009bac144b11709181900360800190a161047a6040518060c0016040528060006001600160a01b0316815260200160008152602001600081526020016000815260200160008152602001600081525090565b338152602083810135818301908152604080860135818501908152606080880135818701908152875160808089019182524360a0808b01918252875160019a81019a909a528a516001600160a01b0316978a019790975296519388019390935292519186019190915251918401919091525160c08301525160e0820152610100015b60408051601f198184030181529190528051602090910120600055505050565b604051610530906010908390602001612906565b6040516020818303038152906040528051906020012060001c6000541461055657600080fd5b6000805561056d60408201356101808301356129bc565b431015801561057a575060015b61058357600080fd5b341561058e57600080fd5b3361059f6080830160608401612286565b6001600160a01b0316146105b257600080fd5b6105c26080820160608301612286565b6001600160a01b03166108fc6105dd346101608501356129bc565b6040518115909202916000818181858888f19350505050158015610605573d6000803e3d6000fd5b507fd6fbb3cbca99542c0c2ad4a426f274a3e1b0990f868dfa7680cc48800395e0fd8160405161036c919061277f565b604051610649906008908390602001612873565b6040516020818303038152906040528051906020012060001c6000541461066f57600080fd5b60008055610685604082013560e08301356129bc565b4310158015610692575060015b61069b57600080fd5b34156106a657600080fd5b336106b76080830160608401612286565b6001600160a01b0316146106ca57600080fd5b6106da6080820160608301612286565b6001600160a01b03166108fc6106f43460c08501356129bc565b6040518115909202916000818181858888f1935050505015801561071c573d6000803e3d6000fd5b507fb9d135d4afaa7938b4616c3637968c0e71f66413b54043da6188989930963f898160405161036c9190612750565b60405161076090600e908390602001612873565b6040516020818303038152906040528051906020012060001c6000541461078657600080fd5b6000805561079c604082013560e08301356129bc565b43101580156107a9575060015b6107b257600080fd5b34156107bd57600080fd5b336107cb6020830183612286565b6001600160a01b0316146107de57600080fd5b6107eb6020820182612286565b6001600160a01b03166108fc6108053460c08501356129bc565b6040518115909202916000818181858888f1935050505015801561082d573d6000803e3d6000fd5b507fe592bdad3da2420c74b7bd9841a064dd887014772397104fb0e8f350ee4ab2c68160405161036c9190612750565b6040516108719060069083906020016127fc565b6040516020818303038152906040528051906020012060001c6000541461089757600080fd5b6000808055604080516020810182529182526108ba9083013560c08401356129bc565b43106108c557600080fd5b34156108d057600080fd5b336108e16080840160608501612286565b6001600160a01b0316146108f457600080fd5b6109023460a08401356129bc565b81526040517f6739bb4acbb3812eea51c48895245b154776bf02dd61571666f6abba93266fec906109349084906126ed565b60405180910390a161099660405180610100016040528060006001600160a01b03168152602001600081526020016000815260200160006001600160a01b03168152602001600081526020016000815260200160008152602001600081525090565b6109a36020840184612286565b6001600160a01b0316815260208084013590820152604080840135908201526109d26080840160608501612286565b6001600160a01b031660608201526080808401359082015260e08084013560a0830152825160c083015243908201526040516104fc906008908390602001612888565b604051610a299060069083906020016127fc565b6040516020818303038152906040528051906020012060001c60005414610a4f57600080fd5b60008055610a65604082013560c08301356129bc565b4310158015610a72575060015b610a7b57600080fd5b3415610a8657600080fd5b33610a946020830183612286565b6001600160a01b031614610aa757600080fd5b610ab46020820182612286565b6001600160a01b03166108fc610ace3460a08501356129bc565b6040518115909202916000818181858888f19350505050158015610af6573d6000803e3d6000fd5b507fcdae4cbd433f8c4039f23f2632824e3ab0089e9b8c2050e8e87e4d6e0a3df09b8160405161036c919061270a565b604051610b3a90600e908390602001612873565b6040516020818303038152906040528051906020012060001c60005414610b6057600080fd5b600080805560408051602081018252918252610b839083013560e08401356129bc565b4310610b8e57600080fd5b3415610b9957600080fd5b33610baa6080840160608501612286565b6001600160a01b031614610bbd57600080fd5b610bcb3460c08401356129bc565b81526040517f1ce2536efdc067dd1e872bca971868b9750c7f8faa8b4eece19fa68af9cb89c090610bfd90849061273b565b60405180910390a1610c0d61205f565b610c1a6020840184612286565b6001600160a01b031681526020808401359082015260408084013590820152610c496080840160608501612286565b6001600160a01b031660608201526080808401359082015260a0808401358183015260408051808301909152906101008501906005908390839080828437600092019190915250505060c0820152815160e0820152436101008201526040516104fc90601090839060200161291b565b604051610ccd9060019083906020016128f2565b6040516020818303038152906040528051906020012060001c60005414610cf357600080fd5b600080805560408051602081018252918252610d169083013560a08401356129bc565b4310610d2157600080fd5b34602083013514610d3157600080fd5b610d3f3460808401356129bc565b81526040517f128ceb6e462bfebf9ef10870b6d9ae608efb33f1cdefac45d94895b5f28af76e90610d7190849061278e565b60405180910390a1610d816120c3565b610d8e6020840184612286565b6001600160a01b03168152602080840135908201526040808401358183015260608085013590830152336080830152805160a08181019092529060c08501906005908390839080828437600092019190915250505060a0820152815160c08201524360e08201526040516104fc906002908390602001612945565b604051610e1d906010908390602001612906565b6040516020818303038152906040528051906020012060001c60005414610e4357600080fd5b60008081905550610e756040518060800160405280600081526020016000815260200160008152602001600081525090565b610e8860408301356101808401356129bc565b4310610e9357600080fd5b3415610e9e57600080fd5b33610eac6020840184612286565b6001600160a01b031614610ebf57600080fd5b604051610edb906101a0840135906101c08501906020016127e2565b60408051601f19818403018152919052805160209091012060a083013514610f0257600080fd5b6003610f1360c084013560046129f3565b610f22906101c08501356129bc565b610f2c9190612a0a565b808252600114610f3d578051610f67565b6003610f4e60e084013560046129f3565b610f5d906101e08501356129bc565b610f679190612a0a565b60208201819052600114610f7f578060200151610faa565b6003610f9161010084013560046129f3565b610fa0906102008501356129bc565b610faa9190612a0a565b60408201819052600114610fc2578060400151610fed565b6003610fd461012084013560046129f3565b610fe3906102208501356129bc565b610fed9190612a0a565b60608201526040517f523e44f49032f50206e5ffcdb88da3392c2e81e3e5608b43c05b289ebe2c3b9e9061102290849061275f565b60405180910390a161103261210c565b61103f6020840184612286565b81516001600160a01b039091169052805160208085013591015280516040808501359101526110746080840160608501612286565b81516001600160a01b039091166060909101526040805160a0818101909252906101c0850190600590839083908082843760009201919091525050506020820151526040805160a08181019092529060c085019060059083908390808284376000920191909152505050602080830151015260608201516001146110fc57816060015161112f565b600361110e61014085013560046129f3565b6101c0850160045b602002013561112591906129bc565b61112f9190612a0a565b602082015160400152611147600160808501356129bc565b60208201516060015261115f346101608501356129bc565b60208201805160800191909152514360a09091015261117d81611e1c565b505050565b6040516111969060069083906020016127fc565b6040516020818303038152906040528051906020012060001c600054146111bc57600080fd5b600080556111d2604082013560c08301356129bc565b43101580156111df575060015b6111e857600080fd5b34156111f357600080fd5b336112046080830160608401612286565b6001600160a01b03161461121757600080fd5b6112276080820160608301612286565b6001600160a01b03166108fc6112413460a08501356129bc565b6040518115909202916000818181858888f19350505050158015611269573d6000803e3d6000fd5b507fe9b2c2b8b894a17634956282f47287e65395bf5f12d38d24114f9df9d064cb938160405161036c919061270a565b6040516112ad90600a908390602001612906565b6040516020818303038152906040528051906020012060001c600054146112d357600080fd5b600080819055506113056040518060800160405280600081526020016000815260200160008152602001600081525090565b61131860408301356101808401356129bc565b431061132357600080fd5b341561132e57600080fd5b3361133f6080840160608501612286565b6001600160a01b03161461135257600080fd5b60405161136e906101a0840135906101c08501906020016127e2565b60408051601f19818403018152919052805160209091012060a08301351461139557600080fd5b60036113a76101c084013560046129f3565b6113b59060c08501356129bc565b6113bf9190612a0a565b8082526001146113d05780516113fa565b60036113e26101e084013560046129f3565b6113f09060e08501356129bc565b6113fa9190612a0a565b6020820181905260011461141257806020015161143d565b600361142461020084013560046129f3565b611433906101008501356129bc565b61143d9190612a0a565b60408201819052600114611455578060400151611480565b600361146761022084013560046129f3565b611476906101208501356129bc565b6114809190612a0a565b60608201526040517f6ee71c1c32e4cf4bb84253364ba7d5a7f07dc28f60fbebebac51aae83206b872906114b590849061275f565b60405180910390a16114c561210c565b6114d26020840184612286565b81516001600160a01b039091169052805160208085013591015280516040808501359101526115076080840160608501612286565b81516001600160a01b039091166060909101526040805160a08181019092529060c0850190600590839083908082843760009201919091525050506020820151526040805160a0818101909252906101c0850190600590839083908082843760009201919091525050506020808301510152606082015160011461158f57816060015161112f565b60036115a161024085013560046129f3565b60c085016004611116565b6040516115c0906008908390602001612873565b6040516020818303038152906040528051906020012060001c600054146115e657600080fd5b6000808055604080516020810182529182526116099083013560e08401356129bc565b431061161457600080fd5b341561161f57600080fd5b3361162d6020840184612286565b6001600160a01b03161461164057600080fd5b61164e3460c08401356129bc565b81526040517f4b39e6355207f2c77bb98c443fb3e5e02a82c850a077da7e0938cd671b3644879061168090849061273b565b60405180910390a161169061205f565b61169d6020840184612286565b6001600160a01b0316815260208084013590820152604080840135908201526116cc6080840160608501612286565b6001600160a01b031660608201526080808401359082015260a0808401358183015260408051808301909152906101008501906005908390839080828437600092019190915250505060c0820152815160e0820152436101008201526040516104fc90600a90839060200161291b565b60405161175090600a908390602001612906565b6040516020818303038152906040528051906020012060001c6000541461177657600080fd5b6000805561178d60408201356101808301356129bc565b431015801561179a575060015b6117a357600080fd5b34156117ae57600080fd5b336117bc6020830183612286565b6001600160a01b0316146117cf57600080fd5b6117dc6020820182612286565b6001600160a01b03166108fc6117f7346101608501356129bc565b6040518115909202916000818181858888f1935050505015801561181f573d6000803e3d6000fd5b507ff145fb2384258d0f3c54ba74b14d362f8412b221ff369ece9d48eb2ad8e034cf8160405161036c919061277f565b6040516118639060069083906020016127fc565b6040516020818303038152906040528051906020012060001c6000541461188957600080fd5b6000808055604080516020810182529182526118ac9083013560c08401356129bc565b43106118b757600080fd5b34156118c257600080fd5b336118d06020840184612286565b6001600160a01b0316146118e357600080fd5b6118f13460a08401356129bc565b81526040517f8034f7b80d60c125999e5b76eb5d44b464ae8dc58c5f3258d4e1e4d905597898906119239084906126ed565b60405180910390a161198560405180610100016040528060006001600160a01b03168152602001600081526020016000815260200160006001600160a01b03168152602001600081526020016000815260200160008152602001600081525090565b6119926020840184612286565b6001600160a01b0316815260208084013590820152604080840135908201526119c16080840160608501612286565b6001600160a01b031660608201526080808401359082015260e08084013560a0830152825160c083015243908201526040516104fc90600e908390602001612888565b604051611a18906002908390602001612930565b6040516020818303038152906040528051906020012060001c60005414611a3e57600080fd5b60008055611a4a612148565b611a5d60408301356101608401356129bc565b4310611a6857600080fd5b3415611a7357600080fd5b33611a816020840184612286565b6001600160a01b031614611a9457600080fd5b604051611ab090610180840135906101a08501906020016127e2565b60408051601f198184030181529190528051602090910120606083013514611ad757600080fd5b8051600090528051600160209091015280516002604090910152805160036060909101819052815160046080909101819052611b189060a0850135906129f3565b611b27906101a08501356129bc565b611b319190612a0a565b60208201819052600114611b49578060200151611b73565b6003611b5a60c084013560046129f3565b611b69906101c08501356129bc565b611b739190612a0a565b60408201819052600114611b8b578060400151611bb5565b6003611b9c60e084013560046129f3565b611bab906101e08501356129bc565b611bb59190612a0a565b60608201819052600114611bcd578060600151611bf8565b6003611bdf61010084013560046129f3565b611bee906102008501356129bc565b611bf89190612a0a565b60808201526040517f85b984f93e23bb278dcf4c426e3ebada3df03a830c17f29b763cc686eeff603090611c2d908490612693565b60405180910390a1611c3d61210c565b611c4a6020840184612286565b81516001600160a01b03909116905280516020808501359101528051604080850135910152611c7f60a0840160808501612286565b81516001600160a01b03909116606090910152815160208083018051929092528351915101526080820151600114611cbb578160800151611ce6565b6003611ccd61012085013560046129f3565b611cdc906102208601356129bc565b611ce69190612a0a565b6020820180516040019190915251600060609091015261115f346101408501356129bc565b604051611d1f9060019083906020016128f2565b6040516020818303038152906040528051906020012060001c60005414611d4557600080fd5b60008055611d5b604082013560a08301356129bc565b4310158015611d68575060015b611d7157600080fd5b3415611d7c57600080fd5b33611d8a6020830183612286565b6001600160a01b031614611d9d57600080fd5b611daa6020820182612286565b6001600160a01b03166108fc611dc43460808501356129bc565b6040518115909202916000818181858888f19350505050158015611dec573d6000803e3d6000fd5b507f17040e3ed853a8df776cd092f1357f15488d98d460f66cd5e6b0cb07d5bc8ae38160405161036c91906127b2565b60018160200151604001511415611f815760006002826020015160600151611e449190612a0a565b1415611ee557611e5261217d565b8151516001600160a01b0390811682528251602090810151818401528351604090810151818501528451606090810151909316838501528185018051909301516080808601919091528351015160a08086019190915292519092015160c08401529051611ec491600691849101612811565b60408051601f19818403018152919052805160209091012060005550611f7c565b611eed61217d565b8151516001600160a01b0390811682528251602090810151818401528351604090810151818501528451606090810151909316838501528185018051909301516080808601919091528351015160a08086019190915292519092015160c08401529051611f5f91600691849101612811565b60408051601f198184030181529190528051602090910120600055505b611fef565b6040805160c081018252600081830181815260608084018381526080850184815260a086018581528487526020808801969096528851516001600160a01b03908116909552885186015190925287519092015190921690529084015190920151909152611fed81611ff2565b505b50565b80516060015160021461200a5780516040015161200e565b8051515b6001600160a01b03166108fc826000015160200151600261202f91906129d4565b6040518115909202916000818181858888f19350505050158015612057573d6000803e3d6000fd5b506000805533ff5b60405180610120016040528060006001600160a01b03168152602001600081526020016000815260200160006001600160a01b0316815260200160008152602001600081526020016120af6121cc565b815260200160008152602001600081525090565b60405180610100016040528060006001600160a01b0316815260200160008152602001600081526020016000815260200160006001600160a01b031681526020016120af6121cc565b6040805160c0810182526000918101828152606082018390526080820183905260a0820192909252908152602081016121436121ea565b905290565b6040518060a0016040528061215b6121cc565b8152602001600081526020016000815260200160008152602001600081525090565b6040518060e0016040528060006001600160a01b03168152602001600081526020016000815260200160006001600160a01b031681526020016000815260200160008152602001600081525090565b6040518060a001604052806005906020820280368337509192915050565b6040518060c001604052806121fd6121cc565b815260200161215b6121cc565b80356001600160a01b038116811461222157600080fd5b919050565b60006101a08284031215612238578081fd5b50919050565b60006101008284031215612238578081fd5b60006101208284031215612238578081fd5b60006102608284031215612238578081fd5b60006101c08284031215612238578081fd5b600060208284031215612297578081fd5b6122a08261220a565b9392505050565b60006102408284031215612238578081fd5b60006101a082840312156122cb578081fd5b6122a08383612226565b600061010082840312156122e7578081fd5b6122a0838361223e565b60006101208284031215612303578081fd5b6122a08383612250565b6000610260828403121561231f578081fd5b6122a08383612262565b60006101c0828403121561233b578081fd5b6122a08383612274565b600060808284031215612238578081fd5b60006101608284031215612238578081fd5b600060e08284031215612238578081fd5b8060005b600581101561239c57815184526020938401939091019060010161237d565b50505050565b8035825260a0602082016020840137600060c08301525050565b6001600160a01b03806123ce8361220a565b1683526020820135602084015260408201356040840152806123f26060840161220a565b166060840152506080810135608083015260a081013560a083015260c081013560c08301525050565b6001600160a01b038061242d8361220a565b1683526020820135602084015260408201356040840152806124516060840161220a565b166060840152506080810135608083015260a081013560a083015260c081013560c083015260e081013560e08301525050565b6001600160a01b036124958261220a565b1682526020810135602083015260408101356040830152606081013560608301526080810135608083015260a081013560a08301525050565b6001600160a01b03806124e08361220a565b1683526020820135602084015260408201356040840152806125046060840161220a565b166060840152506080810135608083015260a081013560a083015260a060c0820160c0840137610160818101359083015261018090810135910152565b60018060a01b0381511682526020810151602083015260408101516040830152606081015161257b60608401826001600160a01b03169052565b506080810151608083015260a081015160a083015260c08101516125a260c0840182612379565b5060e0810151610160830152610100015161018090910152565b6125c6828261241b565b61010060a0818301828501375060006101a08301525050565b6125e9828261241b565b610100808201358015158082146125ff57600080fd5b80838601525050505050565b61261582826124ce565b6101a0808201358015158082146125ff57600080fd5b6001600160a01b038061263d8361220a565b1683526020820135602084015260408201356040840152606082013560608401528061266b6080840161220a565b1660808401525060a080820160a0840137610140818101359083015261016090810135910152565b61024081016126a2828461262b565b6101806126b38184018286016123a2565b5092915050565b6101a081016126c9828461262b565b610180808401358015158082146126df57600080fd5b808386015250505092915050565b61010081016126fc82846123bc565b60e092830135919092015290565b610100810161271982846123bc565b60e083013580151580821461272d57600080fd5b8060e0850152505092915050565b6101a0810161274a82846125bc565b92915050565b610120810161274a82846125df565b610260810161276e82846124ce565b6101a06126b38184018286016123a2565b6101c0810161274a828461260b565b610160810161279d8284612484565b60a060c0840160c08401376000815292915050565b60e081016127c08284612484565b60c08301358015158082146127d457600080fd5b8060c0850152505092915050565b82815260c0810160a0836020840137600081529392505050565b82815261010081016122a060208301846123bc565b60006101008201905083825260018060a01b03808451166020840152602084015160408401526040840151606084015280606085015116608084015250608083015160a083015260a083015160c083015260c083015160e08301529392505050565b82815261012081016122a0602083018461241b565b82815261012081016122a0602083018460018060a01b038082511683526020820151602084015260408201516040840152806060830151166060840152506080810151608083015260a081015160a083015260c081015160c083015260e081015160e08301525050565b82815260e081016122a06020830184612484565b8281526101c081016122a060208301846124ce565b8281526101c081016122a06020830184612541565b8281526101a081016122a0602083018461262b565b60006101a08201905083825260018060a01b038084511660208401526020840151604084015260408401516060840152606084015160808401528060808501511660a08401525060a083015161299e60c0840182612379565b5060c083015161016083015260e08301516101808301529392505050565b600082198211156129cf576129cf612a2a565b500190565b60008160001904831182151516156129ee576129ee612a2a565b500290565b600082821015612a0557612a05612a2a565b500390565b600082612a2557634e487b7160e01b81526012600452602481fd5b500690565b634e487b7160e01b600052601160045260246000fdfea2646970667358221220f3a2b3b0199854bd490d95429dff78fe9ad12d11fb499b6cf9f70cd698a99e9c64736f6c63430008020033`,
  deployMode: `DM_constructor`
   };

export const _Connectors = {
  ALGO: _ALGO,
  ETH: _ETH
   };

