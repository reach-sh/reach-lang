// Automatically generated with Reach 0.1.3
/* eslint-disable */
export const _version = '0.1.3';
export const _backendVersion = 1;


export function getExports(s) {
  const stdlib = s.reachStdlib;
  return {
    };
  };

export function _getViews(s, viewlib) {
  const stdlib = s.reachStdlib;
  
  return {
    infos: {
      },
    views: {
      }
    };
  
  };

export function _getMaps(s) {
  const stdlib = s.reachStdlib;
  const ctc0 = stdlib.T_Tuple([]);
  return {
    mapDataTy: ctc0
    };
  };

export async function Alice(ctc, interact) {
  if (ctc.sendrecv === undefined) {
    return Promise.reject(new Error(`The backend for Alice expects to receive a contract as its first argument.`));}
  if (typeof(interact) !== 'object') {
    return Promise.reject(new Error(`The backend for Alice expects to receive an interact object as its second argument.`));}
  const stdlib = ctc.stdlib;
  const ctc0 = stdlib.T_UInt;
  const ctc1 = stdlib.T_Tuple([ctc0, ctc0]);
  const ctc2 = stdlib.T_Digest;
  const ctc3 = stdlib.T_Null;
  const ctc4 = stdlib.T_Address;
  
  
  const v42 = await ctc.creationTime();
  const v43 = await ctc.creationSecs();
  
  const v40 = stdlib.protect(ctc0, interact.deadline, 'for Alice\'s interact field deadline');
  const v41 = stdlib.protect(ctc0, interact.wager, 'for Alice\'s interact field wager');
  
  const txn1 = await (ctc.sendrecv({
    args: [v41, v40],
    evt_cnt: 2,
    funcNum: 1,
    onlyIf: true,
    out_tys: [ctc0, ctc0],
    pay: [v41, []],
    sim_p: (async (txn1) => {
      const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
      
      const [v48, v49] = txn1.data;
      const v52 = txn1.time;
      const v53 = txn1.secs;
      const v47 = txn1.from;
      
      sim_r.txns.push({
        amt: v48,
        kind: 'to',
        tok: undefined
        });
      const v209 = stdlib.add(v52, v49);
      sim_r.isHalt = false;
      
      return sim_r;
      }),
    soloSend: true,
    timeoutAt: undefined,
    tys: [ctc0, ctc0],
    waitIfNotPresent: false
    }));
  const [v48, v49] = txn1.data;
  const v52 = txn1.time;
  const v53 = txn1.secs;
  const v47 = txn1.from;
  ;
  const v209 = stdlib.add(v52, v49);
  const txn2 = await (ctc.recv({
    evt_cnt: 0,
    funcNum: 2,
    out_tys: [],
    timeoutAt: ['time', v209],
    waitIfNotPresent: false
    }));
  if (txn2.didTimeout) {
    const txn3 = await (ctc.sendrecv({
      args: [v47, v48, v49, v209],
      evt_cnt: 0,
      funcNum: 3,
      onlyIf: true,
      out_tys: [],
      pay: [stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), []],
      sim_p: (async (txn3) => {
        const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
        
        const [] = txn3.data;
        const v215 = txn3.time;
        const v216 = txn3.secs;
        const v212 = txn3.from;
        
        sim_r.txns.push({
          amt: stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0),
          kind: 'to',
          tok: undefined
          });
        const v214 = stdlib.addressEq(v47, v212);
        stdlib.assert(v214, {
          at: 'reach standard library:209:7:dot',
          fs: ['at ./index.rsh:57:37:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
          msg: 'sender correct',
          who: 'Alice'
          });
        sim_r.txns.push({
          amt: v48,
          kind: 'from',
          to: v47,
          tok: undefined
          });
        sim_r.txns.push({
          kind: 'halt',
          tok: undefined
          })
        sim_r.isHalt = true;
        
        return sim_r;
        }),
      soloSend: true,
      timeoutAt: undefined,
      tys: [ctc4, ctc0, ctc0, ctc0],
      waitIfNotPresent: false
      }));
    const [] = txn3.data;
    const v215 = txn3.time;
    const v216 = txn3.secs;
    const v212 = txn3.from;
    ;
    const v214 = stdlib.addressEq(v47, v212);
    stdlib.assert(v214, {
      at: 'reach standard library:209:7:dot',
      fs: ['at ./index.rsh:57:37:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
      msg: 'sender correct',
      who: 'Alice'
      });
    ;
    stdlib.protect(ctc3, await interact.informTimeout(), {
      at: './index.rsh:41:29:application',
      fs: ['at ./index.rsh:40:9:application call to [unknown function] (defined at: ./index.rsh:40:27:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./index.rsh:39:28:function exp)', 'at ./index.rsh:57:37:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
      msg: 'informTimeout',
      who: 'Alice'
      });
    
    return;
    }
  else {
    const [] = txn2.data;
    const v60 = txn2.time;
    const v61 = txn2.secs;
    const v57 = txn2.from;
    const v59 = stdlib.add(v48, v48);
    ;
    let v62 = stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1);
    let v233 = v60;
    let v239 = v59;
    
    while ((() => {
      const v73 = stdlib.eq(v62, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
      
      return v73;})()) {
      const v168 = stdlib.add(v233, v49);
      const v77 = stdlib.protect(ctc0, await interact.getHand(), {
        at: './index.rsh:65:42:application',
        fs: ['at ./index.rsh:64:15:application call to [unknown function] (defined at: ./index.rsh:64:19:function exp)'],
        msg: 'getHand',
        who: 'Alice'
        });
      const v79 = stdlib.protect(ctc0, await interact.random(), {
        at: 'reach standard library:60:31:application',
        fs: ['at ./index.rsh:66:56:application call to "makeCommitment" (defined at: reach standard library:59:8:function exp)', 'at ./index.rsh:64:15:application call to [unknown function] (defined at: ./index.rsh:64:19:function exp)'],
        msg: 'random',
        who: 'Alice'
        });
      const v80 = stdlib.digest(ctc1, [v79, v77]);
      
      const txn3 = await (ctc.sendrecv({
        args: [v47, v48, v49, v57, v168, v239, v80],
        evt_cnt: 1,
        funcNum: 6,
        onlyIf: true,
        out_tys: [ctc2],
        pay: [stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), []],
        sim_p: (async (txn3) => {
          const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
          
          const [v83] = txn3.data;
          const v86 = txn3.time;
          const v87 = txn3.secs;
          const v82 = txn3.from;
          
          sim_r.txns.push({
            amt: stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0),
            kind: 'to',
            tok: undefined
            });
          const v85 = stdlib.addressEq(v47, v82);
          stdlib.assert(v85, {
            at: './index.rsh:69:11:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Alice'
            });
          const v143 = stdlib.add(v86, v49);
          sim_r.isHalt = false;
          
          return sim_r;
          }),
        soloSend: true,
        timeoutAt: ['time', v168],
        tys: [ctc4, ctc0, ctc0, ctc4, ctc0, ctc0, ctc2],
        waitIfNotPresent: false
        }));
      if (txn3.didTimeout) {
        const txn4 = await (ctc.recv({
          evt_cnt: 0,
          funcNum: 7,
          out_tys: [],
          timeoutAt: undefined,
          waitIfNotPresent: false
          }));
        const [] = txn4.data;
        const v174 = txn4.time;
        const v175 = txn4.secs;
        const v171 = txn4.from;
        ;
        const v173 = stdlib.addressEq(v57, v171);
        stdlib.assert(v173, {
          at: 'reach standard library:209:7:dot',
          fs: ['at ./index.rsh:70:39:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
          msg: 'sender correct',
          who: 'Alice'
          });
        ;
        stdlib.protect(ctc3, await interact.informTimeout(), {
          at: './index.rsh:41:29:application',
          fs: ['at ./index.rsh:40:9:application call to [unknown function] (defined at: ./index.rsh:40:27:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./index.rsh:39:28:function exp)', 'at ./index.rsh:70:39:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
          msg: 'informTimeout',
          who: 'Alice'
          });
        
        return;
        }
      else {
        const [v83] = txn3.data;
        const v86 = txn3.time;
        const v87 = txn3.secs;
        const v82 = txn3.from;
        ;
        const v85 = stdlib.addressEq(v47, v82);
        stdlib.assert(v85, {
          at: './index.rsh:69:11:dot',
          fs: [],
          msg: 'sender correct',
          who: 'Alice'
          });
        const v143 = stdlib.add(v86, v49);
        const txn4 = await (ctc.recv({
          evt_cnt: 1,
          funcNum: 8,
          out_tys: [ctc0],
          timeoutAt: ['time', v143],
          waitIfNotPresent: false
          }));
        if (txn4.didTimeout) {
          const txn5 = await (ctc.sendrecv({
            args: [v47, v48, v49, v57, v83, v143, v239],
            evt_cnt: 0,
            funcNum: 9,
            onlyIf: true,
            out_tys: [],
            pay: [stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), []],
            sim_p: (async (txn5) => {
              const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
              
              const [] = txn5.data;
              const v149 = txn5.time;
              const v150 = txn5.secs;
              const v146 = txn5.from;
              
              sim_r.txns.push({
                amt: stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0),
                kind: 'to',
                tok: undefined
                });
              const v148 = stdlib.addressEq(v47, v146);
              stdlib.assert(v148, {
                at: 'reach standard library:209:7:dot',
                fs: ['at ./index.rsh:78:39:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
                msg: 'sender correct',
                who: 'Alice'
                });
              sim_r.txns.push({
                amt: v239,
                kind: 'from',
                to: v47,
                tok: undefined
                });
              sim_r.txns.push({
                kind: 'halt',
                tok: undefined
                })
              sim_r.isHalt = true;
              
              return sim_r;
              }),
            soloSend: true,
            timeoutAt: undefined,
            tys: [ctc4, ctc0, ctc0, ctc4, ctc2, ctc0, ctc0],
            waitIfNotPresent: false
            }));
          const [] = txn5.data;
          const v149 = txn5.time;
          const v150 = txn5.secs;
          const v146 = txn5.from;
          ;
          const v148 = stdlib.addressEq(v47, v146);
          stdlib.assert(v148, {
            at: 'reach standard library:209:7:dot',
            fs: ['at ./index.rsh:78:39:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
            msg: 'sender correct',
            who: 'Alice'
            });
          ;
          stdlib.protect(ctc3, await interact.informTimeout(), {
            at: './index.rsh:41:29:application',
            fs: ['at ./index.rsh:40:9:application call to [unknown function] (defined at: ./index.rsh:40:27:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./index.rsh:39:28:function exp)', 'at ./index.rsh:78:39:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
            msg: 'informTimeout',
            who: 'Alice'
            });
          
          return;
          }
        else {
          const [v93] = txn4.data;
          const v96 = txn4.time;
          const v97 = txn4.secs;
          const v92 = txn4.from;
          ;
          const v95 = stdlib.addressEq(v57, v92);
          stdlib.assert(v95, {
            at: './index.rsh:77:9:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Alice'
            });
          const v118 = stdlib.add(v96, v49);
          const txn5 = await (ctc.sendrecv({
            args: [v47, v48, v49, v57, v83, v93, v118, v239, v79, v77],
            evt_cnt: 2,
            funcNum: 10,
            onlyIf: true,
            out_tys: [ctc0, ctc0],
            pay: [stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), []],
            sim_p: (async (txn5) => {
              const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
              
              const [v102, v103] = txn5.data;
              const v106 = txn5.time;
              const v107 = txn5.secs;
              const v101 = txn5.from;
              
              sim_r.txns.push({
                amt: stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0),
                kind: 'to',
                tok: undefined
                });
              const v105 = stdlib.addressEq(v47, v101);
              stdlib.assert(v105, {
                at: './index.rsh:85:11:dot',
                fs: [],
                msg: 'sender correct',
                who: 'Alice'
                });
              const v109 = stdlib.digest(ctc1, [v102, v103]);
              const v110 = stdlib.digestEq(v83, v109);
              stdlib.assert(v110, {
                at: 'reach standard library:65:17:application',
                fs: ['at ./index.rsh:87:20:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)'],
                msg: null,
                who: 'Alice'
                });
              const v112 = stdlib.sub(stdlib.checkedBigNumberify('./index.rsh:7:14:decimal', stdlib.UInt_max, 4), v93);
              const v113 = stdlib.add(v103, v112);
              const v114 = stdlib.mod(v113, stdlib.checkedBigNumberify('./index.rsh:7:28:decimal', stdlib.UInt_max, 3));
              const cv62 = v114;
              const cv233 = v106;
              const cv239 = v239;
              
              (() => {
                const v62 = cv62;
                const v233 = cv233;
                const v239 = cv239;
                
                if ((() => {
                  const v73 = stdlib.eq(v62, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
                  
                  return v73;})()) {
                  const v168 = stdlib.add(v233, v49);
                  sim_r.isHalt = false;
                  }
                else {
                  const v190 = stdlib.eq(v62, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
                  const v193 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:94:12:decimal', stdlib.UInt_max, 2), v48);
                  const v195 = v190 ? v47 : v57;
                  sim_r.txns.push({
                    amt: v193,
                    kind: 'from',
                    to: v195,
                    tok: undefined
                    });
                  sim_r.txns.push({
                    kind: 'halt',
                    tok: undefined
                    })
                  sim_r.isHalt = true;
                  }})();
              return sim_r;
              }),
            soloSend: true,
            timeoutAt: ['time', v118],
            tys: [ctc4, ctc0, ctc0, ctc4, ctc2, ctc0, ctc0, ctc0, ctc0, ctc0],
            waitIfNotPresent: false
            }));
          if (txn5.didTimeout) {
            const txn6 = await (ctc.recv({
              evt_cnt: 0,
              funcNum: 11,
              out_tys: [],
              timeoutAt: undefined,
              waitIfNotPresent: false
              }));
            const [] = txn6.data;
            const v124 = txn6.time;
            const v125 = txn6.secs;
            const v121 = txn6.from;
            ;
            const v123 = stdlib.addressEq(v57, v121);
            stdlib.assert(v123, {
              at: 'reach standard library:209:7:dot',
              fs: ['at ./index.rsh:86:39:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
              msg: 'sender correct',
              who: 'Alice'
              });
            ;
            stdlib.protect(ctc3, await interact.informTimeout(), {
              at: './index.rsh:41:29:application',
              fs: ['at ./index.rsh:40:9:application call to [unknown function] (defined at: ./index.rsh:40:27:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./index.rsh:39:28:function exp)', 'at ./index.rsh:86:39:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
              msg: 'informTimeout',
              who: 'Alice'
              });
            
            return;
            }
          else {
            const [v102, v103] = txn5.data;
            const v106 = txn5.time;
            const v107 = txn5.secs;
            const v101 = txn5.from;
            ;
            const v105 = stdlib.addressEq(v47, v101);
            stdlib.assert(v105, {
              at: './index.rsh:85:11:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Alice'
              });
            const v109 = stdlib.digest(ctc1, [v102, v103]);
            const v110 = stdlib.digestEq(v83, v109);
            stdlib.assert(v110, {
              at: 'reach standard library:65:17:application',
              fs: ['at ./index.rsh:87:20:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)'],
              msg: null,
              who: 'Alice'
              });
            const v112 = stdlib.sub(stdlib.checkedBigNumberify('./index.rsh:7:14:decimal', stdlib.UInt_max, 4), v93);
            const v113 = stdlib.add(v103, v112);
            const v114 = stdlib.mod(v113, stdlib.checkedBigNumberify('./index.rsh:7:28:decimal', stdlib.UInt_max, 3));
            const cv62 = v114;
            const cv233 = v106;
            const cv239 = v239;
            
            v62 = cv62;
            v233 = cv233;
            v239 = cv239;
            
            continue;}
          }
        }
      }
    const v190 = stdlib.eq(v62, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
    const v193 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:94:12:decimal', stdlib.UInt_max, 2), v48);
    const v195 = v190 ? v47 : v57;
    ;
    stdlib.protect(ctc3, await interact.seeOutcome(v62), {
      at: './index.rsh:98:24:application',
      fs: ['at ./index.rsh:97:7:application call to [unknown function] (defined at: ./index.rsh:97:25:function exp)'],
      msg: 'seeOutcome',
      who: 'Alice'
      });
    
    return;}
  
  
  };
export async function Bob(ctc, interact) {
  if (ctc.sendrecv === undefined) {
    return Promise.reject(new Error(`The backend for Bob expects to receive a contract as its first argument.`));}
  if (typeof(interact) !== 'object') {
    return Promise.reject(new Error(`The backend for Bob expects to receive an interact object as its second argument.`));}
  const stdlib = ctc.stdlib;
  const ctc0 = stdlib.T_UInt;
  const ctc1 = stdlib.T_Null;
  const ctc2 = stdlib.T_Digest;
  const ctc3 = stdlib.T_Tuple([ctc0, ctc0]);
  const ctc4 = stdlib.T_Address;
  
  
  const v42 = await ctc.creationTime();
  const v43 = await ctc.creationSecs();
  
  const txn1 = await (ctc.recv({
    evt_cnt: 2,
    funcNum: 1,
    out_tys: [ctc0, ctc0],
    timeoutAt: undefined,
    waitIfNotPresent: false
    }));
  const [v48, v49] = txn1.data;
  const v52 = txn1.time;
  const v53 = txn1.secs;
  const v47 = txn1.from;
  ;
  const v209 = stdlib.add(v52, v49);
  stdlib.protect(ctc1, await interact.acceptWager(v48), {
    at: './index.rsh:54:25:application',
    fs: ['at ./index.rsh:53:11:application call to [unknown function] (defined at: ./index.rsh:53:15:function exp)'],
    msg: 'acceptWager',
    who: 'Bob'
    });
  
  const txn2 = await (ctc.sendrecv({
    args: [v47, v48, v49, v209],
    evt_cnt: 0,
    funcNum: 2,
    onlyIf: true,
    out_tys: [],
    pay: [v48, []],
    sim_p: (async (txn2) => {
      const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
      
      const [] = txn2.data;
      const v60 = txn2.time;
      const v61 = txn2.secs;
      const v57 = txn2.from;
      
      const v59 = stdlib.add(v48, v48);
      sim_r.txns.push({
        amt: v48,
        kind: 'to',
        tok: undefined
        });
      const v62 = stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1);
      const v233 = v60;
      const v239 = v59;
      
      if ((() => {
        const v73 = stdlib.eq(v62, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
        
        return v73;})()) {
        const v168 = stdlib.add(v233, v49);
        sim_r.isHalt = false;
        }
      else {
        const v190 = stdlib.eq(v62, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
        const v193 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:94:12:decimal', stdlib.UInt_max, 2), v48);
        const v195 = v190 ? v47 : v57;
        sim_r.txns.push({
          amt: v193,
          kind: 'from',
          to: v195,
          tok: undefined
          });
        sim_r.txns.push({
          kind: 'halt',
          tok: undefined
          })
        sim_r.isHalt = true;
        }
      return sim_r;
      }),
    soloSend: true,
    timeoutAt: ['time', v209],
    tys: [ctc4, ctc0, ctc0, ctc0],
    waitIfNotPresent: false
    }));
  if (txn2.didTimeout) {
    const txn3 = await (ctc.recv({
      evt_cnt: 0,
      funcNum: 3,
      out_tys: [],
      timeoutAt: undefined,
      waitIfNotPresent: false
      }));
    const [] = txn3.data;
    const v215 = txn3.time;
    const v216 = txn3.secs;
    const v212 = txn3.from;
    ;
    const v214 = stdlib.addressEq(v47, v212);
    stdlib.assert(v214, {
      at: 'reach standard library:209:7:dot',
      fs: ['at ./index.rsh:57:37:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
      msg: 'sender correct',
      who: 'Bob'
      });
    ;
    stdlib.protect(ctc1, await interact.informTimeout(), {
      at: './index.rsh:41:29:application',
      fs: ['at ./index.rsh:40:9:application call to [unknown function] (defined at: ./index.rsh:40:27:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./index.rsh:39:28:function exp)', 'at ./index.rsh:57:37:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
      msg: 'informTimeout',
      who: 'Bob'
      });
    
    return;
    }
  else {
    const [] = txn2.data;
    const v60 = txn2.time;
    const v61 = txn2.secs;
    const v57 = txn2.from;
    const v59 = stdlib.add(v48, v48);
    ;
    let v62 = stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1);
    let v233 = v60;
    let v239 = v59;
    
    while ((() => {
      const v73 = stdlib.eq(v62, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
      
      return v73;})()) {
      const v168 = stdlib.add(v233, v49);
      const txn3 = await (ctc.recv({
        evt_cnt: 1,
        funcNum: 6,
        out_tys: [ctc2],
        timeoutAt: ['time', v168],
        waitIfNotPresent: false
        }));
      if (txn3.didTimeout) {
        const txn4 = await (ctc.sendrecv({
          args: [v47, v48, v49, v57, v168, v239],
          evt_cnt: 0,
          funcNum: 7,
          onlyIf: true,
          out_tys: [],
          pay: [stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), []],
          sim_p: (async (txn4) => {
            const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
            
            const [] = txn4.data;
            const v174 = txn4.time;
            const v175 = txn4.secs;
            const v171 = txn4.from;
            
            sim_r.txns.push({
              amt: stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0),
              kind: 'to',
              tok: undefined
              });
            const v173 = stdlib.addressEq(v57, v171);
            stdlib.assert(v173, {
              at: 'reach standard library:209:7:dot',
              fs: ['at ./index.rsh:70:39:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
              msg: 'sender correct',
              who: 'Bob'
              });
            sim_r.txns.push({
              amt: v239,
              kind: 'from',
              to: v57,
              tok: undefined
              });
            sim_r.txns.push({
              kind: 'halt',
              tok: undefined
              })
            sim_r.isHalt = true;
            
            return sim_r;
            }),
          soloSend: true,
          timeoutAt: undefined,
          tys: [ctc4, ctc0, ctc0, ctc4, ctc0, ctc0],
          waitIfNotPresent: false
          }));
        const [] = txn4.data;
        const v174 = txn4.time;
        const v175 = txn4.secs;
        const v171 = txn4.from;
        ;
        const v173 = stdlib.addressEq(v57, v171);
        stdlib.assert(v173, {
          at: 'reach standard library:209:7:dot',
          fs: ['at ./index.rsh:70:39:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
          msg: 'sender correct',
          who: 'Bob'
          });
        ;
        stdlib.protect(ctc1, await interact.informTimeout(), {
          at: './index.rsh:41:29:application',
          fs: ['at ./index.rsh:40:9:application call to [unknown function] (defined at: ./index.rsh:40:27:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./index.rsh:39:28:function exp)', 'at ./index.rsh:70:39:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
          msg: 'informTimeout',
          who: 'Bob'
          });
        
        return;
        }
      else {
        const [v83] = txn3.data;
        const v86 = txn3.time;
        const v87 = txn3.secs;
        const v82 = txn3.from;
        ;
        const v85 = stdlib.addressEq(v47, v82);
        stdlib.assert(v85, {
          at: './index.rsh:69:11:dot',
          fs: [],
          msg: 'sender correct',
          who: 'Bob'
          });
        const v143 = stdlib.add(v86, v49);
        const v91 = stdlib.protect(ctc0, await interact.getHand(), {
          at: './index.rsh:75:50:application',
          fs: ['at ./index.rsh:74:13:application call to [unknown function] (defined at: ./index.rsh:74:17:function exp)'],
          msg: 'getHand',
          who: 'Bob'
          });
        
        const txn4 = await (ctc.sendrecv({
          args: [v47, v48, v49, v57, v83, v143, v239, v91],
          evt_cnt: 1,
          funcNum: 8,
          onlyIf: true,
          out_tys: [ctc0],
          pay: [stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), []],
          sim_p: (async (txn4) => {
            const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
            
            const [v93] = txn4.data;
            const v96 = txn4.time;
            const v97 = txn4.secs;
            const v92 = txn4.from;
            
            sim_r.txns.push({
              amt: stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0),
              kind: 'to',
              tok: undefined
              });
            const v95 = stdlib.addressEq(v57, v92);
            stdlib.assert(v95, {
              at: './index.rsh:77:9:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Bob'
              });
            const v118 = stdlib.add(v96, v49);
            sim_r.isHalt = false;
            
            return sim_r;
            }),
          soloSend: true,
          timeoutAt: ['time', v143],
          tys: [ctc4, ctc0, ctc0, ctc4, ctc2, ctc0, ctc0, ctc0],
          waitIfNotPresent: false
          }));
        if (txn4.didTimeout) {
          const txn5 = await (ctc.recv({
            evt_cnt: 0,
            funcNum: 9,
            out_tys: [],
            timeoutAt: undefined,
            waitIfNotPresent: false
            }));
          const [] = txn5.data;
          const v149 = txn5.time;
          const v150 = txn5.secs;
          const v146 = txn5.from;
          ;
          const v148 = stdlib.addressEq(v47, v146);
          stdlib.assert(v148, {
            at: 'reach standard library:209:7:dot',
            fs: ['at ./index.rsh:78:39:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
            msg: 'sender correct',
            who: 'Bob'
            });
          ;
          stdlib.protect(ctc1, await interact.informTimeout(), {
            at: './index.rsh:41:29:application',
            fs: ['at ./index.rsh:40:9:application call to [unknown function] (defined at: ./index.rsh:40:27:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./index.rsh:39:28:function exp)', 'at ./index.rsh:78:39:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
            msg: 'informTimeout',
            who: 'Bob'
            });
          
          return;
          }
        else {
          const [v93] = txn4.data;
          const v96 = txn4.time;
          const v97 = txn4.secs;
          const v92 = txn4.from;
          ;
          const v95 = stdlib.addressEq(v57, v92);
          stdlib.assert(v95, {
            at: './index.rsh:77:9:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Bob'
            });
          const v118 = stdlib.add(v96, v49);
          const txn5 = await (ctc.recv({
            evt_cnt: 2,
            funcNum: 10,
            out_tys: [ctc0, ctc0],
            timeoutAt: ['time', v118],
            waitIfNotPresent: false
            }));
          if (txn5.didTimeout) {
            const txn6 = await (ctc.sendrecv({
              args: [v47, v48, v49, v57, v83, v93, v118, v239],
              evt_cnt: 0,
              funcNum: 11,
              onlyIf: true,
              out_tys: [],
              pay: [stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), []],
              sim_p: (async (txn6) => {
                const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
                
                const [] = txn6.data;
                const v124 = txn6.time;
                const v125 = txn6.secs;
                const v121 = txn6.from;
                
                sim_r.txns.push({
                  amt: stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0),
                  kind: 'to',
                  tok: undefined
                  });
                const v123 = stdlib.addressEq(v57, v121);
                stdlib.assert(v123, {
                  at: 'reach standard library:209:7:dot',
                  fs: ['at ./index.rsh:86:39:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
                  msg: 'sender correct',
                  who: 'Bob'
                  });
                sim_r.txns.push({
                  amt: v239,
                  kind: 'from',
                  to: v57,
                  tok: undefined
                  });
                sim_r.txns.push({
                  kind: 'halt',
                  tok: undefined
                  })
                sim_r.isHalt = true;
                
                return sim_r;
                }),
              soloSend: true,
              timeoutAt: undefined,
              tys: [ctc4, ctc0, ctc0, ctc4, ctc2, ctc0, ctc0, ctc0],
              waitIfNotPresent: false
              }));
            const [] = txn6.data;
            const v124 = txn6.time;
            const v125 = txn6.secs;
            const v121 = txn6.from;
            ;
            const v123 = stdlib.addressEq(v57, v121);
            stdlib.assert(v123, {
              at: 'reach standard library:209:7:dot',
              fs: ['at ./index.rsh:86:39:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
              msg: 'sender correct',
              who: 'Bob'
              });
            ;
            stdlib.protect(ctc1, await interact.informTimeout(), {
              at: './index.rsh:41:29:application',
              fs: ['at ./index.rsh:40:9:application call to [unknown function] (defined at: ./index.rsh:40:27:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./index.rsh:39:28:function exp)', 'at ./index.rsh:86:39:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
              msg: 'informTimeout',
              who: 'Bob'
              });
            
            return;
            }
          else {
            const [v102, v103] = txn5.data;
            const v106 = txn5.time;
            const v107 = txn5.secs;
            const v101 = txn5.from;
            ;
            const v105 = stdlib.addressEq(v47, v101);
            stdlib.assert(v105, {
              at: './index.rsh:85:11:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Bob'
              });
            const v109 = stdlib.digest(ctc3, [v102, v103]);
            const v110 = stdlib.digestEq(v83, v109);
            stdlib.assert(v110, {
              at: 'reach standard library:65:17:application',
              fs: ['at ./index.rsh:87:20:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)'],
              msg: null,
              who: 'Bob'
              });
            const v112 = stdlib.sub(stdlib.checkedBigNumberify('./index.rsh:7:14:decimal', stdlib.UInt_max, 4), v93);
            const v113 = stdlib.add(v103, v112);
            const v114 = stdlib.mod(v113, stdlib.checkedBigNumberify('./index.rsh:7:28:decimal', stdlib.UInt_max, 3));
            const cv62 = v114;
            const cv233 = v106;
            const cv239 = v239;
            
            v62 = cv62;
            v233 = cv233;
            v239 = cv239;
            
            continue;}
          }
        }
      }
    const v190 = stdlib.eq(v62, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
    const v193 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:94:12:decimal', stdlib.UInt_max, 2), v48);
    const v195 = v190 ? v47 : v57;
    ;
    stdlib.protect(ctc1, await interact.seeOutcome(v62), {
      at: './index.rsh:98:24:application',
      fs: ['at ./index.rsh:97:7:application call to [unknown function] (defined at: ./index.rsh:97:25:function exp)'],
      msg: 'seeOutcome',
      who: 'Bob'
      });
    
    return;}
  
  
  };

const _ALGO = {
  appApproval: `#pragma version 4
txn RekeyTo
global ZeroAddress
==
assert
txn Lease
global ZeroAddress
==
assert
int 0
store 0
txn ApplicationID
bz alloc
byte base64()
app_global_get
dup
substring 0 32
store 1
substring 32 64
store 2
txn NumAppArgs
int 3
==
assert
txna ApplicationArgs 0
btoi
dup
bz ctor
// Handler 1
dup
int 1
==
bz l0
pop
txna ApplicationArgs 1
dup
len
int 0
==
assert
pop
txna ApplicationArgs 2
dup
len
int 16
==
assert
dup
substring 0 8
btoi
store 255
dup
substring 8 16
btoi
store 254
pop
// compute state in HM_Check 0
int 8
bzero
sha256
load 1
==
assert
// "CheckPay"
// "./index.rsh:49:9:dot"
// "[]"
load 255
dup
bz l1
load 0
dup
int 1
+
store 0
swap
dig 1
gtxns Amount
==
assert
int pay
dig 1
gtxns TypeEnum
==
assert
int 0
dig 1
gtxns Fee
==
assert
global ZeroAddress
dig 1
gtxns Lease
==
assert
global ZeroAddress
dig 1
gtxns RekeyTo
==
assert
load 2
dig 1
gtxns Receiver
==
assert
l1:
pop
global Round
load 254
+
store 253
// compute state in HM_Set 1
byte base64(AAAAAAAAAAE=)
txn Sender
concat
load 255
itob
concat
load 254
itob
concat
load 253
itob
concat
sha256
store 1
txn OnCompletion
int NoOp
==
assert
b updateState
l0:
// Handler 2
dup
int 2
==
bz l2
pop
txna ApplicationArgs 1
dup
len
int 56
==
assert
dup
substring 0 32
store 255
dup
substring 32 40
btoi
store 254
dup
substring 40 48
btoi
store 253
dup
substring 48 56
btoi
store 252
pop
txna ApplicationArgs 2
dup
len
int 0
==
assert
pop
// compute state in HM_Check 1
byte base64(AAAAAAAAAAE=)
load 255
concat
load 254
itob
concat
load 253
itob
concat
load 252
itob
concat
sha256
load 1
==
assert
global Round
load 252
<
assert
// "CheckPay"
// "./index.rsh:56:7:dot"
// "[]"
load 254
dup
bz l3
load 0
dup
int 1
+
store 0
swap
dig 1
gtxns Amount
==
assert
int pay
dig 1
gtxns TypeEnum
==
assert
int 0
dig 1
gtxns Fee
==
assert
global ZeroAddress
dig 1
gtxns Lease
==
assert
global ZeroAddress
dig 1
gtxns RekeyTo
==
assert
load 2
dig 1
gtxns Receiver
==
assert
l3:
pop
load 255
load 254
itob
concat
load 253
itob
concat
txn Sender
concat
byte base64(AAAAAAAAAAE=)
global Round
itob
concat
load 254
dup
+
itob
concat
b loop4
l2:
// Handler 3
dup
int 3
==
bz l4
pop
txna ApplicationArgs 1
dup
len
int 56
==
assert
dup
substring 0 32
store 255
dup
substring 32 40
btoi
store 254
dup
substring 40 48
btoi
store 253
dup
substring 48 56
btoi
store 252
pop
txna ApplicationArgs 2
dup
len
int 0
==
assert
pop
// compute state in HM_Check 1
byte base64(AAAAAAAAAAE=)
load 255
concat
load 254
itob
concat
load 253
itob
concat
load 252
itob
concat
sha256
load 1
==
assert
global Round
load 252
>=
assert
// "CheckPay"
// "reach standard library:209:7:dot"
// "[at ./index.rsh:57:37:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
// Just "sender correct"
// "reach standard library:209:7:dot"
// "[at ./index.rsh:57:37:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
load 255
txn Sender
==
assert
load 254
dup
bz l5
load 0
dup
int 1
+
store 0
swap
dig 1
gtxns Amount
==
assert
int pay
dig 1
gtxns TypeEnum
==
assert
int 0
dig 1
gtxns Fee
==
assert
global ZeroAddress
dig 1
gtxns Lease
==
assert
global ZeroAddress
dig 1
gtxns RekeyTo
==
assert
load 2
dig 1
gtxns Sender
==
assert
load 255
dig 1
gtxns Receiver
==
assert
l5:
pop
int 0
load 0
dup
int 1
+
store 0
swap
dig 1
gtxns Amount
==
assert
int pay
dig 1
gtxns TypeEnum
==
assert
int 0
dig 1
gtxns Fee
==
assert
global ZeroAddress
dig 1
gtxns Lease
==
assert
global ZeroAddress
dig 1
gtxns RekeyTo
==
assert
load 2
dig 1
gtxns Sender
==
assert
global CreatorAddress
dig 1
gtxns CloseRemainderTo
==
assert
l6:
pop
global ZeroAddress
store 1
txn OnCompletion
int DeleteApplication
==
assert
b updateState
l4:
l7:
l8:
// Handler 6
dup
int 6
==
bz l9
pop
txna ApplicationArgs 1
dup
len
int 96
==
assert
dup
substring 0 32
store 255
dup
substring 32 40
btoi
store 254
dup
substring 40 48
btoi
store 253
dup
substring 48 80
store 252
dup
substring 80 88
btoi
store 251
dup
substring 88 96
btoi
store 250
pop
txna ApplicationArgs 2
dup
len
int 32
==
assert
dup
store 249
pop
// compute state in HM_Check 6
byte base64(AAAAAAAAAAY=)
load 255
concat
load 254
itob
concat
load 253
itob
concat
load 252
concat
load 251
itob
concat
load 250
itob
concat
sha256
load 1
==
assert
global Round
load 251
<
assert
// "CheckPay"
// "./index.rsh:69:11:dot"
// "[]"
// Just "sender correct"
// "./index.rsh:69:11:dot"
// "[]"
load 255
txn Sender
==
assert
global Round
load 253
+
store 248
// compute state in HM_Set 8
byte base64(AAAAAAAAAAg=)
load 255
concat
load 254
itob
concat
load 253
itob
concat
load 252
concat
load 249
concat
load 248
itob
concat
load 250
itob
concat
sha256
store 1
txn OnCompletion
int NoOp
==
assert
b updateState
l9:
// Handler 7
dup
int 7
==
bz l10
pop
txna ApplicationArgs 1
dup
len
int 96
==
assert
dup
substring 0 32
store 255
dup
substring 32 40
btoi
store 254
dup
substring 40 48
btoi
store 253
dup
substring 48 80
store 252
dup
substring 80 88
btoi
store 251
dup
substring 88 96
btoi
store 250
pop
txna ApplicationArgs 2
dup
len
int 0
==
assert
pop
// compute state in HM_Check 6
byte base64(AAAAAAAAAAY=)
load 255
concat
load 254
itob
concat
load 253
itob
concat
load 252
concat
load 251
itob
concat
load 250
itob
concat
sha256
load 1
==
assert
global Round
load 251
>=
assert
// "CheckPay"
// "reach standard library:209:7:dot"
// "[at ./index.rsh:70:39:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
// Just "sender correct"
// "reach standard library:209:7:dot"
// "[at ./index.rsh:70:39:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
load 252
txn Sender
==
assert
load 250
dup
bz l11
load 0
dup
int 1
+
store 0
swap
dig 1
gtxns Amount
==
assert
int pay
dig 1
gtxns TypeEnum
==
assert
int 0
dig 1
gtxns Fee
==
assert
global ZeroAddress
dig 1
gtxns Lease
==
assert
global ZeroAddress
dig 1
gtxns RekeyTo
==
assert
load 2
dig 1
gtxns Sender
==
assert
load 252
dig 1
gtxns Receiver
==
assert
l11:
pop
int 0
load 0
dup
int 1
+
store 0
swap
dig 1
gtxns Amount
==
assert
int pay
dig 1
gtxns TypeEnum
==
assert
int 0
dig 1
gtxns Fee
==
assert
global ZeroAddress
dig 1
gtxns Lease
==
assert
global ZeroAddress
dig 1
gtxns RekeyTo
==
assert
load 2
dig 1
gtxns Sender
==
assert
global CreatorAddress
dig 1
gtxns CloseRemainderTo
==
assert
l12:
pop
global ZeroAddress
store 1
txn OnCompletion
int DeleteApplication
==
assert
b updateState
l10:
// Handler 8
dup
int 8
==
bz l13
pop
txna ApplicationArgs 1
dup
len
int 128
==
assert
dup
substring 0 32
store 255
dup
substring 32 40
btoi
store 254
dup
substring 40 48
btoi
store 253
dup
substring 48 80
store 252
dup
substring 80 112
store 251
dup
substring 112 120
btoi
store 250
dup
substring 120 128
btoi
store 249
pop
txna ApplicationArgs 2
dup
len
int 8
==
assert
dup
btoi
store 248
pop
// compute state in HM_Check 8
byte base64(AAAAAAAAAAg=)
load 255
concat
load 254
itob
concat
load 253
itob
concat
load 252
concat
load 251
concat
load 250
itob
concat
load 249
itob
concat
sha256
load 1
==
assert
global Round
load 250
<
assert
// "CheckPay"
// "./index.rsh:77:9:dot"
// "[]"
// Just "sender correct"
// "./index.rsh:77:9:dot"
// "[]"
load 252
txn Sender
==
assert
global Round
load 253
+
store 247
// compute state in HM_Set 10
byte base64(AAAAAAAAAAo=)
load 255
concat
load 254
itob
concat
load 253
itob
concat
load 252
concat
load 251
concat
load 248
itob
concat
load 247
itob
concat
load 249
itob
concat
sha256
store 1
txn OnCompletion
int NoOp
==
assert
b updateState
l13:
// Handler 9
dup
int 9
==
bz l14
pop
txna ApplicationArgs 1
dup
len
int 128
==
assert
dup
substring 0 32
store 255
dup
substring 32 40
btoi
store 254
dup
substring 40 48
btoi
store 253
dup
substring 48 80
store 252
dup
substring 80 112
store 251
dup
substring 112 120
btoi
store 250
dup
substring 120 128
btoi
store 249
pop
txna ApplicationArgs 2
dup
len
int 0
==
assert
pop
// compute state in HM_Check 8
byte base64(AAAAAAAAAAg=)
load 255
concat
load 254
itob
concat
load 253
itob
concat
load 252
concat
load 251
concat
load 250
itob
concat
load 249
itob
concat
sha256
load 1
==
assert
global Round
load 250
>=
assert
// "CheckPay"
// "reach standard library:209:7:dot"
// "[at ./index.rsh:78:39:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
// Just "sender correct"
// "reach standard library:209:7:dot"
// "[at ./index.rsh:78:39:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
load 255
txn Sender
==
assert
load 249
dup
bz l15
load 0
dup
int 1
+
store 0
swap
dig 1
gtxns Amount
==
assert
int pay
dig 1
gtxns TypeEnum
==
assert
int 0
dig 1
gtxns Fee
==
assert
global ZeroAddress
dig 1
gtxns Lease
==
assert
global ZeroAddress
dig 1
gtxns RekeyTo
==
assert
load 2
dig 1
gtxns Sender
==
assert
load 255
dig 1
gtxns Receiver
==
assert
l15:
pop
int 0
load 0
dup
int 1
+
store 0
swap
dig 1
gtxns Amount
==
assert
int pay
dig 1
gtxns TypeEnum
==
assert
int 0
dig 1
gtxns Fee
==
assert
global ZeroAddress
dig 1
gtxns Lease
==
assert
global ZeroAddress
dig 1
gtxns RekeyTo
==
assert
load 2
dig 1
gtxns Sender
==
assert
global CreatorAddress
dig 1
gtxns CloseRemainderTo
==
assert
l16:
pop
global ZeroAddress
store 1
txn OnCompletion
int DeleteApplication
==
assert
b updateState
l14:
// Handler 10
dup
int 10
==
bz l17
pop
txna ApplicationArgs 1
dup
len
int 136
==
assert
dup
substring 0 32
store 255
dup
substring 32 40
btoi
store 254
dup
substring 40 48
btoi
store 253
dup
substring 48 80
store 252
dup
substring 80 112
store 251
dup
substring 112 120
btoi
store 250
dup
substring 120 128
btoi
store 249
dup
substring 128 136
btoi
store 248
pop
txna ApplicationArgs 2
dup
len
int 16
==
assert
dup
substring 0 8
btoi
store 247
dup
substring 8 16
btoi
store 246
pop
// compute state in HM_Check 10
byte base64(AAAAAAAAAAo=)
load 255
concat
load 254
itob
concat
load 253
itob
concat
load 252
concat
load 251
concat
load 250
itob
concat
load 249
itob
concat
load 248
itob
concat
sha256
load 1
==
assert
global Round
load 249
<
assert
// "CheckPay"
// "./index.rsh:85:11:dot"
// "[]"
// Just "sender correct"
// "./index.rsh:85:11:dot"
// "[]"
load 255
txn Sender
==
assert
// Nothing
// "reach standard library:65:17:application"
// "[at ./index.rsh:87:20:application call to \"checkCommitment\" (defined at: reach standard library:64:8:function exp)]"
load 251
load 247
itob
load 246
itob
concat
sha256
==
assert
load 255
load 254
itob
concat
load 253
itob
concat
load 252
concat
load 246
int 4
load 250
-
+
int 3
%
itob
global Round
itob
concat
load 248
itob
concat
b loop4
l17:
// Handler 11
dup
int 11
==
bz l18
pop
txna ApplicationArgs 1
dup
len
int 136
==
assert
dup
substring 0 32
store 255
dup
substring 32 40
btoi
store 254
dup
substring 40 48
btoi
store 253
dup
substring 48 80
store 252
dup
substring 80 112
store 251
dup
substring 112 120
btoi
store 250
dup
substring 120 128
btoi
store 249
dup
substring 128 136
btoi
store 248
pop
txna ApplicationArgs 2
dup
len
int 0
==
assert
pop
// compute state in HM_Check 10
byte base64(AAAAAAAAAAo=)
load 255
concat
load 254
itob
concat
load 253
itob
concat
load 252
concat
load 251
concat
load 250
itob
concat
load 249
itob
concat
load 248
itob
concat
sha256
load 1
==
assert
global Round
load 249
>=
assert
// "CheckPay"
// "reach standard library:209:7:dot"
// "[at ./index.rsh:86:39:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
// Just "sender correct"
// "reach standard library:209:7:dot"
// "[at ./index.rsh:86:39:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
load 252
txn Sender
==
assert
load 248
dup
bz l19
load 0
dup
int 1
+
store 0
swap
dig 1
gtxns Amount
==
assert
int pay
dig 1
gtxns TypeEnum
==
assert
int 0
dig 1
gtxns Fee
==
assert
global ZeroAddress
dig 1
gtxns Lease
==
assert
global ZeroAddress
dig 1
gtxns RekeyTo
==
assert
load 2
dig 1
gtxns Sender
==
assert
load 252
dig 1
gtxns Receiver
==
assert
l19:
pop
int 0
load 0
dup
int 1
+
store 0
swap
dig 1
gtxns Amount
==
assert
int pay
dig 1
gtxns TypeEnum
==
assert
int 0
dig 1
gtxns Fee
==
assert
global ZeroAddress
dig 1
gtxns Lease
==
assert
global ZeroAddress
dig 1
gtxns RekeyTo
==
assert
load 2
dig 1
gtxns Sender
==
assert
global CreatorAddress
dig 1
gtxns CloseRemainderTo
==
assert
l20:
pop
global ZeroAddress
store 1
txn OnCompletion
int DeleteApplication
==
assert
b updateState
l18:
int 0
assert
loop4:
dup
substring 0 8
btoi
store 255
dup
substring 8 16
btoi
store 254
dup
substring 16 24
btoi
store 253
pop
dup
substring 0 32
store 252
dup
substring 32 40
btoi
store 251
dup
substring 40 48
btoi
store 250
dup
substring 48 80
store 249
pop
load 255
int 1
==
bz l21
load 254
load 250
+
store 248
// compute state in HM_Set 6
byte base64(AAAAAAAAAAY=)
load 252
concat
load 251
itob
concat
load 250
itob
concat
load 249
concat
load 248
itob
concat
load 253
itob
concat
sha256
store 1
txn OnCompletion
int NoOp
==
assert
b updateState
l21:
load 252
load 251
itob
concat
load 249
concat
load 255
itob
concat
byte base64()
loop5:
pop
dup
substring 0 32
store 255
dup
substring 32 40
btoi
store 254
dup
substring 40 72
store 253
dup
substring 72 80
btoi
store 252
pop
int 2
load 254
*
dup
bz l22
load 0
dup
int 1
+
store 0
swap
dig 1
gtxns Amount
==
assert
int pay
dig 1
gtxns TypeEnum
==
assert
int 0
dig 1
gtxns Fee
==
assert
global ZeroAddress
dig 1
gtxns Lease
==
assert
global ZeroAddress
dig 1
gtxns RekeyTo
==
assert
load 2
dig 1
gtxns Sender
==
assert
load 253
load 255
load 252
int 2
==
select
dig 1
gtxns Receiver
==
assert
l22:
pop
int 0
load 0
dup
int 1
+
store 0
swap
dig 1
gtxns Amount
==
assert
int pay
dig 1
gtxns TypeEnum
==
assert
int 0
dig 1
gtxns Fee
==
assert
global ZeroAddress
dig 1
gtxns Lease
==
assert
global ZeroAddress
dig 1
gtxns RekeyTo
==
assert
load 2
dig 1
gtxns Sender
==
assert
global CreatorAddress
dig 1
gtxns CloseRemainderTo
==
assert
l23:
pop
global ZeroAddress
store 1
txn OnCompletion
int DeleteApplication
==
assert
updateState:
byte base64()
load 1
load 2
concat
app_global_put
checkSize:
load 0
dup
dup
int 1
+
global GroupSize
==
assert
txn GroupIndex
==
assert
int 1000
*
txn Fee
<=
assert
done:
int 1
return
alloc:
txn OnCompletion
int NoOp
==
assert
byte base64()
int 64
bzero
app_global_put
b checkSize
ctor:
txn Sender
global CreatorAddress
==
assert
txna ApplicationArgs 1
store 2
// compute state in HM_Set 0
int 8
bzero
sha256
store 1
txn OnCompletion
int NoOp
==
assert
b updateState
`,
  appClear: `#pragma version 4
int 0
`,
  escrow: `#pragma version 4
global GroupSize
int 1
-
dup
gtxns TypeEnum
int appl
==
assert
gtxns ApplicationID
int {{ApplicationID}}
==
assert
done:
int 1
`,
  mapDataKeys: 0,
  mapDataSize: 0,
  unsupported: [],
  version: 2,
  viewKeys: 0,
  viewSize: 0
  };
const _ETH = {
  ABI: `[
  {
    "inputs": [],
    "stateMutability": "payable",
    "type": "constructor"
  },
  {
    "inputs": [
      {
        "internalType": "uint256",
        "name": "msg",
        "type": "uint256"
      }
    ],
    "name": "ReachError",
    "type": "error"
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
            "internalType": "bool",
            "name": "svs",
            "type": "bool"
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
                "name": "v57",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v83",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v93",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v118",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v239",
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
                "name": "v102",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v103",
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
                "name": "v57",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v83",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v93",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v118",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v239",
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
                "internalType": "uint256",
                "name": "v209",
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
                "name": "v209",
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
        "internalType": "struct T7",
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
                "internalType": "address payable",
                "name": "v57",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v168",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v239",
                "type": "uint256"
              }
            ],
            "internalType": "struct T8",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v83",
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
                "name": "v57",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v168",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v239",
                "type": "uint256"
              }
            ],
            "internalType": "struct T8",
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
                "name": "v57",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v83",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v143",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v239",
                "type": "uint256"
              }
            ],
            "internalType": "struct T11",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v93",
                "type": "uint256"
              }
            ],
            "internalType": "struct T16",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct T17",
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
                "name": "v57",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v83",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v143",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v239",
                "type": "uint256"
              }
            ],
            "internalType": "struct T11",
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
        "internalType": "struct T18",
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
            "internalType": "bool",
            "name": "svs",
            "type": "bool"
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
                "name": "v57",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v83",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v93",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v118",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v239",
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
                "name": "v102",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v103",
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
                "name": "v57",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v83",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v93",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v118",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v239",
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
                "internalType": "uint256",
                "name": "v209",
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
                "name": "v209",
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
        "internalType": "struct T7",
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
                "internalType": "address payable",
                "name": "v57",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v168",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v239",
                "type": "uint256"
              }
            ],
            "internalType": "struct T8",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v83",
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
                "name": "v57",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v168",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v239",
                "type": "uint256"
              }
            ],
            "internalType": "struct T8",
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
                "name": "v57",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v83",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v143",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v239",
                "type": "uint256"
              }
            ],
            "internalType": "struct T11",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v93",
                "type": "uint256"
              }
            ],
            "internalType": "struct T16",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "internalType": "struct T17",
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
                "name": "v57",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v83",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v143",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v239",
                "type": "uint256"
              }
            ],
            "internalType": "struct T11",
            "name": "svs",
            "type": "tuple"
          },
          {
            "internalType": "bool",
            "name": "msg",
            "type": "bool"
          }
        ],
        "internalType": "struct T18",
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
  Bytecode: `0x608060408190527f49ff028a829527a47ec6839c7147b484eccf5a2a94853eddac09cef44d9d4e9e90600090a160408051808201825243815242602091820152815160008183018190528184018190528351808303850181526060909201909352805191012090556113f9806100766000396000f3fe60806040526004361061008a5760003560e01c806360bafff71161005957806360bafff7146100e457806361ff86f7146100f7578063aa8594ca1461010a578063e4f83a8f1461011d578063ebc5780c1461013057600080fd5b8063108acff51461009657806338015b16146100ab5780633df7a789146100be578063497ccc6a146100d157600080fd5b3661009157005b600080fd5b6100a96100a4366004611053565b610143565b005b6100a96100b936600461102e565b61024a565b6100a96100cc366004610ffe565b61034f565b6100a96100df366004610ffe565b61055c565b6100a96100f236600461101b565b61065b565b6100a9610105366004610fe2565b61080a565b6100a9610118366004611041565b61090f565b6100a961012b366004611053565b610a39565b6100a961013e366004610fe2565b610b42565b60405161017f9061015b906001908490602001611315565b6040516020818303038152906040528051906020012060001c60005414600e610d12565b600080556101956060820135431015600f610d12565b7f613b30050768160fb8fb1fedba26a4639c7df8d370861f403061c2d46f9802e5816040516101c491906112c4565b60405180910390a16101d83415600c610d12565b6101fa336101e96020840184610fa5565b6001600160a01b031614600d610d12565b6102076020820182610fa5565b6040516001600160a01b039190911690602083013580156108fc02916000818181858888f19350505050158015610242573d6000803e3d6000fd5b506000805533ff5b6040516102869061026290600a908490602001611300565b6040516020818303038152906040528051906020012060001c600054146027610d12565b6000805561029c60c08201354310156028610d12565b7fb9d135d4afaa7938b4616c3637968c0e71f66413b54043da6188989930963f89816040516102cb9190611269565b60405180910390a16102df34156025610d12565b610304336102f36080840160608501610fa5565b6001600160a01b0316146026610d12565b6103146080820160608301610fa5565b6040516001600160a01b03919091169060e083013580156108fc02916000818181858888f19350505050158015610242573d6000803e3d6000fd5b60405161038b906103679060089084906020016112eb565b6040516020818303038152906040528051906020012060001c60005414601a610d12565b600080805560408051602081019091529081526103af60a08301354310601b610d12565b7f6739bb4acbb3812eea51c48895245b154776bf02dd61571666f6abba93266fec826040516103de91906111f8565b60405180910390a16103f234156018610d12565b610417336104066080850160608601610fa5565b6001600160a01b0316146019610d12565b61042560408301354361133d565b8152604080516101008101825260008082526020808301829052928201819052606082018190526080820181905260a0820181905260c0820181905260e08201529061047390840184610fa5565b6001600160a01b0316815260208084013590820152604080840135908201526104a26080840160608501610fa5565b6001600160a01b03908116606083810191825260808681013581860190815260e08089013560a0808901918252895160c0808b01918252808d0135858c0190815260408051600a6020808301919091528e518e16828401528e01519a81019a909a528c0151978901979097529751909816908601529151948401949094525192820192909252915161010083015251610120820152610140015b60408051601f198184030181529190528051602090910120600055505050565b604051610598906105749060089084906020016112eb565b6040516020818303038152906040528051906020012060001c60005414601e610d12565b600080556105ae60a0820135431015601f610d12565b7fcdae4cbd433f8c4039f23f2632824e3ab0089e9b8c2050e8e87e4d6e0a3df09b816040516105dd9190611215565b60405180910390a16105f13415601c610d12565b610613336106026020840184610fa5565b6001600160a01b031614601d610d12565b6106206020820182610fa5565b6040516001600160a01b03919091169060c083013580156108fc02916000818181858888f19350505050158015610242573d6000803e3d6000fd5b6040516106979061067390600a908490602001611300565b6040516020818303038152906040528051906020012060001c600054146023610d12565b600080556106ac60c082013543106024610d12565b7f58822179cfd9cab18ada288d4f94f3d5babb851e53b3ed667629d3cbbbb0cb72816040516106db919061123d565b60405180910390a16106ef34156020610d12565b610711336107006020840184610fa5565b6001600160a01b0316146021610d12565b604080516101008301356020820152610120830135918101919091526107599060600160408051601f1981840301815291905280516020909101206080830135146022610d12565b610761610ef5565b61076e6020830183610fa5565b81516001600160a01b039091169052805160208084013591015280516040808401359101526107a36080830160608401610fa5565b81516001600160a01b0390911660609091015260036107c760a08401356004611374565b6107d69061012085013561133d565b6107e0919061138b565b60208083018051929092528151439101525160e083013560409091015261080681610d37565b5050565b60405161084690610822906006908490602001611329565b6040516020818303038152906040528051906020012060001c600054146016610d12565b6000805561085c60808201354310156017610d12565b7fcc997a9af4abe95cf593cbeb34368171b4d5923d8562b1e54e51006451978b7c8160405161088b91906111d1565b60405180910390a161089f34156014610d12565b6108c4336108b36080840160608501610fa5565b6001600160a01b0316146015610d12565b6108d46080820160608301610fa5565b6040516001600160a01b03919091169060a083013580156108fc02916000818181858888f19350505050158015610242573d6000803e3d6000fd5b61096160006109216020840184610fc7565b60405160200161093d9291909182521515602082015260400190565b6040516020818303038152906040528051906020012060001c600054146008610d12565b600080805560408051602081018252918252517f3c669845d6bbc6fc367c9fa11ea5c8ec9bfd3d70eae7b34375b93f114f740365906109a1908490611293565b60405180910390a16109ba346020840135146007610d12565b6109c860408301354361133d565b81526040805160808082018352600060608084019182523384526020878101358186019081528887013586880190815288518552875160019381019390935286516001600160a01b03169783019790975251918101919091529351918401919091525160a08301529060c00161053c565b604051610a7590610a51906001908490602001611315565b6040516020818303038152906040528051906020012060001c60005414600a610d12565b60008055610a8a60608201354310600b610d12565b7fe2fcb5361608dd42d825c4e917fd4fca89057bb8eb0b7e34b8c2813a114cc15281604051610ab991906112c4565b60405180910390a1610ad2346020830135146009610d12565b610ada610ef5565b610ae76020830183610fa5565b81516001600160a01b039091169052805160208084013591810182905282516040808601359101528251336060909101528083018051600190525143910152610b30908061133d565b60208201516040015261080681610d37565b604051610b7e90610b5a906006908490602001611329565b6040516020818303038152906040528051906020012060001c600054146012610d12565b60008080556040805160208101909152908152610ba2608083013543106013610d12565b7fee6496cc7d28a7c9121e0eebf62951277792d31c40832817d1a4c372e06eb46182604051610bd191906111b5565b60405180910390a1610be534156010610d12565b610c0733610bf66020850185610fa5565b6001600160a01b0316146011610d12565b610c1560408301354361133d565b81526040805160e08101825260008082526020808301829052928201819052606082018190526080820181905260a0820181905260c082015290610c5b90840184610fa5565b6001600160a01b031681526020808401359082015260408084013590820152610c8a6080840160608501610fa5565b6001600160a01b03908116606083810191825260c0868101356080808701918252875160a0808901918252808b0135858a019081526040805160086020808301919091528c518c16828401528c0151988101989098528a0151938701939093529551909616948401949094525190820152915160e0830152516101008201526101200161053c565b816108065760405163100960cb60e01b81526004810182905260240160405180910390fd5b60408051602081019091526000815260208201515160011415610e22578151604001516020808401510151610d6c919061133d565b81526040805160c08082018352600080835260208084018281528486018381526060808701858152608080890187815260a0808b019889528d51516001600160a01b039081168c528e5189015188528e518d015187528e51860151811685528d5183528e8901518d01518a528c516006998101999099528b5181169c89019c909c529551938701939093529251928501929092529051909616908201529351918401919091525160e0830152906101000161053c565b6040805160c081018252600091810182815260608083018481526080840185815260a085018681528486526020808701979097528851516001600160a01b03908116909552885187015190925287519092015190921690529184015151909152610e8b81610e90565b505050565b805160600151600214610ea857805160400151610eac565b8051515b6001600160a01b03166108fc8260000151602001516002610ecd9190611355565b6040518115909202916000818181858888f19350505050158015610242573d6000803e3d6000fd5b6040805160c0810182526000918101828152606082018390526080820183905260a08201929092529081908152602001610f4960405180606001604052806000815260200160008152602001600081525090565b905290565b80356001600160a01b0381168114610f6557600080fd5b919050565b80358015158114610f6557600080fd5b600060e08284031215610f8c57600080fd5b50919050565b60006101008284031215610f8c57600080fd5b600060208284031215610fb757600080fd5b610fc082610f4e565b9392505050565b600060208284031215610fd957600080fd5b610fc082610f6a565b600060e08284031215610ff457600080fd5b610fc08383610f7a565b6000610100828403121561101157600080fd5b610fc08383610f92565b60006101408284031215610f8c57600080fd5b60006101208284031215610f8c57600080fd5b600060608284031215610f8c57600080fd5b600060a08284031215610f8c57600080fd5b6001600160a01b038061107783610f4e565b16835260208201356020840152604082013560408401528061109b60608401610f4e565b166060840152506080810135608083015260a081013560a083015260c081013560c08301525050565b6001600160a01b03806110d683610f4e565b1683526020820135602084015260408201356040840152806110fa60608401610f4e565b166060840152506080810135608083015260a081013560a083015260c081013560c083015260e081013560e08301525050565b6001600160a01b0361113e82610f4e565b1682526020810135602083015260408101356040830152606081013560608301525050565b6001600160a01b038061117583610f4e565b16835260208201356020840152604082013560408401528061119960608401610f4e565b166060840152506080818101359083015260a090810135910152565b60e081016111c38284611163565b60c092830135919092015290565b60e081016111df8284611163565b6111eb60c08401610f6a565b151560c083015292915050565b61010081016112078284611065565b60e092830135919092015290565b61010081016112248284611065565b61123060e08401610f6a565b151560e083015292915050565b610140810161124c82846110c4565b610100838101358382015261012080850135908401525092915050565b610120810161127882846110c4565b610100611286818501610f6a565b1515818401525092915050565b606081016112a083610f6a565b151582526112be602083016020850180358252602090810135910152565b92915050565b60a081016112d2828461112d565b6112de60808401610f6a565b1515608083015292915050565b8281526101008101610fc06020830184611065565b8281526101208101610fc060208301846110c4565b82815260a08101610fc0602083018461112d565b82815260e08101610fc06020830184611163565b60008219821115611350576113506113ad565b500190565b600081600019048311821515161561136f5761136f6113ad565b500290565b600082821015611386576113866113ad565b500390565b6000826113a857634e487b7160e01b600052601260045260246000fd5b500690565b634e487b7160e01b600052601160045260246000fdfea264697066735822122038e20ee81ab3b7c9d6b1aa1a54dd9b9255cc086570c8ed0c5c5156f7ebec0e4264736f6c63430008060033`,
  BytecodeLen: 5231,
  Which: `oD`,
  deployMode: `DM_constructor`,
  version: 1,
  views: {
    }
  };

export const _Connectors = {
  ALGO: _ALGO,
  ETH: _ETH
  };

