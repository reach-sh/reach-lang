// Automatically generated with Reach 0.1.3
/* eslint-disable */
export const _version = '0.1.3';


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
  const ctc4 = stdlib.T_Tuple([ctc0]);
  const ctc5 = stdlib.T_Address;
  const ctc6 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc5, ctc0, ctc0]);
  const ctc7 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc5, ctc0]);
  const ctc8 = stdlib.T_Tuple([]);
  const ctc9 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc5, ctc2, ctc0, ctc0, ctc0]);
  const ctc10 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc5, ctc2, ctc0, ctc0]);
  const ctc11 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc5, ctc2, ctc0]);
  const ctc12 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc0]);
  const ctc13 = stdlib.T_Tuple([ctc0, ctc5, ctc0]);
  
  
  const v53 = await ctc.creationTime();
  const v52 = stdlib.protect(ctc0, interact.wager, 'for Alice\'s interact field wager');
  
  const txn1 = await (ctc.sendrecv(1, 1, stdlib.checkedBigNumberify('./index.rsh:44:9:dot', stdlib.UInt_max, 0), [ctc0, ctc0], [v53, v52], [v52, []], [ctc0], true, true, false, (async (txn1) => {
    const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
    
    sim_r.prevSt = stdlib.digest(ctc1, [stdlib.checkedBigNumberify('./index.rsh:44:9:dot', stdlib.UInt_max, 0), v53]);
    sim_r.prevSt_noPrevTime = stdlib.digest(ctc4, [stdlib.checkedBigNumberify('./index.rsh:44:9:dot', stdlib.UInt_max, 0)]);
    const [v58] = txn1.data;
    const v61 = txn1.time;
    const v57 = txn1.from;
    
    sim_r.txns.push({
      amt: v58,
      kind: 'to',
      tok: undefined
      });
    sim_r.nextSt = stdlib.digest(ctc12, [stdlib.checkedBigNumberify('./index.rsh:46:15:after expr stmt semicolon', stdlib.UInt_max, 1), v57, v58, v61]);
    sim_r.nextSt_noTime = stdlib.digest(ctc13, [stdlib.checkedBigNumberify('./index.rsh:46:15:after expr stmt semicolon', stdlib.UInt_max, 1), v57, v58]);
    sim_r.view = [ctc4, [stdlib.checkedBigNumberify('./index.rsh:46:15:after expr stmt semicolon', stdlib.UInt_max, 0)]];
    sim_r.isHalt = false;
    
    return sim_r;
    })));
  const [v58] = txn1.data;
  const v61 = txn1.time;
  const v57 = txn1.from;
  ;
  const txn2 = await (ctc.recv(2, 0, [], false, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 100)));
  if (txn2.didTimeout) {
    const txn3 = await (ctc.sendrecv(3, 0, stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 2), [ctc5, ctc0, ctc0], [v57, v58, v61], [stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), []], [], true, true, false, (async (txn3) => {
      const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
      
      sim_r.prevSt = stdlib.digest(ctc12, [stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 1), v57, v58, v61]);
      sim_r.prevSt_noPrevTime = stdlib.digest(ctc13, [stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 1), v57, v58]);
      const [] = txn3.data;
      const v203 = txn3.time;
      const v200 = txn3.from;
      
      sim_r.txns.push({
        amt: stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0),
        kind: 'to',
        tok: undefined
        });
      const v202 = stdlib.addressEq(v57, v200);
      stdlib.assert(v202, {
        at: 'reach standard library:209:7:dot',
        fs: ['at ./index.rsh:51:41:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
        msg: 'sender correct',
        who: 'Alice'
        });
      sim_r.txns.push({
        amt: v58,
        kind: 'from',
        to: v57,
        tok: undefined
        });
      sim_r.txns.push({
        kind: 'halt',
        tok: undefined
        })
      sim_r.nextSt = stdlib.digest(ctc8, []);
      sim_r.nextSt_noTime = stdlib.digest(ctc8, []);
      sim_r.view = [ctc8, []];
      sim_r.isHalt = true;
      
      return sim_r;
      })));
    const [] = txn3.data;
    const v203 = txn3.time;
    const v200 = txn3.from;
    ;
    const v202 = stdlib.addressEq(v57, v200);
    stdlib.assert(v202, {
      at: 'reach standard library:209:7:dot',
      fs: ['at ./index.rsh:51:41:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
      msg: 'sender correct',
      who: 'Alice'
      });
    ;
    stdlib.protect(ctc3, await interact.informTimeout(), {
      at: './index.rsh:40:33:application',
      fs: ['at ./index.rsh:39:13:application call to [unknown function] (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:51:41:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
      msg: 'informTimeout',
      who: 'Alice'
      });
    
    return;
    }
  else {
    const [] = txn2.data;
    const v68 = txn2.time;
    const v65 = txn2.from;
    const v67 = stdlib.add(v58, v58);
    ;
    let v69 = stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1);
    let v218 = v68;
    let v220 = v67;
    
    while ((() => {
      const v82 = stdlib.eq(v69, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
      
      return v82;})()) {
      const v87 = stdlib.protect(ctc0, await interact.getHand(), {
        at: './index.rsh:59:42:application',
        fs: ['at ./index.rsh:58:15:application call to [unknown function] (defined at: ./index.rsh:58:19:function exp)'],
        msg: 'getHand',
        who: 'Alice'
        });
      const v89 = stdlib.protect(ctc0, await interact.random(), {
        at: 'reach standard library:60:31:application',
        fs: ['at ./index.rsh:60:52:application call to "makeCommitment" (defined at: reach standard library:59:8:function exp)', 'at ./index.rsh:58:15:application call to [unknown function] (defined at: ./index.rsh:58:19:function exp)'],
        msg: 'random',
        who: 'Alice'
        });
      const v90 = stdlib.digest(ctc1, [v89, v87]);
      
      const txn3 = await (ctc.sendrecv(6, 1, stdlib.checkedBigNumberify('./index.rsh:62:11:dot', stdlib.UInt_max, 3), [ctc5, ctc0, ctc5, ctc0, ctc0, ctc2], [v57, v58, v65, v218, v220, v90], [stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), []], [ctc2], true, true, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 100), (async (txn3) => {
        const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
        
        sim_r.prevSt = stdlib.digest(ctc6, [stdlib.checkedBigNumberify('./index.rsh:62:11:dot', stdlib.UInt_max, 6), v57, v58, v65, v218, v220]);
        sim_r.prevSt_noPrevTime = stdlib.digest(ctc7, [stdlib.checkedBigNumberify('./index.rsh:62:11:dot', stdlib.UInt_max, 6), v57, v58, v65, v220]);
        const [v93] = txn3.data;
        const v96 = txn3.time;
        const v92 = txn3.from;
        
        sim_r.txns.push({
          amt: stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0),
          kind: 'to',
          tok: undefined
          });
        const v95 = stdlib.addressEq(v57, v92);
        stdlib.assert(v95, {
          at: './index.rsh:62:11:dot',
          fs: [],
          msg: 'sender correct',
          who: 'Alice'
          });
        sim_r.nextSt = stdlib.digest(ctc10, [stdlib.checkedBigNumberify('./index.rsh:64:17:after expr stmt semicolon', stdlib.UInt_max, 8), v57, v58, v65, v93, v96, v220]);
        sim_r.nextSt_noTime = stdlib.digest(ctc11, [stdlib.checkedBigNumberify('./index.rsh:64:17:after expr stmt semicolon', stdlib.UInt_max, 8), v57, v58, v65, v93, v220]);
        sim_r.view = [ctc4, [stdlib.checkedBigNumberify('./index.rsh:64:17:after expr stmt semicolon', stdlib.UInt_max, 0)]];
        sim_r.isHalt = false;
        
        return sim_r;
        })));
      if (txn3.didTimeout) {
        const txn4 = await (ctc.recv(7, 0, [], false, false));
        const [] = txn4.data;
        const v166 = txn4.time;
        const v163 = txn4.from;
        ;
        const v165 = stdlib.addressEq(v65, v163);
        stdlib.assert(v165, {
          at: 'reach standard library:209:7:dot',
          fs: ['at ./index.rsh:63:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
          msg: 'sender correct',
          who: 'Alice'
          });
        ;
        stdlib.protect(ctc3, await interact.informTimeout(), {
          at: './index.rsh:40:33:application',
          fs: ['at ./index.rsh:39:13:application call to [unknown function] (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:63:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
          msg: 'informTimeout',
          who: 'Alice'
          });
        
        return;
        }
      else {
        const [v93] = txn3.data;
        const v96 = txn3.time;
        const v92 = txn3.from;
        ;
        const v95 = stdlib.addressEq(v57, v92);
        stdlib.assert(v95, {
          at: './index.rsh:62:11:dot',
          fs: [],
          msg: 'sender correct',
          who: 'Alice'
          });
        const txn4 = await (ctc.recv(8, 1, [ctc0], false, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 100)));
        if (txn4.didTimeout) {
          const txn5 = await (ctc.sendrecv(9, 0, stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 4), [ctc5, ctc0, ctc5, ctc2, ctc0, ctc0], [v57, v58, v65, v93, v96, v220], [stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), []], [], true, true, false, (async (txn5) => {
            const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
            
            sim_r.prevSt = stdlib.digest(ctc10, [stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 8), v57, v58, v65, v93, v96, v220]);
            sim_r.prevSt_noPrevTime = stdlib.digest(ctc11, [stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 8), v57, v58, v65, v93, v220]);
            const [] = txn5.data;
            const v147 = txn5.time;
            const v144 = txn5.from;
            
            sim_r.txns.push({
              amt: stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0),
              kind: 'to',
              tok: undefined
              });
            const v146 = stdlib.addressEq(v57, v144);
            stdlib.assert(v146, {
              at: 'reach standard library:209:7:dot',
              fs: ['at ./index.rsh:70:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
              msg: 'sender correct',
              who: 'Alice'
              });
            sim_r.txns.push({
              amt: v220,
              kind: 'from',
              to: v57,
              tok: undefined
              });
            sim_r.txns.push({
              kind: 'halt',
              tok: undefined
              })
            sim_r.nextSt = stdlib.digest(ctc8, []);
            sim_r.nextSt_noTime = stdlib.digest(ctc8, []);
            sim_r.view = [ctc8, []];
            sim_r.isHalt = true;
            
            return sim_r;
            })));
          const [] = txn5.data;
          const v147 = txn5.time;
          const v144 = txn5.from;
          ;
          const v146 = stdlib.addressEq(v57, v144);
          stdlib.assert(v146, {
            at: 'reach standard library:209:7:dot',
            fs: ['at ./index.rsh:70:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
            msg: 'sender correct',
            who: 'Alice'
            });
          ;
          stdlib.protect(ctc3, await interact.informTimeout(), {
            at: './index.rsh:40:33:application',
            fs: ['at ./index.rsh:39:13:application call to [unknown function] (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:70:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
            msg: 'informTimeout',
            who: 'Alice'
            });
          
          return;
          }
        else {
          const [v102] = txn4.data;
          const v105 = txn4.time;
          const v101 = txn4.from;
          ;
          const v104 = stdlib.addressEq(v65, v101);
          stdlib.assert(v104, {
            at: './index.rsh:69:11:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Alice'
            });
          const txn5 = await (ctc.sendrecv(10, 2, stdlib.checkedBigNumberify('./index.rsh:75:11:dot', stdlib.UInt_max, 5), [ctc5, ctc0, ctc5, ctc2, ctc0, ctc0, ctc0, ctc0, ctc0], [v57, v58, v65, v93, v102, v105, v220, v89, v87], [stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), []], [ctc0, ctc0], true, true, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 100), (async (txn5) => {
            const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
            
            sim_r.prevSt = stdlib.digest(ctc9, [stdlib.checkedBigNumberify('./index.rsh:75:11:dot', stdlib.UInt_max, 10), v57, v58, v65, v93, v102, v105, v220]);
            sim_r.prevSt_noPrevTime = stdlib.digest(ctc10, [stdlib.checkedBigNumberify('./index.rsh:75:11:dot', stdlib.UInt_max, 10), v57, v58, v65, v93, v102, v220]);
            const [v110, v111] = txn5.data;
            const v114 = txn5.time;
            const v109 = txn5.from;
            
            sim_r.txns.push({
              amt: stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0),
              kind: 'to',
              tok: undefined
              });
            const v113 = stdlib.addressEq(v57, v109);
            stdlib.assert(v113, {
              at: './index.rsh:75:11:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Alice'
              });
            const v116 = stdlib.digest(ctc1, [v110, v111]);
            const v117 = stdlib.digestEq(v93, v116);
            stdlib.assert(v117, {
              at: 'reach standard library:65:17:application',
              fs: ['at ./index.rsh:77:24:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)'],
              msg: null,
              who: 'Alice'
              });
            const v120 = stdlib.sub(stdlib.checkedBigNumberify('./index.rsh:7:18:decimal', stdlib.UInt_max, 4), v102);
            const v121 = stdlib.add(v111, v120);
            const v122 = stdlib.mod(v121, stdlib.checkedBigNumberify('./index.rsh:7:32:decimal', stdlib.UInt_max, 3));
            const cv69 = v122;
            const cv218 = v114;
            const cv220 = v220;
            
            (() => {
              const v69 = cv69;
              const v218 = cv218;
              const v220 = cv220;
              
              if ((() => {
                const v82 = stdlib.eq(v69, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
                
                return v82;})()) {
                sim_r.nextSt = stdlib.digest(ctc6, [stdlib.checkedBigNumberify('./index.rsh:56:17:after expr stmt semicolon', stdlib.UInt_max, 6), v57, v58, v65, v218, v220]);
                sim_r.nextSt_noTime = stdlib.digest(ctc7, [stdlib.checkedBigNumberify('./index.rsh:56:17:after expr stmt semicolon', stdlib.UInt_max, 6), v57, v58, v65, v220]);
                sim_r.view = [ctc4, [stdlib.checkedBigNumberify('./index.rsh:56:17:after expr stmt semicolon', stdlib.UInt_max, 0)]];
                sim_r.isHalt = false;
                }
              else {
                const v181 = stdlib.eq(v69, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
                const v184 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:83:16:decimal', stdlib.UInt_max, 2), v58);
                const v186 = v181 ? v57 : v65;
                sim_r.txns.push({
                  amt: v184,
                  kind: 'from',
                  to: v186,
                  tok: undefined
                  });
                sim_r.txns.push({
                  kind: 'halt',
                  tok: undefined
                  })
                sim_r.nextSt = stdlib.digest(ctc8, []);
                sim_r.nextSt_noTime = stdlib.digest(ctc8, []);
                sim_r.view = [ctc8, []];
                sim_r.isHalt = true;
                }})();
            return sim_r;
            })));
          if (txn5.didTimeout) {
            const txn6 = await (ctc.recv(11, 0, [], false, false));
            const [] = txn6.data;
            const v128 = txn6.time;
            const v125 = txn6.from;
            ;
            const v127 = stdlib.addressEq(v65, v125);
            stdlib.assert(v127, {
              at: 'reach standard library:209:7:dot',
              fs: ['at ./index.rsh:76:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
              msg: 'sender correct',
              who: 'Alice'
              });
            ;
            stdlib.protect(ctc3, await interact.informTimeout(), {
              at: './index.rsh:40:33:application',
              fs: ['at ./index.rsh:39:13:application call to [unknown function] (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:76:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
              msg: 'informTimeout',
              who: 'Alice'
              });
            
            return;
            }
          else {
            const [v110, v111] = txn5.data;
            const v114 = txn5.time;
            const v109 = txn5.from;
            ;
            const v113 = stdlib.addressEq(v57, v109);
            stdlib.assert(v113, {
              at: './index.rsh:75:11:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Alice'
              });
            const v116 = stdlib.digest(ctc1, [v110, v111]);
            const v117 = stdlib.digestEq(v93, v116);
            stdlib.assert(v117, {
              at: 'reach standard library:65:17:application',
              fs: ['at ./index.rsh:77:24:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)'],
              msg: null,
              who: 'Alice'
              });
            const v120 = stdlib.sub(stdlib.checkedBigNumberify('./index.rsh:7:18:decimal', stdlib.UInt_max, 4), v102);
            const v121 = stdlib.add(v111, v120);
            const v122 = stdlib.mod(v121, stdlib.checkedBigNumberify('./index.rsh:7:32:decimal', stdlib.UInt_max, 3));
            const cv69 = v122;
            const cv218 = v114;
            const cv220 = v220;
            
            v69 = cv69;
            v218 = cv218;
            v220 = cv220;
            
            continue;}
          }
        }
      }
    const v181 = stdlib.eq(v69, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
    const v184 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:83:16:decimal', stdlib.UInt_max, 2), v58);
    const v186 = v181 ? v57 : v65;
    ;
    stdlib.protect(ctc3, await interact.seeOutcome(v69), {
      at: './index.rsh:87:28:application',
      fs: ['at ./index.rsh:86:11:application call to [unknown function] (defined at: ./index.rsh:86:23:function exp)'],
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
  const ctc4 = stdlib.T_Tuple([]);
  const ctc5 = stdlib.T_Address;
  const ctc6 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc5, ctc2, ctc0, ctc0, ctc0]);
  const ctc7 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc5, ctc2, ctc0, ctc0]);
  const ctc8 = stdlib.T_Tuple([ctc0]);
  const ctc9 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc5, ctc2, ctc0]);
  const ctc10 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc5, ctc0, ctc0]);
  const ctc11 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc5, ctc0]);
  const ctc12 = stdlib.T_Tuple([ctc0, ctc5, ctc0, ctc0]);
  const ctc13 = stdlib.T_Tuple([ctc0, ctc5, ctc0]);
  
  
  const v53 = await ctc.creationTime();
  const txn1 = await (ctc.recv(1, 1, [ctc0], false, false));
  const [v58] = txn1.data;
  const v61 = txn1.time;
  const v57 = txn1.from;
  ;
  stdlib.protect(ctc1, await interact.acceptWager(v58), {
    at: './index.rsh:49:29:application',
    fs: ['at ./index.rsh:48:13:application call to [unknown function] (defined at: ./index.rsh:48:17:function exp)'],
    msg: 'acceptWager',
    who: 'Bob'
    });
  
  const txn2 = await (ctc.sendrecv(2, 0, stdlib.checkedBigNumberify('./index.rsh:50:9:dot', stdlib.UInt_max, 2), [ctc5, ctc0, ctc0], [v57, v58, v61], [v58, []], [], true, true, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 100), (async (txn2) => {
    const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
    
    sim_r.prevSt = stdlib.digest(ctc12, [stdlib.checkedBigNumberify('./index.rsh:50:9:dot', stdlib.UInt_max, 1), v57, v58, v61]);
    sim_r.prevSt_noPrevTime = stdlib.digest(ctc13, [stdlib.checkedBigNumberify('./index.rsh:50:9:dot', stdlib.UInt_max, 1), v57, v58]);
    const [] = txn2.data;
    const v68 = txn2.time;
    const v65 = txn2.from;
    
    const v67 = stdlib.add(v58, v58);
    sim_r.txns.push({
      amt: v58,
      kind: 'to',
      tok: undefined
      });
    const v69 = stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1);
    const v218 = v68;
    const v220 = v67;
    
    if ((() => {
      const v82 = stdlib.eq(v69, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
      
      return v82;})()) {
      sim_r.nextSt = stdlib.digest(ctc10, [stdlib.checkedBigNumberify('./index.rsh:56:17:after expr stmt semicolon', stdlib.UInt_max, 6), v57, v58, v65, v218, v220]);
      sim_r.nextSt_noTime = stdlib.digest(ctc11, [stdlib.checkedBigNumberify('./index.rsh:56:17:after expr stmt semicolon', stdlib.UInt_max, 6), v57, v58, v65, v220]);
      sim_r.view = [ctc8, [stdlib.checkedBigNumberify('./index.rsh:56:17:after expr stmt semicolon', stdlib.UInt_max, 0)]];
      sim_r.isHalt = false;
      }
    else {
      const v181 = stdlib.eq(v69, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
      const v184 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:83:16:decimal', stdlib.UInt_max, 2), v58);
      const v186 = v181 ? v57 : v65;
      sim_r.txns.push({
        amt: v184,
        kind: 'from',
        to: v186,
        tok: undefined
        });
      sim_r.txns.push({
        kind: 'halt',
        tok: undefined
        })
      sim_r.nextSt = stdlib.digest(ctc4, []);
      sim_r.nextSt_noTime = stdlib.digest(ctc4, []);
      sim_r.view = [ctc4, []];
      sim_r.isHalt = true;
      }
    return sim_r;
    })));
  if (txn2.didTimeout) {
    const txn3 = await (ctc.recv(3, 0, [], false, false));
    const [] = txn3.data;
    const v203 = txn3.time;
    const v200 = txn3.from;
    ;
    const v202 = stdlib.addressEq(v57, v200);
    stdlib.assert(v202, {
      at: 'reach standard library:209:7:dot',
      fs: ['at ./index.rsh:51:41:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
      msg: 'sender correct',
      who: 'Bob'
      });
    ;
    stdlib.protect(ctc1, await interact.informTimeout(), {
      at: './index.rsh:40:33:application',
      fs: ['at ./index.rsh:39:13:application call to [unknown function] (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:51:41:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
      msg: 'informTimeout',
      who: 'Bob'
      });
    
    return;
    }
  else {
    const [] = txn2.data;
    const v68 = txn2.time;
    const v65 = txn2.from;
    const v67 = stdlib.add(v58, v58);
    ;
    let v69 = stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1);
    let v218 = v68;
    let v220 = v67;
    
    while ((() => {
      const v82 = stdlib.eq(v69, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
      
      return v82;})()) {
      const txn3 = await (ctc.recv(6, 1, [ctc2], false, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 100)));
      if (txn3.didTimeout) {
        const txn4 = await (ctc.sendrecv(7, 0, stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 3), [ctc5, ctc0, ctc5, ctc0, ctc0], [v57, v58, v65, v218, v220], [stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), []], [], true, true, false, (async (txn4) => {
          const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
          
          sim_r.prevSt = stdlib.digest(ctc10, [stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 6), v57, v58, v65, v218, v220]);
          sim_r.prevSt_noPrevTime = stdlib.digest(ctc11, [stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 6), v57, v58, v65, v220]);
          const [] = txn4.data;
          const v166 = txn4.time;
          const v163 = txn4.from;
          
          sim_r.txns.push({
            amt: stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0),
            kind: 'to',
            tok: undefined
            });
          const v165 = stdlib.addressEq(v65, v163);
          stdlib.assert(v165, {
            at: 'reach standard library:209:7:dot',
            fs: ['at ./index.rsh:63:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
            msg: 'sender correct',
            who: 'Bob'
            });
          sim_r.txns.push({
            amt: v220,
            kind: 'from',
            to: v65,
            tok: undefined
            });
          sim_r.txns.push({
            kind: 'halt',
            tok: undefined
            })
          sim_r.nextSt = stdlib.digest(ctc4, []);
          sim_r.nextSt_noTime = stdlib.digest(ctc4, []);
          sim_r.view = [ctc4, []];
          sim_r.isHalt = true;
          
          return sim_r;
          })));
        const [] = txn4.data;
        const v166 = txn4.time;
        const v163 = txn4.from;
        ;
        const v165 = stdlib.addressEq(v65, v163);
        stdlib.assert(v165, {
          at: 'reach standard library:209:7:dot',
          fs: ['at ./index.rsh:63:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
          msg: 'sender correct',
          who: 'Bob'
          });
        ;
        stdlib.protect(ctc1, await interact.informTimeout(), {
          at: './index.rsh:40:33:application',
          fs: ['at ./index.rsh:39:13:application call to [unknown function] (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:63:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
          msg: 'informTimeout',
          who: 'Bob'
          });
        
        return;
        }
      else {
        const [v93] = txn3.data;
        const v96 = txn3.time;
        const v92 = txn3.from;
        ;
        const v95 = stdlib.addressEq(v57, v92);
        stdlib.assert(v95, {
          at: './index.rsh:62:11:dot',
          fs: [],
          msg: 'sender correct',
          who: 'Bob'
          });
        const v100 = stdlib.protect(ctc0, await interact.getHand(), {
          at: './index.rsh:68:52:application',
          fs: ['at ./index.rsh:67:15:application call to [unknown function] (defined at: ./index.rsh:67:19:function exp)'],
          msg: 'getHand',
          who: 'Bob'
          });
        
        const txn4 = await (ctc.sendrecv(8, 1, stdlib.checkedBigNumberify('./index.rsh:69:11:dot', stdlib.UInt_max, 4), [ctc5, ctc0, ctc5, ctc2, ctc0, ctc0, ctc0], [v57, v58, v65, v93, v96, v220, v100], [stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), []], [ctc0], true, true, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 100), (async (txn4) => {
          const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
          
          sim_r.prevSt = stdlib.digest(ctc7, [stdlib.checkedBigNumberify('./index.rsh:69:11:dot', stdlib.UInt_max, 8), v57, v58, v65, v93, v96, v220]);
          sim_r.prevSt_noPrevTime = stdlib.digest(ctc9, [stdlib.checkedBigNumberify('./index.rsh:69:11:dot', stdlib.UInt_max, 8), v57, v58, v65, v93, v220]);
          const [v102] = txn4.data;
          const v105 = txn4.time;
          const v101 = txn4.from;
          
          sim_r.txns.push({
            amt: stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0),
            kind: 'to',
            tok: undefined
            });
          const v104 = stdlib.addressEq(v65, v101);
          stdlib.assert(v104, {
            at: './index.rsh:69:11:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Bob'
            });
          sim_r.nextSt = stdlib.digest(ctc6, [stdlib.checkedBigNumberify('./index.rsh:71:17:after expr stmt semicolon', stdlib.UInt_max, 10), v57, v58, v65, v93, v102, v105, v220]);
          sim_r.nextSt_noTime = stdlib.digest(ctc7, [stdlib.checkedBigNumberify('./index.rsh:71:17:after expr stmt semicolon', stdlib.UInt_max, 10), v57, v58, v65, v93, v102, v220]);
          sim_r.view = [ctc8, [stdlib.checkedBigNumberify('./index.rsh:71:17:after expr stmt semicolon', stdlib.UInt_max, 0)]];
          sim_r.isHalt = false;
          
          return sim_r;
          })));
        if (txn4.didTimeout) {
          const txn5 = await (ctc.recv(9, 0, [], false, false));
          const [] = txn5.data;
          const v147 = txn5.time;
          const v144 = txn5.from;
          ;
          const v146 = stdlib.addressEq(v57, v144);
          stdlib.assert(v146, {
            at: 'reach standard library:209:7:dot',
            fs: ['at ./index.rsh:70:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
            msg: 'sender correct',
            who: 'Bob'
            });
          ;
          stdlib.protect(ctc1, await interact.informTimeout(), {
            at: './index.rsh:40:33:application',
            fs: ['at ./index.rsh:39:13:application call to [unknown function] (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:70:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
            msg: 'informTimeout',
            who: 'Bob'
            });
          
          return;
          }
        else {
          const [v102] = txn4.data;
          const v105 = txn4.time;
          const v101 = txn4.from;
          ;
          const v104 = stdlib.addressEq(v65, v101);
          stdlib.assert(v104, {
            at: './index.rsh:69:11:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Bob'
            });
          const txn5 = await (ctc.recv(10, 2, [ctc0, ctc0], false, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 100)));
          if (txn5.didTimeout) {
            const txn6 = await (ctc.sendrecv(11, 0, stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 5), [ctc5, ctc0, ctc5, ctc2, ctc0, ctc0, ctc0], [v57, v58, v65, v93, v102, v105, v220], [stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), []], [], true, true, false, (async (txn6) => {
              const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
              
              sim_r.prevSt = stdlib.digest(ctc6, [stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 10), v57, v58, v65, v93, v102, v105, v220]);
              sim_r.prevSt_noPrevTime = stdlib.digest(ctc7, [stdlib.checkedBigNumberify('reach standard library:209:7:dot', stdlib.UInt_max, 10), v57, v58, v65, v93, v102, v220]);
              const [] = txn6.data;
              const v128 = txn6.time;
              const v125 = txn6.from;
              
              sim_r.txns.push({
                amt: stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0),
                kind: 'to',
                tok: undefined
                });
              const v127 = stdlib.addressEq(v65, v125);
              stdlib.assert(v127, {
                at: 'reach standard library:209:7:dot',
                fs: ['at ./index.rsh:76:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
                msg: 'sender correct',
                who: 'Bob'
                });
              sim_r.txns.push({
                amt: v220,
                kind: 'from',
                to: v65,
                tok: undefined
                });
              sim_r.txns.push({
                kind: 'halt',
                tok: undefined
                })
              sim_r.nextSt = stdlib.digest(ctc4, []);
              sim_r.nextSt_noTime = stdlib.digest(ctc4, []);
              sim_r.view = [ctc4, []];
              sim_r.isHalt = true;
              
              return sim_r;
              })));
            const [] = txn6.data;
            const v128 = txn6.time;
            const v125 = txn6.from;
            ;
            const v127 = stdlib.addressEq(v65, v125);
            stdlib.assert(v127, {
              at: 'reach standard library:209:7:dot',
              fs: ['at ./index.rsh:76:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
              msg: 'sender correct',
              who: 'Bob'
              });
            ;
            stdlib.protect(ctc1, await interact.informTimeout(), {
              at: './index.rsh:40:33:application',
              fs: ['at ./index.rsh:39:13:application call to [unknown function] (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:76:43:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
              msg: 'informTimeout',
              who: 'Bob'
              });
            
            return;
            }
          else {
            const [v110, v111] = txn5.data;
            const v114 = txn5.time;
            const v109 = txn5.from;
            ;
            const v113 = stdlib.addressEq(v57, v109);
            stdlib.assert(v113, {
              at: './index.rsh:75:11:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Bob'
              });
            const v116 = stdlib.digest(ctc3, [v110, v111]);
            const v117 = stdlib.digestEq(v93, v116);
            stdlib.assert(v117, {
              at: 'reach standard library:65:17:application',
              fs: ['at ./index.rsh:77:24:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)'],
              msg: null,
              who: 'Bob'
              });
            const v120 = stdlib.sub(stdlib.checkedBigNumberify('./index.rsh:7:18:decimal', stdlib.UInt_max, 4), v102);
            const v121 = stdlib.add(v111, v120);
            const v122 = stdlib.mod(v121, stdlib.checkedBigNumberify('./index.rsh:7:32:decimal', stdlib.UInt_max, 3));
            const cv69 = v122;
            const cv218 = v114;
            const cv220 = v220;
            
            v69 = cv69;
            v218 = cv218;
            v220 = cv220;
            
            continue;}
          }
        }
      }
    const v181 = stdlib.eq(v69, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
    const v184 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:83:16:decimal', stdlib.UInt_max, 2), v58);
    const v186 = v181 ? v57 : v65;
    ;
    stdlib.protect(ctc1, await interact.seeOutcome(v69), {
      at: './index.rsh:87:28:application',
      fs: ['at ./index.rsh:86:11:application call to [unknown function] (defined at: ./index.rsh:86:23:function exp)'],
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
int 1
store 0
txn ApplicationID
bz alloc
int 0
bzero
app_global_get
dup
substring 0 32
store 2
substring 32 64
store 3
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
int 8
==
assert
dup
btoi
store 255
pop
txna ApplicationArgs 2
dup
len
int 8
==
assert
dup
btoi
store 254
pop
// compute state in HM_Check 0
int 0
bzero
int 0
itob
concat
load 255
itob
concat
sha256
load 2
==
assert
int 0
itob
int 0
itob
concat
store 1
int 0
load 1
substring 0 8
btoi
dup
bz l1
dig 1
gtxns FirstValid
<=
assert
b l2
l1:
pop
l2:
load 1
substring 8 16
btoi
dup
bz l3
dig 1
gtxns LastValid
>=
assert
b l4
l3:
pop
l4:
pop
// "CheckPay"
// "./index.rsh:44:9:dot"
// "[]"
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
dup
load 1
substring 0 8
btoi
dup
bz l6
dig 1
gtxns FirstValid
<=
assert
b l7
l6:
pop
l7:
load 1
substring 8 16
btoi
dup
bz l8
dig 1
gtxns LastValid
>=
assert
b l9
l8:
pop
l9:
pop
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
load 3
dig 1
gtxns Receiver
==
assert
l5:
pop
// compute state in HM_Set 1
int 0
bzero
int 1
itob
concat
txn Sender
concat
load 254
itob
concat
global Round
itob
concat
sha256
store 2
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
bz l10
pop
txna ApplicationArgs 1
dup
len
int 48
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
pop
txna ApplicationArgs 2
dup
len
int 0
==
assert
pop
// compute state in HM_Check 1
int 0
bzero
int 1
itob
concat
load 255
concat
load 254
itob
concat
load 253
itob
concat
sha256
load 2
==
assert
int 0
itob
load 253
int 100
+
itob
concat
store 1
int 0
load 1
substring 0 8
btoi
dup
bz l11
dig 1
gtxns FirstValid
<=
assert
b l12
l11:
pop
l12:
load 1
substring 8 16
btoi
dup
bz l13
dig 1
gtxns LastValid
>=
assert
b l14
l13:
pop
l14:
pop
// "CheckPay"
// "./index.rsh:50:9:dot"
// "[]"
load 254
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
dup
load 1
substring 0 8
btoi
dup
bz l16
dig 1
gtxns FirstValid
<=
assert
b l17
l16:
pop
l17:
load 1
substring 8 16
btoi
dup
bz l18
dig 1
gtxns LastValid
>=
assert
b l19
l18:
pop
l19:
pop
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
load 3
dig 1
gtxns Receiver
==
assert
l15:
pop
int 0
bzero
load 255
concat
load 254
itob
concat
txn Sender
concat
int 0
bzero
int 1
itob
concat
global Round
itob
concat
load 254
dup
+
itob
concat
b loop4
l10:
// Handler 3
dup
int 3
==
bz l20
pop
txna ApplicationArgs 1
dup
len
int 48
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
pop
txna ApplicationArgs 2
dup
len
int 0
==
assert
pop
// compute state in HM_Check 1
int 0
bzero
int 1
itob
concat
load 255
concat
load 254
itob
concat
load 253
itob
concat
sha256
load 2
==
assert
load 253
int 100
+
itob
int 0
itob
concat
store 1
int 0
load 1
substring 0 8
btoi
dup
bz l21
dig 1
gtxns FirstValid
<=
assert
b l22
l21:
pop
l22:
load 1
substring 8 16
btoi
dup
bz l23
dig 1
gtxns LastValid
>=
assert
b l24
l23:
pop
l24:
pop
// "CheckPay"
// "reach standard library:209:7:dot"
// "[at ./index.rsh:51:41:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
// Just "sender correct"
// "reach standard library:209:7:dot"
// "[at ./index.rsh:51:41:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
load 255
txn Sender
==
assert
load 254
dup
bz l25
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
dup
load 1
substring 0 8
btoi
dup
bz l26
dig 1
gtxns FirstValid
<=
assert
b l27
l26:
pop
l27:
load 1
substring 8 16
btoi
dup
bz l28
dig 1
gtxns LastValid
>=
assert
b l29
l28:
pop
l29:
pop
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
load 255
dig 1
gtxns Receiver
==
assert
load 3
dig 1
gtxns Sender
==
assert
l25:
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
dup
load 1
substring 0 8
btoi
dup
bz l31
dig 1
gtxns FirstValid
<=
assert
b l32
l31:
pop
l32:
load 1
substring 8 16
btoi
dup
bz l33
dig 1
gtxns LastValid
>=
assert
b l34
l33:
pop
l34:
pop
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
global CreatorAddress
dig 1
gtxns CloseRemainderTo
==
assert
load 3
dig 1
gtxns Sender
==
assert
l30:
pop
global ZeroAddress
store 2
txn OnCompletion
int DeleteApplication
==
assert
b updateState
l20:
l35:
l36:
// Handler 6
dup
int 6
==
bz l37
pop
txna ApplicationArgs 1
dup
len
int 88
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
substring 40 72
store 253
dup
substring 72 80
btoi
store 252
dup
substring 80 88
btoi
store 251
pop
txna ApplicationArgs 2
dup
len
int 32
==
assert
dup
store 250
pop
// compute state in HM_Check 6
int 0
bzero
int 6
itob
concat
load 255
concat
load 254
itob
concat
load 253
concat
load 252
itob
concat
load 251
itob
concat
sha256
load 2
==
assert
int 0
itob
load 252
int 100
+
itob
concat
store 1
int 0
load 1
substring 0 8
btoi
dup
bz l38
dig 1
gtxns FirstValid
<=
assert
b l39
l38:
pop
l39:
load 1
substring 8 16
btoi
dup
bz l40
dig 1
gtxns LastValid
>=
assert
b l41
l40:
pop
l41:
pop
// "CheckPay"
// "./index.rsh:62:11:dot"
// "[]"
// Just "sender correct"
// "./index.rsh:62:11:dot"
// "[]"
load 255
txn Sender
==
assert
// compute state in HM_Set 8
int 0
bzero
int 8
itob
concat
load 255
concat
load 254
itob
concat
load 253
concat
load 250
concat
global Round
itob
concat
load 251
itob
concat
sha256
store 2
txn OnCompletion
int NoOp
==
assert
b updateState
l37:
// Handler 7
dup
int 7
==
bz l42
pop
txna ApplicationArgs 1
dup
len
int 88
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
substring 40 72
store 253
dup
substring 72 80
btoi
store 252
dup
substring 80 88
btoi
store 251
pop
txna ApplicationArgs 2
dup
len
int 0
==
assert
pop
// compute state in HM_Check 6
int 0
bzero
int 6
itob
concat
load 255
concat
load 254
itob
concat
load 253
concat
load 252
itob
concat
load 251
itob
concat
sha256
load 2
==
assert
load 252
int 100
+
itob
int 0
itob
concat
store 1
int 0
load 1
substring 0 8
btoi
dup
bz l43
dig 1
gtxns FirstValid
<=
assert
b l44
l43:
pop
l44:
load 1
substring 8 16
btoi
dup
bz l45
dig 1
gtxns LastValid
>=
assert
b l46
l45:
pop
l46:
pop
// "CheckPay"
// "reach standard library:209:7:dot"
// "[at ./index.rsh:63:43:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
// Just "sender correct"
// "reach standard library:209:7:dot"
// "[at ./index.rsh:63:43:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
load 253
txn Sender
==
assert
load 251
dup
bz l47
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
dup
load 1
substring 0 8
btoi
dup
bz l48
dig 1
gtxns FirstValid
<=
assert
b l49
l48:
pop
l49:
load 1
substring 8 16
btoi
dup
bz l50
dig 1
gtxns LastValid
>=
assert
b l51
l50:
pop
l51:
pop
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
load 253
dig 1
gtxns Receiver
==
assert
load 3
dig 1
gtxns Sender
==
assert
l47:
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
dup
load 1
substring 0 8
btoi
dup
bz l53
dig 1
gtxns FirstValid
<=
assert
b l54
l53:
pop
l54:
load 1
substring 8 16
btoi
dup
bz l55
dig 1
gtxns LastValid
>=
assert
b l56
l55:
pop
l56:
pop
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
global CreatorAddress
dig 1
gtxns CloseRemainderTo
==
assert
load 3
dig 1
gtxns Sender
==
assert
l52:
pop
global ZeroAddress
store 2
txn OnCompletion
int DeleteApplication
==
assert
b updateState
l42:
// Handler 8
dup
int 8
==
bz l57
pop
txna ApplicationArgs 1
dup
len
int 120
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
substring 40 72
store 253
dup
substring 72 104
store 252
dup
substring 104 112
btoi
store 251
dup
substring 112 120
btoi
store 250
pop
txna ApplicationArgs 2
dup
len
int 8
==
assert
dup
btoi
store 249
pop
// compute state in HM_Check 8
int 0
bzero
int 8
itob
concat
load 255
concat
load 254
itob
concat
load 253
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
load 2
==
assert
int 0
itob
load 251
int 100
+
itob
concat
store 1
int 0
load 1
substring 0 8
btoi
dup
bz l58
dig 1
gtxns FirstValid
<=
assert
b l59
l58:
pop
l59:
load 1
substring 8 16
btoi
dup
bz l60
dig 1
gtxns LastValid
>=
assert
b l61
l60:
pop
l61:
pop
// "CheckPay"
// "./index.rsh:69:11:dot"
// "[]"
// Just "sender correct"
// "./index.rsh:69:11:dot"
// "[]"
load 253
txn Sender
==
assert
// compute state in HM_Set 10
int 0
bzero
int 10
itob
concat
load 255
concat
load 254
itob
concat
load 253
concat
load 252
concat
load 249
itob
concat
global Round
itob
concat
load 250
itob
concat
sha256
store 2
txn OnCompletion
int NoOp
==
assert
b updateState
l57:
// Handler 9
dup
int 9
==
bz l62
pop
txna ApplicationArgs 1
dup
len
int 120
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
substring 40 72
store 253
dup
substring 72 104
store 252
dup
substring 104 112
btoi
store 251
dup
substring 112 120
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
// compute state in HM_Check 8
int 0
bzero
int 8
itob
concat
load 255
concat
load 254
itob
concat
load 253
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
load 2
==
assert
load 251
int 100
+
itob
int 0
itob
concat
store 1
int 0
load 1
substring 0 8
btoi
dup
bz l63
dig 1
gtxns FirstValid
<=
assert
b l64
l63:
pop
l64:
load 1
substring 8 16
btoi
dup
bz l65
dig 1
gtxns LastValid
>=
assert
b l66
l65:
pop
l66:
pop
// "CheckPay"
// "reach standard library:209:7:dot"
// "[at ./index.rsh:70:43:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
// Just "sender correct"
// "reach standard library:209:7:dot"
// "[at ./index.rsh:70:43:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
load 255
txn Sender
==
assert
load 250
dup
bz l67
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
dup
load 1
substring 0 8
btoi
dup
bz l68
dig 1
gtxns FirstValid
<=
assert
b l69
l68:
pop
l69:
load 1
substring 8 16
btoi
dup
bz l70
dig 1
gtxns LastValid
>=
assert
b l71
l70:
pop
l71:
pop
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
load 255
dig 1
gtxns Receiver
==
assert
load 3
dig 1
gtxns Sender
==
assert
l67:
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
dup
load 1
substring 0 8
btoi
dup
bz l73
dig 1
gtxns FirstValid
<=
assert
b l74
l73:
pop
l74:
load 1
substring 8 16
btoi
dup
bz l75
dig 1
gtxns LastValid
>=
assert
b l76
l75:
pop
l76:
pop
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
global CreatorAddress
dig 1
gtxns CloseRemainderTo
==
assert
load 3
dig 1
gtxns Sender
==
assert
l72:
pop
global ZeroAddress
store 2
txn OnCompletion
int DeleteApplication
==
assert
b updateState
l62:
// Handler 10
dup
int 10
==
bz l77
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
substring 40 72
store 253
dup
substring 72 104
store 252
dup
substring 104 112
btoi
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
int 16
==
assert
dup
substring 0 8
btoi
store 248
dup
substring 8 16
btoi
store 247
pop
// compute state in HM_Check 10
int 0
bzero
int 10
itob
concat
load 255
concat
load 254
itob
concat
load 253
concat
load 252
concat
load 251
itob
concat
load 250
itob
concat
load 249
itob
concat
sha256
load 2
==
assert
int 0
itob
load 250
int 100
+
itob
concat
store 1
int 0
load 1
substring 0 8
btoi
dup
bz l78
dig 1
gtxns FirstValid
<=
assert
b l79
l78:
pop
l79:
load 1
substring 8 16
btoi
dup
bz l80
dig 1
gtxns LastValid
>=
assert
b l81
l80:
pop
l81:
pop
// "CheckPay"
// "./index.rsh:75:11:dot"
// "[]"
// Just "sender correct"
// "./index.rsh:75:11:dot"
// "[]"
load 255
txn Sender
==
assert
// Nothing
// "reach standard library:65:17:application"
// "[at ./index.rsh:77:24:application call to \"checkCommitment\" (defined at: reach standard library:64:8:function exp)]"
load 252
int 0
bzero
load 248
itob
concat
load 247
itob
concat
sha256
==
assert
int 0
bzero
load 255
concat
load 254
itob
concat
load 253
concat
int 0
bzero
load 247
int 4
load 251
-
+
int 3
%
itob
concat
global Round
itob
concat
load 249
itob
concat
b loop4
l77:
// Handler 11
dup
int 11
==
bz l82
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
substring 40 72
store 253
dup
substring 72 104
store 252
dup
substring 104 112
btoi
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
// compute state in HM_Check 10
int 0
bzero
int 10
itob
concat
load 255
concat
load 254
itob
concat
load 253
concat
load 252
concat
load 251
itob
concat
load 250
itob
concat
load 249
itob
concat
sha256
load 2
==
assert
load 250
int 100
+
itob
int 0
itob
concat
store 1
int 0
load 1
substring 0 8
btoi
dup
bz l83
dig 1
gtxns FirstValid
<=
assert
b l84
l83:
pop
l84:
load 1
substring 8 16
btoi
dup
bz l85
dig 1
gtxns LastValid
>=
assert
b l86
l85:
pop
l86:
pop
// "CheckPay"
// "reach standard library:209:7:dot"
// "[at ./index.rsh:76:43:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
// Just "sender correct"
// "reach standard library:209:7:dot"
// "[at ./index.rsh:76:43:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
load 253
txn Sender
==
assert
load 249
dup
bz l87
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
dup
load 1
substring 0 8
btoi
dup
bz l88
dig 1
gtxns FirstValid
<=
assert
b l89
l88:
pop
l89:
load 1
substring 8 16
btoi
dup
bz l90
dig 1
gtxns LastValid
>=
assert
b l91
l90:
pop
l91:
pop
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
load 253
dig 1
gtxns Receiver
==
assert
load 3
dig 1
gtxns Sender
==
assert
l87:
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
dup
load 1
substring 0 8
btoi
dup
bz l93
dig 1
gtxns FirstValid
<=
assert
b l94
l93:
pop
l94:
load 1
substring 8 16
btoi
dup
bz l95
dig 1
gtxns LastValid
>=
assert
b l96
l95:
pop
l96:
pop
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
global CreatorAddress
dig 1
gtxns CloseRemainderTo
==
assert
load 3
dig 1
gtxns Sender
==
assert
l92:
pop
global ZeroAddress
store 2
txn OnCompletion
int DeleteApplication
==
assert
b updateState
l82:
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
substring 40 72
store 250
pop
load 255
int 1
==
bz l97
// compute state in HM_Set 6
int 0
bzero
int 6
itob
concat
load 252
concat
load 251
itob
concat
load 250
concat
load 254
itob
concat
load 253
itob
concat
sha256
store 2
txn OnCompletion
int NoOp
==
assert
b updateState
l97:
int 0
bzero
load 252
concat
load 251
itob
concat
load 250
concat
load 255
itob
concat
int 0
bzero
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
bz l98
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
dup
load 1
substring 0 8
btoi
dup
bz l99
dig 1
gtxns FirstValid
<=
assert
b l100
l99:
pop
l100:
load 1
substring 8 16
btoi
dup
bz l101
dig 1
gtxns LastValid
>=
assert
b l102
l101:
pop
l102:
pop
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
load 3
dig 1
gtxns Sender
==
assert
l98:
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
dup
load 1
substring 0 8
btoi
dup
bz l104
dig 1
gtxns FirstValid
<=
assert
b l105
l104:
pop
l105:
load 1
substring 8 16
btoi
dup
bz l106
dig 1
gtxns LastValid
>=
assert
b l107
l106:
pop
l107:
pop
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
global CreatorAddress
dig 1
gtxns CloseRemainderTo
==
assert
load 3
dig 1
gtxns Sender
==
assert
l103:
pop
global ZeroAddress
store 2
txn OnCompletion
int DeleteApplication
==
assert
updateState:
int 0
bzero
load 2
load 3
concat
app_global_put
checkSize:
load 0
global GroupSize
==
return
done:
int 1
return
alloc:
txn OnCompletion
int NoOp
==
assert
int 0
bzero
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
store 3
// compute state in HM_Set 0
int 0
bzero
int 0
itob
concat
global Round
itob
concat
sha256
store 2
txn OnCompletion
int NoOp
==
assert
b updateState
`,
  appClear: `#pragma version 4
txn RekeyTo
global ZeroAddress
==
assert
txn Lease
global ZeroAddress
==
assert
global GroupSize
int 1
==
assert
int 0
bzero
app_global_get
substring 0 32
global ZeroAddress
==
return
done:
int 1
`,
  escrow: `#pragma version 4
gtxn 0 TypeEnum
int appl
==
assert
gtxn 0 ApplicationID
int {{ApplicationID}}
==
return
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
            "components": [
              {
                "internalType": "uint256",
                "name": "v53",
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
                "name": "v58",
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
                "name": "v57",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v58",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v65",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v93",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v102",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v105",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v220",
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
                "name": "v110",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v111",
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
                "name": "v57",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v58",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v65",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v93",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v102",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v105",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v220",
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
                "name": "v57",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v58",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v61",
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
                "name": "v57",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v58",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v61",
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
                "name": "v57",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v58",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v65",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v218",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v220",
                "type": "uint256"
              }
            ],
            "internalType": "struct T9",
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
                "name": "v57",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v58",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v65",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v218",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v220",
                "type": "uint256"
              }
            ],
            "internalType": "struct T9",
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
                "name": "v57",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v58",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v65",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v93",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v96",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v220",
                "type": "uint256"
              }
            ],
            "internalType": "struct T12",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v102",
                "type": "uint256"
              }
            ],
            "internalType": "struct T17",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct T18",
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
                "name": "v57",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v58",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v65",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v93",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v96",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v220",
                "type": "uint256"
              }
            ],
            "internalType": "struct T12",
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
        "internalType": "struct T19",
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
                "name": "v53",
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
                "name": "v58",
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
                "name": "v57",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v58",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v65",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v93",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v102",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v105",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v220",
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
                "name": "v110",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v111",
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
                "name": "v57",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v58",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v65",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v93",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v102",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v105",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v220",
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
                "name": "v57",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v58",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v61",
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
                "name": "v57",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v58",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v61",
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
                "name": "v57",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v58",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v65",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v218",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v220",
                "type": "uint256"
              }
            ],
            "internalType": "struct T9",
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
                "name": "v57",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v58",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v65",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v218",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v220",
                "type": "uint256"
              }
            ],
            "internalType": "struct T9",
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
                "name": "v57",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v58",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v65",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v93",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v96",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v220",
                "type": "uint256"
              }
            ],
            "internalType": "struct T12",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v102",
                "type": "uint256"
              }
            ],
            "internalType": "struct T17",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "internalType": "struct T18",
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
                "name": "v57",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v58",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v65",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v93",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v96",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v220",
                "type": "uint256"
              }
            ],
            "internalType": "struct T12",
            "name": "svs",
            "type": "tuple"
          },
          {
            "internalType": "bool",
            "name": "msg",
            "type": "bool"
          }
        ],
        "internalType": "struct T19",
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
  Bytecode: `0x608060408190527f49ff028a829527a47ec6839c7147b484eccf5a2a94853eddac09cef44d9d4e9e90600090a16040805160208082018352438252825180820184526000808252925181528351808301849052905181850152835180820385018152606090910190935282519201919091209055611331806100826000396000f3fe60806040526004361061008a5760003560e01c80639532ef01116100595780639532ef01146100e4578063b32a639f146100f7578063b861cb101461010a578063dc0f106b1461011d578063e163d7c41461013057600080fd5b80632438df70146100965780634188e022146100ab5780636cc4a844146100be5780636dacfd6f146100d157600080fd5b3661009157005b600080fd5b6100a96100a4366004610fc9565b610143565b005b6100a96100b9366004610f75565b61024f565b6100a96100cc366004610f59565b610360565b6100a96100df366004610f75565b610536565b6100a96100f2366004610fb7565b610711565b6100a9610105366004610fa4565b610816565b6100a9610118366004610f59565b61091e565b6100a961012b366004610f91565b610a26565b6100a961013e366004610fc9565b610bcf565b60405161017f9061015b90600190849060200161124d565b6040516020818303038152906040528051906020012060001c60005414600a610cd1565b6000805561019e61019560646040840135611275565b4310600b610cd1565b6101af346020830135146009610cd1565b7f1ca594b20641191c893d80895212a20239e99e17b7304a35c096140ec34f22dd816040516101de91906111f4565b60405180910390a16101ee610e9f565b6101fb6020830183610f37565b81516001600160a01b039091169052805160208084013591810182905282513360409091015280830180516001905251439101526102399080611275565b60208201516040015261024b81610cf6565b5050565b60405161028b90610267906008908490602001611224565b6040516020818303038152906040528051906020012060001c60005414601e610cd1565b600080556102ab6102a160646080840135611275565b431015601f610cd1565b6102b73415601c610cd1565b6102d9336102c86020840184610f37565b6001600160a01b031614601d610cd1565b6102e66020820182610f37565b6040516001600160a01b03919091169060a083013580156108fc02916000818181858888f19350505050158015610321573d6000803e3d6000fd5b507fe30737f1ebfc963c65c5913e78ab44df878e431e05d360e374cda18b462b262b816040516103519190611168565b60405180910390a16000805533ff5b60405161039c90610378906006908490602001611261565b6040516020818303038152906040528051906020012060001c600054146012610cd1565b600080556103bb6103b260646060840135611275565b43106013610cd1565b6103c734156010610cd1565b6103e9336103d86020840184610f37565b6001600160a01b0316146011610cd1565b7f16424d059cabc243859f670786693b7e657c3f04cbc39631fa14608999bfaef9816040516104189190611100565b60405180910390a161046b6040518060c0016040528060006001600160a01b031681526020016000815260200160006001600160a01b031681526020016000815260200160008152602001600081525090565b6104786020830183610f37565b6001600160a01b031681526020808301359082015261049d6060830160408401610f37565b6001600160a01b03908116604083810191825260a0858101356060808701918252436080808901918252808a0135858a01908152865160086020808301919091528b518b1698820198909852968a0151938701939093529551909616948401949094525190820152915160c08301525160e0820152610100015b60408051601f1981840301815291905280516020909101206000555050565b6040516105729061054e906008908490602001611224565b6040516020818303038152906040528051906020012060001c60005414601a610cd1565b6000805561059161058860646080840135611275565b4310601b610cd1565b61059d34156018610cd1565b6105c2336105b16060840160408501610f37565b6001600160a01b0316146019610cd1565b7fa03e2b199cbd4c163bca89aa8e3581bcf82ee511c6ed7b600ee5e8a3e78842b4816040516105f1919061114c565b60405180910390a161064b6040518060e0016040528060006001600160a01b031681526020016000815260200160006001600160a01b03168152602001600081526020016000815260200160008152602001600081525090565b6106586020830183610f37565b6001600160a01b031681526020808301359082015261067d6060830160408401610f37565b6001600160a01b03908116604083810191825260608581013581860190815260c08088013560808089019182524360a0808b01918252808c0135858c019081528851600a6020808301919091528d518d169a82019a909a52988c0151978901979097529751909816908601529151948401949094525192820192909252915160e08301525161010082015261012001610517565b6040805160006020820152823591810191909152610750906060016040516020818303038152906040528051906020012060001c600054146008610cd1565b60008055610765346020830135146007610cd1565b6040805182358152602080840135908201527ff2c62eba998811305a23599b2e6d212befbd7ded3a73f4c08bfb9aefe08dc166910160405180910390a16107cf604051806060016040528060006001600160a01b0316815260200160008152602001600081525090565b338152602082810135818301908152436040808501918252805160019481019490945284516001600160a01b0316908401529051606083015251608082015260a001610517565b6040516108529061082e90600a908490602001611238565b6040516020818303038152906040528051906020012060001c600054146027610cd1565b60008055610872610868606460a0840135611275565b4310156028610cd1565b61087e34156025610cd1565b6108a3336108926060840160408501610f37565b6001600160a01b0316146026610cd1565b6108b36060820160408301610f37565b6040516001600160a01b03919091169060c083013580156108fc02916000818181858888f193505050501580156108ee573d6000803e3d6000fd5b507fe7cd06eed96e73ebf1eef9fa7a8d285d298aa9f119f81a14f729b421ad209bfa8160405161035191906111c3565b60405161095a90610936906006908490602001611261565b6040516020818303038152906040528051906020012060001c600054146016610cd1565b6000805561097a61097060646060840135611275565b4310156017610cd1565b61098634156014610cd1565b6109ab3361099a6060840160408501610f37565b6001600160a01b0316146015610cd1565b6109bb6060820160408301610f37565b6040516001600160a01b039190911690608083013580156108fc02916000818181858888f193505050501580156109f6573d6000803e3d6000fd5b507f5005c4e6004c19e98ada43b5f5d05731c1a82aa5d9215871f88ceb584e0f83e081604051610351919061111c565b604051610a6290610a3e90600a908490602001611238565b6040516020818303038152906040528051906020012060001c600054146023610cd1565b60008055610a81610a78606460a0840135611275565b43106024610cd1565b610a8d34156020610cd1565b610aaf33610a9e6020840184610f37565b6001600160a01b0316146021610cd1565b6040805160e0830135602082015261010083013591810191909152610af69060600160408051601f1981840301815291905280516020909101206060830135146022610cd1565b7f352ea7fc48371f0bd43d7d1ad042d3e6a673947e2deccddfd368812813abc47281604051610b259190611198565b60405180910390a1610b35610e9f565b610b426020830183610f37565b81516001600160a01b0390911690528051602080840135910152610b6c6060830160408401610f37565b81516001600160a01b039091166040909101526003610b90608084013560046112ac565b610b9f90610100850135611275565b610ba991906112c3565b60208083018051929092528151439101525160c083013560409091015261024b81610cf6565b604051610c0b90610be790600190849060200161124d565b6040516020818303038152906040528051906020012060001c60005414600e610cd1565b60008055610c2b610c2160646040840135611275565b431015600f610cd1565b610c373415600c610cd1565b610c5933610c486020840184610f37565b6001600160a01b031614600d610cd1565b610c666020820182610f37565b6040516001600160a01b039190911690602083013580156108fc02916000818181858888f19350505050158015610ca1573d6000803e3d6000fd5b507fc3d6ba703f6ce931b1dd0e05e983d8be7c8ccc7f15219d844425151d856230138160405161035191906111f4565b8161024b5760405163100960cb60e01b81526004810182905260240160405180910390fd5b60208101515160011415610dd157610d486040518060a0016040528060006001600160a01b031681526020016000815260200160006001600160a01b0316815260200160008152602001600081525090565b8151516001600160a01b0390811682528251602090810151818401908152845160409081015184168186019081528387018051850151606080890191825291518401516080808a0191825285516006988101989098528951891695880195909552945191860191909152905190941690830152915160a0820152905160c082015260e001610517565b6040805160c0810182526000818301818152606083018281526080840183815260a085018481528386526020808701959095528751516001600160a01b03908116909452875185015190925286519095015190911690935283015151918290529061024b908290600214610e4a57805160400151610e4e565b8051515b6001600160a01b03166108fc8260000151602001516002610e6f919061128d565b6040518115909202916000818181858888f19350505050158015610e97573d6000803e3d6000fd5b506000805533ff5b6040805160a08101825260009181018281526060820183905260808201929092529081908152602001610eec60405180606001604052806000815260200160008152602001600081525090565b905290565b80356001600160a01b0381168114610f0857600080fd5b919050565b600060c08284031215610f1f57600080fd5b50919050565b600060e08284031215610f1f57600080fd5b600060208284031215610f4957600080fd5b610f5282610ef1565b9392505050565b600060c08284031215610f6b57600080fd5b610f528383610f0d565b600060e08284031215610f8757600080fd5b610f528383610f25565b60006101208284031215610f1f57600080fd5b60006101008284031215610f1f57600080fd5b600060408284031215610f1f57600080fd5b600060808284031215610f1f57600080fd5b6001600160a01b0380610fed83610ef1565b168352602082013560208401528061100760408401610ef1565b16604084015250606081013560608301526080810135608083015260a081013560a08301525050565b6001600160a01b038061104283610ef1565b168352602082013560208401528061105c60408401610ef1565b16604084015250606081013560608301526080810135608083015260a081013560a083015260c081013560c08301525050565b6001600160a01b036110a082610ef1565b16825260208181013590830152604090810135910152565b6001600160a01b03806110ca83610ef1565b16835260208201356020840152806110e460408401610ef1565b1660408401525060608181013590830152608090810135910152565b60c0810161110e82846110b8565b60a092830135919092015290565b60c0810161112a82846110b8565b60a083013580151580821461113e57600080fd5b8060a0850152505092915050565b60e0810161115a8284610fdb565b60c092830135919092015290565b60e081016111768284610fdb565b60c083013580151580821461118a57600080fd5b8060c0850152505092915050565b61012081016111a78284611030565b60e083013560e083015261010080840135818401525092915050565b61010081016111d28284611030565b60e08301358015158082146111e657600080fd5b8060e0850152505092915050565b60808101611202828461108f565b606083013580151580821461121657600080fd5b806060850152505092915050565b82815260e08101610f526020830184610fdb565b8281526101008101610f526020830184611030565b82815260808101610f52602083018461108f565b82815260c08101610f5260208301846110b8565b60008219821115611288576112886112e5565b500190565b60008160001904831182151516156112a7576112a76112e5565b500290565b6000828210156112be576112be6112e5565b500390565b6000826112e057634e487b7160e01b600052601260045260246000fd5b500690565b634e487b7160e01b600052601160045260246000fdfea264697066735822122098afce79c5cd0df723358692728db37a513baf366b9847aa31d2e9ba30addbc364736f6c63430008060033`,
  BytecodeLen: 5043,
  Which: `oD`,
  deployMode: `DM_constructor`,
  views: {
    }
  };

export const _Connectors = {
  ALGO: _ALGO,
  ETH: _ETH
  };

