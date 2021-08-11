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
  if (typeof(ctc) !== 'object' || ctc.sendrecv === undefined) {
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
  
  const v255 = stdlib.protect(ctc0, interact.deadline, 'for Alice\'s interact field deadline');
  const v256 = stdlib.protect(ctc0, interact.wager, 'for Alice\'s interact field wager');
  
  const txn1 = await (ctc.sendrecv({
    args: [v256, v255],
    evt_cnt: 2,
    funcNum: 1,
    onlyIf: true,
    out_tys: [ctc0, ctc0],
    pay: [v256, []],
    sim_p: (async (txn1) => {
      const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
      
      const [v260, v261] = txn1.data;
      const v262 = txn1.time;
      const v263 = txn1.secs;
      const v259 = txn1.from;
      
      sim_r.txns.push({
        amt: v260,
        kind: 'to',
        tok: undefined
        });
      const v267 = stdlib.add(v262, v261);
      sim_r.isHalt = false;
      
      return sim_r;
      }),
    soloSend: true,
    timeoutAt: undefined,
    tys: [ctc0, ctc0],
    waitIfNotPresent: false
    }));
  const [v260, v261] = txn1.data;
  const v262 = txn1.time;
  const v263 = txn1.secs;
  const v259 = txn1.from;
  ;
  const v267 = stdlib.add(v262, v261);
  const txn2 = await (ctc.recv({
    evt_cnt: 0,
    funcNum: 2,
    out_tys: [],
    timeoutAt: ['time', v267],
    waitIfNotPresent: false
    }));
  if (txn2.didTimeout) {
    const txn3 = await (ctc.sendrecv({
      args: [v259, v260, v261, v267],
      evt_cnt: 0,
      funcNum: 3,
      onlyIf: true,
      out_tys: [],
      pay: [stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), []],
      sim_p: (async (txn3) => {
        const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
        
        const [] = txn3.data;
        const v404 = txn3.time;
        const v405 = txn3.secs;
        const v403 = txn3.from;
        
        sim_r.txns.push({
          amt: stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0),
          kind: 'to',
          tok: undefined
          });
        const v407 = stdlib.addressEq(v259, v403);
        stdlib.assert(v407, {
          at: 'reach standard library:209:7:dot',
          fs: ['at ./index.rsh:57:37:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
          msg: 'sender correct',
          who: 'Alice'
          });
        sim_r.txns.push({
          amt: v260,
          kind: 'from',
          to: v259,
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
    const v404 = txn3.time;
    const v405 = txn3.secs;
    const v403 = txn3.from;
    ;
    const v407 = stdlib.addressEq(v259, v403);
    stdlib.assert(v407, {
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
    const v272 = txn2.time;
    const v273 = txn2.secs;
    const v271 = txn2.from;
    const v275 = stdlib.add(v260, v260);
    ;
    let v276 = stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1);
    let v277 = v272;
    let v283 = v275;
    
    while ((() => {
      const v291 = stdlib.eq(v276, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
      
      return v291;})()) {
      const v293 = stdlib.add(v277, v261);
      const v297 = stdlib.protect(ctc0, await interact.getHand(), {
        at: './index.rsh:65:42:application',
        fs: ['at ./index.rsh:64:15:application call to [unknown function] (defined at: ./index.rsh:64:19:function exp)'],
        msg: 'getHand',
        who: 'Alice'
        });
      const v298 = stdlib.protect(ctc0, await interact.random(), {
        at: 'reach standard library:60:31:application',
        fs: ['at ./index.rsh:66:56:application call to "makeCommitment" (defined at: reach standard library:59:8:function exp)', 'at ./index.rsh:64:15:application call to [unknown function] (defined at: ./index.rsh:64:19:function exp)'],
        msg: 'random',
        who: 'Alice'
        });
      const v299 = stdlib.digest(ctc1, [v298, v297]);
      
      const txn3 = await (ctc.sendrecv({
        args: [v259, v260, v261, v271, v283, v293, v299],
        evt_cnt: 1,
        funcNum: 6,
        onlyIf: true,
        out_tys: [ctc2],
        pay: [stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), []],
        sim_p: (async (txn3) => {
          const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
          
          const [v302] = txn3.data;
          const v303 = txn3.time;
          const v304 = txn3.secs;
          const v301 = txn3.from;
          
          sim_r.txns.push({
            amt: stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0),
            kind: 'to',
            tok: undefined
            });
          const v306 = stdlib.addressEq(v259, v301);
          stdlib.assert(v306, {
            at: './index.rsh:69:11:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Alice'
            });
          const v308 = stdlib.add(v303, v261);
          sim_r.isHalt = false;
          
          return sim_r;
          }),
        soloSend: true,
        timeoutAt: ['time', v293],
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
        const v371 = txn4.time;
        const v372 = txn4.secs;
        const v370 = txn4.from;
        ;
        const v374 = stdlib.addressEq(v271, v370);
        stdlib.assert(v374, {
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
        const [v302] = txn3.data;
        const v303 = txn3.time;
        const v304 = txn3.secs;
        const v301 = txn3.from;
        ;
        const v306 = stdlib.addressEq(v259, v301);
        stdlib.assert(v306, {
          at: './index.rsh:69:11:dot',
          fs: [],
          msg: 'sender correct',
          who: 'Alice'
          });
        const v308 = stdlib.add(v303, v261);
        const txn4 = await (ctc.recv({
          evt_cnt: 1,
          funcNum: 8,
          out_tys: [ctc0],
          timeoutAt: ['time', v308],
          waitIfNotPresent: false
          }));
        if (txn4.didTimeout) {
          const txn5 = await (ctc.sendrecv({
            args: [v259, v260, v261, v271, v283, v302, v308],
            evt_cnt: 0,
            funcNum: 9,
            onlyIf: true,
            out_tys: [],
            pay: [stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), []],
            sim_p: (async (txn5) => {
              const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
              
              const [] = txn5.data;
              const v354 = txn5.time;
              const v355 = txn5.secs;
              const v353 = txn5.from;
              
              sim_r.txns.push({
                amt: stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0),
                kind: 'to',
                tok: undefined
                });
              const v357 = stdlib.addressEq(v259, v353);
              stdlib.assert(v357, {
                at: 'reach standard library:209:7:dot',
                fs: ['at ./index.rsh:78:39:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
                msg: 'sender correct',
                who: 'Alice'
                });
              sim_r.txns.push({
                amt: v283,
                kind: 'from',
                to: v259,
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
            tys: [ctc4, ctc0, ctc0, ctc4, ctc0, ctc2, ctc0],
            waitIfNotPresent: false
            }));
          const [] = txn5.data;
          const v354 = txn5.time;
          const v355 = txn5.secs;
          const v353 = txn5.from;
          ;
          const v357 = stdlib.addressEq(v259, v353);
          stdlib.assert(v357, {
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
          const [v314] = txn4.data;
          const v315 = txn4.time;
          const v316 = txn4.secs;
          const v313 = txn4.from;
          ;
          const v318 = stdlib.addressEq(v271, v313);
          stdlib.assert(v318, {
            at: './index.rsh:77:9:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Alice'
            });
          const v320 = stdlib.add(v315, v261);
          const txn5 = await (ctc.sendrecv({
            args: [v259, v260, v261, v271, v283, v302, v314, v320, v298, v297],
            evt_cnt: 2,
            funcNum: 10,
            onlyIf: true,
            out_tys: [ctc0, ctc0],
            pay: [stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), []],
            sim_p: (async (txn5) => {
              const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
              
              const [v325, v326] = txn5.data;
              const v327 = txn5.time;
              const v328 = txn5.secs;
              const v324 = txn5.from;
              
              sim_r.txns.push({
                amt: stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0),
                kind: 'to',
                tok: undefined
                });
              const v330 = stdlib.addressEq(v259, v324);
              stdlib.assert(v330, {
                at: './index.rsh:85:11:dot',
                fs: [],
                msg: 'sender correct',
                who: 'Alice'
                });
              const v331 = stdlib.digest(ctc1, [v325, v326]);
              const v332 = stdlib.digestEq(v302, v331);
              stdlib.assert(v332, {
                at: 'reach standard library:65:17:application',
                fs: ['at ./index.rsh:87:20:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)'],
                msg: null,
                who: 'Alice'
                });
              const v333 = stdlib.sub(stdlib.checkedBigNumberify('./index.rsh:7:14:decimal', stdlib.UInt_max, 4), v314);
              const v334 = stdlib.add(v326, v333);
              const v335 = stdlib.mod(v334, stdlib.checkedBigNumberify('./index.rsh:7:28:decimal', stdlib.UInt_max, 3));
              const cv276 = v335;
              const cv277 = v327;
              const cv283 = v283;
              
              (() => {
                const v276 = cv276;
                const v277 = cv277;
                const v283 = cv283;
                
                if ((() => {
                  const v291 = stdlib.eq(v276, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
                  
                  return v291;})()) {
                  const v293 = stdlib.add(v277, v261);
                  sim_r.isHalt = false;
                  }
                else {
                  const v387 = stdlib.eq(v276, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
                  const v390 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:94:12:decimal', stdlib.UInt_max, 2), v260);
                  const v392 = v387 ? v259 : v271;
                  sim_r.txns.push({
                    amt: v390,
                    kind: 'from',
                    to: v392,
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
            timeoutAt: ['time', v320],
            tys: [ctc4, ctc0, ctc0, ctc4, ctc0, ctc2, ctc0, ctc0, ctc0, ctc0],
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
            const v337 = txn6.time;
            const v338 = txn6.secs;
            const v336 = txn6.from;
            ;
            const v340 = stdlib.addressEq(v271, v336);
            stdlib.assert(v340, {
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
            const [v325, v326] = txn5.data;
            const v327 = txn5.time;
            const v328 = txn5.secs;
            const v324 = txn5.from;
            ;
            const v330 = stdlib.addressEq(v259, v324);
            stdlib.assert(v330, {
              at: './index.rsh:85:11:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Alice'
              });
            const v331 = stdlib.digest(ctc1, [v325, v326]);
            const v332 = stdlib.digestEq(v302, v331);
            stdlib.assert(v332, {
              at: 'reach standard library:65:17:application',
              fs: ['at ./index.rsh:87:20:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)'],
              msg: null,
              who: 'Alice'
              });
            const v333 = stdlib.sub(stdlib.checkedBigNumberify('./index.rsh:7:14:decimal', stdlib.UInt_max, 4), v314);
            const v334 = stdlib.add(v326, v333);
            const v335 = stdlib.mod(v334, stdlib.checkedBigNumberify('./index.rsh:7:28:decimal', stdlib.UInt_max, 3));
            const cv276 = v335;
            const cv277 = v327;
            const cv283 = v283;
            
            v276 = cv276;
            v277 = cv277;
            v283 = cv283;
            
            continue;}
          }
        }
      }
    const v387 = stdlib.eq(v276, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
    const v390 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:94:12:decimal', stdlib.UInt_max, 2), v260);
    const v392 = v387 ? v259 : v271;
    ;
    stdlib.protect(ctc3, await interact.seeOutcome(v276), {
      at: './index.rsh:98:24:application',
      fs: ['at ./index.rsh:97:7:application call to [unknown function] (defined at: ./index.rsh:97:25:function exp)'],
      msg: 'seeOutcome',
      who: 'Alice'
      });
    
    return;}
  
  
  };
export async function Bob(ctc, interact) {
  if (typeof(ctc) !== 'object' || ctc.sendrecv === undefined) {
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
  const [v260, v261] = txn1.data;
  const v262 = txn1.time;
  const v263 = txn1.secs;
  const v259 = txn1.from;
  ;
  const v267 = stdlib.add(v262, v261);
  stdlib.protect(ctc1, await interact.acceptWager(v260), {
    at: './index.rsh:54:25:application',
    fs: ['at ./index.rsh:53:11:application call to [unknown function] (defined at: ./index.rsh:53:15:function exp)'],
    msg: 'acceptWager',
    who: 'Bob'
    });
  
  const txn2 = await (ctc.sendrecv({
    args: [v259, v260, v261, v267],
    evt_cnt: 0,
    funcNum: 2,
    onlyIf: true,
    out_tys: [],
    pay: [v260, []],
    sim_p: (async (txn2) => {
      const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
      
      const [] = txn2.data;
      const v272 = txn2.time;
      const v273 = txn2.secs;
      const v271 = txn2.from;
      
      const v275 = stdlib.add(v260, v260);
      sim_r.txns.push({
        amt: v260,
        kind: 'to',
        tok: undefined
        });
      const v276 = stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1);
      const v277 = v272;
      const v283 = v275;
      
      if ((() => {
        const v291 = stdlib.eq(v276, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
        
        return v291;})()) {
        const v293 = stdlib.add(v277, v261);
        sim_r.isHalt = false;
        }
      else {
        const v387 = stdlib.eq(v276, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
        const v390 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:94:12:decimal', stdlib.UInt_max, 2), v260);
        const v392 = v387 ? v259 : v271;
        sim_r.txns.push({
          amt: v390,
          kind: 'from',
          to: v392,
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
    timeoutAt: ['time', v267],
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
    const v404 = txn3.time;
    const v405 = txn3.secs;
    const v403 = txn3.from;
    ;
    const v407 = stdlib.addressEq(v259, v403);
    stdlib.assert(v407, {
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
    const v272 = txn2.time;
    const v273 = txn2.secs;
    const v271 = txn2.from;
    const v275 = stdlib.add(v260, v260);
    ;
    let v276 = stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1);
    let v277 = v272;
    let v283 = v275;
    
    while ((() => {
      const v291 = stdlib.eq(v276, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
      
      return v291;})()) {
      const v293 = stdlib.add(v277, v261);
      const txn3 = await (ctc.recv({
        evt_cnt: 1,
        funcNum: 6,
        out_tys: [ctc2],
        timeoutAt: ['time', v293],
        waitIfNotPresent: false
        }));
      if (txn3.didTimeout) {
        const txn4 = await (ctc.sendrecv({
          args: [v259, v260, v261, v271, v283, v293],
          evt_cnt: 0,
          funcNum: 7,
          onlyIf: true,
          out_tys: [],
          pay: [stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), []],
          sim_p: (async (txn4) => {
            const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
            
            const [] = txn4.data;
            const v371 = txn4.time;
            const v372 = txn4.secs;
            const v370 = txn4.from;
            
            sim_r.txns.push({
              amt: stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0),
              kind: 'to',
              tok: undefined
              });
            const v374 = stdlib.addressEq(v271, v370);
            stdlib.assert(v374, {
              at: 'reach standard library:209:7:dot',
              fs: ['at ./index.rsh:70:39:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
              msg: 'sender correct',
              who: 'Bob'
              });
            sim_r.txns.push({
              amt: v283,
              kind: 'from',
              to: v271,
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
        const v371 = txn4.time;
        const v372 = txn4.secs;
        const v370 = txn4.from;
        ;
        const v374 = stdlib.addressEq(v271, v370);
        stdlib.assert(v374, {
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
        const [v302] = txn3.data;
        const v303 = txn3.time;
        const v304 = txn3.secs;
        const v301 = txn3.from;
        ;
        const v306 = stdlib.addressEq(v259, v301);
        stdlib.assert(v306, {
          at: './index.rsh:69:11:dot',
          fs: [],
          msg: 'sender correct',
          who: 'Bob'
          });
        const v308 = stdlib.add(v303, v261);
        const v312 = stdlib.protect(ctc0, await interact.getHand(), {
          at: './index.rsh:75:50:application',
          fs: ['at ./index.rsh:74:13:application call to [unknown function] (defined at: ./index.rsh:74:17:function exp)'],
          msg: 'getHand',
          who: 'Bob'
          });
        
        const txn4 = await (ctc.sendrecv({
          args: [v259, v260, v261, v271, v283, v302, v308, v312],
          evt_cnt: 1,
          funcNum: 8,
          onlyIf: true,
          out_tys: [ctc0],
          pay: [stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), []],
          sim_p: (async (txn4) => {
            const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
            
            const [v314] = txn4.data;
            const v315 = txn4.time;
            const v316 = txn4.secs;
            const v313 = txn4.from;
            
            sim_r.txns.push({
              amt: stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0),
              kind: 'to',
              tok: undefined
              });
            const v318 = stdlib.addressEq(v271, v313);
            stdlib.assert(v318, {
              at: './index.rsh:77:9:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Bob'
              });
            const v320 = stdlib.add(v315, v261);
            sim_r.isHalt = false;
            
            return sim_r;
            }),
          soloSend: true,
          timeoutAt: ['time', v308],
          tys: [ctc4, ctc0, ctc0, ctc4, ctc0, ctc2, ctc0, ctc0],
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
          const v354 = txn5.time;
          const v355 = txn5.secs;
          const v353 = txn5.from;
          ;
          const v357 = stdlib.addressEq(v259, v353);
          stdlib.assert(v357, {
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
          const [v314] = txn4.data;
          const v315 = txn4.time;
          const v316 = txn4.secs;
          const v313 = txn4.from;
          ;
          const v318 = stdlib.addressEq(v271, v313);
          stdlib.assert(v318, {
            at: './index.rsh:77:9:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Bob'
            });
          const v320 = stdlib.add(v315, v261);
          const txn5 = await (ctc.recv({
            evt_cnt: 2,
            funcNum: 10,
            out_tys: [ctc0, ctc0],
            timeoutAt: ['time', v320],
            waitIfNotPresent: false
            }));
          if (txn5.didTimeout) {
            const txn6 = await (ctc.sendrecv({
              args: [v259, v260, v261, v271, v283, v302, v314, v320],
              evt_cnt: 0,
              funcNum: 11,
              onlyIf: true,
              out_tys: [],
              pay: [stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), []],
              sim_p: (async (txn6) => {
                const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
                
                const [] = txn6.data;
                const v337 = txn6.time;
                const v338 = txn6.secs;
                const v336 = txn6.from;
                
                sim_r.txns.push({
                  amt: stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0),
                  kind: 'to',
                  tok: undefined
                  });
                const v340 = stdlib.addressEq(v271, v336);
                stdlib.assert(v340, {
                  at: 'reach standard library:209:7:dot',
                  fs: ['at ./index.rsh:86:39:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
                  msg: 'sender correct',
                  who: 'Bob'
                  });
                sim_r.txns.push({
                  amt: v283,
                  kind: 'from',
                  to: v271,
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
              tys: [ctc4, ctc0, ctc0, ctc4, ctc0, ctc2, ctc0, ctc0],
              waitIfNotPresent: false
              }));
            const [] = txn6.data;
            const v337 = txn6.time;
            const v338 = txn6.secs;
            const v336 = txn6.from;
            ;
            const v340 = stdlib.addressEq(v271, v336);
            stdlib.assert(v340, {
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
            const [v325, v326] = txn5.data;
            const v327 = txn5.time;
            const v328 = txn5.secs;
            const v324 = txn5.from;
            ;
            const v330 = stdlib.addressEq(v259, v324);
            stdlib.assert(v330, {
              at: './index.rsh:85:11:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Bob'
              });
            const v331 = stdlib.digest(ctc3, [v325, v326]);
            const v332 = stdlib.digestEq(v302, v331);
            stdlib.assert(v332, {
              at: 'reach standard library:65:17:application',
              fs: ['at ./index.rsh:87:20:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)'],
              msg: null,
              who: 'Bob'
              });
            const v333 = stdlib.sub(stdlib.checkedBigNumberify('./index.rsh:7:14:decimal', stdlib.UInt_max, 4), v314);
            const v334 = stdlib.add(v326, v333);
            const v335 = stdlib.mod(v334, stdlib.checkedBigNumberify('./index.rsh:7:28:decimal', stdlib.UInt_max, 3));
            const cv276 = v335;
            const cv277 = v327;
            const cv283 = v283;
            
            v276 = cv276;
            v277 = cv277;
            v283 = cv283;
            
            continue;}
          }
        }
      }
    const v387 = stdlib.eq(v276, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
    const v390 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:94:12:decimal', stdlib.UInt_max, 2), v260);
    const v392 = v387 ? v259 : v271;
    ;
    stdlib.protect(ctc1, await interact.seeOutcome(v276), {
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
load 250
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
load 251
itob
concat
load 249
concat
load 248
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
load 250
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
load 251
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
substring 80 88
btoi
store 251
dup
substring 88 120
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
itob
concat
load 250
concat
load 249
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
itob
concat
load 250
concat
load 248
itob
concat
load 247
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
substring 80 88
btoi
store 251
dup
substring 88 120
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
itob
concat
load 250
concat
load 249
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
// "[at ./index.rsh:78:39:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
// Just "sender correct"
// "reach standard library:209:7:dot"
// "[at ./index.rsh:78:39:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
load 255
txn Sender
==
assert
load 251
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
substring 80 88
btoi
store 251
dup
substring 88 120
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
itob
concat
load 250
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
load 248
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
load 250
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
load 249
-
+
int 3
%
itob
global Round
itob
concat
load 251
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
substring 80 88
btoi
store 251
dup
substring 88 120
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
itob
concat
load 250
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
load 248
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
load 251
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
load 253
itob
concat
load 248
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
                "name": "v260",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v261",
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
                "name": "v259",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v260",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v261",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v271",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v283",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v302",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v314",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v320",
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
                "name": "v325",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v326",
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
                "name": "v259",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v260",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v261",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v271",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v283",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v302",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v314",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v320",
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
                "name": "v259",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v260",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v261",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v267",
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
                "name": "v259",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v260",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v261",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v267",
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
                "name": "v259",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v260",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v261",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v271",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v283",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v293",
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
                "name": "v302",
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
                "name": "v259",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v260",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v261",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v271",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v283",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v293",
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
                "name": "v259",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v260",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v261",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v271",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v283",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v302",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v308",
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
                "name": "v314",
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
                "name": "v259",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v260",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v261",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v271",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v283",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v302",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v308",
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
                "name": "v260",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v261",
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
                "name": "v259",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v260",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v261",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v271",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v283",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v302",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v314",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v320",
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
                "name": "v325",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v326",
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
                "name": "v259",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v260",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v261",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v271",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v283",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v302",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v314",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v320",
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
                "name": "v259",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v260",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v261",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v267",
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
                "name": "v259",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v260",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v261",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v267",
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
                "name": "v259",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v260",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v261",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v271",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v283",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v293",
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
                "name": "v302",
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
                "name": "v259",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v260",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v261",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v271",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v283",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v293",
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
                "name": "v259",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v260",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v261",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v271",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v283",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v302",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v308",
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
                "name": "v314",
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
                "name": "v259",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v260",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v261",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v271",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v283",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v302",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v308",
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
  Bytecode: `0x608060408190527f49ff028a829527a47ec6839c7147b484eccf5a2a94853eddac09cef44d9d4e9e90600090a1604080518082018252438152426020918201528151600081830181905281840181905283518083038501815260609092019093528051910120905561138b806100766000396000f3fe60806040526004361061008a5760003560e01c806360bafff71161005957806360bafff7146100e457806361ff86f7146100f7578063aa8594ca1461010a578063e4f83a8f1461011d578063ebc5780c1461013057600080fd5b8063108acff51461009657806338015b16146100ab5780633df7a789146100be578063497ccc6a146100d157600080fd5b3661009157005b600080fd5b6100a96100a4366004610fe5565b610143565b005b6100a96100b9366004610fc0565b61024a565b6100a96100cc366004610f90565b61034f565b6100a96100df366004610f90565b610571565b6100a96100f2366004610fad565b610635565b6100a9610105366004610f74565b6107e4565b6100a9610118366004610fd3565b61089e565b6100a961012b366004610fe5565b6109c8565b6100a961013e366004610f74565b610ad1565b60405161017f9061015b9060019084906020016112a7565b6040516020818303038152906040528051906020012060001c60005414600e610ca4565b600080556101956060820135431015600f610ca4565b7f613b30050768160fb8fb1fedba26a4639c7df8d370861f403061c2d46f9802e5816040516101c49190611256565b60405180910390a16101d83415600c610ca4565b6101fa336101e96020840184610f37565b6001600160a01b031614600d610ca4565b6102076020820182610f37565b6040516001600160a01b039190911690602083013580156108fc02916000818181858888f19350505050158015610242573d6000803e3d6000fd5b506000805533ff5b6040516102869061026290600a908490602001611292565b6040516020818303038152906040528051906020012060001c600054146027610ca4565b6000805561029c60e08201354310156028610ca4565b7fb9d135d4afaa7938b4616c3637968c0e71f66413b54043da6188989930963f89816040516102cb91906111fb565b60405180910390a16102df34156025610ca4565b610304336102f36080840160608501610f37565b6001600160a01b0316146026610ca4565b6103146080820160608301610f37565b6040516001600160a01b039190911690608083013580156108fc02916000818181858888f19350505050158015610242573d6000803e3d6000fd5b60405161038b9061036790600890849060200161127d565b6040516020818303038152906040528051906020012060001c60005414601a610ca4565b600080805560408051602081019091529081526103af60c08301354310601b610ca4565b7f6739bb4acbb3812eea51c48895245b154776bf02dd61571666f6abba93266fec826040516103de919061118a565b60405180910390a16103f234156018610ca4565b610417336104066080850160608601610f37565b6001600160a01b0316146019610ca4565b6104256040830135436112cf565b8152604080516101008101825260008082526020808301829052928201819052606082018190526080820181905260a0820181905260c0820181905260e08201529061047390840184610f37565b6001600160a01b0316815260208084013590820152604080840135908201526104a26080840160608501610f37565b6001600160a01b031660608201526080808401359082015260a0808401359082015260e08084013560c083015282519082015260405161055190600a90839060200160006101208201905083825260018060a01b03808451166020840152602084015160408401526040840151606084015280606085015116608084015250608083015160a083015260a083015160c083015260c083015160e083015260e08301516101008301529392505050565b60408051601f198184030181529190528051602090910120600055505050565b6040516105ad9061058990600890849060200161127d565b6040516020818303038152906040528051906020012060001c60005414601e610ca4565b600080556105c360c0820135431015601f610ca4565b7fcdae4cbd433f8c4039f23f2632824e3ab0089e9b8c2050e8e87e4d6e0a3df09b816040516105f291906111a7565b60405180910390a16106063415601c610ca4565b610628336106176020840184610f37565b6001600160a01b031614601d610ca4565b6103146020820182610f37565b6040516106719061064d90600a908490602001611292565b6040516020818303038152906040528051906020012060001c600054146023610ca4565b6000805561068660e082013543106024610ca4565b7f58822179cfd9cab18ada288d4f94f3d5babb851e53b3ed667629d3cbbbb0cb72816040516106b591906111cf565b60405180910390a16106c934156020610ca4565b6106eb336106da6020840184610f37565b6001600160a01b0316146021610ca4565b604080516101008301356020820152610120830135918101919091526107339060600160408051601f19818403018152919052805160209091012060a0830135146022610ca4565b61073b610e87565b6107486020830183610f37565b81516001600160a01b0390911690528051602080840135910152805160408084013591015261077d6080830160608401610f37565b81516001600160a01b0390911660609091015260036107a160c08401356004611306565b6107b0906101208501356112cf565b6107ba919061131d565b60208083018051929092528151439101525160808301356040909101526107e081610cc9565b5050565b604051610820906107fc9060069084906020016112bb565b6040516020818303038152906040528051906020012060001c600054146016610ca4565b6000805561083660a08201354310156017610ca4565b7fcc997a9af4abe95cf593cbeb34368171b4d5923d8562b1e54e51006451978b7c816040516108659190611163565b60405180910390a161087934156014610ca4565b6103043361088d6080840160608501610f37565b6001600160a01b0316146015610ca4565b6108f060006108b06020840184610f59565b6040516020016108cc9291909182521515602082015260400190565b6040516020818303038152906040528051906020012060001c600054146008610ca4565b600080805560408051602081018252918252517f3c669845d6bbc6fc367c9fa11ea5c8ec9bfd3d70eae7b34375b93f114f74036590610930908490611225565b60405180910390a1610949346020840135146007610ca4565b6109576040830135436112cf565b81526040805160808082018352600060608084019182523384526020878101358186019081528887013586880190815288518552875160019381019390935286516001600160a01b03169783019790975251918101919091529351918401919091525160a08301529060c001610551565b604051610a04906109e09060019084906020016112a7565b6040516020818303038152906040528051906020012060001c60005414600a610ca4565b60008055610a1960608201354310600b610ca4565b7fe2fcb5361608dd42d825c4e917fd4fca89057bb8eb0b7e34b8c2813a114cc15281604051610a489190611256565b60405180910390a1610a61346020830135146009610ca4565b610a69610e87565b610a766020830183610f37565b81516001600160a01b039091169052805160208084013591810182905282516040808601359101528251336060909101528083018051600190525143910152610abf90806112cf565b6020820151604001526107e081610cc9565b604051610b0d90610ae99060069084906020016112bb565b6040516020818303038152906040528051906020012060001c600054146012610ca4565b60008080556040805160208101909152908152610b3160a083013543106013610ca4565b7fee6496cc7d28a7c9121e0eebf62951277792d31c40832817d1a4c372e06eb46182604051610b609190611147565b60405180910390a1610b7434156010610ca4565b610b9633610b856020850185610f37565b6001600160a01b0316146011610ca4565b610ba46040830135436112cf565b81526040805160e08101825260008082526020808301829052928201819052606082018190526080820181905260a0820181905260c082015290610bea90840184610f37565b6001600160a01b031681526020808401359082015260408084013590820152610c196080840160608501610f37565b6001600160a01b03908116606083810191825260808681013581860190815260c08089013560a08089019182528951838a019081526040805160086020808301919091528c518c16828401528c0151988101989098528a0151958701959095529551909616948401949094525192820192909252915160e08301525161010082015261012001610551565b816107e05760405163100960cb60e01b81526004810182905260240160405180910390fd5b60408051602081019091526000815260208201515160011415610db4578151604001516020808401510151610cfe91906112cf565b81526040805160c08082018352600080835260208084018281528486018381526060808701858152608080890187815260a0808b019889528d51516001600160a01b039081168c528e5189015188528e518d015187528e51860151811685528e8901518d015183528d518a528c516006998101999099528b5181169c89019c909c529551938701939093529251928501929092529051909616908201529351918401919091525160e08301529061010001610551565b6040805160c081018252600091810182815260608083018481526080840185815260a085018681528486526020808701979097528851516001600160a01b03908116909552885187015190925287519092015190921690529184015151909152610e1d81610e22565b505050565b805160600151600214610e3a57805160400151610e3e565b8051515b6001600160a01b03166108fc8260000151602001516002610e5f91906112e7565b6040518115909202916000818181858888f19350505050158015610242573d6000803e3d6000fd5b6040805160c0810182526000918101828152606082018390526080820183905260a08201929092529081908152602001610edb60405180606001604052806000815260200160008152602001600081525090565b905290565b80356001600160a01b0381168114610ef757600080fd5b919050565b80358015158114610ef757600080fd5b600060e08284031215610f1e57600080fd5b50919050565b60006101008284031215610f1e57600080fd5b600060208284031215610f4957600080fd5b610f5282610ee0565b9392505050565b600060208284031215610f6b57600080fd5b610f5282610efc565b600060e08284031215610f8657600080fd5b610f528383610f0c565b60006101008284031215610fa357600080fd5b610f528383610f24565b60006101408284031215610f1e57600080fd5b60006101208284031215610f1e57600080fd5b600060608284031215610f1e57600080fd5b600060a08284031215610f1e57600080fd5b6001600160a01b038061100983610ee0565b16835260208201356020840152604082013560408401528061102d60608401610ee0565b166060840152506080810135608083015260a081013560a083015260c081013560c08301525050565b6001600160a01b038061106883610ee0565b16835260208201356020840152604082013560408401528061108c60608401610ee0565b166060840152506080810135608083015260a081013560a083015260c081013560c083015260e081013560e08301525050565b6001600160a01b036110d082610ee0565b1682526020810135602083015260408101356040830152606081013560608301525050565b6001600160a01b038061110783610ee0565b16835260208201356020840152604082013560408401528061112b60608401610ee0565b166060840152506080818101359083015260a090810135910152565b60e0810161115582846110f5565b60c092830135919092015290565b60e0810161117182846110f5565b61117d60c08401610efc565b151560c083015292915050565b61010081016111998284610ff7565b60e092830135919092015290565b61010081016111b68284610ff7565b6111c260e08401610efc565b151560e083015292915050565b61014081016111de8284611056565b610100838101358382015261012080850135908401525092915050565b610120810161120a8284611056565b610100611218818501610efc565b1515818401525092915050565b6060810161123283610efc565b15158252611250602083016020850180358252602090810135910152565b92915050565b60a0810161126482846110bf565b61127060808401610efc565b1515608083015292915050565b8281526101008101610f526020830184610ff7565b8281526101208101610f526020830184611056565b82815260a08101610f5260208301846110bf565b82815260e08101610f5260208301846110f5565b600082198211156112e2576112e261133f565b500190565b60008160001904831182151516156113015761130161133f565b500290565b6000828210156113185761131861133f565b500390565b60008261133a57634e487b7160e01b600052601260045260246000fd5b500690565b634e487b7160e01b600052601160045260246000fdfea26469706673582212202c0c168e605664e092a548450521f9a376270fdb37aa1a542e81fbdc4b78e2b164736f6c63430008060033`,
  BytecodeLen: 5121,
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

