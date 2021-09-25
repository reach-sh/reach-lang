// Automatically generated with Reach 0.1.5
/* eslint-disable */
export const _version = '0.1.5';
export const _backendVersion = 3;


export function getExports(s) {
  const stdlib = s.reachStdlib;
  return {
    };
  };

export function _getViews(s, viewlib) {
  const stdlib = s.reachStdlib;
  const ctc0 = stdlib.T_Address;
  const ctc1 = stdlib.T_UInt;
  const ctc2 = stdlib.T_Digest;
  
  return {
    infos: {
      },
    views: {
      1: [ctc0, ctc1, ctc1, ctc1],
      6: [ctc0, ctc1, ctc1, ctc0, ctc1, ctc1],
      8: [ctc0, ctc1, ctc1, ctc0, ctc1, ctc2, ctc1],
      10: [ctc0, ctc1, ctc1, ctc0, ctc1, ctc2, ctc1, ctc1]
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
  
  
  const v263 = stdlib.protect(ctc0, interact.deadline, 'for Alice\'s interact field deadline');
  const v264 = stdlib.protect(ctc0, interact.wager, 'for Alice\'s interact field wager');
  
  const txn1 = await (ctc.sendrecv({
    args: [v264, v263],
    evt_cnt: 2,
    funcNum: 0,
    lct: stdlib.checkedBigNumberify('./examples/tut-8/index.rsh:49:9:dot', stdlib.UInt_max, 0),
    onlyIf: true,
    out_tys: [ctc0, ctc0],
    pay: [v264, []],
    sim_p: (async (txn1) => {
      const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
      
      const {data: [v268, v269], secs: v271, time: v270, didSend: v52, from: v267 } = txn1;
      
      sim_r.txns.push({
        amt: v268,
        kind: 'to',
        tok: undefined
        });
      const v275 = stdlib.add(v270, v269);
      sim_r.isHalt = false;
      
      return sim_r;
      }),
    soloSend: true,
    timeoutAt: undefined,
    tys: [ctc0, ctc0],
    waitIfNotPresent: false
    }));
  const {data: [v268, v269], secs: v271, time: v270, didSend: v52, from: v267 } = txn1;
  ;
  const v275 = stdlib.add(v270, v269);
  const txn2 = await (ctc.recv({
    didSend: false,
    evt_cnt: 0,
    funcNum: 1,
    out_tys: [],
    timeoutAt: ['time', v275],
    waitIfNotPresent: false
    }));
  if (txn2.didTimeout) {
    const txn3 = await (ctc.sendrecv({
      args: [v267, v268, v269, v275],
      evt_cnt: 0,
      funcNum: 2,
      lct: v270,
      onlyIf: true,
      out_tys: [],
      pay: [stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), []],
      sim_p: (async (txn3) => {
        const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
        
        const {data: [], secs: v414, time: v413, didSend: v223, from: v412 } = txn3;
        
        sim_r.txns.push({
          amt: stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0),
          kind: 'to',
          tok: undefined
          });
        const v416 = stdlib.addressEq(v267, v412);
        stdlib.assert(v416, {
          at: 'reach standard library:209:7:dot',
          fs: ['at ./examples/tut-8/index.rsh:57:51:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
          msg: 'sender correct',
          who: 'Alice'
          });
        sim_r.txns.push({
          amt: v268,
          kind: 'from',
          to: v267,
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
    const {data: [], secs: v414, time: v413, didSend: v223, from: v412 } = txn3;
    ;
    const v416 = stdlib.addressEq(v267, v412);
    stdlib.assert(v416, {
      at: 'reach standard library:209:7:dot',
      fs: ['at ./examples/tut-8/index.rsh:57:51:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
      msg: 'sender correct',
      who: 'Alice'
      });
    ;
    stdlib.protect(ctc3, await interact.informTimeout(), {
      at: './examples/tut-8/index.rsh:41:29:application',
      fs: ['at ./examples/tut-8/index.rsh:40:9:application call to [unknown function] (defined at: ./examples/tut-8/index.rsh:40:27:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./examples/tut-8/index.rsh:39:28:function exp)', 'at ./examples/tut-8/index.rsh:57:51:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
      msg: 'informTimeout',
      who: 'Alice'
      });
    
    return;
    }
  else {
    const {data: [], secs: v281, time: v280, didSend: v61, from: v279 } = txn2;
    const v283 = stdlib.add(v268, v268);
    ;
    let v284 = stdlib.checkedBigNumberify('./examples/tut-8/index.rsh:makeEnum', stdlib.UInt_max, 1);
    let v285 = v280;
    let v292 = v283;
    
    while ((() => {
      const v300 = stdlib.eq(v284, stdlib.checkedBigNumberify('./examples/tut-8/index.rsh:makeEnum', stdlib.UInt_max, 1));
      
      return v300;})()) {
      const v302 = stdlib.add(v285, v269);
      const v306 = stdlib.protect(ctc0, await interact.getHand(), {
        at: './examples/tut-8/index.rsh:65:42:application',
        fs: ['at ./examples/tut-8/index.rsh:64:15:application call to [unknown function] (defined at: ./examples/tut-8/index.rsh:64:19:function exp)'],
        msg: 'getHand',
        who: 'Alice'
        });
      const v307 = stdlib.protect(ctc0, await interact.random(), {
        at: 'reach standard library:60:31:application',
        fs: ['at ./examples/tut-8/index.rsh:66:56:application call to "makeCommitment" (defined at: reach standard library:59:8:function exp)', 'at ./examples/tut-8/index.rsh:64:15:application call to [unknown function] (defined at: ./examples/tut-8/index.rsh:64:19:function exp)'],
        msg: 'random',
        who: 'Alice'
        });
      const v308 = stdlib.digest(ctc1, [v307, v306]);
      
      const txn3 = await (ctc.sendrecv({
        args: [v267, v268, v269, v279, v292, v302, v308],
        evt_cnt: 1,
        funcNum: 5,
        lct: v285,
        onlyIf: true,
        out_tys: [ctc2],
        pay: [stdlib.checkedBigNumberify('./examples/tut-8/index.rsh:decimal', stdlib.UInt_max, 0), []],
        sim_p: (async (txn3) => {
          const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
          
          const {data: [v311], secs: v313, time: v312, didSend: v88, from: v310 } = txn3;
          
          sim_r.txns.push({
            amt: stdlib.checkedBigNumberify('./examples/tut-8/index.rsh:decimal', stdlib.UInt_max, 0),
            kind: 'to',
            tok: undefined
            });
          const v315 = stdlib.addressEq(v267, v310);
          stdlib.assert(v315, {
            at: './examples/tut-8/index.rsh:69:11:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Alice'
            });
          const v317 = stdlib.add(v312, v269);
          sim_r.isHalt = false;
          
          return sim_r;
          }),
        soloSend: true,
        timeoutAt: ['time', v302],
        tys: [ctc4, ctc0, ctc0, ctc4, ctc0, ctc0, ctc2],
        waitIfNotPresent: false
        }));
      if (txn3.didTimeout) {
        const txn4 = await (ctc.recv({
          didSend: false,
          evt_cnt: 0,
          funcNum: 6,
          out_tys: [],
          timeoutAt: undefined,
          waitIfNotPresent: false
          }));
        const {data: [], secs: v381, time: v380, didSend: v181, from: v379 } = txn4;
        ;
        const v383 = stdlib.addressEq(v279, v379);
        stdlib.assert(v383, {
          at: 'reach standard library:209:7:dot',
          fs: ['at ./examples/tut-8/index.rsh:70:53:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
          msg: 'sender correct',
          who: 'Alice'
          });
        ;
        stdlib.protect(ctc3, await interact.informTimeout(), {
          at: './examples/tut-8/index.rsh:41:29:application',
          fs: ['at ./examples/tut-8/index.rsh:40:9:application call to [unknown function] (defined at: ./examples/tut-8/index.rsh:40:27:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./examples/tut-8/index.rsh:39:28:function exp)', 'at ./examples/tut-8/index.rsh:70:53:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
          msg: 'informTimeout',
          who: 'Alice'
          });
        
        return;
        }
      else {
        const {data: [v311], secs: v313, time: v312, didSend: v88, from: v310 } = txn3;
        ;
        const v315 = stdlib.addressEq(v267, v310);
        stdlib.assert(v315, {
          at: './examples/tut-8/index.rsh:69:11:dot',
          fs: [],
          msg: 'sender correct',
          who: 'Alice'
          });
        const v317 = stdlib.add(v312, v269);
        const txn4 = await (ctc.recv({
          didSend: false,
          evt_cnt: 1,
          funcNum: 7,
          out_tys: [ctc0],
          timeoutAt: ['time', v317],
          waitIfNotPresent: false
          }));
        if (txn4.didTimeout) {
          const txn5 = await (ctc.sendrecv({
            args: [v267, v268, v269, v279, v292, v311, v317],
            evt_cnt: 0,
            funcNum: 8,
            lct: v312,
            onlyIf: true,
            out_tys: [],
            pay: [stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), []],
            sim_p: (async (txn5) => {
              const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
              
              const {data: [], secs: v364, time: v363, didSend: v155, from: v362 } = txn5;
              
              sim_r.txns.push({
                amt: stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0),
                kind: 'to',
                tok: undefined
                });
              const v366 = stdlib.addressEq(v267, v362);
              stdlib.assert(v366, {
                at: 'reach standard library:209:7:dot',
                fs: ['at ./examples/tut-8/index.rsh:78:53:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
                msg: 'sender correct',
                who: 'Alice'
                });
              sim_r.txns.push({
                amt: v292,
                kind: 'from',
                to: v267,
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
          const {data: [], secs: v364, time: v363, didSend: v155, from: v362 } = txn5;
          ;
          const v366 = stdlib.addressEq(v267, v362);
          stdlib.assert(v366, {
            at: 'reach standard library:209:7:dot',
            fs: ['at ./examples/tut-8/index.rsh:78:53:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
            msg: 'sender correct',
            who: 'Alice'
            });
          ;
          stdlib.protect(ctc3, await interact.informTimeout(), {
            at: './examples/tut-8/index.rsh:41:29:application',
            fs: ['at ./examples/tut-8/index.rsh:40:9:application call to [unknown function] (defined at: ./examples/tut-8/index.rsh:40:27:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./examples/tut-8/index.rsh:39:28:function exp)', 'at ./examples/tut-8/index.rsh:78:53:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
            msg: 'informTimeout',
            who: 'Alice'
            });
          
          return;
          }
        else {
          const {data: [v323], secs: v325, time: v324, didSend: v99, from: v322 } = txn4;
          ;
          const v327 = stdlib.addressEq(v279, v322);
          stdlib.assert(v327, {
            at: './examples/tut-8/index.rsh:77:9:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Alice'
            });
          const v329 = stdlib.add(v324, v269);
          const txn5 = await (ctc.sendrecv({
            args: [v267, v268, v269, v279, v292, v311, v323, v329, v307, v306],
            evt_cnt: 2,
            funcNum: 9,
            lct: v324,
            onlyIf: true,
            out_tys: [ctc0, ctc0],
            pay: [stdlib.checkedBigNumberify('./examples/tut-8/index.rsh:decimal', stdlib.UInt_max, 0), []],
            sim_p: (async (txn5) => {
              const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
              
              const {data: [v334, v335], secs: v337, time: v336, didSend: v110, from: v333 } = txn5;
              
              sim_r.txns.push({
                amt: stdlib.checkedBigNumberify('./examples/tut-8/index.rsh:decimal', stdlib.UInt_max, 0),
                kind: 'to',
                tok: undefined
                });
              const v339 = stdlib.addressEq(v267, v333);
              stdlib.assert(v339, {
                at: './examples/tut-8/index.rsh:85:11:dot',
                fs: [],
                msg: 'sender correct',
                who: 'Alice'
                });
              const v340 = stdlib.digest(ctc1, [v334, v335]);
              const v341 = stdlib.digestEq(v311, v340);
              stdlib.assert(v341, {
                at: 'reach standard library:65:17:application',
                fs: ['at ./examples/tut-8/index.rsh:87:20:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)'],
                msg: null,
                who: 'Alice'
                });
              const v342 = stdlib.sub(stdlib.checkedBigNumberify('./examples/tut-8/index.rsh:7:14:decimal', stdlib.UInt_max, 4), v323);
              const v343 = stdlib.add(v335, v342);
              const v344 = stdlib.mod(v343, stdlib.checkedBigNumberify('./examples/tut-8/index.rsh:7:28:decimal', stdlib.UInt_max, 3));
              const cv284 = v344;
              const cv285 = v336;
              const cv292 = v292;
              
              (() => {
                const v284 = cv284;
                const v285 = cv285;
                const v292 = cv292;
                
                if ((() => {
                  const v300 = stdlib.eq(v284, stdlib.checkedBigNumberify('./examples/tut-8/index.rsh:makeEnum', stdlib.UInt_max, 1));
                  
                  return v300;})()) {
                  const v302 = stdlib.add(v285, v269);
                  sim_r.isHalt = false;
                  }
                else {
                  const v396 = stdlib.eq(v284, stdlib.checkedBigNumberify('./examples/tut-8/index.rsh:makeEnum', stdlib.UInt_max, 2));
                  const v399 = stdlib.mul(stdlib.checkedBigNumberify('./examples/tut-8/index.rsh:94:12:decimal', stdlib.UInt_max, 2), v268);
                  const v401 = v396 ? v267 : v279;
                  sim_r.txns.push({
                    amt: v399,
                    kind: 'from',
                    to: v401,
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
            timeoutAt: ['time', v329],
            tys: [ctc4, ctc0, ctc0, ctc4, ctc0, ctc2, ctc0, ctc0, ctc0, ctc0],
            waitIfNotPresent: false
            }));
          if (txn5.didTimeout) {
            const txn6 = await (ctc.recv({
              didSend: false,
              evt_cnt: 0,
              funcNum: 10,
              out_tys: [],
              timeoutAt: undefined,
              waitIfNotPresent: false
              }));
            const {data: [], secs: v347, time: v346, didSend: v129, from: v345 } = txn6;
            ;
            const v349 = stdlib.addressEq(v279, v345);
            stdlib.assert(v349, {
              at: 'reach standard library:209:7:dot',
              fs: ['at ./examples/tut-8/index.rsh:86:53:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
              msg: 'sender correct',
              who: 'Alice'
              });
            ;
            stdlib.protect(ctc3, await interact.informTimeout(), {
              at: './examples/tut-8/index.rsh:41:29:application',
              fs: ['at ./examples/tut-8/index.rsh:40:9:application call to [unknown function] (defined at: ./examples/tut-8/index.rsh:40:27:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./examples/tut-8/index.rsh:39:28:function exp)', 'at ./examples/tut-8/index.rsh:86:53:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
              msg: 'informTimeout',
              who: 'Alice'
              });
            
            return;
            }
          else {
            const {data: [v334, v335], secs: v337, time: v336, didSend: v110, from: v333 } = txn5;
            ;
            const v339 = stdlib.addressEq(v267, v333);
            stdlib.assert(v339, {
              at: './examples/tut-8/index.rsh:85:11:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Alice'
              });
            const v340 = stdlib.digest(ctc1, [v334, v335]);
            const v341 = stdlib.digestEq(v311, v340);
            stdlib.assert(v341, {
              at: 'reach standard library:65:17:application',
              fs: ['at ./examples/tut-8/index.rsh:87:20:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)'],
              msg: null,
              who: 'Alice'
              });
            const v342 = stdlib.sub(stdlib.checkedBigNumberify('./examples/tut-8/index.rsh:7:14:decimal', stdlib.UInt_max, 4), v323);
            const v343 = stdlib.add(v335, v342);
            const v344 = stdlib.mod(v343, stdlib.checkedBigNumberify('./examples/tut-8/index.rsh:7:28:decimal', stdlib.UInt_max, 3));
            const cv284 = v344;
            const cv285 = v336;
            const cv292 = v292;
            
            v284 = cv284;
            v285 = cv285;
            v292 = cv292;
            
            continue;}
          }
        }
      }
    const v396 = stdlib.eq(v284, stdlib.checkedBigNumberify('./examples/tut-8/index.rsh:makeEnum', stdlib.UInt_max, 2));
    const v399 = stdlib.mul(stdlib.checkedBigNumberify('./examples/tut-8/index.rsh:94:12:decimal', stdlib.UInt_max, 2), v268);
    const v401 = v396 ? v267 : v279;
    ;
    stdlib.protect(ctc3, await interact.seeOutcome(v284), {
      at: './examples/tut-8/index.rsh:98:24:application',
      fs: ['at ./examples/tut-8/index.rsh:97:7:application call to [unknown function] (defined at: ./examples/tut-8/index.rsh:97:25:function exp)'],
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
  
  
  const txn1 = await (ctc.recv({
    didSend: false,
    evt_cnt: 2,
    funcNum: 0,
    out_tys: [ctc0, ctc0],
    timeoutAt: undefined,
    waitIfNotPresent: false
    }));
  const {data: [v268, v269], secs: v271, time: v270, didSend: v52, from: v267 } = txn1;
  ;
  const v275 = stdlib.add(v270, v269);
  stdlib.protect(ctc1, await interact.acceptWager(v268), {
    at: './examples/tut-8/index.rsh:54:25:application',
    fs: ['at ./examples/tut-8/index.rsh:53:11:application call to [unknown function] (defined at: ./examples/tut-8/index.rsh:53:15:function exp)'],
    msg: 'acceptWager',
    who: 'Bob'
    });
  
  const txn2 = await (ctc.sendrecv({
    args: [v267, v268, v269, v275],
    evt_cnt: 0,
    funcNum: 1,
    lct: v270,
    onlyIf: true,
    out_tys: [],
    pay: [v268, []],
    sim_p: (async (txn2) => {
      const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
      
      const {data: [], secs: v281, time: v280, didSend: v61, from: v279 } = txn2;
      
      const v283 = stdlib.add(v268, v268);
      sim_r.txns.push({
        amt: v268,
        kind: 'to',
        tok: undefined
        });
      const v284 = stdlib.checkedBigNumberify('./examples/tut-8/index.rsh:makeEnum', stdlib.UInt_max, 1);
      const v285 = v280;
      const v292 = v283;
      
      if ((() => {
        const v300 = stdlib.eq(v284, stdlib.checkedBigNumberify('./examples/tut-8/index.rsh:makeEnum', stdlib.UInt_max, 1));
        
        return v300;})()) {
        const v302 = stdlib.add(v285, v269);
        sim_r.isHalt = false;
        }
      else {
        const v396 = stdlib.eq(v284, stdlib.checkedBigNumberify('./examples/tut-8/index.rsh:makeEnum', stdlib.UInt_max, 2));
        const v399 = stdlib.mul(stdlib.checkedBigNumberify('./examples/tut-8/index.rsh:94:12:decimal', stdlib.UInt_max, 2), v268);
        const v401 = v396 ? v267 : v279;
        sim_r.txns.push({
          amt: v399,
          kind: 'from',
          to: v401,
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
    timeoutAt: ['time', v275],
    tys: [ctc4, ctc0, ctc0, ctc0],
    waitIfNotPresent: false
    }));
  if (txn2.didTimeout) {
    const txn3 = await (ctc.recv({
      didSend: false,
      evt_cnt: 0,
      funcNum: 2,
      out_tys: [],
      timeoutAt: undefined,
      waitIfNotPresent: false
      }));
    const {data: [], secs: v414, time: v413, didSend: v223, from: v412 } = txn3;
    ;
    const v416 = stdlib.addressEq(v267, v412);
    stdlib.assert(v416, {
      at: 'reach standard library:209:7:dot',
      fs: ['at ./examples/tut-8/index.rsh:57:51:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
      msg: 'sender correct',
      who: 'Bob'
      });
    ;
    stdlib.protect(ctc1, await interact.informTimeout(), {
      at: './examples/tut-8/index.rsh:41:29:application',
      fs: ['at ./examples/tut-8/index.rsh:40:9:application call to [unknown function] (defined at: ./examples/tut-8/index.rsh:40:27:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./examples/tut-8/index.rsh:39:28:function exp)', 'at ./examples/tut-8/index.rsh:57:51:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
      msg: 'informTimeout',
      who: 'Bob'
      });
    
    return;
    }
  else {
    const {data: [], secs: v281, time: v280, didSend: v61, from: v279 } = txn2;
    const v283 = stdlib.add(v268, v268);
    ;
    let v284 = stdlib.checkedBigNumberify('./examples/tut-8/index.rsh:makeEnum', stdlib.UInt_max, 1);
    let v285 = v280;
    let v292 = v283;
    
    while ((() => {
      const v300 = stdlib.eq(v284, stdlib.checkedBigNumberify('./examples/tut-8/index.rsh:makeEnum', stdlib.UInt_max, 1));
      
      return v300;})()) {
      const v302 = stdlib.add(v285, v269);
      const txn3 = await (ctc.recv({
        didSend: false,
        evt_cnt: 1,
        funcNum: 5,
        out_tys: [ctc2],
        timeoutAt: ['time', v302],
        waitIfNotPresent: false
        }));
      if (txn3.didTimeout) {
        const txn4 = await (ctc.sendrecv({
          args: [v267, v268, v269, v279, v292, v302],
          evt_cnt: 0,
          funcNum: 6,
          lct: v285,
          onlyIf: true,
          out_tys: [],
          pay: [stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), []],
          sim_p: (async (txn4) => {
            const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
            
            const {data: [], secs: v381, time: v380, didSend: v181, from: v379 } = txn4;
            
            sim_r.txns.push({
              amt: stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0),
              kind: 'to',
              tok: undefined
              });
            const v383 = stdlib.addressEq(v279, v379);
            stdlib.assert(v383, {
              at: 'reach standard library:209:7:dot',
              fs: ['at ./examples/tut-8/index.rsh:70:53:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
              msg: 'sender correct',
              who: 'Bob'
              });
            sim_r.txns.push({
              amt: v292,
              kind: 'from',
              to: v279,
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
        const {data: [], secs: v381, time: v380, didSend: v181, from: v379 } = txn4;
        ;
        const v383 = stdlib.addressEq(v279, v379);
        stdlib.assert(v383, {
          at: 'reach standard library:209:7:dot',
          fs: ['at ./examples/tut-8/index.rsh:70:53:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
          msg: 'sender correct',
          who: 'Bob'
          });
        ;
        stdlib.protect(ctc1, await interact.informTimeout(), {
          at: './examples/tut-8/index.rsh:41:29:application',
          fs: ['at ./examples/tut-8/index.rsh:40:9:application call to [unknown function] (defined at: ./examples/tut-8/index.rsh:40:27:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./examples/tut-8/index.rsh:39:28:function exp)', 'at ./examples/tut-8/index.rsh:70:53:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
          msg: 'informTimeout',
          who: 'Bob'
          });
        
        return;
        }
      else {
        const {data: [v311], secs: v313, time: v312, didSend: v88, from: v310 } = txn3;
        ;
        const v315 = stdlib.addressEq(v267, v310);
        stdlib.assert(v315, {
          at: './examples/tut-8/index.rsh:69:11:dot',
          fs: [],
          msg: 'sender correct',
          who: 'Bob'
          });
        const v317 = stdlib.add(v312, v269);
        const v321 = stdlib.protect(ctc0, await interact.getHand(), {
          at: './examples/tut-8/index.rsh:75:50:application',
          fs: ['at ./examples/tut-8/index.rsh:74:13:application call to [unknown function] (defined at: ./examples/tut-8/index.rsh:74:17:function exp)'],
          msg: 'getHand',
          who: 'Bob'
          });
        
        const txn4 = await (ctc.sendrecv({
          args: [v267, v268, v269, v279, v292, v311, v317, v321],
          evt_cnt: 1,
          funcNum: 7,
          lct: v312,
          onlyIf: true,
          out_tys: [ctc0],
          pay: [stdlib.checkedBigNumberify('./examples/tut-8/index.rsh:decimal', stdlib.UInt_max, 0), []],
          sim_p: (async (txn4) => {
            const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
            
            const {data: [v323], secs: v325, time: v324, didSend: v99, from: v322 } = txn4;
            
            sim_r.txns.push({
              amt: stdlib.checkedBigNumberify('./examples/tut-8/index.rsh:decimal', stdlib.UInt_max, 0),
              kind: 'to',
              tok: undefined
              });
            const v327 = stdlib.addressEq(v279, v322);
            stdlib.assert(v327, {
              at: './examples/tut-8/index.rsh:77:9:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Bob'
              });
            const v329 = stdlib.add(v324, v269);
            sim_r.isHalt = false;
            
            return sim_r;
            }),
          soloSend: true,
          timeoutAt: ['time', v317],
          tys: [ctc4, ctc0, ctc0, ctc4, ctc0, ctc2, ctc0, ctc0],
          waitIfNotPresent: false
          }));
        if (txn4.didTimeout) {
          const txn5 = await (ctc.recv({
            didSend: false,
            evt_cnt: 0,
            funcNum: 8,
            out_tys: [],
            timeoutAt: undefined,
            waitIfNotPresent: false
            }));
          const {data: [], secs: v364, time: v363, didSend: v155, from: v362 } = txn5;
          ;
          const v366 = stdlib.addressEq(v267, v362);
          stdlib.assert(v366, {
            at: 'reach standard library:209:7:dot',
            fs: ['at ./examples/tut-8/index.rsh:78:53:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
            msg: 'sender correct',
            who: 'Bob'
            });
          ;
          stdlib.protect(ctc1, await interact.informTimeout(), {
            at: './examples/tut-8/index.rsh:41:29:application',
            fs: ['at ./examples/tut-8/index.rsh:40:9:application call to [unknown function] (defined at: ./examples/tut-8/index.rsh:40:27:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./examples/tut-8/index.rsh:39:28:function exp)', 'at ./examples/tut-8/index.rsh:78:53:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
            msg: 'informTimeout',
            who: 'Bob'
            });
          
          return;
          }
        else {
          const {data: [v323], secs: v325, time: v324, didSend: v99, from: v322 } = txn4;
          ;
          const v327 = stdlib.addressEq(v279, v322);
          stdlib.assert(v327, {
            at: './examples/tut-8/index.rsh:77:9:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Bob'
            });
          const v329 = stdlib.add(v324, v269);
          const txn5 = await (ctc.recv({
            didSend: false,
            evt_cnt: 2,
            funcNum: 9,
            out_tys: [ctc0, ctc0],
            timeoutAt: ['time', v329],
            waitIfNotPresent: false
            }));
          if (txn5.didTimeout) {
            const txn6 = await (ctc.sendrecv({
              args: [v267, v268, v269, v279, v292, v311, v323, v329],
              evt_cnt: 0,
              funcNum: 10,
              lct: v324,
              onlyIf: true,
              out_tys: [],
              pay: [stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), []],
              sim_p: (async (txn6) => {
                const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
                
                const {data: [], secs: v347, time: v346, didSend: v129, from: v345 } = txn6;
                
                sim_r.txns.push({
                  amt: stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0),
                  kind: 'to',
                  tok: undefined
                  });
                const v349 = stdlib.addressEq(v279, v345);
                stdlib.assert(v349, {
                  at: 'reach standard library:209:7:dot',
                  fs: ['at ./examples/tut-8/index.rsh:86:53:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
                  msg: 'sender correct',
                  who: 'Bob'
                  });
                sim_r.txns.push({
                  amt: v292,
                  kind: 'from',
                  to: v279,
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
            const {data: [], secs: v347, time: v346, didSend: v129, from: v345 } = txn6;
            ;
            const v349 = stdlib.addressEq(v279, v345);
            stdlib.assert(v349, {
              at: 'reach standard library:209:7:dot',
              fs: ['at ./examples/tut-8/index.rsh:86:53:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
              msg: 'sender correct',
              who: 'Bob'
              });
            ;
            stdlib.protect(ctc1, await interact.informTimeout(), {
              at: './examples/tut-8/index.rsh:41:29:application',
              fs: ['at ./examples/tut-8/index.rsh:40:9:application call to [unknown function] (defined at: ./examples/tut-8/index.rsh:40:27:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./examples/tut-8/index.rsh:39:28:function exp)', 'at ./examples/tut-8/index.rsh:86:53:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
              msg: 'informTimeout',
              who: 'Bob'
              });
            
            return;
            }
          else {
            const {data: [v334, v335], secs: v337, time: v336, didSend: v110, from: v333 } = txn5;
            ;
            const v339 = stdlib.addressEq(v267, v333);
            stdlib.assert(v339, {
              at: './examples/tut-8/index.rsh:85:11:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Bob'
              });
            const v340 = stdlib.digest(ctc3, [v334, v335]);
            const v341 = stdlib.digestEq(v311, v340);
            stdlib.assert(v341, {
              at: 'reach standard library:65:17:application',
              fs: ['at ./examples/tut-8/index.rsh:87:20:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)'],
              msg: null,
              who: 'Bob'
              });
            const v342 = stdlib.sub(stdlib.checkedBigNumberify('./examples/tut-8/index.rsh:7:14:decimal', stdlib.UInt_max, 4), v323);
            const v343 = stdlib.add(v335, v342);
            const v344 = stdlib.mod(v343, stdlib.checkedBigNumberify('./examples/tut-8/index.rsh:7:28:decimal', stdlib.UInt_max, 3));
            const cv284 = v344;
            const cv285 = v336;
            const cv292 = v292;
            
            v284 = cv284;
            v285 = cv285;
            v292 = cv292;
            
            continue;}
          }
        }
      }
    const v396 = stdlib.eq(v284, stdlib.checkedBigNumberify('./examples/tut-8/index.rsh:makeEnum', stdlib.UInt_max, 2));
    const v399 = stdlib.mul(stdlib.checkedBigNumberify('./examples/tut-8/index.rsh:94:12:decimal', stdlib.UInt_max, 2), v268);
    const v401 = v396 ? v267 : v279;
    ;
    stdlib.protect(ctc1, await interact.seeOutcome(v284), {
      at: './examples/tut-8/index.rsh:98:24:application',
      fs: ['at ./examples/tut-8/index.rsh:97:7:application call to [unknown function] (defined at: ./examples/tut-8/index.rsh:97:25:function exp)'],
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
substring 0 8
btoi
store 1
dup
substring 8 16
btoi
store 2
substring 16 48
store 3
txn NumAppArgs
int 3
==
assert
txna ApplicationArgs 0
btoi
// Handler 0
dup
int 0
==
bz l0
pop
// check step
int 0
load 1
==
assert
// check time
txna ApplicationArgs 1
btoi
load 2
==
assert
byte base64()
pop
txna ApplicationArgs 2
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
txn Sender
global CreatorAddress
==
assert
load 255
store 3
// "CheckPay"
// "./examples/tut-8/index.rsh:49:9:dot"
// "[]"
int 100000
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
load 3
dig 1
gtxns Receiver
==
assert
l1:
pop
// "CheckPay"
// "./examples/tut-8/index.rsh:49:9:dot"
// "[]"
load 254
dup
bz l2
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
load 3
dig 1
gtxns Receiver
==
assert
l2:
pop
global Round
load 253
+
store 252
txn Sender
load 254
itob
concat
load 253
itob
concat
load 252
itob
concat
int 1
bzero
dig 1
substring 0 56
app_global_put
pop
int 1
store 1
global Round
store 2
txn OnCompletion
int NoOp
==
assert
b updateState
l0:
// Handler 1
dup
int 1
==
bz l3
pop
// check step
int 1
load 1
==
assert
// check time
txna ApplicationArgs 1
btoi
load 2
==
assert
int 1
bzero
app_global_get
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
global Round
load 252
<
assert
// "CheckPay"
// "./examples/tut-8/index.rsh:56:7:dot"
// "[]"
load 254
dup
bz l4
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
load 3
dig 1
gtxns Receiver
==
assert
l4:
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
b loop3
l3:
// Handler 2
dup
int 2
==
bz l5
pop
// check step
int 1
load 1
==
assert
// check time
txna ApplicationArgs 1
btoi
load 2
==
assert
int 1
bzero
app_global_get
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
global Round
load 252
>=
assert
// "CheckPay"
// "reach standard library:209:7:dot"
// "[at ./examples/tut-8/index.rsh:57:51:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
// Just "sender correct"
// "reach standard library:209:7:dot"
// "[at ./examples/tut-8/index.rsh:57:51:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
load 255
txn Sender
==
assert
load 254
dup
bz l6
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
load 3
dig 1
gtxns Sender
==
assert
load 255
dig 1
gtxns Receiver
==
assert
l6:
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
load 3
dig 1
gtxns Sender
==
assert
global CreatorAddress
dig 1
gtxns CloseRemainderTo
==
assert
l7:
pop
txn OnCompletion
int DeleteApplication
==
assert
b updateState
l5:
l8:
l9:
// Handler 5
dup
int 5
==
bz l10
pop
// check step
int 6
load 1
==
assert
// check time
txna ApplicationArgs 1
btoi
load 2
==
assert
int 1
bzero
app_global_get
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
global Round
load 250
<
assert
// "CheckPay"
// "./examples/tut-8/index.rsh:69:11:dot"
// "[]"
// Just "sender correct"
// "./examples/tut-8/index.rsh:69:11:dot"
// "[]"
load 255
txn Sender
==
assert
global Round
load 253
+
store 248
load 255
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
int 1
bzero
dig 1
substring 0 127
app_global_put
byte base64(AQ==)
dig 1
substring 127 128
app_global_put
pop
int 8
store 1
global Round
store 2
txn OnCompletion
int NoOp
==
assert
b updateState
l10:
// Handler 6
dup
int 6
==
bz l11
pop
// check step
int 6
load 1
==
assert
// check time
txna ApplicationArgs 1
btoi
load 2
==
assert
int 1
bzero
app_global_get
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
global Round
load 250
>=
assert
// "CheckPay"
// "reach standard library:209:7:dot"
// "[at ./examples/tut-8/index.rsh:70:53:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
// Just "sender correct"
// "reach standard library:209:7:dot"
// "[at ./examples/tut-8/index.rsh:70:53:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
load 252
txn Sender
==
assert
load 251
dup
bz l12
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
load 3
dig 1
gtxns Sender
==
assert
load 252
dig 1
gtxns Receiver
==
assert
l12:
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
load 3
dig 1
gtxns Sender
==
assert
global CreatorAddress
dig 1
gtxns CloseRemainderTo
==
assert
l13:
pop
txn OnCompletion
int DeleteApplication
==
assert
b updateState
l11:
// Handler 7
dup
int 7
==
bz l14
pop
// check step
int 8
load 1
==
assert
// check time
txna ApplicationArgs 1
btoi
load 2
==
assert
int 1
bzero
app_global_get
byte base64(AQ==)
app_global_get
concat
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
global Round
load 249
<
assert
// "CheckPay"
// "./examples/tut-8/index.rsh:77:9:dot"
// "[]"
// Just "sender correct"
// "./examples/tut-8/index.rsh:77:9:dot"
// "[]"
load 252
txn Sender
==
assert
global Round
load 253
+
store 247
load 255
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
int 1
bzero
dig 1
substring 0 127
app_global_put
byte base64(AQ==)
dig 1
substring 127 136
app_global_put
pop
int 10
store 1
global Round
store 2
txn OnCompletion
int NoOp
==
assert
b updateState
l14:
// Handler 8
dup
int 8
==
bz l15
pop
// check step
int 8
load 1
==
assert
// check time
txna ApplicationArgs 1
btoi
load 2
==
assert
int 1
bzero
app_global_get
byte base64(AQ==)
app_global_get
concat
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
global Round
load 249
>=
assert
// "CheckPay"
// "reach standard library:209:7:dot"
// "[at ./examples/tut-8/index.rsh:78:53:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
// Just "sender correct"
// "reach standard library:209:7:dot"
// "[at ./examples/tut-8/index.rsh:78:53:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
load 255
txn Sender
==
assert
load 251
dup
bz l16
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
load 3
dig 1
gtxns Sender
==
assert
load 255
dig 1
gtxns Receiver
==
assert
l16:
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
load 3
dig 1
gtxns Sender
==
assert
global CreatorAddress
dig 1
gtxns CloseRemainderTo
==
assert
l17:
pop
txn OnCompletion
int DeleteApplication
==
assert
b updateState
l15:
// Handler 9
dup
int 9
==
bz l18
pop
// check step
int 10
load 1
==
assert
// check time
txna ApplicationArgs 1
btoi
load 2
==
assert
int 1
bzero
app_global_get
byte base64(AQ==)
app_global_get
concat
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
global Round
load 248
<
assert
// "CheckPay"
// "./examples/tut-8/index.rsh:85:11:dot"
// "[]"
// Just "sender correct"
// "./examples/tut-8/index.rsh:85:11:dot"
// "[]"
load 255
txn Sender
==
assert
// Nothing
// "reach standard library:65:17:application"
// "[at ./examples/tut-8/index.rsh:87:20:application call to \"checkCommitment\" (defined at: reach standard library:64:8:function exp)]"
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
b loop3
l18:
// Handler 10
dup
int 10
==
bz l19
pop
// check step
int 10
load 1
==
assert
// check time
txna ApplicationArgs 1
btoi
load 2
==
assert
int 1
bzero
app_global_get
byte base64(AQ==)
app_global_get
concat
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
global Round
load 248
>=
assert
// "CheckPay"
// "reach standard library:209:7:dot"
// "[at ./examples/tut-8/index.rsh:86:53:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
// Just "sender correct"
// "reach standard library:209:7:dot"
// "[at ./examples/tut-8/index.rsh:86:53:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
load 252
txn Sender
==
assert
load 251
dup
bz l20
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
load 3
dig 1
gtxns Sender
==
assert
load 252
dig 1
gtxns Receiver
==
assert
l20:
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
load 3
dig 1
gtxns Sender
==
assert
global CreatorAddress
dig 1
gtxns CloseRemainderTo
==
assert
l21:
pop
txn OnCompletion
int DeleteApplication
==
assert
b updateState
l19:
int 0
assert
loop3:
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
bz l22
load 254
load 250
+
store 248
load 252
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
int 1
bzero
dig 1
substring 0 96
app_global_put
pop
int 6
store 1
global Round
store 2
txn OnCompletion
int NoOp
==
assert
b updateState
l22:
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
loop4:
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
bz l23
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
load 3
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
l23:
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
load 3
dig 1
gtxns Sender
==
assert
global CreatorAddress
dig 1
gtxns CloseRemainderTo
==
assert
l24:
pop
txn OnCompletion
int DeleteApplication
==
assert
updateState:
byte base64()
load 1
itob
load 2
itob
load 3
concat
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
int 0
store 1
int 0
store 2
global ZeroAddress
store 3
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
  stateKeys: 2,
  stateSize: 136,
  unsupported: [],
  version: 4
  };
const _ETH = {
  ABI: `[
  {
    "inputs": [
      {
        "components": [
          {
            "internalType": "uint256",
            "name": "time",
            "type": "uint256"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v268",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v269",
                "type": "uint256"
              }
            ],
            "internalType": "struct T1",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "internalType": "struct T2",
        "name": "_a",
        "type": "tuple"
      }
    ],
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
    "inputs": [
      {
        "components": [
          {
            "internalType": "uint256",
            "name": "time",
            "type": "uint256"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v268",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v269",
                "type": "uint256"
              }
            ],
            "internalType": "struct T1",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct T2",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "e0",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "components": [
          {
            "internalType": "uint256",
            "name": "time",
            "type": "uint256"
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
    "name": "e1",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "components": [
          {
            "internalType": "uint256",
            "name": "time",
            "type": "uint256"
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
    "name": "e10",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "components": [
          {
            "internalType": "uint256",
            "name": "time",
            "type": "uint256"
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
            "internalType": "uint256",
            "name": "time",
            "type": "uint256"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v311",
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
    "name": "e5",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "components": [
          {
            "internalType": "uint256",
            "name": "time",
            "type": "uint256"
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
    "name": "e6",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "components": [
          {
            "internalType": "uint256",
            "name": "time",
            "type": "uint256"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v323",
                "type": "uint256"
              }
            ],
            "internalType": "struct T15",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct T16",
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
            "internalType": "uint256",
            "name": "time",
            "type": "uint256"
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
    "name": "e8",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "components": [
          {
            "internalType": "uint256",
            "name": "time",
            "type": "uint256"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v334",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v335",
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
    "name": "e9",
    "type": "event"
  },
  {
    "inputs": [],
    "name": "_reachCurrentTime",
    "outputs": [
      {
        "internalType": "uint256",
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "components": [
          {
            "internalType": "uint256",
            "name": "time",
            "type": "uint256"
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
            "internalType": "uint256",
            "name": "time",
            "type": "uint256"
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
            "internalType": "uint256",
            "name": "time",
            "type": "uint256"
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
            "internalType": "uint256",
            "name": "time",
            "type": "uint256"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v311",
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
            "internalType": "uint256",
            "name": "time",
            "type": "uint256"
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
            "internalType": "uint256",
            "name": "time",
            "type": "uint256"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v323",
                "type": "uint256"
              }
            ],
            "internalType": "struct T15",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "internalType": "struct T16",
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
            "internalType": "uint256",
            "name": "time",
            "type": "uint256"
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
            "internalType": "uint256",
            "name": "time",
            "type": "uint256"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v334",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v335",
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
  Bytecode: `0x6080604052604051620019b5380380620019b583398101604081905262000026916200020d565b6000808055604080516020810190915290815260408051835181526020808501518051828401520151918101919091527ff6f99043ebaefcd14be52433ca7dc9978aa637aef8ca1601e1816a0abc2f99299060600160405180910390a16020820151516200009890341460076200013d565b6020808301510151620000ac9043620002a4565b81526040805160808082018352600060208084018281528486018381526060808701858152338089528b860180515186525186015184528a5182526001968790554390965588518086019690965292518589015290519084015251828401528451808303909301835260a09091019093528051919262000133926002929091019062000167565b5050505062000308565b81620001635760405163100960cb60e01b81526004810182905260240160405180910390fd5b5050565b8280546200017590620002cb565b90600052602060002090601f016020900481019282620001995760008555620001e4565b82601f10620001b457805160ff1916838001178555620001e4565b82800160010185558215620001e4579182015b82811115620001e4578251825591602001919060010190620001c7565b50620001f2929150620001f6565b5090565b5b80821115620001f25760008155600101620001f7565b600081830360608112156200022157600080fd5b6200022b6200026d565b835181526040601f19830112156200024257600080fd5b6200024c6200026d565b60208581015182526040909501518582015293810193909352509092915050565b604080519081016001600160401b03811182821017156200029e57634e487b7160e01b600052604160045260246000fd5b60405290565b60008219821115620002c657634e487b7160e01b600052601160045260246000fd5b500190565b600181811c90821680620002e057607f821691505b602082108114156200030257634e487b7160e01b600052602260045260246000fd5b50919050565b61169d80620003186000396000f3fe60806040526004361061008a5760003560e01c80637963168e116100595780637963168e146100e457806383230757146100f7578063b1adad0e14610119578063f2c9f6961461012c578063fd948b861461013f57600080fd5b806309fa1e2f14610096578063306ab814146100ab5780634ce4f265146100be5780636d213eec146100d157600080fd5b3661009157005b600080fd5b6100a96100a43660046114dc565b610152565b005b6100a96100b936600461140d565b610372565b6100a96100cc36600461140d565b610503565b6100a96100df36600461140d565b610639565b6100a96100f236600461140d565b6108d5565b34801561010357600080fd5b5060015460405190815260200160405180910390f35b6100a961012736600461140d565b610a5c565b6100a961013a36600461140d565b610bcc565b6100a961014d36600461140d565b610e0b565b610162600a600054146028610f7b565b600154610173908235146029610f7b565b600080805560028054610185906115fa565b80601f01602080910402602001604051908101604052809291908181526020018280546101b1906115fa565b80156101fe5780601f106101d3576101008083540402835291602001916101fe565b820191906000526020600020905b8154815290600101906020018083116101e157829003601f168201915b50505050508060200190518101906102169190611430565b90506102298160e001514310602a610f7b565b604080518335815260208085013590820152838201358183015290517fe6fa34ba311ad7d222e1d3383764f18a0f4e0068349d17d04fc5d04212be1d7f9181900360600190a161027b34156025610f7b565b8051610293906001600160a01b031633146026610f7b565b604080516102df916102b991602080870135928701359101918252602082015260400190565b6040516020818303038152906040528051906020012060001c8260a00151146027610f7b565b6102e7611193565b815181516001600160a01b0391821690526020808401518351909101526040808401518351909101526060808401518351921691015260c08201516003906103309060046115e3565b61033e9060408601356115ac565b610348919061162f565b6020808301805192909252815143910152608083015190516040015261036d81610fa4565b505050565b610382600a60005414602d610f7b565b60015461039390823514602e610f7b565b6000808055600280546103a5906115fa565b80601f01602080910402602001604051908101604052809291908181526020018280546103d1906115fa565b801561041e5780601f106103f35761010080835404028352916020019161041e565b820191906000526020600020905b81548152906001019060200180831161040157829003601f168201915b50505050508060200190518101906104369190611430565b905061044a8160e00151431015602f610f7b565b7fbc00bef455301cf914c30c8a9af2a81c4e58a53a327cc5726ef84b62ea9c1fc9826040516104799190611582565b60405180910390a161048d3415602b610f7b565b60608101516104a8906001600160a01b03163314602c610f7b565b80606001516001600160a01b03166108fc82608001519081150290604051600060405180830381858888f193505050501580156104e9573d6000803e3d6000fd5b5060008080556001819055610500906002906111ec565b33ff5b6105136006600054146018610f7b565b600154610524908235146019610f7b565b600080805560028054610536906115fa565b80601f0160208091040260200160405190810160405280929190818152602001828054610562906115fa565b80156105af5780601f10610584576101008083540402835291602001916105af565b820191906000526020600020905b81548152906001019060200180831161059257829003601f168201915b50505050508060200190518101906105c791906114ee565b90506105db8160a00151431015601a610f7b565b7f2de542e04768f7d432d52df08943d2fab028221c30dba664ab546be3aae5e4a08260405161060a9190611582565b60405180910390a161061e34156016610f7b565b60608101516104a8906001600160a01b031633146017610f7b565b610649600860005414601d610f7b565b60015461065a90823514601e610f7b565b60008080556002805461066c906115fa565b80601f0160208091040260200160405190810160405280929190818152602001828054610698906115fa565b80156106e55780601f106106ba576101008083540402835291602001916106e5565b820191906000526020600020905b8154815290600101906020018083116106c857829003601f168201915b50505050508060200190518101906106fd919061136f565b90506107156040518060200160405280600081525090565b6107268260c001514310601f610f7b565b6040805184358152602080860135908201527f5986e59bba8dd42f2aef79bbc49e99479d332aa932d6435e66d861d23c990e97910160405180910390a161076f3415601b610f7b565b606082015161078a906001600160a01b03163314601c610f7b565b604082015161079990436115ac565b81526040805161010081018252600080825260208201819052918101829052606081018290526080810182905260a0810182905260c0810182905260e081019190915282516001600160a01b0390811682526020808501518184015260408086015181850152606080870151909316928401929092526080808601519084015260a080860151908401528581013560c0840152835160e0840152600a6000554360015590516108aa9183910160006101008201905060018060a01b038084511683526020840151602084015260408401516040840152806060850151166060840152506080830151608083015260a083015160a083015260c083015160c083015260e083015160e083015292915050565b604051602081830303815290604052600290805190602001906108ce929190611229565b5050505050565b6108e56001600054146009610f7b565b6001546108f690823514600a610f7b565b600080805560028054610908906115fa565b80601f0160208091040260200160405190810160405280929190818152602001828054610934906115fa565b80156109815780601f1061095657610100808354040283529160200191610981565b820191906000526020600020905b81548152906001019060200180831161096457829003601f168201915b505050505080602001905181019061099991906112f6565b90506109ac81606001514310600b610f7b565b7f9f41c6cf17ede288cbb2cfbbafdd05b2b2025dea3b047cdb79dbc892d7a9286d826040516109db9190611582565b60405180910390a16109f4816020015134146008610f7b565b6109fc611193565b815181516001600160a01b039091169052602080830180518351830152604080850151845190910152825133606090910152818301805160019052514392019190915251610a4a90806115ac565b60208201516040015261036d81610fa4565b610a6c6008600054146022610f7b565b600154610a7d908235146023610f7b565b600080805560028054610a8f906115fa565b80601f0160208091040260200160405190810160405280929190818152602001828054610abb906115fa565b8015610b085780601f10610add57610100808354040283529160200191610b08565b820191906000526020600020905b815481529060010190602001808311610aeb57829003601f168201915b5050505050806020019051810190610b20919061136f565b9050610b348160c001514310156024610f7b565b7f958f78ebab349905eb0abbf2926ea4aab4a0f19ea393268c746af21c24b4022282604051610b639190611582565b60405180910390a1610b7734156020610f7b565b8051610b8f906001600160a01b031633146021610f7b565b805160808201516040516001600160a01b039092169181156108fc0291906000818181858888f193505050501580156104e9573d6000803e3d6000fd5b610bdc6006600054146013610f7b565b600154610bed908235146014610f7b565b600080805560028054610bff906115fa565b80601f0160208091040260200160405190810160405280929190818152602001828054610c2b906115fa565b8015610c785780601f10610c4d57610100808354040283529160200191610c78565b820191906000526020600020905b815481529060010190602001808311610c5b57829003601f168201915b5050505050806020019051810190610c9091906114ee565b9050610ca86040518060200160405280600081525090565b610cb98260a0015143106015610f7b565b6040805184358152602080860135908201527f3c04125933303f599cc5d20b6f660c4c9857a80c5f4570c2236678d0bd3959e3910160405180910390a1610d0234156011610f7b565b8151610d1a906001600160a01b031633146012610f7b565b6040820151610d2990436115ac565b81526040805160e081018252600080825260208201819052918101829052606081018290526080810182905260a0810182905260c081019190915282516001600160a01b039081168083526020808601518185019081526040808801518187019081526060808a015187168189019081526080808c0151818b019081528d88013560a0808d019182528d5160c0808f0191825260086000554360015589519b8c019c909c529851978a0197909752945193880193909352905190971696850196909652945190830152925191810191909152905160e0820152610100016108aa565b610e1b600160005414600e610f7b565b600154610e2c90823514600f610f7b565b600080805560028054610e3e906115fa565b80601f0160208091040260200160405190810160405280929190818152602001828054610e6a906115fa565b8015610eb75780601f10610e8c57610100808354040283529160200191610eb7565b820191906000526020600020905b815481529060010190602001808311610e9a57829003601f168201915b5050505050806020019051810190610ecf91906112f6565b9050610ee381606001514310156010610f7b565b7fe0777bbb0edbebd8a5c254bf54fd955256e9bf9fb0fe4138cd88ac193a101d1582604051610f129190611582565b60405180910390a1610f263415600c610f7b565b8051610f3e906001600160a01b03163314600d610f7b565b805160208201516040516001600160a01b039092169181156108fc0291906000818181858888f193505050501580156104e9573d6000803e3d6000fd5b81610fa05760405163100960cb60e01b81526004810182905260240160405180910390fd5b5050565b604080516020810190915260008152602082015151600114156110cc578151604001516020808401510151610fd991906115ac565b81526040805160c081018252600080825260208201819052918101829052606081018290526080810182905260a08101919091528251516001600160a01b039081168083528451602090810151818501908152865160409081015181870190815288516060908101518716818901908152858b01518401516080808b019182528b5160a0808d019182526006600055436001558751998a019a909a529651958801959095529251918601919091525190951690830152925191810191909152905160c082015260e001604051602081830303815290604052600290805190602001906110c6929190611229565b50505050565b6040805160c081018252600091810182815260608083018481526080840185815260a085018681528486526020808701979097528851516001600160a01b03908116909552885187015190925287519092015190921690529184015151918290529061036d9082906002146111465780516040015161114a565b8051515b6001600160a01b03166108fc826000015160200151600261116b91906115c4565b6040518115909202916000818181858888f193505050501580156104e9573d6000803e3d6000fd5b6040805160c0810182526000918101828152606082018390526080820183905260a082019290925290819081526020016111e760405180606001604052806000815260200160008152602001600081525090565b905290565b5080546111f8906115fa565b6000825580601f10611208575050565b601f01602090049060005260206000209081019061122691906112ad565b50565b828054611235906115fa565b90600052602060002090601f016020900481019282611257576000855561129d565b82601f1061127057805160ff191683800117855561129d565b8280016001018555821561129d579182015b8281111561129d578251825591602001919060010190611282565b506112a99291506112ad565b5090565b5b808211156112a957600081556001016112ae565b80516001600160a01b03811681146112d957600080fd5b919050565b6000604082840312156112f057600080fd5b50919050565b60006080828403121561130857600080fd5b6040516080810181811067ffffffffffffffff8211171561133957634e487b7160e01b600052604160045260246000fd5b604052611345836112c2565b81526020830151602082015260408301516040820152606083015160608201528091505092915050565b600060e0828403121561138157600080fd5b60405160e0810181811067ffffffffffffffff821117156113b257634e487b7160e01b600052604160045260246000fd5b6040526113be836112c2565b815260208301516020820152604083015160408201526113e0606084016112c2565b60608201526080830151608082015260a083015160a082015260c083015160c08201528091505092915050565b60006040828403121561141f57600080fd5b61142983836112de565b9392505050565b600061010080838503121561144457600080fd5b6040519081019067ffffffffffffffff8211818310171561147557634e487b7160e01b600052604160045260246000fd5b81604052611482846112c2565b815260208401516020820152604084015160408201526114a4606085016112c2565b60608201526080840151608082015260a084015160a082015260c084015160c082015260e084015160e0820152809250505092915050565b6000606082840312156112f057600080fd5b600060c0828403121561150057600080fd5b60405160c0810181811067ffffffffffffffff8211171561153157634e487b7160e01b600052604160045260246000fd5b60405261153d836112c2565b8152602083015160208201526040830151604082015261155f606084016112c2565b60608201526080830151608082015260a083015160a08201528091505092915050565b8135815260408101602083013580151580821461159e57600080fd5b806020850152505092915050565b600082198211156115bf576115bf611651565b500190565b60008160001904831182151516156115de576115de611651565b500290565b6000828210156115f5576115f5611651565b500390565b600181811c9082168061160e57607f821691505b602082108114156112f057634e487b7160e01b600052602260045260246000fd5b60008261164c57634e487b7160e01b600052601260045260246000fd5b500690565b634e487b7160e01b600052601160045260246000fdfea2646970667358221220501eadfa220f17848265fd78ade7f991ea597d14c71791d8c9e3ba95bf84436c64736f6c63430008070033`,
  BytecodeLen: 6581,
  Which: `oD`,
  version: 3,
  views: {
    }
  };

export const _Connectors = {
  ALGO: _ALGO,
  ETH: _ETH
  };

