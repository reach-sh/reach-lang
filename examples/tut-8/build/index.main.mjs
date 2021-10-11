// Automatically generated with Reach 0.1.5
/* eslint-disable */
export const _version = '0.1.5';
export const _backendVersion = 4;

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
export async function Alice(ctcTop, interact) {
  if (typeof(ctcTop) !== 'object' || ctcTop._initialize === undefined) {
    return Promise.reject(new Error(`The backend for Alice expects to receive a contract as its first argument.`));}
  if (typeof(interact) !== 'object') {
    return Promise.reject(new Error(`The backend for Alice expects to receive an interact object as its second argument.`));}
  const ctc = ctcTop._initialize();
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
    lct: stdlib.checkedBigNumberify('./index.rsh:49:9:dot', stdlib.UInt_max, 0),
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
          at: 'reach standard library:206:7:dot',
          fs: ['at ./index.rsh:57:51:application call to "closeTo" (defined at: reach standard library:204:8:function exp)'],
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
      at: 'reach standard library:206:7:dot',
      fs: ['at ./index.rsh:57:51:application call to "closeTo" (defined at: reach standard library:204:8:function exp)'],
      msg: 'sender correct',
      who: 'Alice'
      });
    ;
    stdlib.protect(ctc3, await interact.informTimeout(), {
      at: './index.rsh:41:29:application',
      fs: ['at ./index.rsh:40:9:application call to [unknown function] (defined at: ./index.rsh:40:27:function exp)', 'at reach standard library:209:8:application call to "after" (defined at: ./index.rsh:39:28:function exp)', 'at ./index.rsh:57:51:application call to "closeTo" (defined at: reach standard library:204:8:function exp)'],
      msg: 'informTimeout',
      who: 'Alice'
      });
    
    return;
    }
  else {
    const {data: [], secs: v281, time: v280, didSend: v61, from: v279 } = txn2;
    const v283 = stdlib.add(v268, v268);
    ;
    let v284 = stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1);
    let v285 = v280;
    let v292 = v283;
    
    while ((() => {
      const v300 = stdlib.eq(v284, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
      
      return v300;})()) {
      const v302 = stdlib.add(v285, v269);
      const v306 = stdlib.protect(ctc0, await interact.getHand(), {
        at: './index.rsh:65:42:application',
        fs: ['at ./index.rsh:64:15:application call to [unknown function] (defined at: ./index.rsh:64:19:function exp)'],
        msg: 'getHand',
        who: 'Alice'
        });
      const v307 = stdlib.protect(ctc0, await interact.random(), {
        at: 'reach standard library:57:31:application',
        fs: ['at ./index.rsh:66:56:application call to "makeCommitment" (defined at: reach standard library:56:8:function exp)', 'at ./index.rsh:64:15:application call to [unknown function] (defined at: ./index.rsh:64:19:function exp)'],
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
        pay: [stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), []],
        sim_p: (async (txn3) => {
          const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
          
          const {data: [v311], secs: v313, time: v312, didSend: v88, from: v310 } = txn3;
          
          sim_r.txns.push({
            amt: stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0),
            kind: 'to',
            tok: undefined
            });
          const v315 = stdlib.addressEq(v267, v310);
          stdlib.assert(v315, {
            at: './index.rsh:69:11:dot',
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
          at: 'reach standard library:206:7:dot',
          fs: ['at ./index.rsh:70:53:application call to "closeTo" (defined at: reach standard library:204:8:function exp)'],
          msg: 'sender correct',
          who: 'Alice'
          });
        ;
        stdlib.protect(ctc3, await interact.informTimeout(), {
          at: './index.rsh:41:29:application',
          fs: ['at ./index.rsh:40:9:application call to [unknown function] (defined at: ./index.rsh:40:27:function exp)', 'at reach standard library:209:8:application call to "after" (defined at: ./index.rsh:39:28:function exp)', 'at ./index.rsh:70:53:application call to "closeTo" (defined at: reach standard library:204:8:function exp)'],
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
          at: './index.rsh:69:11:dot',
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
                at: 'reach standard library:206:7:dot',
                fs: ['at ./index.rsh:78:53:application call to "closeTo" (defined at: reach standard library:204:8:function exp)'],
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
            at: 'reach standard library:206:7:dot',
            fs: ['at ./index.rsh:78:53:application call to "closeTo" (defined at: reach standard library:204:8:function exp)'],
            msg: 'sender correct',
            who: 'Alice'
            });
          ;
          stdlib.protect(ctc3, await interact.informTimeout(), {
            at: './index.rsh:41:29:application',
            fs: ['at ./index.rsh:40:9:application call to [unknown function] (defined at: ./index.rsh:40:27:function exp)', 'at reach standard library:209:8:application call to "after" (defined at: ./index.rsh:39:28:function exp)', 'at ./index.rsh:78:53:application call to "closeTo" (defined at: reach standard library:204:8:function exp)'],
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
            at: './index.rsh:77:9:dot',
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
            pay: [stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), []],
            sim_p: (async (txn5) => {
              const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
              
              const {data: [v334, v335], secs: v337, time: v336, didSend: v110, from: v333 } = txn5;
              
              sim_r.txns.push({
                amt: stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0),
                kind: 'to',
                tok: undefined
                });
              const v339 = stdlib.addressEq(v267, v333);
              stdlib.assert(v339, {
                at: './index.rsh:85:11:dot',
                fs: [],
                msg: 'sender correct',
                who: 'Alice'
                });
              const v340 = stdlib.digest(ctc1, [v334, v335]);
              const v341 = stdlib.digestEq(v311, v340);
              stdlib.assert(v341, {
                at: 'reach standard library:62:17:application',
                fs: ['at ./index.rsh:87:20:application call to "checkCommitment" (defined at: reach standard library:61:8:function exp)'],
                msg: null,
                who: 'Alice'
                });
              const v342 = stdlib.sub(stdlib.checkedBigNumberify('./index.rsh:7:14:decimal', stdlib.UInt_max, 4), v323);
              const v343 = stdlib.add(v335, v342);
              const v344 = stdlib.mod(v343, stdlib.checkedBigNumberify('./index.rsh:7:28:decimal', stdlib.UInt_max, 3));
              const cv284 = v344;
              const cv285 = v336;
              const cv292 = v292;
              
              (() => {
                const v284 = cv284;
                const v285 = cv285;
                const v292 = cv292;
                
                if ((() => {
                  const v300 = stdlib.eq(v284, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
                  
                  return v300;})()) {
                  const v302 = stdlib.add(v285, v269);
                  sim_r.isHalt = false;
                  }
                else {
                  const v396 = stdlib.eq(v284, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
                  const v399 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:94:12:decimal', stdlib.UInt_max, 2), v268);
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
              at: 'reach standard library:206:7:dot',
              fs: ['at ./index.rsh:86:53:application call to "closeTo" (defined at: reach standard library:204:8:function exp)'],
              msg: 'sender correct',
              who: 'Alice'
              });
            ;
            stdlib.protect(ctc3, await interact.informTimeout(), {
              at: './index.rsh:41:29:application',
              fs: ['at ./index.rsh:40:9:application call to [unknown function] (defined at: ./index.rsh:40:27:function exp)', 'at reach standard library:209:8:application call to "after" (defined at: ./index.rsh:39:28:function exp)', 'at ./index.rsh:86:53:application call to "closeTo" (defined at: reach standard library:204:8:function exp)'],
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
              at: './index.rsh:85:11:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Alice'
              });
            const v340 = stdlib.digest(ctc1, [v334, v335]);
            const v341 = stdlib.digestEq(v311, v340);
            stdlib.assert(v341, {
              at: 'reach standard library:62:17:application',
              fs: ['at ./index.rsh:87:20:application call to "checkCommitment" (defined at: reach standard library:61:8:function exp)'],
              msg: null,
              who: 'Alice'
              });
            const v342 = stdlib.sub(stdlib.checkedBigNumberify('./index.rsh:7:14:decimal', stdlib.UInt_max, 4), v323);
            const v343 = stdlib.add(v335, v342);
            const v344 = stdlib.mod(v343, stdlib.checkedBigNumberify('./index.rsh:7:28:decimal', stdlib.UInt_max, 3));
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
    const v396 = stdlib.eq(v284, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
    const v399 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:94:12:decimal', stdlib.UInt_max, 2), v268);
    const v401 = v396 ? v267 : v279;
    ;
    stdlib.protect(ctc3, await interact.seeOutcome(v284), {
      at: './index.rsh:98:24:application',
      fs: ['at ./index.rsh:97:7:application call to [unknown function] (defined at: ./index.rsh:97:25:function exp)'],
      msg: 'seeOutcome',
      who: 'Alice'
      });
    
    return;}
  
  
  };
export async function Bob(ctcTop, interact) {
  if (typeof(ctcTop) !== 'object' || ctcTop._initialize === undefined) {
    return Promise.reject(new Error(`The backend for Bob expects to receive a contract as its first argument.`));}
  if (typeof(interact) !== 'object') {
    return Promise.reject(new Error(`The backend for Bob expects to receive an interact object as its second argument.`));}
  const ctc = ctcTop._initialize();
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
    at: './index.rsh:54:25:application',
    fs: ['at ./index.rsh:53:11:application call to [unknown function] (defined at: ./index.rsh:53:15:function exp)'],
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
      const v284 = stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1);
      const v285 = v280;
      const v292 = v283;
      
      if ((() => {
        const v300 = stdlib.eq(v284, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
        
        return v300;})()) {
        const v302 = stdlib.add(v285, v269);
        sim_r.isHalt = false;
        }
      else {
        const v396 = stdlib.eq(v284, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
        const v399 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:94:12:decimal', stdlib.UInt_max, 2), v268);
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
      at: 'reach standard library:206:7:dot',
      fs: ['at ./index.rsh:57:51:application call to "closeTo" (defined at: reach standard library:204:8:function exp)'],
      msg: 'sender correct',
      who: 'Bob'
      });
    ;
    stdlib.protect(ctc1, await interact.informTimeout(), {
      at: './index.rsh:41:29:application',
      fs: ['at ./index.rsh:40:9:application call to [unknown function] (defined at: ./index.rsh:40:27:function exp)', 'at reach standard library:209:8:application call to "after" (defined at: ./index.rsh:39:28:function exp)', 'at ./index.rsh:57:51:application call to "closeTo" (defined at: reach standard library:204:8:function exp)'],
      msg: 'informTimeout',
      who: 'Bob'
      });
    
    return;
    }
  else {
    const {data: [], secs: v281, time: v280, didSend: v61, from: v279 } = txn2;
    const v283 = stdlib.add(v268, v268);
    ;
    let v284 = stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1);
    let v285 = v280;
    let v292 = v283;
    
    while ((() => {
      const v300 = stdlib.eq(v284, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
      
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
              at: 'reach standard library:206:7:dot',
              fs: ['at ./index.rsh:70:53:application call to "closeTo" (defined at: reach standard library:204:8:function exp)'],
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
          at: 'reach standard library:206:7:dot',
          fs: ['at ./index.rsh:70:53:application call to "closeTo" (defined at: reach standard library:204:8:function exp)'],
          msg: 'sender correct',
          who: 'Bob'
          });
        ;
        stdlib.protect(ctc1, await interact.informTimeout(), {
          at: './index.rsh:41:29:application',
          fs: ['at ./index.rsh:40:9:application call to [unknown function] (defined at: ./index.rsh:40:27:function exp)', 'at reach standard library:209:8:application call to "after" (defined at: ./index.rsh:39:28:function exp)', 'at ./index.rsh:70:53:application call to "closeTo" (defined at: reach standard library:204:8:function exp)'],
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
          at: './index.rsh:69:11:dot',
          fs: [],
          msg: 'sender correct',
          who: 'Bob'
          });
        const v317 = stdlib.add(v312, v269);
        const v321 = stdlib.protect(ctc0, await interact.getHand(), {
          at: './index.rsh:75:50:application',
          fs: ['at ./index.rsh:74:13:application call to [unknown function] (defined at: ./index.rsh:74:17:function exp)'],
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
          pay: [stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), []],
          sim_p: (async (txn4) => {
            const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
            
            const {data: [v323], secs: v325, time: v324, didSend: v99, from: v322 } = txn4;
            
            sim_r.txns.push({
              amt: stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0),
              kind: 'to',
              tok: undefined
              });
            const v327 = stdlib.addressEq(v279, v322);
            stdlib.assert(v327, {
              at: './index.rsh:77:9:dot',
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
            at: 'reach standard library:206:7:dot',
            fs: ['at ./index.rsh:78:53:application call to "closeTo" (defined at: reach standard library:204:8:function exp)'],
            msg: 'sender correct',
            who: 'Bob'
            });
          ;
          stdlib.protect(ctc1, await interact.informTimeout(), {
            at: './index.rsh:41:29:application',
            fs: ['at ./index.rsh:40:9:application call to [unknown function] (defined at: ./index.rsh:40:27:function exp)', 'at reach standard library:209:8:application call to "after" (defined at: ./index.rsh:39:28:function exp)', 'at ./index.rsh:78:53:application call to "closeTo" (defined at: reach standard library:204:8:function exp)'],
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
            at: './index.rsh:77:9:dot',
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
                  at: 'reach standard library:206:7:dot',
                  fs: ['at ./index.rsh:86:53:application call to "closeTo" (defined at: reach standard library:204:8:function exp)'],
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
              at: 'reach standard library:206:7:dot',
              fs: ['at ./index.rsh:86:53:application call to "closeTo" (defined at: reach standard library:204:8:function exp)'],
              msg: 'sender correct',
              who: 'Bob'
              });
            ;
            stdlib.protect(ctc1, await interact.informTimeout(), {
              at: './index.rsh:41:29:application',
              fs: ['at ./index.rsh:40:9:application call to [unknown function] (defined at: ./index.rsh:40:27:function exp)', 'at reach standard library:209:8:application call to "after" (defined at: ./index.rsh:39:28:function exp)', 'at ./index.rsh:86:53:application call to "closeTo" (defined at: reach standard library:204:8:function exp)'],
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
              at: './index.rsh:85:11:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Bob'
              });
            const v340 = stdlib.digest(ctc3, [v334, v335]);
            const v341 = stdlib.digestEq(v311, v340);
            stdlib.assert(v341, {
              at: 'reach standard library:62:17:application',
              fs: ['at ./index.rsh:87:20:application call to "checkCommitment" (defined at: reach standard library:61:8:function exp)'],
              msg: null,
              who: 'Bob'
              });
            const v342 = stdlib.sub(stdlib.checkedBigNumberify('./index.rsh:7:14:decimal', stdlib.UInt_max, 4), v323);
            const v343 = stdlib.add(v335, v342);
            const v344 = stdlib.mod(v343, stdlib.checkedBigNumberify('./index.rsh:7:28:decimal', stdlib.UInt_max, 3));
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
    const v396 = stdlib.eq(v284, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
    const v399 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:94:12:decimal', stdlib.UInt_max, 2), v268);
    const v401 = v396 ? v267 : v279;
    ;
    stdlib.protect(ctc1, await interact.seeOutcome(v284), {
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
dup
int 0
==
swap
load 2
==
||
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
// "./index.rsh:49:9:dot"
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
// "./index.rsh:49:9:dot"
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
dup
int 0
==
swap
load 2
==
||
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
// "./index.rsh:56:7:dot"
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
dup
int 0
==
swap
load 2
==
||
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
// "reach standard library:206:7:dot"
// "[at ./index.rsh:57:51:application call to \"closeTo\" (defined at: reach standard library:204:8:function exp)]"
// Just "sender correct"
// "reach standard library:206:7:dot"
// "[at ./index.rsh:57:51:application call to \"closeTo\" (defined at: reach standard library:204:8:function exp)]"
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
dup
int 0
==
swap
load 2
==
||
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
dup
int 0
==
swap
load 2
==
||
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
// "reach standard library:206:7:dot"
// "[at ./index.rsh:70:53:application call to \"closeTo\" (defined at: reach standard library:204:8:function exp)]"
// Just "sender correct"
// "reach standard library:206:7:dot"
// "[at ./index.rsh:70:53:application call to \"closeTo\" (defined at: reach standard library:204:8:function exp)]"
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
dup
int 0
==
swap
load 2
==
||
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
dup
int 0
==
swap
load 2
==
||
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
// "reach standard library:206:7:dot"
// "[at ./index.rsh:78:53:application call to \"closeTo\" (defined at: reach standard library:204:8:function exp)]"
// Just "sender correct"
// "reach standard library:206:7:dot"
// "[at ./index.rsh:78:53:application call to \"closeTo\" (defined at: reach standard library:204:8:function exp)]"
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
dup
int 0
==
swap
load 2
==
||
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
// "reach standard library:62:17:application"
// "[at ./index.rsh:87:20:application call to \"checkCommitment\" (defined at: reach standard library:61:8:function exp)]"
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
dup
int 0
==
swap
load 2
==
||
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
// "reach standard library:206:7:dot"
// "[at ./index.rsh:86:53:application call to \"closeTo\" (defined at: reach standard library:204:8:function exp)]"
// Just "sender correct"
// "reach standard library:206:7:dot"
// "[at ./index.rsh:86:53:application call to \"closeTo\" (defined at: reach standard library:204:8:function exp)]"
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
  Bytecode: `0x6080604052604051620019fd380380620019fd833981016040819052620000269162000244565b6000808055604080516020810190915290815260408051835181526020808501518051828401520151918101919091527ff6f99043ebaefcd14be52433ca7dc9978aa637aef8ca1601e1816a0abc2f99299060600160405180910390a16020820151516200009890341460076200013d565b6020808301510151620000ac9043620002a4565b81526040805160808082018352600060208084018281528486018381526060808701858152338089528b860180515186525186015184528a5182526001968790554390965588518086019690965292518589015290519084015251828401528451808303909301835260a09091019093528051919262000133926002929091019062000167565b5050505062000308565b81620001635760405163100960cb60e01b81526004810182905260240160405180910390fd5b5050565b8280546200017590620002cb565b90600052602060002090601f016020900481019282620001995760008555620001e4565b82601f10620001b457805160ff1916838001178555620001e4565b82800160010185558215620001e4579182015b82811115620001e4578251825591602001919060010190620001c7565b50620001f2929150620001f6565b5090565b5b80821115620001f25760008155600101620001f7565b604080519081016001600160401b03811182821017156200023e57634e487b7160e01b600052604160045260246000fd5b60405290565b600081830360608112156200025857600080fd5b620002626200020d565b835181526040601f19830112156200027957600080fd5b620002836200020d565b60208581015182526040909501518582015293810193909352509092915050565b60008219821115620002c657634e487b7160e01b600052601160045260246000fd5b500190565b600181811c90821680620002e057607f821691505b602082108114156200030257634e487b7160e01b600052602260045260246000fd5b50919050565b6116e580620003186000396000f3fe60806040526004361061008a5760003560e01c80637963168e116100595780637963168e146100e457806383230757146100f7578063b1adad0e14610119578063f2c9f6961461012c578063fd948b861461013f57600080fd5b806309fa1e2f14610096578063306ab814146100ab5780634ce4f265146100be5780636d213eec146100d157600080fd5b3661009157005b600080fd5b6100a96100a436600461130a565b610152565b005b6100a96100b9366004611334565b61037b565b6100a96100cc366004611334565b610515565b6100a96100df366004611334565b610654565b6100a96100f2366004611334565b6108f9565b34801561010357600080fd5b5060015460405190815260200160405180910390f35b6100a9610127366004611334565b610a89565b6100a961013a366004611334565b610c02565b6100a961014d366004611334565b610e4a565b610162600a600054146028610fc3565b61017c8135158061017557506001548235145b6029610fc3565b60008080556002805461018e90611357565b80601f01602080910402602001604051908101604052809291908181526020018280546101ba90611357565b80156102075780601f106101dc57610100808354040283529160200191610207565b820191906000526020600020905b8154815290600101906020018083116101ea57829003601f168201915b505050505080602001905181019061021f91906113a8565b90506102328160e001514310602a610fc3565b604080518335815260208085013590820152838201358183015290517fe6fa34ba311ad7d222e1d3383764f18a0f4e0068349d17d04fc5d04212be1d7f9181900360600190a161028434156025610fc3565b805161029c906001600160a01b031633146026610fc3565b604080516102e8916102c291602080870135928701359101918252602082015260400190565b6040516020818303038152906040528051906020012060001c8260a00151146027610fc3565b6102f06111db565b815181516001600160a01b0391821690526020808401518351909101526040808401518351909101526060808401518351921691015260c082015160039061033990600461146a565b610347906040860135611481565b6103519190611499565b6020808301805192909252815143910152608083015190516040015261037681610fec565b505050565b61038b600a60005414602d610fc3565b6103a58135158061039e57506001548235145b602e610fc3565b6000808055600280546103b790611357565b80601f01602080910402602001604051908101604052809291908181526020018280546103e390611357565b80156104305780601f1061040557610100808354040283529160200191610430565b820191906000526020600020905b81548152906001019060200180831161041357829003601f168201915b505050505080602001905181019061044891906113a8565b905061045c8160e00151431015602f610fc3565b7fbc00bef455301cf914c30c8a9af2a81c4e58a53a327cc5726ef84b62ea9c1fc98260405161048b91906114bb565b60405180910390a161049f3415602b610fc3565b60608101516104ba906001600160a01b03163314602c610fc3565b80606001516001600160a01b03166108fc82608001519081150290604051600060405180830381858888f193505050501580156104fb573d6000803e3d6000fd5b506000808055600181905561051290600290611234565b33ff5b6105256006600054146018610fc3565b61053f8135158061053857506001548235145b6019610fc3565b60008080556002805461055190611357565b80601f016020809104026020016040519081016040528092919081815260200182805461057d90611357565b80156105ca5780601f1061059f576101008083540402835291602001916105ca565b820191906000526020600020905b8154815290600101906020018083116105ad57829003601f168201915b50505050508060200190518101906105e291906114e5565b90506105f68160a00151431015601a610fc3565b7f2de542e04768f7d432d52df08943d2fab028221c30dba664ab546be3aae5e4a08260405161062591906114bb565b60405180910390a161063934156016610fc3565b60608101516104ba906001600160a01b031633146017610fc3565b610664600860005414601d610fc3565b61067e8135158061067757506001548235145b601e610fc3565b60008080556002805461069090611357565b80601f01602080910402602001604051908101604052809291908181526020018280546106bc90611357565b80156107095780601f106106de57610100808354040283529160200191610709565b820191906000526020600020905b8154815290600101906020018083116106ec57829003601f168201915b50505050508060200190518101906107219190611579565b90506107396040518060200160405280600081525090565b61074a8260c001514310601f610fc3565b6040805184358152602080860135908201527f5986e59bba8dd42f2aef79bbc49e99479d332aa932d6435e66d861d23c990e97910160405180910390a16107933415601b610fc3565b60608201516107ae906001600160a01b03163314601c610fc3565b60408201516107bd9043611481565b81526040805161010081018252600080825260208201819052918101829052606081018290526080810182905260a0810182905260c0810182905260e081019190915282516001600160a01b0390811682526020808501518184015260408086015181850152606080870151909316928401929092526080808601519084015260a080860151908401528581013560c0840152835160e0840152600a6000554360015590516108ce9183910160006101008201905060018060a01b038084511683526020840151602084015260408401516040840152806060850151166060840152506080830151608083015260a083015160a083015260c083015160c083015260e083015160e083015292915050565b604051602081830303815290604052600290805190602001906108f2929190611271565b5050505050565b6109096001600054146009610fc3565b6109238135158061091c57506001548235145b600a610fc3565b60008080556002805461093590611357565b80601f016020809104026020016040519081016040528092919081815260200182805461096190611357565b80156109ae5780601f10610983576101008083540402835291602001916109ae565b820191906000526020600020905b81548152906001019060200180831161099157829003601f168201915b50505050508060200190518101906109c69190611617565b90506109d981606001514310600b610fc3565b7f9f41c6cf17ede288cbb2cfbbafdd05b2b2025dea3b047cdb79dbc892d7a9286d82604051610a0891906114bb565b60405180910390a1610a21816020015134146008610fc3565b610a296111db565b815181516001600160a01b039091169052602080830180518351830152604080850151845190910152825133606090910152818301805160019052514392019190915251610a779080611481565b60208201516040015261037681610fec565b610a996008600054146022610fc3565b610ab381351580610aac57506001548235145b6023610fc3565b600080805560028054610ac590611357565b80601f0160208091040260200160405190810160405280929190818152602001828054610af190611357565b8015610b3e5780601f10610b1357610100808354040283529160200191610b3e565b820191906000526020600020905b815481529060010190602001808311610b2157829003601f168201915b5050505050806020019051810190610b569190611579565b9050610b6a8160c001514310156024610fc3565b7f958f78ebab349905eb0abbf2926ea4aab4a0f19ea393268c746af21c24b4022282604051610b9991906114bb565b60405180910390a1610bad34156020610fc3565b8051610bc5906001600160a01b031633146021610fc3565b805160808201516040516001600160a01b039092169181156108fc0291906000818181858888f193505050501580156104fb573d6000803e3d6000fd5b610c126006600054146013610fc3565b610c2c81351580610c2557506001548235145b6014610fc3565b600080805560028054610c3e90611357565b80601f0160208091040260200160405190810160405280929190818152602001828054610c6a90611357565b8015610cb75780601f10610c8c57610100808354040283529160200191610cb7565b820191906000526020600020905b815481529060010190602001808311610c9a57829003601f168201915b5050505050806020019051810190610ccf91906114e5565b9050610ce76040518060200160405280600081525090565b610cf88260a0015143106015610fc3565b6040805184358152602080860135908201527f3c04125933303f599cc5d20b6f660c4c9857a80c5f4570c2236678d0bd3959e3910160405180910390a1610d4134156011610fc3565b8151610d59906001600160a01b031633146012610fc3565b6040820151610d689043611481565b81526040805160e081018252600080825260208201819052918101829052606081018290526080810182905260a0810182905260c081019190915282516001600160a01b039081168083526020808601518185019081526040808801518187019081526060808a015187168189019081526080808c0151818b019081528d88013560a0808d019182528d5160c0808f0191825260086000554360015589519b8c019c909c529851978a0197909752945193880193909352905190971696850196909652945190830152925191810191909152905160e0820152610100016108ce565b610e5a600160005414600e610fc3565b610e7481351580610e6d57506001548235145b600f610fc3565b600080805560028054610e8690611357565b80601f0160208091040260200160405190810160405280929190818152602001828054610eb290611357565b8015610eff5780601f10610ed457610100808354040283529160200191610eff565b820191906000526020600020905b815481529060010190602001808311610ee257829003601f168201915b5050505050806020019051810190610f179190611617565b9050610f2b81606001514310156010610fc3565b7fe0777bbb0edbebd8a5c254bf54fd955256e9bf9fb0fe4138cd88ac193a101d1582604051610f5a91906114bb565b60405180910390a1610f6e3415600c610fc3565b8051610f86906001600160a01b03163314600d610fc3565b805160208201516040516001600160a01b039092169181156108fc0291906000818181858888f193505050501580156104fb573d6000803e3d6000fd5b81610fe85760405163100960cb60e01b81526004810182905260240160405180910390fd5b5050565b604080516020810190915260008152602082015151600114156111145781516040015160208084015101516110219190611481565b81526040805160c081018252600080825260208201819052918101829052606081018290526080810182905260a08101919091528251516001600160a01b039081168083528451602090810151818501908152865160409081015181870190815288516060908101518716818901908152858b01518401516080808b019182528b5160a0808d019182526006600055436001558751998a019a909a529651958801959095529251918601919091525190951690830152925191810191909152905160c082015260e0016040516020818303038152906040526002908051906020019061110e929190611271565b50505050565b6040805160c081018252600091810182815260608083018481526080840185815260a085018681528486526020808701979097528851516001600160a01b03908116909552885187015190925287519092015190921690529184015151918290529061037690829060021461118e57805160400151611192565b8051515b6001600160a01b03166108fc82600001516020015160026111b39190611690565b6040518115909202916000818181858888f193505050501580156104fb573d6000803e3d6000fd5b6040805160c0810182526000918101828152606082018390526080820183905260a0820192909252908190815260200161122f60405180606001604052806000815260200160008152602001600081525090565b905290565b50805461124090611357565b6000825580601f10611250575050565b601f01602090049060005260206000209081019061126e91906112f5565b50565b82805461127d90611357565b90600052602060002090601f01602090048101928261129f57600085556112e5565b82601f106112b857805160ff19168380011785556112e5565b828001600101855582156112e5579182015b828111156112e55782518255916020019190600101906112ca565b506112f19291506112f5565b5090565b5b808211156112f157600081556001016112f6565b60006060828403121561131c57600080fd5b50919050565b60006040828403121561131c57600080fd5b60006040828403121561134657600080fd5b6113508383611322565b9392505050565b600181811c9082168061136b57607f821691505b6020821081141561131c57634e487b7160e01b600052602260045260246000fd5b80516001600160a01b03811681146113a357600080fd5b919050565b60006101008083850312156113bc57600080fd5b6040519081019067ffffffffffffffff821181831017156113ed57634e487b7160e01b600052604160045260246000fd5b816040526113fa8461138c565b8152602084015160208201526040840151604082015261141c6060850161138c565b60608201526080840151608082015260a084015160a082015260c084015160c082015260e084015160e0820152809250505092915050565b634e487b7160e01b600052601160045260246000fd5b60008282101561147c5761147c611454565b500390565b6000821982111561149457611494611454565b500190565b6000826114b657634e487b7160e01b600052601260045260246000fd5b500690565b813581526040810160208301358015158082146114d757600080fd5b806020850152505092915050565b600060c082840312156114f757600080fd5b60405160c0810181811067ffffffffffffffff8211171561152857634e487b7160e01b600052604160045260246000fd5b6040526115348361138c565b815260208301516020820152604083015160408201526115566060840161138c565b60608201526080830151608082015260a083015160a08201528091505092915050565b600060e0828403121561158b57600080fd5b60405160e0810181811067ffffffffffffffff821117156115bc57634e487b7160e01b600052604160045260246000fd5b6040526115c88361138c565b815260208301516020820152604083015160408201526115ea6060840161138c565b60608201526080830151608082015260a083015160a082015260c083015160c08201528091505092915050565b60006080828403121561162957600080fd5b6040516080810181811067ffffffffffffffff8211171561165a57634e487b7160e01b600052604160045260246000fd5b6040526116668361138c565b81526020830151602082015260408301516040820152606083015160608201528091505092915050565b60008160001904831182151516156116aa576116aa611454565b50029056fea26469706673582212200c93681197a73006cff6789132844dd03686327f8fcd3cb827f6c24ffcb0c8d664736f6c63430008090033`,
  BytecodeLen: 6653,
  Which: `oD`,
  version: 3,
  views: {
    }
  };
export const _Connectors = {
  ALGO: _ALGO,
  ETH: _ETH
  };
export const _Participants = {
  "Alice": Alice,
  "Bob": Bob
  };
export const _APIs = {
  };
