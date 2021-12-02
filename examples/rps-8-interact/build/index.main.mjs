// Automatically generated with Reach 0.1.7
/* eslint-disable */
export const _version = '0.1.7';
export const _versionHash = '0.1.7';
export const _backendVersion = 6;

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
      5: [ctc0, ctc1, ctc1, ctc0, ctc1, ctc1],
      7: [ctc0, ctc1, ctc1, ctc0, ctc1, ctc2, ctc1],
      9: [ctc0, ctc1, ctc1, ctc0, ctc1, ctc2, ctc1, ctc1]
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
  
  
  const v262 = stdlib.protect(ctc0, interact.deadline, 'for Alice\'s interact field deadline');
  const v263 = stdlib.protect(ctc0, interact.wager, 'for Alice\'s interact field wager');
  
  const txn1 = await (ctc.sendrecv({
    args: [v263, v262],
    evt_cnt: 2,
    funcNum: 0,
    lct: stdlib.checkedBigNumberify('./index.rsh:49:9:dot', stdlib.UInt_max, 0),
    onlyIf: true,
    out_tys: [ctc0, ctc0],
    pay: [v263, []],
    sim_p: (async (txn1) => {
      const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
      
      const {data: [v267, v268], secs: v270, time: v269, didSend: v52, from: v266 } = txn1;
      
      sim_r.txns.push({
        amt: v267,
        kind: 'to',
        tok: undefined
        });
      const v274 = stdlib.add(v269, v268);
      sim_r.isHalt = false;
      
      return sim_r;
      }),
    soloSend: true,
    timeoutAt: undefined,
    tys: [ctc0, ctc0],
    waitIfNotPresent: false
    }));
  const {data: [v267, v268], secs: v270, time: v269, didSend: v52, from: v266 } = txn1;
  ;
  const v274 = stdlib.add(v269, v268);
  const txn2 = await (ctc.recv({
    didSend: false,
    evt_cnt: 0,
    funcNum: 1,
    out_tys: [],
    timeoutAt: ['time', v274],
    waitIfNotPresent: false
    }));
  if (txn2.didTimeout) {
    const txn3 = await (ctc.sendrecv({
      args: [v266, v267, v268, v274],
      evt_cnt: 0,
      funcNum: 2,
      lct: v269,
      onlyIf: true,
      out_tys: [],
      pay: [stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), []],
      sim_p: (async (txn3) => {
        const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
        
        const {data: [], secs: v412, time: v411, didSend: v223, from: v410 } = txn3;
        
        sim_r.txns.push({
          amt: stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0),
          kind: 'to',
          tok: undefined
          });
        const v414 = stdlib.addressEq(v266, v410);
        stdlib.assert(v414, {
          at: 'reach standard library:206:7:dot',
          fs: ['at ./index.rsh:57:51:application call to "closeTo" (defined at: reach standard library:204:8:function exp)'],
          msg: 'sender correct',
          who: 'Alice'
          });
        sim_r.txns.push({
          amt: v267,
          kind: 'from',
          to: v266,
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
    const {data: [], secs: v412, time: v411, didSend: v223, from: v410 } = txn3;
    ;
    const v414 = stdlib.addressEq(v266, v410);
    stdlib.assert(v414, {
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
    const {data: [], secs: v280, time: v279, didSend: v61, from: v278 } = txn2;
    const v282 = stdlib.add(v267, v267);
    ;
    let v283 = stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1);
    let v284 = v279;
    let v290 = v282;
    
    while (await (async () => {
      const v298 = stdlib.eq(v283, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
      
      return v298;})()) {
      const v300 = stdlib.add(v284, v268);
      const v304 = stdlib.protect(ctc0, await interact.getHand(), {
        at: './index.rsh:65:42:application',
        fs: ['at ./index.rsh:64:15:application call to [unknown function] (defined at: ./index.rsh:64:19:function exp)'],
        msg: 'getHand',
        who: 'Alice'
        });
      const v305 = stdlib.protect(ctc0, await interact.random(), {
        at: 'reach standard library:57:31:application',
        fs: ['at ./index.rsh:66:56:application call to "makeCommitment" (defined at: reach standard library:56:8:function exp)', 'at ./index.rsh:64:15:application call to [unknown function] (defined at: ./index.rsh:64:19:function exp)'],
        msg: 'random',
        who: 'Alice'
        });
      const v306 = stdlib.digest(ctc1, [v305, v304]);
      
      const txn3 = await (ctc.sendrecv({
        args: [v266, v267, v268, v278, v290, v300, v306],
        evt_cnt: 1,
        funcNum: 4,
        lct: v284,
        onlyIf: true,
        out_tys: [ctc2],
        pay: [stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), []],
        sim_p: (async (txn3) => {
          const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
          
          const {data: [v309], secs: v311, time: v310, didSend: v88, from: v308 } = txn3;
          
          sim_r.txns.push({
            amt: stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0),
            kind: 'to',
            tok: undefined
            });
          const v313 = stdlib.addressEq(v266, v308);
          stdlib.assert(v313, {
            at: './index.rsh:69:11:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Alice'
            });
          const v315 = stdlib.add(v310, v268);
          sim_r.isHalt = false;
          
          return sim_r;
          }),
        soloSend: true,
        timeoutAt: ['time', v300],
        tys: [ctc4, ctc0, ctc0, ctc4, ctc0, ctc0, ctc2],
        waitIfNotPresent: false
        }));
      if (txn3.didTimeout) {
        const txn4 = await (ctc.recv({
          didSend: false,
          evt_cnt: 0,
          funcNum: 5,
          out_tys: [],
          timeoutAt: undefined,
          waitIfNotPresent: false
          }));
        const {data: [], secs: v379, time: v378, didSend: v181, from: v377 } = txn4;
        ;
        const v381 = stdlib.addressEq(v278, v377);
        stdlib.assert(v381, {
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
        const {data: [v309], secs: v311, time: v310, didSend: v88, from: v308 } = txn3;
        ;
        const v313 = stdlib.addressEq(v266, v308);
        stdlib.assert(v313, {
          at: './index.rsh:69:11:dot',
          fs: [],
          msg: 'sender correct',
          who: 'Alice'
          });
        const v315 = stdlib.add(v310, v268);
        const txn4 = await (ctc.recv({
          didSend: false,
          evt_cnt: 1,
          funcNum: 6,
          out_tys: [ctc0],
          timeoutAt: ['time', v315],
          waitIfNotPresent: false
          }));
        if (txn4.didTimeout) {
          const txn5 = await (ctc.sendrecv({
            args: [v266, v267, v268, v278, v290, v309, v315],
            evt_cnt: 0,
            funcNum: 7,
            lct: v310,
            onlyIf: true,
            out_tys: [],
            pay: [stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), []],
            sim_p: (async (txn5) => {
              const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
              
              const {data: [], secs: v362, time: v361, didSend: v155, from: v360 } = txn5;
              
              sim_r.txns.push({
                amt: stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0),
                kind: 'to',
                tok: undefined
                });
              const v364 = stdlib.addressEq(v266, v360);
              stdlib.assert(v364, {
                at: 'reach standard library:206:7:dot',
                fs: ['at ./index.rsh:78:53:application call to "closeTo" (defined at: reach standard library:204:8:function exp)'],
                msg: 'sender correct',
                who: 'Alice'
                });
              sim_r.txns.push({
                amt: v290,
                kind: 'from',
                to: v266,
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
          const {data: [], secs: v362, time: v361, didSend: v155, from: v360 } = txn5;
          ;
          const v364 = stdlib.addressEq(v266, v360);
          stdlib.assert(v364, {
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
          const {data: [v321], secs: v323, time: v322, didSend: v99, from: v320 } = txn4;
          ;
          const v325 = stdlib.addressEq(v278, v320);
          stdlib.assert(v325, {
            at: './index.rsh:77:9:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Alice'
            });
          const v327 = stdlib.add(v322, v268);
          const txn5 = await (ctc.sendrecv({
            args: [v266, v267, v268, v278, v290, v309, v321, v327, v305, v304],
            evt_cnt: 2,
            funcNum: 8,
            lct: v322,
            onlyIf: true,
            out_tys: [ctc0, ctc0],
            pay: [stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), []],
            sim_p: (async (txn5) => {
              const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
              
              const {data: [v332, v333], secs: v335, time: v334, didSend: v110, from: v331 } = txn5;
              
              sim_r.txns.push({
                amt: stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0),
                kind: 'to',
                tok: undefined
                });
              const v337 = stdlib.addressEq(v266, v331);
              stdlib.assert(v337, {
                at: './index.rsh:85:11:dot',
                fs: [],
                msg: 'sender correct',
                who: 'Alice'
                });
              const v338 = stdlib.digest(ctc1, [v332, v333]);
              const v339 = stdlib.digestEq(v309, v338);
              stdlib.assert(v339, {
                at: 'reach standard library:62:17:application',
                fs: ['at ./index.rsh:87:20:application call to "checkCommitment" (defined at: reach standard library:61:8:function exp)'],
                msg: null,
                who: 'Alice'
                });
              const v340 = stdlib.sub(stdlib.checkedBigNumberify('./index.rsh:7:18:decimal', stdlib.UInt_max, 4), v321);
              const v341 = stdlib.add(v333, v340);
              const v342 = stdlib.mod(v341, stdlib.checkedBigNumberify('./index.rsh:7:34:decimal', stdlib.UInt_max, 3));
              const cv283 = v342;
              const cv284 = v334;
              const cv290 = v290;
              
              await (async () => {
                const v283 = cv283;
                const v284 = cv284;
                const v290 = cv290;
                
                if (await (async () => {
                  const v298 = stdlib.eq(v283, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
                  
                  return v298;})()) {
                  const v300 = stdlib.add(v284, v268);
                  sim_r.isHalt = false;
                  }
                else {
                  const v394 = stdlib.eq(v283, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
                  const v397 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:94:12:decimal', stdlib.UInt_max, 2), v267);
                  const v399 = v394 ? v266 : v278;
                  sim_r.txns.push({
                    amt: v397,
                    kind: 'from',
                    to: v399,
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
            timeoutAt: ['time', v327],
            tys: [ctc4, ctc0, ctc0, ctc4, ctc0, ctc2, ctc0, ctc0, ctc0, ctc0],
            waitIfNotPresent: false
            }));
          if (txn5.didTimeout) {
            const txn6 = await (ctc.recv({
              didSend: false,
              evt_cnt: 0,
              funcNum: 9,
              out_tys: [],
              timeoutAt: undefined,
              waitIfNotPresent: false
              }));
            const {data: [], secs: v345, time: v344, didSend: v129, from: v343 } = txn6;
            ;
            const v347 = stdlib.addressEq(v278, v343);
            stdlib.assert(v347, {
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
            const {data: [v332, v333], secs: v335, time: v334, didSend: v110, from: v331 } = txn5;
            ;
            const v337 = stdlib.addressEq(v266, v331);
            stdlib.assert(v337, {
              at: './index.rsh:85:11:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Alice'
              });
            const v338 = stdlib.digest(ctc1, [v332, v333]);
            const v339 = stdlib.digestEq(v309, v338);
            stdlib.assert(v339, {
              at: 'reach standard library:62:17:application',
              fs: ['at ./index.rsh:87:20:application call to "checkCommitment" (defined at: reach standard library:61:8:function exp)'],
              msg: null,
              who: 'Alice'
              });
            const v340 = stdlib.sub(stdlib.checkedBigNumberify('./index.rsh:7:18:decimal', stdlib.UInt_max, 4), v321);
            const v341 = stdlib.add(v333, v340);
            const v342 = stdlib.mod(v341, stdlib.checkedBigNumberify('./index.rsh:7:34:decimal', stdlib.UInt_max, 3));
            const cv283 = v342;
            const cv284 = v334;
            const cv290 = v290;
            
            v283 = cv283;
            v284 = cv284;
            v290 = cv290;
            
            continue;}
          
          }
        
        }
      
      }
    const v394 = stdlib.eq(v283, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
    const v397 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:94:12:decimal', stdlib.UInt_max, 2), v267);
    const v399 = v394 ? v266 : v278;
    ;
    stdlib.protect(ctc3, await interact.seeOutcome(v283), {
      at: './index.rsh:98:24:application',
      fs: ['at ./index.rsh:97:7:application call to [unknown function] (defined at: ./index.rsh:97:25:function exp)'],
      msg: 'seeOutcome',
      who: 'Alice'
      });
    
    return;
    }
  
  
  
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
  const {data: [v267, v268], secs: v270, time: v269, didSend: v52, from: v266 } = txn1;
  ;
  const v274 = stdlib.add(v269, v268);
  stdlib.protect(ctc1, await interact.acceptWager(v267), {
    at: './index.rsh:54:25:application',
    fs: ['at ./index.rsh:53:11:application call to [unknown function] (defined at: ./index.rsh:53:15:function exp)'],
    msg: 'acceptWager',
    who: 'Bob'
    });
  
  const txn2 = await (ctc.sendrecv({
    args: [v266, v267, v268, v274],
    evt_cnt: 0,
    funcNum: 1,
    lct: v269,
    onlyIf: true,
    out_tys: [],
    pay: [v267, []],
    sim_p: (async (txn2) => {
      const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
      
      const {data: [], secs: v280, time: v279, didSend: v61, from: v278 } = txn2;
      
      const v282 = stdlib.add(v267, v267);
      sim_r.txns.push({
        amt: v267,
        kind: 'to',
        tok: undefined
        });
      const v283 = stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1);
      const v284 = v279;
      const v290 = v282;
      
      if (await (async () => {
        const v298 = stdlib.eq(v283, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
        
        return v298;})()) {
        const v300 = stdlib.add(v284, v268);
        sim_r.isHalt = false;
        }
      else {
        const v394 = stdlib.eq(v283, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
        const v397 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:94:12:decimal', stdlib.UInt_max, 2), v267);
        const v399 = v394 ? v266 : v278;
        sim_r.txns.push({
          amt: v397,
          kind: 'from',
          to: v399,
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
    timeoutAt: ['time', v274],
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
    const {data: [], secs: v412, time: v411, didSend: v223, from: v410 } = txn3;
    ;
    const v414 = stdlib.addressEq(v266, v410);
    stdlib.assert(v414, {
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
    const {data: [], secs: v280, time: v279, didSend: v61, from: v278 } = txn2;
    const v282 = stdlib.add(v267, v267);
    ;
    let v283 = stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1);
    let v284 = v279;
    let v290 = v282;
    
    while (await (async () => {
      const v298 = stdlib.eq(v283, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
      
      return v298;})()) {
      const v300 = stdlib.add(v284, v268);
      const txn3 = await (ctc.recv({
        didSend: false,
        evt_cnt: 1,
        funcNum: 4,
        out_tys: [ctc2],
        timeoutAt: ['time', v300],
        waitIfNotPresent: false
        }));
      if (txn3.didTimeout) {
        const txn4 = await (ctc.sendrecv({
          args: [v266, v267, v268, v278, v290, v300],
          evt_cnt: 0,
          funcNum: 5,
          lct: v284,
          onlyIf: true,
          out_tys: [],
          pay: [stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), []],
          sim_p: (async (txn4) => {
            const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
            
            const {data: [], secs: v379, time: v378, didSend: v181, from: v377 } = txn4;
            
            sim_r.txns.push({
              amt: stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0),
              kind: 'to',
              tok: undefined
              });
            const v381 = stdlib.addressEq(v278, v377);
            stdlib.assert(v381, {
              at: 'reach standard library:206:7:dot',
              fs: ['at ./index.rsh:70:53:application call to "closeTo" (defined at: reach standard library:204:8:function exp)'],
              msg: 'sender correct',
              who: 'Bob'
              });
            sim_r.txns.push({
              amt: v290,
              kind: 'from',
              to: v278,
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
        const {data: [], secs: v379, time: v378, didSend: v181, from: v377 } = txn4;
        ;
        const v381 = stdlib.addressEq(v278, v377);
        stdlib.assert(v381, {
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
        const {data: [v309], secs: v311, time: v310, didSend: v88, from: v308 } = txn3;
        ;
        const v313 = stdlib.addressEq(v266, v308);
        stdlib.assert(v313, {
          at: './index.rsh:69:11:dot',
          fs: [],
          msg: 'sender correct',
          who: 'Bob'
          });
        const v315 = stdlib.add(v310, v268);
        const v319 = stdlib.protect(ctc0, await interact.getHand(), {
          at: './index.rsh:75:50:application',
          fs: ['at ./index.rsh:74:13:application call to [unknown function] (defined at: ./index.rsh:74:17:function exp)'],
          msg: 'getHand',
          who: 'Bob'
          });
        
        const txn4 = await (ctc.sendrecv({
          args: [v266, v267, v268, v278, v290, v309, v315, v319],
          evt_cnt: 1,
          funcNum: 6,
          lct: v310,
          onlyIf: true,
          out_tys: [ctc0],
          pay: [stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), []],
          sim_p: (async (txn4) => {
            const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
            
            const {data: [v321], secs: v323, time: v322, didSend: v99, from: v320 } = txn4;
            
            sim_r.txns.push({
              amt: stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0),
              kind: 'to',
              tok: undefined
              });
            const v325 = stdlib.addressEq(v278, v320);
            stdlib.assert(v325, {
              at: './index.rsh:77:9:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Bob'
              });
            const v327 = stdlib.add(v322, v268);
            sim_r.isHalt = false;
            
            return sim_r;
            }),
          soloSend: true,
          timeoutAt: ['time', v315],
          tys: [ctc4, ctc0, ctc0, ctc4, ctc0, ctc2, ctc0, ctc0],
          waitIfNotPresent: false
          }));
        if (txn4.didTimeout) {
          const txn5 = await (ctc.recv({
            didSend: false,
            evt_cnt: 0,
            funcNum: 7,
            out_tys: [],
            timeoutAt: undefined,
            waitIfNotPresent: false
            }));
          const {data: [], secs: v362, time: v361, didSend: v155, from: v360 } = txn5;
          ;
          const v364 = stdlib.addressEq(v266, v360);
          stdlib.assert(v364, {
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
          const {data: [v321], secs: v323, time: v322, didSend: v99, from: v320 } = txn4;
          ;
          const v325 = stdlib.addressEq(v278, v320);
          stdlib.assert(v325, {
            at: './index.rsh:77:9:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Bob'
            });
          const v327 = stdlib.add(v322, v268);
          const txn5 = await (ctc.recv({
            didSend: false,
            evt_cnt: 2,
            funcNum: 8,
            out_tys: [ctc0, ctc0],
            timeoutAt: ['time', v327],
            waitIfNotPresent: false
            }));
          if (txn5.didTimeout) {
            const txn6 = await (ctc.sendrecv({
              args: [v266, v267, v268, v278, v290, v309, v321, v327],
              evt_cnt: 0,
              funcNum: 9,
              lct: v322,
              onlyIf: true,
              out_tys: [],
              pay: [stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), []],
              sim_p: (async (txn6) => {
                const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
                
                const {data: [], secs: v345, time: v344, didSend: v129, from: v343 } = txn6;
                
                sim_r.txns.push({
                  amt: stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0),
                  kind: 'to',
                  tok: undefined
                  });
                const v347 = stdlib.addressEq(v278, v343);
                stdlib.assert(v347, {
                  at: 'reach standard library:206:7:dot',
                  fs: ['at ./index.rsh:86:53:application call to "closeTo" (defined at: reach standard library:204:8:function exp)'],
                  msg: 'sender correct',
                  who: 'Bob'
                  });
                sim_r.txns.push({
                  amt: v290,
                  kind: 'from',
                  to: v278,
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
            const {data: [], secs: v345, time: v344, didSend: v129, from: v343 } = txn6;
            ;
            const v347 = stdlib.addressEq(v278, v343);
            stdlib.assert(v347, {
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
            const {data: [v332, v333], secs: v335, time: v334, didSend: v110, from: v331 } = txn5;
            ;
            const v337 = stdlib.addressEq(v266, v331);
            stdlib.assert(v337, {
              at: './index.rsh:85:11:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Bob'
              });
            const v338 = stdlib.digest(ctc3, [v332, v333]);
            const v339 = stdlib.digestEq(v309, v338);
            stdlib.assert(v339, {
              at: 'reach standard library:62:17:application',
              fs: ['at ./index.rsh:87:20:application call to "checkCommitment" (defined at: reach standard library:61:8:function exp)'],
              msg: null,
              who: 'Bob'
              });
            const v340 = stdlib.sub(stdlib.checkedBigNumberify('./index.rsh:7:18:decimal', stdlib.UInt_max, 4), v321);
            const v341 = stdlib.add(v333, v340);
            const v342 = stdlib.mod(v341, stdlib.checkedBigNumberify('./index.rsh:7:34:decimal', stdlib.UInt_max, 3));
            const cv283 = v342;
            const cv284 = v334;
            const cv290 = v290;
            
            v283 = cv283;
            v284 = cv284;
            v290 = cv290;
            
            continue;}
          
          }
        
        }
      
      }
    const v394 = stdlib.eq(v283, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
    const v397 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:94:12:decimal', stdlib.UInt_max, 2), v267);
    const v399 = v394 ? v266 : v278;
    ;
    stdlib.protect(ctc1, await interact.seeOutcome(v283), {
      at: './index.rsh:98:24:application',
      fs: ['at ./index.rsh:97:7:application call to [unknown function] (defined at: ./index.rsh:97:25:function exp)'],
      msg: 'seeOutcome',
      who: 'Bob'
      });
    
    return;
    }
  
  
  
  };
const _ALGO = {
  appApproval: `#pragma version 5
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
int 0
extract_uint64
store 1
int 8
extract_uint64
store 2
txn NumAppArgs
int 3
==
assert
txna ApplicationArgs 0
btoi
preamble:
// Handler 0
dup
int 0
==
bz l0_afterHandler0
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
int 16
==
assert
dup
int 0
extract_uint64
store 255
dup
int 8
extract_uint64
store 254
pop
// "CheckPay"
// "./index.rsh:49:9:dot"
// "[]"
int 100000
dup
bz l1_checkTxnK
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
global CurrentApplicationAddress
dig 1
gtxns Receiver
==
assert
l1_checkTxnK:
pop
// "CheckPay"
// "./index.rsh:49:9:dot"
// "[]"
load 255
dup
bz l2_checkTxnK
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
global CurrentApplicationAddress
dig 1
gtxns Receiver
==
assert
l2_checkTxnK:
pop
global Round
load 254
+
store 253
txn Sender
load 255
itob
concat
load 254
itob
concat
load 253
itob
concat
int 1
bzero
dig 1
extract 0 56
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
l0_afterHandler0:
// Handler 1
dup
int 1
==
bz l3_afterHandler1
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
extract 0 32
store 255
dup
int 32
extract_uint64
store 254
dup
int 40
extract_uint64
store 253
dup
int 48
extract_uint64
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
bz l4_checkTxnK
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
global CurrentApplicationAddress
dig 1
gtxns Receiver
==
assert
l4_checkTxnK:
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
b loopBody3
l3_afterHandler1:
// Handler 2
dup
int 2
==
bz l5_afterHandler2
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
extract 0 32
store 255
dup
int 32
extract_uint64
store 254
dup
int 40
extract_uint64
store 253
dup
int 48
extract_uint64
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
bz l6_makeTxnK
itxn_begin
itxn_field Amount
int pay
itxn_field TypeEnum
load 255
itxn_field Receiver
itxn_submit
int 0
l6_makeTxnK:
pop
int 0
itxn_begin
itxn_field Amount
int pay
itxn_field TypeEnum
global CreatorAddress
itxn_field CloseRemainderTo
global CurrentApplicationAddress
itxn_field Receiver
itxn_submit
int 0
l7_makeTxnK:
pop
txn OnCompletion
int DeleteApplication
==
assert
b updateState
l5_afterHandler2:
l8_afterHandler3:
// Handler 4
dup
int 4
==
bz l9_afterHandler4
pop
// check step
int 5
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
extract 0 32
store 255
dup
int 32
extract_uint64
store 254
dup
int 40
extract_uint64
store 253
dup
extract 48 32
store 252
dup
int 80
extract_uint64
store 251
dup
int 88
extract_uint64
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
extract 0 127
app_global_put
byte base64(AQ==)
dig 1
extract 127 1
app_global_put
pop
int 7
store 1
global Round
store 2
txn OnCompletion
int NoOp
==
assert
b updateState
l9_afterHandler4:
// Handler 5
dup
int 5
==
bz l10_afterHandler5
pop
// check step
int 5
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
extract 0 32
store 255
dup
int 32
extract_uint64
store 254
dup
int 40
extract_uint64
store 253
dup
extract 48 32
store 252
dup
int 80
extract_uint64
store 251
dup
int 88
extract_uint64
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
bz l11_makeTxnK
itxn_begin
itxn_field Amount
int pay
itxn_field TypeEnum
load 252
itxn_field Receiver
itxn_submit
int 0
l11_makeTxnK:
pop
int 0
itxn_begin
itxn_field Amount
int pay
itxn_field TypeEnum
global CreatorAddress
itxn_field CloseRemainderTo
global CurrentApplicationAddress
itxn_field Receiver
itxn_submit
int 0
l12_makeTxnK:
pop
txn OnCompletion
int DeleteApplication
==
assert
b updateState
l10_afterHandler5:
// Handler 6
dup
int 6
==
bz l13_afterHandler6
pop
// check step
int 7
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
extract 0 32
store 255
dup
int 32
extract_uint64
store 254
dup
int 40
extract_uint64
store 253
dup
extract 48 32
store 252
dup
int 80
extract_uint64
store 251
dup
extract 88 32
store 250
dup
int 120
extract_uint64
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
extract 0 127
app_global_put
byte base64(AQ==)
dig 1
extract 127 9
app_global_put
pop
int 9
store 1
global Round
store 2
txn OnCompletion
int NoOp
==
assert
b updateState
l13_afterHandler6:
// Handler 7
dup
int 7
==
bz l14_afterHandler7
pop
// check step
int 7
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
extract 0 32
store 255
dup
int 32
extract_uint64
store 254
dup
int 40
extract_uint64
store 253
dup
extract 48 32
store 252
dup
int 80
extract_uint64
store 251
dup
extract 88 32
store 250
dup
int 120
extract_uint64
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
bz l15_makeTxnK
itxn_begin
itxn_field Amount
int pay
itxn_field TypeEnum
load 255
itxn_field Receiver
itxn_submit
int 0
l15_makeTxnK:
pop
int 0
itxn_begin
itxn_field Amount
int pay
itxn_field TypeEnum
global CreatorAddress
itxn_field CloseRemainderTo
global CurrentApplicationAddress
itxn_field Receiver
itxn_submit
int 0
l16_makeTxnK:
pop
txn OnCompletion
int DeleteApplication
==
assert
b updateState
l14_afterHandler7:
// Handler 8
dup
int 8
==
bz l17_afterHandler8
pop
// check step
int 9
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
extract 0 32
store 255
dup
int 32
extract_uint64
store 254
dup
int 40
extract_uint64
store 253
dup
extract 48 32
store 252
dup
int 80
extract_uint64
store 251
dup
extract 88 32
store 250
dup
int 120
extract_uint64
store 249
dup
int 128
extract_uint64
store 248
pop
txna ApplicationArgs 2
dup
len
int 16
==
assert
dup
int 0
extract_uint64
store 247
dup
int 8
extract_uint64
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
b loopBody3
l17_afterHandler8:
// Handler 9
dup
int 9
==
bz l18_afterHandler9
pop
// check step
int 9
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
extract 0 32
store 255
dup
int 32
extract_uint64
store 254
dup
int 40
extract_uint64
store 253
dup
extract 48 32
store 252
dup
int 80
extract_uint64
store 251
dup
extract 88 32
store 250
dup
int 120
extract_uint64
store 249
dup
int 128
extract_uint64
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
bz l19_makeTxnK
itxn_begin
itxn_field Amount
int pay
itxn_field TypeEnum
load 252
itxn_field Receiver
itxn_submit
int 0
l19_makeTxnK:
pop
int 0
itxn_begin
itxn_field Amount
int pay
itxn_field TypeEnum
global CreatorAddress
itxn_field CloseRemainderTo
global CurrentApplicationAddress
itxn_field Receiver
itxn_submit
int 0
l20_makeTxnK:
pop
txn OnCompletion
int DeleteApplication
==
assert
b updateState
l18_afterHandler9:
int 0
assert
loopBody3:
dup
int 0
extract_uint64
store 255
dup
int 8
extract_uint64
store 254
dup
int 16
extract_uint64
store 253
pop
dup
extract 0 32
store 252
dup
int 32
extract_uint64
store 251
dup
int 40
extract_uint64
store 250
dup
extract 48 32
store 249
pop
load 255
int 1
==
bz l21_ifF
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
extract 0 96
app_global_put
pop
int 5
store 1
global Round
store 2
txn OnCompletion
int NoOp
==
assert
b updateState
l21_ifF:
int 2
load 251
*
dup
bz l22_makeTxnK
itxn_begin
itxn_field Amount
int pay
itxn_field TypeEnum
load 249
load 252
load 255
int 2
==
select
itxn_field Receiver
itxn_submit
int 0
l22_makeTxnK:
pop
int 0
itxn_begin
itxn_field Amount
int pay
itxn_field TypeEnum
global CreatorAddress
itxn_field CloseRemainderTo
global CurrentApplicationAddress
itxn_field Receiver
itxn_submit
int 0
l23_makeTxnK:
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
b updateState
`,
  appClear: `#pragma version 5
int 0
`,
  mapDataKeys: 0,
  mapDataSize: 0,
  stateKeys: 2,
  stateSize: 136,
  unsupported: [],
  version: 6
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
                "name": "v267",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v268",
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
                "name": "v267",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v268",
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
    "name": "_reach_e0",
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
    "name": "_reach_e1",
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
    "name": "_reach_e2",
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
                "name": "v309",
                "type": "uint256"
              }
            ],
            "internalType": "struct T10",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct T11",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "_reach_e4",
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
    "name": "_reach_e5",
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
                "name": "v321",
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
    "name": "_reach_e6",
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
    "name": "_reach_e7",
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
                "name": "v332",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v333",
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
    "name": "_reach_e8",
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
    "name": "_reach_e9",
    "type": "event"
  },
  {
    "stateMutability": "payable",
    "type": "fallback"
  },
  {
    "inputs": [],
    "name": "_reachCreationTime",
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
    "inputs": [],
    "name": "_reachCurrentState",
    "outputs": [
      {
        "internalType": "uint256",
        "name": "",
        "type": "uint256"
      },
      {
        "internalType": "bytes",
        "name": "",
        "type": "bytes"
      }
    ],
    "stateMutability": "view",
    "type": "function"
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
    "name": "_reach_m1",
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
    "name": "_reach_m2",
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
                "name": "v309",
                "type": "uint256"
              }
            ],
            "internalType": "struct T10",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "internalType": "struct T11",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "_reach_m4",
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
    "name": "_reach_m5",
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
                "name": "v321",
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
    "name": "_reach_m6",
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
    "name": "_reach_m7",
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
                "name": "v332",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v333",
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
    "name": "_reach_m8",
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
    "name": "_reach_m9",
    "outputs": [],
    "stateMutability": "payable",
    "type": "function"
  },
  {
    "stateMutability": "payable",
    "type": "receive"
  }
]`,
  Bytecode: `0x608060405260405162001ad738038062001ad7833981016040819052620000269162000248565b600080805543600355604080516020810190915290815260408051835181526020808501518051828401520151918101919091527f80c0078efe412e5091172e0df54decefb16131f320816d23b64aede2bf8e9e4b9060600160405180910390a16020820151516200009c903414600762000141565b6020808301510151620000b09043620002a8565b81526040805160808082018352600060208084018281528486018381526060808701858152338089528b860180515186525186015184528a5182526001968790554390965588518086019690965292518589015290519084015251828401528451808303909301835260a0909101909352805191926200013792600292909101906200016b565b505050506200030c565b81620001675760405163100960cb60e01b81526004810182905260240160405180910390fd5b5050565b8280546200017990620002cf565b90600052602060002090601f0160209004810192826200019d5760008555620001e8565b82601f10620001b857805160ff1916838001178555620001e8565b82800160010185558215620001e8579182015b82811115620001e8578251825591602001919060010190620001cb565b50620001f6929150620001fa565b5090565b5b80821115620001f65760008155600101620001fb565b604080519081016001600160401b03811182821017156200024257634e487b7160e01b600052604160045260246000fd5b60405290565b600081830360608112156200025c57600080fd5b6200026662000211565b835181526040601f19830112156200027d57600080fd5b6200028762000211565b60208581015182526040909501518582015293810193909352509092915050565b60008219821115620002ca57634e487b7160e01b600052601160045260246000fd5b500190565b600181811c90821680620002e457607f821691505b602082108114156200030657634e487b7160e01b600052602260045260246000fd5b50919050565b6117bb806200031c6000396000f3fe60806040526004361061009a5760003560e01c80638e314769116100615780638e31476914610115578063980b6eac14610128578063a209ad4e1461013b578063ab53f2c61461014e578063bf2c5b2414610171578063de7369981461018457005b80631e93b0f1146100a35780632c10a159146100c75780637eea518c146100da57806383230757146100ed5780638328d4c41461010257005b366100a157005b005b3480156100af57600080fd5b506003545b6040519081526020015b60405180910390f35b6100a16100d536600461139b565b610197565b6100a16100e836600461139b565b61032c565b3480156100f957600080fd5b506001546100b4565b6100a16101103660046113be565b6104c0565b6100a161012336600461139b565b6106e4565b6100a161013636600461139b565b610864565b6100a161014936600461139b565b610b09565b34801561015a57600080fd5b50610163610d51565b6040516100be9291906113d0565b6100a161017f36600461139b565b610dee565b6100a161019236600461139b565b610f67565b6101a760016000541460096110a2565b6101c1813515806101ba57506001548235145b600a6110a2565b6000808055600280546101d39061142d565b80601f01602080910402602001604051908101604052809291908181526020018280546101ff9061142d565b801561024c5780601f106102215761010080835404028352916020019161024c565b820191906000526020600020905b81548152906001019060200180831161022f57829003601f168201915b5050505050806020019051810190610264919061147e565b905061027781606001514310600b6110a2565b7f79ca1a789d797004bc78dff9632d64e202e102f2d008dcc20c5a645ef7d4a7d1826040516102a691906114f7565b60405180910390a16102bf8160200151341460086110a2565b6102c7611254565b815181516001600160a01b0390911690526020808301805183518301526040808501518451909101528251336060909101528183018051600190525143920191909152516103159080611537565b602082015160400152610327816110c7565b505050565b61033c600160005414600e6110a2565b6103568135158061034f57506001548235145b600f6110a2565b6000808055600280546103689061142d565b80601f01602080910402602001604051908101604052809291908181526020018280546103949061142d565b80156103e15780601f106103b6576101008083540402835291602001916103e1565b820191906000526020600020905b8154815290600101906020018083116103c457829003601f168201915b50505050508060200190518101906103f9919061147e565b905061040d816060015143101560106110a2565b7f82e152e8b1d7e41adffbddbd5b2fe2e130356df9b7ab7d06526a80d7888af3e18260405161043c91906114f7565b60405180910390a16104503415600c6110a2565b8051610468906001600160a01b03163314600d6110a2565b805160208201516040516001600160a01b039092169181156108fc0291906000818181858888f193505050501580156104a5573d6000803e3d6000fd5b50600080805560018190556104bc906002906112ad565b5050565b6104d060096000541460286110a2565b6104ea813515806104e357506001548235145b60296110a2565b6000808055600280546104fc9061142d565b80601f01602080910402602001604051908101604052809291908181526020018280546105289061142d565b80156105755780601f1061054a57610100808354040283529160200191610575565b820191906000526020600020905b81548152906001019060200180831161055857829003601f168201915b505050505080602001905181019061058d919061154f565b90506105a08160e001514310602a6110a2565b604080518335815260208085013590820152838201358183015290517f41b6d8e223fb0a5cfe68af9f34b07a5a94b63517841457ccfc53fb18b8e41fde9181900360600190a16105f2341560256110a2565b805161060a906001600160a01b0316331460266110a2565b604080516106569161063091602080870135928701359101918252602082015260400190565b6040516020818303038152906040528051906020012060001c8260a001511460276110a2565b61065e611254565b815181516001600160a01b0391821690526020808401518351909101526040808401518351909101526060808401518351921691015260c08201516003906106a79060046115fb565b6106b5906040860135611537565b6106bf9190611612565b60208083018051929092528151439101526080830151905160400152610327816110c7565b6106f460056000541460186110a2565b61070e8135158061070757506001548235145b60196110a2565b6000808055600280546107209061142d565b80601f016020809104026020016040519081016040528092919081815260200182805461074c9061142d565b80156107995780601f1061076e57610100808354040283529160200191610799565b820191906000526020600020905b81548152906001019060200180831161077c57829003601f168201915b50505050508060200190518101906107b19190611634565b90506107c58160a00151431015601a6110a2565b7f9cdba579557d44a893ea7929682d6795d48dd5c40dc981d852842d4b18914de8826040516107f491906114f7565b60405180910390a1610808341560166110a2565b6060810151610823906001600160a01b0316331460176110a2565b80606001516001600160a01b03166108fc82608001519081150290604051600060405180830381858888f193505050501580156104a5573d6000803e3d6000fd5b610874600760005414601d6110a2565b61088e8135158061088757506001548235145b601e6110a2565b6000808055600280546108a09061142d565b80601f01602080910402602001604051908101604052809291908181526020018280546108cc9061142d565b80156109195780601f106108ee57610100808354040283529160200191610919565b820191906000526020600020905b8154815290600101906020018083116108fc57829003601f168201915b505050505080602001905181019061093191906116c8565b90506109496040518060200160405280600081525090565b61095a8260c001514310601f6110a2565b6040805184358152602080860135908201527f47a1195f23e4ca8f87a7a744a702eeb3eb5b0d56dae31e23931e0349a611c709910160405180910390a16109a33415601b6110a2565b60608201516109be906001600160a01b03163314601c6110a2565b60408201516109cd9043611537565b81526040805161010081018252600080825260208201819052918101829052606081018290526080810182905260a0810182905260c0810182905260e081019190915282516001600160a01b0390811682526020808501518184015260408086015181850152606080870151909316928401929092526080808601519084015260a080860151908401528581013560c0840152835160e08401526009600055436001559051610ade9183910160006101008201905060018060a01b038084511683526020840151602084015260408401516040840152806060850151166060840152506080830151608083015260a083015160a083015260c083015160c083015260e083015160e083015292915050565b60405160208183030381529060405260029080519060200190610b029291906112ea565b5050505050565b610b1960056000541460136110a2565b610b3381351580610b2c57506001548235145b60146110a2565b600080805560028054610b459061142d565b80601f0160208091040260200160405190810160405280929190818152602001828054610b719061142d565b8015610bbe5780601f10610b9357610100808354040283529160200191610bbe565b820191906000526020600020905b815481529060010190602001808311610ba157829003601f168201915b5050505050806020019051810190610bd69190611634565b9050610bee6040518060200160405280600081525090565b610bff8260a00151431060156110a2565b6040805184358152602080860135908201527f7d7741a24b17d1850d95beda5136388f520bc575ba9499f2f40fdfa7647ad82f910160405180910390a1610c48341560116110a2565b8151610c60906001600160a01b0316331460126110a2565b6040820151610c6f9043611537565b81526040805160e081018252600080825260208201819052918101829052606081018290526080810182905260a0810182905260c081019190915282516001600160a01b039081168083526020808601518185019081526040808801518187019081526060808a015187168189019081526080808c0151818b019081528d88013560a0808d019182528d5160c0808f0191825260076000554360015589519b8c019c909c529851978a0197909752945193880193909352905190971696850196909652945190830152925191810191909152905160e082015261010001610ade565b600060606000546002808054610d669061142d565b80601f0160208091040260200160405190810160405280929190818152602001828054610d929061142d565b8015610ddf5780601f10610db457610100808354040283529160200191610ddf565b820191906000526020600020905b815481529060010190602001808311610dc257829003601f168201915b50505050509050915091509091565b610dfe60076000541460226110a2565b610e1881351580610e1157506001548235145b60236110a2565b600080805560028054610e2a9061142d565b80601f0160208091040260200160405190810160405280929190818152602001828054610e569061142d565b8015610ea35780601f10610e7857610100808354040283529160200191610ea3565b820191906000526020600020905b815481529060010190602001808311610e8657829003601f168201915b5050505050806020019051810190610ebb91906116c8565b9050610ecf8160c0015143101560246110a2565b7fba16100ad25f3c6798bc3b7e9ca316fb231388e6fa4444c0f477e2a4336514e082604051610efe91906114f7565b60405180910390a1610f12341560206110a2565b8051610f2a906001600160a01b0316331460216110a2565b805160808201516040516001600160a01b039092169181156108fc0291906000818181858888f193505050501580156104a5573d6000803e3d6000fd5b610f77600960005414602d6110a2565b610f9181351580610f8a57506001548235145b602e6110a2565b600080805560028054610fa39061142d565b80601f0160208091040260200160405190810160405280929190818152602001828054610fcf9061142d565b801561101c5780601f10610ff15761010080835404028352916020019161101c565b820191906000526020600020905b815481529060010190602001808311610fff57829003601f168201915b5050505050806020019051810190611034919061154f565b90506110488160e00151431015602f6110a2565b7fb764c356a899e639c4043e82fb6274894baac6d84c74f3b3ae78d8f4b22b00038260405161107791906114f7565b60405180910390a161108b3415602b6110a2565b6060810151610823906001600160a01b03163314602c5b816104bc5760405163100960cb60e01b81526004810182905260240160405180910390fd5b604080516020810190915260008152602082015151600114156111ef5781516040015160208084015101516110fc9190611537565b81526040805160c081018252600080825260208201819052918101829052606081018290526080810182905260a08101919091528251516001600160a01b039081168083528451602090810151818501908152865160409081015181870190815288516060908101518716818901908152858b01518401516080808b019182528b5160a0808d019182526005600055436001558751998a019a909a529651958801959095529251918601919091525190951690830152925191810191909152905160c082015260e001604051602081830303815290604052600290805190602001906111e99291906112ea565b50505050565b6020820151516002146112075781516060015161120b565b8151515b6001600160a01b03166108fc836000015160200151600261122c9190611766565b6040518115909202916000818181858888f193505050501580156104a5573d6000803e3d6000fd5b6040805160c0810182526000918101828152606082018390526080820183905260a082019290925290819081526020016112a860405180606001604052806000815260200160008152602001600081525090565b905290565b5080546112b99061142d565b6000825580601f106112c9575050565b601f0160209004906000526020600020908101906112e7919061136e565b50565b8280546112f69061142d565b90600052602060002090601f016020900481019282611318576000855561135e565b82601f1061133157805160ff191683800117855561135e565b8280016001018555821561135e579182015b8281111561135e578251825591602001919060010190611343565b5061136a92915061136e565b5090565b5b8082111561136a576000815560010161136f565b60006040828403121561139557600080fd5b50919050565b6000604082840312156113ad57600080fd5b6113b78383611383565b9392505050565b60006060828403121561139557600080fd5b82815260006020604081840152835180604085015260005b81811015611404578581018301518582016060015282016113e8565b81811115611416576000606083870101525b50601f01601f191692909201606001949350505050565b600181811c9082168061144157607f821691505b6020821081141561139557634e487b7160e01b600052602260045260246000fd5b80516001600160a01b038116811461147957600080fd5b919050565b60006080828403121561149057600080fd5b6040516080810181811067ffffffffffffffff821117156114c157634e487b7160e01b600052604160045260246000fd5b6040526114cd83611462565b81526020830151602082015260408301516040820152606083015160608201528091505092915050565b8135815260408101602083013580151580821461151357600080fd5b806020850152505092915050565b634e487b7160e01b600052601160045260246000fd5b6000821982111561154a5761154a611521565b500190565b600061010080838503121561156357600080fd5b6040519081019067ffffffffffffffff8211818310171561159457634e487b7160e01b600052604160045260246000fd5b816040526115a184611462565b815260208401516020820152604084015160408201526115c360608501611462565b60608201526080840151608082015260a084015160a082015260c084015160c082015260e084015160e0820152809250505092915050565b60008282101561160d5761160d611521565b500390565b60008261162f57634e487b7160e01b600052601260045260246000fd5b500690565b600060c0828403121561164657600080fd5b60405160c0810181811067ffffffffffffffff8211171561167757634e487b7160e01b600052604160045260246000fd5b60405261168383611462565b815260208301516020820152604083015160408201526116a560608401611462565b60608201526080830151608082015260a083015160a08201528091505092915050565b600060e082840312156116da57600080fd5b60405160e0810181811067ffffffffffffffff8211171561170b57634e487b7160e01b600052604160045260246000fd5b60405261171783611462565b8152602083015160208201526040830151604082015261173960608401611462565b60608201526080830151608082015260a083015160a082015260c083015160c08201528091505092915050565b600081600019048311821515161561178057611780611521565b50029056fea2646970667358221220c450cea51adcfc03f8fded0f485c7c9af92453b0b19a7824d56ba0eaa6f17e6c64736f6c634300080a0033`,
  BytecodeLen: 6871,
  Which: `oD`,
  version: 5,
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
