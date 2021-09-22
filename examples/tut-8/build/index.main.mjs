// Automatically generated with Reach 0.1.5
/* eslint-disable */
export const _version = '0.1.5';
export const _backendVersion = 2;


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
  
  
  const v253 = stdlib.protect(ctc0, interact.deadline, 'for Alice\'s interact field deadline');
  const v254 = stdlib.protect(ctc0, interact.wager, 'for Alice\'s interact field wager');
  
  const txn1 = await (ctc.sendrecv({
    args: [v254, v253],
    evt_cnt: 2,
    funcNum: 0,
    onlyIf: true,
    out_tys: [ctc0, ctc0],
    pay: [v254, []],
    sim_p: (async (txn1) => {
      const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
      
      const [v258, v259] = txn1.data;
      const v260 = txn1.time;
      const v261 = txn1.secs;
      const v257 = txn1.from;
      
      sim_r.txns.push({
        amt: v258,
        kind: 'to',
        tok: undefined
        });
      const v265 = stdlib.add(v260, v259);
      sim_r.isHalt = false;
      
      return sim_r;
      }),
    soloSend: true,
    timeoutAt: undefined,
    tys: [ctc0, ctc0],
    waitIfNotPresent: false
    }));
  const [v258, v259] = txn1.data;
  const v260 = txn1.time;
  const v261 = txn1.secs;
  const v257 = txn1.from;
  ;
  const v265 = stdlib.add(v260, v259);
  const txn2 = await (ctc.recv({
    evt_cnt: 0,
    funcNum: 1,
    out_tys: [],
    timeoutAt: ['time', v265],
    waitIfNotPresent: false
    }));
  if (txn2.didTimeout) {
    const txn3 = await (ctc.sendrecv({
      args: [v257, v258, v259, v265],
      evt_cnt: 0,
      funcNum: 2,
      onlyIf: true,
      out_tys: [],
      pay: [stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), []],
      sim_p: (async (txn3) => {
        const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
        
        const [] = txn3.data;
        const v402 = txn3.time;
        const v403 = txn3.secs;
        const v401 = txn3.from;
        
        sim_r.txns.push({
          amt: stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0),
          kind: 'to',
          tok: undefined
          });
        const v405 = stdlib.addressEq(v257, v401);
        stdlib.assert(v405, {
          at: 'reach standard library:209:7:dot',
          fs: ['at ./index.rsh:57:51:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
          msg: 'sender correct',
          who: 'Alice'
          });
        sim_r.txns.push({
          amt: v258,
          kind: 'from',
          to: v257,
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
    const v402 = txn3.time;
    const v403 = txn3.secs;
    const v401 = txn3.from;
    ;
    const v405 = stdlib.addressEq(v257, v401);
    stdlib.assert(v405, {
      at: 'reach standard library:209:7:dot',
      fs: ['at ./index.rsh:57:51:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
      msg: 'sender correct',
      who: 'Alice'
      });
    ;
    stdlib.protect(ctc3, await interact.informTimeout(), {
      at: './index.rsh:41:29:application',
      fs: ['at ./index.rsh:40:9:application call to [unknown function] (defined at: ./index.rsh:40:27:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./index.rsh:39:28:function exp)', 'at ./index.rsh:57:51:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
      msg: 'informTimeout',
      who: 'Alice'
      });
    
    return;
    }
  else {
    const [] = txn2.data;
    const v270 = txn2.time;
    const v271 = txn2.secs;
    const v269 = txn2.from;
    const v273 = stdlib.add(v258, v258);
    ;
    let v274 = stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1);
    let v275 = v270;
    let v281 = v273;
    
    while ((() => {
      const v289 = stdlib.eq(v274, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
      
      return v289;})()) {
      const v291 = stdlib.add(v275, v259);
      const v295 = stdlib.protect(ctc0, await interact.getHand(), {
        at: './index.rsh:65:42:application',
        fs: ['at ./index.rsh:64:15:application call to [unknown function] (defined at: ./index.rsh:64:19:function exp)'],
        msg: 'getHand',
        who: 'Alice'
        });
      const v296 = stdlib.protect(ctc0, await interact.random(), {
        at: 'reach standard library:60:31:application',
        fs: ['at ./index.rsh:66:56:application call to "makeCommitment" (defined at: reach standard library:59:8:function exp)', 'at ./index.rsh:64:15:application call to [unknown function] (defined at: ./index.rsh:64:19:function exp)'],
        msg: 'random',
        who: 'Alice'
        });
      const v297 = stdlib.digest(ctc1, [v296, v295]);
      
      const txn3 = await (ctc.sendrecv({
        args: [v257, v258, v259, v269, v281, v291, v297],
        evt_cnt: 1,
        funcNum: 5,
        onlyIf: true,
        out_tys: [ctc2],
        pay: [stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), []],
        sim_p: (async (txn3) => {
          const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
          
          const [v300] = txn3.data;
          const v301 = txn3.time;
          const v302 = txn3.secs;
          const v299 = txn3.from;
          
          sim_r.txns.push({
            amt: stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0),
            kind: 'to',
            tok: undefined
            });
          const v304 = stdlib.addressEq(v257, v299);
          stdlib.assert(v304, {
            at: './index.rsh:69:11:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Alice'
            });
          const v306 = stdlib.add(v301, v259);
          sim_r.isHalt = false;
          
          return sim_r;
          }),
        soloSend: true,
        timeoutAt: ['time', v291],
        tys: [ctc4, ctc0, ctc0, ctc4, ctc0, ctc0, ctc2],
        waitIfNotPresent: false
        }));
      if (txn3.didTimeout) {
        const txn4 = await (ctc.recv({
          evt_cnt: 0,
          funcNum: 6,
          out_tys: [],
          timeoutAt: undefined,
          waitIfNotPresent: false
          }));
        const [] = txn4.data;
        const v369 = txn4.time;
        const v370 = txn4.secs;
        const v368 = txn4.from;
        ;
        const v372 = stdlib.addressEq(v269, v368);
        stdlib.assert(v372, {
          at: 'reach standard library:209:7:dot',
          fs: ['at ./index.rsh:70:53:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
          msg: 'sender correct',
          who: 'Alice'
          });
        ;
        stdlib.protect(ctc3, await interact.informTimeout(), {
          at: './index.rsh:41:29:application',
          fs: ['at ./index.rsh:40:9:application call to [unknown function] (defined at: ./index.rsh:40:27:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./index.rsh:39:28:function exp)', 'at ./index.rsh:70:53:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
          msg: 'informTimeout',
          who: 'Alice'
          });
        
        return;
        }
      else {
        const [v300] = txn3.data;
        const v301 = txn3.time;
        const v302 = txn3.secs;
        const v299 = txn3.from;
        ;
        const v304 = stdlib.addressEq(v257, v299);
        stdlib.assert(v304, {
          at: './index.rsh:69:11:dot',
          fs: [],
          msg: 'sender correct',
          who: 'Alice'
          });
        const v306 = stdlib.add(v301, v259);
        const txn4 = await (ctc.recv({
          evt_cnt: 1,
          funcNum: 7,
          out_tys: [ctc0],
          timeoutAt: ['time', v306],
          waitIfNotPresent: false
          }));
        if (txn4.didTimeout) {
          const txn5 = await (ctc.sendrecv({
            args: [v257, v258, v259, v269, v281, v300, v306],
            evt_cnt: 0,
            funcNum: 8,
            onlyIf: true,
            out_tys: [],
            pay: [stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), []],
            sim_p: (async (txn5) => {
              const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
              
              const [] = txn5.data;
              const v352 = txn5.time;
              const v353 = txn5.secs;
              const v351 = txn5.from;
              
              sim_r.txns.push({
                amt: stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0),
                kind: 'to',
                tok: undefined
                });
              const v355 = stdlib.addressEq(v257, v351);
              stdlib.assert(v355, {
                at: 'reach standard library:209:7:dot',
                fs: ['at ./index.rsh:78:53:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
                msg: 'sender correct',
                who: 'Alice'
                });
              sim_r.txns.push({
                amt: v281,
                kind: 'from',
                to: v257,
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
          const v352 = txn5.time;
          const v353 = txn5.secs;
          const v351 = txn5.from;
          ;
          const v355 = stdlib.addressEq(v257, v351);
          stdlib.assert(v355, {
            at: 'reach standard library:209:7:dot',
            fs: ['at ./index.rsh:78:53:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
            msg: 'sender correct',
            who: 'Alice'
            });
          ;
          stdlib.protect(ctc3, await interact.informTimeout(), {
            at: './index.rsh:41:29:application',
            fs: ['at ./index.rsh:40:9:application call to [unknown function] (defined at: ./index.rsh:40:27:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./index.rsh:39:28:function exp)', 'at ./index.rsh:78:53:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
            msg: 'informTimeout',
            who: 'Alice'
            });
          
          return;
          }
        else {
          const [v312] = txn4.data;
          const v313 = txn4.time;
          const v314 = txn4.secs;
          const v311 = txn4.from;
          ;
          const v316 = stdlib.addressEq(v269, v311);
          stdlib.assert(v316, {
            at: './index.rsh:77:9:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Alice'
            });
          const v318 = stdlib.add(v313, v259);
          const txn5 = await (ctc.sendrecv({
            args: [v257, v258, v259, v269, v281, v300, v312, v318, v296, v295],
            evt_cnt: 2,
            funcNum: 9,
            onlyIf: true,
            out_tys: [ctc0, ctc0],
            pay: [stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), []],
            sim_p: (async (txn5) => {
              const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
              
              const [v323, v324] = txn5.data;
              const v325 = txn5.time;
              const v326 = txn5.secs;
              const v322 = txn5.from;
              
              sim_r.txns.push({
                amt: stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0),
                kind: 'to',
                tok: undefined
                });
              const v328 = stdlib.addressEq(v257, v322);
              stdlib.assert(v328, {
                at: './index.rsh:85:11:dot',
                fs: [],
                msg: 'sender correct',
                who: 'Alice'
                });
              const v329 = stdlib.digest(ctc1, [v323, v324]);
              const v330 = stdlib.digestEq(v300, v329);
              stdlib.assert(v330, {
                at: 'reach standard library:65:17:application',
                fs: ['at ./index.rsh:87:20:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)'],
                msg: null,
                who: 'Alice'
                });
              const v331 = stdlib.sub(stdlib.checkedBigNumberify('./index.rsh:7:14:decimal', stdlib.UInt_max, 4), v312);
              const v332 = stdlib.add(v324, v331);
              const v333 = stdlib.mod(v332, stdlib.checkedBigNumberify('./index.rsh:7:28:decimal', stdlib.UInt_max, 3));
              const cv274 = v333;
              const cv275 = v325;
              const cv281 = v281;
              
              (() => {
                const v274 = cv274;
                const v275 = cv275;
                const v281 = cv281;
                
                if ((() => {
                  const v289 = stdlib.eq(v274, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
                  
                  return v289;})()) {
                  const v291 = stdlib.add(v275, v259);
                  sim_r.isHalt = false;
                  }
                else {
                  const v385 = stdlib.eq(v274, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
                  const v388 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:94:12:decimal', stdlib.UInt_max, 2), v258);
                  const v390 = v385 ? v257 : v269;
                  sim_r.txns.push({
                    amt: v388,
                    kind: 'from',
                    to: v390,
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
            timeoutAt: ['time', v318],
            tys: [ctc4, ctc0, ctc0, ctc4, ctc0, ctc2, ctc0, ctc0, ctc0, ctc0],
            waitIfNotPresent: false
            }));
          if (txn5.didTimeout) {
            const txn6 = await (ctc.recv({
              evt_cnt: 0,
              funcNum: 10,
              out_tys: [],
              timeoutAt: undefined,
              waitIfNotPresent: false
              }));
            const [] = txn6.data;
            const v335 = txn6.time;
            const v336 = txn6.secs;
            const v334 = txn6.from;
            ;
            const v338 = stdlib.addressEq(v269, v334);
            stdlib.assert(v338, {
              at: 'reach standard library:209:7:dot',
              fs: ['at ./index.rsh:86:53:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
              msg: 'sender correct',
              who: 'Alice'
              });
            ;
            stdlib.protect(ctc3, await interact.informTimeout(), {
              at: './index.rsh:41:29:application',
              fs: ['at ./index.rsh:40:9:application call to [unknown function] (defined at: ./index.rsh:40:27:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./index.rsh:39:28:function exp)', 'at ./index.rsh:86:53:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
              msg: 'informTimeout',
              who: 'Alice'
              });
            
            return;
            }
          else {
            const [v323, v324] = txn5.data;
            const v325 = txn5.time;
            const v326 = txn5.secs;
            const v322 = txn5.from;
            ;
            const v328 = stdlib.addressEq(v257, v322);
            stdlib.assert(v328, {
              at: './index.rsh:85:11:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Alice'
              });
            const v329 = stdlib.digest(ctc1, [v323, v324]);
            const v330 = stdlib.digestEq(v300, v329);
            stdlib.assert(v330, {
              at: 'reach standard library:65:17:application',
              fs: ['at ./index.rsh:87:20:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)'],
              msg: null,
              who: 'Alice'
              });
            const v331 = stdlib.sub(stdlib.checkedBigNumberify('./index.rsh:7:14:decimal', stdlib.UInt_max, 4), v312);
            const v332 = stdlib.add(v324, v331);
            const v333 = stdlib.mod(v332, stdlib.checkedBigNumberify('./index.rsh:7:28:decimal', stdlib.UInt_max, 3));
            const cv274 = v333;
            const cv275 = v325;
            const cv281 = v281;
            
            v274 = cv274;
            v275 = cv275;
            v281 = cv281;
            
            continue;}
          }
        }
      }
    const v385 = stdlib.eq(v274, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
    const v388 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:94:12:decimal', stdlib.UInt_max, 2), v258);
    const v390 = v385 ? v257 : v269;
    ;
    stdlib.protect(ctc3, await interact.seeOutcome(v274), {
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
  
  
  const txn1 = await (ctc.recv({
    evt_cnt: 2,
    funcNum: 0,
    out_tys: [ctc0, ctc0],
    timeoutAt: undefined,
    waitIfNotPresent: false
    }));
  const [v258, v259] = txn1.data;
  const v260 = txn1.time;
  const v261 = txn1.secs;
  const v257 = txn1.from;
  ;
  const v265 = stdlib.add(v260, v259);
  stdlib.protect(ctc1, await interact.acceptWager(v258), {
    at: './index.rsh:54:25:application',
    fs: ['at ./index.rsh:53:11:application call to [unknown function] (defined at: ./index.rsh:53:15:function exp)'],
    msg: 'acceptWager',
    who: 'Bob'
    });
  
  const txn2 = await (ctc.sendrecv({
    args: [v257, v258, v259, v265],
    evt_cnt: 0,
    funcNum: 1,
    onlyIf: true,
    out_tys: [],
    pay: [v258, []],
    sim_p: (async (txn2) => {
      const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
      
      const [] = txn2.data;
      const v270 = txn2.time;
      const v271 = txn2.secs;
      const v269 = txn2.from;
      
      const v273 = stdlib.add(v258, v258);
      sim_r.txns.push({
        amt: v258,
        kind: 'to',
        tok: undefined
        });
      const v274 = stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1);
      const v275 = v270;
      const v281 = v273;
      
      if ((() => {
        const v289 = stdlib.eq(v274, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
        
        return v289;})()) {
        const v291 = stdlib.add(v275, v259);
        sim_r.isHalt = false;
        }
      else {
        const v385 = stdlib.eq(v274, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
        const v388 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:94:12:decimal', stdlib.UInt_max, 2), v258);
        const v390 = v385 ? v257 : v269;
        sim_r.txns.push({
          amt: v388,
          kind: 'from',
          to: v390,
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
    timeoutAt: ['time', v265],
    tys: [ctc4, ctc0, ctc0, ctc0],
    waitIfNotPresent: false
    }));
  if (txn2.didTimeout) {
    const txn3 = await (ctc.recv({
      evt_cnt: 0,
      funcNum: 2,
      out_tys: [],
      timeoutAt: undefined,
      waitIfNotPresent: false
      }));
    const [] = txn3.data;
    const v402 = txn3.time;
    const v403 = txn3.secs;
    const v401 = txn3.from;
    ;
    const v405 = stdlib.addressEq(v257, v401);
    stdlib.assert(v405, {
      at: 'reach standard library:209:7:dot',
      fs: ['at ./index.rsh:57:51:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
      msg: 'sender correct',
      who: 'Bob'
      });
    ;
    stdlib.protect(ctc1, await interact.informTimeout(), {
      at: './index.rsh:41:29:application',
      fs: ['at ./index.rsh:40:9:application call to [unknown function] (defined at: ./index.rsh:40:27:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./index.rsh:39:28:function exp)', 'at ./index.rsh:57:51:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
      msg: 'informTimeout',
      who: 'Bob'
      });
    
    return;
    }
  else {
    const [] = txn2.data;
    const v270 = txn2.time;
    const v271 = txn2.secs;
    const v269 = txn2.from;
    const v273 = stdlib.add(v258, v258);
    ;
    let v274 = stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1);
    let v275 = v270;
    let v281 = v273;
    
    while ((() => {
      const v289 = stdlib.eq(v274, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
      
      return v289;})()) {
      const v291 = stdlib.add(v275, v259);
      const txn3 = await (ctc.recv({
        evt_cnt: 1,
        funcNum: 5,
        out_tys: [ctc2],
        timeoutAt: ['time', v291],
        waitIfNotPresent: false
        }));
      if (txn3.didTimeout) {
        const txn4 = await (ctc.sendrecv({
          args: [v257, v258, v259, v269, v281, v291],
          evt_cnt: 0,
          funcNum: 6,
          onlyIf: true,
          out_tys: [],
          pay: [stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), []],
          sim_p: (async (txn4) => {
            const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
            
            const [] = txn4.data;
            const v369 = txn4.time;
            const v370 = txn4.secs;
            const v368 = txn4.from;
            
            sim_r.txns.push({
              amt: stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0),
              kind: 'to',
              tok: undefined
              });
            const v372 = stdlib.addressEq(v269, v368);
            stdlib.assert(v372, {
              at: 'reach standard library:209:7:dot',
              fs: ['at ./index.rsh:70:53:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
              msg: 'sender correct',
              who: 'Bob'
              });
            sim_r.txns.push({
              amt: v281,
              kind: 'from',
              to: v269,
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
        const v369 = txn4.time;
        const v370 = txn4.secs;
        const v368 = txn4.from;
        ;
        const v372 = stdlib.addressEq(v269, v368);
        stdlib.assert(v372, {
          at: 'reach standard library:209:7:dot',
          fs: ['at ./index.rsh:70:53:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
          msg: 'sender correct',
          who: 'Bob'
          });
        ;
        stdlib.protect(ctc1, await interact.informTimeout(), {
          at: './index.rsh:41:29:application',
          fs: ['at ./index.rsh:40:9:application call to [unknown function] (defined at: ./index.rsh:40:27:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./index.rsh:39:28:function exp)', 'at ./index.rsh:70:53:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
          msg: 'informTimeout',
          who: 'Bob'
          });
        
        return;
        }
      else {
        const [v300] = txn3.data;
        const v301 = txn3.time;
        const v302 = txn3.secs;
        const v299 = txn3.from;
        ;
        const v304 = stdlib.addressEq(v257, v299);
        stdlib.assert(v304, {
          at: './index.rsh:69:11:dot',
          fs: [],
          msg: 'sender correct',
          who: 'Bob'
          });
        const v306 = stdlib.add(v301, v259);
        const v310 = stdlib.protect(ctc0, await interact.getHand(), {
          at: './index.rsh:75:50:application',
          fs: ['at ./index.rsh:74:13:application call to [unknown function] (defined at: ./index.rsh:74:17:function exp)'],
          msg: 'getHand',
          who: 'Bob'
          });
        
        const txn4 = await (ctc.sendrecv({
          args: [v257, v258, v259, v269, v281, v300, v306, v310],
          evt_cnt: 1,
          funcNum: 7,
          onlyIf: true,
          out_tys: [ctc0],
          pay: [stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), []],
          sim_p: (async (txn4) => {
            const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
            
            const [v312] = txn4.data;
            const v313 = txn4.time;
            const v314 = txn4.secs;
            const v311 = txn4.from;
            
            sim_r.txns.push({
              amt: stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0),
              kind: 'to',
              tok: undefined
              });
            const v316 = stdlib.addressEq(v269, v311);
            stdlib.assert(v316, {
              at: './index.rsh:77:9:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Bob'
              });
            const v318 = stdlib.add(v313, v259);
            sim_r.isHalt = false;
            
            return sim_r;
            }),
          soloSend: true,
          timeoutAt: ['time', v306],
          tys: [ctc4, ctc0, ctc0, ctc4, ctc0, ctc2, ctc0, ctc0],
          waitIfNotPresent: false
          }));
        if (txn4.didTimeout) {
          const txn5 = await (ctc.recv({
            evt_cnt: 0,
            funcNum: 8,
            out_tys: [],
            timeoutAt: undefined,
            waitIfNotPresent: false
            }));
          const [] = txn5.data;
          const v352 = txn5.time;
          const v353 = txn5.secs;
          const v351 = txn5.from;
          ;
          const v355 = stdlib.addressEq(v257, v351);
          stdlib.assert(v355, {
            at: 'reach standard library:209:7:dot',
            fs: ['at ./index.rsh:78:53:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
            msg: 'sender correct',
            who: 'Bob'
            });
          ;
          stdlib.protect(ctc1, await interact.informTimeout(), {
            at: './index.rsh:41:29:application',
            fs: ['at ./index.rsh:40:9:application call to [unknown function] (defined at: ./index.rsh:40:27:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./index.rsh:39:28:function exp)', 'at ./index.rsh:78:53:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
            msg: 'informTimeout',
            who: 'Bob'
            });
          
          return;
          }
        else {
          const [v312] = txn4.data;
          const v313 = txn4.time;
          const v314 = txn4.secs;
          const v311 = txn4.from;
          ;
          const v316 = stdlib.addressEq(v269, v311);
          stdlib.assert(v316, {
            at: './index.rsh:77:9:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Bob'
            });
          const v318 = stdlib.add(v313, v259);
          const txn5 = await (ctc.recv({
            evt_cnt: 2,
            funcNum: 9,
            out_tys: [ctc0, ctc0],
            timeoutAt: ['time', v318],
            waitIfNotPresent: false
            }));
          if (txn5.didTimeout) {
            const txn6 = await (ctc.sendrecv({
              args: [v257, v258, v259, v269, v281, v300, v312, v318],
              evt_cnt: 0,
              funcNum: 10,
              onlyIf: true,
              out_tys: [],
              pay: [stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), []],
              sim_p: (async (txn6) => {
                const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
                
                const [] = txn6.data;
                const v335 = txn6.time;
                const v336 = txn6.secs;
                const v334 = txn6.from;
                
                sim_r.txns.push({
                  amt: stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0),
                  kind: 'to',
                  tok: undefined
                  });
                const v338 = stdlib.addressEq(v269, v334);
                stdlib.assert(v338, {
                  at: 'reach standard library:209:7:dot',
                  fs: ['at ./index.rsh:86:53:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
                  msg: 'sender correct',
                  who: 'Bob'
                  });
                sim_r.txns.push({
                  amt: v281,
                  kind: 'from',
                  to: v269,
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
            const v335 = txn6.time;
            const v336 = txn6.secs;
            const v334 = txn6.from;
            ;
            const v338 = stdlib.addressEq(v269, v334);
            stdlib.assert(v338, {
              at: 'reach standard library:209:7:dot',
              fs: ['at ./index.rsh:86:53:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
              msg: 'sender correct',
              who: 'Bob'
              });
            ;
            stdlib.protect(ctc1, await interact.informTimeout(), {
              at: './index.rsh:41:29:application',
              fs: ['at ./index.rsh:40:9:application call to [unknown function] (defined at: ./index.rsh:40:27:function exp)', 'at reach standard library:212:8:application call to "after" (defined at: ./index.rsh:39:28:function exp)', 'at ./index.rsh:86:53:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
              msg: 'informTimeout',
              who: 'Bob'
              });
            
            return;
            }
          else {
            const [v323, v324] = txn5.data;
            const v325 = txn5.time;
            const v326 = txn5.secs;
            const v322 = txn5.from;
            ;
            const v328 = stdlib.addressEq(v257, v322);
            stdlib.assert(v328, {
              at: './index.rsh:85:11:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Bob'
              });
            const v329 = stdlib.digest(ctc3, [v323, v324]);
            const v330 = stdlib.digestEq(v300, v329);
            stdlib.assert(v330, {
              at: 'reach standard library:65:17:application',
              fs: ['at ./index.rsh:87:20:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)'],
              msg: null,
              who: 'Bob'
              });
            const v331 = stdlib.sub(stdlib.checkedBigNumberify('./index.rsh:7:14:decimal', stdlib.UInt_max, 4), v312);
            const v332 = stdlib.add(v324, v331);
            const v333 = stdlib.mod(v332, stdlib.checkedBigNumberify('./index.rsh:7:28:decimal', stdlib.UInt_max, 3));
            const cv274 = v333;
            const cv275 = v325;
            const cv281 = v281;
            
            v274 = cv274;
            v275 = cv275;
            v281 = cv281;
            
            continue;}
          }
        }
      }
    const v385 = stdlib.eq(v274, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
    const v388 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:94:12:decimal', stdlib.UInt_max, 2), v258);
    const v390 = v385 ? v257 : v269;
    ;
    stdlib.protect(ctc1, await interact.seeOutcome(v274), {
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
// Handler 0
dup
int 0
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
store 2
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
load 2
dig 1
gtxns Receiver
==
assert
l1:
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
load 2
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
// compute state in HM_Set 1
byte base64(AAAAAAAAAAE=)
txn Sender
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
store 1
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
load 2
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
// "[at ./index.rsh:57:51:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
// Just "sender correct"
// "reach standard library:209:7:dot"
// "[at ./index.rsh:57:51:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
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
l7:
pop
global ZeroAddress
store 1
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
l10:
// Handler 6
dup
int 6
==
bz l11
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
// "[at ./index.rsh:70:53:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
// Just "sender correct"
// "reach standard library:209:7:dot"
// "[at ./index.rsh:70:53:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
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
l13:
pop
global ZeroAddress
store 1
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
l14:
// Handler 8
dup
int 8
==
bz l15
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
// "[at ./index.rsh:78:53:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
// Just "sender correct"
// "reach standard library:209:7:dot"
// "[at ./index.rsh:78:53:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
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
l17:
pop
global ZeroAddress
store 1
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
b loop3
l18:
// Handler 10
dup
int 10
==
bz l19
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
// "[at ./index.rsh:86:53:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
// Just "sender correct"
// "reach standard library:209:7:dot"
// "[at ./index.rsh:86:53:application call to \"closeTo\" (defined at: reach standard library:207:8:function exp)]"
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
l21:
pop
global ZeroAddress
store 1
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
l24:
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
// compute state in HM_Set 0
int 8
bzero
sha256
store 1
global ZeroAddress
store 2
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
  version: 3,
  viewKeys: 0,
  viewSize: 0
  };
const _ETH = {
  ABI: `[
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
                "name": "v258",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v259",
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
            "internalType": "bool",
            "name": "svs",
            "type": "bool"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v258",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v259",
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
                "internalType": "address payable",
                "name": "v257",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v258",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v259",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v265",
                "type": "uint256"
              }
            ],
            "internalType": "struct T0",
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
                "name": "v257",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v258",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v259",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v269",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v281",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v300",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v312",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v318",
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
                "name": "v257",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v258",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v259",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v265",
                "type": "uint256"
              }
            ],
            "internalType": "struct T0",
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
                "name": "v257",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v258",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v259",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v269",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v281",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v291",
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
                "name": "v300",
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
            "components": [
              {
                "internalType": "address payable",
                "name": "v257",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v258",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v259",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v269",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v281",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v291",
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
                "name": "v257",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v258",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v259",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v269",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v281",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v300",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v306",
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
                "name": "v312",
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
                "name": "v257",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v258",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v259",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v269",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v281",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v300",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v306",
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
                "name": "v257",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v258",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v259",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v269",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v281",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v300",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v312",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v318",
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
                "name": "v323",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v324",
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
                "internalType": "address payable",
                "name": "v257",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v258",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v259",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v265",
                "type": "uint256"
              }
            ],
            "internalType": "struct T0",
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
                "name": "v257",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v258",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v259",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v269",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v281",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v300",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v312",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v318",
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
                "name": "v257",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v258",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v259",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v265",
                "type": "uint256"
              }
            ],
            "internalType": "struct T0",
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
                "name": "v257",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v258",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v259",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v269",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v281",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v291",
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
                "name": "v300",
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
            "components": [
              {
                "internalType": "address payable",
                "name": "v257",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v258",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v259",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v269",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v281",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v291",
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
                "name": "v257",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v258",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v259",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v269",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v281",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v300",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v306",
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
                "name": "v312",
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
                "name": "v257",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v258",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v259",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v269",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v281",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v300",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v306",
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
                "name": "v257",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v258",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v259",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v269",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v281",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v300",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v312",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v318",
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
                "name": "v323",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v324",
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
  Bytecode: `0x60806040526040516200145438038062001454833981016040819052620000269162000178565b60005462000037901560086200014e565b600160009081556040805160208101909152908152604080518351151581526020808501518051828401520151918101919091527f9d5ba219b87dd13e03a1956c722934895023c1d5a7e5c00d947ca1995412acab9060600160405180910390a1602082015151620000ad90341460076200014e565b6020808301510151620000c190436200021e565b81526040805160808082018352600060208084018281528486018381526060808701858152338852998401805151845251840151825297518952865160018185015295516001600160a01b031686880152905196850196909652945191830191909152935160a0808301919091528251808303909101815260c09091019091528051910120905562000245565b81620001745760405163100960cb60e01b81526004810182905260240160405180910390fd5b5050565b600081830360608112156200018c57600080fd5b62000196620001e7565b83518015158114620001a757600080fd5b81526040601f1983011215620001bc57600080fd5b620001c6620001e7565b60208581015182526040909501518582015293810193909352509092915050565b604080519081016001600160401b03811182821017156200021857634e487b7160e01b600052604160045260246000fd5b60405290565b600082198211156200024057634e487b7160e01b600052601160045260246000fd5b500190565b6111ff80620002556000396000f3fe60806040526004361061007f5760003560e01c8063cf5086021161004e578063cf508602146100d9578063dc633495146100ec578063e4f83a8f146100ff578063f443a28f1461011257600080fd5b80631ec1ab6d1461008b5780633b74b33a146100a05780633b962ab3146100b35780636d4900ae146100c657600080fd5b3661008657005b600080fd5b61009e610099366004610e07565b610125565b005b61009e6100ae366004610e07565b610233565b61009e6100c1366004610e23565b610424565b61009e6100d4366004610e66565b6104e9565b61009e6100e7366004610e53565b6105f7565b61009e6100fa366004610e40565b6106b2565b61009e61010d366004610e66565b61085e565b61009e610120366004610e23565b61095e565b6040516101619061013d90600690849060200161112f565b6040516020818303038152906040528051906020012060001c600054146016610b62565b600160005561017860a08201354310156017610b62565b7f2a1ae4c0a095161254f4ecea68f616d94a34e8108628e6be95569a51a9eafd64816040516101a79190610fe4565b60405180910390a16101bb34156014610b62565b6101e0336101cf6080840160608501610de5565b6001600160a01b0316146015610b62565b6101f06080820160608301610de5565b6040516001600160a01b039190911690608083013580156108fc02916000818181858888f1935050505015801561022b573d6000803e3d6000fd5b506000805533ff5b60405161026f9061024b90600690849060200161112f565b6040516020818303038152906040528051906020012060001c600054146012610b62565b60016000908155604080516020810190915290815261029560a083013543106013610b62565b7f38ea967788413d32da9cf5fa9e9988de96945693fc96038c2572b4b9ad3c6e08826040516102c49190610fc8565b60405180910390a16102d834156010610b62565b6102fa336102e96020850185610de5565b6001600160a01b0316146011610b62565b610308604083013543611143565b81526040805160e08101825260008082526020808301829052928201819052606082018190526080820181905260a0820181905260c08201529061034e90840184610de5565b6001600160a01b03168152602080840135908201526040808401359082015261037d6080840160608501610de5565b6001600160a01b03908116606083810191825260808681013581860190815260c08089013560a08089019182528951838a019081526040805160086020808301919091528c518c16828401528c0151988101989098528a0151958701959095529551909616948401949094525192820192909252915160e083015251610100820152610120015b60408051601f198184030181529190528051602090910120600055505050565b6040516104609061043c906008908490602001611105565b6040516020818303038152906040528051906020012060001c60005414601e610b62565b600160005561047760c0820135431015601f610b62565b7f56ed6936527d17fa80129fafb1256d72e8ac3627c12594ff8dcc9cba2c2b5a6c816040516104a69190611031565b60405180910390a16104ba3415601c610b62565b6104dc336104cb6020840184610de5565b6001600160a01b031614601d610b62565b6101f06020820182610de5565b604051610525906105019060019084906020016110f1565b6040516020818303038152906040528051906020012060001c60005414600a610b62565b600160005561053b60608201354310600b610b62565b7f5c212effa0f7d379821cc3f261ad724a41039683bb9d9745221629df85f7171c8160405161056a91906110c1565b60405180910390a1610583346020830135146009610b62565b61058b610d45565b6105986020830183610de5565b81516001600160a01b0390911690528051602080840135918101829052825160408086013591015282513360609091015280830180516001905251439101526105e19080611143565b6020820151604001526105f381610b87565b5050565b6040516106339061060f90600a90849060200161111a565b6040516020818303038152906040528051906020012060001c600054146027610b62565b600160005561064a60e08201354310156028610b62565b7fd7b0ec994d58e0f0e9c6b883c43e1f89692e638a7e7b1019ab9543aa768b8df581604051610679919061108e565b60405180910390a161068d34156025610b62565b6101e0336106a16080840160608501610de5565b6001600160a01b0316146026610b62565b6040516106ee906106ca90600a90849060200161111a565b6040516020818303038152906040528051906020012060001c600054146023610b62565b600160005561070460e082013543106024610b62565b7f9d7e5708cde6a18107f1c7a38169eee25fca31598b87879225b23551ffaabb60816040516107339190611062565b60405180910390a161074734156020610b62565b610769336107586020840184610de5565b6001600160a01b0316146021610b62565b604080516101008301356020820152610120830135918101919091526107b19060600160408051601f19818403018152919052805160209091012060a0830135146022610b62565b6107b9610d45565b6107c66020830183610de5565b81516001600160a01b039091169052805160208084013591015280516040808401359101526107fb6080830160608401610de5565b81516001600160a01b03909116606090910152600361081f60c0840135600461117a565b61082e90610120850135611143565b6108389190611191565b60208083018051929092528151439101525160808301356040909101526105f381610b87565b60405161089a906108769060019084906020016110f1565b6040516020818303038152906040528051906020012060001c60005414600e610b62565b60016000556108b16060820135431015600f610b62565b7fe2fcb5361608dd42d825c4e917fd4fca89057bb8eb0b7e34b8c2813a114cc152816040516108e091906110c1565b60405180910390a16108f43415600c610b62565b610916336109056020840184610de5565b6001600160a01b031614600d610b62565b6109236020820182610de5565b6040516001600160a01b039190911690602083013580156108fc02916000818181858888f1935050505015801561022b573d6000803e3d6000fd5b60405161099a90610976906008908490602001611105565b6040516020818303038152906040528051906020012060001c60005414601a610b62565b6001600090815560408051602081019091529081526109c060c08301354310601b610b62565b7fdfd035919af65444a2feb843249f397ffc0d187cfcce385d1ab580de78b70843826040516109ef9190611014565b60405180910390a1610a0334156018610b62565b610a2833610a176080850160608601610de5565b6001600160a01b0316146019610b62565b610a36604083013543611143565b8152604080516101008101825260008082526020808301829052928201819052606082018190526080820181905260a0820181905260c0820181905260e082015290610a8490840184610de5565b6001600160a01b031681526020808401359082015260408084013590820152610ab36080840160608501610de5565b6001600160a01b031660608201526080808401359082015260a0808401359082015260e08084013560c083015282519082015260405161040490600a90839060200160006101208201905083825260018060a01b03808451166020840152602084015160408401526040840151606084015280606085015116608084015250608083015160a083015260a083015160c083015260c083015160e083015260e08301516101008301529392505050565b816105f35760405163100960cb60e01b81526004810182905260240160405180910390fd5b60408051602081019091526000815260208201515160011415610c72578151604001516020808401510151610bbc9190611143565b81526040805160c08082018352600080835260208084018281528486018381526060808701858152608080890187815260a0808b019889528d51516001600160a01b039081168c528e5189015188528e518d015187528e51860151811685528e8901518d015183528d518a528c516006998101999099528b5181169c89019c909c529551938701939093529251928501929092529051909616908201529351918401919091525160e08301529061010001610404565b6040805160c081018252600091810182815260608083018481526080840185815260a085018681528486526020808701979097528851516001600160a01b03908116909552885187015190925287519092015190921690529184015151909152610cdb81610ce0565b505050565b805160600151600214610cf857805160400151610cfc565b8051515b6001600160a01b03166108fc8260000151602001516002610d1d919061115b565b6040518115909202916000818181858888f1935050505015801561022b573d6000803e3d6000fd5b6040805160c0810182526000918101828152606082018390526080820183905260a08201929092529081908152602001610d9960405180606001604052806000815260200160008152602001600081525090565b905290565b80356001600160a01b0381168114610db557600080fd5b919050565b600060e08284031215610dcc57600080fd5b50919050565b60006101008284031215610dcc57600080fd5b600060208284031215610df757600080fd5b610e0082610d9e565b9392505050565b600060e08284031215610e1957600080fd5b610e008383610dba565b60006101008284031215610e3657600080fd5b610e008383610dd2565b60006101408284031215610dcc57600080fd5b60006101208284031215610dcc57600080fd5b600060a08284031215610dcc57600080fd5b6001600160a01b03610e8982610d9e565b1682526020810135602083015260408101356040830152606081013560608301525050565b6001600160a01b0380610ec083610d9e565b168352602082013560208401526040820135604084015280610ee460608401610d9e565b166060840152506080810135608083015260a081013560a083015260c081013560c08301525050565b6001600160a01b0380610f1f83610d9e565b168352602082013560208401526040820135604084015280610f4360608401610d9e565b166060840152506080810135608083015260a081013560a083015260c081013560c083015260e081013560e08301525050565b6001600160a01b0380610f8883610d9e565b168352602082013560208401526040820135604084015280610fac60608401610d9e565b166060840152506080818101359083015260a090810135910152565b60e08101610fd68284610f76565b60c092830135919092015290565b60e08101610ff28284610f76565b60c083013580151580821461100657600080fd5b8060c0850152505092915050565b61010081016110238284610eae565b60e092830135919092015290565b61010081016110408284610eae565b60e083013580151580821461105457600080fd5b8060e0850152505092915050565b61014081016110718284610f0d565b610100838101359083015261012092830135929091019190915290565b610120810161109d8284610f0d565b610100808401358015158082146110b357600080fd5b808386015250505092915050565b60a081016110cf8284610e78565b60808301358015158082146110e357600080fd5b806080850152505092915050565b82815260a08101610e006020830184610e78565b8281526101008101610e006020830184610eae565b8281526101208101610e006020830184610f0d565b82815260e08101610e006020830184610f76565b60008219821115611156576111566111b3565b500190565b6000816000190483118215151615611175576111756111b3565b500290565b60008282101561118c5761118c6111b3565b500390565b6000826111ae57634e487b7160e01b600052601260045260246000fd5b500690565b634e487b7160e01b600052601160045260246000fdfea26469706673582212208d132b59189fb9f7c8ea142faf156b0c2675744c847058bf956e57f2466513e464736f6c63430008070033`,
  BytecodeLen: 5204,
  Which: `oD`,
  version: 2,
  views: {
    }
  };

export const _Connectors = {
  ALGO: _ALGO,
  ETH: _ETH
  };

