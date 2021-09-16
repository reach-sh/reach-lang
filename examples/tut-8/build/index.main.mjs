// Automatically generated with Reach 0.1.4
/* eslint-disable */
export const _version = '0.1.4';
export const _backendVersion = 2;
export const _deployMode = 'DM_constructor';


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
  
  
  const v257 = stdlib.protect(ctc0, interact.deadline, 'for Alice\'s interact field deadline');
  const v258 = stdlib.protect(ctc0, interact.wager, 'for Alice\'s interact field wager');
  
  const txn1 = await (ctc.recv({
    evt_cnt: 0,
    funcNum: 0,
    out_tys: [],
    timeoutAt: undefined,
    waitIfNotPresent: false
    }));
  const [] = txn1.data;
  const v260 = txn1.time;
  const v261 = txn1.secs;
  const v259 = txn1.from;
  ;
  const txn2 = await (ctc.sendrecv({
    args: [v258, v257],
    evt_cnt: 2,
    funcNum: 1,
    onlyIf: true,
    out_tys: [ctc0, ctc0],
    pay: [v258, []],
    sim_p: (async (txn2) => {
      const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
      
      const [v266, v267] = txn2.data;
      const v268 = txn2.time;
      const v269 = txn2.secs;
      const v265 = txn2.from;
      
      sim_r.txns.push({
        amt: v266,
        kind: 'to',
        tok: undefined
        });
      const v273 = stdlib.add(v268, v267);
      sim_r.isHalt = false;
      
      return sim_r;
      }),
    soloSend: true,
    timeoutAt: undefined,
    tys: [ctc0, ctc0],
    waitIfNotPresent: false
    }));
  const [v266, v267] = txn2.data;
  const v268 = txn2.time;
  const v269 = txn2.secs;
  const v265 = txn2.from;
  ;
  const v273 = stdlib.add(v268, v267);
  const txn3 = await (ctc.recv({
    evt_cnt: 0,
    funcNum: 2,
    out_tys: [],
    timeoutAt: ['time', v273],
    waitIfNotPresent: false
    }));
  if (txn3.didTimeout) {
    const txn4 = await (ctc.sendrecv({
      args: [v265, v266, v267, v273],
      evt_cnt: 0,
      funcNum: 3,
      onlyIf: true,
      out_tys: [],
      pay: [stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), []],
      sim_p: (async (txn4) => {
        const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
        
        const [] = txn4.data;
        const v410 = txn4.time;
        const v411 = txn4.secs;
        const v409 = txn4.from;
        
        sim_r.txns.push({
          amt: stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0),
          kind: 'to',
          tok: undefined
          });
        const v413 = stdlib.addressEq(v265, v409);
        stdlib.assert(v413, {
          at: 'reach standard library:209:7:dot',
          fs: ['at ./index.rsh:57:37:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
          msg: 'sender correct',
          who: 'Alice'
          });
        sim_r.txns.push({
          amt: v266,
          kind: 'from',
          to: v265,
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
    const [] = txn4.data;
    const v410 = txn4.time;
    const v411 = txn4.secs;
    const v409 = txn4.from;
    ;
    const v413 = stdlib.addressEq(v265, v409);
    stdlib.assert(v413, {
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
    const [] = txn3.data;
    const v278 = txn3.time;
    const v279 = txn3.secs;
    const v277 = txn3.from;
    const v281 = stdlib.add(v266, v266);
    ;
    let v282 = stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1);
    let v283 = v278;
    let v289 = v281;
    
    while ((() => {
      const v297 = stdlib.eq(v282, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
      
      return v297;})()) {
      const v299 = stdlib.add(v283, v267);
      const v303 = stdlib.protect(ctc0, await interact.getHand(), {
        at: './index.rsh:65:42:application',
        fs: ['at ./index.rsh:64:15:application call to [unknown function] (defined at: ./index.rsh:64:19:function exp)'],
        msg: 'getHand',
        who: 'Alice'
        });
      const v304 = stdlib.protect(ctc0, await interact.random(), {
        at: 'reach standard library:60:31:application',
        fs: ['at ./index.rsh:66:56:application call to "makeCommitment" (defined at: reach standard library:59:8:function exp)', 'at ./index.rsh:64:15:application call to [unknown function] (defined at: ./index.rsh:64:19:function exp)'],
        msg: 'random',
        who: 'Alice'
        });
      const v305 = stdlib.digest(ctc1, [v304, v303]);
      
      const txn4 = await (ctc.sendrecv({
        args: [v265, v266, v267, v277, v289, v299, v305],
        evt_cnt: 1,
        funcNum: 6,
        onlyIf: true,
        out_tys: [ctc2],
        pay: [stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), []],
        sim_p: (async (txn4) => {
          const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
          
          const [v308] = txn4.data;
          const v309 = txn4.time;
          const v310 = txn4.secs;
          const v307 = txn4.from;
          
          sim_r.txns.push({
            amt: stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0),
            kind: 'to',
            tok: undefined
            });
          const v312 = stdlib.addressEq(v265, v307);
          stdlib.assert(v312, {
            at: './index.rsh:69:11:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Alice'
            });
          const v314 = stdlib.add(v309, v267);
          sim_r.isHalt = false;
          
          return sim_r;
          }),
        soloSend: true,
        timeoutAt: ['time', v299],
        tys: [ctc4, ctc0, ctc0, ctc4, ctc0, ctc0, ctc2],
        waitIfNotPresent: false
        }));
      if (txn4.didTimeout) {
        const txn5 = await (ctc.recv({
          evt_cnt: 0,
          funcNum: 7,
          out_tys: [],
          timeoutAt: undefined,
          waitIfNotPresent: false
          }));
        const [] = txn5.data;
        const v377 = txn5.time;
        const v378 = txn5.secs;
        const v376 = txn5.from;
        ;
        const v380 = stdlib.addressEq(v277, v376);
        stdlib.assert(v380, {
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
        const [v308] = txn4.data;
        const v309 = txn4.time;
        const v310 = txn4.secs;
        const v307 = txn4.from;
        ;
        const v312 = stdlib.addressEq(v265, v307);
        stdlib.assert(v312, {
          at: './index.rsh:69:11:dot',
          fs: [],
          msg: 'sender correct',
          who: 'Alice'
          });
        const v314 = stdlib.add(v309, v267);
        const txn5 = await (ctc.recv({
          evt_cnt: 1,
          funcNum: 8,
          out_tys: [ctc0],
          timeoutAt: ['time', v314],
          waitIfNotPresent: false
          }));
        if (txn5.didTimeout) {
          const txn6 = await (ctc.sendrecv({
            args: [v265, v266, v267, v277, v289, v308, v314],
            evt_cnt: 0,
            funcNum: 9,
            onlyIf: true,
            out_tys: [],
            pay: [stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), []],
            sim_p: (async (txn6) => {
              const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
              
              const [] = txn6.data;
              const v360 = txn6.time;
              const v361 = txn6.secs;
              const v359 = txn6.from;
              
              sim_r.txns.push({
                amt: stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0),
                kind: 'to',
                tok: undefined
                });
              const v363 = stdlib.addressEq(v265, v359);
              stdlib.assert(v363, {
                at: 'reach standard library:209:7:dot',
                fs: ['at ./index.rsh:78:39:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
                msg: 'sender correct',
                who: 'Alice'
                });
              sim_r.txns.push({
                amt: v289,
                kind: 'from',
                to: v265,
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
          const [] = txn6.data;
          const v360 = txn6.time;
          const v361 = txn6.secs;
          const v359 = txn6.from;
          ;
          const v363 = stdlib.addressEq(v265, v359);
          stdlib.assert(v363, {
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
          const [v320] = txn5.data;
          const v321 = txn5.time;
          const v322 = txn5.secs;
          const v319 = txn5.from;
          ;
          const v324 = stdlib.addressEq(v277, v319);
          stdlib.assert(v324, {
            at: './index.rsh:77:9:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Alice'
            });
          const v326 = stdlib.add(v321, v267);
          const txn6 = await (ctc.sendrecv({
            args: [v265, v266, v267, v277, v289, v308, v320, v326, v304, v303],
            evt_cnt: 2,
            funcNum: 10,
            onlyIf: true,
            out_tys: [ctc0, ctc0],
            pay: [stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), []],
            sim_p: (async (txn6) => {
              const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
              
              const [v331, v332] = txn6.data;
              const v333 = txn6.time;
              const v334 = txn6.secs;
              const v330 = txn6.from;
              
              sim_r.txns.push({
                amt: stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0),
                kind: 'to',
                tok: undefined
                });
              const v336 = stdlib.addressEq(v265, v330);
              stdlib.assert(v336, {
                at: './index.rsh:85:11:dot',
                fs: [],
                msg: 'sender correct',
                who: 'Alice'
                });
              const v337 = stdlib.digest(ctc1, [v331, v332]);
              const v338 = stdlib.digestEq(v308, v337);
              stdlib.assert(v338, {
                at: 'reach standard library:65:17:application',
                fs: ['at ./index.rsh:87:20:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)'],
                msg: null,
                who: 'Alice'
                });
              const v339 = stdlib.sub(stdlib.checkedBigNumberify('./index.rsh:7:14:decimal', stdlib.UInt_max, 4), v320);
              const v340 = stdlib.add(v332, v339);
              const v341 = stdlib.mod(v340, stdlib.checkedBigNumberify('./index.rsh:7:28:decimal', stdlib.UInt_max, 3));
              const cv282 = v341;
              const cv283 = v333;
              const cv289 = v289;
              
              (() => {
                const v282 = cv282;
                const v283 = cv283;
                const v289 = cv289;
                
                if ((() => {
                  const v297 = stdlib.eq(v282, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
                  
                  return v297;})()) {
                  const v299 = stdlib.add(v283, v267);
                  sim_r.isHalt = false;
                  }
                else {
                  const v393 = stdlib.eq(v282, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
                  const v396 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:94:12:decimal', stdlib.UInt_max, 2), v266);
                  const v398 = v393 ? v265 : v277;
                  sim_r.txns.push({
                    amt: v396,
                    kind: 'from',
                    to: v398,
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
            timeoutAt: ['time', v326],
            tys: [ctc4, ctc0, ctc0, ctc4, ctc0, ctc2, ctc0, ctc0, ctc0, ctc0],
            waitIfNotPresent: false
            }));
          if (txn6.didTimeout) {
            const txn7 = await (ctc.recv({
              evt_cnt: 0,
              funcNum: 11,
              out_tys: [],
              timeoutAt: undefined,
              waitIfNotPresent: false
              }));
            const [] = txn7.data;
            const v343 = txn7.time;
            const v344 = txn7.secs;
            const v342 = txn7.from;
            ;
            const v346 = stdlib.addressEq(v277, v342);
            stdlib.assert(v346, {
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
            const [v331, v332] = txn6.data;
            const v333 = txn6.time;
            const v334 = txn6.secs;
            const v330 = txn6.from;
            ;
            const v336 = stdlib.addressEq(v265, v330);
            stdlib.assert(v336, {
              at: './index.rsh:85:11:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Alice'
              });
            const v337 = stdlib.digest(ctc1, [v331, v332]);
            const v338 = stdlib.digestEq(v308, v337);
            stdlib.assert(v338, {
              at: 'reach standard library:65:17:application',
              fs: ['at ./index.rsh:87:20:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)'],
              msg: null,
              who: 'Alice'
              });
            const v339 = stdlib.sub(stdlib.checkedBigNumberify('./index.rsh:7:14:decimal', stdlib.UInt_max, 4), v320);
            const v340 = stdlib.add(v332, v339);
            const v341 = stdlib.mod(v340, stdlib.checkedBigNumberify('./index.rsh:7:28:decimal', stdlib.UInt_max, 3));
            const cv282 = v341;
            const cv283 = v333;
            const cv289 = v289;
            
            v282 = cv282;
            v283 = cv283;
            v289 = cv289;
            
            continue;}
          }
        }
      }
    const v393 = stdlib.eq(v282, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
    const v396 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:94:12:decimal', stdlib.UInt_max, 2), v266);
    const v398 = v393 ? v265 : v277;
    ;
    stdlib.protect(ctc3, await interact.seeOutcome(v282), {
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
    evt_cnt: 0,
    funcNum: 0,
    out_tys: [],
    timeoutAt: undefined,
    waitIfNotPresent: false
    }));
  const [] = txn1.data;
  const v260 = txn1.time;
  const v261 = txn1.secs;
  const v259 = txn1.from;
  ;
  const txn2 = await (ctc.recv({
    evt_cnt: 2,
    funcNum: 1,
    out_tys: [ctc0, ctc0],
    timeoutAt: undefined,
    waitIfNotPresent: false
    }));
  const [v266, v267] = txn2.data;
  const v268 = txn2.time;
  const v269 = txn2.secs;
  const v265 = txn2.from;
  ;
  const v273 = stdlib.add(v268, v267);
  stdlib.protect(ctc1, await interact.acceptWager(v266), {
    at: './index.rsh:54:25:application',
    fs: ['at ./index.rsh:53:11:application call to [unknown function] (defined at: ./index.rsh:53:15:function exp)'],
    msg: 'acceptWager',
    who: 'Bob'
    });
  
  const txn3 = await (ctc.sendrecv({
    args: [v265, v266, v267, v273],
    evt_cnt: 0,
    funcNum: 2,
    onlyIf: true,
    out_tys: [],
    pay: [v266, []],
    sim_p: (async (txn3) => {
      const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
      
      const [] = txn3.data;
      const v278 = txn3.time;
      const v279 = txn3.secs;
      const v277 = txn3.from;
      
      const v281 = stdlib.add(v266, v266);
      sim_r.txns.push({
        amt: v266,
        kind: 'to',
        tok: undefined
        });
      const v282 = stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1);
      const v283 = v278;
      const v289 = v281;
      
      if ((() => {
        const v297 = stdlib.eq(v282, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
        
        return v297;})()) {
        const v299 = stdlib.add(v283, v267);
        sim_r.isHalt = false;
        }
      else {
        const v393 = stdlib.eq(v282, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
        const v396 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:94:12:decimal', stdlib.UInt_max, 2), v266);
        const v398 = v393 ? v265 : v277;
        sim_r.txns.push({
          amt: v396,
          kind: 'from',
          to: v398,
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
    timeoutAt: ['time', v273],
    tys: [ctc4, ctc0, ctc0, ctc0],
    waitIfNotPresent: false
    }));
  if (txn3.didTimeout) {
    const txn4 = await (ctc.recv({
      evt_cnt: 0,
      funcNum: 3,
      out_tys: [],
      timeoutAt: undefined,
      waitIfNotPresent: false
      }));
    const [] = txn4.data;
    const v410 = txn4.time;
    const v411 = txn4.secs;
    const v409 = txn4.from;
    ;
    const v413 = stdlib.addressEq(v265, v409);
    stdlib.assert(v413, {
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
    const [] = txn3.data;
    const v278 = txn3.time;
    const v279 = txn3.secs;
    const v277 = txn3.from;
    const v281 = stdlib.add(v266, v266);
    ;
    let v282 = stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1);
    let v283 = v278;
    let v289 = v281;
    
    while ((() => {
      const v297 = stdlib.eq(v282, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
      
      return v297;})()) {
      const v299 = stdlib.add(v283, v267);
      const txn4 = await (ctc.recv({
        evt_cnt: 1,
        funcNum: 6,
        out_tys: [ctc2],
        timeoutAt: ['time', v299],
        waitIfNotPresent: false
        }));
      if (txn4.didTimeout) {
        const txn5 = await (ctc.sendrecv({
          args: [v265, v266, v267, v277, v289, v299],
          evt_cnt: 0,
          funcNum: 7,
          onlyIf: true,
          out_tys: [],
          pay: [stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), []],
          sim_p: (async (txn5) => {
            const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
            
            const [] = txn5.data;
            const v377 = txn5.time;
            const v378 = txn5.secs;
            const v376 = txn5.from;
            
            sim_r.txns.push({
              amt: stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0),
              kind: 'to',
              tok: undefined
              });
            const v380 = stdlib.addressEq(v277, v376);
            stdlib.assert(v380, {
              at: 'reach standard library:209:7:dot',
              fs: ['at ./index.rsh:70:39:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
              msg: 'sender correct',
              who: 'Bob'
              });
            sim_r.txns.push({
              amt: v289,
              kind: 'from',
              to: v277,
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
        const [] = txn5.data;
        const v377 = txn5.time;
        const v378 = txn5.secs;
        const v376 = txn5.from;
        ;
        const v380 = stdlib.addressEq(v277, v376);
        stdlib.assert(v380, {
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
        const [v308] = txn4.data;
        const v309 = txn4.time;
        const v310 = txn4.secs;
        const v307 = txn4.from;
        ;
        const v312 = stdlib.addressEq(v265, v307);
        stdlib.assert(v312, {
          at: './index.rsh:69:11:dot',
          fs: [],
          msg: 'sender correct',
          who: 'Bob'
          });
        const v314 = stdlib.add(v309, v267);
        const v318 = stdlib.protect(ctc0, await interact.getHand(), {
          at: './index.rsh:75:50:application',
          fs: ['at ./index.rsh:74:13:application call to [unknown function] (defined at: ./index.rsh:74:17:function exp)'],
          msg: 'getHand',
          who: 'Bob'
          });
        
        const txn5 = await (ctc.sendrecv({
          args: [v265, v266, v267, v277, v289, v308, v314, v318],
          evt_cnt: 1,
          funcNum: 8,
          onlyIf: true,
          out_tys: [ctc0],
          pay: [stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), []],
          sim_p: (async (txn5) => {
            const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
            
            const [v320] = txn5.data;
            const v321 = txn5.time;
            const v322 = txn5.secs;
            const v319 = txn5.from;
            
            sim_r.txns.push({
              amt: stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0),
              kind: 'to',
              tok: undefined
              });
            const v324 = stdlib.addressEq(v277, v319);
            stdlib.assert(v324, {
              at: './index.rsh:77:9:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Bob'
              });
            const v326 = stdlib.add(v321, v267);
            sim_r.isHalt = false;
            
            return sim_r;
            }),
          soloSend: true,
          timeoutAt: ['time', v314],
          tys: [ctc4, ctc0, ctc0, ctc4, ctc0, ctc2, ctc0, ctc0],
          waitIfNotPresent: false
          }));
        if (txn5.didTimeout) {
          const txn6 = await (ctc.recv({
            evt_cnt: 0,
            funcNum: 9,
            out_tys: [],
            timeoutAt: undefined,
            waitIfNotPresent: false
            }));
          const [] = txn6.data;
          const v360 = txn6.time;
          const v361 = txn6.secs;
          const v359 = txn6.from;
          ;
          const v363 = stdlib.addressEq(v265, v359);
          stdlib.assert(v363, {
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
          const [v320] = txn5.data;
          const v321 = txn5.time;
          const v322 = txn5.secs;
          const v319 = txn5.from;
          ;
          const v324 = stdlib.addressEq(v277, v319);
          stdlib.assert(v324, {
            at: './index.rsh:77:9:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Bob'
            });
          const v326 = stdlib.add(v321, v267);
          const txn6 = await (ctc.recv({
            evt_cnt: 2,
            funcNum: 10,
            out_tys: [ctc0, ctc0],
            timeoutAt: ['time', v326],
            waitIfNotPresent: false
            }));
          if (txn6.didTimeout) {
            const txn7 = await (ctc.sendrecv({
              args: [v265, v266, v267, v277, v289, v308, v320, v326],
              evt_cnt: 0,
              funcNum: 11,
              onlyIf: true,
              out_tys: [],
              pay: [stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), []],
              sim_p: (async (txn7) => {
                const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
                
                const [] = txn7.data;
                const v343 = txn7.time;
                const v344 = txn7.secs;
                const v342 = txn7.from;
                
                sim_r.txns.push({
                  amt: stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0),
                  kind: 'to',
                  tok: undefined
                  });
                const v346 = stdlib.addressEq(v277, v342);
                stdlib.assert(v346, {
                  at: 'reach standard library:209:7:dot',
                  fs: ['at ./index.rsh:86:39:application call to "closeTo" (defined at: reach standard library:207:8:function exp)'],
                  msg: 'sender correct',
                  who: 'Bob'
                  });
                sim_r.txns.push({
                  amt: v289,
                  kind: 'from',
                  to: v277,
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
            const [] = txn7.data;
            const v343 = txn7.time;
            const v344 = txn7.secs;
            const v342 = txn7.from;
            ;
            const v346 = stdlib.addressEq(v277, v342);
            stdlib.assert(v346, {
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
            const [v331, v332] = txn6.data;
            const v333 = txn6.time;
            const v334 = txn6.secs;
            const v330 = txn6.from;
            ;
            const v336 = stdlib.addressEq(v265, v330);
            stdlib.assert(v336, {
              at: './index.rsh:85:11:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Bob'
              });
            const v337 = stdlib.digest(ctc3, [v331, v332]);
            const v338 = stdlib.digestEq(v308, v337);
            stdlib.assert(v338, {
              at: 'reach standard library:65:17:application',
              fs: ['at ./index.rsh:87:20:application call to "checkCommitment" (defined at: reach standard library:64:8:function exp)'],
              msg: null,
              who: 'Bob'
              });
            const v339 = stdlib.sub(stdlib.checkedBigNumberify('./index.rsh:7:14:decimal', stdlib.UInt_max, 4), v320);
            const v340 = stdlib.add(v332, v339);
            const v341 = stdlib.mod(v340, stdlib.checkedBigNumberify('./index.rsh:7:28:decimal', stdlib.UInt_max, 3));
            const cv282 = v341;
            const cv283 = v333;
            const cv289 = v289;
            
            v282 = cv282;
            v283 = cv283;
            v289 = cv289;
            
            continue;}
          }
        }
      }
    const v393 = stdlib.eq(v282, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
    const v396 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:94:12:decimal', stdlib.UInt_max, 2), v266);
    const v398 = v393 ? v265 : v277;
    ;
    stdlib.protect(ctc1, await interact.seeOutcome(v282), {
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
int 32
==
assert
dup
store 255
pop
txn Sender
global CreatorAddress
==
assert
load 255
store 2
// "CheckPay"
// "./index.rsh:37:11:after expr stmt semicolon"
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
// "./index.rsh:37:11:after expr stmt semicolon"
// "[]"
// compute state in HM_Set 1
byte base64(AAAAAAAAAAE=)
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
bz l2
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
// compute state in HM_Check 1
byte base64(AAAAAAAAAAE=)
sha256
load 1
==
assert
// "CheckPay"
// "./index.rsh:49:9:dot"
// "[]"
load 255
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
global Round
load 254
+
store 253
// compute state in HM_Set 2
byte base64(AAAAAAAAAAI=)
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
l2:
// Handler 2
dup
int 2
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
// compute state in HM_Check 2
byte base64(AAAAAAAAAAI=)
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
gtxns Receiver
==
assert
l5:
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
l4:
// Handler 3
dup
int 3
==
bz l6
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
// compute state in HM_Check 2
byte base64(AAAAAAAAAAI=)
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
bz l7
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
l7:
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
l8:
pop
global ZeroAddress
store 1
txn OnCompletion
int DeleteApplication
==
assert
b updateState
l6:
l9:
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
int 32
==
assert
dup
store 249
pop
// compute state in HM_Check 7
byte base64(AAAAAAAAAAc=)
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
// compute state in HM_Set 9
byte base64(AAAAAAAAAAk=)
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
l11:
// Handler 7
dup
int 7
==
bz l12
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
// compute state in HM_Check 7
byte base64(AAAAAAAAAAc=)
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
bz l13
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
l13:
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
l14:
pop
global ZeroAddress
store 1
txn OnCompletion
int DeleteApplication
==
assert
b updateState
l12:
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
int 8
==
assert
dup
btoi
store 248
pop
// compute state in HM_Check 9
byte base64(AAAAAAAAAAk=)
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
// compute state in HM_Set 11
byte base64(AAAAAAAAAAs=)
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
l15:
// Handler 9
dup
int 9
==
bz l16
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
// compute state in HM_Check 9
byte base64(AAAAAAAAAAk=)
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
bz l17
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
l17:
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
l18:
pop
global ZeroAddress
store 1
txn OnCompletion
int DeleteApplication
==
assert
b updateState
l16:
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
// compute state in HM_Check 11
byte base64(AAAAAAAAAAs=)
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
l19:
// Handler 11
dup
int 11
==
bz l20
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
// compute state in HM_Check 11
byte base64(AAAAAAAAAAs=)
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
bz l21
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
l21:
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
l22:
pop
global ZeroAddress
store 1
txn OnCompletion
int DeleteApplication
==
assert
b updateState
l20:
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
bz l23
load 254
load 250
+
store 248
// compute state in HM_Set 7
byte base64(AAAAAAAAAAc=)
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
l23:
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
bz l24
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
l24:
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
l25:
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
            "internalType": "bool",
            "name": "msg",
            "type": "bool"
          }
        ],
        "internalType": "struct T1",
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
            "internalType": "bool",
            "name": "msg",
            "type": "bool"
          }
        ],
        "indexed": false,
        "internalType": "struct T1",
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
            "internalType": "bool",
            "name": "svs",
            "type": "bool"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v266",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v267",
                "type": "uint256"
              }
            ],
            "internalType": "struct T3",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct T4",
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
                "name": "v265",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v266",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v267",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v277",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v289",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v308",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v320",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v326",
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
                "name": "v331",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v332",
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
                "name": "v265",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v266",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v267",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v277",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v289",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v308",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v320",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v326",
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
                "name": "v265",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v266",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v267",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v273",
                "type": "uint256"
              }
            ],
            "internalType": "struct T2",
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
                "name": "v265",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v266",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v267",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v273",
                "type": "uint256"
              }
            ],
            "internalType": "struct T2",
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
                "name": "v265",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v266",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v267",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v277",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v289",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v299",
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
                "name": "v308",
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
                "name": "v265",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v266",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v267",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v277",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v289",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v299",
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
                "name": "v265",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v266",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v267",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v277",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v289",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v308",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v314",
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
                "name": "v320",
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
                "name": "v265",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v266",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v267",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v277",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v289",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v308",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v314",
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
            "internalType": "bool",
            "name": "svs",
            "type": "bool"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v266",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v267",
                "type": "uint256"
              }
            ],
            "internalType": "struct T3",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "internalType": "struct T4",
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
                "name": "v265",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v266",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v267",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v277",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v289",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v308",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v320",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v326",
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
                "name": "v331",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v332",
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
                "name": "v265",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v266",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v267",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v277",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v289",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v308",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v320",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v326",
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
                "name": "v265",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v266",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v267",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v273",
                "type": "uint256"
              }
            ],
            "internalType": "struct T2",
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
                "name": "v265",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v266",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v267",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v273",
                "type": "uint256"
              }
            ],
            "internalType": "struct T2",
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
                "name": "v265",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v266",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v267",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v277",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v289",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v299",
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
                "name": "v308",
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
                "name": "v265",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v266",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v267",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v277",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v289",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v299",
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
                "name": "v265",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v266",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v267",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v277",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v289",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v308",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v314",
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
                "name": "v320",
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
                "name": "v265",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v266",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v267",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v277",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v289",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v308",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v314",
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
  Bytecode: `0x60806040526040516200151a3803806200151a833981016040819052620000269162000105565b6000546200003790156008620000c5565b6001600055604080518251151581526020808401511515908201527fee65f2dcf8be6cdab3bac48c73b8a6524225820fd17a0a77f34c81e47ebe406e910160405180910390a16200008b34156007620000c5565b6040805160016020820152600091810182905260600160408051601f19818403018152919052805160209091012060005550620001739050565b81620000eb5760405163100960cb60e01b81526004810182905260240160405180910390fd5b5050565b805180151581146200010057600080fd5b919050565b6000604082840312156200011857600080fd5b604080519081016001600160401b03811182821017156200014957634e487b7160e01b600052604160045260246000fd5b6040526200015783620000ef565b81526200016760208401620000ef565b60208201529392505050565b61139780620001836000396000f3fe60806040526004361061008a5760003560e01c806360bafff71161005957806360bafff7146100e457806361ff86f7146100f7578063aa8594ca1461010a578063e4f83a8f1461011d578063ebc5780c1461013057600080fd5b8063108acff51461009657806338015b16146100ab5780633df7a789146100be578063497ccc6a146100d157600080fd5b3661009157005b600080fd5b6100a96100a4366004610ff1565b610143565b005b6100a96100b9366004610fcc565b61024b565b6100a96100cc366004610f9c565b610351565b6100a96100df366004610f9c565b610575565b6100a96100f2366004610fb9565b61063a565b6100a9610105366004610f80565b6107ea565b6100a9610118366004610fdf565b6108a5565b6100a961012b366004610ff1565b6109d1565b6100a961013e366004610f80565b610adb565b60405161017f9061015b9060029084906020016112b3565b6040516020818303038152906040528051906020012060001c600054146010610cb0565b600160005561019660608201354310156011610cb0565b7f613b30050768160fb8fb1fedba26a4639c7df8d370861f403061c2d46f9802e5816040516101c59190611262565b60405180910390a16101d93415600e610cb0565b6101fb336101ea6020840184610f43565b6001600160a01b031614600f610cb0565b6102086020820182610f43565b6040516001600160a01b039190911690602083013580156108fc02916000818181858888f19350505050158015610243573d6000803e3d6000fd5b506000805533ff5b6040516102879061026390600b90849060200161129e565b6040516020818303038152906040528051906020012060001c600054146029610cb0565b600160005561029e60e0820135431015602a610cb0565b7fb9d135d4afaa7938b4616c3637968c0e71f66413b54043da6188989930963f89816040516102cd9190611207565b60405180910390a16102e134156027610cb0565b610306336102f56080840160608501610f43565b6001600160a01b0316146028610cb0565b6103166080820160608301610f43565b6040516001600160a01b039190911690608083013580156108fc02916000818181858888f19350505050158015610243573d6000803e3d6000fd5b60405161038d90610369906009908490602001611289565b6040516020818303038152906040528051906020012060001c60005414601c610cb0565b6001600090815560408051602081019091529081526103b360c08301354310601d610cb0565b7f6739bb4acbb3812eea51c48895245b154776bf02dd61571666f6abba93266fec826040516103e29190611196565b60405180910390a16103f63415601a610cb0565b61041b3361040a6080850160608601610f43565b6001600160a01b031614601b610cb0565b6104296040830135436112db565b8152604080516101008101825260008082526020808301829052928201819052606082018190526080820181905260a0820181905260c0820181905260e08201529061047790840184610f43565b6001600160a01b0316815260208084013590820152604080840135908201526104a66080840160608501610f43565b6001600160a01b031660608201526080808401359082015260a0808401359082015260e08084013560c083015282519082015260405161055590600b90839060200160006101208201905083825260018060a01b03808451166020840152602084015160408401526040840151606084015280606085015116608084015250608083015160a083015260a083015160c083015260c083015160e083015260e08301516101008301529392505050565b60408051601f198184030181529190528051602090910120600055505050565b6040516105b19061058d906009908490602001611289565b6040516020818303038152906040528051906020012060001c600054146020610cb0565b60016000556105c860c08201354310156021610cb0565b7fcdae4cbd433f8c4039f23f2632824e3ab0089e9b8c2050e8e87e4d6e0a3df09b816040516105f791906111b3565b60405180910390a161060b3415601e610cb0565b61062d3361061c6020840184610f43565b6001600160a01b031614601f610cb0565b6103166020820182610f43565b6040516106769061065290600b90849060200161129e565b6040516020818303038152906040528051906020012060001c600054146025610cb0565b600160005561068c60e082013543106026610cb0565b7f58822179cfd9cab18ada288d4f94f3d5babb851e53b3ed667629d3cbbbb0cb72816040516106bb91906111db565b60405180910390a16106cf34156022610cb0565b6106f1336106e06020840184610f43565b6001600160a01b0316146023610cb0565b604080516101008301356020820152610120830135918101919091526107399060600160408051601f19818403018152919052805160209091012060a0830135146024610cb0565b610741610e93565b61074e6020830183610f43565b81516001600160a01b039091169052805160208084013591015280516040808401359101526107836080830160608401610f43565b81516001600160a01b0390911660609091015260036107a760c08401356004611312565b6107b6906101208501356112db565b6107c09190611329565b60208083018051929092528151439101525160808301356040909101526107e681610cd5565b5050565b604051610826906108029060079084906020016112c7565b6040516020818303038152906040528051906020012060001c600054146018610cb0565b600160005561083d60a08201354310156019610cb0565b7fcc997a9af4abe95cf593cbeb34368171b4d5923d8562b1e54e51006451978b7c8160405161086c919061116f565b60405180910390a161088034156016610cb0565b610306336108946080840160608501610f43565b6001600160a01b0316146017610cb0565b6108f760016108b76020840184610f65565b6040516020016108d39291909182521515602082015260400190565b6040516020818303038152906040528051906020012060001c60005414600a610cb0565b6001600090815560408051602081018252918252517f3c669845d6bbc6fc367c9fa11ea5c8ec9bfd3d70eae7b34375b93f114f74036590610939908490611231565b60405180910390a1610952346020840135146009610cb0565b6109606040830135436112db565b81526040805160808082018352600060608084019182523384526020878101358186019081528887013586880190815288518552875160029381019390935286516001600160a01b03169783019790975251918101919091529351918401919091525160a08301529060c001610555565b604051610a0d906109e99060029084906020016112b3565b6040516020818303038152906040528051906020012060001c60005414600c610cb0565b6001600055610a2360608201354310600d610cb0565b7fe2fcb5361608dd42d825c4e917fd4fca89057bb8eb0b7e34b8c2813a114cc15281604051610a529190611262565b60405180910390a1610a6b34602083013514600b610cb0565b610a73610e93565b610a806020830183610f43565b81516001600160a01b039091169052805160208084013591810182905282516040808601359101528251336060909101528083018051600190525143910152610ac990806112db565b6020820151604001526107e681610cd5565b604051610b1790610af39060079084906020016112c7565b6040516020818303038152906040528051906020012060001c600054146014610cb0565b600160009081556040805160208101909152908152610b3d60a083013543106015610cb0565b7fee6496cc7d28a7c9121e0eebf62951277792d31c40832817d1a4c372e06eb46182604051610b6c9190611153565b60405180910390a1610b8034156012610cb0565b610ba233610b916020850185610f43565b6001600160a01b0316146013610cb0565b610bb06040830135436112db565b81526040805160e08101825260008082526020808301829052928201819052606082018190526080820181905260a0820181905260c082015290610bf690840184610f43565b6001600160a01b031681526020808401359082015260408084013590820152610c256080840160608501610f43565b6001600160a01b03908116606083810191825260808681013581860190815260c08089013560a08089019182528951838a019081526040805160096020808301919091528c518c16828401528c0151988101989098528a0151958701959095529551909616948401949094525192820192909252915160e08301525161010082015261012001610555565b816107e65760405163100960cb60e01b81526004810182905260240160405180910390fd5b60408051602081019091526000815260208201515160011415610dc0578151604001516020808401510151610d0a91906112db565b81526040805160c08082018352600080835260208084018281528486018381526060808701858152608080890187815260a0808b019889528d51516001600160a01b039081168c528e5189015188528e518d015187528e51860151811685528e8901518d015183528d518a528c516007998101999099528b5181169c89019c909c529551938701939093529251928501929092529051909616908201529351918401919091525160e08301529061010001610555565b6040805160c081018252600091810182815260608083018481526080840185815260a085018681528486526020808701979097528851516001600160a01b03908116909552885187015190925287519092015190921690529184015151909152610e2981610e2e565b505050565b805160600151600214610e4657805160400151610e4a565b8051515b6001600160a01b03166108fc8260000151602001516002610e6b91906112f3565b6040518115909202916000818181858888f19350505050158015610243573d6000803e3d6000fd5b6040805160c0810182526000918101828152606082018390526080820183905260a08201929092529081908152602001610ee760405180606001604052806000815260200160008152602001600081525090565b905290565b80356001600160a01b0381168114610f0357600080fd5b919050565b80358015158114610f0357600080fd5b600060e08284031215610f2a57600080fd5b50919050565b60006101008284031215610f2a57600080fd5b600060208284031215610f5557600080fd5b610f5e82610eec565b9392505050565b600060208284031215610f7757600080fd5b610f5e82610f08565b600060e08284031215610f9257600080fd5b610f5e8383610f18565b60006101008284031215610faf57600080fd5b610f5e8383610f30565b60006101408284031215610f2a57600080fd5b60006101208284031215610f2a57600080fd5b600060608284031215610f2a57600080fd5b600060a08284031215610f2a57600080fd5b6001600160a01b038061101583610eec565b16835260208201356020840152604082013560408401528061103960608401610eec565b166060840152506080810135608083015260a081013560a083015260c081013560c08301525050565b6001600160a01b038061107483610eec565b16835260208201356020840152604082013560408401528061109860608401610eec565b166060840152506080810135608083015260a081013560a083015260c081013560c083015260e081013560e08301525050565b6001600160a01b036110dc82610eec565b1682526020810135602083015260408101356040830152606081013560608301525050565b6001600160a01b038061111383610eec565b16835260208201356020840152604082013560408401528061113760608401610eec565b166060840152506080818101359083015260a090810135910152565b60e081016111618284611101565b60c092830135919092015290565b60e0810161117d8284611101565b61118960c08401610f08565b151560c083015292915050565b61010081016111a58284611003565b60e092830135919092015290565b61010081016111c28284611003565b6111ce60e08401610f08565b151560e083015292915050565b61014081016111ea8284611062565b610100838101358382015261012080850135908401525092915050565b61012081016112168284611062565b610100611224818501610f08565b1515818401525092915050565b6060810161123e83610f08565b1515825261125c602083016020850180358252602090810135910152565b92915050565b60a0810161127082846110cb565b61127c60808401610f08565b1515608083015292915050565b8281526101008101610f5e6020830184611003565b8281526101208101610f5e6020830184611062565b82815260a08101610f5e60208301846110cb565b82815260e08101610f5e6020830184611101565b600082198211156112ee576112ee61134b565b500190565b600081600019048311821515161561130d5761130d61134b565b500290565b6000828210156113245761132461134b565b500390565b60008261134657634e487b7160e01b600052601260045260246000fd5b500690565b634e487b7160e01b600052601160045260246000fdfea2646970667358221220e1fa923f00a601cd6d15c1a3530d2bf0d0a3b16d281550bcdb5f6ea561d6004d64736f6c63430008070033`,
  BytecodeLen: 5402,
  Which: `oD`,
  version: 2,
  views: {
    }
  };

export const _Connectors = {
  ALGO: _ALGO,
  ETH: _ETH
  };

