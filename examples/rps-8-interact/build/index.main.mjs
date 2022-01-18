// Automatically generated with Reach 0.1.8 (f52ff8c4*)
/* eslint-disable */
export const _version = '0.1.8';
export const _versionHash = '0.1.8 (f52ff8c4*)';
export const _backendVersion = 8;

export function getExports(s) {
  const stdlib = s.reachStdlib;
  return {
    };
  };
export function _getEvents(s) {
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
  
  
  const v299 = stdlib.protect(ctc0, interact.deadline, 'for Alice\'s interact field deadline');
  const v300 = stdlib.protect(ctc0, interact.wager, 'for Alice\'s interact field wager');
  
  const txn1 = await (ctc.sendrecv({
    args: [v300, v299],
    evt_cnt: 2,
    funcNum: 0,
    lct: stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:49:9:dot', stdlib.UInt_max, 0),
    onlyIf: true,
    out_tys: [ctc0, ctc0],
    pay: [v300, []],
    sim_p: (async (txn1) => {
      const sim_r = { txns: [], mapRefs: [], maps: [] };
      
      const {data: [v304, v305], secs: v307, time: v306, didSend: v56, from: v303 } = txn1;
      
      sim_r.txns.push({
        amt: v304,
        kind: 'to',
        tok: undefined
        });
      const v316 = stdlib.add(v306, v305);
      sim_r.isHalt = false;
      
      return sim_r;
      }),
    soloSend: true,
    timeoutAt: undefined,
    tys: [ctc0, ctc0],
    waitIfNotPresent: false
    }));
  const {data: [v304, v305], secs: v307, time: v306, didSend: v56, from: v303 } = txn1;
  ;
  const v316 = stdlib.add(v306, v305);
  const txn2 = await (ctc.recv({
    didSend: false,
    evt_cnt: 0,
    funcNum: 1,
    out_tys: [],
    timeoutAt: ['time', v316],
    waitIfNotPresent: false
    }));
  if (txn2.didTimeout) {
    const txn3 = await (ctc.sendrecv({
      args: [v303, v304, v305, v316],
      evt_cnt: 0,
      funcNum: 2,
      lct: v306,
      onlyIf: true,
      out_tys: [],
      pay: [stdlib.checkedBigNumberify('reach standard library:202:11:decimal', stdlib.UInt_max, 0), []],
      sim_p: (async (txn3) => {
        const sim_r = { txns: [], mapRefs: [], maps: [] };
        
        const {data: [], secs: v475, time: v474, didSend: v260, from: v473 } = txn3;
        
        sim_r.txns.push({
          amt: stdlib.checkedBigNumberify('reach standard library:202:11:decimal', stdlib.UInt_max, 0),
          kind: 'to',
          tok: undefined
          });
        sim_r.txns.push({
          amt: v304,
          kind: 'from',
          to: v303,
          tok: undefined
          });
        sim_r.txns.push({
          kind: 'halt',
          tok: undefined
          })
        sim_r.isHalt = true;
        
        return sim_r;
        }),
      soloSend: false,
      timeoutAt: undefined,
      tys: [ctc4, ctc0, ctc0, ctc0],
      waitIfNotPresent: false
      }));
    const {data: [], secs: v475, time: v474, didSend: v260, from: v473 } = txn3;
    ;
    ;
    stdlib.protect(ctc3, await interact.informTimeout(), {
      at: './examples/rps-8-interact/index.rsh:41:29:application',
      fs: ['at ./examples/rps-8-interact/index.rsh:40:9:application call to [unknown function] (defined at: ./examples/rps-8-interact/index.rsh:40:27:function exp)', 'at reach standard library:205:8:application call to "after" (defined at: ./examples/rps-8-interact/index.rsh:39:28:function exp)', 'at ./examples/rps-8-interact/index.rsh:57:51:application call to "closeTo" (defined at: reach standard library:200:8:function exp)'],
      msg: 'informTimeout',
      who: 'Alice'
      });
    
    return;
    
    }
  else {
    const {data: [], secs: v322, time: v321, didSend: v65, from: v320 } = txn2;
    const v324 = stdlib.add(v304, v304);
    ;
    let v325 = stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:makeEnum', stdlib.UInt_max, 1);
    let v326 = v321;
    let v332 = v324;
    
    while (await (async () => {
      const v340 = stdlib.eq(v325, stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:makeEnum', stdlib.UInt_max, 1));
      
      return v340;})()) {
      const v347 = stdlib.add(v326, v305);
      const v351 = stdlib.protect(ctc0, await interact.getHand(), {
        at: './examples/rps-8-interact/index.rsh:65:42:application',
        fs: ['at ./examples/rps-8-interact/index.rsh:64:15:application call to [unknown function] (defined at: ./examples/rps-8-interact/index.rsh:64:19:function exp)'],
        msg: 'getHand',
        who: 'Alice'
        });
      const v352 = stdlib.protect(ctc0, await interact.random(), {
        at: 'reach standard library:53:31:application',
        fs: ['at ./examples/rps-8-interact/index.rsh:66:56:application call to "makeCommitment" (defined at: reach standard library:52:8:function exp)', 'at ./examples/rps-8-interact/index.rsh:64:15:application call to [unknown function] (defined at: ./examples/rps-8-interact/index.rsh:64:19:function exp)'],
        msg: 'random',
        who: 'Alice'
        });
      const v353 = stdlib.digest(ctc1, [v352, v351]);
      
      const txn3 = await (ctc.sendrecv({
        args: [v303, v304, v305, v320, v332, v347, v353],
        evt_cnt: 1,
        funcNum: 4,
        lct: v326,
        onlyIf: true,
        out_tys: [ctc2],
        pay: [stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:69:11:decimal', stdlib.UInt_max, 0), []],
        sim_p: (async (txn3) => {
          const sim_r = { txns: [], mapRefs: [], maps: [] };
          
          const {data: [v356], secs: v358, time: v357, didSend: v92, from: v355 } = txn3;
          
          sim_r.txns.push({
            amt: stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:69:11:decimal', stdlib.UInt_max, 0),
            kind: 'to',
            tok: undefined
            });
          const v360 = stdlib.addressEq(v303, v355);
          stdlib.assert(v360, {
            at: './examples/rps-8-interact/index.rsh:69:11:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Alice'
            });
          const v367 = stdlib.add(v357, v305);
          sim_r.isHalt = false;
          
          return sim_r;
          }),
        soloSend: true,
        timeoutAt: ['time', v347],
        tys: [ctc4, ctc0, ctc0, ctc4, ctc0, ctc0, ctc2],
        waitIfNotPresent: false
        }));
      if (txn3.didTimeout) {
        const txn4 = await (ctc.sendrecv({
          args: [v303, v304, v305, v320, v332, v347],
          evt_cnt: 0,
          funcNum: 5,
          lct: v326,
          onlyIf: true,
          out_tys: [],
          pay: [stdlib.checkedBigNumberify('reach standard library:202:11:decimal', stdlib.UInt_max, 0), []],
          sim_p: (async (txn4) => {
            const sim_r = { txns: [], mapRefs: [], maps: [] };
            
            const {data: [], secs: v440, time: v439, didSend: v212, from: v438 } = txn4;
            
            sim_r.txns.push({
              amt: stdlib.checkedBigNumberify('reach standard library:202:11:decimal', stdlib.UInt_max, 0),
              kind: 'to',
              tok: undefined
              });
            const v442 = stdlib.addressEq(v303, v438);
            const v443 = stdlib.addressEq(v320, v438);
            const v444 = v442 ? true : v443;
            stdlib.assert(v444, {
              at: 'reach standard library:202:11:dot',
              fs: ['at ./examples/rps-8-interact/index.rsh:70:53:application call to "closeTo" (defined at: reach standard library:200:8:function exp)'],
              msg: 'sender correct',
              who: 'Alice'
              });
            sim_r.txns.push({
              amt: v332,
              kind: 'from',
              to: v320,
              tok: undefined
              });
            sim_r.txns.push({
              kind: 'halt',
              tok: undefined
              })
            sim_r.isHalt = true;
            
            return sim_r;
            }),
          soloSend: false,
          timeoutAt: undefined,
          tys: [ctc4, ctc0, ctc0, ctc4, ctc0, ctc0],
          waitIfNotPresent: false
          }));
        const {data: [], secs: v440, time: v439, didSend: v212, from: v438 } = txn4;
        ;
        const v442 = stdlib.addressEq(v303, v438);
        const v443 = stdlib.addressEq(v320, v438);
        const v444 = v442 ? true : v443;
        stdlib.assert(v444, {
          at: 'reach standard library:202:11:dot',
          fs: ['at ./examples/rps-8-interact/index.rsh:70:53:application call to "closeTo" (defined at: reach standard library:200:8:function exp)'],
          msg: 'sender correct',
          who: 'Alice'
          });
        ;
        stdlib.protect(ctc3, await interact.informTimeout(), {
          at: './examples/rps-8-interact/index.rsh:41:29:application',
          fs: ['at ./examples/rps-8-interact/index.rsh:40:9:application call to [unknown function] (defined at: ./examples/rps-8-interact/index.rsh:40:27:function exp)', 'at reach standard library:205:8:application call to "after" (defined at: ./examples/rps-8-interact/index.rsh:39:28:function exp)', 'at ./examples/rps-8-interact/index.rsh:70:53:application call to "closeTo" (defined at: reach standard library:200:8:function exp)'],
          msg: 'informTimeout',
          who: 'Alice'
          });
        
        return;
        
        }
      else {
        const {data: [v356], secs: v358, time: v357, didSend: v92, from: v355 } = txn3;
        ;
        const v360 = stdlib.addressEq(v303, v355);
        stdlib.assert(v360, {
          at: './examples/rps-8-interact/index.rsh:69:11:dot',
          fs: [],
          msg: 'sender correct',
          who: 'Alice'
          });
        const v367 = stdlib.add(v357, v305);
        const txn4 = await (ctc.recv({
          didSend: false,
          evt_cnt: 1,
          funcNum: 6,
          out_tys: [ctc0],
          timeoutAt: ['time', v367],
          waitIfNotPresent: false
          }));
        if (txn4.didTimeout) {
          const txn5 = await (ctc.sendrecv({
            args: [v303, v304, v305, v320, v332, v356, v367],
            evt_cnt: 0,
            funcNum: 7,
            lct: v357,
            onlyIf: true,
            out_tys: [],
            pay: [stdlib.checkedBigNumberify('reach standard library:202:11:decimal', stdlib.UInt_max, 0), []],
            sim_p: (async (txn5) => {
              const sim_r = { txns: [], mapRefs: [], maps: [] };
              
              const {data: [], secs: v421, time: v420, didSend: v177, from: v419 } = txn5;
              
              sim_r.txns.push({
                amt: stdlib.checkedBigNumberify('reach standard library:202:11:decimal', stdlib.UInt_max, 0),
                kind: 'to',
                tok: undefined
                });
              const v423 = stdlib.addressEq(v303, v419);
              const v424 = stdlib.addressEq(v320, v419);
              const v425 = v423 ? true : v424;
              stdlib.assert(v425, {
                at: 'reach standard library:202:11:dot',
                fs: ['at ./examples/rps-8-interact/index.rsh:78:53:application call to "closeTo" (defined at: reach standard library:200:8:function exp)'],
                msg: 'sender correct',
                who: 'Alice'
                });
              sim_r.txns.push({
                amt: v332,
                kind: 'from',
                to: v303,
                tok: undefined
                });
              sim_r.txns.push({
                kind: 'halt',
                tok: undefined
                })
              sim_r.isHalt = true;
              
              return sim_r;
              }),
            soloSend: false,
            timeoutAt: undefined,
            tys: [ctc4, ctc0, ctc0, ctc4, ctc0, ctc2, ctc0],
            waitIfNotPresent: false
            }));
          const {data: [], secs: v421, time: v420, didSend: v177, from: v419 } = txn5;
          ;
          const v423 = stdlib.addressEq(v303, v419);
          const v424 = stdlib.addressEq(v320, v419);
          const v425 = v423 ? true : v424;
          stdlib.assert(v425, {
            at: 'reach standard library:202:11:dot',
            fs: ['at ./examples/rps-8-interact/index.rsh:78:53:application call to "closeTo" (defined at: reach standard library:200:8:function exp)'],
            msg: 'sender correct',
            who: 'Alice'
            });
          ;
          stdlib.protect(ctc3, await interact.informTimeout(), {
            at: './examples/rps-8-interact/index.rsh:41:29:application',
            fs: ['at ./examples/rps-8-interact/index.rsh:40:9:application call to [unknown function] (defined at: ./examples/rps-8-interact/index.rsh:40:27:function exp)', 'at reach standard library:205:8:application call to "after" (defined at: ./examples/rps-8-interact/index.rsh:39:28:function exp)', 'at ./examples/rps-8-interact/index.rsh:78:53:application call to "closeTo" (defined at: reach standard library:200:8:function exp)'],
            msg: 'informTimeout',
            who: 'Alice'
            });
          
          return;
          
          }
        else {
          const {data: [v373], secs: v375, time: v374, didSend: v103, from: v372 } = txn4;
          ;
          const v377 = stdlib.addressEq(v320, v372);
          stdlib.assert(v377, {
            at: './examples/rps-8-interact/index.rsh:77:9:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Alice'
            });
          const v384 = stdlib.add(v374, v305);
          const txn5 = await (ctc.sendrecv({
            args: [v303, v304, v305, v320, v332, v356, v373, v384, v352, v351],
            evt_cnt: 2,
            funcNum: 8,
            lct: v374,
            onlyIf: true,
            out_tys: [ctc0, ctc0],
            pay: [stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:85:11:decimal', stdlib.UInt_max, 0), []],
            sim_p: (async (txn5) => {
              const sim_r = { txns: [], mapRefs: [], maps: [] };
              
              const {data: [v389, v390], secs: v392, time: v391, didSend: v114, from: v388 } = txn5;
              
              sim_r.txns.push({
                amt: stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:85:11:decimal', stdlib.UInt_max, 0),
                kind: 'to',
                tok: undefined
                });
              const v394 = stdlib.addressEq(v303, v388);
              stdlib.assert(v394, {
                at: './examples/rps-8-interact/index.rsh:85:11:dot',
                fs: [],
                msg: 'sender correct',
                who: 'Alice'
                });
              const v395 = stdlib.digest(ctc1, [v389, v390]);
              const v396 = stdlib.digestEq(v356, v395);
              stdlib.assert(v396, {
                at: 'reach standard library:58:17:application',
                fs: ['at ./examples/rps-8-interact/index.rsh:87:20:application call to "checkCommitment" (defined at: reach standard library:57:8:function exp)'],
                msg: null,
                who: 'Alice'
                });
              const v397 = stdlib.sub(stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:7:18:decimal', stdlib.UInt_max, 4), v373);
              const v398 = stdlib.add(v390, v397);
              const v399 = stdlib.mod(v398, stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:7:34:decimal', stdlib.UInt_max, 3));
              const cv325 = v399;
              const cv326 = v391;
              const cv332 = v332;
              
              await (async () => {
                const v325 = cv325;
                const v326 = cv326;
                const v332 = cv332;
                
                if (await (async () => {
                  const v340 = stdlib.eq(v325, stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:makeEnum', stdlib.UInt_max, 1));
                  
                  return v340;})()) {
                  const v347 = stdlib.add(v326, v305);
                  sim_r.isHalt = false;
                  }
                else {
                  const v457 = stdlib.eq(v325, stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:makeEnum', stdlib.UInt_max, 2));
                  const v460 = stdlib.mul(stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:94:12:decimal', stdlib.UInt_max, 2), v304);
                  const v462 = v457 ? v303 : v320;
                  sim_r.txns.push({
                    amt: v460,
                    kind: 'from',
                    to: v462,
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
            timeoutAt: ['time', v384],
            tys: [ctc4, ctc0, ctc0, ctc4, ctc0, ctc2, ctc0, ctc0, ctc0, ctc0],
            waitIfNotPresent: false
            }));
          if (txn5.didTimeout) {
            const txn6 = await (ctc.sendrecv({
              args: [v303, v304, v305, v320, v332, v356, v373, v384],
              evt_cnt: 0,
              funcNum: 9,
              lct: v374,
              onlyIf: true,
              out_tys: [],
              pay: [stdlib.checkedBigNumberify('reach standard library:202:11:decimal', stdlib.UInt_max, 0), []],
              sim_p: (async (txn6) => {
                const sim_r = { txns: [], mapRefs: [], maps: [] };
                
                const {data: [], secs: v402, time: v401, didSend: v142, from: v400 } = txn6;
                
                sim_r.txns.push({
                  amt: stdlib.checkedBigNumberify('reach standard library:202:11:decimal', stdlib.UInt_max, 0),
                  kind: 'to',
                  tok: undefined
                  });
                const v404 = stdlib.addressEq(v303, v400);
                const v405 = stdlib.addressEq(v320, v400);
                const v406 = v404 ? true : v405;
                stdlib.assert(v406, {
                  at: 'reach standard library:202:11:dot',
                  fs: ['at ./examples/rps-8-interact/index.rsh:86:53:application call to "closeTo" (defined at: reach standard library:200:8:function exp)'],
                  msg: 'sender correct',
                  who: 'Alice'
                  });
                sim_r.txns.push({
                  amt: v332,
                  kind: 'from',
                  to: v320,
                  tok: undefined
                  });
                sim_r.txns.push({
                  kind: 'halt',
                  tok: undefined
                  })
                sim_r.isHalt = true;
                
                return sim_r;
                }),
              soloSend: false,
              timeoutAt: undefined,
              tys: [ctc4, ctc0, ctc0, ctc4, ctc0, ctc2, ctc0, ctc0],
              waitIfNotPresent: false
              }));
            const {data: [], secs: v402, time: v401, didSend: v142, from: v400 } = txn6;
            ;
            const v404 = stdlib.addressEq(v303, v400);
            const v405 = stdlib.addressEq(v320, v400);
            const v406 = v404 ? true : v405;
            stdlib.assert(v406, {
              at: 'reach standard library:202:11:dot',
              fs: ['at ./examples/rps-8-interact/index.rsh:86:53:application call to "closeTo" (defined at: reach standard library:200:8:function exp)'],
              msg: 'sender correct',
              who: 'Alice'
              });
            ;
            stdlib.protect(ctc3, await interact.informTimeout(), {
              at: './examples/rps-8-interact/index.rsh:41:29:application',
              fs: ['at ./examples/rps-8-interact/index.rsh:40:9:application call to [unknown function] (defined at: ./examples/rps-8-interact/index.rsh:40:27:function exp)', 'at reach standard library:205:8:application call to "after" (defined at: ./examples/rps-8-interact/index.rsh:39:28:function exp)', 'at ./examples/rps-8-interact/index.rsh:86:53:application call to "closeTo" (defined at: reach standard library:200:8:function exp)'],
              msg: 'informTimeout',
              who: 'Alice'
              });
            
            return;
            
            }
          else {
            const {data: [v389, v390], secs: v392, time: v391, didSend: v114, from: v388 } = txn5;
            ;
            const v394 = stdlib.addressEq(v303, v388);
            stdlib.assert(v394, {
              at: './examples/rps-8-interact/index.rsh:85:11:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Alice'
              });
            const v395 = stdlib.digest(ctc1, [v389, v390]);
            const v396 = stdlib.digestEq(v356, v395);
            stdlib.assert(v396, {
              at: 'reach standard library:58:17:application',
              fs: ['at ./examples/rps-8-interact/index.rsh:87:20:application call to "checkCommitment" (defined at: reach standard library:57:8:function exp)'],
              msg: null,
              who: 'Alice'
              });
            const v397 = stdlib.sub(stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:7:18:decimal', stdlib.UInt_max, 4), v373);
            const v398 = stdlib.add(v390, v397);
            const v399 = stdlib.mod(v398, stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:7:34:decimal', stdlib.UInt_max, 3));
            const cv325 = v399;
            const cv326 = v391;
            const cv332 = v332;
            
            v325 = cv325;
            v326 = cv326;
            v332 = cv332;
            
            continue;}
          
          }
        
        }
      
      }
    const v457 = stdlib.eq(v325, stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:makeEnum', stdlib.UInt_max, 2));
    const v460 = stdlib.mul(stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:94:12:decimal', stdlib.UInt_max, 2), v304);
    const v462 = v457 ? v303 : v320;
    ;
    stdlib.protect(ctc3, await interact.seeOutcome(v325), {
      at: './examples/rps-8-interact/index.rsh:98:24:application',
      fs: ['at ./examples/rps-8-interact/index.rsh:97:7:application call to [unknown function] (defined at: ./examples/rps-8-interact/index.rsh:97:25:function exp)'],
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
  const {data: [v304, v305], secs: v307, time: v306, didSend: v56, from: v303 } = txn1;
  ;
  const v316 = stdlib.add(v306, v305);
  stdlib.protect(ctc1, await interact.acceptWager(v304), {
    at: './examples/rps-8-interact/index.rsh:54:25:application',
    fs: ['at ./examples/rps-8-interact/index.rsh:53:11:application call to [unknown function] (defined at: ./examples/rps-8-interact/index.rsh:53:15:function exp)'],
    msg: 'acceptWager',
    who: 'Bob'
    });
  
  const txn2 = await (ctc.sendrecv({
    args: [v303, v304, v305, v316],
    evt_cnt: 0,
    funcNum: 1,
    lct: v306,
    onlyIf: true,
    out_tys: [],
    pay: [v304, []],
    sim_p: (async (txn2) => {
      const sim_r = { txns: [], mapRefs: [], maps: [] };
      
      const {data: [], secs: v322, time: v321, didSend: v65, from: v320 } = txn2;
      
      const v324 = stdlib.add(v304, v304);
      sim_r.txns.push({
        amt: v304,
        kind: 'to',
        tok: undefined
        });
      const v325 = stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:makeEnum', stdlib.UInt_max, 1);
      const v326 = v321;
      const v332 = v324;
      
      if (await (async () => {
        const v340 = stdlib.eq(v325, stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:makeEnum', stdlib.UInt_max, 1));
        
        return v340;})()) {
        const v347 = stdlib.add(v326, v305);
        sim_r.isHalt = false;
        }
      else {
        const v457 = stdlib.eq(v325, stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:makeEnum', stdlib.UInt_max, 2));
        const v460 = stdlib.mul(stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:94:12:decimal', stdlib.UInt_max, 2), v304);
        const v462 = v457 ? v303 : v320;
        sim_r.txns.push({
          amt: v460,
          kind: 'from',
          to: v462,
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
    timeoutAt: ['time', v316],
    tys: [ctc4, ctc0, ctc0, ctc0],
    waitIfNotPresent: false
    }));
  if (txn2.didTimeout) {
    const txn3 = await (ctc.sendrecv({
      args: [v303, v304, v305, v316],
      evt_cnt: 0,
      funcNum: 2,
      lct: v306,
      onlyIf: true,
      out_tys: [],
      pay: [stdlib.checkedBigNumberify('reach standard library:202:11:decimal', stdlib.UInt_max, 0), []],
      sim_p: (async (txn3) => {
        const sim_r = { txns: [], mapRefs: [], maps: [] };
        
        const {data: [], secs: v475, time: v474, didSend: v260, from: v473 } = txn3;
        
        sim_r.txns.push({
          amt: stdlib.checkedBigNumberify('reach standard library:202:11:decimal', stdlib.UInt_max, 0),
          kind: 'to',
          tok: undefined
          });
        sim_r.txns.push({
          amt: v304,
          kind: 'from',
          to: v303,
          tok: undefined
          });
        sim_r.txns.push({
          kind: 'halt',
          tok: undefined
          })
        sim_r.isHalt = true;
        
        return sim_r;
        }),
      soloSend: false,
      timeoutAt: undefined,
      tys: [ctc4, ctc0, ctc0, ctc0],
      waitIfNotPresent: false
      }));
    const {data: [], secs: v475, time: v474, didSend: v260, from: v473 } = txn3;
    ;
    ;
    stdlib.protect(ctc1, await interact.informTimeout(), {
      at: './examples/rps-8-interact/index.rsh:41:29:application',
      fs: ['at ./examples/rps-8-interact/index.rsh:40:9:application call to [unknown function] (defined at: ./examples/rps-8-interact/index.rsh:40:27:function exp)', 'at reach standard library:205:8:application call to "after" (defined at: ./examples/rps-8-interact/index.rsh:39:28:function exp)', 'at ./examples/rps-8-interact/index.rsh:57:51:application call to "closeTo" (defined at: reach standard library:200:8:function exp)'],
      msg: 'informTimeout',
      who: 'Bob'
      });
    
    return;
    
    }
  else {
    const {data: [], secs: v322, time: v321, didSend: v65, from: v320 } = txn2;
    const v324 = stdlib.add(v304, v304);
    ;
    let v325 = stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:makeEnum', stdlib.UInt_max, 1);
    let v326 = v321;
    let v332 = v324;
    
    while (await (async () => {
      const v340 = stdlib.eq(v325, stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:makeEnum', stdlib.UInt_max, 1));
      
      return v340;})()) {
      const v347 = stdlib.add(v326, v305);
      const txn3 = await (ctc.recv({
        didSend: false,
        evt_cnt: 1,
        funcNum: 4,
        out_tys: [ctc2],
        timeoutAt: ['time', v347],
        waitIfNotPresent: false
        }));
      if (txn3.didTimeout) {
        const txn4 = await (ctc.sendrecv({
          args: [v303, v304, v305, v320, v332, v347],
          evt_cnt: 0,
          funcNum: 5,
          lct: v326,
          onlyIf: true,
          out_tys: [],
          pay: [stdlib.checkedBigNumberify('reach standard library:202:11:decimal', stdlib.UInt_max, 0), []],
          sim_p: (async (txn4) => {
            const sim_r = { txns: [], mapRefs: [], maps: [] };
            
            const {data: [], secs: v440, time: v439, didSend: v212, from: v438 } = txn4;
            
            sim_r.txns.push({
              amt: stdlib.checkedBigNumberify('reach standard library:202:11:decimal', stdlib.UInt_max, 0),
              kind: 'to',
              tok: undefined
              });
            const v442 = stdlib.addressEq(v303, v438);
            const v443 = stdlib.addressEq(v320, v438);
            const v444 = v442 ? true : v443;
            stdlib.assert(v444, {
              at: 'reach standard library:202:11:dot',
              fs: ['at ./examples/rps-8-interact/index.rsh:70:53:application call to "closeTo" (defined at: reach standard library:200:8:function exp)'],
              msg: 'sender correct',
              who: 'Bob'
              });
            sim_r.txns.push({
              amt: v332,
              kind: 'from',
              to: v320,
              tok: undefined
              });
            sim_r.txns.push({
              kind: 'halt',
              tok: undefined
              })
            sim_r.isHalt = true;
            
            return sim_r;
            }),
          soloSend: false,
          timeoutAt: undefined,
          tys: [ctc4, ctc0, ctc0, ctc4, ctc0, ctc0],
          waitIfNotPresent: false
          }));
        const {data: [], secs: v440, time: v439, didSend: v212, from: v438 } = txn4;
        ;
        const v442 = stdlib.addressEq(v303, v438);
        const v443 = stdlib.addressEq(v320, v438);
        const v444 = v442 ? true : v443;
        stdlib.assert(v444, {
          at: 'reach standard library:202:11:dot',
          fs: ['at ./examples/rps-8-interact/index.rsh:70:53:application call to "closeTo" (defined at: reach standard library:200:8:function exp)'],
          msg: 'sender correct',
          who: 'Bob'
          });
        ;
        stdlib.protect(ctc1, await interact.informTimeout(), {
          at: './examples/rps-8-interact/index.rsh:41:29:application',
          fs: ['at ./examples/rps-8-interact/index.rsh:40:9:application call to [unknown function] (defined at: ./examples/rps-8-interact/index.rsh:40:27:function exp)', 'at reach standard library:205:8:application call to "after" (defined at: ./examples/rps-8-interact/index.rsh:39:28:function exp)', 'at ./examples/rps-8-interact/index.rsh:70:53:application call to "closeTo" (defined at: reach standard library:200:8:function exp)'],
          msg: 'informTimeout',
          who: 'Bob'
          });
        
        return;
        
        }
      else {
        const {data: [v356], secs: v358, time: v357, didSend: v92, from: v355 } = txn3;
        ;
        const v360 = stdlib.addressEq(v303, v355);
        stdlib.assert(v360, {
          at: './examples/rps-8-interact/index.rsh:69:11:dot',
          fs: [],
          msg: 'sender correct',
          who: 'Bob'
          });
        const v367 = stdlib.add(v357, v305);
        const v371 = stdlib.protect(ctc0, await interact.getHand(), {
          at: './examples/rps-8-interact/index.rsh:75:50:application',
          fs: ['at ./examples/rps-8-interact/index.rsh:74:13:application call to [unknown function] (defined at: ./examples/rps-8-interact/index.rsh:74:17:function exp)'],
          msg: 'getHand',
          who: 'Bob'
          });
        
        const txn4 = await (ctc.sendrecv({
          args: [v303, v304, v305, v320, v332, v356, v367, v371],
          evt_cnt: 1,
          funcNum: 6,
          lct: v357,
          onlyIf: true,
          out_tys: [ctc0],
          pay: [stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:77:9:decimal', stdlib.UInt_max, 0), []],
          sim_p: (async (txn4) => {
            const sim_r = { txns: [], mapRefs: [], maps: [] };
            
            const {data: [v373], secs: v375, time: v374, didSend: v103, from: v372 } = txn4;
            
            sim_r.txns.push({
              amt: stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:77:9:decimal', stdlib.UInt_max, 0),
              kind: 'to',
              tok: undefined
              });
            const v377 = stdlib.addressEq(v320, v372);
            stdlib.assert(v377, {
              at: './examples/rps-8-interact/index.rsh:77:9:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Bob'
              });
            const v384 = stdlib.add(v374, v305);
            sim_r.isHalt = false;
            
            return sim_r;
            }),
          soloSend: true,
          timeoutAt: ['time', v367],
          tys: [ctc4, ctc0, ctc0, ctc4, ctc0, ctc2, ctc0, ctc0],
          waitIfNotPresent: false
          }));
        if (txn4.didTimeout) {
          const txn5 = await (ctc.sendrecv({
            args: [v303, v304, v305, v320, v332, v356, v367],
            evt_cnt: 0,
            funcNum: 7,
            lct: v357,
            onlyIf: true,
            out_tys: [],
            pay: [stdlib.checkedBigNumberify('reach standard library:202:11:decimal', stdlib.UInt_max, 0), []],
            sim_p: (async (txn5) => {
              const sim_r = { txns: [], mapRefs: [], maps: [] };
              
              const {data: [], secs: v421, time: v420, didSend: v177, from: v419 } = txn5;
              
              sim_r.txns.push({
                amt: stdlib.checkedBigNumberify('reach standard library:202:11:decimal', stdlib.UInt_max, 0),
                kind: 'to',
                tok: undefined
                });
              const v423 = stdlib.addressEq(v303, v419);
              const v424 = stdlib.addressEq(v320, v419);
              const v425 = v423 ? true : v424;
              stdlib.assert(v425, {
                at: 'reach standard library:202:11:dot',
                fs: ['at ./examples/rps-8-interact/index.rsh:78:53:application call to "closeTo" (defined at: reach standard library:200:8:function exp)'],
                msg: 'sender correct',
                who: 'Bob'
                });
              sim_r.txns.push({
                amt: v332,
                kind: 'from',
                to: v303,
                tok: undefined
                });
              sim_r.txns.push({
                kind: 'halt',
                tok: undefined
                })
              sim_r.isHalt = true;
              
              return sim_r;
              }),
            soloSend: false,
            timeoutAt: undefined,
            tys: [ctc4, ctc0, ctc0, ctc4, ctc0, ctc2, ctc0],
            waitIfNotPresent: false
            }));
          const {data: [], secs: v421, time: v420, didSend: v177, from: v419 } = txn5;
          ;
          const v423 = stdlib.addressEq(v303, v419);
          const v424 = stdlib.addressEq(v320, v419);
          const v425 = v423 ? true : v424;
          stdlib.assert(v425, {
            at: 'reach standard library:202:11:dot',
            fs: ['at ./examples/rps-8-interact/index.rsh:78:53:application call to "closeTo" (defined at: reach standard library:200:8:function exp)'],
            msg: 'sender correct',
            who: 'Bob'
            });
          ;
          stdlib.protect(ctc1, await interact.informTimeout(), {
            at: './examples/rps-8-interact/index.rsh:41:29:application',
            fs: ['at ./examples/rps-8-interact/index.rsh:40:9:application call to [unknown function] (defined at: ./examples/rps-8-interact/index.rsh:40:27:function exp)', 'at reach standard library:205:8:application call to "after" (defined at: ./examples/rps-8-interact/index.rsh:39:28:function exp)', 'at ./examples/rps-8-interact/index.rsh:78:53:application call to "closeTo" (defined at: reach standard library:200:8:function exp)'],
            msg: 'informTimeout',
            who: 'Bob'
            });
          
          return;
          
          }
        else {
          const {data: [v373], secs: v375, time: v374, didSend: v103, from: v372 } = txn4;
          ;
          const v377 = stdlib.addressEq(v320, v372);
          stdlib.assert(v377, {
            at: './examples/rps-8-interact/index.rsh:77:9:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Bob'
            });
          const v384 = stdlib.add(v374, v305);
          const txn5 = await (ctc.recv({
            didSend: false,
            evt_cnt: 2,
            funcNum: 8,
            out_tys: [ctc0, ctc0],
            timeoutAt: ['time', v384],
            waitIfNotPresent: false
            }));
          if (txn5.didTimeout) {
            const txn6 = await (ctc.sendrecv({
              args: [v303, v304, v305, v320, v332, v356, v373, v384],
              evt_cnt: 0,
              funcNum: 9,
              lct: v374,
              onlyIf: true,
              out_tys: [],
              pay: [stdlib.checkedBigNumberify('reach standard library:202:11:decimal', stdlib.UInt_max, 0), []],
              sim_p: (async (txn6) => {
                const sim_r = { txns: [], mapRefs: [], maps: [] };
                
                const {data: [], secs: v402, time: v401, didSend: v142, from: v400 } = txn6;
                
                sim_r.txns.push({
                  amt: stdlib.checkedBigNumberify('reach standard library:202:11:decimal', stdlib.UInt_max, 0),
                  kind: 'to',
                  tok: undefined
                  });
                const v404 = stdlib.addressEq(v303, v400);
                const v405 = stdlib.addressEq(v320, v400);
                const v406 = v404 ? true : v405;
                stdlib.assert(v406, {
                  at: 'reach standard library:202:11:dot',
                  fs: ['at ./examples/rps-8-interact/index.rsh:86:53:application call to "closeTo" (defined at: reach standard library:200:8:function exp)'],
                  msg: 'sender correct',
                  who: 'Bob'
                  });
                sim_r.txns.push({
                  amt: v332,
                  kind: 'from',
                  to: v320,
                  tok: undefined
                  });
                sim_r.txns.push({
                  kind: 'halt',
                  tok: undefined
                  })
                sim_r.isHalt = true;
                
                return sim_r;
                }),
              soloSend: false,
              timeoutAt: undefined,
              tys: [ctc4, ctc0, ctc0, ctc4, ctc0, ctc2, ctc0, ctc0],
              waitIfNotPresent: false
              }));
            const {data: [], secs: v402, time: v401, didSend: v142, from: v400 } = txn6;
            ;
            const v404 = stdlib.addressEq(v303, v400);
            const v405 = stdlib.addressEq(v320, v400);
            const v406 = v404 ? true : v405;
            stdlib.assert(v406, {
              at: 'reach standard library:202:11:dot',
              fs: ['at ./examples/rps-8-interact/index.rsh:86:53:application call to "closeTo" (defined at: reach standard library:200:8:function exp)'],
              msg: 'sender correct',
              who: 'Bob'
              });
            ;
            stdlib.protect(ctc1, await interact.informTimeout(), {
              at: './examples/rps-8-interact/index.rsh:41:29:application',
              fs: ['at ./examples/rps-8-interact/index.rsh:40:9:application call to [unknown function] (defined at: ./examples/rps-8-interact/index.rsh:40:27:function exp)', 'at reach standard library:205:8:application call to "after" (defined at: ./examples/rps-8-interact/index.rsh:39:28:function exp)', 'at ./examples/rps-8-interact/index.rsh:86:53:application call to "closeTo" (defined at: reach standard library:200:8:function exp)'],
              msg: 'informTimeout',
              who: 'Bob'
              });
            
            return;
            
            }
          else {
            const {data: [v389, v390], secs: v392, time: v391, didSend: v114, from: v388 } = txn5;
            ;
            const v394 = stdlib.addressEq(v303, v388);
            stdlib.assert(v394, {
              at: './examples/rps-8-interact/index.rsh:85:11:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Bob'
              });
            const v395 = stdlib.digest(ctc3, [v389, v390]);
            const v396 = stdlib.digestEq(v356, v395);
            stdlib.assert(v396, {
              at: 'reach standard library:58:17:application',
              fs: ['at ./examples/rps-8-interact/index.rsh:87:20:application call to "checkCommitment" (defined at: reach standard library:57:8:function exp)'],
              msg: null,
              who: 'Bob'
              });
            const v397 = stdlib.sub(stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:7:18:decimal', stdlib.UInt_max, 4), v373);
            const v398 = stdlib.add(v390, v397);
            const v399 = stdlib.mod(v398, stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:7:34:decimal', stdlib.UInt_max, 3));
            const cv325 = v399;
            const cv326 = v391;
            const cv332 = v332;
            
            v325 = cv325;
            v326 = cv326;
            v332 = cv332;
            
            continue;}
          
          }
        
        }
      
      }
    const v457 = stdlib.eq(v325, stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:makeEnum', stdlib.UInt_max, 2));
    const v460 = stdlib.mul(stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:94:12:decimal', stdlib.UInt_max, 2), v304);
    const v462 = v457 ? v303 : v320;
    ;
    stdlib.protect(ctc1, await interact.seeOutcome(v325), {
      at: './examples/rps-8-interact/index.rsh:98:24:application',
      fs: ['at ./examples/rps-8-interact/index.rsh:97:7:application call to [unknown function] (defined at: ./examples/rps-8-interact/index.rsh:97:25:function exp)'],
      msg: 'seeOutcome',
      who: 'Bob'
      });
    
    return;
    }
  
  
  
  };
const _ALGO = {
  ABI: {
    sigs: []
    },
  appApproval: `BSARAAEgBSgIUAkHeBACgAEEA1gwJgIBAQAiNQAxGEEGsClkSSJbNQEhBVs1AjYaABdJQQAJIjUDIzUFQgaRNhoBFzYaAhc1AzYaAzUESSUMQAMbSSEIDEAB3kkhBQxAAUdJIQcMQACZIQcSRCEHNAESRDQDSSISTDQCEhFEI69kKGRQSVcAIDX/SSRbNf5JIQRbNf1JVzAgNfxJIQZbNftJV1ggNfpJIQlbNflJIQxbNfhINARJFSISREiABKIFZo6wMgY0+A9ENP8xABI0/DEAEhFENPtJQQAMsbIII7IQNPyyB7MiSCKxsggjshAyCbIJMgqyB7MiSDEZJRJEQgWnSCEHNAESRDQDSSISTDQCEhFEI69kKGRQSVcAIDX/SSRbNf5JIQRbNf1JVzAgNfxJIQZbNftJV1ggNfpJIQlbNflJIQxbNfhINARJFSEKEkRJIls190khBVs19kiABDUaKtA09xZQNPYWULAyBjT4DEQ0/zEAEkQ0+jT3FjT2FlABEkQ0/zT+FlA0/RZQNPxQNPYhDTT5CQghDhgWMgYWUDT7FlBCBGNIIQg0ARJENANJIhJMNAISEUQjr2QoZFBJVwAgNf9JJFs1/kkhBFs1/UlXMCA1/EkhBls1+0lXWCA1+kkhCVs1+Ug0BEkVIhJESIAE4huzqbAyBjT5D0Q0/zEAEjT8MQASEUQ0+0lBAAyxsggjshA0/7IHsyJIIrGyCCOyEDIJsgkyCrIHsyJIMRklEkRCBHBJgQYMQACpSCEINAESRDQDSSISTDQCEhFEI69kKGRQSVcAIDX/SSRbNf5JIQRbNf1JVzAgNfxJIQZbNftJV1ggNfpJIQlbNflINARJFSEFEkRJFzX4SIAEcO3vejT4FlCwMgY0+QxENPwxABJEMgY0/Qg19zT/NP4WUDT9FlA0/FA0+xZQNPpQNPgWUDT3FlAjr0sBVwB/ZyhLAVd/CWdIIQc1ATIGNQIxGSISREIDwEglNAESRDQDSSISTDQCEhFEI69kSVcAIDX/SSRbNf5JIQRbNf1JVzAgNfxJIQZbNftJIQ9bNfpINARJFSISREiABMyZklywMgY0+g9ENP8xABI0/DEAEhFENPtJQQAMsbIII7IQNPyyB7MiSCKxsggjshAyCbIJMgqyB7MiSDEZJRJEQgM6SSELDEABFUkhDgxAAKBJIQ0MQACYSCU0ARJENANJIhJMNAISEUQjr2RJVwAgNf9JJFs1/kkhBFs1/UlXMCA1/EkhBls1+0khD1s1+kg0BEkVJBJESTX5SIAEOLAjLTT5ULAyBjT6DEQ0/zEAEkQyBjT9CDX4NP80/hZQNP0WUDT8UDT7FlA0+VA0+BZQI69LAVcAf2coSwFXfwFnSCEINQEyBjUCMRkiEkRCAo1ISCM0ARJENANJIhJMNAISEUQjr2RJVwAgNf9JJFs1/kkhBFs1/UkhEFs1/Eg0BEkVIhJESIAEQbFATbAyBjT8D0Q0/klBAAyxsggjshA0/7IHsyJIIrGyCCOyEDIJsgkyCrIHsyJIMRklEkRCAh5JIwxAAKBIIzQBEkQ0A0kiEkw0AhIRRCOvZElXACA1/0kkWzX+SSEEWzX9SSEQWzX8SDQESRUiEkRIgASai5F0sDIGNPwMRDT+SUEANDQASSMINQBMSwE4CBJEI0sBOBASRCJLATgBEkQyA0sBOAYSRDIDSwE4IBJEMgpLATgHEkRINP80/hZQNP0WUDEAUIAIAAAAAAAAAAEyBhZQNP5JCBZQQgDbSCI0ARJENANJIhJMNAISEUQpSDQESRUhChJESSJbNf9JIQVbNf5IgASs0R/DNP8WUDT+FlCwgaCNBklBADQ0AEkjCDUATEsBOAgSRCNLATgQEkQiSwE4ARJEMgNLATgGEkQyA0sBOCASRDIKSwE4BxJESDT/SUEANDQASSMINQBMSwE4CBJEI0sBOBASRCJLATgBEkQyA0sBOAYSRDIDSwE4IBJEMgpLATgHEkRIMgY0/gg1/TEANP8WUDT+FlA0/RZQI69LAVcAOGdIIzUBMgY1AjEZIhJEQgCdSSJbNf9JIQVbNf5JIQpbNf1ISVcAIDX8SSRbNftJIQRbNfpJVzAgNflINP8jEkEANDT+NPoINfg0/DT7FlA0+hZQNPlQNP0WUDT4FlAjr0sBVwBgZ0glNQEyBjUCMRkiEkRCADghCzT7C0lBABSxsggjshA0+TT8NP8hCxJNsgezIkgisbIII7IQMgmyCTIKsgezIkgxGSUSREIAACk0ARY0AhZQZzQFQQAKgAQVH3x1NAZQsDQASSMIMgQSRDEWEkQjQyJDMRkiEkQiNQEiNQJC/8k=`,
  appClear: `BYEA`,
  extraPages: 0,
  mapDataKeys: 0,
  mapDataSize: 0,
  stateKeys: 2,
  stateSize: 136,
  unsupported: [],
  version: 9
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
                "name": "v304",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v305",
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
                "name": "v304",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v305",
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
                "name": "v356",
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
                "name": "v373",
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
                "name": "v389",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v390",
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
                "name": "v356",
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
                "name": "v373",
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
                "name": "v389",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v390",
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
  Bytecode: `0x608060405260405162001b0d38038062001b0d833981016040819052620000269162000248565b600080805543600355604080516020810190915290815260408051835181526020808501518051828401520151918101919091527f80c0078efe412e5091172e0df54decefb16131f320816d23b64aede2bf8e9e4b9060600160405180910390a16020820151516200009c903414600762000141565b6020808301510151620000b09043620002a8565b81526040805160808082018352600060208084018281528486018381526060808701858152338089528b860180515186525186015184528a5182526001968790554390965588518086019690965292518589015290519084015251828401528451808303909301835260a0909101909352805191926200013792600292909101906200016b565b505050506200030c565b81620001675760405163100960cb60e01b81526004810182905260240160405180910390fd5b5050565b8280546200017990620002cf565b90600052602060002090601f0160209004810192826200019d5760008555620001e8565b82601f10620001b857805160ff1916838001178555620001e8565b82800160010185558215620001e8579182015b82811115620001e8578251825591602001919060010190620001cb565b50620001f6929150620001fa565b5090565b5b80821115620001f65760008155600101620001fb565b604080519081016001600160401b03811182821017156200024257634e487b7160e01b600052604160045260246000fd5b60405290565b600081830360608112156200025c57600080fd5b6200026662000211565b835181526040601f19830112156200027d57600080fd5b6200028762000211565b60208581015182526040909501518582015293810193909352509092915050565b60008219821115620002ca57634e487b7160e01b600052601160045260246000fd5b500190565b600181811c90821680620002e457607f821691505b602082108114156200030657634e487b7160e01b600052602260045260246000fd5b50919050565b6117f1806200031c6000396000f3fe60806040526004361061009a5760003560e01c80638e314769116100615780638e31476914610115578063980b6eac14610128578063a209ad4e1461013b578063ab53f2c61461014e578063bf2c5b2414610171578063de7369981461018457005b80631e93b0f1146100a35780632c10a159146100c75780637eea518c146100da57806383230757146100ed5780638328d4c41461010257005b366100a157005b005b3480156100af57600080fd5b506003545b6040519081526020015b60405180910390f35b6100a16100d53660046113d1565b610197565b6100a16100e83660046113d1565b61032c565b3480156100f957600080fd5b506001546100b4565b6100a16101103660046113f4565b6104a8565b6100a16101233660046113d1565b6106cc565b6100a16101363660046113d1565b610865565b6100a16101493660046113d1565b610b0a565b34801561015a57600080fd5b50610163610d52565b6040516100be929190611406565b6100a161017f3660046113d1565b610def565b6100a16101923660046113d1565b610f84565b6101a760016000541460096110d8565b6101c1813515806101ba57506001548235145b600a6110d8565b6000808055600280546101d390611463565b80601f01602080910402602001604051908101604052809291908181526020018280546101ff90611463565b801561024c5780601f106102215761010080835404028352916020019161024c565b820191906000526020600020905b81548152906001019060200180831161022f57829003601f168201915b505050505080602001905181019061026491906114b4565b905061027781606001514310600b6110d8565b7f79ca1a789d797004bc78dff9632d64e202e102f2d008dcc20c5a645ef7d4a7d1826040516102a6919061152d565b60405180910390a16102bf8160200151341460086110d8565b6102c761128a565b815181516001600160a01b039091169052602080830180518351830152604080850151845190910152825133606090910152818301805160019052514392019190915251610315908061156d565b602082015160400152610327816110fd565b505050565b61033c600160005414600d6110d8565b6103568135158061034f57506001548235145b600e6110d8565b60008080556002805461036890611463565b80601f016020809104026020016040519081016040528092919081815260200182805461039490611463565b80156103e15780601f106103b6576101008083540402835291602001916103e1565b820191906000526020600020905b8154815290600101906020018083116103c457829003601f168201915b50505050508060200190518101906103f991906114b4565b905061040d8160600151431015600f6110d8565b7f82e152e8b1d7e41adffbddbd5b2fe2e130356df9b7ab7d06526a80d7888af3e18260405161043c919061152d565b60405180910390a16104503415600c6110d8565b805160208201516040516001600160a01b039092169181156108fc0291906000818181858888f1935050505015801561048d573d6000803e3d6000fd5b50600080805560018190556104a4906002906112e3565b5050565b6104b860096000541460276110d8565b6104d2813515806104cb57506001548235145b60286110d8565b6000808055600280546104e490611463565b80601f016020809104026020016040519081016040528092919081815260200182805461051090611463565b801561055d5780601f106105325761010080835404028352916020019161055d565b820191906000526020600020905b81548152906001019060200180831161054057829003601f168201915b50505050508060200190518101906105759190611585565b90506105888160e00151431060296110d8565b604080518335815260208085013590820152838201358183015290517f41b6d8e223fb0a5cfe68af9f34b07a5a94b63517841457ccfc53fb18b8e41fde9181900360600190a16105da341560246110d8565b80516105f2906001600160a01b0316331460256110d8565b6040805161063e9161061891602080870135928701359101918252602082015260400190565b6040516020818303038152906040528051906020012060001c8260a001511460266110d8565b61064661128a565b815181516001600160a01b0391821690526020808401518351909101526040808401518351909101526060808401518351921691015260c082015160039061068f906004611631565b61069d90604086013561156d565b6106a79190611648565b60208083018051929092528151439101526080830151905160400152610327816110fd565b6106dc60056000541460176110d8565b6106f6813515806106ef57506001548235145b60186110d8565b60008080556002805461070890611463565b80601f016020809104026020016040519081016040528092919081815260200182805461073490611463565b80156107815780601f1061075657610100808354040283529160200191610781565b820191906000526020600020905b81548152906001019060200180831161076457829003601f168201915b5050505050806020019051810190610799919061166a565b90506107ad8160a0015143101560196110d8565b7f9cdba579557d44a893ea7929682d6795d48dd5c40dc981d852842d4b18914de8826040516107dc919061152d565b60405180910390a16107f0341560156110d8565b8051610824906001600160a01b0316331461081a5760608201516001600160a01b0316331461081d565b60015b60166110d8565b80606001516001600160a01b03166108fc82608001519081150290604051600060405180830381858888f1935050505015801561048d573d6000803e3d6000fd5b610875600760005414601c6110d8565b61088f8135158061088857506001548235145b601d6110d8565b6000808055600280546108a190611463565b80601f01602080910402602001604051908101604052809291908181526020018280546108cd90611463565b801561091a5780601f106108ef5761010080835404028352916020019161091a565b820191906000526020600020905b8154815290600101906020018083116108fd57829003601f168201915b505050505080602001905181019061093291906116fe565b905061094a6040518060200160405280600081525090565b61095b8260c001514310601e6110d8565b6040805184358152602080860135908201527f47a1195f23e4ca8f87a7a744a702eeb3eb5b0d56dae31e23931e0349a611c709910160405180910390a16109a43415601a6110d8565b60608201516109bf906001600160a01b03163314601b6110d8565b60408201516109ce904361156d565b81526040805161010081018252600080825260208201819052918101829052606081018290526080810182905260a0810182905260c0810182905260e081019190915282516001600160a01b0390811682526020808501518184015260408086015181850152606080870151909316928401929092526080808601519084015260a080860151908401528581013560c0840152835160e08401526009600055436001559051610adf9183910160006101008201905060018060a01b038084511683526020840151602084015260408401516040840152806060850151166060840152506080830151608083015260a083015160a083015260c083015160c083015260e083015160e083015292915050565b60405160208183030381529060405260029080519060200190610b03929190611320565b5050505050565b610b1a60056000541460126110d8565b610b3481351580610b2d57506001548235145b60136110d8565b600080805560028054610b4690611463565b80601f0160208091040260200160405190810160405280929190818152602001828054610b7290611463565b8015610bbf5780601f10610b9457610100808354040283529160200191610bbf565b820191906000526020600020905b815481529060010190602001808311610ba257829003601f168201915b5050505050806020019051810190610bd7919061166a565b9050610bef6040518060200160405280600081525090565b610c008260a00151431060146110d8565b6040805184358152602080860135908201527f7d7741a24b17d1850d95beda5136388f520bc575ba9499f2f40fdfa7647ad82f910160405180910390a1610c49341560106110d8565b8151610c61906001600160a01b0316331460116110d8565b6040820151610c70904361156d565b81526040805160e081018252600080825260208201819052918101829052606081018290526080810182905260a0810182905260c081019190915282516001600160a01b039081168083526020808601518185019081526040808801518187019081526060808a015187168189019081526080808c0151818b019081528d88013560a0808d019182528d5160c0808f0191825260076000554360015589519b8c019c909c529851978a0197909752945193880193909352905190971696850196909652945190830152925191810191909152905160e082015261010001610adf565b600060606000546002808054610d6790611463565b80601f0160208091040260200160405190810160405280929190818152602001828054610d9390611463565b8015610de05780601f10610db557610100808354040283529160200191610de0565b820191906000526020600020905b815481529060010190602001808311610dc357829003601f168201915b50505050509050915091509091565b610dff60076000541460216110d8565b610e1981351580610e1257506001548235145b60226110d8565b600080805560028054610e2b90611463565b80601f0160208091040260200160405190810160405280929190818152602001828054610e5790611463565b8015610ea45780601f10610e7957610100808354040283529160200191610ea4565b820191906000526020600020905b815481529060010190602001808311610e8757829003601f168201915b5050505050806020019051810190610ebc91906116fe565b9050610ed08160c0015143101560236110d8565b7fba16100ad25f3c6798bc3b7e9ca316fb231388e6fa4444c0f477e2a4336514e082604051610eff919061152d565b60405180910390a1610f133415601f6110d8565b8051610f47906001600160a01b03163314610f3d5760608201516001600160a01b03163314610f40565b60015b60206110d8565b805160808201516040516001600160a01b039092169181156108fc0291906000818181858888f1935050505015801561048d573d6000803e3d6000fd5b610f94600960005414602c6110d8565b610fae81351580610fa757506001548235145b602d6110d8565b600080805560028054610fc090611463565b80601f0160208091040260200160405190810160405280929190818152602001828054610fec90611463565b80156110395780601f1061100e57610100808354040283529160200191611039565b820191906000526020600020905b81548152906001019060200180831161101c57829003601f168201915b50505050508060200190518101906110519190611585565b90506110658160e00151431015602e6110d8565b7fb764c356a899e639c4043e82fb6274894baac6d84c74f3b3ae78d8f4b22b000382604051611094919061152d565b60405180910390a16110a83415602a6110d8565b8051610824906001600160a01b031633146110d25760608201516001600160a01b031633146110d5565b60015b602b5b816104a45760405163100960cb60e01b81526004810182905260240160405180910390fd5b60408051602081019091526000815260208201515160011415611225578151604001516020808401510151611132919061156d565b81526040805160c081018252600080825260208201819052918101829052606081018290526080810182905260a08101919091528251516001600160a01b039081168083528451602090810151818501908152865160409081015181870190815288516060908101518716818901908152858b01518401516080808b019182528b5160a0808d019182526005600055436001558751998a019a909a529651958801959095529251918601919091525190951690830152925191810191909152905160c082015260e0016040516020818303038152906040526002908051906020019061121f929190611320565b50505050565b60208201515160021461123d57815160600151611241565b8151515b6001600160a01b03166108fc8360000151602001516002611262919061179c565b6040518115909202916000818181858888f1935050505015801561048d573d6000803e3d6000fd5b6040805160c0810182526000918101828152606082018390526080820183905260a082019290925290819081526020016112de60405180606001604052806000815260200160008152602001600081525090565b905290565b5080546112ef90611463565b6000825580601f106112ff575050565b601f01602090049060005260206000209081019061131d91906113a4565b50565b82805461132c90611463565b90600052602060002090601f01602090048101928261134e5760008555611394565b82601f1061136757805160ff1916838001178555611394565b82800160010185558215611394579182015b82811115611394578251825591602001919060010190611379565b506113a09291506113a4565b5090565b5b808211156113a057600081556001016113a5565b6000604082840312156113cb57600080fd5b50919050565b6000604082840312156113e357600080fd5b6113ed83836113b9565b9392505050565b6000606082840312156113cb57600080fd5b82815260006020604081840152835180604085015260005b8181101561143a5785810183015185820160600152820161141e565b8181111561144c576000606083870101525b50601f01601f191692909201606001949350505050565b600181811c9082168061147757607f821691505b602082108114156113cb57634e487b7160e01b600052602260045260246000fd5b80516001600160a01b03811681146114af57600080fd5b919050565b6000608082840312156114c657600080fd5b6040516080810181811067ffffffffffffffff821117156114f757634e487b7160e01b600052604160045260246000fd5b60405261150383611498565b81526020830151602082015260408301516040820152606083015160608201528091505092915050565b8135815260408101602083013580151580821461154957600080fd5b806020850152505092915050565b634e487b7160e01b600052601160045260246000fd5b6000821982111561158057611580611557565b500190565b600061010080838503121561159957600080fd5b6040519081019067ffffffffffffffff821181831017156115ca57634e487b7160e01b600052604160045260246000fd5b816040526115d784611498565b815260208401516020820152604084015160408201526115f960608501611498565b60608201526080840151608082015260a084015160a082015260c084015160c082015260e084015160e0820152809250505092915050565b60008282101561164357611643611557565b500390565b60008261166557634e487b7160e01b600052601260045260246000fd5b500690565b600060c0828403121561167c57600080fd5b60405160c0810181811067ffffffffffffffff821117156116ad57634e487b7160e01b600052604160045260246000fd5b6040526116b983611498565b815260208301516020820152604083015160408201526116db60608401611498565b60608201526080830151608082015260a083015160a08201528091505092915050565b600060e0828403121561171057600080fd5b60405160e0810181811067ffffffffffffffff8211171561174157634e487b7160e01b600052604160045260246000fd5b60405261174d83611498565b8152602083015160208201526040830151604082015261176f60608401611498565b60608201526080830151608082015260a083015160a082015260c083015160c08201528091505092915050565b60008160001904831182151516156117b6576117b6611557565b50029056fea264697066735822122095212d581150f512055857943938ae094230c5681ef11a1ee51b7cc4258d2cca64736f6c634300080a0033`,
  BytecodeLen: 6925,
  Which: `oD`,
  version: 6,
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
