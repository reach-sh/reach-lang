// Automatically generated with Reach 0.1.13
/* eslint-disable */
export const _version = '0.1.13';
export const _versionHash = '0.1.13';
export const _backendVersion = 27;

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
  const ctc1 = stdlib.T_Digest;
  const ctc2 = stdlib.T_Null;
  const ctc3 = stdlib.T_Address;
  
  
  const v300 = stdlib.protect(ctc0, interact.deadline, 'for Alice\'s interact field deadline');
  const v301 = stdlib.protect(ctc0, interact.wager, 'for Alice\'s interact field wager');
  
  const txn1 = await (ctc.sendrecv({
    args: [v301, v300],
    evt_cnt: 2,
    funcNum: 0,
    lct: stdlib.checkedBigNumberify('./index.rsh:49:9:dot', stdlib.UInt_max, '0'),
    onlyIf: true,
    out_tys: [ctc0, ctc0],
    pay: [v301, []],
    sim_p: (async (txn1) => {
      const sim_r = { txns: [], mapRefs: [], maps: [] };
      let sim_txn_ctr = stdlib.UInt_max;
      const getSimTokCtr = () => { sim_txn_ctr = sim_txn_ctr.sub(1); return sim_txn_ctr; };
      
      
      const {data: [v305, v306], secs: v308, time: v307, didSend: v56, from: v304 } = txn1;
      
      sim_r.txns.push({
        amt: v305,
        kind: 'to',
        tok: undefined /* Nothing */
        });
      const v317 = stdlib.safeAdd(v307, v306);
      sim_r.isHalt = false;
      
      return sim_r;
      }),
    soloSend: true,
    timeoutAt: undefined /* mto */,
    tys: [ctc0, ctc0],
    waitIfNotPresent: false
    }));
  const {data: [v305, v306], secs: v308, time: v307, didSend: v56, from: v304 } = txn1;
  ;
  const v317 = stdlib.safeAdd(v307, v306);
  const txn2 = await (ctc.recv({
    didSend: false,
    evt_cnt: 0,
    funcNum: 1,
    out_tys: [],
    timeoutAt: ['time', v317],
    waitIfNotPresent: false
    }));
  if (txn2.didTimeout) {
    const txn3 = await (ctc.sendrecv({
      args: [v304, v305, v306, v317],
      evt_cnt: 0,
      funcNum: 2,
      lct: v307,
      onlyIf: true,
      out_tys: [],
      pay: [stdlib.checkedBigNumberify('reach standard library:197:11:decimal', stdlib.UInt_max, '0'), []],
      sim_p: (async (txn3) => {
        const sim_r = { txns: [], mapRefs: [], maps: [] };
        let sim_txn_ctr = stdlib.UInt_max;
        const getSimTokCtr = () => { sim_txn_ctr = sim_txn_ctr.sub(1); return sim_txn_ctr; };
        
        
        const {data: [], secs: v471, time: v470, didSend: v253, from: v469 } = txn3;
        
        ;
        sim_r.txns.push({
          kind: 'from',
          to: v304,
          tok: undefined /* Nothing */
          });
        sim_r.txns.push({
          kind: 'halt',
          tok: undefined /* Nothing */
          })
        sim_r.isHalt = true;
        
        return sim_r;
        }),
      soloSend: false,
      timeoutAt: undefined /* mto */,
      tys: [ctc3, ctc0, ctc0, ctc0],
      waitIfNotPresent: false
      }));
    const {data: [], secs: v471, time: v470, didSend: v253, from: v469 } = txn3;
    ;
    ;
    stdlib.protect(ctc2, await interact.informTimeout(), {
      at: './index.rsh:41:29:application',
      fs: ['at ./index.rsh:40:9:application call to [unknown function] (defined at: ./index.rsh:40:27:function exp)', 'at reach standard library:200:8:application call to "after" (defined at: ./index.rsh:39:28:function exp)', 'at ./index.rsh:57:51:application call to "closeTo" (defined at: reach standard library:195:8:function exp)'],
      msg: 'informTimeout',
      who: 'Alice'
      });
    
    return;
    
    }
  else {
    const {data: [], secs: v323, time: v322, didSend: v65, from: v321 } = txn2;
    const v325 = stdlib.add(v305, v305);
    ;
    let v326 = stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, '1');
    let v327 = v322;
    let v334 = v325;
    
    let txn3 = txn2;
    while (await (async () => {
      const v342 = stdlib.eq(v326, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, '1'));
      
      return v342;})()) {
      const v349 = stdlib.safeAdd(v327, v306);
      const v353 = stdlib.protect(ctc0, await interact.getHand(), {
        at: './index.rsh:65:42:application',
        fs: ['at ./index.rsh:64:15:application call to [unknown function] (defined at: ./index.rsh:64:19:function exp)'],
        msg: 'getHand',
        who: 'Alice'
        });
      const v354 = stdlib.protect(ctc0, await interact.random(), {
        at: 'reach standard library:64:31:application',
        fs: ['at ./index.rsh:66:56:application call to "makeCommitment" (defined at: reach standard library:63:8:function exp)', 'at ./index.rsh:64:15:application call to [unknown function] (defined at: ./index.rsh:64:19:function exp)'],
        msg: 'random',
        who: 'Alice'
        });
      const v355 = stdlib.digest([ctc0, ctc0], [v354, v353]);
      
      const txn4 = await (ctc.sendrecv({
        args: [v304, v305, v306, v321, v334, v349, v355],
        evt_cnt: 1,
        funcNum: 4,
        lct: v327,
        onlyIf: true,
        out_tys: [ctc1],
        pay: [stdlib.checkedBigNumberify('./index.rsh:69:11:decimal', stdlib.UInt_max, '0'), []],
        sim_p: (async (txn4) => {
          const sim_r = { txns: [], mapRefs: [], maps: [] };
          let sim_txn_ctr = stdlib.UInt_max;
          const getSimTokCtr = () => { sim_txn_ctr = sim_txn_ctr.sub(1); return sim_txn_ctr; };
          
          
          const {data: [v358], secs: v360, time: v359, didSend: v91, from: v357 } = txn4;
          
          ;
          const v368 = stdlib.safeAdd(v359, v306);
          sim_r.isHalt = false;
          
          return sim_r;
          }),
        soloSend: true,
        timeoutAt: ['time', v349],
        tys: [ctc3, ctc0, ctc0, ctc3, ctc0, ctc0, ctc1],
        waitIfNotPresent: false
        }));
      if (txn4.didTimeout) {
        const txn5 = await (ctc.sendrecv({
          args: [v304, v305, v306, v321, v334, v349],
          evt_cnt: 0,
          funcNum: 5,
          lct: v327,
          onlyIf: true,
          out_tys: [],
          pay: [stdlib.checkedBigNumberify('reach standard library:197:11:decimal', stdlib.UInt_max, '0'), []],
          sim_p: (async (txn5) => {
            const sim_r = { txns: [], mapRefs: [], maps: [] };
            let sim_txn_ctr = stdlib.UInt_max;
            const getSimTokCtr = () => { sim_txn_ctr = sim_txn_ctr.sub(1); return sim_txn_ctr; };
            
            
            const {data: [], secs: v437, time: v436, didSend: v206, from: v435 } = txn5;
            
            ;
            sim_r.txns.push({
              kind: 'from',
              to: v321,
              tok: undefined /* Nothing */
              });
            sim_r.txns.push({
              kind: 'halt',
              tok: undefined /* Nothing */
              })
            sim_r.isHalt = true;
            
            return sim_r;
            }),
          soloSend: false,
          timeoutAt: undefined /* mto */,
          tys: [ctc3, ctc0, ctc0, ctc3, ctc0, ctc0],
          waitIfNotPresent: false
          }));
        const {data: [], secs: v437, time: v436, didSend: v206, from: v435 } = txn5;
        ;
        const v438 = stdlib.addressEq(v304, v435);
        const v439 = stdlib.addressEq(v321, v435);
        const v440 = v438 ? true : v439;
        stdlib.assert(v440, {
          at: 'reach standard library:197:11:dot',
          fs: ['at ./index.rsh:70:53:application call to "closeTo" (defined at: reach standard library:195:8:function exp)'],
          msg: 'sender correct',
          who: 'Alice'
          });
        ;
        stdlib.protect(ctc2, await interact.informTimeout(), {
          at: './index.rsh:41:29:application',
          fs: ['at ./index.rsh:40:9:application call to [unknown function] (defined at: ./index.rsh:40:27:function exp)', 'at reach standard library:200:8:application call to "after" (defined at: ./index.rsh:39:28:function exp)', 'at ./index.rsh:70:53:application call to "closeTo" (defined at: reach standard library:195:8:function exp)'],
          msg: 'informTimeout',
          who: 'Alice'
          });
        
        return;
        
        }
      else {
        const {data: [v358], secs: v360, time: v359, didSend: v91, from: v357 } = txn4;
        ;
        const v361 = stdlib.addressEq(v304, v357);
        stdlib.assert(v361, {
          at: './index.rsh:69:11:dot',
          fs: [],
          msg: 'sender correct',
          who: 'Alice'
          });
        const v368 = stdlib.safeAdd(v359, v306);
        const txn5 = await (ctc.recv({
          didSend: false,
          evt_cnt: 1,
          funcNum: 6,
          out_tys: [ctc0],
          timeoutAt: ['time', v368],
          waitIfNotPresent: false
          }));
        if (txn5.didTimeout) {
          const txn6 = await (ctc.sendrecv({
            args: [v304, v305, v306, v321, v334, v358, v368],
            evt_cnt: 0,
            funcNum: 7,
            lct: v359,
            onlyIf: true,
            out_tys: [],
            pay: [stdlib.checkedBigNumberify('reach standard library:197:11:decimal', stdlib.UInt_max, '0'), []],
            sim_p: (async (txn6) => {
              const sim_r = { txns: [], mapRefs: [], maps: [] };
              let sim_txn_ctr = stdlib.UInt_max;
              const getSimTokCtr = () => { sim_txn_ctr = sim_txn_ctr.sub(1); return sim_txn_ctr; };
              
              
              const {data: [], secs: v419, time: v418, didSend: v172, from: v417 } = txn6;
              
              ;
              sim_r.txns.push({
                kind: 'from',
                to: v304,
                tok: undefined /* Nothing */
                });
              sim_r.txns.push({
                kind: 'halt',
                tok: undefined /* Nothing */
                })
              sim_r.isHalt = true;
              
              return sim_r;
              }),
            soloSend: false,
            timeoutAt: undefined /* mto */,
            tys: [ctc3, ctc0, ctc0, ctc3, ctc0, ctc1, ctc0],
            waitIfNotPresent: false
            }));
          const {data: [], secs: v419, time: v418, didSend: v172, from: v417 } = txn6;
          ;
          const v420 = stdlib.addressEq(v304, v417);
          const v421 = stdlib.addressEq(v321, v417);
          const v422 = v420 ? true : v421;
          stdlib.assert(v422, {
            at: 'reach standard library:197:11:dot',
            fs: ['at ./index.rsh:78:53:application call to "closeTo" (defined at: reach standard library:195:8:function exp)'],
            msg: 'sender correct',
            who: 'Alice'
            });
          ;
          stdlib.protect(ctc2, await interact.informTimeout(), {
            at: './index.rsh:41:29:application',
            fs: ['at ./index.rsh:40:9:application call to [unknown function] (defined at: ./index.rsh:40:27:function exp)', 'at reach standard library:200:8:application call to "after" (defined at: ./index.rsh:39:28:function exp)', 'at ./index.rsh:78:53:application call to "closeTo" (defined at: reach standard library:195:8:function exp)'],
            msg: 'informTimeout',
            who: 'Alice'
            });
          
          return;
          
          }
        else {
          const {data: [v374], secs: v376, time: v375, didSend: v101, from: v373 } = txn5;
          ;
          const v377 = stdlib.addressEq(v321, v373);
          stdlib.assert(v377, {
            at: './index.rsh:77:9:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Alice'
            });
          const v384 = stdlib.safeAdd(v375, v306);
          const txn6 = await (ctc.sendrecv({
            args: [v304, v305, v306, v321, v334, v358, v374, v384, v354, v353],
            evt_cnt: 2,
            funcNum: 8,
            lct: v375,
            onlyIf: true,
            out_tys: [ctc0, ctc0],
            pay: [stdlib.checkedBigNumberify('./index.rsh:85:11:decimal', stdlib.UInt_max, '0'), []],
            sim_p: (async (txn6) => {
              const sim_r = { txns: [], mapRefs: [], maps: [] };
              let sim_txn_ctr = stdlib.UInt_max;
              const getSimTokCtr = () => { sim_txn_ctr = sim_txn_ctr.sub(1); return sim_txn_ctr; };
              
              
              const {data: [v389, v390], secs: v392, time: v391, didSend: v111, from: v388 } = txn6;
              
              ;
              const v396 = stdlib.safeSub(stdlib.checkedBigNumberify('./index.rsh:7:18:decimal', stdlib.UInt_max, '4'), v374);
              const v397 = stdlib.safeAdd(v390, v396);
              const v398 = stdlib.safeMod(v397, stdlib.checkedBigNumberify('./index.rsh:7:34:decimal', stdlib.UInt_max, '3'));
              const cv326 = v398;
              const cv327 = v391;
              const cv334 = v334;
              
              await (async () => {
                const v326 = cv326;
                const v327 = cv327;
                const v334 = cv334;
                
                if (await (async () => {
                  const v342 = stdlib.eq(v326, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, '1'));
                  
                  return v342;})()) {
                  const v349 = stdlib.safeAdd(v327, v306);
                  sim_r.isHalt = false;
                  }
                else {
                  const v453 = stdlib.eq(v326, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, '2'));
                  const v456 = stdlib.safeMul(stdlib.checkedBigNumberify('./index.rsh:94:12:decimal', stdlib.UInt_max, '2'), v305);
                  const v458 = v453 ? v304 : v321;
                  sim_r.txns.push({
                    kind: 'from',
                    to: v458,
                    tok: undefined /* Nothing */
                    });
                  sim_r.txns.push({
                    kind: 'halt',
                    tok: undefined /* Nothing */
                    })
                  sim_r.isHalt = true;
                  }})();
              return sim_r;
              }),
            soloSend: true,
            timeoutAt: ['time', v384],
            tys: [ctc3, ctc0, ctc0, ctc3, ctc0, ctc1, ctc0, ctc0, ctc0, ctc0],
            waitIfNotPresent: false
            }));
          if (txn6.didTimeout) {
            const txn7 = await (ctc.sendrecv({
              args: [v304, v305, v306, v321, v334, v358, v374, v384],
              evt_cnt: 0,
              funcNum: 9,
              lct: v375,
              onlyIf: true,
              out_tys: [],
              pay: [stdlib.checkedBigNumberify('reach standard library:197:11:decimal', stdlib.UInt_max, '0'), []],
              sim_p: (async (txn7) => {
                const sim_r = { txns: [], mapRefs: [], maps: [] };
                let sim_txn_ctr = stdlib.UInt_max;
                const getSimTokCtr = () => { sim_txn_ctr = sim_txn_ctr.sub(1); return sim_txn_ctr; };
                
                
                const {data: [], secs: v401, time: v400, didSend: v138, from: v399 } = txn7;
                
                ;
                sim_r.txns.push({
                  kind: 'from',
                  to: v321,
                  tok: undefined /* Nothing */
                  });
                sim_r.txns.push({
                  kind: 'halt',
                  tok: undefined /* Nothing */
                  })
                sim_r.isHalt = true;
                
                return sim_r;
                }),
              soloSend: false,
              timeoutAt: undefined /* mto */,
              tys: [ctc3, ctc0, ctc0, ctc3, ctc0, ctc1, ctc0, ctc0],
              waitIfNotPresent: false
              }));
            const {data: [], secs: v401, time: v400, didSend: v138, from: v399 } = txn7;
            ;
            const v402 = stdlib.addressEq(v304, v399);
            const v403 = stdlib.addressEq(v321, v399);
            const v404 = v402 ? true : v403;
            stdlib.assert(v404, {
              at: 'reach standard library:197:11:dot',
              fs: ['at ./index.rsh:86:53:application call to "closeTo" (defined at: reach standard library:195:8:function exp)'],
              msg: 'sender correct',
              who: 'Alice'
              });
            ;
            stdlib.protect(ctc2, await interact.informTimeout(), {
              at: './index.rsh:41:29:application',
              fs: ['at ./index.rsh:40:9:application call to [unknown function] (defined at: ./index.rsh:40:27:function exp)', 'at reach standard library:200:8:application call to "after" (defined at: ./index.rsh:39:28:function exp)', 'at ./index.rsh:86:53:application call to "closeTo" (defined at: reach standard library:195:8:function exp)'],
              msg: 'informTimeout',
              who: 'Alice'
              });
            
            return;
            
            }
          else {
            const {data: [v389, v390], secs: v392, time: v391, didSend: v111, from: v388 } = txn6;
            ;
            const v393 = stdlib.addressEq(v304, v388);
            stdlib.assert(v393, {
              at: './index.rsh:85:11:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Alice'
              });
            const v394 = stdlib.digest([ctc0, ctc0], [v389, v390]);
            const v395 = stdlib.digestEq(v358, v394);
            stdlib.assert(v395, {
              at: 'reach standard library:69:17:application',
              fs: ['at ./index.rsh:87:20:application call to "checkCommitment" (defined at: reach standard library:68:8:function exp)'],
              msg: null,
              who: 'Alice'
              });
            const v396 = stdlib.safeSub(stdlib.checkedBigNumberify('./index.rsh:7:18:decimal', stdlib.UInt_max, '4'), v374);
            const v397 = stdlib.safeAdd(v390, v396);
            const v398 = stdlib.safeMod(v397, stdlib.checkedBigNumberify('./index.rsh:7:34:decimal', stdlib.UInt_max, '3'));
            const cv326 = v398;
            const cv327 = v391;
            const cv334 = v334;
            
            v326 = cv326;
            v327 = cv327;
            v334 = cv334;
            
            txn3 = txn6;
            continue;}
          
          }
        
        }
      
      }
    const v453 = stdlib.eq(v326, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, '2'));
    const v456 = stdlib.safeMul(stdlib.checkedBigNumberify('./index.rsh:94:12:decimal', stdlib.UInt_max, '2'), v305);
    const v458 = v453 ? v304 : v321;
    ;
    stdlib.protect(ctc2, await interact.seeOutcome(v326), {
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
  const ctc3 = stdlib.T_Address;
  
  
  const txn1 = await (ctc.recv({
    didSend: false,
    evt_cnt: 2,
    funcNum: 0,
    out_tys: [ctc0, ctc0],
    timeoutAt: undefined /* mto */,
    waitIfNotPresent: false
    }));
  const {data: [v305, v306], secs: v308, time: v307, didSend: v56, from: v304 } = txn1;
  ;
  const v317 = stdlib.safeAdd(v307, v306);
  stdlib.protect(ctc1, await interact.acceptWager(v305), {
    at: './index.rsh:54:25:application',
    fs: ['at ./index.rsh:53:11:application call to [unknown function] (defined at: ./index.rsh:53:15:function exp)'],
    msg: 'acceptWager',
    who: 'Bob'
    });
  
  const txn2 = await (ctc.sendrecv({
    args: [v304, v305, v306, v317],
    evt_cnt: 0,
    funcNum: 1,
    lct: v307,
    onlyIf: true,
    out_tys: [],
    pay: [v305, []],
    sim_p: (async (txn2) => {
      const sim_r = { txns: [], mapRefs: [], maps: [] };
      let sim_txn_ctr = stdlib.UInt_max;
      const getSimTokCtr = () => { sim_txn_ctr = sim_txn_ctr.sub(1); return sim_txn_ctr; };
      
      
      const {data: [], secs: v323, time: v322, didSend: v65, from: v321 } = txn2;
      
      const v325 = stdlib.add(v305, v305);
      sim_r.txns.push({
        amt: v305,
        kind: 'to',
        tok: undefined /* Nothing */
        });
      const v326 = stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, '1');
      const v327 = v322;
      const v334 = v325;
      
      if (await (async () => {
        const v342 = stdlib.eq(v326, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, '1'));
        
        return v342;})()) {
        const v349 = stdlib.safeAdd(v327, v306);
        sim_r.isHalt = false;
        }
      else {
        const v453 = stdlib.eq(v326, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, '2'));
        const v456 = stdlib.safeMul(stdlib.checkedBigNumberify('./index.rsh:94:12:decimal', stdlib.UInt_max, '2'), v305);
        const v458 = v453 ? v304 : v321;
        sim_r.txns.push({
          kind: 'from',
          to: v458,
          tok: undefined /* Nothing */
          });
        sim_r.txns.push({
          kind: 'halt',
          tok: undefined /* Nothing */
          })
        sim_r.isHalt = true;
        }
      return sim_r;
      }),
    soloSend: true,
    timeoutAt: ['time', v317],
    tys: [ctc3, ctc0, ctc0, ctc0],
    waitIfNotPresent: false
    }));
  if (txn2.didTimeout) {
    const txn3 = await (ctc.sendrecv({
      args: [v304, v305, v306, v317],
      evt_cnt: 0,
      funcNum: 2,
      lct: v307,
      onlyIf: true,
      out_tys: [],
      pay: [stdlib.checkedBigNumberify('reach standard library:197:11:decimal', stdlib.UInt_max, '0'), []],
      sim_p: (async (txn3) => {
        const sim_r = { txns: [], mapRefs: [], maps: [] };
        let sim_txn_ctr = stdlib.UInt_max;
        const getSimTokCtr = () => { sim_txn_ctr = sim_txn_ctr.sub(1); return sim_txn_ctr; };
        
        
        const {data: [], secs: v471, time: v470, didSend: v253, from: v469 } = txn3;
        
        ;
        sim_r.txns.push({
          kind: 'from',
          to: v304,
          tok: undefined /* Nothing */
          });
        sim_r.txns.push({
          kind: 'halt',
          tok: undefined /* Nothing */
          })
        sim_r.isHalt = true;
        
        return sim_r;
        }),
      soloSend: false,
      timeoutAt: undefined /* mto */,
      tys: [ctc3, ctc0, ctc0, ctc0],
      waitIfNotPresent: false
      }));
    const {data: [], secs: v471, time: v470, didSend: v253, from: v469 } = txn3;
    ;
    ;
    stdlib.protect(ctc1, await interact.informTimeout(), {
      at: './index.rsh:41:29:application',
      fs: ['at ./index.rsh:40:9:application call to [unknown function] (defined at: ./index.rsh:40:27:function exp)', 'at reach standard library:200:8:application call to "after" (defined at: ./index.rsh:39:28:function exp)', 'at ./index.rsh:57:51:application call to "closeTo" (defined at: reach standard library:195:8:function exp)'],
      msg: 'informTimeout',
      who: 'Bob'
      });
    
    return;
    
    }
  else {
    const {data: [], secs: v323, time: v322, didSend: v65, from: v321 } = txn2;
    const v325 = stdlib.add(v305, v305);
    ;
    let v326 = stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, '1');
    let v327 = v322;
    let v334 = v325;
    
    let txn3 = txn2;
    while (await (async () => {
      const v342 = stdlib.eq(v326, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, '1'));
      
      return v342;})()) {
      const v349 = stdlib.safeAdd(v327, v306);
      const txn4 = await (ctc.recv({
        didSend: false,
        evt_cnt: 1,
        funcNum: 4,
        out_tys: [ctc2],
        timeoutAt: ['time', v349],
        waitIfNotPresent: false
        }));
      if (txn4.didTimeout) {
        const txn5 = await (ctc.sendrecv({
          args: [v304, v305, v306, v321, v334, v349],
          evt_cnt: 0,
          funcNum: 5,
          lct: v327,
          onlyIf: true,
          out_tys: [],
          pay: [stdlib.checkedBigNumberify('reach standard library:197:11:decimal', stdlib.UInt_max, '0'), []],
          sim_p: (async (txn5) => {
            const sim_r = { txns: [], mapRefs: [], maps: [] };
            let sim_txn_ctr = stdlib.UInt_max;
            const getSimTokCtr = () => { sim_txn_ctr = sim_txn_ctr.sub(1); return sim_txn_ctr; };
            
            
            const {data: [], secs: v437, time: v436, didSend: v206, from: v435 } = txn5;
            
            ;
            sim_r.txns.push({
              kind: 'from',
              to: v321,
              tok: undefined /* Nothing */
              });
            sim_r.txns.push({
              kind: 'halt',
              tok: undefined /* Nothing */
              })
            sim_r.isHalt = true;
            
            return sim_r;
            }),
          soloSend: false,
          timeoutAt: undefined /* mto */,
          tys: [ctc3, ctc0, ctc0, ctc3, ctc0, ctc0],
          waitIfNotPresent: false
          }));
        const {data: [], secs: v437, time: v436, didSend: v206, from: v435 } = txn5;
        ;
        const v438 = stdlib.addressEq(v304, v435);
        const v439 = stdlib.addressEq(v321, v435);
        const v440 = v438 ? true : v439;
        stdlib.assert(v440, {
          at: 'reach standard library:197:11:dot',
          fs: ['at ./index.rsh:70:53:application call to "closeTo" (defined at: reach standard library:195:8:function exp)'],
          msg: 'sender correct',
          who: 'Bob'
          });
        ;
        stdlib.protect(ctc1, await interact.informTimeout(), {
          at: './index.rsh:41:29:application',
          fs: ['at ./index.rsh:40:9:application call to [unknown function] (defined at: ./index.rsh:40:27:function exp)', 'at reach standard library:200:8:application call to "after" (defined at: ./index.rsh:39:28:function exp)', 'at ./index.rsh:70:53:application call to "closeTo" (defined at: reach standard library:195:8:function exp)'],
          msg: 'informTimeout',
          who: 'Bob'
          });
        
        return;
        
        }
      else {
        const {data: [v358], secs: v360, time: v359, didSend: v91, from: v357 } = txn4;
        ;
        const v361 = stdlib.addressEq(v304, v357);
        stdlib.assert(v361, {
          at: './index.rsh:69:11:dot',
          fs: [],
          msg: 'sender correct',
          who: 'Bob'
          });
        const v368 = stdlib.safeAdd(v359, v306);
        const v372 = stdlib.protect(ctc0, await interact.getHand(), {
          at: './index.rsh:75:50:application',
          fs: ['at ./index.rsh:74:13:application call to [unknown function] (defined at: ./index.rsh:74:17:function exp)'],
          msg: 'getHand',
          who: 'Bob'
          });
        
        const txn5 = await (ctc.sendrecv({
          args: [v304, v305, v306, v321, v334, v358, v368, v372],
          evt_cnt: 1,
          funcNum: 6,
          lct: v359,
          onlyIf: true,
          out_tys: [ctc0],
          pay: [stdlib.checkedBigNumberify('./index.rsh:77:9:decimal', stdlib.UInt_max, '0'), []],
          sim_p: (async (txn5) => {
            const sim_r = { txns: [], mapRefs: [], maps: [] };
            let sim_txn_ctr = stdlib.UInt_max;
            const getSimTokCtr = () => { sim_txn_ctr = sim_txn_ctr.sub(1); return sim_txn_ctr; };
            
            
            const {data: [v374], secs: v376, time: v375, didSend: v101, from: v373 } = txn5;
            
            ;
            const v384 = stdlib.safeAdd(v375, v306);
            sim_r.isHalt = false;
            
            return sim_r;
            }),
          soloSend: true,
          timeoutAt: ['time', v368],
          tys: [ctc3, ctc0, ctc0, ctc3, ctc0, ctc2, ctc0, ctc0],
          waitIfNotPresent: false
          }));
        if (txn5.didTimeout) {
          const txn6 = await (ctc.sendrecv({
            args: [v304, v305, v306, v321, v334, v358, v368],
            evt_cnt: 0,
            funcNum: 7,
            lct: v359,
            onlyIf: true,
            out_tys: [],
            pay: [stdlib.checkedBigNumberify('reach standard library:197:11:decimal', stdlib.UInt_max, '0'), []],
            sim_p: (async (txn6) => {
              const sim_r = { txns: [], mapRefs: [], maps: [] };
              let sim_txn_ctr = stdlib.UInt_max;
              const getSimTokCtr = () => { sim_txn_ctr = sim_txn_ctr.sub(1); return sim_txn_ctr; };
              
              
              const {data: [], secs: v419, time: v418, didSend: v172, from: v417 } = txn6;
              
              ;
              sim_r.txns.push({
                kind: 'from',
                to: v304,
                tok: undefined /* Nothing */
                });
              sim_r.txns.push({
                kind: 'halt',
                tok: undefined /* Nothing */
                })
              sim_r.isHalt = true;
              
              return sim_r;
              }),
            soloSend: false,
            timeoutAt: undefined /* mto */,
            tys: [ctc3, ctc0, ctc0, ctc3, ctc0, ctc2, ctc0],
            waitIfNotPresent: false
            }));
          const {data: [], secs: v419, time: v418, didSend: v172, from: v417 } = txn6;
          ;
          const v420 = stdlib.addressEq(v304, v417);
          const v421 = stdlib.addressEq(v321, v417);
          const v422 = v420 ? true : v421;
          stdlib.assert(v422, {
            at: 'reach standard library:197:11:dot',
            fs: ['at ./index.rsh:78:53:application call to "closeTo" (defined at: reach standard library:195:8:function exp)'],
            msg: 'sender correct',
            who: 'Bob'
            });
          ;
          stdlib.protect(ctc1, await interact.informTimeout(), {
            at: './index.rsh:41:29:application',
            fs: ['at ./index.rsh:40:9:application call to [unknown function] (defined at: ./index.rsh:40:27:function exp)', 'at reach standard library:200:8:application call to "after" (defined at: ./index.rsh:39:28:function exp)', 'at ./index.rsh:78:53:application call to "closeTo" (defined at: reach standard library:195:8:function exp)'],
            msg: 'informTimeout',
            who: 'Bob'
            });
          
          return;
          
          }
        else {
          const {data: [v374], secs: v376, time: v375, didSend: v101, from: v373 } = txn5;
          ;
          const v377 = stdlib.addressEq(v321, v373);
          stdlib.assert(v377, {
            at: './index.rsh:77:9:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Bob'
            });
          const v384 = stdlib.safeAdd(v375, v306);
          const txn6 = await (ctc.recv({
            didSend: false,
            evt_cnt: 2,
            funcNum: 8,
            out_tys: [ctc0, ctc0],
            timeoutAt: ['time', v384],
            waitIfNotPresent: false
            }));
          if (txn6.didTimeout) {
            const txn7 = await (ctc.sendrecv({
              args: [v304, v305, v306, v321, v334, v358, v374, v384],
              evt_cnt: 0,
              funcNum: 9,
              lct: v375,
              onlyIf: true,
              out_tys: [],
              pay: [stdlib.checkedBigNumberify('reach standard library:197:11:decimal', stdlib.UInt_max, '0'), []],
              sim_p: (async (txn7) => {
                const sim_r = { txns: [], mapRefs: [], maps: [] };
                let sim_txn_ctr = stdlib.UInt_max;
                const getSimTokCtr = () => { sim_txn_ctr = sim_txn_ctr.sub(1); return sim_txn_ctr; };
                
                
                const {data: [], secs: v401, time: v400, didSend: v138, from: v399 } = txn7;
                
                ;
                sim_r.txns.push({
                  kind: 'from',
                  to: v321,
                  tok: undefined /* Nothing */
                  });
                sim_r.txns.push({
                  kind: 'halt',
                  tok: undefined /* Nothing */
                  })
                sim_r.isHalt = true;
                
                return sim_r;
                }),
              soloSend: false,
              timeoutAt: undefined /* mto */,
              tys: [ctc3, ctc0, ctc0, ctc3, ctc0, ctc2, ctc0, ctc0],
              waitIfNotPresent: false
              }));
            const {data: [], secs: v401, time: v400, didSend: v138, from: v399 } = txn7;
            ;
            const v402 = stdlib.addressEq(v304, v399);
            const v403 = stdlib.addressEq(v321, v399);
            const v404 = v402 ? true : v403;
            stdlib.assert(v404, {
              at: 'reach standard library:197:11:dot',
              fs: ['at ./index.rsh:86:53:application call to "closeTo" (defined at: reach standard library:195:8:function exp)'],
              msg: 'sender correct',
              who: 'Bob'
              });
            ;
            stdlib.protect(ctc1, await interact.informTimeout(), {
              at: './index.rsh:41:29:application',
              fs: ['at ./index.rsh:40:9:application call to [unknown function] (defined at: ./index.rsh:40:27:function exp)', 'at reach standard library:200:8:application call to "after" (defined at: ./index.rsh:39:28:function exp)', 'at ./index.rsh:86:53:application call to "closeTo" (defined at: reach standard library:195:8:function exp)'],
              msg: 'informTimeout',
              who: 'Bob'
              });
            
            return;
            
            }
          else {
            const {data: [v389, v390], secs: v392, time: v391, didSend: v111, from: v388 } = txn6;
            ;
            const v393 = stdlib.addressEq(v304, v388);
            stdlib.assert(v393, {
              at: './index.rsh:85:11:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Bob'
              });
            const v394 = stdlib.digest([ctc0, ctc0], [v389, v390]);
            const v395 = stdlib.digestEq(v358, v394);
            stdlib.assert(v395, {
              at: 'reach standard library:69:17:application',
              fs: ['at ./index.rsh:87:20:application call to "checkCommitment" (defined at: reach standard library:68:8:function exp)'],
              msg: null,
              who: 'Bob'
              });
            const v396 = stdlib.safeSub(stdlib.checkedBigNumberify('./index.rsh:7:18:decimal', stdlib.UInt_max, '4'), v374);
            const v397 = stdlib.safeAdd(v390, v396);
            const v398 = stdlib.safeMod(v397, stdlib.checkedBigNumberify('./index.rsh:7:34:decimal', stdlib.UInt_max, '3'));
            const cv326 = v398;
            const cv327 = v391;
            const cv334 = v334;
            
            v326 = cv326;
            v327 = cv327;
            v334 = cv334;
            
            txn3 = txn6;
            continue;}
          
          }
        
        }
      
      }
    const v453 = stdlib.eq(v326, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, '2'));
    const v456 = stdlib.safeMul(stdlib.checkedBigNumberify('./index.rsh:94:12:decimal', stdlib.UInt_max, '2'), v305);
    const v458 = v453 ? v304 : v321;
    ;
    stdlib.protect(ctc1, await interact.seeOutcome(v326), {
      at: './index.rsh:98:24:application',
      fs: ['at ./index.rsh:97:7:application call to [unknown function] (defined at: ./index.rsh:97:25:function exp)'],
      msg: 'seeOutcome',
      who: 'Bob'
      });
    
    return;
    }
  
  
  
  };
const _ALGO = {
  ABI: {
    impure: [`_reachp_0((uint64,uint64,uint64))void`, `_reachp_1((uint64))void`, `_reachp_2((uint64))void`, `_reachp_4((uint64,digest))void`, `_reachp_5((uint64))void`, `_reachp_6((uint64,uint64))void`, `_reachp_7((uint64))void`, `_reachp_8((uint64,uint64,uint64))void`, `_reachp_9((uint64))void`],
    pure: [],
    sigs: [`_reachp_0((uint64,uint64,uint64))void`, `_reachp_1((uint64))void`, `_reachp_2((uint64))void`, `_reachp_4((uint64,digest))void`, `_reachp_5((uint64))void`, `_reachp_6((uint64,uint64))void`, `_reachp_7((uint64))void`, `_reachp_8((uint64,uint64,uint64))void`, `_reachp_9((uint64))void`]
    },
  GlobalNumByteSlice: 3,
  GlobalNumUint: 0,
  LocalNumByteSlice: 0,
  LocalNumUint: 0,
  appApproval: `CCANAAEIKFAFIAcJEAMCeCYDAAEAAQExGEEDtShkSSJbNQEkWzUCKWQqZFCCCQQbbvlQBHUO5AYEhGNipQSkcTyWBN/ZIygE4lfE+QTlOmzuBOxvOQ8E/0yWvTYaAI4JAygDEgMdAvMDBwM+AzMDVANJADEANRA0CyJbNQw0CyRbNRM0CyEJWzUSgAT3cRNNNAwWUDQTFlA0EhZQsDQMiAOWNBOIA58yBjQSCDUNNBA0ExZQNBIWUDQNFlAhBK9QIzIGNQI1ASlLAVcAf2cqTFd/CWcoNAEWNAIWUGcxGSISRIgD4DQDQAAKgAQVH3x1NARQsCNDMQA1DyM0ARJEiAOsNAsXNQyABNUVGRQ0DBZQsDQMiAMhMgY0DQxENBOIAyQjNRgyBjUXNBNJCDUONBgjEkECxDQXNBIINQs0EDQTFlA0EhZQNA9QNA4WUDQLFlAlr1AhBTIGQv9nIzQBEkSIA0o0Cxc1DIAEl073FzQMFlCwNAyIAr8yBjQND0Q0EzQQiAK8MRkhBRJEiAM2IjIKMgmIA2pC/0shBTQBEkSIAug0DCJbNQ00DFcIIDURgARPbztENA0WUDQRULA0DYgCdTIGNAsMRDQQMQASRDIGNBIINRQ0EDQTFlA0EhZQNA9QNA4WUDQRUDQUFlAkr1AhBzIGQv7LIQU0ARJEiAKKNAwXNQ2ABIGqms80DRZQsDQNiAIiMgY0Cw9ENBAxABI0DzEAEhFENA40D4gCE0L/VCEHNAESRIgCJzQLIls1DTQLJFs1DIAESiHL/DQNFlA0DBZQsDQNiAHdMgY0FAxENA8xABJEMgY0Egg1DTQQNBMWUDQSFlA0D1A0DhZQNBFQNAwWUDQNFlAhCDIGQv4yIQc0ARJEiAHINAsXNQyABHGosaM0DBZQsDQMiAGJMgY0FA9ENBAxABI0DzEAEhFENA40EIgBekL+uyEINAESRIgB/TQLIls1FDQLJFs1FjQLIQlbNRWABBKGW8A0FBZQNBYWUDQVFlCwNBSIATkyBjQNDEQ0EDEAEkQ0ETQWFjQVFlABEkQ0FYEENAwJCCEKGDUYMgY1F0L+BCEINAESRIgBnTQLFzUMgARjV1FcNAwWULA0DIgA7zIGNA0PRDQQMQASNA8xABIRRDQONA+IAOBC/iGIAMyBoI0GNAYINQY2GgE1C0L8+ogAuDYaATULQv1yiACtNhoBNQtC/c2IAKI2GgE1DEL+AIgAlzYaATUMQv5TiACMNhoBNQtC/oKIAIE2GgE1C0L+1ogAdjYaATULQv8FiABrNhoBNQtC/1oiMTQSRCEKMTUSRCIxNhJEIjE3EkSIAEuBiAGvIiJC/NExGSISREL88CELNBMLNA80EDQYIQsSTYgANkL9dyKyASOyELIHsgiziUiJTAlJNQYyCYgAG4kJSUH/7kk1BogAE4kjNQOJSSISTDQCEhFEibFC/8kxFjQAIwhJNQAJRwI4BzIKEkQ4ECMSRDgIEkSJSVcAIDUQSSEGWzUTSSVbNRJJVzAgNQ9JIQRbNQ5JV1ggNREhDFs1FIlJVwAgNRBJIQZbNRNJJVs1EklXMCA1D0khBFs1DoFYWzULiUlXACA1EEkhBls1E0klWzUSgTBbNQ2JNAY0B0oPQf9PQv9XSVcAIDUQSSEGWzUTSSVbNRJJVzAgNQ9JIQRbNQ5JV1ggNRFJIQxbNQyBgAFbNQ2JsbIJQv8I`,
  appApprovalMap: {
    0: `2`,
    1: `2`,
    10: `2`,
    100: `23`,
    1000: `585`,
    1001: `586`,
    1002: `586`,
    1003: `586`,
    1004: `587`,
    1005: `588`,
    1006: `589`,
    1007: `590`,
    1008: `590`,
    1009: `590`,
    101: `23`,
    1010: `592`,
    1011: `592`,
    1012: `593`,
    1013: `594`,
    1014: `595`,
    1015: `597`,
    1016: `597`,
    1017: `597`,
    1018: `599`,
    1019: `599`,
    102: `23`,
    1020: `600`,
    1021: `600`,
    1022: `601`,
    1023: `603`,
    1024: `603`,
    1025: `604`,
    1026: `604`,
    1027: `605`,
    1028: `605`,
    1029: `606`,
    103: `23`,
    1030: `606`,
    1031: `607`,
    1032: `608`,
    1033: `609`,
    1034: `609`,
    1035: `609`,
    1036: `610`,
    1037: `610`,
    1038: `610`,
    1039: `612`,
    104: `23`,
    1040: `613`,
    1041: `613`,
    1042: `614`,
    1043: `615`,
    1044: `615`,
    1045: `616`,
    1046: `616`,
    1047: `617`,
    1048: `617`,
    1049: `618`,
    105: `23`,
    1050: `619`,
    1051: `621`,
    1052: `622`,
    1053: `624`,
    1054: `625`,
    1055: `626`,
    1056: `627`,
    1057: `627`,
    1058: `628`,
    1059: `628`,
    106: `23`,
    1060: `629`,
    1061: `629`,
    1062: `629`,
    1063: `630`,
    1064: `632`,
    1065: `633`,
    1066: `634`,
    1067: `634`,
    1068: `634`,
    1069: `635`,
    107: `23`,
    1070: `636`,
    1071: `636`,
    1072: `637`,
    1073: `637`,
    1074: `637`,
    1075: `638`,
    1076: `640`,
    1077: `641`,
    1078: `641`,
    1079: `642`,
    108: `23`,
    1080: `644`,
    1081: `645`,
    1082: `646`,
    1083: `647`,
    1084: `648`,
    1085: `648`,
    1086: `649`,
    1087: `650`,
    1088: `651`,
    1089: `652`,
    109: `23`,
    1090: `654`,
    1091: `655`,
    1092: `655`,
    1093: `655`,
    1094: `658`,
    1095: `658`,
    1096: `659`,
    1097: `659`,
    1098: `660`,
    1099: `661`,
    11: `2`,
    110: `23`,
    1100: `662`,
    1101: `663`,
    1102: `663`,
    1103: `664`,
    1104: `665`,
    1105: `665`,
    1106: `666`,
    1107: `666`,
    1108: `667`,
    1109: `667`,
    111: `23`,
    1110: `668`,
    1111: `669`,
    1112: `670`,
    1113: `670`,
    1114: `671`,
    1115: `672`,
    1116: `673`,
    1117: `674`,
    1118: `674`,
    1119: `675`,
    112: `23`,
    1120: `676`,
    1121: `677`,
    1122: `679`,
    1123: `680`,
    1124: `680`,
    1125: `680`,
    1126: `681`,
    1127: `681`,
    1128: `682`,
    1129: `683`,
    113: `23`,
    1130: `683`,
    1131: `684`,
    1132: `685`,
    1133: `685`,
    1134: `686`,
    1135: `687`,
    1136: `688`,
    1137: `689`,
    1138: `689`,
    1139: `690`,
    114: `25`,
    1140: `691`,
    1141: `691`,
    1142: `691`,
    1143: `692`,
    1144: `692`,
    1145: `693`,
    1146: `694`,
    1147: `694`,
    1148: `695`,
    1149: `696`,
    115: `27`,
    1150: `696`,
    1151: `697`,
    1152: `698`,
    1153: `698`,
    1154: `698`,
    1155: `699`,
    1156: `699`,
    1157: `700`,
    1158: `700`,
    1159: `701`,
    116: `27`,
    1160: `702`,
    1161: `702`,
    1162: `703`,
    1163: `705`,
    1164: `706`,
    1165: `706`,
    1166: `706`,
    1167: `707`,
    1168: `707`,
    1169: `708`,
    117: `28`,
    1170: `709`,
    1171: `709`,
    1172: `710`,
    1173: `711`,
    1174: `711`,
    1175: `712`,
    1176: `713`,
    1177: `714`,
    1178: `715`,
    1179: `715`,
    118: `28`,
    1180: `716`,
    1181: `717`,
    1182: `717`,
    1183: `717`,
    1184: `718`,
    1185: `718`,
    1186: `719`,
    1187: `720`,
    1188: `720`,
    1189: `721`,
    119: `29`,
    1190: `722`,
    1191: `722`,
    1192: `723`,
    1193: `723`,
    1194: `724`,
    1195: `725`,
    1196: `725`,
    1197: `726`,
    1198: `728`,
    1199: `729`,
    12: `2`,
    120: `29`,
    1200: `729`,
    1201: `729`,
    1202: `730`,
    1203: `730`,
    1204: `731`,
    1205: `732`,
    1206: `732`,
    1207: `733`,
    1208: `734`,
    1209: `734`,
    121: `30`,
    1210: `735`,
    1211: `736`,
    1212: `737`,
    1213: `738`,
    1214: `738`,
    1215: `739`,
    1216: `739`,
    1217: `740`,
    1218: `741`,
    1219: `741`,
    122: `31`,
    1220: `742`,
    1221: `744`,
    1222: `744`,
    1223: `745`,
    1224: `745`,
    1225: `746`,
    1226: `747`,
    1227: `748`,
    1228: `748`,
    1229: `748`,
    123: `32`,
    1230: `749`,
    1231: `749`,
    1232: `749`,
    1233: `751`,
    1234: `752`,
    1235: `752`,
    1236: `752`,
    1237: `753`,
    1238: `753`,
    1239: `754`,
    124: `32`,
    1240: `755`,
    1241: `755`,
    1242: `756`,
    1243: `757`,
    1244: `757`,
    1245: `758`,
    1246: `759`,
    1247: `760`,
    1248: `761`,
    1249: `761`,
    125: `33`,
    1250: `762`,
    1251: `763`,
    1252: `763`,
    1253: `763`,
    1254: `764`,
    1255: `764`,
    1256: `765`,
    1257: `766`,
    1258: `766`,
    1259: `767`,
    126: `33`,
    1260: `768`,
    1261: `768`,
    1262: `769`,
    1263: `770`,
    1264: `770`,
    1265: `770`,
    1266: `771`,
    1267: `771`,
    1268: `772`,
    1269: `773`,
    127: `34`,
    1270: `773`,
    1271: `774`,
    1272: `775`,
    1273: `775`,
    1274: `776`,
    1275: `776`,
    1276: `776`,
    1277: `777`,
    1278: `778`,
    1279: `778`,
    128: `35`,
    1280: `779`,
    1281: `781`,
    1282: `782`,
    1283: `782`,
    1284: `783`,
    129: `36`,
    13: `2`,
    130: `36`,
    131: `37`,
    132: `37`,
    133: `38`,
    134: `38`,
    135: `39`,
    136: `40`,
    137: `40`,
    138: `41`,
    139: `41`,
    14: `2`,
    140: `41`,
    141: `41`,
    142: `41`,
    143: `41`,
    144: `42`,
    145: `42`,
    146: `43`,
    147: `44`,
    148: `45`,
    149: `45`,
    15: `2`,
    150: `46`,
    151: `47`,
    152: `48`,
    153: `48`,
    154: `49`,
    155: `50`,
    156: `51`,
    157: `53`,
    158: `53`,
    159: `54`,
    16: `2`,
    160: `54`,
    161: `54`,
    162: `55`,
    163: `55`,
    164: `56`,
    165: `56`,
    166: `56`,
    167: `59`,
    168: `59`,
    169: `60`,
    17: `2`,
    170: `60`,
    171: `61`,
    172: `62`,
    173: `62`,
    174: `64`,
    175: `64`,
    176: `65`,
    177: `65`,
    178: `66`,
    179: `67`,
    18: `2`,
    180: `68`,
    181: `68`,
    182: `69`,
    183: `70`,
    184: `71`,
    185: `71`,
    186: `72`,
    187: `73`,
    188: `74`,
    189: `74`,
    19: `2`,
    190: `75`,
    191: `76`,
    192: `77`,
    193: `78`,
    194: `78`,
    195: `80`,
    196: `80`,
    197: `81`,
    198: `81`,
    199: `82`,
    2: `2`,
    20: `2`,
    200: `83`,
    201: `83`,
    202: `84`,
    203: `84`,
    204: `84`,
    205: `85`,
    206: `86`,
    207: `87`,
    208: `88`,
    209: `88`,
    21: `2`,
    210: `88`,
    211: `89`,
    212: `90`,
    213: `91`,
    214: `91`,
    215: `92`,
    216: `93`,
    217: `93`,
    218: `94`,
    219: `95`,
    22: `2`,
    220: `96`,
    221: `97`,
    222: `97`,
    223: `98`,
    224: `99`,
    225: `100`,
    226: `102`,
    227: `102`,
    228: `102`,
    229: `104`,
    23: `4`,
    230: `104`,
    231: `105`,
    232: `105`,
    233: `105`,
    234: `107`,
    235: `107`,
    236: `107`,
    237: `107`,
    238: `107`,
    239: `107`,
    24: `4`,
    240: `108`,
    241: `108`,
    242: `109`,
    243: `110`,
    244: `112`,
    245: `113`,
    246: `115`,
    247: `115`,
    248: `116`,
    249: `116`,
    25: `5`,
    250: `117`,
    251: `118`,
    252: `118`,
    253: `119`,
    254: `120`,
    255: `121`,
    256: `121`,
    257: `121`,
    258: `122`,
    259: `122`,
    26: `5`,
    260: `123`,
    261: `124`,
    262: `124`,
    263: `125`,
    264: `125`,
    265: `125`,
    266: `125`,
    267: `125`,
    268: `125`,
    269: `126`,
    27: `5`,
    270: `126`,
    271: `127`,
    272: `128`,
    273: `129`,
    274: `131`,
    275: `131`,
    276: `132`,
    277: `132`,
    278: `132`,
    279: `133`,
    28: `6`,
    280: `133`,
    281: `134`,
    282: `134`,
    283: `135`,
    284: `136`,
    285: `137`,
    286: `137`,
    287: `138`,
    288: `138`,
    289: `138`,
    29: `7`,
    290: `141`,
    291: `142`,
    292: `142`,
    293: `143`,
    294: `143`,
    295: `144`,
    296: `144`,
    297: `145`,
    298: `145`,
    299: `146`,
    3: `2`,
    30: `8`,
    300: `147`,
    301: `148`,
    302: `148`,
    303: `150`,
    304: `150`,
    305: `151`,
    306: `152`,
    307: `153`,
    308: `153`,
    309: `153`,
    31: `9`,
    310: `154`,
    311: `154`,
    312: `155`,
    313: `155`,
    314: `156`,
    315: `157`,
    316: `157`,
    317: `159`,
    318: `159`,
    319: `160`,
    32: `10`,
    320: `160`,
    321: `161`,
    322: `162`,
    323: `163`,
    324: `163`,
    325: `164`,
    326: `165`,
    327: `166`,
    328: `166`,
    329: `167`,
    33: `11`,
    330: `168`,
    331: `168`,
    332: `169`,
    333: `170`,
    334: `171`,
    335: `171`,
    336: `172`,
    337: `173`,
    338: `174`,
    339: `175`,
    34: `11`,
    340: `176`,
    341: `177`,
    342: `177`,
    343: `178`,
    344: `178`,
    345: `179`,
    346: `179`,
    347: `179`,
    348: `181`,
    349: `182`,
    35: `12`,
    350: `182`,
    351: `183`,
    352: `184`,
    353: `185`,
    354: `185`,
    355: `185`,
    356: `186`,
    357: `186`,
    358: `187`,
    359: `188`,
    36: `13`,
    360: `188`,
    361: `189`,
    362: `189`,
    363: `189`,
    364: `189`,
    365: `189`,
    366: `189`,
    367: `190`,
    368: `190`,
    369: `191`,
    37: `14`,
    370: `192`,
    371: `193`,
    372: `195`,
    373: `195`,
    374: `196`,
    375: `196`,
    376: `196`,
    377: `197`,
    378: `197`,
    379: `198`,
    38: `14`,
    380: `198`,
    381: `199`,
    382: `200`,
    383: `201`,
    384: `201`,
    385: `203`,
    386: `203`,
    387: `204`,
    388: `204`,
    389: `204`,
    39: `15`,
    390: `206`,
    391: `206`,
    392: `207`,
    393: `207`,
    394: `208`,
    395: `209`,
    396: `211`,
    397: `211`,
    398: `211`,
    399: `213`,
    4: `2`,
    40: `16`,
    400: `214`,
    401: `214`,
    402: `215`,
    403: `215`,
    404: `216`,
    405: `216`,
    406: `216`,
    407: `217`,
    408: `217`,
    409: `217`,
    41: `17`,
    410: `219`,
    411: `219`,
    412: `220`,
    413: `220`,
    414: `221`,
    415: `222`,
    416: `223`,
    417: `223`,
    418: `223`,
    419: `224`,
    42: `18`,
    420: `224`,
    421: `225`,
    422: `226`,
    423: `227`,
    424: `227`,
    425: `228`,
    426: `228`,
    427: `229`,
    428: `229`,
    429: `229`,
    43: `19`,
    430: `230`,
    431: `230`,
    432: `231`,
    433: `231`,
    434: `231`,
    435: `231`,
    436: `231`,
    437: `231`,
    438: `232`,
    439: `232`,
    44: `21`,
    440: `233`,
    441: `234`,
    442: `235`,
    443: `235`,
    444: `236`,
    445: `237`,
    446: `239`,
    447: `239`,
    448: `240`,
    449: `240`,
    45: `21`,
    450: `240`,
    451: `241`,
    452: `241`,
    453: `242`,
    454: `242`,
    455: `243`,
    456: `244`,
    457: `245`,
    458: `245`,
    459: `246`,
    46: `21`,
    460: `246`,
    461: `247`,
    462: `248`,
    463: `251`,
    464: `251`,
    465: `252`,
    466: `252`,
    467: `253`,
    468: `254`,
    469: `254`,
    47: `21`,
    470: `256`,
    471: `256`,
    472: `257`,
    473: `257`,
    474: `258`,
    475: `259`,
    476: `260`,
    477: `260`,
    478: `261`,
    479: `262`,
    48: `21`,
    480: `263`,
    481: `263`,
    482: `264`,
    483: `265`,
    484: `265`,
    485: `266`,
    486: `267`,
    487: `268`,
    488: `268`,
    489: `269`,
    49: `21`,
    490: `270`,
    491: `270`,
    492: `271`,
    493: `272`,
    494: `273`,
    495: `274`,
    496: `275`,
    497: `276`,
    498: `276`,
    499: `277`,
    5: `2`,
    50: `21`,
    500: `277`,
    501: `278`,
    502: `278`,
    503: `278`,
    504: `280`,
    505: `280`,
    506: `281`,
    507: `281`,
    508: `282`,
    509: `283`,
    51: `21`,
    510: `284`,
    511: `284`,
    512: `284`,
    513: `285`,
    514: `285`,
    515: `286`,
    516: `287`,
    517: `287`,
    518: `288`,
    519: `288`,
    52: `21`,
    520: `288`,
    521: `288`,
    522: `288`,
    523: `288`,
    524: `289`,
    525: `289`,
    526: `290`,
    527: `291`,
    528: `292`,
    529: `294`,
    53: `21`,
    530: `294`,
    531: `295`,
    532: `295`,
    533: `295`,
    534: `296`,
    535: `296`,
    536: `297`,
    537: `297`,
    538: `298`,
    539: `299`,
    54: `21`,
    540: `300`,
    541: `300`,
    542: `301`,
    543: `301`,
    544: `302`,
    545: `303`,
    546: `303`,
    547: `304`,
    548: `304`,
    549: `305`,
    55: `21`,
    550: `306`,
    551: `307`,
    552: `311`,
    553: `311`,
    554: `313`,
    555: `313`,
    556: `314`,
    557: `314`,
    558: `314`,
    559: `315`,
    56: `21`,
    560: `315`,
    561: `315`,
    562: `317`,
    563: `317`,
    564: `318`,
    565: `318`,
    566: `319`,
    567: `320`,
    568: `321`,
    569: `321`,
    57: `21`,
    570: `321`,
    571: `322`,
    572: `322`,
    573: `323`,
    574: `324`,
    575: `325`,
    576: `325`,
    577: `326`,
    578: `326`,
    579: `327`,
    58: `21`,
    580: `328`,
    581: `329`,
    582: `329`,
    583: `330`,
    584: `330`,
    585: `330`,
    586: `330`,
    587: `330`,
    588: `330`,
    589: `331`,
    59: `21`,
    590: `331`,
    591: `332`,
    592: `333`,
    593: `334`,
    594: `334`,
    595: `335`,
    596: `336`,
    597: `337`,
    598: `339`,
    599: `339`,
    6: `2`,
    60: `21`,
    600: `340`,
    601: `340`,
    602: `340`,
    603: `341`,
    604: `341`,
    605: `342`,
    606: `342`,
    607: `343`,
    608: `344`,
    609: `345`,
    61: `21`,
    610: `345`,
    611: `346`,
    612: `346`,
    613: `347`,
    614: `348`,
    615: `351`,
    616: `351`,
    617: `352`,
    618: `352`,
    619: `353`,
    62: `21`,
    620: `354`,
    621: `354`,
    622: `356`,
    623: `356`,
    624: `357`,
    625: `357`,
    626: `358`,
    627: `359`,
    628: `360`,
    629: `360`,
    63: `21`,
    630: `361`,
    631: `362`,
    632: `363`,
    633: `363`,
    634: `364`,
    635: `365`,
    636: `365`,
    637: `366`,
    638: `367`,
    639: `368`,
    64: `21`,
    640: `368`,
    641: `369`,
    642: `370`,
    643: `370`,
    644: `371`,
    645: `372`,
    646: `373`,
    647: `373`,
    648: `374`,
    649: `375`,
    65: `21`,
    650: `376`,
    651: `376`,
    652: `377`,
    653: `377`,
    654: `378`,
    655: `378`,
    656: `378`,
    657: `380`,
    658: `380`,
    659: `381`,
    66: `21`,
    660: `381`,
    661: `382`,
    662: `383`,
    663: `384`,
    664: `384`,
    665: `384`,
    666: `385`,
    667: `385`,
    668: `386`,
    669: `387`,
    67: `21`,
    670: `387`,
    671: `388`,
    672: `388`,
    673: `388`,
    674: `388`,
    675: `388`,
    676: `388`,
    677: `389`,
    678: `389`,
    679: `390`,
    68: `21`,
    680: `391`,
    681: `392`,
    682: `394`,
    683: `394`,
    684: `395`,
    685: `395`,
    686: `395`,
    687: `396`,
    688: `396`,
    689: `397`,
    69: `21`,
    690: `397`,
    691: `398`,
    692: `399`,
    693: `400`,
    694: `400`,
    695: `401`,
    696: `401`,
    697: `402`,
    698: `403`,
    699: `403`,
    7: `2`,
    70: `21`,
    700: `404`,
    701: `404`,
    702: `405`,
    703: `406`,
    704: `407`,
    705: `411`,
    706: `411`,
    707: `413`,
    708: `413`,
    709: `414`,
    71: `21`,
    710: `414`,
    711: `414`,
    712: `415`,
    713: `415`,
    714: `415`,
    715: `417`,
    716: `417`,
    717: `418`,
    718: `418`,
    719: `419`,
    72: `21`,
    720: `420`,
    721: `421`,
    722: `421`,
    723: `421`,
    724: `422`,
    725: `422`,
    726: `423`,
    727: `424`,
    728: `425`,
    729: `425`,
    73: `21`,
    730: `426`,
    731: `426`,
    732: `427`,
    733: `428`,
    734: `429`,
    735: `429`,
    736: `430`,
    737: `430`,
    738: `431`,
    739: `431`,
    74: `21`,
    740: `432`,
    741: `433`,
    742: `433`,
    743: `434`,
    744: `434`,
    745: `434`,
    746: `434`,
    747: `434`,
    748: `434`,
    749: `435`,
    75: `21`,
    750: `435`,
    751: `436`,
    752: `437`,
    753: `438`,
    754: `438`,
    755: `439`,
    756: `440`,
    757: `441`,
    758: `441`,
    759: `442`,
    76: `21`,
    760: `443`,
    761: `444`,
    762: `446`,
    763: `446`,
    764: `447`,
    765: `447`,
    766: `447`,
    767: `448`,
    768: `448`,
    769: `449`,
    77: `21`,
    770: `449`,
    771: `450`,
    772: `451`,
    773: `452`,
    774: `452`,
    775: `453`,
    776: `453`,
    777: `454`,
    778: `455`,
    779: `458`,
    78: `21`,
    780: `458`,
    781: `459`,
    782: `459`,
    783: `460`,
    784: `461`,
    785: `461`,
    786: `462`,
    787: `463`,
    788: `464`,
    789: `465`,
    79: `21`,
    790: `466`,
    791: `470`,
    792: `470`,
    793: `471`,
    794: `471`,
    795: `472`,
    796: `472`,
    797: `473`,
    798: `474`,
    799: `475`,
    8: `2`,
    80: `21`,
    800: `475`,
    801: `476`,
    802: `477`,
    803: `477`,
    804: `478`,
    805: `478`,
    806: `479`,
    807: `479`,
    808: `480`,
    809: `480`,
    81: `21`,
    810: `480`,
    811: `482`,
    812: `482`,
    813: `483`,
    814: `483`,
    815: `484`,
    816: `485`,
    817: `486`,
    818: `486`,
    819: `486`,
    82: `21`,
    820: `487`,
    821: `487`,
    822: `488`,
    823: `489`,
    824: `489`,
    825: `490`,
    826: `490`,
    827: `490`,
    828: `490`,
    829: `490`,
    83: `21`,
    830: `490`,
    831: `491`,
    832: `491`,
    833: `492`,
    834: `493`,
    835: `494`,
    836: `496`,
    837: `496`,
    838: `497`,
    839: `497`,
    84: `21`,
    840: `497`,
    841: `498`,
    842: `498`,
    843: `499`,
    844: `499`,
    845: `500`,
    846: `501`,
    847: `502`,
    848: `502`,
    849: `503`,
    85: `21`,
    850: `503`,
    851: `504`,
    852: `505`,
    853: `505`,
    854: `506`,
    855: `506`,
    856: `507`,
    857: `508`,
    858: `509`,
    859: `513`,
    86: `21`,
    860: `513`,
    861: `515`,
    862: `515`,
    863: `516`,
    864: `516`,
    865: `516`,
    866: `517`,
    867: `517`,
    868: `517`,
    869: `519`,
    87: `21`,
    870: `519`,
    871: `519`,
    872: `520`,
    873: `520`,
    874: `520`,
    875: `520`,
    876: `522`,
    877: `522`,
    878: `523`,
    879: `524`,
    88: `21`,
    880: `524`,
    881: `525`,
    882: `525`,
    883: `525`,
    884: `526`,
    885: `526`,
    886: `527`,
    887: `527`,
    888: `527`,
    889: `529`,
    89: `21`,
    890: `529`,
    891: `529`,
    892: `530`,
    893: `530`,
    894: `530`,
    895: `531`,
    896: `531`,
    897: `532`,
    898: `532`,
    899: `532`,
    9: `2`,
    90: `21`,
    900: `534`,
    901: `534`,
    902: `534`,
    903: `535`,
    904: `535`,
    905: `535`,
    906: `536`,
    907: `536`,
    908: `537`,
    909: `537`,
    91: `22`,
    910: `537`,
    911: `539`,
    912: `539`,
    913: `539`,
    914: `540`,
    915: `540`,
    916: `540`,
    917: `541`,
    918: `541`,
    919: `542`,
    92: `22`,
    920: `542`,
    921: `542`,
    922: `544`,
    923: `544`,
    924: `544`,
    925: `545`,
    926: `545`,
    927: `545`,
    928: `546`,
    929: `546`,
    93: `22`,
    930: `547`,
    931: `547`,
    932: `547`,
    933: `549`,
    934: `549`,
    935: `549`,
    936: `550`,
    937: `550`,
    938: `550`,
    939: `551`,
    94: `23`,
    940: `551`,
    941: `552`,
    942: `552`,
    943: `552`,
    944: `554`,
    945: `554`,
    946: `554`,
    947: `555`,
    948: `555`,
    949: `555`,
    95: `23`,
    950: `556`,
    951: `556`,
    952: `557`,
    953: `557`,
    954: `557`,
    955: `559`,
    956: `559`,
    957: `559`,
    958: `560`,
    959: `560`,
    96: `23`,
    960: `560`,
    961: `561`,
    962: `561`,
    963: `562`,
    964: `562`,
    965: `562`,
    966: `564`,
    967: `564`,
    968: `564`,
    969: `565`,
    97: `23`,
    970: `565`,
    971: `565`,
    972: `566`,
    973: `566`,
    974: `567`,
    975: `567`,
    976: `567`,
    977: `569`,
    978: `570`,
    979: `570`,
    98: `23`,
    980: `571`,
    981: `572`,
    982: `573`,
    983: `573`,
    984: `574`,
    985: `574`,
    986: `575`,
    987: `576`,
    988: `577`,
    989: `578`,
    99: `23`,
    990: `578`,
    991: `579`,
    992: `580`,
    993: `581`,
    994: `582`,
    995: `582`,
    996: `583`,
    997: `584`,
    998: `585`,
    999: `585`
    },
  appClear: `CA==`,
  appClearMap: {
    },
  companionInfo: null,
  extraPages: 0,
  stateKeys: 2,
  stateSize: 136,
  unsupported: [],
  version: 13,
  warnings: []
  };
const _ETH = {
  ABI: `[{"inputs":[{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"},{"internalType":"uint256","name":"elem1","type":"uint256"},{"internalType":"uint256","name":"elem2","type":"uint256"}],"internalType":"struct T0","name":"v650","type":"tuple"}],"stateMutability":"payable","type":"constructor"},{"inputs":[{"internalType":"uint256","name":"msg","type":"uint256"}],"name":"ReachError","type":"error"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"address","name":"_who","type":"address"},{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"},{"internalType":"uint256","name":"elem1","type":"uint256"},{"internalType":"uint256","name":"elem2","type":"uint256"}],"indexed":false,"internalType":"struct T0","name":"_a","type":"tuple"}],"name":"_reach_e0","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"address","name":"_who","type":"address"},{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"}],"indexed":false,"internalType":"struct T2","name":"_a","type":"tuple"}],"name":"_reach_e1","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"address","name":"_who","type":"address"},{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"}],"indexed":false,"internalType":"struct T2","name":"_a","type":"tuple"}],"name":"_reach_e2","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"address","name":"_who","type":"address"},{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"},{"internalType":"uint256","name":"elem1","type":"uint256"}],"indexed":false,"internalType":"struct T4","name":"_a","type":"tuple"}],"name":"_reach_e4","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"address","name":"_who","type":"address"},{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"}],"indexed":false,"internalType":"struct T2","name":"_a","type":"tuple"}],"name":"_reach_e5","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"address","name":"_who","type":"address"},{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"},{"internalType":"uint256","name":"elem1","type":"uint256"}],"indexed":false,"internalType":"struct T7","name":"_a","type":"tuple"}],"name":"_reach_e6","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"address","name":"_who","type":"address"},{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"}],"indexed":false,"internalType":"struct T2","name":"_a","type":"tuple"}],"name":"_reach_e7","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"address","name":"_who","type":"address"},{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"},{"internalType":"uint256","name":"elem1","type":"uint256"},{"internalType":"uint256","name":"elem2","type":"uint256"}],"indexed":false,"internalType":"struct T0","name":"_a","type":"tuple"}],"name":"_reach_e8","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"address","name":"_who","type":"address"},{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"}],"indexed":false,"internalType":"struct T2","name":"_a","type":"tuple"}],"name":"_reach_e9","type":"event"},{"stateMutability":"payable","type":"fallback"},{"inputs":[],"name":"_reachCreationTime","outputs":[{"internalType":"uint256","name":"","type":"uint256"}],"stateMutability":"view","type":"function"},{"inputs":[],"name":"_reachCurrentState","outputs":[{"internalType":"uint256","name":"","type":"uint256"},{"internalType":"bytes","name":"","type":"bytes"}],"stateMutability":"view","type":"function"},{"inputs":[],"name":"_reachCurrentTime","outputs":[{"internalType":"uint256","name":"","type":"uint256"}],"stateMutability":"view","type":"function"},{"inputs":[{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"}],"internalType":"struct T2","name":"v656","type":"tuple"}],"name":"_reachp_1","outputs":[{"internalType":"bool","name":"","type":"bool"}],"stateMutability":"payable","type":"function"},{"inputs":[{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"}],"internalType":"struct T2","name":"v659","type":"tuple"}],"name":"_reachp_2","outputs":[{"internalType":"bool","name":"","type":"bool"}],"stateMutability":"payable","type":"function"},{"inputs":[{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"},{"internalType":"uint256","name":"elem1","type":"uint256"}],"internalType":"struct T4","name":"v662","type":"tuple"}],"name":"_reachp_4","outputs":[{"internalType":"bool","name":"","type":"bool"}],"stateMutability":"payable","type":"function"},{"inputs":[{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"}],"internalType":"struct T2","name":"v665","type":"tuple"}],"name":"_reachp_5","outputs":[{"internalType":"bool","name":"","type":"bool"}],"stateMutability":"payable","type":"function"},{"inputs":[{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"},{"internalType":"uint256","name":"elem1","type":"uint256"}],"internalType":"struct T7","name":"v668","type":"tuple"}],"name":"_reachp_6","outputs":[{"internalType":"bool","name":"","type":"bool"}],"stateMutability":"payable","type":"function"},{"inputs":[{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"}],"internalType":"struct T2","name":"v671","type":"tuple"}],"name":"_reachp_7","outputs":[{"internalType":"bool","name":"","type":"bool"}],"stateMutability":"payable","type":"function"},{"inputs":[{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"},{"internalType":"uint256","name":"elem1","type":"uint256"},{"internalType":"uint256","name":"elem2","type":"uint256"}],"internalType":"struct T0","name":"v677","type":"tuple"}],"name":"_reachp_8","outputs":[{"internalType":"bool","name":"","type":"bool"}],"stateMutability":"payable","type":"function"},{"inputs":[{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"}],"internalType":"struct T2","name":"v680","type":"tuple"}],"name":"_reachp_9","outputs":[{"internalType":"bool","name":"","type":"bool"}],"stateMutability":"payable","type":"function"},{"stateMutability":"payable","type":"receive"}]`,
  Bytecode: `0x608062001e909081380391601f1980601f85011683019360018060401b0392848610848711176200036f578160609286926040988952833981010312620003855783519260608401848110848211176200036f5785528051845260209385858301519286830193845201519486820195865243600355865191818301838110878211176200036f578852600080935260049060ff82541662000358577f4f453854b6a90dba7951e2aeeb8854b2b5f80576fe444dd363a967d18d9175e460808a5133815283518682015287518c8201528a516060820152a15180159081156200034b575b50156200033457835134036200031d5787519360808501858110888211176200030a57895282850197848952898601908582526060870192868452338852518a5280518252514301804311620002f757438110620002f357825260018086554381558a5196516001600160a01b0316858801529851868b015251606086015251608080860191909152845260a0840186811185821017620002e05788528351958611620002cd57600254908782811c92168015620002c2575b83831014620002af5750601f811162000263575b508093601f8611600114620001fb57505091839491849394620001ef575b50501b916000199060031b1c1916176002555b51611b0590816200038b8239f35b015192503880620001ce565b600283528183209493928692918316915b888383106200024857505050106200022e575b505050811b01600255620001e1565b015160001960f88460031b161c191690553880806200021f565b8587015188559096019594850194879350908101906200020c565b60028352818320601f870160051c810191838810620002a4575b601f0160051c019087905b82811062000298575050620001b0565b84815501879062000288565b90915081906200027d565b634e487b7160e01b845260229052602483fd5b91607f16916200019c565b634e487b7160e01b835260419052602482fd5b634e487b7160e01b845260418252602484fd5b8580fd5b634e487b7160e01b865260118452602486fd5b634e487b7160e01b855260418352602485fd5b602490600989519163100960cb60e01b8352820152fd5b602490600889519163100960cb60e01b8352820152fd5b90506001541438620000e3565b885163100960cb60e01b8152600781840152602490fd5b634e487b7160e01b600052604160045260246000fd5b600080fdfe608080604052600436101561001a575b50361561001857005b005b60003560e01c9081631e93b0f11461163b575080632f132302146114aa57806330dd5d0814611307578063573b85101461100b5780638323075714610fed578063ab53f2c614610f79578063b3722a9914610de5578063b559601a146108e0578063e3342bfa146105d7578063ee9fe9921461022a5763f5a239bc146100a0573861000f565b60203660031901126102255760006040516100ba81611690565b526100c4366117b2565b60016000540361020c576100e86100d96116fd565b602080825183010191016117f6565b9060ff600454166101f3577f794b69bffed607ab45148da3c7f9c613ba8e4d2d82b625153563a3a2f536190a6040518061012384338361184b565b0390a15180159081156101e7575b50156101ce57606081015143106101b5573461019c57600080808093602060018060a01b0382511691015190828215610193575bf1156101875760008055600060015561017c6118b4565b602060405160008152f35b6040513d6000823e3d90fd5b506108fc610165565b60405163100960cb60e01b815260136004820152602490fd5b60405163100960cb60e01b815260126004820152602490fd5b60405163100960cb60e01b815260116004820152602490fd5b90506001541438610131565b60405163100960cb60e01b815260106004820152602490fd5b60405163100960cb60e01b8152600f6004820152602490fd5b600080fd5b604036600319011261022557600060405161024481611690565b5261024e36611940565b6007600054036105be576102726102636116fd565b602080825183010191016119fd565b60ff600454166105a5577f9449691f9246ec4b81aeb2f34bd8715f4e442367780c7ac745057e0a3a212bd6604051806102ac8533836119d6565b0390a181518015908115610599575b50156105805760c08101514310156105675734610550576060810151336001600160a01b039091160361053757610381906020604051936102fb856116c6565b60008552600082860152600060408601526000606086015260006080860152600060a0860152600060c0860152600060e086015260018060a01b038351168552818301518286015260a0604084019384516040880152600180831b03606082015116606088015260808101516080880152015160a0860152015160c0840152514361192c565b60e0820152600960005560019043825560e06040519160018060a01b038151166020840152602081015160408401526040810151606084015260018060a01b036060820151166080840152608081015160a084015260a081015160c084015260c08101518284015201516101009081830152815261012081019060018060401b03918181108382111761052157604052805191821161052157610425600254611656565b601f81116104d9575b50602090601f831160011461047357928293918392600094610468575b50501b916000199060031b1c191617600255602060405160008152f35b01519250848061044b565b90601f1983169160026000528360206000209360005b878282106104c0575050106104a7575b505050811b0160025561017c565b015160001960f88460031b161c19169055828080610499565b8486015187559095019460209485019487935001610489565b600260005261051190600080516020611ad9833981519152601f850160051c81019160208610610517575b601f0160051c019061189d565b8361042e565b9091508190610504565b634e487b7160e01b600052604160045260246000fd5b60405163100960cb60e01b815260256004820152602490fd5b602460405163100960cb60e01b8152816004820152fd5b60405163100960cb60e01b815260236004820152602490fd5b60405163100960cb60e01b815260226004820152602490fd5b905060015414836102bb565b60405163100960cb60e01b815260216004820152602490fd5b60405163100960cb60e01b815260206004820152602490fd5b60403660031901126102255760006040516105f181611690565b526105fb36611940565b6005600054036108c75761060d6116fd565b90610622602092838082518301019101611978565b60ff600454166108ae577f2b488b46f65093e5cc34b7a23106f60902da34f9221ba5c98d129851d4f8efe66040518061065c8533836119d6565b0390a1815180159081156108a2575b50156108895760a081015143101561087057346108575780516001600160a01b039190339083160361083e5761069f611865565b9180825116835284820151938584019485526040830191825193604086019485526106f0886080846060850151169360608a019485520151946080890195865201519460a08801958652514361192c565b9460c087019586526007600055600197438955836040519851168a890152516040880152516060870152511660808501525160a08401525160c08301525160e082015260e08152610740816116c6565b8051906001600160401b0382116105215761075c600254611656565b601f811161080d575b508390601f83116001146107a85792829391839260009461079d575b50501b916000199060031b1c1916176002555b60405160008152f35b015192508580610781565b90601f19831691600260005283866000209360005b88888383106107f657505050106107dd575b505050811b01600255610794565b015160001960f88460031b161c191690558380806107cf565b8686015188559096019594850194879350016107bd565b61083890600260005285600020601f850160051c81019187861061051757601f0160051c019061189d565b84610765565b60405163100960cb60e01b815260196004820152602490fd5b60405163100960cb60e01b815260186004820152602490fd5b60405163100960cb60e01b815260176004820152602490fd5b60405163100960cb60e01b815260166004820152602490fd5b9050600154148461066b565b60405163100960cb60e01b815260156004820152602490fd5b60405163100960cb60e01b815260146004820152602490fd5b60603660031901126102255760006040516108fa81611690565b526040516001600160401b0360608201818111838210176105215760405260043582526020820191602435835260408101926044358452600960005403610dcc576109556109466116fd565b60208082518301019101611a65565b9160ff60045416610db3577fd6d1c39c0a5020e00dd6931b7ce85fc33eab108766c9c35e10ee90df64121f446080604051338152835160208201528451604082015287516060820152a1518015908115610da7575b5015610d8e5760e0820151431015610d755734610d5c578151336001600160a01b0390911603610d435760a0820151905190845190604051916020830193845260408301526040825260608201928284108685111761052157836040528251902003610d2c575050610a1a611865565b9260018060a01b038251168452602082015160208501526040820151604085015260018060a01b0360608301511660608501525160c082015160040360048111610cff57610a6c60039160809361192c565b069182828601524360a0860152015160c084015260018114600014610c4f5750610b0160405192610a9c846116ab565b6000808552602080860182815260408088018481526060808a0186815260808b0187815260a0808d019890985288516001600160a01b039081168d52968901519095529287018051909252860151909316905260c0840151905291015190519061192c565b60a083015260056000554360015560a060405192600180831b0381511660208501526020810151604085015260408101516060850152600180831b036060820151166080850152608081015182850152015160c083015260c08252610b65826116e2565b815190811161052157610b79600254611656565b601f8111610c12575b50602091601f8211600114610bc757918192600092610bbc575b50508160011b916000199060031b1c191617600255602060405160008152f35b015190508280610b9c565b601f19821692600260005260206000209160005b858110610bfa575083600195106104a757505050811b0160025561017c565b91926020600181928685015181550194019201610bdb565b6002600052610c4990600080516020611ad9833981519152601f840160051c8101916020851061051757601f0160051c019061189d565b82610b82565b600214159050610d155780516001600160a01b0316906020905b0151906000918015908115610cbf575b5015610225576000808093819382908215610cb5575b6001600160a01b031690f11561018757600080556000600155610cb06118b4565b61017c565b6108fc9150610c8f565b909250600181901b906001600160ff1b0381168103610cff578193610ce957600291041483610c79565b634e487b7160e01b600052601260045260246000fd5b634e487b7160e01b600052601160045260246000fd5b60608101516001600160a01b031690602090610c69565b63100960cb60e01b82526032606490910152602490fd5b60405163100960cb60e01b815260316004820152602490fd5b60405163100960cb60e01b815260306004820152602490fd5b60405163100960cb60e01b8152602f6004820152602490fd5b60405163100960cb60e01b8152602e6004820152602490fd5b905060015414856109aa565b60405163100960cb60e01b8152602d6004820152602490fd5b60405163100960cb60e01b8152602c6004820152602490fd5b6020366003190112610225576000604051610dff81611690565b52610e09366117b2565b600960005403610f6057610e1e6109466116fd565b9060ff60045416610f47577fd64d3134781556c6a626b018733c1e4f7a8f056da03bbd004578b0e46c1b667e60405180610e5984338361184b565b0390a1518015908115610f3b575b5015610f225760e08101514310610f095734610ef05780516001600160a01b039081163303610ee25760015b15610ec9576000808084608082956060830151169101519082821561019357f1156101875760008055600060015561017c6118b4565b60405163100960cb60e01b815260386004820152602490fd5b338160608401511614610e93565b60405163100960cb60e01b815260376004820152602490fd5b60405163100960cb60e01b815260366004820152602490fd5b60405163100960cb60e01b815260356004820152602490fd5b90506001541482610e67565b60405163100960cb60e01b815260346004820152602490fd5b60405163100960cb60e01b815260336004820152602490fd5b3461022557600036600319011261022557600054610f956116fd565b6040519182528160206040818301528251908160408401526000935b828510610fd4575050606092506000838284010152601f80199101168101030190f35b8481018201518686016060015293810193859350610fb1565b34610225576000366003190112610225576020600154604051908152f35b60208060031936011261022557600060405161102681611690565b52611030366117b2565b60019081600054036112ee576110556110476116fd565b8480825183010191016117f6565b9060ff600454166112d5577fcf0e8bec53cd91fa87ecf8f6f405ac75914a22acdb92a3553ee5c294fee815966040518061109084338361184b565b0390a15180159081156112ca575b50156112b157606081015143101561129857828101908151340361127f576110c4611865565b90600160a01b60019003808251168352835191868401928352604001519360408401948552606084019033825286608086015260a0850190438252519060c08601918001825260405196611117886116ab565b6000885289880191600083526040890193600085528660608b01966000885260808c0199828d60008d5260a0019c60008e5251168d5251865284518752511686525187525190516111679161192c565b865260056000554388558360405197511689880152516040870152516060860152511660808401525160a08301525160c082015260c081526111a8816116e2565b8051906001600160401b038211610521576111c4600254611656565b601f811161124e575b508390601f83116001146112035792829391839260009461079d5750501b916000199060031b1c19161760025560405160008152f35b90601f19831691600260005283866000209360005b888883831061123757505050106107dd57505050811b01600255610794565b868601518855909601959485019487935001611218565b61127990600260005285600020601f850160051c81019187861061051757601f0160051c019061189d565b846111cd565b60405163100960cb60e01b8152600e6004820152602490fd5b60405163100960cb60e01b8152600d6004820152602490fd5b60405163100960cb60e01b8152600c6004820152602490fd5b90508254148461109e565b60405163100960cb60e01b8152600b6004820152602490fd5b60405163100960cb60e01b8152600a6004820152602490fd5b602036600319011261022557600060405161132181611690565b5261132b366117b2565b6005600054036114915761134f6113406116fd565b60208082518301019101611978565b9060ff60045416611478577f46f247599a5fa9114da586bc9a4d716618c03f0781a481e77299e07c647c12496040518061138a84338361184b565b0390a151801590811561146c575b50156114535760a0810151431061143a57346114215780516001600160a01b0390811633036114135760015b156113fa576000808084608082956060830151169101519082821561019357f1156101875760008055600060015561017c6118b4565b60405163100960cb60e01b8152601f6004820152602490fd5b3381606084015116146113c4565b60405163100960cb60e01b8152601e6004820152602490fd5b60405163100960cb60e01b8152601d6004820152602490fd5b60405163100960cb60e01b8152601c6004820152602490fd5b90506001541482611398565b60405163100960cb60e01b8152601b6004820152602490fd5b60405163100960cb60e01b8152601a6004820152602490fd5b60203660031901126102255760006040516114c481611690565b526114ce366117b2565b600760005403611622576114e36102636116fd565b9060ff60045416611609577fa4850b05c9188495196ad609440a82393348559ec3e1eb1c2ab8d784a9e9d4016040518061151e84338361184b565b0390a15180159081156115fd575b50156115e45760c081015143106115cb57346115b25780516001600160a01b0390811633036115a45760015b1561158b576000808084608082958251169101519082821561019357f1156101875760008055600060015561017c6118b4565b60405163100960cb60e01b8152602b6004820152602490fd5b338160608401511614611558565b60405163100960cb60e01b8152602a6004820152602490fd5b60405163100960cb60e01b815260296004820152602490fd5b60405163100960cb60e01b815260286004820152602490fd5b9050600154148261152c565b60405163100960cb60e01b815260276004820152602490fd5b60405163100960cb60e01b815260266004820152602490fd5b34610225576000366003190112610225576020906003548152f35b90600182811c92168015611686575b602083101461167057565b634e487b7160e01b600052602260045260246000fd5b91607f1691611665565b602081019081106001600160401b0382111761052157604052565b60c081019081106001600160401b0382111761052157604052565b61010081019081106001600160401b0382111761052157604052565b60e081019081106001600160401b0382111761052157604052565b60405190600060025461170f81611656565b808552600191808316908115611793575060011461174d575b5050829003601f01601f191682016001600160401b0381118382101761052157604052565b60026000908152602093509183600080516020611ad98339815191525b83851061177f57505050508301013880611728565b80548886018301529301928490820161176a565b919250506020925060ff191682850152151560051b8301013880611728565b60209060031901126102255760405190602082016001600160401b03811183821017610521576040526004358252565b51906001600160a01b038216820361022557565b90816080910312610225576040519060808201906001600160401b038211838310176105215760609160405261182b816117e2565b835260208101516020840152604081015160408401520151606082015290565b6001600160a01b0390911681529051602082015260400190565b60405190611872826116e2565b8160c06000918281528260208201528260408201528260608201528260808201528260a08201520152565b8181106118a8575050565b6000815560010161189d565b6118bf600254611656565b806118c75750565b601f81116001146118da57506000600255565b600260005261191f90601f0160051c600080516020611ad9833981519152017f405787fa12a823e0f2b7631cc41b3ba8828b3321ca811111fa75cd3aa3bb5acf61189d565b6000602081208160025555565b9190820191828111610cff57821061022557565b60409060031901126102255760408051919082016001600160401b038111838210176105215760405260043582526024356020830152565b908160c09103126102255760a060405191611992836116ab565b61199b816117e2565b835260208101516020840152604081015160408401526119bd606082016117e2565b606084015260808101516080840152015160a082015290565b6001600160a01b039091168152815160208083019190915290910151604082015260600190565b908160e09103126102255760c060405191611a17836116e2565b611a20816117e2565b83526020810151602084015260408101516040840152611a42606082016117e2565b60608401526080810151608084015260a081015160a0840152015160c082015290565b90816101009103126102255760e060405191611a80836116c6565b611a89816117e2565b83526020810151602084015260408101516040840152611aab606082016117e2565b60608401526080810151608084015260a081015160a084015260c081015160c0840152015160e08201529056fe405787fa12a823e0f2b7631cc41b3ba8828b3321ca811111fa75cd3aa3bb5acea164736f6c6343000811000a`,
  BytecodeLen: 7824,
  version: 9,
  views: {
    }
  };
export const _stateSourceMap = {
  1: {
    at: './index.rsh:51:11:after expr stmt semicolon',
    fs: [],
    msg: null,
    who: 'Module'
    },
  2: {
    at: 'reach standard library:199:11:after expr stmt semicolon',
    fs: ['at ./index.rsh:57:51:application call to "closeTo" (defined at: reach standard library:195:8:function exp)'],
    msg: null,
    who: 'Module'
    },
  4: {
    at: './index.rsh:95:11:after expr stmt semicolon',
    fs: [],
    msg: null,
    who: 'Module'
    },
  5: {
    at: './index.rsh:62:13:after expr stmt semicolon',
    fs: [],
    msg: null,
    who: 'Module'
    },
  6: {
    at: 'reach standard library:199:11:after expr stmt semicolon',
    fs: ['at ./index.rsh:70:53:application call to "closeTo" (defined at: reach standard library:195:8:function exp)'],
    msg: null,
    who: 'Module'
    },
  7: {
    at: './index.rsh:71:13:after expr stmt semicolon',
    fs: [],
    msg: null,
    who: 'Module'
    },
  8: {
    at: 'reach standard library:199:11:after expr stmt semicolon',
    fs: ['at ./index.rsh:78:53:application call to "closeTo" (defined at: reach standard library:195:8:function exp)'],
    msg: null,
    who: 'Module'
    },
  9: {
    at: './index.rsh:79:13:after expr stmt semicolon',
    fs: [],
    msg: null,
    who: 'Module'
    },
  10: {
    at: 'reach standard library:199:11:after expr stmt semicolon',
    fs: ['at ./index.rsh:86:53:application call to "closeTo" (defined at: reach standard library:195:8:function exp)'],
    msg: null,
    who: 'Module'
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
