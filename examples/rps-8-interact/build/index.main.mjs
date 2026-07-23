// Automatically generated with Reach 0.1.13 (2002b6c6*)
/* eslint-disable */
export const _version = '0.1.13';
export const _versionHash = '0.1.13 (2002b6c6*)';
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
    lct: stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:49:9:dot', stdlib.UInt_max, '0'),
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
      at: './examples/rps-8-interact/index.rsh:41:29:application',
      fs: ['at ./examples/rps-8-interact/index.rsh:40:9:application call to [unknown function] (defined at: ./examples/rps-8-interact/index.rsh:40:27:function exp)', 'at reach standard library:200:8:application call to "after" (defined at: ./examples/rps-8-interact/index.rsh:39:28:function exp)', 'at ./examples/rps-8-interact/index.rsh:57:51:application call to "closeTo" (defined at: reach standard library:195:8:function exp)'],
      msg: 'informTimeout',
      who: 'Alice'
      });
    
    return;
    
    }
  else {
    const {data: [], secs: v323, time: v322, didSend: v65, from: v321 } = txn2;
    const v325 = stdlib.add(v305, v305);
    ;
    let v326 = stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:makeEnum', stdlib.UInt_max, '1');
    let v327 = v322;
    let v334 = v325;
    
    let txn3 = txn2;
    while (await (async () => {
      const v342 = stdlib.eq(v326, stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:makeEnum', stdlib.UInt_max, '1'));
      
      return v342;})()) {
      const v349 = stdlib.safeAdd(v327, v306);
      const v353 = stdlib.protect(ctc0, await interact.getHand(), {
        at: './examples/rps-8-interact/index.rsh:65:42:application',
        fs: ['at ./examples/rps-8-interact/index.rsh:64:15:application call to [unknown function] (defined at: ./examples/rps-8-interact/index.rsh:64:19:function exp)'],
        msg: 'getHand',
        who: 'Alice'
        });
      const v354 = stdlib.protect(ctc0, await interact.random(), {
        at: 'reach standard library:64:31:application',
        fs: ['at ./examples/rps-8-interact/index.rsh:66:56:application call to "makeCommitment" (defined at: reach standard library:63:8:function exp)', 'at ./examples/rps-8-interact/index.rsh:64:15:application call to [unknown function] (defined at: ./examples/rps-8-interact/index.rsh:64:19:function exp)'],
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
        pay: [stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:69:11:decimal', stdlib.UInt_max, '0'), []],
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
          fs: ['at ./examples/rps-8-interact/index.rsh:70:53:application call to "closeTo" (defined at: reach standard library:195:8:function exp)'],
          msg: 'sender correct',
          who: 'Alice'
          });
        ;
        stdlib.protect(ctc2, await interact.informTimeout(), {
          at: './examples/rps-8-interact/index.rsh:41:29:application',
          fs: ['at ./examples/rps-8-interact/index.rsh:40:9:application call to [unknown function] (defined at: ./examples/rps-8-interact/index.rsh:40:27:function exp)', 'at reach standard library:200:8:application call to "after" (defined at: ./examples/rps-8-interact/index.rsh:39:28:function exp)', 'at ./examples/rps-8-interact/index.rsh:70:53:application call to "closeTo" (defined at: reach standard library:195:8:function exp)'],
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
          at: './examples/rps-8-interact/index.rsh:69:11:dot',
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
            fs: ['at ./examples/rps-8-interact/index.rsh:78:53:application call to "closeTo" (defined at: reach standard library:195:8:function exp)'],
            msg: 'sender correct',
            who: 'Alice'
            });
          ;
          stdlib.protect(ctc2, await interact.informTimeout(), {
            at: './examples/rps-8-interact/index.rsh:41:29:application',
            fs: ['at ./examples/rps-8-interact/index.rsh:40:9:application call to [unknown function] (defined at: ./examples/rps-8-interact/index.rsh:40:27:function exp)', 'at reach standard library:200:8:application call to "after" (defined at: ./examples/rps-8-interact/index.rsh:39:28:function exp)', 'at ./examples/rps-8-interact/index.rsh:78:53:application call to "closeTo" (defined at: reach standard library:195:8:function exp)'],
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
            at: './examples/rps-8-interact/index.rsh:77:9:dot',
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
            pay: [stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:85:11:decimal', stdlib.UInt_max, '0'), []],
            sim_p: (async (txn6) => {
              const sim_r = { txns: [], mapRefs: [], maps: [] };
              let sim_txn_ctr = stdlib.UInt_max;
              const getSimTokCtr = () => { sim_txn_ctr = sim_txn_ctr.sub(1); return sim_txn_ctr; };
              
              
              const {data: [v389, v390], secs: v392, time: v391, didSend: v111, from: v388 } = txn6;
              
              ;
              const v396 = stdlib.safeSub(stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:7:18:decimal', stdlib.UInt_max, '4'), v374);
              const v397 = stdlib.safeAdd(v390, v396);
              const v398 = stdlib.safeMod(v397, stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:7:34:decimal', stdlib.UInt_max, '3'));
              const cv326 = v398;
              const cv327 = v391;
              const cv334 = v334;
              
              await (async () => {
                const v326 = cv326;
                const v327 = cv327;
                const v334 = cv334;
                
                if (await (async () => {
                  const v342 = stdlib.eq(v326, stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:makeEnum', stdlib.UInt_max, '1'));
                  
                  return v342;})()) {
                  const v349 = stdlib.safeAdd(v327, v306);
                  sim_r.isHalt = false;
                  }
                else {
                  const v453 = stdlib.eq(v326, stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:makeEnum', stdlib.UInt_max, '2'));
                  const v456 = stdlib.safeMul(stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:94:12:decimal', stdlib.UInt_max, '2'), v305);
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
              fs: ['at ./examples/rps-8-interact/index.rsh:86:53:application call to "closeTo" (defined at: reach standard library:195:8:function exp)'],
              msg: 'sender correct',
              who: 'Alice'
              });
            ;
            stdlib.protect(ctc2, await interact.informTimeout(), {
              at: './examples/rps-8-interact/index.rsh:41:29:application',
              fs: ['at ./examples/rps-8-interact/index.rsh:40:9:application call to [unknown function] (defined at: ./examples/rps-8-interact/index.rsh:40:27:function exp)', 'at reach standard library:200:8:application call to "after" (defined at: ./examples/rps-8-interact/index.rsh:39:28:function exp)', 'at ./examples/rps-8-interact/index.rsh:86:53:application call to "closeTo" (defined at: reach standard library:195:8:function exp)'],
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
              at: './examples/rps-8-interact/index.rsh:85:11:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Alice'
              });
            const v394 = stdlib.digest([ctc0, ctc0], [v389, v390]);
            const v395 = stdlib.digestEq(v358, v394);
            stdlib.assert(v395, {
              at: 'reach standard library:69:17:application',
              fs: ['at ./examples/rps-8-interact/index.rsh:87:20:application call to "checkCommitment" (defined at: reach standard library:68:8:function exp)'],
              msg: null,
              who: 'Alice'
              });
            const v396 = stdlib.safeSub(stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:7:18:decimal', stdlib.UInt_max, '4'), v374);
            const v397 = stdlib.safeAdd(v390, v396);
            const v398 = stdlib.safeMod(v397, stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:7:34:decimal', stdlib.UInt_max, '3'));
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
    const v453 = stdlib.eq(v326, stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:makeEnum', stdlib.UInt_max, '2'));
    const v456 = stdlib.safeMul(stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:94:12:decimal', stdlib.UInt_max, '2'), v305);
    const v458 = v453 ? v304 : v321;
    ;
    stdlib.protect(ctc2, await interact.seeOutcome(v326), {
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
    at: './examples/rps-8-interact/index.rsh:54:25:application',
    fs: ['at ./examples/rps-8-interact/index.rsh:53:11:application call to [unknown function] (defined at: ./examples/rps-8-interact/index.rsh:53:15:function exp)'],
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
      const v326 = stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:makeEnum', stdlib.UInt_max, '1');
      const v327 = v322;
      const v334 = v325;
      
      if (await (async () => {
        const v342 = stdlib.eq(v326, stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:makeEnum', stdlib.UInt_max, '1'));
        
        return v342;})()) {
        const v349 = stdlib.safeAdd(v327, v306);
        sim_r.isHalt = false;
        }
      else {
        const v453 = stdlib.eq(v326, stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:makeEnum', stdlib.UInt_max, '2'));
        const v456 = stdlib.safeMul(stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:94:12:decimal', stdlib.UInt_max, '2'), v305);
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
      at: './examples/rps-8-interact/index.rsh:41:29:application',
      fs: ['at ./examples/rps-8-interact/index.rsh:40:9:application call to [unknown function] (defined at: ./examples/rps-8-interact/index.rsh:40:27:function exp)', 'at reach standard library:200:8:application call to "after" (defined at: ./examples/rps-8-interact/index.rsh:39:28:function exp)', 'at ./examples/rps-8-interact/index.rsh:57:51:application call to "closeTo" (defined at: reach standard library:195:8:function exp)'],
      msg: 'informTimeout',
      who: 'Bob'
      });
    
    return;
    
    }
  else {
    const {data: [], secs: v323, time: v322, didSend: v65, from: v321 } = txn2;
    const v325 = stdlib.add(v305, v305);
    ;
    let v326 = stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:makeEnum', stdlib.UInt_max, '1');
    let v327 = v322;
    let v334 = v325;
    
    let txn3 = txn2;
    while (await (async () => {
      const v342 = stdlib.eq(v326, stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:makeEnum', stdlib.UInt_max, '1'));
      
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
          fs: ['at ./examples/rps-8-interact/index.rsh:70:53:application call to "closeTo" (defined at: reach standard library:195:8:function exp)'],
          msg: 'sender correct',
          who: 'Bob'
          });
        ;
        stdlib.protect(ctc1, await interact.informTimeout(), {
          at: './examples/rps-8-interact/index.rsh:41:29:application',
          fs: ['at ./examples/rps-8-interact/index.rsh:40:9:application call to [unknown function] (defined at: ./examples/rps-8-interact/index.rsh:40:27:function exp)', 'at reach standard library:200:8:application call to "after" (defined at: ./examples/rps-8-interact/index.rsh:39:28:function exp)', 'at ./examples/rps-8-interact/index.rsh:70:53:application call to "closeTo" (defined at: reach standard library:195:8:function exp)'],
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
          at: './examples/rps-8-interact/index.rsh:69:11:dot',
          fs: [],
          msg: 'sender correct',
          who: 'Bob'
          });
        const v368 = stdlib.safeAdd(v359, v306);
        const v372 = stdlib.protect(ctc0, await interact.getHand(), {
          at: './examples/rps-8-interact/index.rsh:75:50:application',
          fs: ['at ./examples/rps-8-interact/index.rsh:74:13:application call to [unknown function] (defined at: ./examples/rps-8-interact/index.rsh:74:17:function exp)'],
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
          pay: [stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:77:9:decimal', stdlib.UInt_max, '0'), []],
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
            fs: ['at ./examples/rps-8-interact/index.rsh:78:53:application call to "closeTo" (defined at: reach standard library:195:8:function exp)'],
            msg: 'sender correct',
            who: 'Bob'
            });
          ;
          stdlib.protect(ctc1, await interact.informTimeout(), {
            at: './examples/rps-8-interact/index.rsh:41:29:application',
            fs: ['at ./examples/rps-8-interact/index.rsh:40:9:application call to [unknown function] (defined at: ./examples/rps-8-interact/index.rsh:40:27:function exp)', 'at reach standard library:200:8:application call to "after" (defined at: ./examples/rps-8-interact/index.rsh:39:28:function exp)', 'at ./examples/rps-8-interact/index.rsh:78:53:application call to "closeTo" (defined at: reach standard library:195:8:function exp)'],
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
            at: './examples/rps-8-interact/index.rsh:77:9:dot',
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
              fs: ['at ./examples/rps-8-interact/index.rsh:86:53:application call to "closeTo" (defined at: reach standard library:195:8:function exp)'],
              msg: 'sender correct',
              who: 'Bob'
              });
            ;
            stdlib.protect(ctc1, await interact.informTimeout(), {
              at: './examples/rps-8-interact/index.rsh:41:29:application',
              fs: ['at ./examples/rps-8-interact/index.rsh:40:9:application call to [unknown function] (defined at: ./examples/rps-8-interact/index.rsh:40:27:function exp)', 'at reach standard library:200:8:application call to "after" (defined at: ./examples/rps-8-interact/index.rsh:39:28:function exp)', 'at ./examples/rps-8-interact/index.rsh:86:53:application call to "closeTo" (defined at: reach standard library:195:8:function exp)'],
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
              at: './examples/rps-8-interact/index.rsh:85:11:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Bob'
              });
            const v394 = stdlib.digest([ctc0, ctc0], [v389, v390]);
            const v395 = stdlib.digestEq(v358, v394);
            stdlib.assert(v395, {
              at: 'reach standard library:69:17:application',
              fs: ['at ./examples/rps-8-interact/index.rsh:87:20:application call to "checkCommitment" (defined at: reach standard library:68:8:function exp)'],
              msg: null,
              who: 'Bob'
              });
            const v396 = stdlib.safeSub(stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:7:18:decimal', stdlib.UInt_max, '4'), v374);
            const v397 = stdlib.safeAdd(v390, v396);
            const v398 = stdlib.safeMod(v397, stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:7:34:decimal', stdlib.UInt_max, '3'));
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
    const v453 = stdlib.eq(v326, stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:makeEnum', stdlib.UInt_max, '2'));
    const v456 = stdlib.safeMul(stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:94:12:decimal', stdlib.UInt_max, '2'), v305);
    const v458 = v453 ? v304 : v321;
    ;
    stdlib.protect(ctc1, await interact.seeOutcome(v326), {
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
    impure: [`_reachp_0((uint64,uint64,uint64))void`, `_reachp_1((uint64))void`, `_reachp_2((uint64))void`, `_reachp_4((uint64,digest))void`, `_reachp_5((uint64))void`, `_reachp_6((uint64,uint64))void`, `_reachp_7((uint64))void`, `_reachp_8((uint64,uint64,uint64))void`, `_reachp_9((uint64))void`],
    pure: [],
    sigs: [`_reachp_0((uint64,uint64,uint64))void`, `_reachp_1((uint64))void`, `_reachp_2((uint64))void`, `_reachp_4((uint64,digest))void`, `_reachp_5((uint64))void`, `_reachp_6((uint64,uint64))void`, `_reachp_7((uint64))void`, `_reachp_8((uint64,uint64,uint64))void`, `_reachp_9((uint64))void`]
    },
  GlobalNumByteSlice: 3,
  GlobalNumUint: 0,
  LocalNumByteSlice: 0,
  LocalNumUint: 0,
  appApproval: `CCANAAEIKFAFIAcJEAMCeCYDAAEAAQExGEEDtShkSSJbNQEkWzUCKWQqZFCCCQQbbvlQBHUO5AYEhGNipQSkcTyWBN/ZIygE4lfE+QTlOmzuBOxvOQ8E/0yWvTYaAI4JAygDEgMdAvMDBwM+AzMDVANJADEANRA0CyJbNQw0CyRbNRM0CyEJWzUSgAT3cRNNNAwWUDQTFlA0EhZQsDQMiAOWNBOIA58yBjQSCDUNNBA0ExZQNBIWUDQNFlAhBK9QIzIGNQI1ASlLAVcAf2cqTFd/CWcoNAEWNAIWUGcxGSISRIgD4DQDQAAKgAQVH3x1NARQsCNDMQA1DyM0ARJEiAOsNAsXNQyABNUVGRQ0DBZQsDQMiAMhMgY0DQxENBOIAyQjMgY0E0kINQ41FzUYNBgjEkECxDQXNBIINQs0EDQTFlA0EhZQNA9QNA4WUDQLFlAlr1AhBTIGQv9nIzQBEkSIA0o0Cxc1DIAEl073FzQMFlCwNAyIAr8yBjQND0Q0EzQQiAK8MRkhBRJEiAM2IjIKMgmIA2pC/0shBTQBEkSIAug0DCJbNQ00DFcIIDURgARPbztENA0WUDQRULA0DYgCdTIGNAsMRDQQMQASRDIGNBIINRQ0EDQTFlA0EhZQNA9QNA4WUDQRUDQUFlAkr1AhBzIGQv7LIQU0ARJEiAKKNAwXNQ2ABIGqms80DRZQsDQNiAIiMgY0Cw9ENBAxABI0DzEAEhFENA40D4gCE0L/VCEHNAESRIgCJzQLIls1DTQLJFs1DIAESiHL/DQNFlA0DBZQsDQNiAHdMgY0FAxENA8xABJEMgY0Egg1DTQQNBMWUDQSFlA0D1A0DhZQNBFQNAwWUDQNFlAhCDIGQv4yIQc0ARJEiAHINAsXNQyABHGosaM0DBZQsDQMiAGJMgY0FA9ENBAxABI0DzEAEhFENA40EIgBekL+uyEINAESRIgB/TQLIls1FDQLJFs1FjQLIQlbNRWABBKGW8A0FBZQNBYWUDQVFlCwNBSIATkyBjQNDEQ0EDEAEkQ0ETQWFjQVFlABEkQ0FYEENAwJCCEKGDIGNRc1GEL+BCEINAESRIgBnTQLFzUMgARjV1FcNAwWULA0DIgA7zIGNA0PRDQQMQASNA8xABIRRDQONA+IAOBC/iGIAMyBoI0GNAYINQY2GgE1C0L8+ogAuDYaATULQv1yiACtNhoBNQtC/c2IAKI2GgE1DEL+AIgAlzYaATUMQv5TiACMNhoBNQtC/oKIAIE2GgE1C0L+1ogAdjYaATULQv8FiABrNhoBNQtC/1oiMTQSRCEKMTUSRCIxNhJEIjE3EkSIAEuBiAGvIiJC/NExGSISREL88CELNBMLNA80EDQYIQsSTYgANkL9dyKyASOyELIHsgiziUiJTAlJNQYyCYgAG4kJSUH/7kk1BogAE4kjNQOJSSISTDQCEhFEibFC/8kxFjQAIwhJNQAJRwI4BzIKEkQ4ECMSRDgIEkSJSVcAIDUQSSEGWzUTSSVbNRJJVzAgNQ9JIQRbNQ5JV1ggNREhDFs1FIlJVwAgNRBJIQZbNRNJJVs1EklXMCA1D0khBFs1DoFYWzULiUlXACA1EEkhBls1E0klWzUSgTBbNQ2JNAY0B0oPQf9PQv9XSVcAIDUQSSEGWzUTSSVbNRJJVzAgNQ9JIQRbNQ5JV1ggNRFJIQxbNQyBgAFbNQ2JsbIJQv8I`,
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
    296: `145`,
    297: `146`,
    298: `146`,
    299: `147`,
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
  ABI: `[{"inputs":[{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"},{"internalType":"uint256","name":"elem1","type":"uint256"},{"internalType":"uint256","name":"elem2","type":"uint256"}],"internalType":"struct T0","name":"v650","type":"tuple"}],"stateMutability":"payable","type":"constructor"},{"inputs":[{"internalType":"uint256","name":"msg","type":"uint256"}],"name":"ReachError","type":"error"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"address","name":"_who","type":"address"},{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"},{"internalType":"uint256","name":"elem1","type":"uint256"},{"internalType":"uint256","name":"elem2","type":"uint256"}],"indexed":false,"internalType":"struct T0","name":"_a","type":"tuple"}],"name":"_reach_e0","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"address","name":"_who","type":"address"},{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"}],"indexed":false,"internalType":"struct T2","name":"_a","type":"tuple"}],"name":"_reach_e1","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"address","name":"_who","type":"address"},{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"}],"indexed":false,"internalType":"struct T2","name":"_a","type":"tuple"}],"name":"_reach_e2","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"address","name":"_who","type":"address"},{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"},{"internalType":"uint256","name":"elem1","type":"uint256"}],"indexed":false,"internalType":"struct T4","name":"_a","type":"tuple"}],"name":"_reach_e4","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"address","name":"_who","type":"address"},{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"}],"indexed":false,"internalType":"struct T2","name":"_a","type":"tuple"}],"name":"_reach_e5","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"address","name":"_who","type":"address"},{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"},{"internalType":"uint256","name":"elem1","type":"uint256"}],"indexed":false,"internalType":"struct T7","name":"_a","type":"tuple"}],"name":"_reach_e6","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"address","name":"_who","type":"address"},{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"}],"indexed":false,"internalType":"struct T2","name":"_a","type":"tuple"}],"name":"_reach_e7","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"address","name":"_who","type":"address"},{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"},{"internalType":"uint256","name":"elem1","type":"uint256"},{"internalType":"uint256","name":"elem2","type":"uint256"}],"indexed":false,"internalType":"struct T0","name":"_a","type":"tuple"}],"name":"_reach_e8","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"address","name":"_who","type":"address"},{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"}],"indexed":false,"internalType":"struct T2","name":"_a","type":"tuple"}],"name":"_reach_e9","type":"event"},{"stateMutability":"payable","type":"fallback"},{"inputs":[],"name":"_reachCreationTime","outputs":[{"internalType":"uint256","name":"","type":"uint256"}],"stateMutability":"view","type":"function"},{"inputs":[],"name":"_reachCurrentState","outputs":[{"internalType":"uint256","name":"","type":"uint256"},{"internalType":"bytes","name":"","type":"bytes"}],"stateMutability":"view","type":"function"},{"inputs":[],"name":"_reachCurrentTime","outputs":[{"internalType":"uint256","name":"","type":"uint256"}],"stateMutability":"view","type":"function"},{"inputs":[{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"}],"internalType":"struct T2","name":"v653","type":"tuple"}],"name":"_reachp_1","outputs":[{"internalType":"bool","name":"","type":"bool"}],"stateMutability":"payable","type":"function"},{"inputs":[{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"}],"internalType":"struct T2","name":"v656","type":"tuple"}],"name":"_reachp_2","outputs":[{"internalType":"bool","name":"","type":"bool"}],"stateMutability":"payable","type":"function"},{"inputs":[{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"},{"internalType":"uint256","name":"elem1","type":"uint256"}],"internalType":"struct T4","name":"v659","type":"tuple"}],"name":"_reachp_4","outputs":[{"internalType":"bool","name":"","type":"bool"}],"stateMutability":"payable","type":"function"},{"inputs":[{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"}],"internalType":"struct T2","name":"v662","type":"tuple"}],"name":"_reachp_5","outputs":[{"internalType":"bool","name":"","type":"bool"}],"stateMutability":"payable","type":"function"},{"inputs":[{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"},{"internalType":"uint256","name":"elem1","type":"uint256"}],"internalType":"struct T7","name":"v665","type":"tuple"}],"name":"_reachp_6","outputs":[{"internalType":"bool","name":"","type":"bool"}],"stateMutability":"payable","type":"function"},{"inputs":[{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"}],"internalType":"struct T2","name":"v668","type":"tuple"}],"name":"_reachp_7","outputs":[{"internalType":"bool","name":"","type":"bool"}],"stateMutability":"payable","type":"function"},{"inputs":[{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"},{"internalType":"uint256","name":"elem1","type":"uint256"},{"internalType":"uint256","name":"elem2","type":"uint256"}],"internalType":"struct T0","name":"v671","type":"tuple"}],"name":"_reachp_8","outputs":[{"internalType":"bool","name":"","type":"bool"}],"stateMutability":"payable","type":"function"},{"inputs":[{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"}],"internalType":"struct T2","name":"v674","type":"tuple"}],"name":"_reachp_9","outputs":[{"internalType":"bool","name":"","type":"bool"}],"stateMutability":"payable","type":"function"},{"stateMutability":"payable","type":"receive"}]`,
  Bytecode: `0x608080604052606081611d13803803809161001a8285610384565b83398101031261037f5760405190600090606083016001600160401b0381118482101761036b576040528051835260406020820151916020850192835201519260408101938452436003556040516020810181811060018060401b038211176103575760405283905260045460ff16610343577f4f453854b6a90dba7951e2aeeb8854b2b5f80576fe444dd363a967d18d9175e46080604051338152835160208201528451604082015286516060820152a1518015908115610337575b5015610323578051340361030f5760405192608084016001600160401b038111858210176102fb576040526020840183815260408501918483526060860193858552338752518252805183525143018043116102e7574381106102e357835260018455436001556040519460018060a01b0390511660208601525160408501525160608401525160808301526080825261017260a083610384565b8151906001600160401b0382116102cf57600254600181811c911680156102c5575b60208210146102b157601f811161024e575b50602090601f83116001146101ed579192836101e2575b50508160011b916000199060031b1c1916176002555b60405161195590816103be8239f35b0151905038806101bd565b90601f1983169360028352818320925b8581106102365750836001951061021d575b505050811b016002556101d3565b015160001960f88460031b161c1916905538808061020f565b919260206001819286850151815501940192016101fd565b600282527f405787fa12a823e0f2b7631cc41b3ba8828b3321ca811111fa75cd3aa3bb5ace601f840160051c810191602085106102a7575b601f0160051c01905b81811061029c57506101a6565b82815560010161028f565b9091508190610286565b634e487b7160e01b82526022600452602482fd5b90607f1690610194565b634e487b7160e01b81526041600452602490fd5b8480fd5b634e487b7160e01b85526011600452602485fd5b634e487b7160e01b84526041600452602484fd5b63100960cb60e01b82526009600452602482fd5b63100960cb60e01b82526008600452602482fd5b905060015414386100d7565b63100960cb60e01b83526007600452602483fd5b634e487b7160e01b85526041600452602485fd5b634e487b7160e01b83526041600452602483fd5b600080fd5b601f909101601f19168101906001600160401b038211908210176103a757604052565b634e487b7160e01b600052604160045260246000fdfe608080604052600436101561001a575b50361561001857005b005b60003560e01c9081631e93b0f11461146f575080632f132302146112f157806330dd5d0814611170578063573b851014610ef05780638323075714610ed2578063ab53f2c614610e62578063b3722a9914610ce1578063b559601a14610812578063e3342bfa1461054b578063ee9fe9921461020e5763f5a239bc146100a0573861000f565b6020366003190112610209576000806040516100bb8161148a565b526100c53661151a565b60018254036101f5576100e86100d9611574565b602080825183010191016117ee565b9060ff600454166101e1577f794b69bffed607ab45148da3c7f9c613ba8e4d2d82b625153563a3a2f536190a604051806101238433836116cf565b0390a15180159081156101d5575b50156101c157606081015143106101ad5734610199578180808093602060018060a01b0382511691015190828215610190575bf115610184578080602092558060015561017c611713565b604051908152f35b604051903d90823e3d90fd5b506108fc610164565b63100960cb60e01b82526013600452602482fd5b63100960cb60e01b82526012600452602482fd5b63100960cb60e01b82526011600452602482fd5b90506001541438610131565b63100960cb60e01b83526010600452602483fd5b63100960cb60e01b8252600f600452602482fd5b600080fd5b6040366003190112610209576000806040516102298161148a565b526102333661161b565b600782540361053757610256610247611574565b60208082518301019101611667565b60ff60045416610523577f9449691f9246ec4b81aeb2f34bd8715f4e442367780c7ac745057e0a3a212bd6604051806102908533836118ed565b0390a181518015908115610517575b50156105035760c08101514310156104ef57346104db5760608101903360018060a01b03835116036104c757839182916040519485926102de846114c0565b8484526020840185815261035760408601928784526020606088019589875260a060808a01988b8a5260e0828c019b8d8d5260c081019d8e52019c8d528d600180841b038251169052838101518752604081019485518952600180841b03905116895260808101518a5201518952015188525143611914565b875260098955436001556040805198516001600160a01b0390811660208b01529151908901529051606088015290511660808601525160a08501525160c08401525160e0830152516101008083019190915281526103b7610120826114f7565b80516001600160401b0381116104b357916020926103d660025461153a565b601f8111610479575b508392601f82116001146104185782938291610402949261040d575b5050611700565b600255604051908152f35b0151905085806103fb565b601f198216936002845280842091845b8681106104625750836001959610610449575b505050811b0160025561017c565b015160001960f88460031b161c1916905584808061043b565b919287600181928685015181550194019201610428565b6104a39060028452858420601f840160051c8101918785106104a9575b601f0160051c01906116e9565b846103df565b9091508190610496565b634e487b7160e01b83526041600452602483fd5b63100960cb60e01b84526025600452602484fd5b63100960cb60e01b83526024600481905283fd5b63100960cb60e01b83526023600452602483fd5b63100960cb60e01b83526022600452602483fd5b9050600154148461029f565b63100960cb60e01b83526021600452602483fd5b63100960cb60e01b82526020600452602482fd5b6040366003190112610209576000806040516105668161148a565b526105703661161b565b60058254036107fe57610593610584611574565b60208082518301019101611790565b9060ff600454166107ea577f2b488b46f65093e5cc34b7a23106f60902da34f9221ba5c98d129851d4f8efe6604051806105ce8433836118ed565b0390a1805180159081156107de575b50156107ca5760a08201514310156107b657346107a2578151336001600160a01b039091160361078e5761060f611843565b9160018060a01b03815116835260208101519160208401928352604082019182516040860190815261066e6020608060018060a01b036060860151169460608a019586520151946080890195865201519460a088019586525143611914565b9460c0870195865260078855436001556040519660018060a01b03905116602088015251604087015251606086015260018060a01b0390511660808501525160a08401525160c08301525160e082015260e081526106ce610100826114f7565b80516001600160401b0381116104b357916020926106ed60025461153a565b601f811161075f575b508392601f82116001146107185782938291610402949261040d575050611700565b601f198216936002845280842091845b868110610748575083600195961061044957505050811b0160025561017c565b919287600181928685015181550194019201610728565b6107889060028452858420601f840160051c8101918785106104a957601f0160051c01906116e9565b846106f6565b63100960cb60e01b83526019600452602483fd5b63100960cb60e01b83526018600452602483fd5b63100960cb60e01b83526017600452602483fd5b63100960cb60e01b83526016600452602483fd5b905060015414846105dd565b63100960cb60e01b83526015600452602483fd5b63100960cb60e01b82526014600452602482fd5b606036600319011261020957600060405161082c8161148a565b52604051606081016001600160401b03811182821017610ccb57604052600435815260208101602435815260408201906044358252600960005403610cb557610885610876611574565b6020808251830101910161187a565b9260ff60045416610c9f577fd6d1c39c0a5020e00dd6931b7ce85fc33eab108766c9c35e10ee90df64121f446080604051338152835160208201528451604082015285516060820152a1518015908115610c93575b5015610c7d5760e0830151431015610c675734610c51578251336001600160a01b0390911603610c3b5760a083015190518251604051906020820192835260408201526040815261092c6060826114f7565b51902003610c255761093c611843565b60018060a01b0383511681526020830151602082019081526040840151926040830193845260018060a01b036060860151169060608401918252519460c081015160040360048111610c0f57610996600391600098611914565b0690816080860152608060a086019143835201519160c0860192835260018114600014610b355750610a23604051966109ce886114a5565b88885260208801898152604089018a815260608a018b815260808b018c815260a08c018d81529a516001600160a01b039081168d52995184528451835297519098168852945186529251905194959490611914565b855260058755436001556040805196516001600160a01b0390811660208901529151908701529051606086015290511660808401525160a08301525160c0808301919091528152610a7560e0826114f7565b80516001600160401b0381116104b35791602092610a9460025461153a565b601f8111610b06575b508392601f8211600114610abf5782938291610402949261040d575050611700565b601f198216936002845280842091845b868110610aef575083600195961061044957505050811b0160025561017c565b919287600181928685015181550194019201610acf565b610b2f9060028452858420601f840160051c8101918785106104a957601f0160051c01906116e9565b84610a9d565b939695509192600214159150610bfd90505750516001600160a01b0316915b51918183158015610bac575b83945015610ba85782809291819282908215610b9e575b6001600160a01b031690f1156101845780806020925580600155610b99611713565b61017c565b6108fc9150610b77565b5050fd5b9050600184901b6001600160ff1b0385168503610be9578091610bd55783946002910414610b60565b634e487b7160e01b84526012600452602484fd5b634e487b7160e01b84526011600452602484fd5b516001600160a01b0316929050610b54565b634e487b7160e01b600052601160045260246000fd5b63100960cb60e01b600052603260045260246000fd5b63100960cb60e01b600052603160045260246000fd5b63100960cb60e01b600052603060045260246000fd5b63100960cb60e01b600052602f60045260246000fd5b63100960cb60e01b600052602e60045260246000fd5b905060015414846108da565b63100960cb60e01b600052602d60045260246000fd5b63100960cb60e01b600052602c60045260246000fd5b634e487b7160e01b600052604160045260246000fd5b602036600319011261020957600080604051610cfc8161148a565b52610d063661151a565b6009825403610e4e57610d1a610876611574565b9060ff60045416610e3a577fd64d3134781556c6a626b018733c1e4f7a8f056da03bbd004578b0e46c1b667e60405180610d558433836116cf565b0390a1518015908115610e2e575b5015610e1a5760e08101514310610e065734610df25780516001600160a01b03163303610ddd5760015b15610dc9578180808093608060018060a01b036060830151169101519082821561019057f115610184578080602092558060015561017c611713565b63100960cb60e01b82526038600452602482fd5b60608101516001600160a01b03163314610d8d565b63100960cb60e01b82526037600452602482fd5b63100960cb60e01b82526036600452602482fd5b63100960cb60e01b82526035600452602482fd5b90506001541483610d63565b63100960cb60e01b83526034600452602483fd5b63100960cb60e01b82526033600452602482fd5b3461020957600036600319011261020957600054610e7e611574565b604051918252604060208301528181519182604083015260005b838110610eba5750508160006060809484010152601f80199101168101030190f35b60208282018101516060878401015285935001610e98565b34610209576000366003190112610209576020600154604051908152f35b602036600319011261020957600080604051610f0b8161148a565b52610f153661151a565b600182540361115c57610f296100d9611574565b9060ff60045416611148577fcf0e8bec53cd91fa87ecf8f6f405ac75914a22acdb92a3553ee5c294fee8159660405180610f648433836116cf565b0390a151801590811561113c575b50156111285760608101514310156111145760208101908151340361110057610f99611843565b9060018060a01b0381511682526040835191602084019283520151926040830193845260608301903382526001608085015260a0840190438252519060c085019180018252610fee604051966109ce886114a5565b855260058755436001556040805196516001600160a01b0390811660208901529151908701529051606086015290511660808401525160a08301525160c080830191909152815261104060e0826114f7565b80516001600160401b0381116104b3579160209261105f60025461153a565b601f81116110d1575b508392601f821160011461108a5782938291610402949261040d575050611700565b601f198216936002845280842091845b8681106110ba575083600195961061044957505050811b0160025561017c565b91928760018192868501518155019401920161109a565b6110fa9060028452858420601f840160051c8101918785106104a957601f0160051c01906116e9565b84611068565b63100960cb60e01b8352600e600452602483fd5b63100960cb60e01b8252600d600452602482fd5b63100960cb60e01b8252600c600452602482fd5b90506001541483610f72565b63100960cb60e01b8352600b600452602483fd5b63100960cb60e01b8252600a600452602482fd5b60203660031901126102095760008060405161118b8161148a565b526111953661151a565b60058254036112dd576111a9610584611574565b9060ff600454166112c9577f46f247599a5fa9114da586bc9a4d716618c03f0781a481e77299e07c647c1249604051806111e48433836116cf565b0390a15180159081156112bd575b50156112a95760a0810151431061129557346112815780516001600160a01b0316330361126c5760015b15611258578180808093608060018060a01b036060830151169101519082821561019057f115610184578080602092558060015561017c611713565b63100960cb60e01b8252601f600452602482fd5b60608101516001600160a01b0316331461121c565b63100960cb60e01b8252601e600452602482fd5b63100960cb60e01b8252601d600452602482fd5b63100960cb60e01b8252601c600452602482fd5b905060015414836111f2565b63100960cb60e01b8352601b600452602483fd5b63100960cb60e01b8252601a600452602482fd5b60203660031901126102095760008060405161130c8161148a565b526113163661151a565b600782540361145b5761132a610247611574565b9060ff60045416611447577fa4850b05c9188495196ad609440a82393348559ec3e1eb1c2ab8d784a9e9d401604051806113658433836116cf565b0390a151801590811561143b575b50156114275760c0810151431061141357346113ff5780516001600160a01b031633036113ea5760015b156113d6578180808093608060018060a01b038251169101519082821561019057f115610184578080602092558060015561017c611713565b63100960cb60e01b8252602b600452602482fd5b60608101516001600160a01b0316331461139d565b63100960cb60e01b8252602a600452602482fd5b63100960cb60e01b82526029600452602482fd5b63100960cb60e01b82526028600452602482fd5b90506001541483611373565b63100960cb60e01b83526027600452602483fd5b63100960cb60e01b82526026600452602482fd5b34610209576000366003190112610209576020906003548152f35b602081019081106001600160401b03821117610ccb57604052565b60c081019081106001600160401b03821117610ccb57604052565b61010081019081106001600160401b03821117610ccb57604052565b60e081019081106001600160401b03821117610ccb57604052565b601f909101601f19168101906001600160401b03821190821017610ccb57604052565b602090600319011261020957604051906115338261148a565b6004358252565b90600182811c9216801561156a575b602083101461155457565b634e487b7160e01b600052602260045260246000fd5b91607f1691611549565b60405190600082600254916115888361153a565b80835292600181169081156115fc57506001146115ae575b6115ac925003836114f7565b565b506002600090815290916000805160206119298339815191525b8183106115e05750509060206115ac928201016115a0565b60209193508060019154838589010152019101909184926115c8565b602092506115ac94915060ff191682840152151560051b8201016115a0565b60409060031901126102095760408051919082016001600160401b03811183821017610ccb5760405260043582526024356020830152565b51906001600160a01b038216820361020957565b908160e09103126102095760c060405191611681836114dc565b61168a81611653565b835260208101516020840152604081015160408401526116ac60608201611653565b60608401526080810151608084015260a081015160a0840152015160c082015290565b6001600160a01b0390911681529051602082015260400190565b8181106116f4575050565b600081556001016116e9565b8160011b916000199060031b1c19161790565b61171e60025461153a565b806117265750565b601f811160011461173957506000600255565b600260005261177e90601f0160051c600080516020611929833981519152017f405787fa12a823e0f2b7631cc41b3ba8828b3321ca811111fa75cd3aa3bb5acf6116e9565b60026000526000602081208160025555565b908160c09103126102095760a0604051916117aa836114a5565b6117b381611653565b835260208101516020840152604081015160408401526117d560608201611653565b606084015260808101516080840152015160a082015290565b90816080910312610209576040519060808201906001600160401b03821183831017610ccb5760609160405261182381611653565b835260208101516020840152604081015160408401520151606082015290565b60405190611850826114dc565b600060c0838281528260208201528260408201528260608201528260808201528260a08201520152565b90816101009103126102095760e060405191611895836114c0565b61189e81611653565b835260208101516020840152604081015160408401526118c060608201611653565b60608401526080810151608084015260a081015160a084015260c081015160c0840152015160e082015290565b6001600160a01b039091168152815160208083019190915290910151604082015260600190565b9190820191828111610c0f5782106102095756fe405787fa12a823e0f2b7631cc41b3ba8828b3321ca811111fa75cd3aa3bb5acea164736f6c634300081a000a`,
  BytecodeLen: 7443,
  version: 9,
  views: {
    }
  };
export const _stateSourceMap = {
  1: {
    at: './examples/rps-8-interact/index.rsh:51:11:after expr stmt semicolon',
    fs: [],
    msg: null,
    who: 'Module'
    },
  2: {
    at: 'reach standard library:199:11:after expr stmt semicolon',
    fs: ['at ./examples/rps-8-interact/index.rsh:57:51:application call to "closeTo" (defined at: reach standard library:195:8:function exp)'],
    msg: null,
    who: 'Module'
    },
  4: {
    at: './examples/rps-8-interact/index.rsh:95:11:after expr stmt semicolon',
    fs: [],
    msg: null,
    who: 'Module'
    },
  5: {
    at: './examples/rps-8-interact/index.rsh:62:13:after expr stmt semicolon',
    fs: [],
    msg: null,
    who: 'Module'
    },
  6: {
    at: 'reach standard library:199:11:after expr stmt semicolon',
    fs: ['at ./examples/rps-8-interact/index.rsh:70:53:application call to "closeTo" (defined at: reach standard library:195:8:function exp)'],
    msg: null,
    who: 'Module'
    },
  7: {
    at: './examples/rps-8-interact/index.rsh:71:13:after expr stmt semicolon',
    fs: [],
    msg: null,
    who: 'Module'
    },
  8: {
    at: 'reach standard library:199:11:after expr stmt semicolon',
    fs: ['at ./examples/rps-8-interact/index.rsh:78:53:application call to "closeTo" (defined at: reach standard library:195:8:function exp)'],
    msg: null,
    who: 'Module'
    },
  9: {
    at: './examples/rps-8-interact/index.rsh:79:13:after expr stmt semicolon',
    fs: [],
    msg: null,
    who: 'Module'
    },
  10: {
    at: 'reach standard library:199:11:after expr stmt semicolon',
    fs: ['at ./examples/rps-8-interact/index.rsh:86:53:application call to "closeTo" (defined at: reach standard library:195:8:function exp)'],
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
