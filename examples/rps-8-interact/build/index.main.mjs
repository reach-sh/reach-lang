// Automatically generated with Reach 0.1.9 (f897426c*)
/* eslint-disable */
export const _version = '0.1.9';
export const _versionHash = '0.1.9 (f897426c*)';
export const _backendVersion = 11;

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
      const v317 = stdlib.add(v307, v306);
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
  const v317 = stdlib.add(v307, v306);
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
      pay: [stdlib.checkedBigNumberify('reach standard library:189:11:decimal', stdlib.UInt_max, '0'), []],
      sim_p: (async (txn3) => {
        const sim_r = { txns: [], mapRefs: [], maps: [] };
        let sim_txn_ctr = stdlib.UInt_max;
        const getSimTokCtr = () => { sim_txn_ctr = sim_txn_ctr.sub(1); return sim_txn_ctr; };
        
        
        const {data: [], secs: v471, time: v470, didSend: v253, from: v469 } = txn3;
        
        ;
        sim_r.txns.push({
          amt: v305,
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
      tys: [ctc4, ctc0, ctc0, ctc0],
      waitIfNotPresent: false
      }));
    const {data: [], secs: v471, time: v470, didSend: v253, from: v469 } = txn3;
    ;
    ;
    stdlib.protect(ctc3, await interact.informTimeout(), {
      at: './examples/rps-8-interact/index.rsh:41:29:application',
      fs: ['at ./examples/rps-8-interact/index.rsh:40:9:application call to [unknown function] (defined at: ./examples/rps-8-interact/index.rsh:40:27:function exp)', 'at reach standard library:192:8:application call to "after" (defined at: ./examples/rps-8-interact/index.rsh:39:28:function exp)', 'at ./examples/rps-8-interact/index.rsh:57:51:application call to "closeTo" (defined at: reach standard library:187:8:function exp)'],
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
    
    while (await (async () => {
      const v342 = stdlib.eq(v326, stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:makeEnum', stdlib.UInt_max, '1'));
      
      return v342;})()) {
      const v349 = stdlib.add(v327, v306);
      const v353 = stdlib.protect(ctc0, await interact.getHand(), {
        at: './examples/rps-8-interact/index.rsh:65:42:application',
        fs: ['at ./examples/rps-8-interact/index.rsh:64:15:application call to [unknown function] (defined at: ./examples/rps-8-interact/index.rsh:64:19:function exp)'],
        msg: 'getHand',
        who: 'Alice'
        });
      const v354 = stdlib.protect(ctc0, await interact.random(), {
        at: 'reach standard library:53:31:application',
        fs: ['at ./examples/rps-8-interact/index.rsh:66:56:application call to "makeCommitment" (defined at: reach standard library:52:8:function exp)', 'at ./examples/rps-8-interact/index.rsh:64:15:application call to [unknown function] (defined at: ./examples/rps-8-interact/index.rsh:64:19:function exp)'],
        msg: 'random',
        who: 'Alice'
        });
      const v355 = stdlib.digest(ctc1, [v354, v353]);
      
      const txn3 = await (ctc.sendrecv({
        args: [v304, v305, v306, v321, v334, v349, v355],
        evt_cnt: 1,
        funcNum: 4,
        lct: v327,
        onlyIf: true,
        out_tys: [ctc2],
        pay: [stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:69:11:decimal', stdlib.UInt_max, '0'), []],
        sim_p: (async (txn3) => {
          const sim_r = { txns: [], mapRefs: [], maps: [] };
          let sim_txn_ctr = stdlib.UInt_max;
          const getSimTokCtr = () => { sim_txn_ctr = sim_txn_ctr.sub(1); return sim_txn_ctr; };
          
          
          const {data: [v358], secs: v360, time: v359, didSend: v91, from: v357 } = txn3;
          
          ;
          const v361 = stdlib.addressEq(v304, v357);
          ;
          const v368 = stdlib.add(v359, v306);
          sim_r.isHalt = false;
          
          return sim_r;
          }),
        soloSend: true,
        timeoutAt: ['time', v349],
        tys: [ctc4, ctc0, ctc0, ctc4, ctc0, ctc0, ctc2],
        waitIfNotPresent: false
        }));
      if (txn3.didTimeout) {
        const txn4 = await (ctc.sendrecv({
          args: [v304, v305, v306, v321, v334, v349],
          evt_cnt: 0,
          funcNum: 5,
          lct: v327,
          onlyIf: true,
          out_tys: [],
          pay: [stdlib.checkedBigNumberify('reach standard library:189:11:decimal', stdlib.UInt_max, '0'), []],
          sim_p: (async (txn4) => {
            const sim_r = { txns: [], mapRefs: [], maps: [] };
            let sim_txn_ctr = stdlib.UInt_max;
            const getSimTokCtr = () => { sim_txn_ctr = sim_txn_ctr.sub(1); return sim_txn_ctr; };
            
            
            const {data: [], secs: v437, time: v436, didSend: v206, from: v435 } = txn4;
            
            ;
            const v438 = stdlib.addressEq(v304, v435);
            const v439 = stdlib.addressEq(v321, v435);
            const v440 = v438 ? true : v439;
            ;
            sim_r.txns.push({
              amt: v334,
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
          tys: [ctc4, ctc0, ctc0, ctc4, ctc0, ctc0],
          waitIfNotPresent: false
          }));
        const {data: [], secs: v437, time: v436, didSend: v206, from: v435 } = txn4;
        ;
        const v438 = stdlib.addressEq(v304, v435);
        const v439 = stdlib.addressEq(v321, v435);
        const v440 = v438 ? true : v439;
        stdlib.assert(v440, {
          at: 'reach standard library:189:11:dot',
          fs: ['at ./examples/rps-8-interact/index.rsh:70:53:application call to "closeTo" (defined at: reach standard library:187:8:function exp)'],
          msg: 'sender correct',
          who: 'Alice'
          });
        ;
        stdlib.protect(ctc3, await interact.informTimeout(), {
          at: './examples/rps-8-interact/index.rsh:41:29:application',
          fs: ['at ./examples/rps-8-interact/index.rsh:40:9:application call to [unknown function] (defined at: ./examples/rps-8-interact/index.rsh:40:27:function exp)', 'at reach standard library:192:8:application call to "after" (defined at: ./examples/rps-8-interact/index.rsh:39:28:function exp)', 'at ./examples/rps-8-interact/index.rsh:70:53:application call to "closeTo" (defined at: reach standard library:187:8:function exp)'],
          msg: 'informTimeout',
          who: 'Alice'
          });
        
        return;
        
        }
      else {
        const {data: [v358], secs: v360, time: v359, didSend: v91, from: v357 } = txn3;
        ;
        const v361 = stdlib.addressEq(v304, v357);
        stdlib.assert(v361, {
          at: './examples/rps-8-interact/index.rsh:69:11:dot',
          fs: [],
          msg: 'sender correct',
          who: 'Alice'
          });
        const v368 = stdlib.add(v359, v306);
        const txn4 = await (ctc.recv({
          didSend: false,
          evt_cnt: 1,
          funcNum: 6,
          out_tys: [ctc0],
          timeoutAt: ['time', v368],
          waitIfNotPresent: false
          }));
        if (txn4.didTimeout) {
          const txn5 = await (ctc.sendrecv({
            args: [v304, v305, v306, v321, v334, v358, v368],
            evt_cnt: 0,
            funcNum: 7,
            lct: v359,
            onlyIf: true,
            out_tys: [],
            pay: [stdlib.checkedBigNumberify('reach standard library:189:11:decimal', stdlib.UInt_max, '0'), []],
            sim_p: (async (txn5) => {
              const sim_r = { txns: [], mapRefs: [], maps: [] };
              let sim_txn_ctr = stdlib.UInt_max;
              const getSimTokCtr = () => { sim_txn_ctr = sim_txn_ctr.sub(1); return sim_txn_ctr; };
              
              
              const {data: [], secs: v419, time: v418, didSend: v172, from: v417 } = txn5;
              
              ;
              const v420 = stdlib.addressEq(v304, v417);
              const v421 = stdlib.addressEq(v321, v417);
              const v422 = v420 ? true : v421;
              ;
              sim_r.txns.push({
                amt: v334,
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
            tys: [ctc4, ctc0, ctc0, ctc4, ctc0, ctc2, ctc0],
            waitIfNotPresent: false
            }));
          const {data: [], secs: v419, time: v418, didSend: v172, from: v417 } = txn5;
          ;
          const v420 = stdlib.addressEq(v304, v417);
          const v421 = stdlib.addressEq(v321, v417);
          const v422 = v420 ? true : v421;
          stdlib.assert(v422, {
            at: 'reach standard library:189:11:dot',
            fs: ['at ./examples/rps-8-interact/index.rsh:78:53:application call to "closeTo" (defined at: reach standard library:187:8:function exp)'],
            msg: 'sender correct',
            who: 'Alice'
            });
          ;
          stdlib.protect(ctc3, await interact.informTimeout(), {
            at: './examples/rps-8-interact/index.rsh:41:29:application',
            fs: ['at ./examples/rps-8-interact/index.rsh:40:9:application call to [unknown function] (defined at: ./examples/rps-8-interact/index.rsh:40:27:function exp)', 'at reach standard library:192:8:application call to "after" (defined at: ./examples/rps-8-interact/index.rsh:39:28:function exp)', 'at ./examples/rps-8-interact/index.rsh:78:53:application call to "closeTo" (defined at: reach standard library:187:8:function exp)'],
            msg: 'informTimeout',
            who: 'Alice'
            });
          
          return;
          
          }
        else {
          const {data: [v374], secs: v376, time: v375, didSend: v101, from: v373 } = txn4;
          ;
          const v377 = stdlib.addressEq(v321, v373);
          stdlib.assert(v377, {
            at: './examples/rps-8-interact/index.rsh:77:9:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Alice'
            });
          const v384 = stdlib.add(v375, v306);
          const txn5 = await (ctc.sendrecv({
            args: [v304, v305, v306, v321, v334, v358, v374, v384, v354, v353],
            evt_cnt: 2,
            funcNum: 8,
            lct: v375,
            onlyIf: true,
            out_tys: [ctc0, ctc0],
            pay: [stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:85:11:decimal', stdlib.UInt_max, '0'), []],
            sim_p: (async (txn5) => {
              const sim_r = { txns: [], mapRefs: [], maps: [] };
              let sim_txn_ctr = stdlib.UInt_max;
              const getSimTokCtr = () => { sim_txn_ctr = sim_txn_ctr.sub(1); return sim_txn_ctr; };
              
              
              const {data: [v389, v390], secs: v392, time: v391, didSend: v111, from: v388 } = txn5;
              
              ;
              const v393 = stdlib.addressEq(v304, v388);
              ;
              const v394 = stdlib.digest(ctc1, [v389, v390]);
              const v395 = stdlib.digestEq(v358, v394);
              ;
              const v396 = stdlib.sub(stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:7:18:decimal', stdlib.UInt_max, '4'), v374);
              const v397 = stdlib.add(v390, v396);
              const v398 = stdlib.mod(v397, stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:7:34:decimal', stdlib.UInt_max, '3'));
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
                  const v349 = stdlib.add(v327, v306);
                  sim_r.isHalt = false;
                  }
                else {
                  const v453 = stdlib.eq(v326, stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:makeEnum', stdlib.UInt_max, '2'));
                  const v456 = stdlib.mul(stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:94:12:decimal', stdlib.UInt_max, '2'), v305);
                  const v458 = v453 ? v304 : v321;
                  sim_r.txns.push({
                    amt: v456,
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
            tys: [ctc4, ctc0, ctc0, ctc4, ctc0, ctc2, ctc0, ctc0, ctc0, ctc0],
            waitIfNotPresent: false
            }));
          if (txn5.didTimeout) {
            const txn6 = await (ctc.sendrecv({
              args: [v304, v305, v306, v321, v334, v358, v374, v384],
              evt_cnt: 0,
              funcNum: 9,
              lct: v375,
              onlyIf: true,
              out_tys: [],
              pay: [stdlib.checkedBigNumberify('reach standard library:189:11:decimal', stdlib.UInt_max, '0'), []],
              sim_p: (async (txn6) => {
                const sim_r = { txns: [], mapRefs: [], maps: [] };
                let sim_txn_ctr = stdlib.UInt_max;
                const getSimTokCtr = () => { sim_txn_ctr = sim_txn_ctr.sub(1); return sim_txn_ctr; };
                
                
                const {data: [], secs: v401, time: v400, didSend: v138, from: v399 } = txn6;
                
                ;
                const v402 = stdlib.addressEq(v304, v399);
                const v403 = stdlib.addressEq(v321, v399);
                const v404 = v402 ? true : v403;
                ;
                sim_r.txns.push({
                  amt: v334,
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
              tys: [ctc4, ctc0, ctc0, ctc4, ctc0, ctc2, ctc0, ctc0],
              waitIfNotPresent: false
              }));
            const {data: [], secs: v401, time: v400, didSend: v138, from: v399 } = txn6;
            ;
            const v402 = stdlib.addressEq(v304, v399);
            const v403 = stdlib.addressEq(v321, v399);
            const v404 = v402 ? true : v403;
            stdlib.assert(v404, {
              at: 'reach standard library:189:11:dot',
              fs: ['at ./examples/rps-8-interact/index.rsh:86:53:application call to "closeTo" (defined at: reach standard library:187:8:function exp)'],
              msg: 'sender correct',
              who: 'Alice'
              });
            ;
            stdlib.protect(ctc3, await interact.informTimeout(), {
              at: './examples/rps-8-interact/index.rsh:41:29:application',
              fs: ['at ./examples/rps-8-interact/index.rsh:40:9:application call to [unknown function] (defined at: ./examples/rps-8-interact/index.rsh:40:27:function exp)', 'at reach standard library:192:8:application call to "after" (defined at: ./examples/rps-8-interact/index.rsh:39:28:function exp)', 'at ./examples/rps-8-interact/index.rsh:86:53:application call to "closeTo" (defined at: reach standard library:187:8:function exp)'],
              msg: 'informTimeout',
              who: 'Alice'
              });
            
            return;
            
            }
          else {
            const {data: [v389, v390], secs: v392, time: v391, didSend: v111, from: v388 } = txn5;
            ;
            const v393 = stdlib.addressEq(v304, v388);
            stdlib.assert(v393, {
              at: './examples/rps-8-interact/index.rsh:85:11:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Alice'
              });
            const v394 = stdlib.digest(ctc1, [v389, v390]);
            const v395 = stdlib.digestEq(v358, v394);
            stdlib.assert(v395, {
              at: 'reach standard library:58:17:application',
              fs: ['at ./examples/rps-8-interact/index.rsh:87:20:application call to "checkCommitment" (defined at: reach standard library:57:8:function exp)'],
              msg: null,
              who: 'Alice'
              });
            const v396 = stdlib.sub(stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:7:18:decimal', stdlib.UInt_max, '4'), v374);
            const v397 = stdlib.add(v390, v396);
            const v398 = stdlib.mod(v397, stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:7:34:decimal', stdlib.UInt_max, '3'));
            const cv326 = v398;
            const cv327 = v391;
            const cv334 = v334;
            
            v326 = cv326;
            v327 = cv327;
            v334 = cv334;
            
            continue;}
          
          }
        
        }
      
      }
    const v453 = stdlib.eq(v326, stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:makeEnum', stdlib.UInt_max, '2'));
    const v456 = stdlib.mul(stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:94:12:decimal', stdlib.UInt_max, '2'), v305);
    const v458 = v453 ? v304 : v321;
    ;
    stdlib.protect(ctc3, await interact.seeOutcome(v326), {
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
    timeoutAt: undefined /* mto */,
    waitIfNotPresent: false
    }));
  const {data: [v305, v306], secs: v308, time: v307, didSend: v56, from: v304 } = txn1;
  ;
  const v317 = stdlib.add(v307, v306);
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
        const v349 = stdlib.add(v327, v306);
        sim_r.isHalt = false;
        }
      else {
        const v453 = stdlib.eq(v326, stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:makeEnum', stdlib.UInt_max, '2'));
        const v456 = stdlib.mul(stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:94:12:decimal', stdlib.UInt_max, '2'), v305);
        const v458 = v453 ? v304 : v321;
        sim_r.txns.push({
          amt: v456,
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
    tys: [ctc4, ctc0, ctc0, ctc0],
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
      pay: [stdlib.checkedBigNumberify('reach standard library:189:11:decimal', stdlib.UInt_max, '0'), []],
      sim_p: (async (txn3) => {
        const sim_r = { txns: [], mapRefs: [], maps: [] };
        let sim_txn_ctr = stdlib.UInt_max;
        const getSimTokCtr = () => { sim_txn_ctr = sim_txn_ctr.sub(1); return sim_txn_ctr; };
        
        
        const {data: [], secs: v471, time: v470, didSend: v253, from: v469 } = txn3;
        
        ;
        sim_r.txns.push({
          amt: v305,
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
      tys: [ctc4, ctc0, ctc0, ctc0],
      waitIfNotPresent: false
      }));
    const {data: [], secs: v471, time: v470, didSend: v253, from: v469 } = txn3;
    ;
    ;
    stdlib.protect(ctc1, await interact.informTimeout(), {
      at: './examples/rps-8-interact/index.rsh:41:29:application',
      fs: ['at ./examples/rps-8-interact/index.rsh:40:9:application call to [unknown function] (defined at: ./examples/rps-8-interact/index.rsh:40:27:function exp)', 'at reach standard library:192:8:application call to "after" (defined at: ./examples/rps-8-interact/index.rsh:39:28:function exp)', 'at ./examples/rps-8-interact/index.rsh:57:51:application call to "closeTo" (defined at: reach standard library:187:8:function exp)'],
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
    
    while (await (async () => {
      const v342 = stdlib.eq(v326, stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:makeEnum', stdlib.UInt_max, '1'));
      
      return v342;})()) {
      const v349 = stdlib.add(v327, v306);
      const txn3 = await (ctc.recv({
        didSend: false,
        evt_cnt: 1,
        funcNum: 4,
        out_tys: [ctc2],
        timeoutAt: ['time', v349],
        waitIfNotPresent: false
        }));
      if (txn3.didTimeout) {
        const txn4 = await (ctc.sendrecv({
          args: [v304, v305, v306, v321, v334, v349],
          evt_cnt: 0,
          funcNum: 5,
          lct: v327,
          onlyIf: true,
          out_tys: [],
          pay: [stdlib.checkedBigNumberify('reach standard library:189:11:decimal', stdlib.UInt_max, '0'), []],
          sim_p: (async (txn4) => {
            const sim_r = { txns: [], mapRefs: [], maps: [] };
            let sim_txn_ctr = stdlib.UInt_max;
            const getSimTokCtr = () => { sim_txn_ctr = sim_txn_ctr.sub(1); return sim_txn_ctr; };
            
            
            const {data: [], secs: v437, time: v436, didSend: v206, from: v435 } = txn4;
            
            ;
            const v438 = stdlib.addressEq(v304, v435);
            const v439 = stdlib.addressEq(v321, v435);
            const v440 = v438 ? true : v439;
            ;
            sim_r.txns.push({
              amt: v334,
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
          tys: [ctc4, ctc0, ctc0, ctc4, ctc0, ctc0],
          waitIfNotPresent: false
          }));
        const {data: [], secs: v437, time: v436, didSend: v206, from: v435 } = txn4;
        ;
        const v438 = stdlib.addressEq(v304, v435);
        const v439 = stdlib.addressEq(v321, v435);
        const v440 = v438 ? true : v439;
        stdlib.assert(v440, {
          at: 'reach standard library:189:11:dot',
          fs: ['at ./examples/rps-8-interact/index.rsh:70:53:application call to "closeTo" (defined at: reach standard library:187:8:function exp)'],
          msg: 'sender correct',
          who: 'Bob'
          });
        ;
        stdlib.protect(ctc1, await interact.informTimeout(), {
          at: './examples/rps-8-interact/index.rsh:41:29:application',
          fs: ['at ./examples/rps-8-interact/index.rsh:40:9:application call to [unknown function] (defined at: ./examples/rps-8-interact/index.rsh:40:27:function exp)', 'at reach standard library:192:8:application call to "after" (defined at: ./examples/rps-8-interact/index.rsh:39:28:function exp)', 'at ./examples/rps-8-interact/index.rsh:70:53:application call to "closeTo" (defined at: reach standard library:187:8:function exp)'],
          msg: 'informTimeout',
          who: 'Bob'
          });
        
        return;
        
        }
      else {
        const {data: [v358], secs: v360, time: v359, didSend: v91, from: v357 } = txn3;
        ;
        const v361 = stdlib.addressEq(v304, v357);
        stdlib.assert(v361, {
          at: './examples/rps-8-interact/index.rsh:69:11:dot',
          fs: [],
          msg: 'sender correct',
          who: 'Bob'
          });
        const v368 = stdlib.add(v359, v306);
        const v372 = stdlib.protect(ctc0, await interact.getHand(), {
          at: './examples/rps-8-interact/index.rsh:75:50:application',
          fs: ['at ./examples/rps-8-interact/index.rsh:74:13:application call to [unknown function] (defined at: ./examples/rps-8-interact/index.rsh:74:17:function exp)'],
          msg: 'getHand',
          who: 'Bob'
          });
        
        const txn4 = await (ctc.sendrecv({
          args: [v304, v305, v306, v321, v334, v358, v368, v372],
          evt_cnt: 1,
          funcNum: 6,
          lct: v359,
          onlyIf: true,
          out_tys: [ctc0],
          pay: [stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:77:9:decimal', stdlib.UInt_max, '0'), []],
          sim_p: (async (txn4) => {
            const sim_r = { txns: [], mapRefs: [], maps: [] };
            let sim_txn_ctr = stdlib.UInt_max;
            const getSimTokCtr = () => { sim_txn_ctr = sim_txn_ctr.sub(1); return sim_txn_ctr; };
            
            
            const {data: [v374], secs: v376, time: v375, didSend: v101, from: v373 } = txn4;
            
            ;
            const v377 = stdlib.addressEq(v321, v373);
            ;
            const v384 = stdlib.add(v375, v306);
            sim_r.isHalt = false;
            
            return sim_r;
            }),
          soloSend: true,
          timeoutAt: ['time', v368],
          tys: [ctc4, ctc0, ctc0, ctc4, ctc0, ctc2, ctc0, ctc0],
          waitIfNotPresent: false
          }));
        if (txn4.didTimeout) {
          const txn5 = await (ctc.sendrecv({
            args: [v304, v305, v306, v321, v334, v358, v368],
            evt_cnt: 0,
            funcNum: 7,
            lct: v359,
            onlyIf: true,
            out_tys: [],
            pay: [stdlib.checkedBigNumberify('reach standard library:189:11:decimal', stdlib.UInt_max, '0'), []],
            sim_p: (async (txn5) => {
              const sim_r = { txns: [], mapRefs: [], maps: [] };
              let sim_txn_ctr = stdlib.UInt_max;
              const getSimTokCtr = () => { sim_txn_ctr = sim_txn_ctr.sub(1); return sim_txn_ctr; };
              
              
              const {data: [], secs: v419, time: v418, didSend: v172, from: v417 } = txn5;
              
              ;
              const v420 = stdlib.addressEq(v304, v417);
              const v421 = stdlib.addressEq(v321, v417);
              const v422 = v420 ? true : v421;
              ;
              sim_r.txns.push({
                amt: v334,
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
            tys: [ctc4, ctc0, ctc0, ctc4, ctc0, ctc2, ctc0],
            waitIfNotPresent: false
            }));
          const {data: [], secs: v419, time: v418, didSend: v172, from: v417 } = txn5;
          ;
          const v420 = stdlib.addressEq(v304, v417);
          const v421 = stdlib.addressEq(v321, v417);
          const v422 = v420 ? true : v421;
          stdlib.assert(v422, {
            at: 'reach standard library:189:11:dot',
            fs: ['at ./examples/rps-8-interact/index.rsh:78:53:application call to "closeTo" (defined at: reach standard library:187:8:function exp)'],
            msg: 'sender correct',
            who: 'Bob'
            });
          ;
          stdlib.protect(ctc1, await interact.informTimeout(), {
            at: './examples/rps-8-interact/index.rsh:41:29:application',
            fs: ['at ./examples/rps-8-interact/index.rsh:40:9:application call to [unknown function] (defined at: ./examples/rps-8-interact/index.rsh:40:27:function exp)', 'at reach standard library:192:8:application call to "after" (defined at: ./examples/rps-8-interact/index.rsh:39:28:function exp)', 'at ./examples/rps-8-interact/index.rsh:78:53:application call to "closeTo" (defined at: reach standard library:187:8:function exp)'],
            msg: 'informTimeout',
            who: 'Bob'
            });
          
          return;
          
          }
        else {
          const {data: [v374], secs: v376, time: v375, didSend: v101, from: v373 } = txn4;
          ;
          const v377 = stdlib.addressEq(v321, v373);
          stdlib.assert(v377, {
            at: './examples/rps-8-interact/index.rsh:77:9:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Bob'
            });
          const v384 = stdlib.add(v375, v306);
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
              args: [v304, v305, v306, v321, v334, v358, v374, v384],
              evt_cnt: 0,
              funcNum: 9,
              lct: v375,
              onlyIf: true,
              out_tys: [],
              pay: [stdlib.checkedBigNumberify('reach standard library:189:11:decimal', stdlib.UInt_max, '0'), []],
              sim_p: (async (txn6) => {
                const sim_r = { txns: [], mapRefs: [], maps: [] };
                let sim_txn_ctr = stdlib.UInt_max;
                const getSimTokCtr = () => { sim_txn_ctr = sim_txn_ctr.sub(1); return sim_txn_ctr; };
                
                
                const {data: [], secs: v401, time: v400, didSend: v138, from: v399 } = txn6;
                
                ;
                const v402 = stdlib.addressEq(v304, v399);
                const v403 = stdlib.addressEq(v321, v399);
                const v404 = v402 ? true : v403;
                ;
                sim_r.txns.push({
                  amt: v334,
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
              tys: [ctc4, ctc0, ctc0, ctc4, ctc0, ctc2, ctc0, ctc0],
              waitIfNotPresent: false
              }));
            const {data: [], secs: v401, time: v400, didSend: v138, from: v399 } = txn6;
            ;
            const v402 = stdlib.addressEq(v304, v399);
            const v403 = stdlib.addressEq(v321, v399);
            const v404 = v402 ? true : v403;
            stdlib.assert(v404, {
              at: 'reach standard library:189:11:dot',
              fs: ['at ./examples/rps-8-interact/index.rsh:86:53:application call to "closeTo" (defined at: reach standard library:187:8:function exp)'],
              msg: 'sender correct',
              who: 'Bob'
              });
            ;
            stdlib.protect(ctc1, await interact.informTimeout(), {
              at: './examples/rps-8-interact/index.rsh:41:29:application',
              fs: ['at ./examples/rps-8-interact/index.rsh:40:9:application call to [unknown function] (defined at: ./examples/rps-8-interact/index.rsh:40:27:function exp)', 'at reach standard library:192:8:application call to "after" (defined at: ./examples/rps-8-interact/index.rsh:39:28:function exp)', 'at ./examples/rps-8-interact/index.rsh:86:53:application call to "closeTo" (defined at: reach standard library:187:8:function exp)'],
              msg: 'informTimeout',
              who: 'Bob'
              });
            
            return;
            
            }
          else {
            const {data: [v389, v390], secs: v392, time: v391, didSend: v111, from: v388 } = txn5;
            ;
            const v393 = stdlib.addressEq(v304, v388);
            stdlib.assert(v393, {
              at: './examples/rps-8-interact/index.rsh:85:11:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Bob'
              });
            const v394 = stdlib.digest(ctc3, [v389, v390]);
            const v395 = stdlib.digestEq(v358, v394);
            stdlib.assert(v395, {
              at: 'reach standard library:58:17:application',
              fs: ['at ./examples/rps-8-interact/index.rsh:87:20:application call to "checkCommitment" (defined at: reach standard library:57:8:function exp)'],
              msg: null,
              who: 'Bob'
              });
            const v396 = stdlib.sub(stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:7:18:decimal', stdlib.UInt_max, '4'), v374);
            const v397 = stdlib.add(v390, v396);
            const v398 = stdlib.mod(v397, stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:7:34:decimal', stdlib.UInt_max, '3'));
            const cv326 = v398;
            const cv327 = v391;
            const cv334 = v334;
            
            v326 = cv326;
            v327 = cv327;
            v334 = cv334;
            
            continue;}
          
          }
        
        }
      
      }
    const v453 = stdlib.eq(v326, stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:makeEnum', stdlib.UInt_max, '2'));
    const v456 = stdlib.mul(stdlib.checkedBigNumberify('./examples/rps-8-interact/index.rsh:94:12:decimal', stdlib.UInt_max, '2'), v305);
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
    impure: [],
    pure: [],
    sigs: []
    },
  appApproval: `BiAPAAFQBQkgCAcoAniAAQRYMCYDAQABAQAiNQAxGEEEjypkSSJbNQEhBls1AjYaABdJQQAHIjUEIzUGADYaAhc1BDYaAzYaARdJJQxAAixJIQcMQAE4SSEGDEAA3kkhBAxAAFYhBBJEIQQ0ARJENARJIhJMNAISEUQoZClkUEk1A1cwIDX/gASiBWaOsDIGNAMhC1sPRDQDVwAgMQASNP8xABIRRLEisgE0AyRbsggjshA0/7IHs0IDp0ghBDQBEkQ0BEkiEkw0AhIRRChkKWRQSTUDVwAgNf9JNQVJIls1/iEGWzX9gAQ1GirQNP4WUDT9FlCwMgY0AyELWwxENP8xABJENANXWCA0/hY0/RZQARJENP80AyEFWzQDIQhbNANXMCA0/SEMNAMhClsJCIEDGDIGNAMkW0ICxUghBzQBEkQ0BEkiEkw0AhIRRChkKWRQSTUDVwAgNf+ABOIbs6mwMgY0AyEKWw9ENP8xABI0A1cwIDEAEhFEsSKyATQDJFuyCCOyEDT/sgezQgLTSYEGDEAAl0ghBzQBEkQ0BEkiEkw0AhIRRChkKWRQSTUDSUpKVwAgNf8hBVs1/iEIWzX9VzAgNfwkWzX7V1ggNfpJNQUXNfmABHDt73o0+RZQsDIGNAMhClsMRDT8MQASRDIGNP0INfg0/zT+FlA0/RZQNPxQNPsWUDT6UDT5FlA0+BZQKEsBVwB/ZylLAVd/CWdIIQQ1ATIGNQJCAlBIJTQBEkQ0BEkiEkw0AhIRRChkSTUDVzAgNf+ABMyZklywMgY0AyENWw9ENANXACAxABI0/zEAEhFEsSKyATQDJFuyCCOyEDT/sgezQgHmSSEJDEAA0EkhDAxAAIhIJTQBEkQ0BEkiEkw0AhIRRChkSTUDSUpJVwAgNf8hBVs1/iEIWzX9VzAgNfwkWzX7STUFNfqABDiwIy00+lCwMgY0AyENWwxENP8xABJEMgY0/Qg1+TT/NP4WUDT9FlA0/FA0+xZQNPpQNPkWUChLAVcAf2cpSwFXfwFnSCEHNQEyBjUCQgFrIQkSRCM0ARJENARJIhJMNAISEUQoZDUDgARBsUBNsDIGNAMhDlsPRLEisgE0AyEFW7III7IQNANXACCyB7NCAQ9JIwxAAEhIIzQBEkQ0BEkiEkw0AhIRRChkSTUDIQVbNf+ABJqLkXSwMgY0AyEOWwxENP+IATE0A1cAIDT/NAMhCFsxACMyBjT/SQhCAGBIIjQBEkQ0BEkiEkw0AhIRREk1BUkiWzX/IQZbNf6ABKzRH8M0/xZQNP4WULCBoI0GiADlNP+IAOAyBjT+CDX9MQA0/xZQNP4WUDT9FlAoSwFXADhnSCM1ATIGNQJCAHw1/zX+Nf01/DX7Nfo1+TT9IxJBAC40/jT7CDX4NPk0+hZQNPsWUDT8UDT/FlA0+BZQKEsBVwBgZ0glNQEyBjUCQgA5sSKyASEJNPoLsggjshA0/DT5NP0hCRJNsgezQgAAMRklEkSxIrIBIrIII7IQMgmyCTIKsgezQgAFMRkiEkQqNAEWNAIWUGc0BkEACoAEFR98dTQHULA0AEkjCDIEEkQxFhJEI0MxGSISREL/3yI1ASI1AkL/wzQASUojCDUAOAcyChJEOBAjEkQ4CBJEiQ==`,
  appClear: `Bg==`,
  companionInfo: null,
  extraPages: 0,
  mapDataKeys: 0,
  mapDataSize: 0,
  stateKeys: 2,
  stateSize: 136,
  unsupported: [],
  version: 10,
  warnings: []
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
                "name": "v305",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v306",
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
                "name": "v305",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v306",
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
                "name": "v358",
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
                "name": "v374",
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
                "name": "v358",
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
                "name": "v374",
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
  Bytecode: `0x608060405260405162001b0d38038062001b0d833981016040819052620000269162000248565b600080805543600355604080516020810190915290815260408051835181526020808501518051828401520151918101919091527f80c0078efe412e5091172e0df54decefb16131f320816d23b64aede2bf8e9e4b9060600160405180910390a16020820151516200009c903414600762000141565b6020808301510151620000b09043620002a8565b81526040805160808082018352600060208084018281528486018381526060808701858152338089528b860180515186525186015184528a5182526001968790554390965588518086019690965292518589015290519084015251828401528451808303909301835260a0909101909352805191926200013792600292909101906200016b565b505050506200030c565b81620001675760405163100960cb60e01b81526004810182905260240160405180910390fd5b5050565b8280546200017990620002cf565b90600052602060002090601f0160209004810192826200019d5760008555620001e8565b82601f10620001b857805160ff1916838001178555620001e8565b82800160010185558215620001e8579182015b82811115620001e8578251825591602001919060010190620001cb565b50620001f6929150620001fa565b5090565b5b80821115620001f65760008155600101620001fb565b604080519081016001600160401b03811182821017156200024257634e487b7160e01b600052604160045260246000fd5b60405290565b600081830360608112156200025c57600080fd5b6200026662000211565b835181526040601f19830112156200027d57600080fd5b6200028762000211565b60208581015182526040909501518582015293810193909352509092915050565b60008219821115620002ca57634e487b7160e01b600052601160045260246000fd5b500190565b600181811c90821680620002e457607f821691505b602082108114156200030657634e487b7160e01b600052602260045260246000fd5b50919050565b6117f1806200031c6000396000f3fe60806040526004361061009a5760003560e01c80638e314769116100615780638e31476914610115578063980b6eac14610128578063a209ad4e1461013b578063ab53f2c61461014e578063bf2c5b2414610171578063de7369981461018457005b80631e93b0f1146100a35780632c10a159146100c75780637eea518c146100da57806383230757146100ed5780638328d4c41461010257005b366100a157005b005b3480156100af57600080fd5b506003545b6040519081526020015b60405180910390f35b6100a16100d53660046113d1565b610197565b6100a16100e83660046113d1565b61032c565b3480156100f957600080fd5b506001546100b4565b6100a16101103660046113f4565b6104a8565b6100a16101233660046113d1565b6106cc565b6100a16101363660046113d1565b610865565b6100a16101493660046113d1565b610b0a565b34801561015a57600080fd5b50610163610d52565b6040516100be929190611406565b6100a161017f3660046113d1565b610def565b6100a16101923660046113d1565b610f84565b6101a760016000541460096110d8565b6101c1813515806101ba57506001548235145b600a6110d8565b6000808055600280546101d390611463565b80601f01602080910402602001604051908101604052809291908181526020018280546101ff90611463565b801561024c5780601f106102215761010080835404028352916020019161024c565b820191906000526020600020905b81548152906001019060200180831161022f57829003601f168201915b505050505080602001905181019061026491906114b4565b905061027781606001514310600b6110d8565b7f79ca1a789d797004bc78dff9632d64e202e102f2d008dcc20c5a645ef7d4a7d1826040516102a6919061152d565b60405180910390a16102bf8160200151341460086110d8565b6102c761128a565b815181516001600160a01b039091169052602080830180518351830152604080850151845190910152825133606090910152818301805160019052514392019190915251610315908061156d565b602082015160400152610327816110fd565b505050565b61033c600160005414600d6110d8565b6103568135158061034f57506001548235145b600e6110d8565b60008080556002805461036890611463565b80601f016020809104026020016040519081016040528092919081815260200182805461039490611463565b80156103e15780601f106103b6576101008083540402835291602001916103e1565b820191906000526020600020905b8154815290600101906020018083116103c457829003601f168201915b50505050508060200190518101906103f991906114b4565b905061040d8160600151431015600f6110d8565b7f82e152e8b1d7e41adffbddbd5b2fe2e130356df9b7ab7d06526a80d7888af3e18260405161043c919061152d565b60405180910390a16104503415600c6110d8565b805160208201516040516001600160a01b039092169181156108fc0291906000818181858888f1935050505015801561048d573d6000803e3d6000fd5b50600080805560018190556104a4906002906112e3565b5050565b6104b860096000541460276110d8565b6104d2813515806104cb57506001548235145b60286110d8565b6000808055600280546104e490611463565b80601f016020809104026020016040519081016040528092919081815260200182805461051090611463565b801561055d5780601f106105325761010080835404028352916020019161055d565b820191906000526020600020905b81548152906001019060200180831161054057829003601f168201915b50505050508060200190518101906105759190611585565b90506105888160e00151431060296110d8565b604080518335815260208085013590820152838201358183015290517f41b6d8e223fb0a5cfe68af9f34b07a5a94b63517841457ccfc53fb18b8e41fde9181900360600190a16105da341560246110d8565b80516105f2906001600160a01b0316331460256110d8565b6040805161063e9161061891602080870135928701359101918252602082015260400190565b6040516020818303038152906040528051906020012060001c8260a001511460266110d8565b61064661128a565b815181516001600160a01b0391821690526020808401518351909101526040808401518351909101526060808401518351921691015260c082015160039061068f906004611631565b61069d90604086013561156d565b6106a79190611648565b60208083018051929092528151439101526080830151905160400152610327816110fd565b6106dc60056000541460176110d8565b6106f6813515806106ef57506001548235145b60186110d8565b60008080556002805461070890611463565b80601f016020809104026020016040519081016040528092919081815260200182805461073490611463565b80156107815780601f1061075657610100808354040283529160200191610781565b820191906000526020600020905b81548152906001019060200180831161076457829003601f168201915b5050505050806020019051810190610799919061166a565b90506107ad8160a0015143101560196110d8565b7f9cdba579557d44a893ea7929682d6795d48dd5c40dc981d852842d4b18914de8826040516107dc919061152d565b60405180910390a16107f0341560156110d8565b8051610824906001600160a01b0316331461081a5760608201516001600160a01b0316331461081d565b60015b60166110d8565b80606001516001600160a01b03166108fc82608001519081150290604051600060405180830381858888f1935050505015801561048d573d6000803e3d6000fd5b610875600760005414601c6110d8565b61088f8135158061088857506001548235145b601d6110d8565b6000808055600280546108a190611463565b80601f01602080910402602001604051908101604052809291908181526020018280546108cd90611463565b801561091a5780601f106108ef5761010080835404028352916020019161091a565b820191906000526020600020905b8154815290600101906020018083116108fd57829003601f168201915b505050505080602001905181019061093291906116fe565b905061094a6040518060200160405280600081525090565b61095b8260c001514310601e6110d8565b6040805184358152602080860135908201527f47a1195f23e4ca8f87a7a744a702eeb3eb5b0d56dae31e23931e0349a611c709910160405180910390a16109a43415601a6110d8565b60608201516109bf906001600160a01b03163314601b6110d8565b60408201516109ce904361156d565b81526040805161010081018252600080825260208201819052918101829052606081018290526080810182905260a0810182905260c0810182905260e081019190915282516001600160a01b0390811682526020808501518184015260408086015181850152606080870151909316928401929092526080808601519084015260a080860151908401528581013560c0840152835160e08401526009600055436001559051610adf9183910160006101008201905060018060a01b038084511683526020840151602084015260408401516040840152806060850151166060840152506080830151608083015260a083015160a083015260c083015160c083015260e083015160e083015292915050565b60405160208183030381529060405260029080519060200190610b03929190611320565b5050505050565b610b1a60056000541460126110d8565b610b3481351580610b2d57506001548235145b60136110d8565b600080805560028054610b4690611463565b80601f0160208091040260200160405190810160405280929190818152602001828054610b7290611463565b8015610bbf5780601f10610b9457610100808354040283529160200191610bbf565b820191906000526020600020905b815481529060010190602001808311610ba257829003601f168201915b5050505050806020019051810190610bd7919061166a565b9050610bef6040518060200160405280600081525090565b610c008260a00151431060146110d8565b6040805184358152602080860135908201527f7d7741a24b17d1850d95beda5136388f520bc575ba9499f2f40fdfa7647ad82f910160405180910390a1610c49341560106110d8565b8151610c61906001600160a01b0316331460116110d8565b6040820151610c70904361156d565b81526040805160e081018252600080825260208201819052918101829052606081018290526080810182905260a0810182905260c081019190915282516001600160a01b039081168083526020808601518185019081526040808801518187019081526060808a015187168189019081526080808c0151818b019081528d88013560a0808d019182528d5160c0808f0191825260076000554360015589519b8c019c909c529851978a0197909752945193880193909352905190971696850196909652945190830152925191810191909152905160e082015261010001610adf565b600060606000546002808054610d6790611463565b80601f0160208091040260200160405190810160405280929190818152602001828054610d9390611463565b8015610de05780601f10610db557610100808354040283529160200191610de0565b820191906000526020600020905b815481529060010190602001808311610dc357829003601f168201915b50505050509050915091509091565b610dff60076000541460216110d8565b610e1981351580610e1257506001548235145b60226110d8565b600080805560028054610e2b90611463565b80601f0160208091040260200160405190810160405280929190818152602001828054610e5790611463565b8015610ea45780601f10610e7957610100808354040283529160200191610ea4565b820191906000526020600020905b815481529060010190602001808311610e8757829003601f168201915b5050505050806020019051810190610ebc91906116fe565b9050610ed08160c0015143101560236110d8565b7fba16100ad25f3c6798bc3b7e9ca316fb231388e6fa4444c0f477e2a4336514e082604051610eff919061152d565b60405180910390a1610f133415601f6110d8565b8051610f47906001600160a01b03163314610f3d5760608201516001600160a01b03163314610f40565b60015b60206110d8565b805160808201516040516001600160a01b039092169181156108fc0291906000818181858888f1935050505015801561048d573d6000803e3d6000fd5b610f94600960005414602c6110d8565b610fae81351580610fa757506001548235145b602d6110d8565b600080805560028054610fc090611463565b80601f0160208091040260200160405190810160405280929190818152602001828054610fec90611463565b80156110395780601f1061100e57610100808354040283529160200191611039565b820191906000526020600020905b81548152906001019060200180831161101c57829003601f168201915b50505050508060200190518101906110519190611585565b90506110658160e00151431015602e6110d8565b7fb764c356a899e639c4043e82fb6274894baac6d84c74f3b3ae78d8f4b22b000382604051611094919061152d565b60405180910390a16110a83415602a6110d8565b8051610824906001600160a01b031633146110d25760608201516001600160a01b031633146110d5565b60015b602b5b816104a45760405163100960cb60e01b81526004810182905260240160405180910390fd5b60408051602081019091526000815260208201515160011415611225578151604001516020808401510151611132919061156d565b81526040805160c081018252600080825260208201819052918101829052606081018290526080810182905260a08101919091528251516001600160a01b039081168083528451602090810151818501908152865160409081015181870190815288516060908101518716818901908152858b01518401516080808b019182528b5160a0808d019182526005600055436001558751998a019a909a529651958801959095529251918601919091525190951690830152925191810191909152905160c082015260e0016040516020818303038152906040526002908051906020019061121f929190611320565b50505050565b60208201515160021461123d57815160600151611241565b8151515b6001600160a01b03166108fc8360000151602001516002611262919061179c565b6040518115909202916000818181858888f1935050505015801561048d573d6000803e3d6000fd5b6040805160c0810182526000918101828152606082018390526080820183905260a082019290925290819081526020016112de60405180606001604052806000815260200160008152602001600081525090565b905290565b5080546112ef90611463565b6000825580601f106112ff575050565b601f01602090049060005260206000209081019061131d91906113a4565b50565b82805461132c90611463565b90600052602060002090601f01602090048101928261134e5760008555611394565b82601f1061136757805160ff1916838001178555611394565b82800160010185558215611394579182015b82811115611394578251825591602001919060010190611379565b506113a09291506113a4565b5090565b5b808211156113a057600081556001016113a5565b6000604082840312156113cb57600080fd5b50919050565b6000604082840312156113e357600080fd5b6113ed83836113b9565b9392505050565b6000606082840312156113cb57600080fd5b82815260006020604081840152835180604085015260005b8181101561143a5785810183015185820160600152820161141e565b8181111561144c576000606083870101525b50601f01601f191692909201606001949350505050565b600181811c9082168061147757607f821691505b602082108114156113cb57634e487b7160e01b600052602260045260246000fd5b80516001600160a01b03811681146114af57600080fd5b919050565b6000608082840312156114c657600080fd5b6040516080810181811067ffffffffffffffff821117156114f757634e487b7160e01b600052604160045260246000fd5b60405261150383611498565b81526020830151602082015260408301516040820152606083015160608201528091505092915050565b8135815260408101602083013580151580821461154957600080fd5b806020850152505092915050565b634e487b7160e01b600052601160045260246000fd5b6000821982111561158057611580611557565b500190565b600061010080838503121561159957600080fd5b6040519081019067ffffffffffffffff821181831017156115ca57634e487b7160e01b600052604160045260246000fd5b816040526115d784611498565b815260208401516020820152604084015160408201526115f960608501611498565b60608201526080840151608082015260a084015160a082015260c084015160c082015260e084015160e0820152809250505092915050565b60008282101561164357611643611557565b500390565b60008261166557634e487b7160e01b600052601260045260246000fd5b500690565b600060c0828403121561167c57600080fd5b60405160c0810181811067ffffffffffffffff821117156116ad57634e487b7160e01b600052604160045260246000fd5b6040526116b983611498565b815260208301516020820152604083015160408201526116db60608401611498565b60608201526080830151608082015260a083015160a08201528091505092915050565b600060e0828403121561171057600080fd5b60405160e0810181811067ffffffffffffffff8211171561174157634e487b7160e01b600052604160045260246000fd5b60405261174d83611498565b8152602083015160208201526040830151604082015261176f60608401611498565b60608201526080830151608082015260a083015160a082015260c083015160c08201528091505092915050565b60008160001904831182151516156117b6576117b6611557565b50029056fea264697066735822122007cea3a70f0ecfb5da57d8f03a3e55c6795987650d355d432fa7741e905ef36c64736f6c634300080c0033`,
  BytecodeLen: 6925,
  Which: `oD`,
  version: 6,
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
    at: 'reach standard library:191:11:after expr stmt semicolon',
    fs: ['at ./examples/rps-8-interact/index.rsh:57:51:application call to "closeTo" (defined at: reach standard library:187:8:function exp)'],
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
    at: 'reach standard library:191:11:after expr stmt semicolon',
    fs: ['at ./examples/rps-8-interact/index.rsh:70:53:application call to "closeTo" (defined at: reach standard library:187:8:function exp)'],
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
    at: 'reach standard library:191:11:after expr stmt semicolon',
    fs: ['at ./examples/rps-8-interact/index.rsh:78:53:application call to "closeTo" (defined at: reach standard library:187:8:function exp)'],
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
    at: 'reach standard library:191:11:after expr stmt semicolon',
    fs: ['at ./examples/rps-8-interact/index.rsh:86:53:application call to "closeTo" (defined at: reach standard library:187:8:function exp)'],
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
