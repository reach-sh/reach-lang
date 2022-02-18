// Automatically generated with Reach 0.1.8 (2460e416*)
/* eslint-disable */
export const _version = '0.1.8';
export const _versionHash = '0.1.8 (2460e416*)';
export const _backendVersion = 10;

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
  const ctc1 = stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, 32));
  const ctc2 = stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, 8));
  const ctc3 = stdlib.T_UInt;
  const ctc4 = stdlib.T_Token;
  
  return {
    infos: {
      },
    views: {
      1: [ctc0, ctc1, ctc2, ctc3, ctc3, ctc4],
      2: [ctc0, ctc1, ctc2, ctc3, ctc3, ctc4, ctc0],
      3: [ctc0, ctc1, ctc2, ctc3, ctc4, ctc0, ctc3],
      4: [ctc0, ctc1, ctc2, ctc3, ctc4, ctc0, ctc3],
      5: [ctc0, ctc1, ctc2, ctc3, ctc4, ctc0, ctc3],
      6: [ctc0, ctc0, ctc3, ctc4],
      7: [ctc0, ctc0, ctc3, ctc4, ctc3],
      8: [ctc0, ctc0, ctc3, ctc4, ctc3],
      9: [ctc0, ctc3, ctc4, ctc3]
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
  const ctc1 = stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, 32));
  const ctc2 = stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, 8));
  const ctc3 = stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, 96));
  const ctc4 = stdlib.T_Object({
    amt: ctc0,
    metadata: ctc1,
    name: ctc1,
    supply: ctc0,
    symbol: ctc2,
    url: ctc3
    });
  const ctc5 = stdlib.T_Token;
  const ctc6 = stdlib.T_Null;
  const ctc7 = stdlib.T_Address;
  
  
  const v225 = stdlib.protect(ctc4, await interact.getParams(), {
    at: './index.rsh:23:87:application',
    fs: ['at ./index.rsh:22:9:application call to [unknown function] (defined at: ./index.rsh:22:13:function exp)'],
    msg: 'getParams',
    who: 'Alice'
    });
  const v226 = v225.name;
  const v227 = v225.symbol;
  const v228 = v225.url;
  const v229 = v225.metadata;
  const v230 = v225.supply;
  const v231 = v225.amt;
  const v232 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:24:12:decimal', stdlib.UInt_max, 4), v231);
  const v233 = stdlib.le(v232, v230);
  stdlib.assert(v233, {
    at: './index.rsh:24:11:application',
    fs: ['at ./index.rsh:22:9:application call to [unknown function] (defined at: ./index.rsh:22:13:function exp)'],
    msg: null,
    who: 'Alice'
    });
  const v235 = stdlib.le(v232, stdlib.UInt_max);
  stdlib.assert(v235, {
    at: './index.rsh:25:11:application',
    fs: ['at ./index.rsh:22:9:application call to [unknown function] (defined at: ./index.rsh:22:13:function exp)'],
    msg: null,
    who: 'Alice'
    });
  
  const txn1 = await (ctc.sendrecv({
    args: [v226, v227, v228, v229, v230, v231],
    evt_cnt: 6,
    funcNum: 0,
    lct: stdlib.checkedBigNumberify('./index.rsh:27:5:dot', stdlib.UInt_max, 0),
    onlyIf: true,
    out_tys: [ctc1, ctc2, ctc3, ctc1, ctc0, ctc0],
    pay: [stdlib.checkedBigNumberify('./index.rsh:27:5:decimal', stdlib.UInt_max, 0), []],
    sim_p: (async (txn1) => {
      const sim_r = { txns: [], mapRefs: [], maps: [] };
      
      const {data: [v237, v238, v239, v240, v241, v242], secs: v244, time: v243, didSend: v43, from: v236 } = txn1;
      
      sim_r.txns.push({
        amt: stdlib.checkedBigNumberify('./index.rsh:27:5:decimal', stdlib.UInt_max, 0),
        kind: 'to',
        tok: undefined
        });
      const v246 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:28:11:decimal', stdlib.UInt_max, 4), v242);
      const v247 = stdlib.le(v246, v241);
      stdlib.assert(v247, {
        at: './index.rsh:28:10:application',
        fs: [],
        msg: null,
        who: 'Alice'
        });
      const v249 = stdlib.le(v246, stdlib.UInt_max);
      stdlib.assert(v249, {
        at: './index.rsh:29:10:application',
        fs: [],
        msg: null,
        who: 'Alice'
        });
      const v250 = stdlib.simTokenNew(sim_r, v237, v238, v239, v240, v241, undefined);
      const v251 = await txn1.getOutput('internal', 'v250', ctc5, v250);
      
      sim_r.isHalt = false;
      
      return sim_r;
      }),
    soloSend: true,
    timeoutAt: undefined,
    tys: [ctc1, ctc2, ctc3, ctc1, ctc0, ctc0],
    waitIfNotPresent: false
    }));
  const {data: [v237, v238, v239, v240, v241, v242], secs: v244, time: v243, didSend: v43, from: v236 } = txn1;
  ;
  const v246 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:28:11:decimal', stdlib.UInt_max, 4), v242);
  const v247 = stdlib.le(v246, v241);
  stdlib.assert(v247, {
    at: './index.rsh:28:10:application',
    fs: [],
    msg: null,
    who: 'Alice'
    });
  const v249 = stdlib.le(v246, stdlib.UInt_max);
  stdlib.assert(v249, {
    at: './index.rsh:29:10:application',
    fs: [],
    msg: null,
    who: 'Alice'
    });
  const v250 = undefined;
  const v251 = await txn1.getOutput('internal', 'v250', ctc5, v250);
  const v253 = {
    metadata: v240,
    name: v237,
    supply: v241,
    symbol: v238,
    url: v239
    };
  stdlib.protect(ctc6, await interact.showToken(v251, v253), {
    at: './index.rsh:1:39:application',
    fs: ['at ./index.rsh:1:21:application call to [unknown function] (defined at: ./index.rsh:1:25:function exp)', 'at ./index.rsh:33:23:application call to "liftedInteract" (defined at: ./index.rsh:33:23:application)'],
    msg: 'showToken',
    who: 'Alice'
    });
  
  const txn2 = await (ctc.recv({
    didSend: false,
    evt_cnt: 0,
    funcNum: 1,
    out_tys: [],
    timeoutAt: undefined,
    waitIfNotPresent: false
    }));
  const {data: [], secs: v256, time: v255, didSend: v59, from: v254 } = txn2;
  ;
  const txn3 = await (ctc.recv({
    didSend: false,
    evt_cnt: 0,
    funcNum: 2,
    out_tys: [],
    timeoutAt: undefined,
    waitIfNotPresent: false
    }));
  const {data: [], secs: v262, time: v261, didSend: v70, from: v260 } = txn3;
  ;
  const v264 = stdlib.addressEq(v254, v260);
  stdlib.assert(v264, {
    at: './index.rsh:45:5:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Alice'
    });
  const v265 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:41:14:decimal', stdlib.UInt_max, 2), v242);
  ;
  const txn4 = await (ctc.sendrecv({
    args: [v236, v237, v238, v241, v251, v254, v265],
    evt_cnt: 0,
    funcNum: 3,
    lct: v261,
    onlyIf: true,
    out_tys: [],
    pay: [stdlib.checkedBigNumberify('./index.rsh:48:5:decimal', stdlib.UInt_max, 0), []],
    sim_p: (async (txn4) => {
      const sim_r = { txns: [], mapRefs: [], maps: [] };
      
      const {data: [], secs: v274, time: v273, didSend: v86, from: v272 } = txn4;
      
      sim_r.txns.push({
        amt: stdlib.checkedBigNumberify('./index.rsh:48:5:decimal', stdlib.UInt_max, 0),
        kind: 'to',
        tok: undefined
        });
      const v276 = stdlib.addressEq(v236, v272);
      stdlib.assert(v276, {
        at: './index.rsh:48:5:dot',
        fs: [],
        msg: 'sender correct',
        who: 'Alice'
        });
      sim_r.txns.push({
        amt: v265,
        kind: 'from',
        to: v236,
        tok: v251
        });
      
      sim_r.isHalt = false;
      
      return sim_r;
      }),
    soloSend: true,
    timeoutAt: undefined,
    tys: [ctc7, ctc1, ctc2, ctc0, ctc5, ctc7, ctc0],
    waitIfNotPresent: false
    }));
  const {data: [], secs: v274, time: v273, didSend: v86, from: v272 } = txn4;
  ;
  const v276 = stdlib.addressEq(v236, v272);
  stdlib.assert(v276, {
    at: './index.rsh:48:5:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Alice'
    });
  ;
  stdlib.protect(ctc6, await interact.didTransfer(true, v242), {
    at: './index.rsh:1:39:application',
    fs: ['at ./index.rsh:1:21:application call to [unknown function] (defined at: ./index.rsh:1:25:function exp)', 'at ./index.rsh:42:29:application call to "liftedInteract" (defined at: ./index.rsh:42:29:application)', 'at ./index.rsh:49:14:application call to "doTransfer1" (defined at: ./index.rsh:40:35:function exp)'],
    msg: 'didTransfer',
    who: 'Alice'
    });
  
  const txn5 = await (ctc.sendrecv({
    args: [v236, v237, v238, v241, v251, v254, v265],
    evt_cnt: 0,
    funcNum: 4,
    lct: v273,
    onlyIf: true,
    out_tys: [],
    pay: [stdlib.checkedBigNumberify('./index.rsh:51:5:dot', stdlib.UInt_max, 0), [[v265, v251]]],
    sim_p: (async (txn5) => {
      const sim_r = { txns: [], mapRefs: [], maps: [] };
      
      const {data: [], secs: v287, time: v286, didSend: v106, from: v285 } = txn5;
      
      sim_r.txns.push({
        amt: stdlib.checkedBigNumberify('./index.rsh:51:5:dot', stdlib.UInt_max, 0),
        kind: 'to',
        tok: undefined
        });
      sim_r.txns.push({
        amt: v265,
        kind: 'to',
        tok: v251
        });
      const v292 = stdlib.addressEq(v236, v285);
      stdlib.assert(v292, {
        at: './index.rsh:51:5:dot',
        fs: [],
        msg: 'sender correct',
        who: 'Alice'
        });
      sim_r.isHalt = false;
      
      return sim_r;
      }),
    soloSend: true,
    timeoutAt: undefined,
    tys: [ctc7, ctc1, ctc2, ctc0, ctc5, ctc7, ctc0],
    waitIfNotPresent: false
    }));
  const {data: [], secs: v287, time: v286, didSend: v106, from: v285 } = txn5;
  ;
  ;
  const v292 = stdlib.addressEq(v236, v285);
  stdlib.assert(v292, {
    at: './index.rsh:51:5:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Alice'
    });
  const txn6 = await (ctc.recv({
    didSend: false,
    evt_cnt: 0,
    funcNum: 5,
    out_tys: [],
    timeoutAt: undefined,
    waitIfNotPresent: false
    }));
  const {data: [], secs: v296, time: v295, didSend: v116, from: v294 } = txn6;
  ;
  ;
  const v301 = stdlib.addressEq(v254, v294);
  stdlib.assert(v301, {
    at: './index.rsh:53:5:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Alice'
    });
  undefined;
  undefined;
  const v312 = '                                                                                                ';
  const v313 = '                                ';
  const v314 = undefined;
  const v315 = await txn6.getOutput('internal', 'v314', ctc5, v314);
  const v317 = {
    name: v237,
    symbol: v238
    };
  stdlib.protect(ctc6, await interact.showToken(v315, v317), {
    at: './index.rsh:1:39:application',
    fs: ['at ./index.rsh:1:21:application call to [unknown function] (defined at: ./index.rsh:1:25:function exp)', 'at ./index.rsh:59:23:application call to "liftedInteract" (defined at: ./index.rsh:59:23:application)'],
    msg: 'showToken',
    who: 'Alice'
    });
  
  const txn7 = await (ctc.recv({
    didSend: false,
    evt_cnt: 0,
    funcNum: 6,
    out_tys: [],
    timeoutAt: undefined,
    waitIfNotPresent: false
    }));
  const {data: [], secs: v322, time: v321, didSend: v146, from: v320 } = txn7;
  ;
  const v324 = stdlib.addressEq(v254, v320);
  stdlib.assert(v324, {
    at: './index.rsh:63:5:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Alice'
    });
  const v329 = stdlib.sub(stdlib.UInt_max, v265);
  ;
  const txn8 = await (ctc.sendrecv({
    args: [v236, v254, v265, v315, v329],
    evt_cnt: 0,
    funcNum: 7,
    lct: v321,
    onlyIf: true,
    out_tys: [],
    pay: [stdlib.checkedBigNumberify('./index.rsh:66:5:decimal', stdlib.UInt_max, 0), []],
    sim_p: (async (txn8) => {
      const sim_r = { txns: [], mapRefs: [], maps: [] };
      
      const {data: [], secs: v334, time: v333, didSend: v162, from: v332 } = txn8;
      
      sim_r.txns.push({
        amt: stdlib.checkedBigNumberify('./index.rsh:66:5:decimal', stdlib.UInt_max, 0),
        kind: 'to',
        tok: undefined
        });
      const v336 = stdlib.addressEq(v236, v332);
      stdlib.assert(v336, {
        at: './index.rsh:66:5:dot',
        fs: [],
        msg: 'sender correct',
        who: 'Alice'
        });
      const v341 = stdlib.sub(v329, v265);
      sim_r.txns.push({
        amt: v265,
        kind: 'from',
        to: v236,
        tok: v315
        });
      
      const v348 = stdlib.sub(v341, v341);
      stdlib.simTokenBurn(sim_r, v315, v341);
      sim_r.isHalt = false;
      
      return sim_r;
      }),
    soloSend: true,
    timeoutAt: undefined,
    tys: [ctc7, ctc7, ctc0, ctc5, ctc0],
    waitIfNotPresent: false
    }));
  const {data: [], secs: v334, time: v333, didSend: v162, from: v332 } = txn8;
  ;
  const v336 = stdlib.addressEq(v236, v332);
  stdlib.assert(v336, {
    at: './index.rsh:66:5:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Alice'
    });
  const v341 = stdlib.sub(v329, v265);
  ;
  stdlib.protect(ctc6, await interact.didTransfer(true, v242), {
    at: './index.rsh:1:39:application',
    fs: ['at ./index.rsh:1:21:application call to [unknown function] (defined at: ./index.rsh:1:25:function exp)', 'at ./index.rsh:42:29:application call to "liftedInteract" (defined at: ./index.rsh:42:29:application)', 'at ./index.rsh:67:14:application call to "doTransfer1" (defined at: ./index.rsh:40:35:function exp)'],
    msg: 'didTransfer',
    who: 'Alice'
    });
  
  const v348 = stdlib.sub(v341, v341);
  undefined;
  const txn9 = await (ctc.sendrecv({
    args: [v236, v254, v265, v315, v348],
    evt_cnt: 0,
    funcNum: 8,
    lct: v333,
    onlyIf: true,
    out_tys: [],
    pay: [stdlib.checkedBigNumberify('./index.rsh:70:5:dot', stdlib.UInt_max, 0), [[v265, v315]]],
    sim_p: (async (txn9) => {
      const sim_r = { txns: [], mapRefs: [], maps: [] };
      
      const {data: [], secs: v354, time: v353, didSend: v189, from: v352 } = txn9;
      
      sim_r.txns.push({
        amt: stdlib.checkedBigNumberify('./index.rsh:70:5:dot', stdlib.UInt_max, 0),
        kind: 'to',
        tok: undefined
        });
      const v358 = stdlib.add(v348, v265);
      sim_r.txns.push({
        amt: v265,
        kind: 'to',
        tok: v315
        });
      const v359 = stdlib.addressEq(v236, v352);
      stdlib.assert(v359, {
        at: './index.rsh:70:5:dot',
        fs: [],
        msg: 'sender correct',
        who: 'Alice'
        });
      sim_r.isHalt = false;
      
      return sim_r;
      }),
    soloSend: true,
    timeoutAt: undefined,
    tys: [ctc7, ctc7, ctc0, ctc5, ctc0],
    waitIfNotPresent: false
    }));
  const {data: [], secs: v354, time: v353, didSend: v189, from: v352 } = txn9;
  ;
  const v358 = stdlib.add(v348, v265);
  ;
  const v359 = stdlib.addressEq(v236, v352);
  stdlib.assert(v359, {
    at: './index.rsh:70:5:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Alice'
    });
  const txn10 = await (ctc.recv({
    didSend: false,
    evt_cnt: 0,
    funcNum: 9,
    out_tys: [],
    timeoutAt: undefined,
    waitIfNotPresent: false
    }));
  const {data: [], secs: v363, time: v362, didSend: v199, from: v361 } = txn10;
  ;
  const v367 = stdlib.add(v358, v265);
  ;
  const v368 = stdlib.addressEq(v254, v361);
  stdlib.assert(v368, {
    at: './index.rsh:72:5:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Alice'
    });
  undefined;
  undefined;
  return;
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  };
export async function Bob(ctcTop, interact) {
  if (typeof(ctcTop) !== 'object' || ctcTop._initialize === undefined) {
    return Promise.reject(new Error(`The backend for Bob expects to receive a contract as its first argument.`));}
  if (typeof(interact) !== 'object') {
    return Promise.reject(new Error(`The backend for Bob expects to receive an interact object as its second argument.`));}
  const ctc = ctcTop._initialize();
  const stdlib = ctc.stdlib;
  const ctc0 = stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, 32));
  const ctc1 = stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, 8));
  const ctc2 = stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, 96));
  const ctc3 = stdlib.T_UInt;
  const ctc4 = stdlib.T_Token;
  const ctc5 = stdlib.T_Null;
  const ctc6 = stdlib.T_Address;
  
  
  const txn1 = await (ctc.recv({
    didSend: false,
    evt_cnt: 6,
    funcNum: 0,
    out_tys: [ctc0, ctc1, ctc2, ctc0, ctc3, ctc3],
    timeoutAt: undefined,
    waitIfNotPresent: false
    }));
  const {data: [v237, v238, v239, v240, v241, v242], secs: v244, time: v243, didSend: v43, from: v236 } = txn1;
  ;
  const v246 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:28:11:decimal', stdlib.UInt_max, 4), v242);
  const v247 = stdlib.le(v246, v241);
  stdlib.assert(v247, {
    at: './index.rsh:28:10:application',
    fs: [],
    msg: null,
    who: 'Bob'
    });
  const v249 = stdlib.le(v246, stdlib.UInt_max);
  stdlib.assert(v249, {
    at: './index.rsh:29:10:application',
    fs: [],
    msg: null,
    who: 'Bob'
    });
  const v250 = undefined;
  const v251 = await txn1.getOutput('internal', 'v250', ctc4, v250);
  const txn2 = await (ctc.sendrecv({
    args: [v236, v237, v238, v241, v242, v251],
    evt_cnt: 0,
    funcNum: 1,
    lct: v243,
    onlyIf: true,
    out_tys: [],
    pay: [stdlib.checkedBigNumberify('./index.rsh:36:5:decimal', stdlib.UInt_max, 0), []],
    sim_p: (async (txn2) => {
      const sim_r = { txns: [], mapRefs: [], maps: [] };
      
      const {data: [], secs: v256, time: v255, didSend: v59, from: v254 } = txn2;
      
      sim_r.txns.push({
        amt: stdlib.checkedBigNumberify('./index.rsh:36:5:decimal', stdlib.UInt_max, 0),
        kind: 'to',
        tok: undefined
        });
      
      sim_r.isHalt = false;
      
      return sim_r;
      }),
    soloSend: true,
    timeoutAt: undefined,
    tys: [ctc6, ctc0, ctc1, ctc3, ctc3, ctc4],
    waitIfNotPresent: false
    }));
  const {data: [], secs: v256, time: v255, didSend: v59, from: v254 } = txn2;
  ;
  const v259 = {
    metadata: v240,
    name: v237,
    supply: v241,
    symbol: v238,
    url: v239
    };
  stdlib.protect(ctc5, await interact.showToken(v251, v259), {
    at: './index.rsh:1:39:application',
    fs: ['at ./index.rsh:1:21:application call to [unknown function] (defined at: ./index.rsh:1:25:function exp)', 'at ./index.rsh:37:23:application call to "liftedInteract" (defined at: ./index.rsh:37:23:application)'],
    msg: 'showToken',
    who: 'Bob'
    });
  
  const txn3 = await (ctc.sendrecv({
    args: [v236, v237, v238, v241, v242, v251, v254],
    evt_cnt: 0,
    funcNum: 2,
    lct: v255,
    onlyIf: true,
    out_tys: [],
    pay: [stdlib.checkedBigNumberify('./index.rsh:45:5:decimal', stdlib.UInt_max, 0), []],
    sim_p: (async (txn3) => {
      const sim_r = { txns: [], mapRefs: [], maps: [] };
      
      const {data: [], secs: v262, time: v261, didSend: v70, from: v260 } = txn3;
      
      sim_r.txns.push({
        amt: stdlib.checkedBigNumberify('./index.rsh:45:5:decimal', stdlib.UInt_max, 0),
        kind: 'to',
        tok: undefined
        });
      const v264 = stdlib.addressEq(v254, v260);
      stdlib.assert(v264, {
        at: './index.rsh:45:5:dot',
        fs: [],
        msg: 'sender correct',
        who: 'Bob'
        });
      const v265 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:41:14:decimal', stdlib.UInt_max, 2), v242);
      sim_r.txns.push({
        amt: v265,
        kind: 'from',
        to: v254,
        tok: v251
        });
      
      sim_r.isHalt = false;
      
      return sim_r;
      }),
    soloSend: true,
    timeoutAt: undefined,
    tys: [ctc6, ctc0, ctc1, ctc3, ctc3, ctc4, ctc6],
    waitIfNotPresent: false
    }));
  const {data: [], secs: v262, time: v261, didSend: v70, from: v260 } = txn3;
  ;
  const v264 = stdlib.addressEq(v254, v260);
  stdlib.assert(v264, {
    at: './index.rsh:45:5:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Bob'
    });
  const v265 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:41:14:decimal', stdlib.UInt_max, 2), v242);
  ;
  stdlib.protect(ctc5, await interact.didTransfer(true, v242), {
    at: './index.rsh:1:39:application',
    fs: ['at ./index.rsh:1:21:application call to [unknown function] (defined at: ./index.rsh:1:25:function exp)', 'at ./index.rsh:42:29:application call to "liftedInteract" (defined at: ./index.rsh:42:29:application)', 'at ./index.rsh:46:14:application call to "doTransfer1" (defined at: ./index.rsh:40:35:function exp)'],
    msg: 'didTransfer',
    who: 'Bob'
    });
  
  const txn4 = await (ctc.recv({
    didSend: false,
    evt_cnt: 0,
    funcNum: 3,
    out_tys: [],
    timeoutAt: undefined,
    waitIfNotPresent: false
    }));
  const {data: [], secs: v274, time: v273, didSend: v86, from: v272 } = txn4;
  ;
  const v276 = stdlib.addressEq(v236, v272);
  stdlib.assert(v276, {
    at: './index.rsh:48:5:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Bob'
    });
  ;
  const txn5 = await (ctc.recv({
    didSend: false,
    evt_cnt: 0,
    funcNum: 4,
    out_tys: [],
    timeoutAt: undefined,
    waitIfNotPresent: false
    }));
  const {data: [], secs: v287, time: v286, didSend: v106, from: v285 } = txn5;
  ;
  ;
  const v292 = stdlib.addressEq(v236, v285);
  stdlib.assert(v292, {
    at: './index.rsh:51:5:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Bob'
    });
  const txn6 = await (ctc.sendrecv({
    args: [v236, v237, v238, v241, v251, v254, v265],
    evt_cnt: 0,
    funcNum: 5,
    lct: v286,
    onlyIf: true,
    out_tys: [],
    pay: [stdlib.checkedBigNumberify('./index.rsh:53:5:dot', stdlib.UInt_max, 0), [[v265, v251]]],
    sim_p: (async (txn6) => {
      const sim_r = { txns: [], mapRefs: [], maps: [] };
      
      const {data: [], secs: v296, time: v295, didSend: v116, from: v294 } = txn6;
      
      sim_r.txns.push({
        amt: stdlib.checkedBigNumberify('./index.rsh:53:5:dot', stdlib.UInt_max, 0),
        kind: 'to',
        tok: undefined
        });
      sim_r.txns.push({
        amt: v265,
        kind: 'to',
        tok: v251
        });
      const v301 = stdlib.addressEq(v254, v294);
      stdlib.assert(v301, {
        at: './index.rsh:53:5:dot',
        fs: [],
        msg: 'sender correct',
        who: 'Bob'
        });
      stdlib.simTokenBurn(sim_r, v251, v241);
      stdlib.simTokenDestroy(sim_r, v251);
      const v312 = '                                                                                                ';
      const v313 = '                                ';
      const v314 = stdlib.simTokenNew(sim_r, v237, v238, v312, v313, stdlib.UInt_max, undefined);
      const v315 = await txn6.getOutput('internal', 'v314', ctc4, v314);
      
      sim_r.isHalt = false;
      
      return sim_r;
      }),
    soloSend: true,
    timeoutAt: undefined,
    tys: [ctc6, ctc0, ctc1, ctc3, ctc4, ctc6, ctc3],
    waitIfNotPresent: false
    }));
  const {data: [], secs: v296, time: v295, didSend: v116, from: v294 } = txn6;
  ;
  ;
  const v301 = stdlib.addressEq(v254, v294);
  stdlib.assert(v301, {
    at: './index.rsh:53:5:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Bob'
    });
  undefined;
  undefined;
  const v312 = '                                                                                                ';
  const v313 = '                                ';
  const v314 = undefined;
  const v315 = await txn6.getOutput('internal', 'v314', ctc4, v314);
  const v319 = {
    name: v237,
    symbol: v238
    };
  stdlib.protect(ctc5, await interact.showToken(v315, v319), {
    at: './index.rsh:1:39:application',
    fs: ['at ./index.rsh:1:21:application call to [unknown function] (defined at: ./index.rsh:1:25:function exp)', 'at ./index.rsh:60:23:application call to "liftedInteract" (defined at: ./index.rsh:60:23:application)'],
    msg: 'showToken',
    who: 'Bob'
    });
  
  const txn7 = await (ctc.sendrecv({
    args: [v236, v254, v265, v315],
    evt_cnt: 0,
    funcNum: 6,
    lct: v295,
    onlyIf: true,
    out_tys: [],
    pay: [stdlib.checkedBigNumberify('./index.rsh:63:5:decimal', stdlib.UInt_max, 0), []],
    sim_p: (async (txn7) => {
      const sim_r = { txns: [], mapRefs: [], maps: [] };
      
      const {data: [], secs: v322, time: v321, didSend: v146, from: v320 } = txn7;
      
      sim_r.txns.push({
        amt: stdlib.checkedBigNumberify('./index.rsh:63:5:decimal', stdlib.UInt_max, 0),
        kind: 'to',
        tok: undefined
        });
      const v324 = stdlib.addressEq(v254, v320);
      stdlib.assert(v324, {
        at: './index.rsh:63:5:dot',
        fs: [],
        msg: 'sender correct',
        who: 'Bob'
        });
      const v329 = stdlib.sub(stdlib.UInt_max, v265);
      sim_r.txns.push({
        amt: v265,
        kind: 'from',
        to: v254,
        tok: v315
        });
      
      sim_r.isHalt = false;
      
      return sim_r;
      }),
    soloSend: true,
    timeoutAt: undefined,
    tys: [ctc6, ctc6, ctc3, ctc4],
    waitIfNotPresent: false
    }));
  const {data: [], secs: v322, time: v321, didSend: v146, from: v320 } = txn7;
  ;
  const v324 = stdlib.addressEq(v254, v320);
  stdlib.assert(v324, {
    at: './index.rsh:63:5:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Bob'
    });
  const v329 = stdlib.sub(stdlib.UInt_max, v265);
  ;
  stdlib.protect(ctc5, await interact.didTransfer(true, v242), {
    at: './index.rsh:1:39:application',
    fs: ['at ./index.rsh:1:21:application call to [unknown function] (defined at: ./index.rsh:1:25:function exp)', 'at ./index.rsh:42:29:application call to "liftedInteract" (defined at: ./index.rsh:42:29:application)', 'at ./index.rsh:64:14:application call to "doTransfer1" (defined at: ./index.rsh:40:35:function exp)'],
    msg: 'didTransfer',
    who: 'Bob'
    });
  
  const txn8 = await (ctc.recv({
    didSend: false,
    evt_cnt: 0,
    funcNum: 7,
    out_tys: [],
    timeoutAt: undefined,
    waitIfNotPresent: false
    }));
  const {data: [], secs: v334, time: v333, didSend: v162, from: v332 } = txn8;
  ;
  const v336 = stdlib.addressEq(v236, v332);
  stdlib.assert(v336, {
    at: './index.rsh:66:5:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Bob'
    });
  const v341 = stdlib.sub(v329, v265);
  ;
  const v348 = stdlib.sub(v341, v341);
  undefined;
  const txn9 = await (ctc.recv({
    didSend: false,
    evt_cnt: 0,
    funcNum: 8,
    out_tys: [],
    timeoutAt: undefined,
    waitIfNotPresent: false
    }));
  const {data: [], secs: v354, time: v353, didSend: v189, from: v352 } = txn9;
  ;
  const v358 = stdlib.add(v348, v265);
  ;
  const v359 = stdlib.addressEq(v236, v352);
  stdlib.assert(v359, {
    at: './index.rsh:70:5:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Bob'
    });
  const txn10 = await (ctc.sendrecv({
    args: [v254, v265, v315, v358],
    evt_cnt: 0,
    funcNum: 9,
    lct: v353,
    onlyIf: true,
    out_tys: [],
    pay: [stdlib.checkedBigNumberify('./index.rsh:72:5:dot', stdlib.UInt_max, 0), [[v265, v315]]],
    sim_p: (async (txn10) => {
      const sim_r = { txns: [], mapRefs: [], maps: [] };
      
      const {data: [], secs: v363, time: v362, didSend: v199, from: v361 } = txn10;
      
      sim_r.txns.push({
        amt: stdlib.checkedBigNumberify('./index.rsh:72:5:dot', stdlib.UInt_max, 0),
        kind: 'to',
        tok: undefined
        });
      const v367 = stdlib.add(v358, v265);
      sim_r.txns.push({
        amt: v265,
        kind: 'to',
        tok: v315
        });
      const v368 = stdlib.addressEq(v254, v361);
      stdlib.assert(v368, {
        at: './index.rsh:72:5:dot',
        fs: [],
        msg: 'sender correct',
        who: 'Bob'
        });
      stdlib.simTokenBurn(sim_r, v315, v367);
      stdlib.simTokenDestroy(sim_r, v315);
      sim_r.txns.push({
        kind: 'halt',
        tok: undefined
        })
      sim_r.isHalt = true;
      
      return sim_r;
      }),
    soloSend: true,
    timeoutAt: undefined,
    tys: [ctc6, ctc3, ctc4, ctc3],
    waitIfNotPresent: false
    }));
  const {data: [], secs: v363, time: v362, didSend: v199, from: v361 } = txn10;
  ;
  const v367 = stdlib.add(v358, v265);
  ;
  const v368 = stdlib.addressEq(v254, v361);
  stdlib.assert(v368, {
    at: './index.rsh:72:5:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Bob'
    });
  undefined;
  undefined;
  return;
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  };
const _ALGO = {
  ABI: {
    impure: [],
    pure: [],
    sigs: []
    },
  appApproval: ``,
  appClear: ``,
  extraPages: -1,
  mapDataKeys: 0,
  mapDataSize: 0,
  stateKeys: 2,
  stateSize: 128,
  unsupported: [],
  version: 9,
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
                "components": [
                  {
                    "internalType": "bytes32",
                    "name": "elem0",
                    "type": "bytes32"
                  }
                ],
                "internalType": "struct T0",
                "name": "v237",
                "type": "tuple"
              },
              {
                "components": [
                  {
                    "internalType": "bytes8",
                    "name": "elem0",
                    "type": "bytes8"
                  }
                ],
                "internalType": "struct T1",
                "name": "v238",
                "type": "tuple"
              },
              {
                "components": [
                  {
                    "internalType": "bytes32",
                    "name": "elem0",
                    "type": "bytes32"
                  },
                  {
                    "internalType": "bytes32",
                    "name": "elem1",
                    "type": "bytes32"
                  },
                  {
                    "internalType": "bytes32",
                    "name": "elem2",
                    "type": "bytes32"
                  }
                ],
                "internalType": "struct T3",
                "name": "v239",
                "type": "tuple"
              },
              {
                "components": [
                  {
                    "internalType": "bytes32",
                    "name": "elem0",
                    "type": "bytes32"
                  }
                ],
                "internalType": "struct T0",
                "name": "v240",
                "type": "tuple"
              },
              {
                "internalType": "uint256",
                "name": "v241",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v242",
                "type": "uint256"
              }
            ],
            "internalType": "struct T4",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "internalType": "struct T5",
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
                "components": [
                  {
                    "internalType": "bytes32",
                    "name": "elem0",
                    "type": "bytes32"
                  }
                ],
                "internalType": "struct T0",
                "name": "v237",
                "type": "tuple"
              },
              {
                "components": [
                  {
                    "internalType": "bytes8",
                    "name": "elem0",
                    "type": "bytes8"
                  }
                ],
                "internalType": "struct T1",
                "name": "v238",
                "type": "tuple"
              },
              {
                "components": [
                  {
                    "internalType": "bytes32",
                    "name": "elem0",
                    "type": "bytes32"
                  },
                  {
                    "internalType": "bytes32",
                    "name": "elem1",
                    "type": "bytes32"
                  },
                  {
                    "internalType": "bytes32",
                    "name": "elem2",
                    "type": "bytes32"
                  }
                ],
                "internalType": "struct T3",
                "name": "v239",
                "type": "tuple"
              },
              {
                "components": [
                  {
                    "internalType": "bytes32",
                    "name": "elem0",
                    "type": "bytes32"
                  }
                ],
                "internalType": "struct T0",
                "name": "v240",
                "type": "tuple"
              },
              {
                "internalType": "uint256",
                "name": "v241",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v242",
                "type": "uint256"
              }
            ],
            "internalType": "struct T4",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct T5",
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
        "internalType": "struct T8",
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
        "internalType": "struct T8",
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
    "name": "_reach_e3",
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
        "internalType": "struct T8",
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
        "internalType": "struct T8",
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
        "internalType": "struct T8",
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
        "internalType": "struct T8",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "_reach_e9",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": false,
        "internalType": "address payable",
        "name": "v0",
        "type": "address"
      }
    ],
    "name": "_reach_oe_v250",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": false,
        "internalType": "address payable",
        "name": "v0",
        "type": "address"
      }
    ],
    "name": "_reach_oe_v314",
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
        "internalType": "struct T8",
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
        "internalType": "struct T8",
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
    "name": "_reach_m3",
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
        "internalType": "struct T8",
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
        "internalType": "struct T8",
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
        "internalType": "struct T8",
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
        "internalType": "struct T8",
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
  Bytecode: `0x60806040526040516200492b3803806200492b83398101604081905262000026916200058e565b6000808055436003556040805160608082018352838252602080830185905282840194909452825185518152858501518051518287015280860151516001600160c01b031916828601528085015180518385015295860151608080840191909152959094015160a080830191909152918401515160c08201529383015160e085015290910151610100830152907f615a93736030355775bf049b6b8f7fc85968358ac6fdb653c3afb52c27606a78906101200160405180910390a1620000ef341560076200035b565b602082015160a001516200010590600462000670565b8082526020830151608001516200012091111560086200035b565b6200012e600160096200035b565b602080830151515160405162000148920190815260200190565b60408051601f1981840301815290829052602084810151810151516001600160c01b031916908301529060280160408051808303601f1901815282825260208681015183015180518183015191850151928601529284019290925260608301919091529060800160408051601f1981840301815282825260208781015160600151519084015291016040516020818303038152906040528560200151608001516012604051620001f89062000385565b6200020996959493929190620006ee565b604051809103906000f08015801562000226573d6000803e3d6000fd5b506001600160a01b031660208281018290526040808401839052519182527f41ec50d31b6162f73174da158b41ddca57a1853a852b3bfe8829b84c6d151540910160405180910390a16040805160c080820183526000808352835160208082018652828252808501918252855180820187528381528587019081526060808701858152608080890187815260a0808b0189815233808d528f89018051518b5280518a01518952805186015187525183015184528e8e01516001600160a01b03908116835260019b8c905543909b558d51808a0191909152985151898e01529551516001600160c01b03191694880194909452915190860152519084015251909216818401528451808203909301835260e0019093528051919262000351926002929091019062000393565b5050505062000796565b81620003815760405163100960cb60e01b81526004810182905260240160405180910390fd5b5050565b610f7580620039b683390190565b828054620003a19062000759565b90600052602060002090601f016020900481019282620003c5576000855562000410565b82601f10620003e057805160ff191683800117855562000410565b8280016001018555821562000410579182015b8281111562000410578251825591602001919060010190620003f3565b506200041e92915062000422565b5090565b5b808211156200041e576000815560010162000423565b604080519081016001600160401b03811182821017156200046a57634e487b7160e01b600052604160045260246000fd5b60405290565b60405160c081016001600160401b03811182821017156200046a57634e487b7160e01b600052604160045260246000fd5b604051602081016001600160401b03811182821017156200046a57634e487b7160e01b600052604160045260246000fd5b600060208284031215620004e557600080fd5b604051602081016001600160401b03811182821017156200051657634e487b7160e01b600052604160045260246000fd5b6040529151825250919050565b6000606082840312156200053657600080fd5b604051606081016001600160401b03811182821017156200056757634e487b7160e01b600052604160045260246000fd5b80604052508091508251815260208301516020820152604083015160408201525092915050565b6000818303610120811215620005a357600080fd5b620005ad62000439565b8351815261010080601f1984011215620005c657600080fd5b620005d062000470565b620005df8760208801620004d2565b81526020603f1985011215620005f457600080fd5b620005fe620004a1565b60408701519094506001600160c01b0319811681146200061d57600080fd5b84526020810184905262000635876060880162000523565b6040820152620006498760c08801620004d2565b606082015260e0860151608082015294015160a08501526020810193909352509092915050565b60008160001904831182151516156200069957634e487b7160e01b600052601160045260246000fd5b500290565b6000815180845260005b81811015620006c657602081850181015186830182015201620006a8565b81811115620006d9576000602083870101525b50601f01601f19169290920160200192915050565b60c0815260006200070360c08301896200069e565b82810360208401526200071781896200069e565b905082810360408401526200072d81886200069e565b905082810360608401526200074381876200069e565b6080840195909552505060a00152949350505050565b600181811c908216806200076e57607f821691505b602082108114156200079057634e487b7160e01b600052602260045260246000fd5b50919050565b61321080620007a66000396000f3fe608060405260043610620000b35760003560e01c8063a7661d541162000069578063a7661d541462000155578063ab53f2c6146200016c578063bf2c5b241462000194578063c798003714620001ab578063de73699814620001c2578063e533a29d14620001d957005b80631e93b0f114620000bd5780632c10a15914620000e257806373b4522c14620000f95780637eea518c14620001105780638323075714620001275780638e314769146200013e57005b36620000bb57005b005b348015620000ca57600080fd5b506003545b6040519081526020015b60405180910390f35b620000bb620000f336600462001b3e565b620001f0565b620000bb6200010a36600462001b3e565b620003f6565b620000bb6200012136600462001b3e565b620005c4565b3480156200013457600080fd5b50600154620000cf565b620000bb6200014f36600462001b3e565b620007e7565b620000bb6200016636600462001b3e565b62000b7c565b3480156200017957600080fd5b506200018462000d52565b604051620000d992919062001bb4565b620000bb620001a536600462001b3e565b62000df7565b620000bb620001bc36600462001b3e565b62001021565b620000bb620001d336600462001b3e565b62001227565b620000bb620001ea36600462001b3e565b620013d7565b62000202600160005414600b620015e9565b6200021f813515806200021757506001548235145b600c620015e9565b600080805560028054620002339062001bcf565b80601f0160208091040260200160405190810160405280929190818152602001828054620002619062001bcf565b8015620002b25780601f106200028657610100808354040283529160200191620002b2565b820191906000526020600020905b8154815290600101906020018083116200029457829003601f168201915b5050505050806020019051810190620002cc919062001d27565b90507f79ca1a789d797004bc78dff9632d64e202e102f2d008dcc20c5a645ef7d4a7d182604051620002ff919062001de1565b60405180910390a1620003153415600a620015e9565b6200031f620019fd565b81516001600160a01b039081168083526020808501518185019081526040808701518187019081526060808901518189019081526080808b0151818b0190815260a0808d01518b16818d019081523360c0808f0191825260026000554360015589519b8c019c909c52985151978a01979097529451516001600160c01b03191693880193909352905190860152519084015251841692820192909252905190911660e0820152610100015b60405160208183030381529060405260029080519060200190620003f092919062001a50565b50505050565b620004086003600054146013620015e9565b62000425813515806200041d57506001548235145b6014620015e9565b600080805560028054620004399062001bcf565b80601f0160208091040260200160405190810160405280929190818152602001828054620004679062001bcf565b8015620004b85780601f106200048c57610100808354040283529160200191620004b8565b820191906000526020600020905b8154815290600101906020018083116200049a57829003601f168201915b5050505050806020019051810190620004d2919062001e08565b90507f25e9400ad0fddc8c71fc4eb2845b4d0fc13ca66f2a17e1b7e0f544d275234c098260405162000505919062001de1565b60405180910390a16200051b34156011620015e9565b805162000535906001600160a01b031633146012620015e9565b6200054e816080015182600001518360c0015162001610565b62000558620019fd565b81516001600160a01b03908116825260208084015181840152604080850151818501526060808601519085015260808086015184169085015260a0808601519093169284019290925260c080850151908401526004600055436001559051620003ca9183910162001e9e565b620005d6600260005414600f620015e9565b620005f381351580620005eb57506001548235145b6010620015e9565b600080805560028054620006079062001bcf565b80601f0160208091040260200160405190810160405280929190818152602001828054620006359062001bcf565b8015620006865780601f106200065a5761010080835404028352916020019162000686565b820191906000526020600020905b8154815290600101906020018083116200066857829003601f168201915b5050505050806020019051810190620006a0919062001f03565b9050620006b96040518060200160405280600081525090565b7f82e152e8b1d7e41adffbddbd5b2fe2e130356df9b7ab7d06526a80d7888af3e183604051620006ea919062001de1565b60405180910390a1620007003415600d620015e9565b60c08201516200071d906001600160a01b03163314600e620015e9565b60808201516200072f90600262001fac565b80825260a083015160c0840151620007479262001610565b62000751620019fd565b82516001600160a01b03908116825260208085015181840152604080860151818501526060808701519085015260a0808701518416608086015260c080880151909416908501528451928401929092526003600055436001559051620007ba9183910162001e9e565b60405160208183030381529060405260029080519060200190620007e092919062001a50565b5050505050565b620007f9600560005414601d620015e9565b62000816813515806200080e57506001548235145b601e620015e9565b6000808055600280546200082a9062001bcf565b80601f0160208091040260200160405190810160405280929190818152602001828054620008589062001bcf565b8015620008a95780601f106200087d57610100808354040283529160200191620008a9565b820191906000526020600020905b8154815290600101906020018083116200088b57829003601f168201915b5050505050806020019051810190620008c3919062001e08565b90506200090c6040805160e08101825260006080820181815260a0830182905260c083018290528252825160208082018552828252830152918101829052606081019190915290565b7f9cdba579557d44a893ea7929682d6795d48dd5c40dc981d852842d4b18914de8836040516200093d919062001de1565b60405180910390a1620009533415601a620015e9565b620009746200096c3384608001518560c001516200162c565b601b620015e9565b60a082015162000991906001600160a01b03163314601c620015e9565b620009a58260800151836060015162001646565b620009b482608001516200165c565b80516000908190528151602090810182905282516040908101839052818401519290925283810151518251918201520160408051808303601f1901815282825290840151516001600160c01b03191660208301529060280160408051808303601f190181528282528451805160208083015192850151908601919091529284015260608301919091529060800160408051601f1981840301815282825260208681015151908401529101604051602081830303815290604052600019601260405162000a809062001adf565b62000a919695949392919062001fce565b604051809103906000f08015801562000aae573d6000803e3d6000fd5b506001600160a01b0316604082810182905260608301829052519081527f8ca9afbab83c218d5d744f8a616d542bed0d553465a1a8bcf187bf24d43bb6209060200160405180910390a160408051608080820183526000808352602080840182815284860183815260608087018581528a516001600160a01b03908116808a5260a0808e01518316875260c08e015186528c850151831684526006909855436001558a51968701529351841698850198909852905190830152945190941691840191909152909101620007ba565b62000b8e6004600054146018620015e9565b62000bab8135158062000ba357506001548235145b6019620015e9565b60008080556002805462000bbf9062001bcf565b80601f016020809104026020016040519081016040528092919081815260200182805462000bed9062001bcf565b801562000c3e5780601f1062000c125761010080835404028352916020019162000c3e565b820191906000526020600020905b81548152906001019060200180831162000c2057829003601f168201915b505050505080602001905181019062000c58919062001e08565b90507fbe072b3e7ff68f92e7d9d05168a4666cd1ba2609e77c14d9feaf0d14991875d18260405162000c8b919062001de1565b60405180910390a162000ca134156015620015e9565b62000cc262000cba3383608001518460c001516200162c565b6016620015e9565b805162000cdc906001600160a01b031633146017620015e9565b62000ce6620019fd565b81516001600160a01b03908116825260208084015181840152604080850151818501526060808601519085015260808086015184169085015260a0808601519093169284019290925260c080850151908401526005600055436001559051620003ca9183910162001e9e565b60006060600054600280805462000d699062001bcf565b80601f016020809104026020016040519081016040528092919081815260200182805462000d979062001bcf565b801562000de85780601f1062000dbc5761010080835404028352916020019162000de8565b820191906000526020600020905b81548152906001019060200180831162000dca57829003601f168201915b50505050509050915091509091565b62000e096007600054146025620015e9565b62000e268135158062000e1e57506001548235145b6026620015e9565b60008080556002805462000e3a9062001bcf565b80601f016020809104026020016040519081016040528092919081815260200182805462000e689062001bcf565b801562000eb95780601f1062000e8d5761010080835404028352916020019162000eb9565b820191906000526020600020905b81548152906001019060200180831162000e9b57829003601f168201915b505050505080602001905181019062000ed39190620020d2565b905062000ef3604051806040016040528060008152602001600081525090565b7fba16100ad25f3c6798bc3b7e9ca316fb231388e6fa4444c0f477e2a4336514e08360405162000f24919062001de1565b60405180910390a162000f3a34156023620015e9565b815162000f54906001600160a01b031633146024620015e9565b8160400151826080015162000f6a9190620020f1565b815260608201518251604084015162000f8592919062001610565b805162000f939080620020f1565b60208201526060820151815162000fab919062001646565b6040805160a081018252600080825260208083018281528385018381526060808601858152608087018681528a516001600160a01b0390811689528b87015181169095528a8901519093529089015190921690915285820151905260089091554360015591519091620007ba918391016200210b565b620010336006600054146021620015e9565b62001050813515806200104857506001548235145b6022620015e9565b600080805560028054620010649062001bcf565b80601f0160208091040260200160405190810160405280929190818152602001828054620010929062001bcf565b8015620010e35780601f10620010b757610100808354040283529160200191620010e3565b820191906000526020600020905b815481529060010190602001808311620010c557829003601f168201915b5050505050806020019051810190620010fd919062002150565b9050620011166040518060200160405280600081525090565b7f4df267dd1a05b613b05cdeee7d7e028d3842cff6f4e5a9d9dde2cdaf415627598360405162001147919062001de1565b60405180910390a16200115d3415601f620015e9565b62001182336001600160a01b031683602001516001600160a01b0316146020620015e9565b60408201516200119590600019620020f1565b8152606082015160208301516040840151620011b392919062001610565b6040805160a081018252600080825260208083018281528385018381526060808601858152608087018681528a516001600160a01b0390811689528b87015181169095528a890151909352908901519092169091528551905260079091554360015591519091620007ba918391016200210b565b62001239600960005414602f620015e9565b62001256813515806200124e57506001548235145b6030620015e9565b6000808055600280546200126a9062001bcf565b80601f0160208091040260200160405190810160405280929190818152602001828054620012989062001bcf565b8015620012e95780601f10620012bd57610100808354040283529160200191620012e9565b820191906000526020600020905b815481529060010190602001808311620012cb57829003601f168201915b5050505050806020019051810190620013039190620021b1565b90507fb764c356a899e639c4043e82fb6274894baac6d84c74f3b3ae78d8f4b22b00038260405162001336919062001de1565b60405180910390a16200134c3415602c620015e9565b6200136d6200136533836040015184602001516200162c565b602d620015e9565b805162001387906001600160a01b03163314602e620015e9565b620013ac816040015182602001518360600151620013a691906200220c565b62001646565b620013bb81604001516200165c565b60008080556001819055620013d39060029062001aed565b5050565b620013e9600860005414602a620015e9565b6200140681351580620013fe57506001548235145b602b620015e9565b6000808055600280546200141a9062001bcf565b80601f0160208091040260200160405190810160405280929190818152602001828054620014489062001bcf565b8015620014995780601f106200146d5761010080835404028352916020019162001499565b820191906000526020600020905b8154815290600101906020018083116200147b57829003601f168201915b5050505050806020019051810190620014b39190620020d2565b9050620014cc6040518060200160405280600081525090565b7fb9845e39b4c5715a32bc04872bfe1723e638b66042817fdde0a44e5b0466b6dc83604051620014fd919062001de1565b60405180910390a16200151334156027620015e9565b816040015182608001516200152991906200220c565b815260608201516040830151620015509162001548913391906200162c565b6028620015e9565b81516200156a906001600160a01b031633146029620015e9565b60408051608080820183526000808352602080840182815284860183815260608087018581528a8501516001600160a01b03908116808a528c8b015186528c840151821685528b518352600990975543600155895195860196909652925197840197909752519092169481019490945251908301529060a001620007ba565b81620013d35760405163100960cb60e01b8152600481018290526024015b60405180910390fd5b6200161d83838362001674565b6200162757600080fd5b505050565b60006200163c838530856200174d565b90505b9392505050565b6200165282826200182f565b620013d357600080fd5b620016678162001913565b6200167157600080fd5b50565b604080516001600160a01b038481166024830152604480830185905283518084039091018152606490920183526020820180516001600160e01b031663a9059cbb60e01b179052915160009283928392918816918391620016d59162002227565b60006040518083038185875af1925050503d806000811462001714576040519150601f19603f3d011682016040523d82523d6000602084013e62001719565b606091505b50915091506200172c82826002620019be565b508080602001905181019062001743919062002245565b9695505050505050565b604080516001600160a01b0385811660248301528481166044830152606480830185905283518084039091018152608490920183526020820180516001600160e01b03166323b872dd60e01b179052915160009283928392918916918391620017b69162002227565b60006040518083038185875af1925050503d8060008114620017f5576040519150601f19603f3d011682016040523d82523d6000602084013e620017fa565b606091505b50915091506200180d82826001620019be565b508080602001905181019062001824919062002245565b979650505050505050565b6000806000846001600160a01b031660006342966c6860e01b866040516024016200185c91815260200190565b60408051601f198184030181529181526020820180516001600160e01b03166001600160e01b03199094169390931790925290516200189c919062002227565b60006040518083038185875af1925050503d8060008114620018db576040519150601f19603f3d011682016040523d82523d6000602084013e620018e0565b606091505b5091509150620018f382826003620019be565b50808060200190518101906200190a919062002245565b95945050505050565b60408051600481526024810182526020810180516001600160e01b031663083197ef60e41b1790529051600091829182916001600160a01b0386169183916200195c9162002227565b60006040518083038185875af1925050503d80600081146200199b576040519150601f19603f3d011682016040523d82523d6000602084013e620019a0565b606091505b5091509150620019b382826004620019be565b506001949350505050565b60608315620019cf5750816200163f565b825115620019e05782518084602001fd5b60405163100960cb60e01b81526004810183905260240162001607565b6040805160e0810182526000808252825160208082018552828252808401919091528351908101845281815292820192909252606081018290526080810182905260a0810182905260c081019190915290565b82805462001a5e9062001bcf565b90600052602060002090601f01602090048101928262001a82576000855562001acd565b82601f1062001a9d57805160ff191683800117855562001acd565b8280016001018555821562001acd579182015b8281111562001acd57825182559160200191906001019062001ab0565b5062001adb92915062001b27565b5090565b610f75806200226683390190565b50805462001afb9062001bcf565b6000825580601f1062001b0c575050565b601f0160209004906000526020600020908101906200167191905b5b8082111562001adb576000815560010162001b28565b60006040828403121562001b5157600080fd5b50919050565b60005b8381101562001b7457818101518382015260200162001b5a565b83811115620003f05750506000910152565b6000815180845262001ba081602086016020860162001b57565b601f01601f19169290920160200192915050565b8281526040602082015260006200163c604083018462001b86565b600181811c9082168062001be457607f821691505b6020821081141562001b5157634e487b7160e01b600052602260045260246000fd5b6040516020810167ffffffffffffffff8111828210171562001c3857634e487b7160e01b600052604160045260246000fd5b60405290565b60405160e0810167ffffffffffffffff8111828210171562001c3857634e487b7160e01b600052604160045260246000fd5b6040516080810167ffffffffffffffff8111828210171562001c3857634e487b7160e01b600052604160045260246000fd5b80516001600160a01b038116811462001cba57600080fd5b919050565b60006020828403121562001cd257600080fd5b62001cdc62001c06565b9151825250919050565b60006020828403121562001cf957600080fd5b62001d0362001c06565b82519091506001600160c01b03198116811462001d1f57600080fd5b815292915050565b600060c0828403121562001d3a57600080fd5b60405160c0810181811067ffffffffffffffff8211171562001d6c57634e487b7160e01b600052604160045260246000fd5b60405262001d7a8362001ca2565b815262001d8b846020850162001cbf565b602082015262001d9f846040850162001ce6565b6040820152606083015160608201526080830151608082015262001dc660a0840162001ca2565b60a08201529392505050565b80151581146200167157600080fd5b8135815260408101602083013562001df98162001dd2565b80151560208401525092915050565b600060e0828403121562001e1b57600080fd5b62001e2562001c3e565b62001e308362001ca2565b815262001e41846020850162001cbf565b602082015262001e55846040850162001ce6565b60408201526060830151606082015262001e726080840162001ca2565b608082015262001e8560a0840162001ca2565b60a082015260c083015160c08201528091505092915050565b81516001600160a01b0390811682526020808401515190830152604080840151516001600160c01b031916908301526060808401519083015260808084015182169083015260a0838101519091169082015260c0918201519181019190915260e00190565b600060e0828403121562001f1657600080fd5b62001f2062001c3e565b62001f2b8362001ca2565b815262001f3c846020850162001cbf565b602082015262001f50846040850162001ce6565b6040820152606083015160608201526080830151608082015262001f7760a0840162001ca2565b60a082015262001f8a60c0840162001ca2565b60c08201529392505050565b634e487b7160e01b600052601160045260246000fd5b600081600019048311821515161562001fc95762001fc962001f96565b500290565b60c08152600062001fe360c083018962001b86565b828103602084015262001ff7818962001b86565b905082810360408401526200200d818862001b86565b9050828103606084015262002023818762001b86565b6080840195909552505060a00152949350505050565b600060a082840312156200204c57600080fd5b60405160a0810181811067ffffffffffffffff821117156200207e57634e487b7160e01b600052604160045260246000fd5b6040529050806200208f8362001ca2565b81526200209f6020840162001ca2565b602082015260408301516040820152620020bc6060840162001ca2565b6060820152608083015160808201525092915050565b600060a08284031215620020e557600080fd5b6200163f838362002039565b60008282101562002106576200210662001f96565b500390565b81516001600160a01b03908116825260208084015182169083015260408084015190830152606080840151909116908201526080918201519181019190915260a00190565b6000608082840312156200216357600080fd5b6200216d62001c70565b620021788362001ca2565b8152620021886020840162001ca2565b602082015260408301516040820152620021a56060840162001ca2565b60608201529392505050565b600060808284031215620021c457600080fd5b620021ce62001c70565b620021d98362001ca2565b815260208301516020820152620021f36040840162001ca2565b6040820152606083015160608201528091505092915050565b6000821982111562002222576200222262001f96565b500190565b600082516200223b81846020870162001b57565b9190910192915050565b6000602082840312156200225857600080fd5b81516200163f8162001dd256fe60806040523480156200001157600080fd5b5060405162000f7538038062000f75833981016040819052620000349162000341565b8551869086906200004d906003906020850190620001ce565b50805162000063906004906020840190620001ce565b506200006f9150503390565b600580546001600160a01b0319166001600160a01b039290921691821790556200009a9083620000e6565b8351620000af906006906020870190620001ce565b508251620000c5906007906020860190620001ce565b506008805460ff191660ff9290921691909117905550620004709350505050565b6001600160a01b038216620001415760405162461bcd60e51b815260206004820152601f60248201527f45524332303a206d696e7420746f20746865207a65726f206164647265737300604482015260640160405180910390fd5b80600260008282546200015591906200040c565b90915550506001600160a01b03821660009081526020819052604081208054839290620001849084906200040c565b90915550506040518181526001600160a01b038316906000907fddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef9060200160405180910390a35050565b828054620001dc9062000433565b90600052602060002090601f0160209004810192826200020057600085556200024b565b82601f106200021b57805160ff19168380011785556200024b565b828001600101855582156200024b579182015b828111156200024b5782518255916020019190600101906200022e565b50620002599291506200025d565b5090565b5b808211156200025957600081556001016200025e565b634e487b7160e01b600052604160045260246000fd5b600082601f8301126200029c57600080fd5b81516001600160401b0380821115620002b957620002b962000274565b604051601f8301601f19908116603f01168101908282118183101715620002e457620002e462000274565b816040528381526020925086838588010111156200030157600080fd5b600091505b8382101562000325578582018301518183018401529082019062000306565b83821115620003375760008385830101525b9695505050505050565b60008060008060008060c087890312156200035b57600080fd5b86516001600160401b03808211156200037357600080fd5b620003818a838b016200028a565b975060208901519150808211156200039857600080fd5b620003a68a838b016200028a565b96506040890151915080821115620003bd57600080fd5b620003cb8a838b016200028a565b95506060890151915080821115620003e257600080fd5b50620003f189828a016200028a565b9350506080870151915060a087015190509295509295509295565b600082198211156200042e57634e487b7160e01b600052601160045260246000fd5b500190565b600181811c908216806200044857607f821691505b602082108114156200046a57634e487b7160e01b600052602260045260246000fd5b50919050565b610af580620004806000396000f3fe608060405234801561001057600080fd5b50600436106100cf5760003560e01c806342966c681161008c57806383197ef01161006657806383197ef01461019b57806395d89b41146101a5578063a9059cbb146101ad578063dd62ed3e146101c057600080fd5b806342966c68146101575780635600f04f1461016a57806370a082311461017257600080fd5b806306fdde03146100d4578063095ea7b3146100f257806318160ddd1461011557806323b872dd14610127578063313ce5671461013a578063392f37e91461014f575b600080fd5b6100dc6101f9565b6040516100e991906108fa565b60405180910390f35b61010561010036600461096b565b61028b565b60405190151581526020016100e9565b6002545b6040519081526020016100e9565b610105610135366004610995565b6102a1565b60085460405160ff90911681526020016100e9565b6100dc610357565b6101056101653660046109d1565b610366565b6100dc6103dc565b6101196101803660046109ea565b6001600160a01b031660009081526020819052604090205490565b6101a36103eb565b005b6100dc610492565b6101056101bb36600461096b565b6104a1565b6101196101ce366004610a0c565b6001600160a01b03918216600090815260016020908152604080832093909416825291909152205490565b60606003805461020890610a3f565b80601f016020809104026020016040519081016040528092919081815260200182805461023490610a3f565b80156102815780601f1061025657610100808354040283529160200191610281565b820191906000526020600020905b81548152906001019060200180831161026457829003601f168201915b5050505050905090565b60006102983384846104ae565b50600192915050565b60006102ae8484846105d3565b6001600160a01b0384166000908152600160209081526040808320338452909152902054828110156103385760405162461bcd60e51b815260206004820152602860248201527f45524332303a207472616e7366657220616d6f756e74206578636565647320616044820152676c6c6f77616e636560c01b60648201526084015b60405180910390fd5b61034c85336103478685610a90565b6104ae565b506001949350505050565b60606007805461020890610a3f565b6005546000906001600160a01b0316336001600160a01b0316146103be5760405162461bcd60e51b815260206004820152600f60248201526e36bab9ba1031329031b932b0ba37b960891b604482015260640161032f565b6005546103d4906001600160a01b0316836107ab565b506001919050565b60606006805461020890610a3f565b6005546001600160a01b0316336001600160a01b0316146104405760405162461bcd60e51b815260206004820152600f60248201526e36bab9ba1031329031b932b0ba37b960891b604482015260640161032f565b600254156104845760405162461bcd60e51b81526020600482015260116024820152706d757374206265206e6f20737570706c7960781b604482015260640161032f565b6005546001600160a01b0316ff5b60606004805461020890610a3f565b60006102983384846105d3565b6001600160a01b0383166105105760405162461bcd60e51b8152602060048201526024808201527f45524332303a20617070726f76652066726f6d20746865207a65726f206164646044820152637265737360e01b606482015260840161032f565b6001600160a01b0382166105715760405162461bcd60e51b815260206004820152602260248201527f45524332303a20617070726f766520746f20746865207a65726f206164647265604482015261737360f01b606482015260840161032f565b6001600160a01b0383811660008181526001602090815260408083209487168084529482529182902085905590518481527f8c5be1e5ebec7d5bd14f71427d1e84f3dd0314c0f7b2291e5b200ac8c7c3b92591015b60405180910390a3505050565b6001600160a01b0383166106375760405162461bcd60e51b815260206004820152602560248201527f45524332303a207472616e736665722066726f6d20746865207a65726f206164604482015264647265737360d81b606482015260840161032f565b6001600160a01b0382166106995760405162461bcd60e51b815260206004820152602360248201527f45524332303a207472616e7366657220746f20746865207a65726f206164647260448201526265737360e81b606482015260840161032f565b6001600160a01b038316600090815260208190526040902054818110156107115760405162461bcd60e51b815260206004820152602660248201527f45524332303a207472616e7366657220616d6f756e7420657863656564732062604482015265616c616e636560d01b606482015260840161032f565b61071b8282610a90565b6001600160a01b038086166000908152602081905260408082209390935590851681529081208054849290610751908490610aa7565b92505081905550826001600160a01b0316846001600160a01b03167fddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef8460405161079d91815260200190565b60405180910390a350505050565b6001600160a01b03821661080b5760405162461bcd60e51b815260206004820152602160248201527f45524332303a206275726e2066726f6d20746865207a65726f206164647265736044820152607360f81b606482015260840161032f565b6001600160a01b0382166000908152602081905260409020548181101561087f5760405162461bcd60e51b815260206004820152602260248201527f45524332303a206275726e20616d6f756e7420657863656564732062616c616e604482015261636560f01b606482015260840161032f565b6108898282610a90565b6001600160a01b038416600090815260208190526040812091909155600280548492906108b7908490610a90565b90915550506040518281526000906001600160a01b038516907fddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef906020016105c6565b600060208083528351808285015260005b818110156109275785810183015185820160400152820161090b565b81811115610939576000604083870101525b50601f01601f1916929092016040019392505050565b80356001600160a01b038116811461096657600080fd5b919050565b6000806040838503121561097e57600080fd5b6109878361094f565b946020939093013593505050565b6000806000606084860312156109aa57600080fd5b6109b38461094f565b92506109c16020850161094f565b9150604084013590509250925092565b6000602082840312156109e357600080fd5b5035919050565b6000602082840312156109fc57600080fd5b610a058261094f565b9392505050565b60008060408385031215610a1f57600080fd5b610a288361094f565b9150610a366020840161094f565b90509250929050565b600181811c90821680610a5357607f821691505b60208210811415610a7457634e487b7160e01b600052602260045260246000fd5b50919050565b634e487b7160e01b600052601160045260246000fd5b600082821015610aa257610aa2610a7a565b500390565b60008219821115610aba57610aba610a7a565b50019056fea2646970667358221220d092b83e5587c5cc302dbf0a6786ab038886e8e54dedd1e420f3bfa1915dc64364736f6c634300080a0033a2646970667358221220c8bb1f0bea04e718c96304fe5ae43a9c99750dea2c3e8b6a65192ec4e340df8164736f6c634300080a003360806040523480156200001157600080fd5b5060405162000f7538038062000f75833981016040819052620000349162000341565b8551869086906200004d906003906020850190620001ce565b50805162000063906004906020840190620001ce565b506200006f9150503390565b600580546001600160a01b0319166001600160a01b039290921691821790556200009a9083620000e6565b8351620000af906006906020870190620001ce565b508251620000c5906007906020860190620001ce565b506008805460ff191660ff9290921691909117905550620004709350505050565b6001600160a01b038216620001415760405162461bcd60e51b815260206004820152601f60248201527f45524332303a206d696e7420746f20746865207a65726f206164647265737300604482015260640160405180910390fd5b80600260008282546200015591906200040c565b90915550506001600160a01b03821660009081526020819052604081208054839290620001849084906200040c565b90915550506040518181526001600160a01b038316906000907fddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef9060200160405180910390a35050565b828054620001dc9062000433565b90600052602060002090601f0160209004810192826200020057600085556200024b565b82601f106200021b57805160ff19168380011785556200024b565b828001600101855582156200024b579182015b828111156200024b5782518255916020019190600101906200022e565b50620002599291506200025d565b5090565b5b808211156200025957600081556001016200025e565b634e487b7160e01b600052604160045260246000fd5b600082601f8301126200029c57600080fd5b81516001600160401b0380821115620002b957620002b962000274565b604051601f8301601f19908116603f01168101908282118183101715620002e457620002e462000274565b816040528381526020925086838588010111156200030157600080fd5b600091505b8382101562000325578582018301518183018401529082019062000306565b83821115620003375760008385830101525b9695505050505050565b60008060008060008060c087890312156200035b57600080fd5b86516001600160401b03808211156200037357600080fd5b620003818a838b016200028a565b975060208901519150808211156200039857600080fd5b620003a68a838b016200028a565b96506040890151915080821115620003bd57600080fd5b620003cb8a838b016200028a565b95506060890151915080821115620003e257600080fd5b50620003f189828a016200028a565b9350506080870151915060a087015190509295509295509295565b600082198211156200042e57634e487b7160e01b600052601160045260246000fd5b500190565b600181811c908216806200044857607f821691505b602082108114156200046a57634e487b7160e01b600052602260045260246000fd5b50919050565b610af580620004806000396000f3fe608060405234801561001057600080fd5b50600436106100cf5760003560e01c806342966c681161008c57806383197ef01161006657806383197ef01461019b57806395d89b41146101a5578063a9059cbb146101ad578063dd62ed3e146101c057600080fd5b806342966c68146101575780635600f04f1461016a57806370a082311461017257600080fd5b806306fdde03146100d4578063095ea7b3146100f257806318160ddd1461011557806323b872dd14610127578063313ce5671461013a578063392f37e91461014f575b600080fd5b6100dc6101f9565b6040516100e991906108fa565b60405180910390f35b61010561010036600461096b565b61028b565b60405190151581526020016100e9565b6002545b6040519081526020016100e9565b610105610135366004610995565b6102a1565b60085460405160ff90911681526020016100e9565b6100dc610357565b6101056101653660046109d1565b610366565b6100dc6103dc565b6101196101803660046109ea565b6001600160a01b031660009081526020819052604090205490565b6101a36103eb565b005b6100dc610492565b6101056101bb36600461096b565b6104a1565b6101196101ce366004610a0c565b6001600160a01b03918216600090815260016020908152604080832093909416825291909152205490565b60606003805461020890610a3f565b80601f016020809104026020016040519081016040528092919081815260200182805461023490610a3f565b80156102815780601f1061025657610100808354040283529160200191610281565b820191906000526020600020905b81548152906001019060200180831161026457829003601f168201915b5050505050905090565b60006102983384846104ae565b50600192915050565b60006102ae8484846105d3565b6001600160a01b0384166000908152600160209081526040808320338452909152902054828110156103385760405162461bcd60e51b815260206004820152602860248201527f45524332303a207472616e7366657220616d6f756e74206578636565647320616044820152676c6c6f77616e636560c01b60648201526084015b60405180910390fd5b61034c85336103478685610a90565b6104ae565b506001949350505050565b60606007805461020890610a3f565b6005546000906001600160a01b0316336001600160a01b0316146103be5760405162461bcd60e51b815260206004820152600f60248201526e36bab9ba1031329031b932b0ba37b960891b604482015260640161032f565b6005546103d4906001600160a01b0316836107ab565b506001919050565b60606006805461020890610a3f565b6005546001600160a01b0316336001600160a01b0316146104405760405162461bcd60e51b815260206004820152600f60248201526e36bab9ba1031329031b932b0ba37b960891b604482015260640161032f565b600254156104845760405162461bcd60e51b81526020600482015260116024820152706d757374206265206e6f20737570706c7960781b604482015260640161032f565b6005546001600160a01b0316ff5b60606004805461020890610a3f565b60006102983384846105d3565b6001600160a01b0383166105105760405162461bcd60e51b8152602060048201526024808201527f45524332303a20617070726f76652066726f6d20746865207a65726f206164646044820152637265737360e01b606482015260840161032f565b6001600160a01b0382166105715760405162461bcd60e51b815260206004820152602260248201527f45524332303a20617070726f766520746f20746865207a65726f206164647265604482015261737360f01b606482015260840161032f565b6001600160a01b0383811660008181526001602090815260408083209487168084529482529182902085905590518481527f8c5be1e5ebec7d5bd14f71427d1e84f3dd0314c0f7b2291e5b200ac8c7c3b92591015b60405180910390a3505050565b6001600160a01b0383166106375760405162461bcd60e51b815260206004820152602560248201527f45524332303a207472616e736665722066726f6d20746865207a65726f206164604482015264647265737360d81b606482015260840161032f565b6001600160a01b0382166106995760405162461bcd60e51b815260206004820152602360248201527f45524332303a207472616e7366657220746f20746865207a65726f206164647260448201526265737360e81b606482015260840161032f565b6001600160a01b038316600090815260208190526040902054818110156107115760405162461bcd60e51b815260206004820152602660248201527f45524332303a207472616e7366657220616d6f756e7420657863656564732062604482015265616c616e636560d01b606482015260840161032f565b61071b8282610a90565b6001600160a01b038086166000908152602081905260408082209390935590851681529081208054849290610751908490610aa7565b92505081905550826001600160a01b0316846001600160a01b03167fddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef8460405161079d91815260200190565b60405180910390a350505050565b6001600160a01b03821661080b5760405162461bcd60e51b815260206004820152602160248201527f45524332303a206275726e2066726f6d20746865207a65726f206164647265736044820152607360f81b606482015260840161032f565b6001600160a01b0382166000908152602081905260409020548181101561087f5760405162461bcd60e51b815260206004820152602260248201527f45524332303a206275726e20616d6f756e7420657863656564732062616c616e604482015261636560f01b606482015260840161032f565b6108898282610a90565b6001600160a01b038416600090815260208190526040812091909155600280548492906108b7908490610a90565b90915550506040518281526000906001600160a01b038516907fddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef906020016105c6565b600060208083528351808285015260005b818110156109275785810183015185820160400152820161090b565b81811115610939576000604083870101525b50601f01601f1916929092016040019392505050565b80356001600160a01b038116811461096657600080fd5b919050565b6000806040838503121561097e57600080fd5b6109878361094f565b946020939093013593505050565b6000806000606084860312156109aa57600080fd5b6109b38461094f565b92506109c16020850161094f565b9150604084013590509250925092565b6000602082840312156109e357600080fd5b5035919050565b6000602082840312156109fc57600080fd5b610a058261094f565b9392505050565b60008060408385031215610a1f57600080fd5b610a288361094f565b9150610a366020840161094f565b90509250929050565b600181811c90821680610a5357607f821691505b60208210811415610a7457634e487b7160e01b600052602260045260246000fd5b50919050565b634e487b7160e01b600052601160045260246000fd5b600082821015610aa257610aa2610a7a565b500390565b60008219821115610aba57610aba610a7a565b50019056fea2646970667358221220d092b83e5587c5cc302dbf0a6786ab038886e8e54dedd1e420f3bfa1915dc64364736f6c634300080a0033`,
  BytecodeLen: 18731,
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
