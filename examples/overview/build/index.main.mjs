// Automatically generated with Reach 0.1.3
/* eslint-disable */
export const _version = '0.1.3';
export const _backendVersion = 1;


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
  if (ctc.sendrecv === undefined) {
    return Promise.reject(new Error(`The backend for Alice expects to receive a contract as its first argument.`));}
  if (typeof(interact) !== 'object') {
    return Promise.reject(new Error(`The backend for Alice expects to receive an interact object as its second argument.`));}
  const stdlib = ctc.stdlib;
  const ctc0 = stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, 128));
  const ctc1 = stdlib.T_UInt;
  const ctc2 = stdlib.T_Tuple([]);
  const ctc3 = stdlib.T_Address;
  const ctc4 = stdlib.T_Tuple([ctc1, ctc3, ctc1, ctc1]);
  const ctc5 = stdlib.T_Tuple([ctc1, ctc3, ctc1]);
  const ctc6 = stdlib.T_Tuple([ctc1]);
  const ctc7 = stdlib.T_Tuple([ctc1, ctc1]);
  
  
  const v20 = await ctc.creationTime();
  const v18 = stdlib.protect(ctc0, interact.info, 'for Alice\'s interact field info');
  const v19 = stdlib.protect(ctc1, interact.request, 'for Alice\'s interact field request');
  
  const txn1 = await (ctc.sendrecv({
    args: [v20, v19],
    evt_cnt: 1,
    funcNum: 1,
    hasLastTime: stdlib.checkedBigNumberify('./index.rsh:17:5:dot', stdlib.UInt_max, 0),
    onlyIf: true,
    out_tys: [ctc1],
    pay: [stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), []],
    sim_p: (async (txn1) => {
      const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
      
      sim_r.prevSt = stdlib.digest(ctc7, [stdlib.checkedBigNumberify('./index.rsh:17:5:dot', stdlib.UInt_max, 0), v20]);
      sim_r.prevSt_noPrevTime = stdlib.digest(ctc6, [stdlib.checkedBigNumberify('./index.rsh:17:5:dot', stdlib.UInt_max, 0)]);
      const [v25] = txn1.data;
      const v27 = txn1.time;
      const v24 = txn1.from;
      
      sim_r.txns.push({
        amt: stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0),
        kind: 'to',
        tok: undefined
        });
      sim_r.nextSt = stdlib.digest(ctc4, [stdlib.checkedBigNumberify('./index.rsh:18:11:after expr stmt semicolon', stdlib.UInt_max, 1), v24, v25, v27]);
      sim_r.nextSt_noTime = stdlib.digest(ctc5, [stdlib.checkedBigNumberify('./index.rsh:18:11:after expr stmt semicolon', stdlib.UInt_max, 1), v24, v25]);
      sim_r.view = [ctc6, [stdlib.checkedBigNumberify('./index.rsh:18:11:after expr stmt semicolon', stdlib.UInt_max, 0)]];
      sim_r.isHalt = false;
      
      return sim_r;
      }),
    soloSend: true,
    timeout_delay: false,
    tys: [ctc1, ctc1],
    waitIfNotPresent: false
    }));
  const [v25] = txn1.data;
  const v27 = txn1.time;
  const v24 = txn1.from;
  ;
  const txn2 = await (ctc.recv({
    evt_cnt: 0,
    funcNum: 2,
    out_tys: [],
    timeout_delay: false,
    waitIfNotPresent: false
    }));
  const [] = txn2.data;
  const v34 = txn2.time;
  const v31 = txn2.from;
  ;
  const txn3 = await (ctc.sendrecv({
    args: [v24, v25, v34, v18],
    evt_cnt: 1,
    funcNum: 3,
    hasLastTime: stdlib.checkedBigNumberify('./index.rsh:27:5:dot', stdlib.UInt_max, 2),
    onlyIf: true,
    out_tys: [ctc0],
    pay: [stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), []],
    sim_p: (async (txn3) => {
      const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
      
      sim_r.prevSt = stdlib.digest(ctc4, [stdlib.checkedBigNumberify('./index.rsh:27:5:dot', stdlib.UInt_max, 2), v24, v25, v34]);
      sim_r.prevSt_noPrevTime = stdlib.digest(ctc5, [stdlib.checkedBigNumberify('./index.rsh:27:5:dot', stdlib.UInt_max, 2), v24, v25]);
      const [v39] = txn3.data;
      const v42 = txn3.time;
      const v38 = txn3.from;
      
      sim_r.txns.push({
        amt: stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0),
        kind: 'to',
        tok: undefined
        });
      const v41 = stdlib.addressEq(v24, v38);
      stdlib.assert(v41, {
        at: './index.rsh:27:5:dot',
        fs: [],
        msg: 'sender correct',
        who: 'Alice'
        });
      sim_r.txns.push({
        amt: v25,
        kind: 'from',
        to: v24,
        tok: undefined
        });
      sim_r.txns.push({
        kind: 'halt',
        tok: undefined
        })
      sim_r.nextSt = stdlib.digest(ctc2, []);
      sim_r.nextSt_noTime = stdlib.digest(ctc2, []);
      sim_r.view = [ctc2, []];
      sim_r.isHalt = true;
      
      return sim_r;
      }),
    soloSend: true,
    timeout_delay: false,
    tys: [ctc3, ctc1, ctc1, ctc0],
    waitIfNotPresent: false
    }));
  const [v39] = txn3.data;
  const v42 = txn3.time;
  const v38 = txn3.from;
  ;
  const v41 = stdlib.addressEq(v24, v38);
  stdlib.assert(v41, {
    at: './index.rsh:27:5:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Alice'
    });
  ;
  return;
  
  
  
  };
export async function Bob(ctc, interact) {
  if (ctc.sendrecv === undefined) {
    return Promise.reject(new Error(`The backend for Bob expects to receive a contract as its first argument.`));}
  if (typeof(interact) !== 'object') {
    return Promise.reject(new Error(`The backend for Bob expects to receive an interact object as its second argument.`));}
  const stdlib = ctc.stdlib;
  const ctc0 = stdlib.T_UInt;
  const ctc1 = stdlib.T_Null;
  const ctc2 = stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, 128));
  const ctc3 = stdlib.T_Tuple([ctc0]);
  const ctc4 = stdlib.T_Address;
  const ctc5 = stdlib.T_Tuple([ctc0, ctc4, ctc0, ctc0]);
  const ctc6 = stdlib.T_Tuple([ctc0, ctc4, ctc0]);
  
  
  const v20 = await ctc.creationTime();
  const txn1 = await (ctc.recv({
    evt_cnt: 1,
    funcNum: 1,
    out_tys: [ctc0],
    timeout_delay: false,
    waitIfNotPresent: false
    }));
  const [v25] = txn1.data;
  const v27 = txn1.time;
  const v24 = txn1.from;
  ;
  stdlib.protect(ctc1, await interact.want(v25), {
    at: './index.rsh:21:18:application',
    fs: ['at ./index.rsh:20:9:application call to [unknown function] (defined at: ./index.rsh:20:13:function exp)'],
    msg: 'want',
    who: 'Bob'
    });
  
  const txn2 = await (ctc.sendrecv({
    args: [v24, v25, v27],
    evt_cnt: 0,
    funcNum: 2,
    hasLastTime: stdlib.checkedBigNumberify('./index.rsh:22:5:dot', stdlib.UInt_max, 2),
    onlyIf: true,
    out_tys: [],
    pay: [v25, []],
    sim_p: (async (txn2) => {
      const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
      
      sim_r.prevSt = stdlib.digest(ctc5, [stdlib.checkedBigNumberify('./index.rsh:22:5:dot', stdlib.UInt_max, 1), v24, v25, v27]);
      sim_r.prevSt_noPrevTime = stdlib.digest(ctc6, [stdlib.checkedBigNumberify('./index.rsh:22:5:dot', stdlib.UInt_max, 1), v24, v25]);
      const [] = txn2.data;
      const v34 = txn2.time;
      const v31 = txn2.from;
      
      sim_r.txns.push({
        amt: v25,
        kind: 'to',
        tok: undefined
        });
      sim_r.nextSt = stdlib.digest(ctc5, [stdlib.checkedBigNumberify('./index.rsh:23:11:after expr stmt semicolon', stdlib.UInt_max, 2), v24, v25, v34]);
      sim_r.nextSt_noTime = stdlib.digest(ctc6, [stdlib.checkedBigNumberify('./index.rsh:23:11:after expr stmt semicolon', stdlib.UInt_max, 2), v24, v25]);
      sim_r.view = [ctc3, [stdlib.checkedBigNumberify('./index.rsh:23:11:after expr stmt semicolon', stdlib.UInt_max, 0)]];
      sim_r.isHalt = false;
      
      return sim_r;
      }),
    soloSend: true,
    timeout_delay: false,
    tys: [ctc4, ctc0, ctc0],
    waitIfNotPresent: false
    }));
  const [] = txn2.data;
  const v34 = txn2.time;
  const v31 = txn2.from;
  ;
  const txn3 = await (ctc.recv({
    evt_cnt: 1,
    funcNum: 3,
    out_tys: [ctc2],
    timeout_delay: false,
    waitIfNotPresent: false
    }));
  const [v39] = txn3.data;
  const v42 = txn3.time;
  const v38 = txn3.from;
  ;
  const v41 = stdlib.addressEq(v24, v38);
  stdlib.assert(v41, {
    at: './index.rsh:27:5:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Bob'
    });
  ;
  stdlib.protect(ctc1, await interact.got(v39), {
    at: './index.rsh:32:17:application',
    fs: ['at ./index.rsh:31:9:application call to [unknown function] (defined at: ./index.rsh:31:13:function exp)'],
    msg: 'got',
    who: 'Bob'
    });
  
  return;
  
  
  
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
store 2
substring 32 64
store 3
txn NumAppArgs
int 3
==
assert
txna ApplicationArgs 0
btoi
dup
bz ctor
// Handler 1
dup
int 1
==
bz l0
pop
txna ApplicationArgs 1
dup
len
int 8
==
assert
dup
btoi
store 255
pop
txna ApplicationArgs 2
dup
len
int 8
==
assert
dup
btoi
store 254
pop
// compute state in HM_Check 0
int 8
bzero
load 255
itob
concat
sha256
load 2
==
assert
int 16
bzero
store 1
int 0
load 1
substring 0 8
btoi
dup
bz l1
dig 1
gtxns FirstValid
<=
assert
b l2
l1:
pop
l2:
load 1
substring 8 16
btoi
dup
bz l3
dig 1
gtxns LastValid
>=
assert
b l4
l3:
pop
l4:
pop
// "CheckPay"
// "./index.rsh:17:5:dot"
// "[]"
// compute state in HM_Set 1
byte base64(AAAAAAAAAAE=)
txn Sender
concat
load 254
itob
concat
global Round
itob
concat
sha256
store 2
txn OnCompletion
int NoOp
==
assert
b updateState
l0:
// Handler 2
dup
int 2
==
bz l5
pop
txna ApplicationArgs 1
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
sha256
load 2
==
assert
int 16
bzero
store 1
int 0
load 1
substring 0 8
btoi
dup
bz l6
dig 1
gtxns FirstValid
<=
assert
b l7
l6:
pop
l7:
load 1
substring 8 16
btoi
dup
bz l8
dig 1
gtxns LastValid
>=
assert
b l9
l8:
pop
l9:
pop
// "CheckPay"
// "./index.rsh:22:5:dot"
// "[]"
load 254
dup
bz l10
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
dup
load 1
substring 0 8
btoi
dup
bz l11
dig 1
gtxns FirstValid
<=
assert
b l12
l11:
pop
l12:
load 1
substring 8 16
btoi
dup
bz l13
dig 1
gtxns LastValid
>=
assert
b l14
l13:
pop
l14:
pop
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
l10:
pop
// compute state in HM_Set 2
byte base64(AAAAAAAAAAI=)
load 255
concat
load 254
itob
concat
global Round
itob
concat
sha256
store 2
txn OnCompletion
int NoOp
==
assert
b updateState
l5:
// Handler 3
dup
int 3
==
bz l15
pop
txna ApplicationArgs 1
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
txna ApplicationArgs 2
dup
len
int 128
==
assert
dup
store 252
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
sha256
load 2
==
assert
int 16
bzero
store 1
int 0
load 1
substring 0 8
btoi
dup
bz l16
dig 1
gtxns FirstValid
<=
assert
b l17
l16:
pop
l17:
load 1
substring 8 16
btoi
dup
bz l18
dig 1
gtxns LastValid
>=
assert
b l19
l18:
pop
l19:
pop
// "CheckPay"
// "./index.rsh:27:5:dot"
// "[]"
// Just "sender correct"
// "./index.rsh:27:5:dot"
// "[]"
load 255
txn Sender
==
assert
load 254
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
dup
load 1
substring 0 8
btoi
dup
bz l21
dig 1
gtxns FirstValid
<=
assert
b l22
l21:
pop
l22:
load 1
substring 8 16
btoi
dup
bz l23
dig 1
gtxns LastValid
>=
assert
b l24
l23:
pop
l24:
pop
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
dup
load 1
substring 0 8
btoi
dup
bz l26
dig 1
gtxns FirstValid
<=
assert
b l27
l26:
pop
l27:
load 1
substring 8 16
btoi
dup
bz l28
dig 1
gtxns LastValid
>=
assert
b l29
l28:
pop
l29:
pop
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
l25:
pop
global ZeroAddress
store 2
txn OnCompletion
int DeleteApplication
==
assert
b updateState
l15:
int 0
assert
updateState:
byte base64()
load 2
load 3
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
byte base64()
int 64
bzero
app_global_put
b checkSize
ctor:
txn Sender
global CreatorAddress
==
assert
txna ApplicationArgs 1
store 3
// compute state in HM_Set 0
int 8
bzero
global Round
itob
concat
sha256
store 2
txn OnCompletion
int NoOp
==
assert
b updateState
`,
  appClear: `#pragma version 4
txn RekeyTo
global ZeroAddress
==
assert
txn Lease
global ZeroAddress
==
assert
global GroupSize
int 1
==
assert
byte base64()
app_global_get
substring 0 32
global ZeroAddress
==
assert
done:
int 1
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
  version: 2,
  viewKeys: 0,
  viewSize: 0
  };
const _ETH = {
  ABI: `[
  {
    "inputs": [],
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
    "inputs": [],
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
                "internalType": "uint256",
                "name": "v20",
                "type": "uint256"
              }
            ],
            "internalType": "struct T0",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v25",
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
                "name": "v24",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v25",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v27",
                "type": "uint256"
              }
            ],
            "internalType": "struct T1",
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
        "internalType": "struct T6",
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
                "name": "v24",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v25",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v34",
                "type": "uint256"
              }
            ],
            "internalType": "struct T4",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint8[128]",
                "name": "v39",
                "type": "uint8[128]"
              }
            ],
            "internalType": "struct T7",
            "name": "msg",
            "type": "tuple"
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
    "inputs": [
      {
        "components": [
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v20",
                "type": "uint256"
              }
            ],
            "internalType": "struct T0",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v25",
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
                "name": "v24",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v25",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v27",
                "type": "uint256"
              }
            ],
            "internalType": "struct T1",
            "name": "svs",
            "type": "tuple"
          },
          {
            "internalType": "bool",
            "name": "msg",
            "type": "bool"
          }
        ],
        "internalType": "struct T6",
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
                "name": "v24",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v25",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v34",
                "type": "uint256"
              }
            ],
            "internalType": "struct T4",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint8[128]",
                "name": "v39",
                "type": "uint8[128]"
              }
            ],
            "internalType": "struct T7",
            "name": "msg",
            "type": "tuple"
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
    "stateMutability": "payable",
    "type": "receive"
  }
]`,
  Bytecode: `0x608060408190527f49ff028a829527a47ec6839c7147b484eccf5a2a94853eddac09cef44d9d4e9e90600090a16040805160208082018352438252825180820184526000808252925181528351808301849052905181850152835180820385018152606090910190935282519201919091209055610531806100826000396000f3fe6080604052600436106100385760003560e01c80632438df70146100445780639532ef0114610059578063f512f77e1461006c57600080fd5b3661003f57005b600080fd5b6100576100523660046103e3565b61007f565b005b6100576100673660046103cb565b61018f565b61005761007a3660046103f5565b61026f565b6040516100bb906100979060019084906020016104b6565b6040516020818303038152906040528051906020012060001c60005414600a610364565b600080556100d0346020830135146009610364565b7f1ca594b20641191c893d80895212a20239e99e17b7304a35c096140ec34f22dd816040516100ff9190610431565b60405180910390a1610134604051806060016040528060006001600160a01b0316815260200160008152602001600081525090565b61014160208301836103a9565b6001600160a01b03168152602080830135818301524360408084019190915251610170916002918491016104ca565b60408051601f1981840301815291905280516020909101206000555050565b60408051600060208201528235918101919091526101ce906060016040516020818303038152906040528051906020012060001c600054146008610364565b600080556101de34156007610364565b6040805182358152602080840135908201527ff2c62eba998811305a23599b2e6d212befbd7ded3a73f4c08bfb9aefe08dc166910160405180910390a1610248604051806060016040528060006001600160a01b0316815260200160008152602001600081525090565b338152602080830135818301524360408084019190915251610170916001918491016104ca565b6040516102ab906102879060029084906020016104b6565b6040516020818303038152906040528051906020012060001c60005414600d610364565b600080556102bb3415600b610364565b6102dd336102cc60208401846103a9565b6001600160a01b031614600c610364565b6102ea60208201826103a9565b6040516001600160a01b039190911690602083013580156108fc02916000818181858888f19350505050158015610325573d6000803e3d6000fd5b507f6ca511835aec60423a26d24cdbe1d3b53c20c6d05a3c891aed1744e1f97974bf816040516103559190610461565b60405180910390a16000805533ff5b816103895760405163100960cb60e01b81526004810182905260240160405180910390fd5b5050565b80356001600160a01b03811681146103a457600080fd5b919050565b6000602082840312156103bb57600080fd5b6103c48261038d565b9392505050565b6000604082840312156103dd57600080fd5b50919050565b6000608082840312156103dd57600080fd5b600061106082840312156103dd57600080fd5b6001600160a01b036104198261038d565b16825260208181013590830152604090810135910152565b6080810161043f8284610408565b606083013580151580821461045357600080fd5b806060850152505092915050565b61106081016104708284610408565b60608201606084016000805b60808110156104ac57823560ff8116808214610496578384fd5b855250602093840193929092019160010161047c565b5050505092915050565b828152608081016103c46020830184610408565b828152608081016103c4602083018480516001600160a01b031682526020808201519083015260409081015191015256fea26469706673582212205b0344a1412287b3e2d70819028c3ab06a54ced43ef77eed10738c9a1559377a64736f6c63430008060033`,
  BytecodeLen: 1459,
  Which: `oD`,
  deployMode: `DM_constructor`,
  version: 1,
  views: {
    }
  };

export const _Connectors = {
  ALGO: _ALGO,
  ETH: _ETH
  };

