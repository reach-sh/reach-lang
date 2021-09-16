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
  const ctc0 = stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, 128));
  const ctc1 = stdlib.T_UInt;
  const ctc2 = stdlib.T_Address;
  
  
  const v53 = stdlib.protect(ctc0, interact.info, 'for Alice\'s interact field info');
  const v54 = stdlib.protect(ctc1, interact.request, 'for Alice\'s interact field request');
  
  const txn1 = await (ctc.sendrecv({
    args: [v54],
    evt_cnt: 1,
    funcNum: 0,
    onlyIf: true,
    out_tys: [ctc1],
    pay: [stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), []],
    sim_p: (async (txn1) => {
      const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
      
      const [v58] = txn1.data;
      const v59 = txn1.time;
      const v60 = txn1.secs;
      const v57 = txn1.from;
      
      sim_r.txns.push({
        amt: stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0),
        kind: 'to',
        tok: undefined
        });
      sim_r.isHalt = false;
      
      return sim_r;
      }),
    soloSend: true,
    timeoutAt: undefined,
    tys: [ctc1],
    waitIfNotPresent: false
    }));
  const [v58] = txn1.data;
  const v59 = txn1.time;
  const v60 = txn1.secs;
  const v57 = txn1.from;
  ;
  const txn2 = await (ctc.recv({
    evt_cnt: 0,
    funcNum: 1,
    out_tys: [],
    timeoutAt: undefined,
    waitIfNotPresent: false
    }));
  const [] = txn2.data;
  const v65 = txn2.time;
  const v66 = txn2.secs;
  const v64 = txn2.from;
  ;
  const txn3 = await (ctc.sendrecv({
    args: [v57, v58, v53],
    evt_cnt: 1,
    funcNum: 2,
    onlyIf: true,
    out_tys: [ctc0],
    pay: [stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), []],
    sim_p: (async (txn3) => {
      const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
      
      const [v72] = txn3.data;
      const v73 = txn3.time;
      const v74 = txn3.secs;
      const v71 = txn3.from;
      
      sim_r.txns.push({
        amt: stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0),
        kind: 'to',
        tok: undefined
        });
      const v76 = stdlib.addressEq(v57, v71);
      stdlib.assert(v76, {
        at: './index.rsh:27:5:dot',
        fs: [],
        msg: 'sender correct',
        who: 'Alice'
        });
      sim_r.txns.push({
        amt: v58,
        kind: 'from',
        to: v57,
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
    tys: [ctc2, ctc1, ctc0],
    waitIfNotPresent: false
    }));
  const [v72] = txn3.data;
  const v73 = txn3.time;
  const v74 = txn3.secs;
  const v71 = txn3.from;
  ;
  const v76 = stdlib.addressEq(v57, v71);
  stdlib.assert(v76, {
    at: './index.rsh:27:5:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Alice'
    });
  ;
  return;
  
  
  
  };
export async function Bob(ctc, interact) {
  if (typeof(ctc) !== 'object' || ctc.sendrecv === undefined) {
    return Promise.reject(new Error(`The backend for Bob expects to receive a contract as its first argument.`));}
  if (typeof(interact) !== 'object') {
    return Promise.reject(new Error(`The backend for Bob expects to receive an interact object as its second argument.`));}
  const stdlib = ctc.stdlib;
  const ctc0 = stdlib.T_UInt;
  const ctc1 = stdlib.T_Null;
  const ctc2 = stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, 128));
  const ctc3 = stdlib.T_Address;
  
  
  const txn1 = await (ctc.recv({
    evt_cnt: 1,
    funcNum: 0,
    out_tys: [ctc0],
    timeoutAt: undefined,
    waitIfNotPresent: false
    }));
  const [v58] = txn1.data;
  const v59 = txn1.time;
  const v60 = txn1.secs;
  const v57 = txn1.from;
  ;
  stdlib.protect(ctc1, await interact.want(v58), {
    at: './index.rsh:21:18:application',
    fs: ['at ./index.rsh:20:9:application call to [unknown function] (defined at: ./index.rsh:20:13:function exp)'],
    msg: 'want',
    who: 'Bob'
    });
  
  const txn2 = await (ctc.sendrecv({
    args: [v57, v58],
    evt_cnt: 0,
    funcNum: 1,
    onlyIf: true,
    out_tys: [],
    pay: [v58, []],
    sim_p: (async (txn2) => {
      const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
      
      const [] = txn2.data;
      const v65 = txn2.time;
      const v66 = txn2.secs;
      const v64 = txn2.from;
      
      sim_r.txns.push({
        amt: v58,
        kind: 'to',
        tok: undefined
        });
      sim_r.isHalt = false;
      
      return sim_r;
      }),
    soloSend: true,
    timeoutAt: undefined,
    tys: [ctc3, ctc0],
    waitIfNotPresent: false
    }));
  const [] = txn2.data;
  const v65 = txn2.time;
  const v66 = txn2.secs;
  const v64 = txn2.from;
  ;
  const txn3 = await (ctc.recv({
    evt_cnt: 1,
    funcNum: 2,
    out_tys: [ctc2],
    timeoutAt: undefined,
    waitIfNotPresent: false
    }));
  const [v72] = txn3.data;
  const v73 = txn3.time;
  const v74 = txn3.secs;
  const v71 = txn3.from;
  ;
  const v76 = stdlib.addressEq(v57, v71);
  stdlib.assert(v76, {
    at: './index.rsh:27:5:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Bob'
    });
  ;
  stdlib.protect(ctc1, await interact.got(v72), {
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
int 40
==
assert
dup
substring 0 32
store 255
dup
substring 32 40
btoi
store 254
pop
txn Sender
global CreatorAddress
==
assert
load 255
store 2
// "CheckPay"
// "./index.rsh:17:5:dot"
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
// "./index.rsh:17:5:dot"
// "[]"
// compute state in HM_Set 1
byte base64(AAAAAAAAAAE=)
txn Sender
concat
load 254
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
bz l2
pop
txna ApplicationArgs 1
dup
len
int 40
==
assert
dup
substring 0 32
store 255
dup
substring 32 40
btoi
store 254
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
sha256
load 1
==
assert
// "CheckPay"
// "./index.rsh:22:5:dot"
// "[]"
load 254
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
// compute state in HM_Set 2
byte base64(AAAAAAAAAAI=)
load 255
concat
load 254
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
int 40
==
assert
dup
substring 0 32
store 255
dup
substring 32 40
btoi
store 254
pop
txna ApplicationArgs 2
dup
len
int 128
==
assert
dup
store 253
pop
// compute state in HM_Check 2
byte base64(AAAAAAAAAAI=)
load 255
concat
load 254
itob
concat
sha256
load 1
==
assert
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
gtxns Sender
==
assert
load 255
dig 1
gtxns Receiver
==
assert
l5:
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
l6:
pop
global ZeroAddress
store 1
txn OnCompletion
int DeleteApplication
==
assert
b updateState
l4:
int 0
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
                "name": "v58",
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
                "name": "v58",
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
                "name": "v57",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v58",
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
                "name": "v57",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v58",
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
                "internalType": "uint8[128]",
                "name": "v72",
                "type": "uint8[128]"
              }
            ],
            "internalType": "struct T5",
            "name": "msg",
            "type": "tuple"
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
    "inputs": [
      {
        "components": [
          {
            "components": [
              {
                "internalType": "address payable",
                "name": "v57",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v58",
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
                "name": "v57",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v58",
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
                "internalType": "uint8[128]",
                "name": "v72",
                "type": "uint8[128]"
              }
            ],
            "internalType": "struct T5",
            "name": "msg",
            "type": "tuple"
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
    "stateMutability": "payable",
    "type": "receive"
  }
]`,
  Bytecode: `0x60806040526040516105b03803806105b08339810160408190526100229161010b565b600054610031901560086100e2565b60016000556040805182511515815260208084015151908201527faff782b1924a183a27e241c932e6b8714f423aa173223de3f7e3e2cb89682676910160405180910390a1610082341560076100e2565b6040805180820182526000602080830182815233845294810151518552835160018183015292516001600160a01b0316838501529351606080840191909152835180840390910181526080909201909252805192019190912090556101b9565b816101075760405163100960cb60e01b81526004810182905260240160405180910390fd5b5050565b600081830360408082121561011f57600080fd5b80518082016001600160401b03808211838310171561014e57634e487b7160e01b600052604160045260246000fd5b908352855190811515821461016257600080fd5b8183526020601f198601121561017757600080fd5b8351945060208501915084821081831117156101a357634e487b7160e01b600052604160045260246000fd5b5090915260209384015182529283015250919050565b6103e8806101c86000396000f3fe60806040526004361061002d5760003560e01c80631c1ba5ee146100395780637f6e0eb71461004e57600080fd5b3661003457005b600080fd5b61004c6100473660046102cf565b610061565b005b61004c61005c3660046102e7565b610171565b60405161009d9061007990600190849060200161039e565b6040516020818303038152906040528051906020012060001c60005414600a610268565b60016000556040517f5dbde784174da6839951446f0cdb4600662ac319bb541eb52c458586ef0f9dfb906100d2908390610319565b60405180910390a16100eb346020830135146009610268565b604080518082019091526000808252602082015261010c60208301836102ad565b6001600160a01b03168152602080830135818301526040516101529160029184910191825280516001600160a01b03166020808401919091520151604082015260600190565b60408051601f1981840301815291905280516020909101206000555050565b6040516101ad9061018990600290849060200161039e565b6040516020818303038152906040528051906020012060001c60005414600d610268565b60016000556040517fed56411bf8f63ffdf329be2a88a3331a1f05fb01aeaf4dccefabc6809f0aa044906101e2908390610349565b60405180910390a16101f63415600b610268565b6102183361020760208401846102ad565b6001600160a01b031614600c610268565b61022560208201826102ad565b6040516001600160a01b039190911690602083013580156108fc02916000818181858888f19350505050158015610260573d6000803e3d6000fd5b506000805533ff5b8161028d5760405163100960cb60e01b81526004810182905260240160405180910390fd5b5050565b80356001600160a01b03811681146102a857600080fd5b919050565b6000602082840312156102bf57600080fd5b6102c882610291565b9392505050565b6000606082840312156102e157600080fd5b50919050565b600061104082840312156102e157600080fd5b6001600160a01b0361030b82610291565b168252602090810135910152565b6060810161032782846102fa565b604083013580151580821461033b57600080fd5b806040850152505092915050565b611040810161035882846102fa565b60408201604084016000805b608081101561039457823560ff811680821461037e578384fd5b8552506020938401939290920191600101610364565b5050505092915050565b828152606081016102c860208301846102fa56fea26469706673582212200e61f120109c0edecda3d4b4eeb9cb1e09fa1039f8d3b1223a5a9cc189f9e2fe64736f6c63430008070033`,
  BytecodeLen: 1456,
  Which: `oD`,
  version: 2,
  views: {
    }
  };

export const _Connectors = {
  ALGO: _ALGO,
  ETH: _ETH
  };

