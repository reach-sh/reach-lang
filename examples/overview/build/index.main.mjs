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
  const ctc2 = stdlib.T_Address;
  
  
  const v17 = await ctc.creationTime();
  const v18 = await ctc.creationSecs();
  
  const v15 = stdlib.protect(ctc0, interact.info, 'for Alice\'s interact field info');
  const v16 = stdlib.protect(ctc1, interact.request, 'for Alice\'s interact field request');
  
  const txn1 = await (ctc.sendrecv({
    args: [v16],
    evt_cnt: 1,
    funcNum: 1,
    onlyIf: true,
    out_tys: [ctc1],
    pay: [stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), []],
    sim_p: (async (txn1) => {
      const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
      
      const [v23] = txn1.data;
      const v25 = txn1.time;
      const v26 = txn1.secs;
      const v22 = txn1.from;
      
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
  const [v23] = txn1.data;
  const v25 = txn1.time;
  const v26 = txn1.secs;
  const v22 = txn1.from;
  ;
  const txn2 = await (ctc.recv({
    evt_cnt: 0,
    funcNum: 2,
    out_tys: [],
    timeoutAt: undefined,
    waitIfNotPresent: false
    }));
  const [] = txn2.data;
  const v33 = txn2.time;
  const v34 = txn2.secs;
  const v30 = txn2.from;
  ;
  const txn3 = await (ctc.sendrecv({
    args: [v22, v23, v15],
    evt_cnt: 1,
    funcNum: 3,
    onlyIf: true,
    out_tys: [ctc0],
    pay: [stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), []],
    sim_p: (async (txn3) => {
      const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
      
      const [v39] = txn3.data;
      const v42 = txn3.time;
      const v43 = txn3.secs;
      const v38 = txn3.from;
      
      sim_r.txns.push({
        amt: stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0),
        kind: 'to',
        tok: undefined
        });
      const v41 = stdlib.addressEq(v22, v38);
      stdlib.assert(v41, {
        at: './index.rsh:27:5:dot',
        fs: [],
        msg: 'sender correct',
        who: 'Alice'
        });
      sim_r.txns.push({
        amt: v23,
        kind: 'from',
        to: v22,
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
  const [v39] = txn3.data;
  const v42 = txn3.time;
  const v43 = txn3.secs;
  const v38 = txn3.from;
  ;
  const v41 = stdlib.addressEq(v22, v38);
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
  const ctc3 = stdlib.T_Address;
  
  
  const v17 = await ctc.creationTime();
  const v18 = await ctc.creationSecs();
  
  const txn1 = await (ctc.recv({
    evt_cnt: 1,
    funcNum: 1,
    out_tys: [ctc0],
    timeoutAt: undefined,
    waitIfNotPresent: false
    }));
  const [v23] = txn1.data;
  const v25 = txn1.time;
  const v26 = txn1.secs;
  const v22 = txn1.from;
  ;
  stdlib.protect(ctc1, await interact.want(v23), {
    at: './index.rsh:21:18:application',
    fs: ['at ./index.rsh:20:9:application call to [unknown function] (defined at: ./index.rsh:20:13:function exp)'],
    msg: 'want',
    who: 'Bob'
    });
  
  const txn2 = await (ctc.sendrecv({
    args: [v22, v23],
    evt_cnt: 0,
    funcNum: 2,
    onlyIf: true,
    out_tys: [],
    pay: [v23, []],
    sim_p: (async (txn2) => {
      const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
      
      const [] = txn2.data;
      const v33 = txn2.time;
      const v34 = txn2.secs;
      const v30 = txn2.from;
      
      sim_r.txns.push({
        amt: v23,
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
  const v33 = txn2.time;
  const v34 = txn2.secs;
  const v30 = txn2.from;
  ;
  const txn3 = await (ctc.recv({
    evt_cnt: 1,
    funcNum: 3,
    out_tys: [ctc2],
    timeoutAt: undefined,
    waitIfNotPresent: false
    }));
  const [v39] = txn3.data;
  const v42 = txn3.time;
  const v43 = txn3.secs;
  const v38 = txn3.from;
  ;
  const v41 = stdlib.addressEq(v22, v38);
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
store 1
substring 32 64
store 2
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
int 0
==
assert
pop
txna ApplicationArgs 2
dup
len
int 8
==
assert
dup
btoi
store 255
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
load 255
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
// Handler 2
dup
int 2
==
bz l1
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
l1:
// Handler 3
dup
int 3
==
bz l3
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
gtxns Sender
==
assert
load 255
dig 1
gtxns Receiver
==
assert
l4:
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
l5:
pop
global ZeroAddress
store 1
txn OnCompletion
int DeleteApplication
==
assert
b updateState
l3:
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
store 2
// compute state in HM_Set 0
int 8
bzero
sha256
store 1
txn OnCompletion
int NoOp
==
assert
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
            "internalType": "bool",
            "name": "svs",
            "type": "bool"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v23",
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
                "name": "v22",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v23",
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
        "internalType": "struct T4",
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
                "name": "v22",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v23",
                "type": "uint256"
              }
            ],
            "internalType": "struct T1",
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
    "name": "e3",
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
                "name": "v23",
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
                "name": "v22",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v23",
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
        "internalType": "struct T4",
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
                "name": "v22",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v23",
                "type": "uint256"
              }
            ],
            "internalType": "struct T1",
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
  Bytecode: `0x608060408190527f49ff028a829527a47ec6839c7147b484eccf5a2a94853eddac09cef44d9d4e9e90600090a16040805180820182524381524260209182015281516000818301819052818401819052835180830385018152606090920190935280519101209055610534806100766000396000f3fe6080604052600436106100385760003560e01c80631dc091ad1461004457806329c0fdf314610059578063cc5fe27f1461006c57600080fd5b3661003f57005b600080fd5b6100576100523660046103f1565b61007f565b005b610057610067366004610409565b610179565b61005761007a36600461041b565b610269565b6100d1600061009160208401846103d6565b6040516020016100ad9291909182521515602082015260400190565b6040516020818303038152906040528051906020012060001c60005414600861035f565b600080556040517f120854c39fdbc847613c8c1a66d23aa6d099c4db8f64d852475191e60a6298d99061010590839061044d565b60405180910390a16101193415600761035f565b604080518082018252338152602083810135818301908152835160019281019290925282516001600160a01b03169382019390935291516060830152906080015b60408051601f1981840301815291905280516020909101206000555050565b6040516101b5906101919060019084906020016104ea565b6040516020818303038152906040528051906020012060001c60005414600a61035f565b600080556040517fba9848ba573d919b437f603568b07882c6af6fc804c046ac1cd11132ef3ba165906101e990839061046e565b60405180910390a161020234602083013514600961035f565b604080518082019091526000808252602082015261022360208301836103b4565b6001600160a01b031681526020808301358183015260405161015a9160029184910191825280516001600160a01b03166020808401919091520151604082015260600190565b6040516102a5906102819060029084906020016104ea565b6040516020818303038152906040528051906020012060001c60005414600d61035f565b600080556040517f1744c2be3b3814193b70ec591ef6860fe14b411a9b67c63641dc62b583c4193b906102d9908390610495565b60405180910390a16102ed3415600b61035f565b61030f336102fe60208401846103b4565b6001600160a01b031614600c61035f565b61031c60208201826103b4565b6040516001600160a01b039190911690602083013580156108fc02916000818181858888f19350505050158015610357573d6000803e3d6000fd5b506000805533ff5b816103845760405163100960cb60e01b81526004810182905260240160405180910390fd5b5050565b80356001600160a01b038116811461039f57600080fd5b919050565b8035801515811461039f57600080fd5b6000602082840312156103c657600080fd5b6103cf82610388565b9392505050565b6000602082840312156103e857600080fd5b6103cf826103a4565b60006040828403121561040357600080fd5b50919050565b60006060828403121561040357600080fd5b6000611040828403121561040357600080fd5b6001600160a01b0361043f82610388565b168252602090810135910152565b6040810161045a836103a4565b151582526020830135602083015292915050565b6060810161047c828461042e565b610488604084016103a4565b1515604083015292915050565b61104081016104a4828461042e565b60408201604084016000805b60808110156104e057823560ff81168082146104ca578384fd5b85525060209384019392909201916001016104b0565b5050505092915050565b828152606081016103cf602083018461042e56fea26469706673582212209b407deed8d08b3bed53608e3c433654df682034eac8fbdd2759d97f8a30abdf64736f6c63430008060033`,
  BytecodeLen: 1450,
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

