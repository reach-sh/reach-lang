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
  const ctc4 = stdlib.T_Tuple([ctc1]);
  const ctc5 = stdlib.T_Tuple([ctc1, ctc3, ctc1]);
  
  
  const v28 = await ctc.creationTime();
  const v29 = await ctc.creationSecs();
  
  const v26 = stdlib.protect(ctc0, interact.info, 'for Alice\'s interact field info');
  const v27 = stdlib.protect(ctc1, interact.request, 'for Alice\'s interact field request');
  
  const txn1 = await (ctc.sendrecv({
    args: [v27],
    evt_cnt: 1,
    funcNum: 1,
    onlyIf: true,
    out_tys: [ctc1],
    pay: [stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), []],
    sim_p: (async (txn1) => {
      const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
      
      const [v34] = txn1.data;
      const v36 = txn1.time;
      const v37 = txn1.secs;
      const v33 = txn1.from;
      
      sim_r.txns.push({
        amt: stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0),
        kind: 'to',
        tok: undefined
        });
      sim_r.nextSt = stdlib.digest(ctc5, [stdlib.checkedBigNumberify('./index.rsh:18:11:after expr stmt semicolon', stdlib.UInt_max, 1), v33, v34]);
      sim_r.nextSt_noTime = stdlib.digest(ctc5, [stdlib.checkedBigNumberify('./index.rsh:18:11:after expr stmt semicolon', stdlib.UInt_max, 1), v33, v34]);
      sim_r.view = [ctc4, [stdlib.checkedBigNumberify('./index.rsh:18:11:after expr stmt semicolon', stdlib.UInt_max, 0)]];
      sim_r.isHalt = false;
      
      return sim_r;
      }),
    soloSend: true,
    timeoutAt: undefined,
    tys: [ctc1],
    waitIfNotPresent: false
    }));
  const [v34] = txn1.data;
  const v36 = txn1.time;
  const v37 = txn1.secs;
  const v33 = txn1.from;
  ;
  const txn2 = await (ctc.recv({
    evt_cnt: 0,
    funcNum: 2,
    out_tys: [],
    timeoutAt: undefined,
    waitIfNotPresent: false
    }));
  const [] = txn2.data;
  const v44 = txn2.time;
  const v45 = txn2.secs;
  const v41 = txn2.from;
  ;
  const txn3 = await (ctc.sendrecv({
    args: [v33, v34, v26],
    evt_cnt: 1,
    funcNum: 3,
    onlyIf: true,
    out_tys: [ctc0],
    pay: [stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), []],
    sim_p: (async (txn3) => {
      const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
      
      const [v50] = txn3.data;
      const v53 = txn3.time;
      const v54 = txn3.secs;
      const v49 = txn3.from;
      
      sim_r.txns.push({
        amt: stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0),
        kind: 'to',
        tok: undefined
        });
      const v52 = stdlib.addressEq(v33, v49);
      stdlib.assert(v52, {
        at: './index.rsh:27:5:dot',
        fs: [],
        msg: 'sender correct',
        who: 'Alice'
        });
      sim_r.txns.push({
        amt: v34,
        kind: 'from',
        to: v33,
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
    timeoutAt: undefined,
    tys: [ctc3, ctc1, ctc0],
    waitIfNotPresent: false
    }));
  const [v50] = txn3.data;
  const v53 = txn3.time;
  const v54 = txn3.secs;
  const v49 = txn3.from;
  ;
  const v52 = stdlib.addressEq(v33, v49);
  stdlib.assert(v52, {
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
  const ctc5 = stdlib.T_Tuple([ctc0, ctc4, ctc0]);
  
  
  const v28 = await ctc.creationTime();
  const v29 = await ctc.creationSecs();
  
  const txn1 = await (ctc.recv({
    evt_cnt: 1,
    funcNum: 1,
    out_tys: [ctc0],
    timeoutAt: undefined,
    waitIfNotPresent: false
    }));
  const [v34] = txn1.data;
  const v36 = txn1.time;
  const v37 = txn1.secs;
  const v33 = txn1.from;
  ;
  stdlib.protect(ctc1, await interact.want(v34), {
    at: './index.rsh:21:18:application',
    fs: ['at ./index.rsh:20:9:application call to [unknown function] (defined at: ./index.rsh:20:13:function exp)'],
    msg: 'want',
    who: 'Bob'
    });
  
  const txn2 = await (ctc.sendrecv({
    args: [v33, v34],
    evt_cnt: 0,
    funcNum: 2,
    onlyIf: true,
    out_tys: [],
    pay: [v34, []],
    sim_p: (async (txn2) => {
      const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
      
      const [] = txn2.data;
      const v44 = txn2.time;
      const v45 = txn2.secs;
      const v41 = txn2.from;
      
      sim_r.txns.push({
        amt: v34,
        kind: 'to',
        tok: undefined
        });
      sim_r.nextSt = stdlib.digest(ctc5, [stdlib.checkedBigNumberify('./index.rsh:23:11:after expr stmt semicolon', stdlib.UInt_max, 2), v33, v34]);
      sim_r.nextSt_noTime = stdlib.digest(ctc5, [stdlib.checkedBigNumberify('./index.rsh:23:11:after expr stmt semicolon', stdlib.UInt_max, 2), v33, v34]);
      sim_r.view = [ctc3, [stdlib.checkedBigNumberify('./index.rsh:23:11:after expr stmt semicolon', stdlib.UInt_max, 0)]];
      sim_r.isHalt = false;
      
      return sim_r;
      }),
    soloSend: true,
    timeoutAt: undefined,
    tys: [ctc4, ctc0],
    waitIfNotPresent: false
    }));
  const [] = txn2.data;
  const v44 = txn2.time;
  const v45 = txn2.secs;
  const v41 = txn2.from;
  ;
  const txn3 = await (ctc.recv({
    evt_cnt: 1,
    funcNum: 3,
    out_tys: [ctc2],
    timeoutAt: undefined,
    waitIfNotPresent: false
    }));
  const [v50] = txn3.data;
  const v53 = txn3.time;
  const v54 = txn3.secs;
  const v49 = txn3.from;
  ;
  const v52 = stdlib.addressEq(v33, v49);
  stdlib.assert(v52, {
    at: './index.rsh:27:5:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Bob'
    });
  ;
  stdlib.protect(ctc1, await interact.got(v50), {
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
                "name": "v34",
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
                "name": "v33",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v34",
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
                "name": "v33",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v34",
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
                "name": "v50",
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
                "name": "v34",
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
                "name": "v33",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v34",
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
                "name": "v33",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v34",
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
                "name": "v50",
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
  Bytecode: `0x608060408190527f49ff028a829527a47ec6839c7147b484eccf5a2a94853eddac09cef44d9d4e9e90600090a16040805180820182524381524260209182015281516000818301819052818401819052835180830385018152606090920190935280519101209055610531806100766000396000f3fe6080604052600436106100385760003560e01c80631dc091ad1461004457806329c0fdf314610059578063cc5fe27f1461006c57600080fd5b3661003f57005b600080fd5b6100576100523660046103ee565b61007f565b005b610057610067366004610406565b610178565b61005761007a366004610418565b610267565b6100d1600061009160208401846103d3565b6040516020016100ad9291909182521515602082015260400190565b6040516020818303038152906040528051906020012060001c60005414600861035c565b600080556100e13415600761035c565b7f120854c39fdbc847613c8c1a66d23aa6d099c4db8f64d852475191e60a6298d981604051610110919061044a565b60405180910390a1604080518082018252338152602083810135818301908152835160019281019290925282516001600160a01b03169382019390935291516060830152906080015b60408051601f1981840301815291905280516020909101206000555050565b6040516101b4906101909060019084906020016104e7565b6040516020818303038152906040528051906020012060001c60005414600a61035c565b600080556101c934602083013514600961035c565b7fba9848ba573d919b437f603568b07882c6af6fc804c046ac1cd11132ef3ba165816040516101f8919061046b565b60405180910390a1604080518082019091526000808252602082015261022160208301836103b1565b6001600160a01b03168152602080830135818301526040516101599160029184910191825280516001600160a01b03166020808401919091520151604082015260600190565b6040516102a39061027f9060029084906020016104e7565b6040516020818303038152906040528051906020012060001c60005414600d61035c565b600080556102b33415600b61035c565b6102d5336102c460208401846103b1565b6001600160a01b031614600c61035c565b6102e260208201826103b1565b6040516001600160a01b039190911690602083013580156108fc02916000818181858888f1935050505015801561031d573d6000803e3d6000fd5b507f1744c2be3b3814193b70ec591ef6860fe14b411a9b67c63641dc62b583c4193b8160405161034d9190610492565b60405180910390a16000805533ff5b816103815760405163100960cb60e01b81526004810182905260240160405180910390fd5b5050565b80356001600160a01b038116811461039c57600080fd5b919050565b8035801515811461039c57600080fd5b6000602082840312156103c357600080fd5b6103cc82610385565b9392505050565b6000602082840312156103e557600080fd5b6103cc826103a1565b60006040828403121561040057600080fd5b50919050565b60006060828403121561040057600080fd5b6000611040828403121561040057600080fd5b6001600160a01b0361043c82610385565b168252602090810135910152565b60408101610457836103a1565b151582526020830135602083015292915050565b60608101610479828461042b565b610485604084016103a1565b1515604083015292915050565b61104081016104a1828461042b565b60408201604084016000805b60808110156104dd57823560ff81168082146104c7578384fd5b85525060209384019392909201916001016104ad565b5050505092915050565b828152606081016103cc602083018461042b56fea264697066735822122072432185ce38ab61bceb7a397991bf0b6a969212d238c9b97b3a6da22a377dbf64736f6c63430008060033`,
  BytecodeLen: 1447,
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

