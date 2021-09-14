// Automatically generated with Reach 0.1.4
/* eslint-disable */
export const _version = '0.1.4';
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
  
  
  const v57 = stdlib.protect(ctc0, interact.info, 'for Alice\'s interact field info');
  const v58 = stdlib.protect(ctc1, interact.request, 'for Alice\'s interact field request');
  
  const txn1 = await (ctc.recv({
    evt_cnt: 0,
    funcNum: 0,
    out_tys: [],
    timeoutAt: undefined,
    waitIfNotPresent: false
    }));
  const [] = txn1.data;
  const v60 = txn1.time;
  const v61 = txn1.secs;
  const v59 = txn1.from;
  ;
  const txn2 = await (ctc.sendrecv({
    args: [v58],
    evt_cnt: 1,
    funcNum: 1,
    onlyIf: true,
    out_tys: [ctc1],
    pay: [stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), []],
    sim_p: (async (txn2) => {
      const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
      
      const [v66] = txn2.data;
      const v67 = txn2.time;
      const v68 = txn2.secs;
      const v65 = txn2.from;
      
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
  const [v66] = txn2.data;
  const v67 = txn2.time;
  const v68 = txn2.secs;
  const v65 = txn2.from;
  ;
  const txn3 = await (ctc.recv({
    evt_cnt: 0,
    funcNum: 2,
    out_tys: [],
    timeoutAt: undefined,
    waitIfNotPresent: false
    }));
  const [] = txn3.data;
  const v73 = txn3.time;
  const v74 = txn3.secs;
  const v72 = txn3.from;
  ;
  const txn4 = await (ctc.sendrecv({
    args: [v65, v66, v57],
    evt_cnt: 1,
    funcNum: 3,
    onlyIf: true,
    out_tys: [ctc0],
    pay: [stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), []],
    sim_p: (async (txn4) => {
      const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
      
      const [v80] = txn4.data;
      const v81 = txn4.time;
      const v82 = txn4.secs;
      const v79 = txn4.from;
      
      sim_r.txns.push({
        amt: stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0),
        kind: 'to',
        tok: undefined
        });
      const v84 = stdlib.addressEq(v65, v79);
      stdlib.assert(v84, {
        at: './index.rsh:27:5:dot',
        fs: [],
        msg: 'sender correct',
        who: 'Alice'
        });
      sim_r.txns.push({
        amt: v66,
        kind: 'from',
        to: v65,
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
  const [v80] = txn4.data;
  const v81 = txn4.time;
  const v82 = txn4.secs;
  const v79 = txn4.from;
  ;
  const v84 = stdlib.addressEq(v65, v79);
  stdlib.assert(v84, {
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
    evt_cnt: 0,
    funcNum: 0,
    out_tys: [],
    timeoutAt: undefined,
    waitIfNotPresent: false
    }));
  const [] = txn1.data;
  const v60 = txn1.time;
  const v61 = txn1.secs;
  const v59 = txn1.from;
  ;
  const txn2 = await (ctc.recv({
    evt_cnt: 1,
    funcNum: 1,
    out_tys: [ctc0],
    timeoutAt: undefined,
    waitIfNotPresent: false
    }));
  const [v66] = txn2.data;
  const v67 = txn2.time;
  const v68 = txn2.secs;
  const v65 = txn2.from;
  ;
  stdlib.protect(ctc1, await interact.want(v66), {
    at: './index.rsh:21:18:application',
    fs: ['at ./index.rsh:20:9:application call to [unknown function] (defined at: ./index.rsh:20:13:function exp)'],
    msg: 'want',
    who: 'Bob'
    });
  
  const txn3 = await (ctc.sendrecv({
    args: [v65, v66],
    evt_cnt: 0,
    funcNum: 2,
    onlyIf: true,
    out_tys: [],
    pay: [v66, []],
    sim_p: (async (txn3) => {
      const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };
      
      const [] = txn3.data;
      const v73 = txn3.time;
      const v74 = txn3.secs;
      const v72 = txn3.from;
      
      sim_r.txns.push({
        amt: v66,
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
  const [] = txn3.data;
  const v73 = txn3.time;
  const v74 = txn3.secs;
  const v72 = txn3.from;
  ;
  const txn4 = await (ctc.recv({
    evt_cnt: 1,
    funcNum: 3,
    out_tys: [ctc2],
    timeoutAt: undefined,
    waitIfNotPresent: false
    }));
  const [v80] = txn4.data;
  const v81 = txn4.time;
  const v82 = txn4.secs;
  const v79 = txn4.from;
  ;
  const v84 = stdlib.addressEq(v65, v79);
  stdlib.assert(v84, {
    at: './index.rsh:27:5:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Bob'
    });
  ;
  stdlib.protect(ctc1, await interact.got(v80), {
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
// compute state in HM_Check 0
int 8
bzero
sha256
load 1
==
assert
// "CheckPay"
// "./index.rsh:13:11:after expr stmt semicolon"
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
bz l1
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
// compute state in HM_Check 1
byte base64(AAAAAAAAAAE=)
sha256
load 1
==
assert
// "CheckPay"
// "./index.rsh:17:5:dot"
// "[]"
// compute state in HM_Set 2
byte base64(AAAAAAAAAAI=)
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
l1:
// Handler 2
dup
int 2
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
// compute state in HM_Set 3
byte base64(AAAAAAAAAAM=)
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
// Handler 3
dup
int 3
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
// compute state in HM_Check 3
byte base64(AAAAAAAAAAM=)
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
                "name": "v66",
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
                "name": "v65",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v66",
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
        "internalType": "struct T5",
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
                "name": "v65",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v66",
                "type": "uint256"
              }
            ],
            "internalType": "struct T2",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint8[128]",
                "name": "v80",
                "type": "uint8[128]"
              }
            ],
            "internalType": "struct T6",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct T7",
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
                "name": "v66",
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
                "name": "v65",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v66",
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
        "internalType": "struct T5",
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
                "name": "v65",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v66",
                "type": "uint256"
              }
            ],
            "internalType": "struct T2",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint8[128]",
                "name": "v80",
                "type": "uint8[128]"
              }
            ],
            "internalType": "struct T6",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "internalType": "struct T7",
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
  Bytecode: `0x60806040526040516106a83803806106a8833981016040819052610022916100fa565b600054610031901560086100bc565b6001600055604080518251151581526020808401511515908201527fee65f2dcf8be6cdab3bac48c73b8a6524225820fd17a0a77f34c81e47ebe406e910160405180910390a1610083341560076100bc565b6040805160016020820152600091810182905260600160408051601f198184030181529190528051602090910120600055506101629050565b816100e15760405163100960cb60e01b81526004810182905260240160405180910390fd5b5050565b805180151581146100f557600080fd5b919050565b60006040828403121561010c57600080fd5b604080519081016001600160401b038111828210171561013c57634e487b7160e01b600052604160045260246000fd5b604052610148836100e5565b8152610156602084016100e5565b60208201529392505050565b610537806101716000396000f3fe6080604052600436106100385760003560e01c80631dc091ad1461004457806329c0fdf314610059578063cc5fe27f1461006c57600080fd5b3661003f57005b600080fd5b6100576100523660046103f4565b61007f565b005b61005761006736600461040c565b61017a565b61005761007a36600461041e565b61026b565b6100d1600161009160208401846103d9565b6040516020016100ad9291909182521515602082015260400190565b6040516020818303038152906040528051906020012060001c60005414600a610362565b60016000556040517f120854c39fdbc847613c8c1a66d23aa6d099c4db8f64d852475191e60a6298d990610106908390610450565b60405180910390a161011a34156009610362565b604080518082018252338152602083810135818301908152835160029281019290925282516001600160a01b03169382019390935291516060830152906080015b60408051601f1981840301815291905280516020909101206000555050565b6040516101b6906101929060029084906020016104ed565b6040516020818303038152906040528051906020012060001c60005414600c610362565b60016000556040517fba9848ba573d919b437f603568b07882c6af6fc804c046ac1cd11132ef3ba165906101eb908390610471565b60405180910390a161020434602083013514600b610362565b604080518082019091526000808252602082015261022560208301836103b7565b6001600160a01b031681526020808301358183015260405161015b9160039184910191825280516001600160a01b03166020808401919091520151604082015260600190565b6040516102a7906102839060039084906020016104ed565b6040516020818303038152906040528051906020012060001c60005414600f610362565b60016000556040517f1744c2be3b3814193b70ec591ef6860fe14b411a9b67c63641dc62b583c4193b906102dc908390610498565b60405180910390a16102f03415600d610362565b6103123361030160208401846103b7565b6001600160a01b031614600e610362565b61031f60208201826103b7565b6040516001600160a01b039190911690602083013580156108fc02916000818181858888f1935050505015801561035a573d6000803e3d6000fd5b506000805533ff5b816103875760405163100960cb60e01b81526004810182905260240160405180910390fd5b5050565b80356001600160a01b03811681146103a257600080fd5b919050565b803580151581146103a257600080fd5b6000602082840312156103c957600080fd5b6103d28261038b565b9392505050565b6000602082840312156103eb57600080fd5b6103d2826103a7565b60006040828403121561040657600080fd5b50919050565b60006060828403121561040657600080fd5b6000611040828403121561040657600080fd5b6001600160a01b036104428261038b565b168252602090810135910152565b6040810161045d836103a7565b151582526020830135602083015292915050565b6060810161047f8284610431565b61048b604084016103a7565b1515604083015292915050565b61104081016104a78284610431565b60408201604084016000805b60808110156104e357823560ff81168082146104cd578384fd5b85525060209384019392909201916001016104b3565b5050505092915050565b828152606081016103d2602083018461043156fea264697066735822122063697fdeb3e46d5234e7aa2213a478d75367e77620a4877ab035f6554a15c5ce64736f6c63430008070033`,
  BytecodeLen: 1704,
  Which: `oD`,
  deployMode: `DM_constructor`,
  version: 2,
  views: {
    }
  };

export const _Connectors = {
  ALGO: _ALGO,
  ETH: _ETH
  };

