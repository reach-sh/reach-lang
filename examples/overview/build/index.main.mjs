// Automatically generated with Reach 0.1.7 (ee2b5f5d*)
/* eslint-disable */
export const _version = '0.1.7';
export const _versionHash = '0.1.7 (ee2b5f5d*)';
export const _backendVersion = 7;

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
  
  return {
    infos: {
      },
    views: {
      1: [ctc0, ctc1],
      2: [ctc0, ctc1]
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
  const ctc0 = stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, 128));
  const ctc1 = stdlib.T_UInt;
  const ctc2 = stdlib.T_Address;
  
  
  const v60 = stdlib.protect(ctc0, interact.info, 'for Alice\'s interact field info');
  const v61 = stdlib.protect(ctc1, interact.request, 'for Alice\'s interact field request');
  
  const txn1 = await (ctc.sendrecv({
    args: [v61],
    evt_cnt: 1,
    funcNum: 0,
    lct: stdlib.checkedBigNumberify('./index.rsh:17:5:dot', stdlib.UInt_max, 0),
    onlyIf: true,
    out_tys: [ctc1],
    pay: [stdlib.checkedBigNumberify('./index.rsh:17:5:decimal', stdlib.UInt_max, 0), []],
    sim_p: (async (txn1) => {
      const sim_r = { txns: [], mapRefs: [], maps: [] };
      
      const {data: [v65], secs: v67, time: v66, didSend: v29, from: v64 } = txn1;
      
      sim_r.txns.push({
        amt: stdlib.checkedBigNumberify('./index.rsh:17:5:decimal', stdlib.UInt_max, 0),
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
  const {data: [v65], secs: v67, time: v66, didSend: v29, from: v64 } = txn1;
  ;
  const txn2 = await (ctc.recv({
    didSend: false,
    evt_cnt: 0,
    funcNum: 1,
    out_tys: [],
    timeoutAt: undefined,
    waitIfNotPresent: false
    }));
  const {data: [], secs: v73, time: v72, didSend: v38, from: v71 } = txn2;
  ;
  const txn3 = await (ctc.sendrecv({
    args: [v64, v65, v60],
    evt_cnt: 1,
    funcNum: 2,
    lct: v72,
    onlyIf: true,
    out_tys: [ctc0],
    pay: [stdlib.checkedBigNumberify('./index.rsh:27:5:decimal', stdlib.UInt_max, 0), []],
    sim_p: (async (txn3) => {
      const sim_r = { txns: [], mapRefs: [], maps: [] };
      
      const {data: [v79], secs: v81, time: v80, didSend: v48, from: v78 } = txn3;
      
      sim_r.txns.push({
        amt: stdlib.checkedBigNumberify('./index.rsh:27:5:decimal', stdlib.UInt_max, 0),
        kind: 'to',
        tok: undefined
        });
      const v83 = stdlib.addressEq(v64, v78);
      stdlib.assert(v83, {
        at: './index.rsh:27:5:dot',
        fs: [],
        msg: 'sender correct',
        who: 'Alice'
        });
      sim_r.txns.push({
        amt: v65,
        kind: 'from',
        to: v64,
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
  const {data: [v79], secs: v81, time: v80, didSend: v48, from: v78 } = txn3;
  ;
  const v83 = stdlib.addressEq(v64, v78);
  stdlib.assert(v83, {
    at: './index.rsh:27:5:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Alice'
    });
  ;
  return;
  
  
  
  
  
  
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
  const ctc2 = stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, 128));
  const ctc3 = stdlib.T_Address;
  
  
  const txn1 = await (ctc.recv({
    didSend: false,
    evt_cnt: 1,
    funcNum: 0,
    out_tys: [ctc0],
    timeoutAt: undefined,
    waitIfNotPresent: false
    }));
  const {data: [v65], secs: v67, time: v66, didSend: v29, from: v64 } = txn1;
  ;
  stdlib.protect(ctc1, await interact.want(v65), {
    at: './index.rsh:21:18:application',
    fs: ['at ./index.rsh:20:9:application call to [unknown function] (defined at: ./index.rsh:20:13:function exp)'],
    msg: 'want',
    who: 'Bob'
    });
  
  const txn2 = await (ctc.sendrecv({
    args: [v64, v65],
    evt_cnt: 0,
    funcNum: 1,
    lct: v66,
    onlyIf: true,
    out_tys: [],
    pay: [v65, []],
    sim_p: (async (txn2) => {
      const sim_r = { txns: [], mapRefs: [], maps: [] };
      
      const {data: [], secs: v73, time: v72, didSend: v38, from: v71 } = txn2;
      
      sim_r.txns.push({
        amt: v65,
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
  const {data: [], secs: v73, time: v72, didSend: v38, from: v71 } = txn2;
  ;
  const txn3 = await (ctc.recv({
    didSend: false,
    evt_cnt: 1,
    funcNum: 2,
    out_tys: [ctc2],
    timeoutAt: undefined,
    waitIfNotPresent: false
    }));
  const {data: [v79], secs: v81, time: v80, didSend: v48, from: v78 } = txn3;
  ;
  const v83 = stdlib.addressEq(v64, v78);
  stdlib.assert(v83, {
    at: './index.rsh:27:5:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Bob'
    });
  ;
  stdlib.protect(ctc1, await interact.got(v79), {
    at: './index.rsh:32:17:application',
    fs: ['at ./index.rsh:31:9:application call to [unknown function] (defined at: ./index.rsh:31:13:function exp)'],
    msg: 'got',
    who: 'Bob'
    });
  
  return;
  
  
  
  
  
  
  };
const _ALGO = {
  appApproval: `#pragma version 5
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
int 0
extract_uint64
store 1
int 8
extract_uint64
store 2
txn NumAppArgs
int 3
==
assert
txna ApplicationArgs 0
btoi
preamble:
// Handler 0
dup
int 0
==
bz l0_afterHandler0
pop
// check step
int 0
load 1
==
assert
// check time
txna ApplicationArgs 1
btoi
dup
int 0
==
swap
load 2
==
||
assert
byte base64()
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
byte base64(ODJjNA==)
load 255
itob
concat
log // 8
// "CheckPay"
// "./index.rsh:17:5:dot"
// "[]"
int 100000
dup
bz l1_checkTxnK
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
global CurrentApplicationAddress
dig 1
gtxns Receiver
==
assert
l1_checkTxnK:
pop
// "CheckPay"
// "./index.rsh:17:5:dot"
// "[]"
txn Sender
load 255
itob
concat
int 1
bzero
dig 1
extract 0 40
app_global_put
pop
int 1
store 1
global Round
store 2
txn OnCompletion
int NoOp
==
assert
b updateState
l0_afterHandler0:
// Handler 1
dup
int 1
==
bz l2_afterHandler1
pop
// check step
int 1
load 1
==
assert
// check time
txna ApplicationArgs 1
btoi
dup
int 0
==
swap
load 2
==
||
assert
int 1
bzero
app_global_get
dup
extract 0 32
store 255
dup
int 32
extract_uint64
store 254
pop
txna ApplicationArgs 2
dup
len
int 0
==
assert
pop
byte base64(OWE4Yg==)
log // 0
// "CheckPay"
// "./index.rsh:22:5:dot"
// "[]"
load 254
dup
bz l3_checkTxnK
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
global CurrentApplicationAddress
dig 1
gtxns Receiver
==
assert
l3_checkTxnK:
pop
load 255
load 254
itob
concat
int 1
bzero
dig 1
extract 0 40
app_global_put
pop
int 2
store 1
global Round
store 2
txn OnCompletion
int NoOp
==
assert
b updateState
l2_afterHandler1:
// Handler 2
dup
int 2
==
bz l4_afterHandler2
pop
// check step
int 2
load 1
==
assert
// check time
txna ApplicationArgs 1
btoi
dup
int 0
==
swap
load 2
==
||
assert
int 1
bzero
app_global_get
dup
extract 0 32
store 255
dup
int 32
extract_uint64
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
byte base64(NWU5NQ==)
load 253
concat
log // 128
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
bz l5_makeTxnK
itxn_begin
itxn_field Amount
int pay
itxn_field TypeEnum
load 255
itxn_field Receiver
itxn_submit
int 0
l5_makeTxnK:
pop
int 0
itxn_begin
itxn_field Amount
int pay
itxn_field TypeEnum
global CreatorAddress
itxn_field CloseRemainderTo
global CurrentApplicationAddress
itxn_field Receiver
itxn_submit
int 0
l6_makeTxnK:
pop
txn OnCompletion
int DeleteApplication
==
assert
b updateState
l4_afterHandler2:
int 0
assert
updateState:
byte base64()
load 1
itob
load 2
itob
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
int 0
store 1
int 0
store 2
b updateState
`,
  appClear: `#pragma version 5
int 0
`,
  mapDataKeys: 0,
  mapDataSize: 0,
  stateKeys: 1,
  stateSize: 40,
  unsupported: [],
  version: 7
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
                "name": "v65",
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
                "name": "v65",
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
        "internalType": "struct T4",
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
            "components": [
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
                  },
                  {
                    "internalType": "bytes32",
                    "name": "elem3",
                    "type": "bytes32"
                  }
                ],
                "internalType": "struct T5",
                "name": "v79",
                "type": "tuple"
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
    "name": "_reach_e2",
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
        "internalType": "struct T4",
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
            "components": [
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
                  },
                  {
                    "internalType": "bytes32",
                    "name": "elem3",
                    "type": "bytes32"
                  }
                ],
                "internalType": "struct T5",
                "name": "v79",
                "type": "tuple"
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
    "name": "_reach_m2",
    "outputs": [],
    "stateMutability": "payable",
    "type": "function"
  },
  {
    "stateMutability": "payable",
    "type": "receive"
  }
]`,
  Bytecode: `0x60806040526040516109a53803806109a583398101604081905261002291610199565b6000805543600355604080518251815260208084015151908201527fd2fa1fac07e66d08cb46271d2f3a2585d9761d2d591a51cc30a2056942bc38db910160405180910390a1610074341560076100d7565b6040805180820182526000602080830182815233808552868301515182526001938490554390935584518083019390935251828501528351808303850181526060909201909352805191926100cf9260029290910190610100565b505050610272565b816100fc5760405163100960cb60e01b81526004810182905260240160405180910390fd5b5050565b82805461010c90610237565b90600052602060002090601f01602090048101928261012e5760008555610174565b82601f1061014757805160ff1916838001178555610174565b82800160010185558215610174579182015b82811115610174578251825591602001919060010190610159565b50610180929150610184565b5090565b5b808211156101805760008155600101610185565b60008183036040808212156101ad57600080fd5b80518082016001600160401b0380821183831017156101dc57634e487b7160e01b600052604160045260246000fd5b818452865183526020601f19860112156101f557600080fd5b83519450602085019150848210818311171561022157634e487b7160e01b600052604160045260246000fd5b5090915260209384015182529283015250919050565b600181811c9082168061024b57607f821691505b6020821081141561026c57634e487b7160e01b600052602260045260246000fd5b50919050565b610724806102816000396000f3fe60806040526004361061004b5760003560e01c80631e93b0f1146100545780632c10a15914610078578063832307571461008b5780638dc34f55146100a0578063ab53f2c6146100b357005b3661005257005b005b34801561006057600080fd5b506003545b6040519081526020015b60405180910390f35b610052610086366004610596565b6100d6565b34801561009757600080fd5b50600154610065565b6100526100ae3660046105ae565b610257565b3480156100bf57600080fd5b506100c86103fe565b60405161006f9291906105c0565b6100e6600160005414600961049b565b610100813515806100f957506001548235145b600a61049b565b6000808055600280546101129061061d565b80601f016020809104026020016040519081016040528092919081815260200182805461013e9061061d565b801561018b5780601f106101605761010080835404028352916020019161018b565b820191906000526020600020905b81548152906001019060200180831161016e57829003601f168201915b50505050508060200190518101906101a39190610652565b90507f79ca1a789d797004bc78dff9632d64e202e102f2d008dcc20c5a645ef7d4a7d1826040516101d491906106c4565b60405180910390a16101ed81602001513414600861049b565b6040805180820182526000808252602080830182815285516001600160a01b031680855286830151825260029384905543600155855180840191909152905181860152845180820386018152606090910190945283519293610251939101906104c0565b50505050565b610267600260005414600d61049b565b6102818135158061027a57506001548235145b600e61049b565b6000808055600280546102939061061d565b80601f01602080910402602001604051908101604052809291908181526020018280546102bf9061061d565b801561030c5780601f106102e15761010080835404028352916020019161030c565b820191906000526020600020905b8154815290600101906020018083116102ef57829003601f168201915b50505050508060200190518101906103249190610652565b6040805184358152602080860135908201528482013581830152606080860135908201526080808601359082015290519192507f9bc7f45cbc6b90af1c8b5451d19edb456532b360cd108d3bf38702d65dfe51fb919081900360a00190a161038e3415600b61049b565b80516103a6906001600160a01b03163314600c61049b565b805160208201516040516001600160a01b039092169181156108fc0291906000818181858888f193505050501580156103e3573d6000803e3d6000fd5b50600080805560018190556103fa90600290610544565b5050565b6000606060005460028080546104139061061d565b80601f016020809104026020016040519081016040528092919081815260200182805461043f9061061d565b801561048c5780601f106104615761010080835404028352916020019161048c565b820191906000526020600020905b81548152906001019060200180831161046f57829003601f168201915b50505050509050915091509091565b816103fa5760405163100960cb60e01b81526004810182905260240160405180910390fd5b8280546104cc9061061d565b90600052602060002090601f0160209004810192826104ee5760008555610534565b82601f1061050757805160ff1916838001178555610534565b82800160010185558215610534579182015b82811115610534578251825591602001919060010190610519565b50610540929150610581565b5090565b5080546105509061061d565b6000825580601f10610560575050565b601f01602090049060005260206000209081019061057e9190610581565b50565b5b808211156105405760008155600101610582565b6000604082840312156105a857600080fd5b50919050565b600060a082840312156105a857600080fd5b82815260006020604081840152835180604085015260005b818110156105f4578581018301518582016060015282016105d8565b81811115610606576000606083870101525b50601f01601f191692909201606001949350505050565b600181811c9082168061063157607f821691505b602082108114156105a857634e487b7160e01b600052602260045260246000fd5b60006040828403121561066457600080fd5b6040516040810181811067ffffffffffffffff8211171561069557634e487b7160e01b600052604160045260246000fd5b60405282516001600160a01b03811681146106af57600080fd5b81526020928301519281019290925250919050565b813581526040810160208301358015158082146106e057600080fd5b80602085015250509291505056fea264697066735822122006c30e850d073d613f7786ff4c6f7d2d0b012ba6f6a98ae801f1449ad95dabc864736f6c634300080a0033`,
  BytecodeLen: 2469,
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
