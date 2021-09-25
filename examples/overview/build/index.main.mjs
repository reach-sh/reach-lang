// Automatically generated with Reach 0.1.5
/* eslint-disable */
export const _version = '0.1.5';
export const _backendVersion = 3;


export function getExports(s) {
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
    lct: stdlib.checkedBigNumberify('./index.rsh:17:5:dot', stdlib.UInt_max, 0),
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
    lct: v65,
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
    lct: v59,
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
substring 0 8
btoi
store 1
dup
substring 8 16
btoi
store 2
substring 16 48
store 3
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
// check step
int 0
load 1
==
assert
// check time
txna ApplicationArgs 1
btoi
load 2
==
assert
byte base64()
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
store 3
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
load 3
dig 1
gtxns Receiver
==
assert
l1:
pop
// "CheckPay"
// "./index.rsh:17:5:dot"
// "[]"
txn Sender
load 254
itob
concat
int 1
bzero
dig 1
substring 0 40
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
l0:
// Handler 1
dup
int 1
==
bz l2
pop
// check step
int 1
load 1
==
assert
// check time
txna ApplicationArgs 1
btoi
load 2
==
assert
int 1
bzero
app_global_get
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
load 3
dig 1
gtxns Receiver
==
assert
l3:
pop
load 255
load 254
itob
concat
int 1
bzero
dig 1
substring 0 40
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
l2:
// Handler 2
dup
int 2
==
bz l4
pop
// check step
int 2
load 1
==
assert
// check time
txna ApplicationArgs 1
btoi
load 2
==
assert
int 1
bzero
app_global_get
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
l6:
pop
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
itob
load 2
itob
load 3
concat
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
global ZeroAddress
store 3
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
  stateKeys: 1,
  stateSize: 40,
  unsupported: [],
  version: 4
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
                "name": "v58",
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
                "name": "v58",
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
    "name": "e0",
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
    "name": "e1",
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
            "internalType": "uint256",
            "name": "time",
            "type": "uint256"
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
  Bytecode: `0x608060405260405161087938038061087983398101604081905261002291610195565b60008055604080518251815260208084015151908201527fcbe8b01c100825cba852556e4d2f014b5e636208419a4c3d83f7ef63111ab885910160405180910390a1610070341560076100d3565b6040805180820182526000602080830182815233808552868301515182526001938490554390935584518083019390935251828501528351808303850181526060909201909352805191926100cb92600292909101906100fc565b50505061026e565b816100f85760405163100960cb60e01b81526004810182905260240160405180910390fd5b5050565b82805461010890610233565b90600052602060002090601f01602090048101928261012a5760008555610170565b82601f1061014357805160ff1916838001178555610170565b82800160010185558215610170579182015b82811115610170578251825591602001919060010190610155565b5061017c929150610180565b5090565b5b8082111561017c5760008155600101610181565b60008183036040808212156101a957600080fd5b80518082016001600160401b0380821183831017156101d857634e487b7160e01b600052604160045260246000fd5b818452865183526020601f19860112156101f157600080fd5b83519450602085019150848210818311171561021d57634e487b7160e01b600052604160045260246000fd5b5090915260209384015182529283015250919050565b600181811c9082168061024757607f821691505b6020821081141561026857634e487b7160e01b600052602260045260246000fd5b50919050565b6105fc8061027d6000396000f3fe6080604052600436106100385760003560e01c80633029bd57146100445780637963168e14610059578063832307571461006c57600080fd5b3661003f57005b600080fd5b610057610052366004610507565b61008e565b005b6100576100673660046104ef565b610206565b34801561007857600080fd5b5060015460405190815260200160405180910390f35b61009e600260005414600d61037e565b6001546100af90823514600e61037e565b6000808055600280546100c190610591565b80601f01602080910402602001604051908101604052809291908181526020018280546100ed90610591565b801561013a5780601f1061010f5761010080835404028352916020019161013a565b820191906000526020600020905b81548152906001019060200180831161011d57829003601f168201915b5050505050806020019051810190610152919061047d565b90507f6684f814e2616a68bb64e11e05af1bab6e72e48d2a71fad84c56974257f0d420826040516101839190610544565b60405180910390a16101973415600b61037e565b80516101af906001600160a01b03163314600c61037e565b805160208201516040516001600160a01b039092169181156108fc0291906000818181858888f193505050501580156101ec573d6000803e3d6000fd5b5060008080556001819055610203906002906103a7565b33ff5b610216600160005414600961037e565b60015461022790823514600a61037e565b60008080556002805461023990610591565b80601f016020809104026020016040519081016040528092919081815260200182805461026590610591565b80156102b25780601f10610287576101008083540402835291602001916102b2565b820191906000526020600020905b81548152906001019060200180831161029557829003601f168201915b50505050508060200190518101906102ca919061047d565b90507f9f41c6cf17ede288cbb2cfbbafdd05b2b2025dea3b047cdb79dbc892d7a9286d826040516102fb919061051a565b60405180910390a161031481602001513414600861037e565b6040805180820182526000808252602080830182815285516001600160a01b031680855286830151825260029384905543600155855180840191909152905181860152845180820386018152606090910190945283519293610378939101906103e4565b50505050565b816103a35760405163100960cb60e01b81526004810182905260240160405180910390fd5b5050565b5080546103b390610591565b6000825580601f106103c3575050565b601f0160209004906000526020600020908101906103e19190610468565b50565b8280546103f090610591565b90600052602060002090601f0160209004810192826104125760008555610458565b82601f1061042b57805160ff1916838001178555610458565b82800160010185558215610458579182015b8281111561045857825182559160200191906001019061043d565b50610464929150610468565b5090565b5b808211156104645760008155600101610469565b60006040828403121561048f57600080fd5b6040516040810181811067ffffffffffffffff821117156104c057634e487b7160e01b600052604160045260246000fd5b60405282516001600160a01b03811681146104da57600080fd5b81526020928301519281019290925250919050565b60006040828403121561050157600080fd5b50919050565b6000611020828403121561050157600080fd5b8135815260408101602083013580151580821461053657600080fd5b806020850152505092915050565b81358152611020810160208083018185016000805b608081101561058657823560ff8116808214610573578384fd5b8552509284019291840191600101610559565b505050505092915050565b600181811c908216806105a557607f821691505b6020821081141561050157634e487b7160e01b600052602260045260246000fdfea26469706673582212200dd956334212a28b559587a0d09bd43b613ca92f46abb1aa5ee028581a38e39164736f6c63430008070033`,
  BytecodeLen: 2169,
  Which: `oD`,
  version: 3,
  views: {
    }
  };

export const _Connectors = {
  ALGO: _ALGO,
  ETH: _ETH
  };

