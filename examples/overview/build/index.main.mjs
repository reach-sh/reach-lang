// Automatically generated with Reach 0.1.2
/* eslint-disable no-unused-vars, no-empty-pattern, no-useless-escape, no-loop-func */
export const _version = '0.1.2';


export async function Alice(ctc, interact) {
  const stdlib = ctc.stdlib;
  const v0 = await ctc.creationTime();
  
  const txn1 = await (ctc.sendrecv('Alice', 1, 1, stdlib.checkedBigNumberify('./index.rsh:13:9:dot', stdlib.UInt_max, 0), [stdlib.T_UInt, stdlib.T_UInt], [v0, stdlib.protect(stdlib.T_UInt, interact.request, null)], stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), [stdlib.T_UInt], true, true, false, ((txn1) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:13:9:dot', stdlib.UInt_max, 0), v0]);
    sim_r.prevSt_noPrevTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:13:9:dot', stdlib.UInt_max, 0)]);
    const [v4] = txn1.data;
    const v5 = txn1.value;
    const v11 = txn1.time;
    const v3 = txn1.from;
    
    stdlib.assert(true, {
      at: './index.rsh:13:9:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Alice'
       });
    const v6 = stdlib.eq(v5, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
    stdlib.assert(v6, {
      at: './index.rsh:13:9:dot',
      fs: [],
      msg: 'pay amount correct',
      who: 'Alice'
       });
    
    
    sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address]), [stdlib.checkedBigNumberify('./index.rsh:14:15:after expr stmt semicolon', stdlib.UInt_max, 1), v11, v4, v3]);
    sim_r.nextSt_noTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address]), [stdlib.checkedBigNumberify('./index.rsh:14:15:after expr stmt semicolon', stdlib.UInt_max, 1), v4, v3]);
    sim_r.isHalt = false;
    
    return sim_r;
     })));
  const [v4] = txn1.data;
  const v5 = txn1.value;
  const v11 = txn1.time;
  const v3 = txn1.from;
  stdlib.assert(true, {
    at: './index.rsh:13:9:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Alice'
     });
  const v6 = stdlib.eq(v5, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
  stdlib.assert(v6, {
    at: './index.rsh:13:9:dot',
    fs: [],
    msg: 'pay amount correct',
    who: 'Alice'
     });
  
  
  const txn2 = await (ctc.recv('Alice', 2, 0, [], false, false));
  const [] = txn2.data;
  const v16 = txn2.value;
  const v22 = txn2.time;
  const v15 = txn2.from;
  stdlib.assert(true, {
    at: './index.rsh:18:9:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Alice'
     });
  const v17 = stdlib.eq(v16, v4);
  stdlib.assert(v17, {
    at: './index.rsh:18:9:dot',
    fs: [],
    msg: 'pay amount correct',
    who: 'Alice'
     });
  
  
  const txn3 = await (ctc.sendrecv('Alice', 3, 1, stdlib.checkedBigNumberify('./index.rsh:23:9:dot', stdlib.UInt_max, 2), [stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, 128))], [v4, v3, v22, stdlib.protect(stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, 128)), interact.info, null)], stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), [stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, 128))], true, true, false, ((txn3) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:23:9:dot', stdlib.UInt_max, 2), v4, v3, v22]);
    sim_r.prevSt_noPrevTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address]), [stdlib.checkedBigNumberify('./index.rsh:23:9:dot', stdlib.UInt_max, 2), v4, v3]);
    const [v26] = txn3.data;
    const v27 = txn3.value;
    const v34 = txn3.time;
    const v25 = txn3.from;
    
    const v29 = stdlib.addressEq(v3, v25);
    stdlib.assert(v29, {
      at: './index.rsh:23:9:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Alice'
       });
    const v28 = stdlib.eq(v27, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
    stdlib.assert(v28, {
      at: './index.rsh:23:9:dot',
      fs: [],
      msg: 'pay amount correct',
      who: 'Alice'
       });
    
    
    
    
    
    sim_r.txns.push({
      amt: v4,
      to: v3
       });
    
    
    
    
    
    sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([]), []);
    sim_r.nextSt_noTime = stdlib.digest(stdlib.T_Tuple([]), []);
    sim_r.isHalt = true;
    
    return sim_r;
     })));
  const [v26] = txn3.data;
  const v27 = txn3.value;
  const v34 = txn3.time;
  const v25 = txn3.from;
  const v29 = stdlib.addressEq(v3, v25);
  stdlib.assert(v29, {
    at: './index.rsh:23:9:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Alice'
     });
  const v28 = stdlib.eq(v27, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
  stdlib.assert(v28, {
    at: './index.rsh:23:9:dot',
    fs: [],
    msg: 'pay amount correct',
    who: 'Alice'
     });
  
  
  
  
  
  ;
  
  
  
  
  
  return;
  
  
  
   }
export async function Bob(ctc, interact) {
  const stdlib = ctc.stdlib;
  const v0 = await ctc.creationTime();
  const txn1 = await (ctc.recv('Bob', 1, 1, [stdlib.T_UInt], false, false));
  const [v4] = txn1.data;
  const v5 = txn1.value;
  const v11 = txn1.time;
  const v3 = txn1.from;
  stdlib.assert(true, {
    at: './index.rsh:13:9:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Bob'
     });
  const v6 = stdlib.eq(v5, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
  stdlib.assert(v6, {
    at: './index.rsh:13:9:dot',
    fs: [],
    msg: 'pay amount correct',
    who: 'Bob'
     });
  
  
  
  stdlib.protect(stdlib.T_Null, await interact.want(v4), {
    at: './index.rsh:17:22:application',
    fs: ['at ./index.rsh:17:35:after expr stmt semicolon call to [unknown function] (defined at: ./index.rsh:16:17:function exp)'],
    msg: 'want',
    who: 'Bob'
     });
  const txn2 = await (ctc.sendrecv('Bob', 2, 0, stdlib.checkedBigNumberify('./index.rsh:18:9:dot', stdlib.UInt_max, 0), [stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address], [v11, v4, v3], v4, [], true, true, false, ((txn2) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address]), [stdlib.checkedBigNumberify('./index.rsh:18:9:dot', stdlib.UInt_max, 1), v11, v4, v3]);
    sim_r.prevSt_noPrevTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address]), [stdlib.checkedBigNumberify('./index.rsh:18:9:dot', stdlib.UInt_max, 1), v4, v3]);
    const [] = txn2.data;
    const v16 = txn2.value;
    const v22 = txn2.time;
    const v15 = txn2.from;
    
    stdlib.assert(true, {
      at: './index.rsh:18:9:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Bob'
       });
    const v17 = stdlib.eq(v16, v4);
    stdlib.assert(v17, {
      at: './index.rsh:18:9:dot',
      fs: [],
      msg: 'pay amount correct',
      who: 'Bob'
       });
    
    
    sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:19:15:after expr stmt semicolon', stdlib.UInt_max, 2), v4, v3, v22]);
    sim_r.nextSt_noTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address]), [stdlib.checkedBigNumberify('./index.rsh:19:15:after expr stmt semicolon', stdlib.UInt_max, 2), v4, v3]);
    sim_r.isHalt = false;
    
    return sim_r;
     })));
  const [] = txn2.data;
  const v16 = txn2.value;
  const v22 = txn2.time;
  const v15 = txn2.from;
  stdlib.assert(true, {
    at: './index.rsh:18:9:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Bob'
     });
  const v17 = stdlib.eq(v16, v4);
  stdlib.assert(v17, {
    at: './index.rsh:18:9:dot',
    fs: [],
    msg: 'pay amount correct',
    who: 'Bob'
     });
  
  
  const txn3 = await (ctc.recv('Bob', 3, 1, [stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, 128))], false, false));
  const [v26] = txn3.data;
  const v27 = txn3.value;
  const v34 = txn3.time;
  const v25 = txn3.from;
  const v29 = stdlib.addressEq(v3, v25);
  stdlib.assert(v29, {
    at: './index.rsh:23:9:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Bob'
     });
  const v28 = stdlib.eq(v27, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
  stdlib.assert(v28, {
    at: './index.rsh:23:9:dot',
    fs: [],
    msg: 'pay amount correct',
    who: 'Bob'
     });
  
  
  
  
  
  ;
  
  
  
  
  
  stdlib.protect(stdlib.T_Null, await interact.got(v26), {
    at: './index.rsh:28:21:application',
    fs: ['at ./index.rsh:28:31:after expr stmt semicolon call to [unknown function] (defined at: ./index.rsh:27:17:function exp)'],
    msg: 'got',
    who: 'Bob'
     });
  return;
  
  
  
   }

const _ALGO = {
  appApproval: `#pragma version 2
// Check that we're an App
txn TypeEnum
int appl
==
assert
txn RekeyTo
global ZeroAddress
==
assert
// Check that everyone's here
global GroupSize
int 4
>=
assert
// Check txnAppl (us)
txn GroupIndex
int 0
==
assert
// Check txnFromHandler
int 0
gtxn 2 Sender
byte "{{m1}}"
==
||
gtxn 2 Sender
byte "{{m2}}"
==
||
gtxn 2 Sender
byte "{{m3}}"
==
||
assert
byte base64(cw==)
app_global_get
gtxna 2 Args 0
==
assert
byte base64(bA==)
app_global_get
gtxna 2 Args 4
btoi
==
assert
// Don't check anyone else, because Handler does
// Update state
byte base64(cw==)
gtxna 2 Args 1
app_global_put
byte base64(bA==)
global Round
app_global_put
byte base64(aA==)
gtxna 2 Args 2
btoi
app_global_put
byte base64(aA==)
app_global_get
bnz halted
txn OnCompletion
int NoOp
==
assert
b done
halted:
txn OnCompletion
int DeleteApplication
==
assert
done:
int 1
return
`,
  appApproval0: `#pragma version 2
// Check that we're an App
txn TypeEnum
int appl
==
assert
txn RekeyTo
global ZeroAddress
==
assert
txn Sender
byte "{{Deployer}}"
==
assert
txn ApplicationID
bz init
global GroupSize
int 5
==
assert
txn OnCompletion
int UpdateApplication
==
assert
byte base64(cw==)
// compute state in HM_Set
int 0
itob
keccak256
app_global_put
byte base64(bA==)
global Round
app_global_put
byte base64(aA==)
int 0
app_global_put
b done
init:
global GroupSize
int 1
==
assert
txn OnCompletion
int NoOp
==
assert
done:
int 1
return
`,
  appClear: `#pragma version 2
// We're alone
global GroupSize
int 1
==
assert
// We're halted
byte base64(aA==)
app_global_get
int 1
==
assert
done:
int 1
return
`,
  ctc: `#pragma version 2
// Check size
global GroupSize
int 4
>=
assert
// Check txnAppl
gtxn 0 TypeEnum
int appl
==
assert
gtxn 0 ApplicationID
byte "{{ApplicationID}}"
btoi
==
assert
// Don't check anything else, because app does
// Check us
txn TypeEnum
int pay
==
assert
txn RekeyTo
global ZeroAddress
==
assert
txn CloseRemainderTo
global ZeroAddress
==
assert
txn GroupIndex
int 4
>=
assert
done:
int 1
return
`,
  stepargs: [0, 89, 121, 249],
  steps: [null, `#pragma version 2
// Handler 1
// Check txnAppl
gtxn 0 TypeEnum
int appl
==
assert
gtxn 0 ApplicationID
byte "{{ApplicationID}}"
btoi
==
assert
// Check txnToHandler
gtxn 1 TypeEnum
int pay
==
assert
gtxn 1 Receiver
txn Sender
==
assert
gtxn 1 Amount
gtxn 2 Fee
==
assert
// Check txnToContract
gtxn 3 TypeEnum
int pay
==
assert
gtxn 3 Receiver
byte "{{ContractAddr}}"
==
assert
// Check txnFromHandler (us)
txn GroupIndex
int 2
==
assert
txn TypeEnum
int pay
==
assert
txn Amount
int 0
==
assert
txn Receiver
gtxn 1 Sender
==
assert
txn NumArgs
int 6
==
assert
// compute state in HM_Check 0
int 0
itob
keccak256
arg 0
==
assert
// Run body
// Just "sender correct"
// "./index.rsh:13:9:dot"
// "[]"
int 1
assert
// Just "pay amount correct"
// "./index.rsh:13:9:dot"
// "[]"
gtxn 3 Amount
arg 3
btoi
-
int 0
==
assert
// compute state in HM_Set
int 1
itob
arg 5
concat
gtxn 3 Sender
concat
keccak256
arg 1
==
assert
arg 2
btoi
int 0
==
assert
b done
// Check GroupSize
global GroupSize
int 4
==
assert
arg 3
btoi
int 0
==
assert
// Check time limits
done:
int 1
return
`, `#pragma version 2
// Handler 2
// Check txnAppl
gtxn 0 TypeEnum
int appl
==
assert
gtxn 0 ApplicationID
byte "{{ApplicationID}}"
btoi
==
assert
// Check txnToHandler
gtxn 1 TypeEnum
int pay
==
assert
gtxn 1 Receiver
txn Sender
==
assert
gtxn 1 Amount
gtxn 2 Fee
==
assert
// Check txnToContract
gtxn 3 TypeEnum
int pay
==
assert
gtxn 3 Receiver
byte "{{ContractAddr}}"
==
assert
// Check txnFromHandler (us)
txn GroupIndex
int 2
==
assert
txn TypeEnum
int pay
==
assert
txn Amount
int 0
==
assert
txn Receiver
gtxn 1 Sender
==
assert
txn NumArgs
int 7
==
assert
// compute state in HM_Check 1
int 1
itob
arg 5
concat
arg 6
concat
keccak256
arg 0
==
assert
// Run body
// Just "sender correct"
// "./index.rsh:18:9:dot"
// "[]"
int 1
assert
// Just "pay amount correct"
// "./index.rsh:18:9:dot"
// "[]"
gtxn 3 Amount
arg 3
btoi
-
arg 5
btoi
==
assert
// compute state in HM_Set
int 2
itob
arg 5
concat
arg 6
concat
keccak256
arg 1
==
assert
arg 2
btoi
int 0
==
assert
b done
// Check GroupSize
global GroupSize
int 4
==
assert
arg 3
btoi
int 0
==
assert
// Check time limits
done:
int 1
return
`, `#pragma version 2
// Handler 3
// Check txnAppl
gtxn 0 TypeEnum
int appl
==
assert
gtxn 0 ApplicationID
byte "{{ApplicationID}}"
btoi
==
assert
// Check txnToHandler
gtxn 1 TypeEnum
int pay
==
assert
gtxn 1 Receiver
txn Sender
==
assert
gtxn 1 Amount
gtxn 2 Fee
==
assert
// Check txnToContract
gtxn 3 TypeEnum
int pay
==
assert
gtxn 3 Receiver
byte "{{ContractAddr}}"
==
assert
// Check txnFromHandler (us)
txn GroupIndex
int 2
==
assert
txn TypeEnum
int pay
==
assert
txn Amount
int 0
==
assert
txn Receiver
gtxn 1 Sender
==
assert
txn NumArgs
int 8
==
assert
// compute state in HM_Check 2
int 2
itob
arg 5
concat
arg 6
concat
keccak256
arg 0
==
assert
// Run body
// Just "sender correct"
// "./index.rsh:23:9:dot"
// "[]"
arg 6
gtxn 3 Sender
==
assert
// Just "pay amount correct"
// "./index.rsh:23:9:dot"
// "[]"
gtxn 3 Amount
arg 3
btoi
-
int 0
==
assert
gtxn 4 TypeEnum
int pay
==
assert
gtxn 4 Receiver
arg 6
==
assert
gtxn 4 Amount
arg 5
btoi
==
assert
gtxn 4 Sender
byte "{{ContractAddr}}"
==
assert
arg 2
btoi
int 1
==
assert
b done
// Check GroupSize
global GroupSize
int 5
==
assert
arg 3
btoi
gtxn 4 Fee
==
assert
// Check time limits
done:
int 1
return
`],
  unsupported: false
   };
const _ETH = {
  ABI: `[
  {
    "inputs": [],
    "stateMutability": "payable",
    "type": "constructor"
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
                "name": "v0",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a0postsvs",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v4",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a1msg",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct ReachContract.a1",
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
                "internalType": "uint256",
                "name": "v11",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v4",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v3",
                "type": "address"
              }
            ],
            "internalType": "struct ReachContract.a1postsvs",
            "name": "svs",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct ReachContract.a2",
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
                "internalType": "uint256",
                "name": "v4",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v3",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v22",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a2postsvs",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint8[128]",
                "name": "v26",
                "type": "uint8[128]"
              }
            ],
            "internalType": "struct ReachContract.a3msg",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct ReachContract.a3",
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
                "name": "v0",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a0postsvs",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v4",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a1msg",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "internalType": "struct ReachContract.a1",
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
                "internalType": "uint256",
                "name": "v11",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v4",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v3",
                "type": "address"
              }
            ],
            "internalType": "struct ReachContract.a1postsvs",
            "name": "svs",
            "type": "tuple"
          }
        ],
        "internalType": "struct ReachContract.a2",
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
                "internalType": "uint256",
                "name": "v4",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v3",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v22",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a2postsvs",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint8[128]",
                "name": "v26",
                "type": "uint8[128]"
              }
            ],
            "internalType": "struct ReachContract.a3msg",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "internalType": "struct ReachContract.a3",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "m3",
    "outputs": [],
    "stateMutability": "payable",
    "type": "function"
  }
]`,
  Bytecode: `0x608060408190527f49ff028a829527a47ec6839c7147b484eccf5a2a94853eddac09cef44d9d4e9e90600090a161003461007a565b43815261003f61007a565b8151815260405161005790600090839060200161008d565b60408051601f1981840301815291905280516020909101206000555061009c9050565b6040518060200160405280600081525090565b91825251602082015260400190565b610566806100ab6000396000f3fe6080604052600436106100335760003560e01c80621deb861461003857806319e123561461004d5780639532ef0114610060575b600080fd5b61004b6100463660046103b2565b610073565b005b61004b61005b3660046103a1565b610163565b61004b61006e36600461038a565b61024e565b6040516100879060029083906020016104ec565b6040516020818303038152906040528051906020012060001c600054146100ad57600080fd5b336100be6040830160208401610369565b6001600160a01b0316146100d157600080fd5b34156100dc57600080fd5b6100ec6040820160208301610369565b6040516001600160a01b039190911690823580156108fc02916000818181858888f19350505050158015610124573d6000803e3d6000fd5b507ff0576dc799b60fe09428f90051394a37f1e32628d37b68e2a161b81436b45757816040516101549190610444565b60405180910390a16000805533ff5b6040516101779060019083906020016104a8565b6040516020818303038152906040528051906020012060001c6000541461019d57600080fd5b346020820135146101ad57600080fd5b7f1c771c6bb0c334e475100c2f63c3efcafa0587cbc11a2259c6484422d8a840ea816040516101dc9190610430565b60405180910390a16101ec6102f9565b602082013581526102036060830160408401610369565b6001600160a01b0316602080830191909152436040808401919091525161022f91600291849101610500565b60408051601f1981840301815291905280516020909101206000555050565b604051610262906000908390602001610499565b6040516020818303038152906040528051906020012060001c6000541461028857600080fd5b341561029357600080fd5b7ff2c62eba998811305a23599b2e6d212befbd7ded3a73f4c08bfb9aefe08dc166816040516102c29190610419565b60405180910390a16102d2610323565b43815260208083013581830152336040808401919091525161022f916001918491016104bc565b60405180606001604052806000815260200160006001600160a01b03168152602001600081525090565b6040518060600160405280600081526020016000815260200160006001600160a01b031681525090565b80356001600160a01b038116811461036457600080fd5b919050565b60006020828403121561037a578081fd5b6103838261034d565b9392505050565b60006040828403121561039b578081fd5b50919050565b60006060828403121561039b578081fd5b6000611060828403121561039b578081fd5b80358252602080820135908301526001600160a01b036103e66040830161034d565b1660408301525050565b803582526001600160a01b036104086020830161034d565b166020830152604090810135910152565b813581526020918201359181019190915260400190565b6060810161043e82846103c4565b92915050565b611060810161045382846103f0565b60608201606084016000805b608081101561048f57823560ff8116808214610479578384fd5b855250602093840193929092019160010161045f565b5050505092915050565b91825235602082015260400190565b8281526080810161038360208301846103c4565b918252805160208084019190915281015160408084019190915201516001600160a01b0316606082015260800190565b8281526080810161038360208301846103f0565b91825280516020808401919091528101516001600160a01b0316604080840191909152015160608201526080019056fea26469706673582212206dd918c561d7372a5f6d68893f032b5059970ce453fcdf5f994fd79dfb2c961864736f6c63430008000033`,
  deployMode: `DM_constructor`
   };

export const _Connectors = {
  ALGO: _ALGO,
  ETH: _ETH
   };

