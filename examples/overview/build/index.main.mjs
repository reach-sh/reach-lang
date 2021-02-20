// Automatically generated with Reach 0.1.2
/* eslint-disable no-unused-vars, no-empty-pattern, no-useless-escape, no-loop-func */
export const _version = '0.1.2';


export async function Alice(ctc, interact) {
  const stdlib = ctc.stdlib;
  
  const v2 = await ctc.creationTime();
  const v0 = stdlib.protect(stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, 128)), interact.info, null);
  const v1 = stdlib.protect(stdlib.T_UInt, interact.request, null);
  const txn1 = await (ctc.sendrecv('Alice', 1, 1, stdlib.checkedBigNumberify('./index.rsh:13:9:dot', stdlib.UInt_max, 0), [stdlib.T_UInt, stdlib.T_UInt], [v2, v1], stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), [stdlib.T_UInt], true, true, false, ((txn1) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:13:9:dot', stdlib.UInt_max, 0), v2]);
    sim_r.prevSt_noPrevTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:13:9:dot', stdlib.UInt_max, 0)]);
    const [v6] = txn1.data;
    const v7 = txn1.value;
    const v11 = txn1.time;
    const v5 = txn1.from;
    
    const v8 = stdlib.eq(v7, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
    stdlib.assert(v8, {
      at: './index.rsh:13:9:dot',
      fs: [],
      msg: 'pay amount correct',
      who: 'Alice'
       });
    stdlib.assert(true, {
      at: './index.rsh:13:9:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Alice'
       });
    sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:14:15:after expr stmt semicolon', stdlib.UInt_max, 1), v5, v6, v11]);
    sim_r.nextSt_noTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:14:15:after expr stmt semicolon', stdlib.UInt_max, 1), v5, v6]);
    sim_r.isHalt = false;
    
    return sim_r;
     })));
  const [v6] = txn1.data;
  const v7 = txn1.value;
  const v11 = txn1.time;
  const v5 = txn1.from;
  const v8 = stdlib.eq(v7, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
  stdlib.assert(v8, {
    at: './index.rsh:13:9:dot',
    fs: [],
    msg: 'pay amount correct',
    who: 'Alice'
     });
  stdlib.assert(true, {
    at: './index.rsh:13:9:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Alice'
     });
  const txn2 = await (ctc.recv('Alice', 2, 0, [], false, false));
  const [] = txn2.data;
  const v15 = txn2.value;
  const v19 = txn2.time;
  const v14 = txn2.from;
  const v16 = stdlib.eq(v15, v6);
  stdlib.assert(v16, {
    at: './index.rsh:18:9:dot',
    fs: [],
    msg: 'pay amount correct',
    who: 'Alice'
     });
  stdlib.assert(true, {
    at: './index.rsh:18:9:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Alice'
     });
  const txn3 = await (ctc.sendrecv('Alice', 3, 1, stdlib.checkedBigNumberify('./index.rsh:23:9:dot', stdlib.UInt_max, 2), [stdlib.T_Address, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, 128))], [v5, v6, v19, v0], stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), [stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, 128))], true, true, false, ((txn3) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:23:9:dot', stdlib.UInt_max, 2), v5, v6, v19]);
    sim_r.prevSt_noPrevTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:23:9:dot', stdlib.UInt_max, 2), v5, v6]);
    const [v23] = txn3.data;
    const v24 = txn3.value;
    const v29 = txn3.time;
    const v22 = txn3.from;
    
    const v25 = stdlib.eq(v24, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
    stdlib.assert(v25, {
      at: './index.rsh:23:9:dot',
      fs: [],
      msg: 'pay amount correct',
      who: 'Alice'
       });
    const v26 = stdlib.addressEq(v5, v22);
    stdlib.assert(v26, {
      at: './index.rsh:23:9:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Alice'
       });
    sim_r.txns.push({
      amt: v6,
      to: v5
       });
    sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([]), []);
    sim_r.nextSt_noTime = stdlib.digest(stdlib.T_Tuple([]), []);
    sim_r.isHalt = true;
    
    return sim_r;
     })));
  const [v23] = txn3.data;
  const v24 = txn3.value;
  const v29 = txn3.time;
  const v22 = txn3.from;
  const v25 = stdlib.eq(v24, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
  stdlib.assert(v25, {
    at: './index.rsh:23:9:dot',
    fs: [],
    msg: 'pay amount correct',
    who: 'Alice'
     });
  const v26 = stdlib.addressEq(v5, v22);
  stdlib.assert(v26, {
    at: './index.rsh:23:9:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Alice'
     });
  ;
  return;
  
  
  
   }
export async function Bob(ctc, interact) {
  const stdlib = ctc.stdlib;
  
  const v2 = await ctc.creationTime();
  const txn1 = await (ctc.recv('Bob', 1, 1, [stdlib.T_UInt], false, false));
  const [v6] = txn1.data;
  const v7 = txn1.value;
  const v11 = txn1.time;
  const v5 = txn1.from;
  const v8 = stdlib.eq(v7, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
  stdlib.assert(v8, {
    at: './index.rsh:13:9:dot',
    fs: [],
    msg: 'pay amount correct',
    who: 'Bob'
     });
  stdlib.assert(true, {
    at: './index.rsh:13:9:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Bob'
     });
  stdlib.protect(stdlib.T_Null, await interact.want(v6), {
    at: './index.rsh:17:22:application',
    fs: ['at ./index.rsh:16:13:application call to [unknown function] (defined at: ./index.rsh:16:17:function exp)'],
    msg: 'want',
    who: 'Bob'
     });
  const txn2 = await (ctc.sendrecv('Bob', 2, 0, stdlib.checkedBigNumberify('./index.rsh:18:9:dot', stdlib.UInt_max, 2), [stdlib.T_Address, stdlib.T_UInt, stdlib.T_UInt], [v5, v6, v11], v6, [], true, true, false, ((txn2) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:18:9:dot', stdlib.UInt_max, 1), v5, v6, v11]);
    sim_r.prevSt_noPrevTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:18:9:dot', stdlib.UInt_max, 1), v5, v6]);
    const [] = txn2.data;
    const v15 = txn2.value;
    const v19 = txn2.time;
    const v14 = txn2.from;
    
    const v16 = stdlib.eq(v15, v6);
    stdlib.assert(v16, {
      at: './index.rsh:18:9:dot',
      fs: [],
      msg: 'pay amount correct',
      who: 'Bob'
       });
    stdlib.assert(true, {
      at: './index.rsh:18:9:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Bob'
       });
    sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:19:15:after expr stmt semicolon', stdlib.UInt_max, 2), v5, v6, v19]);
    sim_r.nextSt_noTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:19:15:after expr stmt semicolon', stdlib.UInt_max, 2), v5, v6]);
    sim_r.isHalt = false;
    
    return sim_r;
     })));
  const [] = txn2.data;
  const v15 = txn2.value;
  const v19 = txn2.time;
  const v14 = txn2.from;
  const v16 = stdlib.eq(v15, v6);
  stdlib.assert(v16, {
    at: './index.rsh:18:9:dot',
    fs: [],
    msg: 'pay amount correct',
    who: 'Bob'
     });
  stdlib.assert(true, {
    at: './index.rsh:18:9:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Bob'
     });
  const txn3 = await (ctc.recv('Bob', 3, 1, [stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, 128))], false, false));
  const [v23] = txn3.data;
  const v24 = txn3.value;
  const v29 = txn3.time;
  const v22 = txn3.from;
  const v25 = stdlib.eq(v24, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
  stdlib.assert(v25, {
    at: './index.rsh:23:9:dot',
    fs: [],
    msg: 'pay amount correct',
    who: 'Bob'
     });
  const v26 = stdlib.addressEq(v5, v22);
  stdlib.assert(v26, {
    at: './index.rsh:23:9:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Bob'
     });
  ;
  stdlib.protect(stdlib.T_Null, await interact.got(v23), {
    at: './index.rsh:28:21:application',
    fs: ['at ./index.rsh:27:13:application call to [unknown function] (defined at: ./index.rsh:27:17:function exp)'],
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
// compute state in HM_Set 0
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
// Just "sender correct"
// "./index.rsh:13:9:dot"
// "[]"
int 1
assert
// compute state in HM_Set 1
int 1
itob
gtxn 3 Sender
concat
arg 5
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
// Just "pay amount correct"
// "./index.rsh:18:9:dot"
// "[]"
gtxn 3 Amount
arg 3
btoi
-
arg 6
btoi
==
assert
// Just "sender correct"
// "./index.rsh:18:9:dot"
// "[]"
int 1
assert
// compute state in HM_Set 2
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
// Just "sender correct"
// "./index.rsh:23:9:dot"
// "[]"
arg 5
gtxn 3 Sender
==
assert
gtxn 4 TypeEnum
int pay
==
assert
gtxn 4 Receiver
arg 5
==
assert
gtxn 4 Amount
arg 6
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
                "name": "v2",
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
                "name": "v6",
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
                "internalType": "address payable",
                "name": "v5",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v6",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v11",
                "type": "uint256"
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
                "internalType": "address payable",
                "name": "v5",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v6",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v19",
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
                "name": "v23",
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
                "name": "v2",
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
                "name": "v6",
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
                "internalType": "address payable",
                "name": "v5",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v6",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v11",
                "type": "uint256"
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
                "internalType": "address payable",
                "name": "v5",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v6",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v19",
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
                "name": "v23",
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
  Bytecode: `0x608060408190527f49ff028a829527a47ec6839c7147b484eccf5a2a94853eddac09cef44d9d4e9e90600090a161003461007a565b43815261003f61007a565b8151815260405161005790600090839060200161008d565b60408051601f1981840301815291905280516020909101206000555061009c9050565b6040518060200160405280600081525090565b91825251602082015260400190565b6104c9806100ab6000396000f3fe6080604052600436106100345760003560e01c80639532ef0114610039578063f512f77e1461004e578063fd7b785414610061575b600080fd5b61004c610047366004610357565b610074565b005b61004c61005c36600461037f565b61013e565b61004c61006f36600461036e565b61022b565b60405161008890600090839060200161045c565b6040516020818303038152906040528051906020012060001c600054146100ae57600080fd5b34156100b957600080fd5b7ff2c62eba998811305a23599b2e6d212befbd7ded3a73f4c08bfb9aefe08dc166816040516100e891906103dc565b60405180910390a16100f86102f0565b33815260208083013581830152436040808401919091525161011f9160019184910161047f565b60408051601f1981840301815291905280516020909101206000555050565b60405161015290600290839060200161046b565b6040516020818303038152906040528051906020012060001c6000541461017857600080fd5b341561018357600080fd5b336101916020830183610336565b6001600160a01b0316146101a457600080fd5b6101b16020820182610336565b6040516001600160a01b039190911690602083013580156108fc02916000818181858888f193505050501580156101ec573d6000803e3d6000fd5b507f6ca511835aec60423a26d24cdbe1d3b53c20c6d05a3c891aed1744e1f97974bf8160405161021c9190610407565b60405180910390a16000805533ff5b60405161023f90600190839060200161046b565b6040516020818303038152906040528051906020012060001c6000541461026557600080fd5b3460208201351461027557600080fd5b7f54f54df36af4d52c378250b6463f21d5fc23bbbc8ced980e3ef0ddec4b30cb6b816040516102a491906103f3565b60405180910390a16102b46102f0565b6102c16020830183610336565b6001600160a01b0316815260208083013581830152436040808401919091525161011f9160029184910161047f565b604051806060016040528060006001600160a01b0316815260200160008152602001600081525090565b80356001600160a01b038116811461033157600080fd5b919050565b600060208284031215610347578081fd5b6103508261031a565b9392505050565b600060408284031215610368578081fd5b50919050565b600060608284031215610368578081fd5b60006110608284031215610368578081fd5b6001600160a01b036103a28261031a565b16825260208181013590830152604090810135910152565b80516001600160a01b0316825260208082015190830152604090810151910152565b813581526020918201359181019190915260400190565b606081016104018284610391565b92915050565b61106081016104168284610391565b60608201606084016000805b608081101561045257823560ff811680821461043c578384fd5b8552506020938401939290920191600101610422565b5050505092915050565b91825235602082015260400190565b828152608081016103506020830184610391565b8281526080810161035060208301846103ba56fea26469706673582212202b828a94720d00b4db0313c1613b4e09eb357833b6fe330a4420c9be7044065664736f6c63430008000033`,
  deployMode: `DM_constructor`
   };

export const _Connectors = {
  ALGO: _ALGO,
  ETH: _ETH
   };

