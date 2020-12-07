// Automatically generated with Reach 0.1.2
/* eslint-disable no-unused-vars, no-empty-pattern, no-useless-escape, no-loop-func */
export const _version = '0.1.2';


export async function Alice(ctc, interact) {
  const stdlib = ctc.stdlibT;
const txn1 = await ctc.sendrecv('Alice', 1, 1, [stdlib.T_UInt], [stdlib.protect(stdlib.T_UInt, interact.request, null)], stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), [stdlib.T_UInt], false, ((txn1) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:13:8:dot', stdlib.UInt_max, 0)]);
    const [v3] = txn1.data;
    const v4 = txn1.value;
    const v2 = txn1.from;
    
    stdlib.assert(true, {
      at: './index.rsh:13:8:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Alice'
       });
    const v5 = stdlib.eq(v4, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
    stdlib.assert(v5, {
      at: './index.rsh:13:8:dot',
      fs: [],
      msg: 'pay amount correct',
      who: 'Alice'
       });
    sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address]), [stdlib.checkedBigNumberify('./index.rsh:14:15:after expr stmt semicolon', stdlib.UInt_max, 1), v3, v2]);
    sim_r.isHalt = false;
    
    return sim_r;
     }));
  const [v3] = txn1.data;
  const v4 = txn1.value;
  const v2 = txn1.from;
  stdlib.assert(true, {
    at: './index.rsh:13:8:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Alice'
     });
  const v5 = stdlib.eq(v4, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
  stdlib.assert(v5, {
    at: './index.rsh:13:8:dot',
    fs: [],
    msg: 'pay amount correct',
    who: 'Alice'
     });
  const txn2 = await ctc.recv('Alice', 2, 0, [], false);
  const [] = txn2.data;
  const v14 = txn2.value;
  const v13 = txn2.from;
  stdlib.assert(true, {
    at: './index.rsh:18:8:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Alice'
     });
  const v15 = stdlib.eq(v14, v3);
  stdlib.assert(v15, {
    at: './index.rsh:18:8:dot',
    fs: [],
    msg: 'pay amount correct',
    who: 'Alice'
     });
  const txn3 = await ctc.sendrecv('Alice', 3, 1, [stdlib.T_UInt, stdlib.T_Address, stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, 128))], [v3, v2, stdlib.protect(stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, 128)), interact.info, null)], stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), [stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, 128))], false, ((txn3) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address]), [stdlib.checkedBigNumberify('./index.rsh:23:8:dot', stdlib.UInt_max, 2), v3, v2]);
    const [v23] = txn3.data;
    const v24 = txn3.value;
    const v22 = txn3.from;
    
    const v26 = stdlib.addressEq(v2, v22);
    stdlib.assert(v26, {
      at: './index.rsh:23:8:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Alice'
       });
    const v25 = stdlib.eq(v24, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
    stdlib.assert(v25, {
      at: './index.rsh:23:8:dot',
      fs: [],
      msg: 'pay amount correct',
      who: 'Alice'
       });
    sim_r.txns.push({
      amt: v3,
      to: v2
       });
    sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([]), []);
    sim_r.isHalt = true;
    
    return sim_r;
     }));
  const [v23] = txn3.data;
  const v24 = txn3.value;
  const v22 = txn3.from;
  const v26 = stdlib.addressEq(v2, v22);
  stdlib.assert(v26, {
    at: './index.rsh:23:8:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Alice'
     });
  const v25 = stdlib.eq(v24, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
  stdlib.assert(v25, {
    at: './index.rsh:23:8:dot',
    fs: [],
    msg: 'pay amount correct',
    who: 'Alice'
     });
  ;
  return;
  
  
   }
export async function Bob(ctc, interact) {
  const stdlib = ctc.stdlibT;
const txn1 = await ctc.recv('Bob', 1, 1, [stdlib.T_UInt], false);
  const [v3] = txn1.data;
  const v4 = txn1.value;
  const v2 = txn1.from;
  stdlib.assert(true, {
    at: './index.rsh:13:8:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Bob'
     });
  const v5 = stdlib.eq(v4, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
  stdlib.assert(v5, {
    at: './index.rsh:13:8:dot',
    fs: [],
    msg: 'pay amount correct',
    who: 'Bob'
     });
  stdlib.protect(stdlib.T_Null, await interact.want(v3), {
    at: './index.rsh:17:22:application',
    fs: ['at ./index.rsh:17:35:after expr stmt semicolon call to [unknown function] (defined at: ./index.rsh:16:17:function exp)'],
    msg: 'want',
    who: 'Bob'
     });
  const txn2 = await ctc.sendrecv('Bob', 2, 0, [stdlib.T_UInt, stdlib.T_Address], [v3, v2], v3, [], false, ((txn2) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address]), [stdlib.checkedBigNumberify('./index.rsh:18:8:dot', stdlib.UInt_max, 1), v3, v2]);
    const [] = txn2.data;
    const v14 = txn2.value;
    const v13 = txn2.from;
    
    stdlib.assert(true, {
      at: './index.rsh:18:8:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Bob'
       });
    const v15 = stdlib.eq(v14, v3);
    stdlib.assert(v15, {
      at: './index.rsh:18:8:dot',
      fs: [],
      msg: 'pay amount correct',
      who: 'Bob'
       });
    sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address]), [stdlib.checkedBigNumberify('./index.rsh:19:15:after expr stmt semicolon', stdlib.UInt_max, 2), v3, v2]);
    sim_r.isHalt = false;
    
    return sim_r;
     }));
  const [] = txn2.data;
  const v14 = txn2.value;
  const v13 = txn2.from;
  stdlib.assert(true, {
    at: './index.rsh:18:8:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Bob'
     });
  const v15 = stdlib.eq(v14, v3);
  stdlib.assert(v15, {
    at: './index.rsh:18:8:dot',
    fs: [],
    msg: 'pay amount correct',
    who: 'Bob'
     });
  const txn3 = await ctc.recv('Bob', 3, 1, [stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, 128))], false);
  const [v23] = txn3.data;
  const v24 = txn3.value;
  const v22 = txn3.from;
  const v26 = stdlib.addressEq(v2, v22);
  stdlib.assert(v26, {
    at: './index.rsh:23:8:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Bob'
     });
  const v25 = stdlib.eq(v24, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
  stdlib.assert(v25, {
    at: './index.rsh:23:8:dot',
    fs: [],
    msg: 'pay amount correct',
    who: 'Bob'
     });
  ;
  stdlib.protect(stdlib.T_Null, await interact.got(v23), {
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
int 0
itob
keccak256
arg 0
==
assert
// Run body
// Just "sender correct"
// "./index.rsh:13:8:dot"
// "[]"
int 1
assert
// Just "pay amount correct"
// "./index.rsh:13:8:dot"
// "[]"
gtxn 3 Amount
arg 3
btoi
-
int 0
==
assert
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
// "./index.rsh:18:8:dot"
// "[]"
int 1
assert
// Just "pay amount correct"
// "./index.rsh:18:8:dot"
// "[]"
gtxn 3 Amount
arg 3
btoi
-
arg 5
btoi
==
assert
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
// "./index.rsh:23:8:dot"
// "[]"
arg 6
gtxn 3 Sender
==
assert
// Just "pay amount correct"
// "./index.rsh:23:8:dot"
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
                "name": "_last",
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
                "name": "v3",
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
                "name": "_last",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v3",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v2",
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
                "name": "_last",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v3",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v2",
                "type": "address"
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
                "name": "_last",
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
                "name": "v3",
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
                "name": "_last",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v3",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v2",
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
                "name": "_last",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v3",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v2",
                "type": "address"
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
  Bytecode: `0x608060408190527f49ff028a829527a47ec6839c7147b484eccf5a2a94853eddac09cef44d9d4e9e90600090a161003461006c565b43815260405161004b90600090839060200161007f565b60408051601f1981840301815291905280516020909101206000555061008e565b6040518060200160405280600081525090565b91825251602082015260400190565b6104d78061009d6000396000f3fe6080604052600436106100345760003560e01c806318ebfc731461003957806319e123561461004e5780639532ef0114610061575b600080fd5b61004c61004736600461038a565b610074565b005b61004c61005c366004610379565b610167565b61004c61006f366004610362565b610250565b604051610088906002908390602001610479565b6040516020818303038152906040528051906020012060001c600054146100ae57600080fd5b336100bf6060830160408401610341565b6001600160a01b0316146100d257600080fd5b34156100dd57600080fd5b6100ed6060820160408301610341565b6040516001600160a01b039190911690602083013580156108fc02916000818181858888f19350505050158015610128573d6000803e3d6000fd5b507f0b2104bf18a660f777a99e7b1e6b3bdb20d1ff947840ca0430adc9044bd339b8816040516101589190610415565b60405180910390a16000805533ff5b60405161017b906001908390602001610479565b6040516020818303038152906040528051906020012060001c600054146101a157600080fd5b346020820135146101b157600080fd5b7f1c771c6bb0c334e475100c2f63c3efcafa0587cbc11a2259c6484422d8a840ea816040516101e09190610401565b60405180910390a16101f06102fb565b4381526020808301359082015261020d6060830160408401610341565b6001600160a01b03166040808301919091525161023190600290839060200161048d565b60408051601f1981840301815291905280516020909101206000555050565b60405161026490600090839060200161046a565b6040516020818303038152906040528051906020012060001c6000541461028a57600080fd5b341561029557600080fd5b7ff2c62eba998811305a23599b2e6d212befbd7ded3a73f4c08bfb9aefe08dc166816040516102c491906103ea565b60405180910390a16102d46102fb565b4381526020808301358183015233604080840191909152516102319160019184910161048d565b6040518060600160405280600081526020016000815260200160006001600160a01b031681525090565b80356001600160a01b038116811461033c57600080fd5b919050565b600060208284031215610352578081fd5b61035b82610325565b9392505050565b600060408284031215610373578081fd5b50919050565b600060608284031215610373578081fd5b60006110608284031215610373578081fd5b80358252602080820135908301526001600160a01b036103be60408301610325565b1660408301525050565b80518252602080820151908301526040908101516001600160a01b0316910152565b813581526020918201359181019190915260400190565b6060810161040f828461039c565b92915050565b6110608101610424828461039c565b60608201606084016000805b608081101561046057823560ff811680821461044a578384fd5b8552506020938401939290920191600101610430565b5050505092915050565b91825235602082015260400190565b8281526080810161035b602083018461039c565b8281526080810161035b60208301846103c856fea264697066735822122060878555c973deebc8560fae08a87f7a224d869daf2e143884f2eb70737a9d9764736f6c63430007040033`,
  deployMode: `DM_constructor`
   };

export const _Connectors = {
  ALGO: _ALGO,
  ETH: _ETH
   };

