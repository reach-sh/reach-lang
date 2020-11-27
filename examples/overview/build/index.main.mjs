// Automatically generated with Reach 0.1.2
/* eslint-disable no-unused-vars, no-empty-pattern, no-useless-escape, no-loop-func */
export const _version = '0.1.2';


export async function Alice(stdlib, ctc, interact) {
  const txn1 = await ctc.sendrecv('Alice', 1, 1, [stdlib.T_UInt], [stdlib.protect(stdlib.T_UInt, interact.request, null)], stdlib.checkedBigNumberify('./index.rsh:13:25:after expr stmt semicolon', stdlib.UInt_max, 0), [stdlib.T_UInt], false, ((txn1) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:13:8:dot', stdlib.UInt_max, 0)]);
    const [v2] = txn1.data;
    const v4 = txn1.value;
    const v3 = txn1.from;
    
    const v5 = stdlib.eq(v4, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
    stdlib.assert(v5, {
      at: './index.rsh:13:25:after expr stmt semicolon',
      fs: [],
      msg: 'pay amount correct',
      who: 'Alice'
       });
    sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:14:15:after expr stmt semicolon', stdlib.UInt_max, 1), v3, v2]);
    sim_r.isHalt = false;
    
    return sim_r;
     }));
  const [v2] = txn1.data;
  const v4 = txn1.value;
  const v3 = txn1.from;
  const v5 = stdlib.eq(v4, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
  stdlib.assert(v5, {
    at: './index.rsh:13:25:after expr stmt semicolon',
    fs: [],
    msg: 'pay amount correct',
    who: 'Alice'
     });
  const txn2 = await ctc.recv('Alice', 2, 0, [], false);
  const [] = txn2.data;
  const v14 = txn2.value;
  const v13 = txn2.from;
  const v15 = stdlib.eq(v14, v2);
  stdlib.assert(v15, {
    at: './index.rsh:18:21:after expr stmt semicolon',
    fs: [],
    msg: 'pay amount correct',
    who: 'Alice'
     });
  const txn3 = await ctc.sendrecv('Alice', 3, 1, [stdlib.T_Address, stdlib.T_UInt, stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, 128))], [v3, v2, stdlib.protect(stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, 128)), interact.info, null)], stdlib.checkedBigNumberify('./index.rsh:23:22:after expr stmt semicolon', stdlib.UInt_max, 0), [stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, 128))], false, ((txn3) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:23:8:dot', stdlib.UInt_max, 2), v3, v2]);
    const [v22] = txn3.data;
    const v23 = txn3.value;
    
    const v24 = stdlib.eq(v23, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
    stdlib.assert(v24, {
      at: './index.rsh:23:22:after expr stmt semicolon',
      fs: [],
      msg: 'pay amount correct',
      who: 'Alice'
       });
    sim_r.txns.push({
      amt: v2,
      to: v3
       });
    sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([]), []);
    sim_r.isHalt = true;
    
    return sim_r;
     }));
  const [v22] = txn3.data;
  const v23 = txn3.value;
  const v24 = stdlib.eq(v23, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
  stdlib.assert(v24, {
    at: './index.rsh:23:22:after expr stmt semicolon',
    fs: [],
    msg: 'pay amount correct',
    who: 'Alice'
     });
  ;
  return;
  
  
   }
export async function Bob(stdlib, ctc, interact) {
  const txn1 = await ctc.recv('Bob', 1, 1, [stdlib.T_UInt], false);
  const [v2] = txn1.data;
  const v4 = txn1.value;
  const v3 = txn1.from;
  const v5 = stdlib.eq(v4, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
  stdlib.assert(v5, {
    at: './index.rsh:13:25:after expr stmt semicolon',
    fs: [],
    msg: 'pay amount correct',
    who: 'Bob'
     });
  stdlib.protect(stdlib.T_Null, await interact.want(v2), {
    at: './index.rsh:17:22:application',
    fs: ['at ./index.rsh:17:35:after expr stmt semicolon call to "function" (defined at: ./index.rsh:16:17:function exp)'],
    msg: 'want',
    who: 'Bob'
     });
  const txn2 = await ctc.sendrecv('Bob', 2, 0, [stdlib.T_Address, stdlib.T_UInt], [v3, v2], v2, [], false, ((txn2) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:18:8:dot', stdlib.UInt_max, 1), v3, v2]);
    const [] = txn2.data;
    const v14 = txn2.value;
    const v13 = txn2.from;
    
    const v15 = stdlib.eq(v14, v2);
    stdlib.assert(v15, {
      at: './index.rsh:18:21:after expr stmt semicolon',
      fs: [],
      msg: 'pay amount correct',
      who: 'Bob'
       });
    sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:19:15:after expr stmt semicolon', stdlib.UInt_max, 2), v3, v2]);
    sim_r.isHalt = false;
    
    return sim_r;
     }));
  const [] = txn2.data;
  const v14 = txn2.value;
  const v13 = txn2.from;
  const v15 = stdlib.eq(v14, v2);
  stdlib.assert(v15, {
    at: './index.rsh:18:21:after expr stmt semicolon',
    fs: [],
    msg: 'pay amount correct',
    who: 'Bob'
     });
  const txn3 = await ctc.recv('Bob', 3, 1, [stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, 128))], false);
  const [v22] = txn3.data;
  const v23 = txn3.value;
  const v24 = stdlib.eq(v23, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
  stdlib.assert(v24, {
    at: './index.rsh:23:22:after expr stmt semicolon',
    fs: [],
    msg: 'pay amount correct',
    who: 'Bob'
     });
  ;
  stdlib.protect(stdlib.T_Null, await interact.got(v22), {
    at: './index.rsh:28:21:application',
    fs: ['at ./index.rsh:28:31:after expr stmt semicolon call to "function" (defined at: ./index.rsh:27:17:function exp)'],
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
// Just "pay amount correct"
// "./index.rsh:13:25:after expr stmt semicolon"
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
// "./index.rsh:18:21:after expr stmt semicolon"
// "[]"
gtxn 3 Amount
arg 3
btoi
-
arg 6
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
gtxn 3 Sender
arg 5
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
// Just "pay amount correct"
// "./index.rsh:23:22:after expr stmt semicolon"
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
                "name": "v2",
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
                "internalType": "address payable",
                "name": "v3",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v2",
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
                "internalType": "uint256",
                "name": "_last",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v3",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v2",
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
                "name": "v22",
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
                "name": "v2",
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
                "internalType": "address payable",
                "name": "v3",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v2",
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
                "internalType": "uint256",
                "name": "_last",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v3",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v2",
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
                "name": "v22",
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
  Bytecode: `0x608060408190527f49ff028a829527a47ec6839c7147b484eccf5a2a94853eddac09cef44d9d4e9e90600090a161003461006c565b43815260405161004b90600090839060200161007f565b60408051601f1981840301815291905280516020909101206000555061008e565b6040518060200160405280600081525090565b91825251602082015260400190565b6104dd8061009d6000396000f3fe6080604052600436106100335760003560e01c80621deb86146100385780639532ef011461004d578063becab94114610060575b600080fd5b61004b610046366004610393565b610073565b005b61004b61005b36600461036b565b61016f565b61004b61006e366004610382565b61023c565b60405161008790600290839060200161047f565b6040516020818303038152906040528051906020012060001c600054146100ad57600080fd5b6100bd604082016020830161034a565b6001600160a01b0316336001600160a01b0316146100da57600080fd5b34156100e557600080fd5b6100f5604082016020830161034a565b604080516001600160a01b0392909216919083013580156108fc02916000818181858888f19350505050158015610130573d6000803e3d6000fd5b507ff0576dc799b60fe09428f90051394a37f1e32628d37b68e2a161b81436b4575781604051610160919061041b565b60405180910390a16000805533ff5b604051610183906000908390602001610470565b6040516020818303038152906040528051906020012060001c600054146101a957600080fd5b34156101b457600080fd5b7ff2c62eba998811305a23599b2e6d212befbd7ded3a73f4c08bfb9aefe08dc166816040516101e391906103f0565b60405180910390a16101f3610304565b43815233602080830191909152828101356040808401919091525161021d91600191849101610493565b60408051601f1981840301815291905280516020909101206000555050565b60405161025090600190839060200161047f565b6040516020818303038152906040528051906020012060001c6000541461027657600080fd5b3460408201351461028657600080fd5b7fd48df6dba41e682c72cf1d60ee8d30789a04d67f8ac1a7db2be6d9e70dbb345d816040516102b59190610407565b60405180910390a16102c5610304565b4381526102d8604083016020840161034a565b6001600160a01b0316602080830191909152604080840135818401525161021d91600291849101610493565b60405180606001604052806000815260200160006001600160a01b03168152602001600081525090565b80356001600160a01b038116811461034557600080fd5b919050565b60006020828403121561035b578081fd5b6103648261032e565b9392505050565b60006040828403121561037c578081fd5b50919050565b60006060828403121561037c578081fd5b6000611060828403121561037c578081fd5b803582526001600160a01b036103bd6020830161032e565b166020830152604090810135910152565b805182526020808201516001600160a01b031690830152604090810151910152565b813581526020918201359181019190915260400190565b6060810161041582846103a5565b92915050565b611060810161042a82846103a5565b60608201606084016000805b608081101561046657823560ff8116808214610450578384fd5b8552506020938401939290920191600101610436565b5050505092915050565b91825235602082015260400190565b8281526080810161036460208301846103a5565b8281526080810161036460208301846103ce56fea2646970667358221220e009874c30a30954f7521610abf71ffa777f18e33ce11bb41d954210d8770fb864736f6c63430007040033`,
  deployMode: `DM_constructor`
   };

export const _Connectors = {
  ALGO: _ALGO,
  ETH: _ETH
   };

