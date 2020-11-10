// Automatically generated with Reach 0.1.2
/* eslint-disable no-unused-vars, no-empty-pattern, no-useless-escape, no-loop-func */
export const _version = '0.1.2';


export async function Alice(stdlib, ctc, interact) {
  const txn1 = await ctc.sendrecv('Alice', 1, 1, [stdlib.T_UInt], [stdlib.protect(stdlib.T_UInt, interact.request, null)], stdlib.checkedBigNumberify('./index.rsh:13:25:after expr stmt semicolon', stdlib.UInt_max, 0), [stdlib.T_UInt], false, ((txn1) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:13:8:dot', stdlib.UInt_max, 0)]);
    const [v1] = txn1.data;
    const v3 = txn1.value;
    const v2 = txn1.from;
    
    const v4 = stdlib.eq(v3, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
    stdlib.assert(v4, {
      at: './index.rsh:13:25:after expr stmt semicolon',
      fs: [],
      msg: 'pay amount correct',
      who: 'Alice'
       });
    sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:14:15:after expr stmt semicolon', stdlib.UInt_max, 1), v2, v1]);
    sim_r.isHalt = false;
    
    return sim_r;
     }));
  const [v1] = txn1.data;
  const v3 = txn1.value;
  const v2 = txn1.from;
  const v4 = stdlib.eq(v3, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
  stdlib.assert(v4, {
    at: './index.rsh:13:25:after expr stmt semicolon',
    fs: [],
    msg: 'pay amount correct',
    who: 'Alice'
     });
  const txn2 = await ctc.recv('Alice', 2, 0, [], false);
  const [] = txn2.data;
  const v12 = txn2.value;
  const v11 = txn2.from;
  const v13 = stdlib.eq(v12, v1);
  stdlib.assert(v13, {
    at: './index.rsh:18:21:after expr stmt semicolon',
    fs: [],
    msg: 'pay amount correct',
    who: 'Alice'
     });
  const txn3 = await ctc.sendrecv('Alice', 3, 1, [stdlib.T_Address, stdlib.T_UInt, stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, 128))], [v2, v1, stdlib.protect(stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, 128)), interact.info, null)], stdlib.checkedBigNumberify('./index.rsh:23:22:after expr stmt semicolon', stdlib.UInt_max, 0), [stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, 128))], false, ((txn3) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:23:8:dot', stdlib.UInt_max, 2), v2, v1]);
    const [v19] = txn3.data;
    const v20 = txn3.value;
    
    const v21 = stdlib.eq(v20, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
    stdlib.assert(v21, {
      at: './index.rsh:23:22:after expr stmt semicolon',
      fs: [],
      msg: 'pay amount correct',
      who: 'Alice'
       });
    sim_r.txns.push({
      amt: v1,
      to: v2
       });
    sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([]), []);
    sim_r.isHalt = true;
    
    return sim_r;
     }));
  const [v19] = txn3.data;
  const v20 = txn3.value;
  const v21 = stdlib.eq(v20, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
  stdlib.assert(v21, {
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
  const [v1] = txn1.data;
  const v3 = txn1.value;
  const v2 = txn1.from;
  const v4 = stdlib.eq(v3, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
  stdlib.assert(v4, {
    at: './index.rsh:13:25:after expr stmt semicolon',
    fs: [],
    msg: 'pay amount correct',
    who: 'Bob'
     });
  stdlib.protect(stdlib.T_Null, await interact.want(v1), {
    at: './index.rsh:17:22:application',
    fs: ['at ./index.rsh:17:35:after expr stmt semicolon call to "function" (defined at: ./index.rsh:16:17:function exp)'],
    msg: 'want',
    who: 'Bob'
     });
  const txn2 = await ctc.sendrecv('Bob', 2, 0, [stdlib.T_Address, stdlib.T_UInt], [v2, v1], v1, [], false, ((txn2) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:18:8:dot', stdlib.UInt_max, 1), v2, v1]);
    const [] = txn2.data;
    const v12 = txn2.value;
    const v11 = txn2.from;
    
    const v13 = stdlib.eq(v12, v1);
    stdlib.assert(v13, {
      at: './index.rsh:18:21:after expr stmt semicolon',
      fs: [],
      msg: 'pay amount correct',
      who: 'Bob'
       });
    sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:19:15:after expr stmt semicolon', stdlib.UInt_max, 2), v2, v1]);
    sim_r.isHalt = false;
    
    return sim_r;
     }));
  const [] = txn2.data;
  const v12 = txn2.value;
  const v11 = txn2.from;
  const v13 = stdlib.eq(v12, v1);
  stdlib.assert(v13, {
    at: './index.rsh:18:21:after expr stmt semicolon',
    fs: [],
    msg: 'pay amount correct',
    who: 'Bob'
     });
  const txn3 = await ctc.recv('Bob', 3, 1, [stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, 128))], false);
  const [v19] = txn3.data;
  const v20 = txn3.value;
  const v21 = stdlib.eq(v20, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
  stdlib.assert(v21, {
    at: './index.rsh:23:22:after expr stmt semicolon',
    fs: [],
    msg: 'pay amount correct',
    who: 'Bob'
     });
  ;
  stdlib.protect(stdlib.T_Null, await interact.got(v19), {
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
    "inputs": [
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "v1",
        "type": "uint256"
      }
    ],
    "name": "e1",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [],
    "name": "e2",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": false,
        "internalType": "uint8[128]",
        "name": "v19",
        "type": "uint8[128]"
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
            "internalType": "uint256",
            "name": "_last",
            "type": "uint256"
          },
          {
            "internalType": "uint256",
            "name": "v1",
            "type": "uint256"
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
            "internalType": "uint256",
            "name": "_last",
            "type": "uint256"
          },
          {
            "internalType": "address payable",
            "name": "v2",
            "type": "address"
          },
          {
            "internalType": "uint256",
            "name": "v1",
            "type": "uint256"
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
            "internalType": "uint256",
            "name": "_last",
            "type": "uint256"
          },
          {
            "internalType": "address payable",
            "name": "v2",
            "type": "address"
          },
          {
            "internalType": "uint256",
            "name": "v1",
            "type": "uint256"
          },
          {
            "internalType": "uint8[128]",
            "name": "v19",
            "type": "uint8[128]"
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
  Bytecode: `0x608060405261001160004360a0610031565b60408051601f19818403018152919052805160209091012060005561003f565b918252602082015260400190565b61041b8061004e6000396000f3fe6080604052600436106100345760003560e01c806303fcf1691461003957806339e2e1881461004e5780639cb54e4014610061575b600080fd5b61004c61004736600461032b565b610074565b005b61004c61005c366004610353565b61012f565b61004c61006f366004610342565b610247565b604051610089906000908335906020016103b3565b6040516020818303038152906040528051906020012060001c600054146100af57600080fd5b34156100ba57600080fd5b7f3680e78b6fdf571695c81f108d81181ea63f50c100e6375e765b14bd7ac0adbb81602001356040516100ed91906103aa565b60405180910390a160014333836020013560405160200161011194939291906103c1565b60408051601f19818403018152919052805160209091012060005550565b6002813561014360408401602085016102fd565b836040013560405160200161015b94939291906103c1565b6040516020818303038152906040528051906020012060001c6000541461018157600080fd5b61019160408201602083016102fd565b6001600160a01b0316336001600160a01b0316146101ae57600080fd5b34156101b957600080fd5b6101c960408201602083016102fd565b6001600160a01b03166108fc82604001359081150290604051600060405180830381858888f19350505050158015610205573d6000803e3d6000fd5b507f9e002e2c088613addad19285fc8a8ac405855799616b25831ee303b3dc254d58816060016040516102389190610365565b60405180910390a16000805533ff5b6001813561025b60408401602085016102fd565b836040013560405160200161027394939291906103c1565b6040516020818303038152906040528051906020012060001c6000541461029957600080fd5b806040013534146102a957600080fd5b6040517f9b31f9e88fd11f71bfbf93b0237bc9a0900b8479a307f60435e40543e383403590600090a16002436102e560408401602085016102fd565b836040013560405160200161011194939291906103c1565b60006020828403121561030e578081fd5b81356001600160a01b0381168114610324578182fd5b9392505050565b60006040828403121561033c578081fd5b50919050565b60006060828403121561033c578081fd5b6000611060828403121561033c578081fd5b611000810181836000805b60808110156103a057823560ff811680821461038a578384fd5b8552506020938401939290920191600101610370565b5050505092915050565b90815260200190565b918252602082015260400190565b93845260208401929092526001600160a01b0316604083015260608201526080019056fea26469706673582212202df5a06b3db753c975331eec80dab2a1e2d6b3478bcf781c5f94dc2bf47539da64736f6c63430007040033`,
  deployMode: `DM_constructor`
   };

export const _Connectors = {
  ALGO: _ALGO,
  ETH: _ETH
   };

