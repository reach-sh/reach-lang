// Automatically generated with Reach 0.1.2
export const _version = '0.1.2';

export async function Alice(stdlib, ctc, interact) {
  
  
  const txn1 = await ctc.sendrecv('Alice', 1, 1, [stdlib.T_UInt256], [stdlib.protect(stdlib.T_UInt256, interact.request, null)], stdlib.bigNumberify(0), [stdlib.T_UInt256], false, ((txn1) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.digest(stdlib.bigNumberify(0));
    const [v1] = txn1.data;
    const v3 = txn1.value;
    const v2 = txn1.from;
    
    const v4 = stdlib.eq(v3, stdlib.bigNumberify(0));
    stdlib.assert(v4, {
      at: './index.rsh:13:25:after expr stmt semicolon',
      fs: [],
      msg: 'pay amount correct',
      who: 'Alice' });
    sim_r.nextSt = stdlib.digest(stdlib.bigNumberify(1), v2, v1);
    sim_r.isHalt = false;
    return sim_r; }));
  const [v1] = txn1.data;
  const v3 = txn1.value;
  const v2 = txn1.from;
  const v4 = stdlib.eq(v3, stdlib.bigNumberify(0));
  stdlib.assert(v4, {
    at: './index.rsh:13:25:after expr stmt semicolon',
    fs: [],
    msg: 'pay amount correct',
    who: 'Alice' });
  const txn2 = await ctc.recv('Alice', 2, 0, [], false);
  const [] = txn2.data;
  const v10 = txn2.value;
  const v9 = txn2.from;
  const v11 = stdlib.eq(v10, v1);
  stdlib.assert(v11, {
    at: './index.rsh:18:21:after expr stmt semicolon',
    fs: [],
    msg: 'pay amount correct',
    who: 'Alice' });
  
  
  const txn3 = await ctc.sendrecv('Alice', 3, 1, [stdlib.T_Address, stdlib.T_UInt256, stdlib.T_Bytes], [v2, v1, stdlib.protect(stdlib.T_Bytes, interact.info, null)], stdlib.bigNumberify(0), [stdlib.T_Bytes], false, ((txn3) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.digest(stdlib.bigNumberify(2), v2, v1);
    const [v15] = txn3.data;
    const v16 = txn3.value;
    
    const v17 = stdlib.eq(v16, stdlib.bigNumberify(0));
    stdlib.assert(v17, {
      at: './index.rsh:23:22:after expr stmt semicolon',
      fs: [],
      msg: 'pay amount correct',
      who: 'Alice' });
    sim_r.txns.push({
      amt: v1,
      to: v2 });
    sim_r.nextSt = stdlib.digest();
    sim_r.isHalt = true;
    return sim_r; }));
  const [v15] = txn3.data;
  const v16 = txn3.value;
  const v17 = stdlib.eq(v16, stdlib.bigNumberify(0));
  stdlib.assert(v17, {
    at: './index.rsh:23:22:after expr stmt semicolon',
    fs: [],
    msg: 'pay amount correct',
    who: 'Alice' });
  ;
  return; }
export async function Bob(stdlib, ctc, interact) {
  const txn1 = await ctc.recv('Bob', 1, 1, [stdlib.T_UInt256], false);
  const [v1] = txn1.data;
  const v3 = txn1.value;
  const v2 = txn1.from;
  const v4 = stdlib.eq(v3, stdlib.bigNumberify(0));
  stdlib.assert(v4, {
    at: './index.rsh:13:25:after expr stmt semicolon',
    fs: [],
    msg: 'pay amount correct',
    who: 'Bob' });
  stdlib.protect(stdlib.T_Null, await interact.want(v1), {
    at: './index.rsh:17:22:application',
    fs: ['at ./index.rsh:17:35:after expr stmt semicolon call to "function" (defined at: ./index.rsh:16:17:function exp)'],
    msg: 'want',
    who: 'Bob' });
  
  
  const txn2 = await ctc.sendrecv('Bob', 2, 0, [stdlib.T_Address, stdlib.T_UInt256], [v2, v1], v1, [], false, ((txn2) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.digest(stdlib.bigNumberify(1), v2, v1);
    const [] = txn2.data;
    const v10 = txn2.value;
    const v9 = txn2.from;
    
    const v11 = stdlib.eq(v10, v1);
    stdlib.assert(v11, {
      at: './index.rsh:18:21:after expr stmt semicolon',
      fs: [],
      msg: 'pay amount correct',
      who: 'Bob' });
    sim_r.nextSt = stdlib.digest(stdlib.bigNumberify(2), v2, v1);
    sim_r.isHalt = false;
    return sim_r; }));
  const [] = txn2.data;
  const v10 = txn2.value;
  const v9 = txn2.from;
  const v11 = stdlib.eq(v10, v1);
  stdlib.assert(v11, {
    at: './index.rsh:18:21:after expr stmt semicolon',
    fs: [],
    msg: 'pay amount correct',
    who: 'Bob' });
  const txn3 = await ctc.recv('Bob', 3, 1, [stdlib.T_Bytes], false);
  const [v15] = txn3.data;
  const v16 = txn3.value;
  const v17 = stdlib.eq(v16, stdlib.bigNumberify(0));
  stdlib.assert(v17, {
    at: './index.rsh:23:22:after expr stmt semicolon',
    fs: [],
    msg: 'pay amount correct',
    who: 'Bob' });
  ;
  stdlib.protect(stdlib.T_Null, await interact.got(v15), {
    at: './index.rsh:28:21:application',
    fs: ['at ./index.rsh:28:31:after expr stmt semicolon call to "function" (defined at: ./index.rsh:27:17:function exp)'],
    msg: 'got',
    who: 'Bob' });
  
  return; }

const _ALGO = {
  appApproval: `#pragma version 2
  // Check that we're an App
  txn TypeEnum
  int appl
  ==
  bz revert
  txn RekeyTo
  global ZeroAddress
  ==
  bz revert
  // Check that everyone's here
  global GroupSize
  int 4
  >=
  bz revert
  // Check txnAppl (us)
  txn GroupIndex
  int 0
  ==
  bz revert
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
  bz revert
  byte base64(cw==)
  app_global_get
  gtxna 2 Args 0
  ==
  bz revert
  byte base64(bA==)
  app_global_get
  gtxna 2 Args 4
  btoi
  ==
  bz revert
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
  bz revert
  b done
  halted:
  txn OnCompletion
  int DeleteApplication
  ==
  bz revert
  b done
  revert:
  int 0
  return
  done:
  int 1
  return
  `,
  appApproval0: `#pragma version 2
  // Check that we're an App
  txn TypeEnum
  int appl
  ==
  bz revert
  txn RekeyTo
  global ZeroAddress
  ==
  bz revert
  txn Sender
  byte "{{Deployer}}"
  ==
  bz revert
  txn ApplicationID
  bz init
  global GroupSize
  int 5
  ==
  bz revert
  txn OnCompletion
  int UpdateApplication
  ==
  bz revert
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
  bz revert
  txn OnCompletion
  int NoOp
  ==
  bz revert
  b done
  revert:
  int 0
  return
  done:
  int 1
  return
  `,
  appClear: `#pragma version 2
  // We're alone
  global GroupSize
  int 1
  ==
  bz revert
  // We're halted
  byte base64(aA==)
  app_global_get
  int 1
  ==
  bz revert
  b done
  revert:
  int 0
  return
  done:
  int 1
  return
  `,
  ctc: `#pragma version 2
  // Check size
  global GroupSize
  int 4
  >=
  bz revert
  // Check txnAppl
  gtxn 0 TypeEnum
  int appl
  ==
  bz revert
  gtxn 0 ApplicationID
  byte "{{ApplicationID}}"
  btoi
  ==
  bz revert
  // Don't check anything else, because app does
  // Check us
  txn TypeEnum
  int pay
  ==
  bz revert
  txn RekeyTo
  global ZeroAddress
  ==
  bz revert
  txn CloseRemainderTo
  global ZeroAddress
  ==
  bz revert
  txn GroupIndex
  int 4
  >=
  bz revert
  b done
  revert:
  int 0
  return
  done:
  int 1
  return
  `,
  steps: [null, `#pragma version 2
  // Check txnAppl
  gtxn 0 TypeEnum
  int appl
  ==
  bz revert
  gtxn 0 ApplicationID
  byte "{{ApplicationID}}"
  btoi
  ==
  bz revert
  // Check txnToHandler
  gtxn 1 TypeEnum
  int pay
  ==
  bz revert
  gtxn 1 Receiver
  txn Sender
  ==
  bz revert
  gtxn 1 Amount
  gtxn 2 Fee
  ==
  bz revert
  // Check txnToContract
  gtxn 3 TypeEnum
  int pay
  ==
  bz revert
  gtxn 3 Receiver
  byte "{{ContractAddr}}"
  ==
  bz revert
  // Check txnFromHandler (us)
  txn GroupIndex
  int 2
  ==
  bz revert
  txn TypeEnum
  int pay
  ==
  bz revert
  txn Amount
  int 0
  ==
  bz revert
  txn Receiver
  gtxn 1 Sender
  ==
  bz revert
  txn NumArgs
  int 6
  ==
  bz revert
  int 0
  itob
  keccak256
  arg 0
  ==
  bz revert
  // Run body
  gtxn 3 Amount
  arg 3
  btoi
  -
  int 0
  ==
  bz revert
  int 1
  itob
  gtxn 3 Sender
  concat
  arg 5
  concat
  keccak256
  arg 1
  ==
  bz revert
  arg 2
  btoi
  int 0
  ==
  bz revert
  b done
  // Check GroupSize
  global GroupSize
  int 4
  ==
  bz revert
  arg 3
  btoi
  int 0
  ==
  bz revert
  // Check time limits
  revert:
  int 0
  return
  done:
  int 1
  return
  `, `#pragma version 2
  // Check txnAppl
  gtxn 0 TypeEnum
  int appl
  ==
  bz revert
  gtxn 0 ApplicationID
  byte "{{ApplicationID}}"
  btoi
  ==
  bz revert
  // Check txnToHandler
  gtxn 1 TypeEnum
  int pay
  ==
  bz revert
  gtxn 1 Receiver
  txn Sender
  ==
  bz revert
  gtxn 1 Amount
  gtxn 2 Fee
  ==
  bz revert
  // Check txnToContract
  gtxn 3 TypeEnum
  int pay
  ==
  bz revert
  gtxn 3 Receiver
  byte "{{ContractAddr}}"
  ==
  bz revert
  // Check txnFromHandler (us)
  txn GroupIndex
  int 2
  ==
  bz revert
  txn TypeEnum
  int pay
  ==
  bz revert
  txn Amount
  int 0
  ==
  bz revert
  txn Receiver
  gtxn 1 Sender
  ==
  bz revert
  txn NumArgs
  int 7
  ==
  bz revert
  int 1
  itob
  arg 5
  concat
  arg 6
  concat
  keccak256
  arg 0
  ==
  bz revert
  // Run body
  gtxn 3 Amount
  arg 3
  btoi
  -
  arg 6
  btoi
  ==
  bz revert
  int 2
  itob
  arg 5
  concat
  arg 6
  concat
  keccak256
  arg 1
  ==
  bz revert
  arg 2
  btoi
  int 0
  ==
  bz revert
  b done
  // Check GroupSize
  global GroupSize
  int 4
  ==
  bz revert
  arg 3
  btoi
  int 0
  ==
  bz revert
  // Check time limits
  revert:
  int 0
  return
  done:
  int 1
  return
  `, `#pragma version 2
  // Check txnAppl
  gtxn 0 TypeEnum
  int appl
  ==
  bz revert
  gtxn 0 ApplicationID
  byte "{{ApplicationID}}"
  btoi
  ==
  bz revert
  // Check txnToHandler
  gtxn 1 TypeEnum
  int pay
  ==
  bz revert
  gtxn 1 Receiver
  txn Sender
  ==
  bz revert
  gtxn 1 Amount
  gtxn 2 Fee
  ==
  bz revert
  // Check txnToContract
  gtxn 3 TypeEnum
  int pay
  ==
  bz revert
  gtxn 3 Receiver
  byte "{{ContractAddr}}"
  ==
  bz revert
  // Check txnFromHandler (us)
  txn GroupIndex
  int 2
  ==
  bz revert
  txn TypeEnum
  int pay
  ==
  bz revert
  txn Amount
  int 0
  ==
  bz revert
  txn Receiver
  gtxn 1 Sender
  ==
  bz revert
  txn NumArgs
  int 8
  ==
  bz revert
  gtxn 3 Sender
  arg 5
  ==
  bz revert
  int 2
  itob
  arg 5
  concat
  arg 6
  concat
  keccak256
  arg 0
  ==
  bz revert
  // Run body
  gtxn 3 Amount
  arg 3
  btoi
  -
  int 0
  ==
  bz revert
  gtxn 4 TypeEnum
  int pay
  ==
  bz revert
  gtxn 4 Receiver
  arg 5
  ==
  bz revert
  gtxn 4 Amount
  arg 6
  btoi
  ==
  bz revert
  gtxn 4 Sender
  byte "{{ContractAddr}}"
  ==
  bz revert
  arg 2
  btoi
  int 1
  ==
  bz revert
  b done
  // Check GroupSize
  global GroupSize
  int 5
  ==
  bz revert
  arg 3
  btoi
  gtxn 4 Fee
  ==
  bz revert
  // Check time limits
  revert:
  int 0
  return
  done:
  int 1
  return
  `],
  unsupported: false };
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
          "internalType": "bytes",
          "name": "v15",
          "type": "bytes"
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
              "internalType": "bytes",
              "name": "v15",
              "type": "bytes"
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
  Bytecode: `0x608060405261001160004360a0610031565b60408051601f19818403018152919052805160209091012060005561003f565b918252602082015260400190565b6104818061004e6000396000f3fe6080604052600436106100345760003560e01c806303fcf1691461003957806321730c631461004e5780639cb54e4014610061575b600080fd5b61004c610047366004610335565b610074565b005b61004c61005c36600461035d565b61012f565b61004c61006f36600461034c565b610251565b604051610089906000908335906020016103cd565b6040516020818303038152906040528051906020012060001c600054146100af57600080fd5b34156100ba57600080fd5b7f3680e78b6fdf571695c81f108d81181ea63f50c100e6375e765b14bd7ac0adbb81602001356040516100ed91906103c4565b60405180910390a160014333836020013560405160200161011194939291906103db565b60408051601f19818403018152919052805160209091012060005550565b600281356101436040840160208501610307565b836040013560405160200161015b94939291906103db565b6040516020818303038152906040528051906020012060001c6000541461018157600080fd5b6101916040820160208301610307565b6001600160a01b0316336001600160a01b0316146101ae57600080fd5b34156101b957600080fd5b6101c96040820160208301610307565b6001600160a01b03166108fc82604001359081150290604051600060405180830381858888f19350505050158015610205573d6000803e3d6000fd5b507fc65a85ba7eeba425db1b78f7a7e675c7110ba1276d025effd7ccf97de4fb260a61023460608301836103ff565b604051610242929190610395565b60405180910390a16000805533ff5b600181356102656040840160208501610307565b836040013560405160200161027d94939291906103db565b6040516020818303038152906040528051906020012060001c600054146102a357600080fd5b806040013534146102b357600080fd5b6040517f9b31f9e88fd11f71bfbf93b0237bc9a0900b8479a307f60435e40543e383403590600090a16002436102ef6040840160208501610307565b836040013560405160200161011194939291906103db565b600060208284031215610318578081fd5b81356001600160a01b038116811461032e578182fd5b9392505050565b600060408284031215610346578081fd5b50919050565b600060608284031215610346578081fd5b60006020828403121561036e578081fd5b813567ffffffffffffffff811115610384578182fd5b82016080818503121561032e578182fd5b60006020825282602083015282846040840137818301604090810191909152601f909201601f19160101919050565b90815260200190565b918252602082015260400190565b93845260208401929092526001600160a01b03166040830152606082015260800190565b6000808335601e19843603018112610415578283fd5b83018035915067ffffffffffffffff82111561042f578283fd5b60200191503681900382131561044457600080fd5b925092905056fea264697066735822122062233e8ddacb6c9e072e590c9327ac4c31b1552fa3ed9068a86bf27a01819e4264736f6c63430007010033`,
  deployMode: `DM_constructor` };

export const _Connectors = {
  ALGO: _ALGO,
  ETH: _ETH };
