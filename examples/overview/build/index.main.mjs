// Automatically generated with Reach 0.1.2
export const _version = '0.1.2';

export async function Alice(stdlib, ctc, interact) {
  const txn0 = {
    balance: stdlib.bigNumberify(0),
    value: stdlib.bigNumberify(0) };
  
  
  const txn1 = await ctc.sendrecv('Alice', 1, 1, [stdlib.T_UInt256], [stdlib.protect(stdlib.T_UInt256, interact.request, null)], stdlib.bigNumberify(0), [stdlib.T_UInt256], false, ((txn1) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.keccak256(stdlib.bigNumberify(0));
    const [v1] = txn1.data;
    const v2 = txn1.from;
    
    const v3 = txn1.value;
    const v5 = stdlib.eq(stdlib.bigNumberify(0), v3);
    stdlib.assert(v5, {
      at: './index.rsh:application',
      fs: [],
      who: 'Alice' });
    sim_r.nextSt = stdlib.keccak256(stdlib.bigNumberify(1), v2, v1);
    sim_r.isHalt = false;
    return sim_r; }));
  const [v1] = txn1.data;
  const v2 = txn1.from;
  const v3 = txn1.value;
  const v5 = stdlib.eq(stdlib.bigNumberify(0), v3);
  stdlib.assert(v5, {
    at: './index.rsh:application',
    fs: [],
    who: 'Alice' });
  const txn2 = await ctc.recv('Alice', 2, 0, [], false);
  const [] = txn2.data;
  const v8 = txn2.from;
  const v9 = txn2.value;
  const v11 = stdlib.eq(v1, v9);
  stdlib.assert(v11, {
    at: './index.rsh:application',
    fs: [],
    who: 'Alice' });
  
  
  const txn3 = await ctc.sendrecv('Alice', 3, 1, [stdlib.T_Address, stdlib.T_UInt256, stdlib.T_Bytes], [v2, v1, stdlib.protect(stdlib.T_Bytes, interact.info, null)], stdlib.bigNumberify(0), [stdlib.T_Bytes], false, ((txn3) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.keccak256(stdlib.bigNumberify(2), v2, v1);
    const [v13] = txn3.data;
    
    const v14 = txn3.value;
    const v16 = stdlib.eq(stdlib.bigNumberify(0), v14);
    stdlib.assert(v16, {
      at: './index.rsh:application',
      fs: [],
      who: 'Alice' });
    sim_r.txns.push({
      amt: v1,
      to: v2 });
    sim_r.nextSt = stdlib.keccak256();
    sim_r.isHalt = true;
    return sim_r; }));
  const [v13] = txn3.data;
  const v14 = txn3.value;
  const v16 = stdlib.eq(stdlib.bigNumberify(0), v14);
  stdlib.assert(v16, {
    at: './index.rsh:application',
    fs: [],
    who: 'Alice' });
  ;
  return; }
export async function Bob(stdlib, ctc, interact) {
  const txn0 = {
    balance: stdlib.bigNumberify(0),
    value: stdlib.bigNumberify(0) };
  const txn1 = await ctc.recv('Bob', 1, 1, [stdlib.T_UInt256], false);
  const [v1] = txn1.data;
  const v2 = txn1.from;
  const v3 = txn1.value;
  const v5 = stdlib.eq(stdlib.bigNumberify(0), v3);
  stdlib.assert(v5, {
    at: './index.rsh:application',
    fs: [],
    who: 'Bob' });
  stdlib.protect(stdlib.T_Null, await interact.want(v1), {
    at: './index.rsh:17:22:application',
    fs: ['at ./index.rsh:17:35:after expr stmt semicolon call to "function" (defined at: ./index.rsh:16:17:function exp)'],
    who: 'Bob' });
  
  
  const txn2 = await ctc.sendrecv('Bob', 2, 0, [stdlib.T_Address, stdlib.T_UInt256], [v2, v1], v1, [], false, ((txn2) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.keccak256(stdlib.bigNumberify(1), v2, v1);
    const [] = txn2.data;
    const v8 = txn2.from;
    
    const v9 = txn2.value;
    const v11 = stdlib.eq(v1, v9);
    stdlib.assert(v11, {
      at: './index.rsh:application',
      fs: [],
      who: 'Bob' });
    sim_r.nextSt = stdlib.keccak256(stdlib.bigNumberify(2), v2, v1);
    sim_r.isHalt = false;
    return sim_r; }));
  const [] = txn2.data;
  const v8 = txn2.from;
  const v9 = txn2.value;
  const v11 = stdlib.eq(v1, v9);
  stdlib.assert(v11, {
    at: './index.rsh:application',
    fs: [],
    who: 'Bob' });
  const txn3 = await ctc.recv('Bob', 3, 1, [stdlib.T_Bytes], false);
  const [v13] = txn3.data;
  const v14 = txn3.value;
  const v16 = stdlib.eq(stdlib.bigNumberify(0), v14);
  stdlib.assert(v16, {
    at: './index.rsh:application',
    fs: [],
    who: 'Bob' });
  ;
  stdlib.protect(stdlib.T_Null, await interact.got(v13), {
    at: './index.rsh:28:21:application',
    fs: ['at ./index.rsh:28:31:after expr stmt semicolon call to "function" (defined at: ./index.rsh:27:17:function exp)'],
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
  m1: `#pragma version 2
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
  int 0
  gtxn 3 Amount
  arg 3
  btoi
  -
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
  `,
  m2: `#pragma version 2
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
  arg 6
  btoi
  gtxn 3 Amount
  arg 3
  btoi
  -
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
  `,
  m3: `#pragma version 2
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
  int 0
  gtxn 3 Amount
  arg 3
  btoi
  -
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
  `,
  steps: `3`,
  unsupported: `False` };
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
          "name": "_bal",
          "type": "uint256"
        },
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
      "inputs": [
        {
          "indexed": false,
          "internalType": "uint256",
          "name": "_bal",
          "type": "uint256"
        }
      ],
      "name": "e2",
      "type": "event"
    },
    {
      "anonymous": false,
      "inputs": [
        {
          "indexed": false,
          "internalType": "uint256",
          "name": "_bal",
          "type": "uint256"
        },
        {
          "indexed": false,
          "internalType": "bytes",
          "name": "v13",
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
              "name": "v13",
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
  Bytecode: `0x608060405261001160004360a0610031565b60408051601f19818403018152919052805160209091012060005561003f565b918252602082015260400190565b61049a8061004e6000396000f3fe6080604052600436106100345760003560e01c806303fcf1691461003957806321730c631461004e5780639cb54e4014610061575b600080fd5b61004c610047366004610347565b610074565b005b61004c61005c36600461036f565b610131565b61004c61006f36600461035e565b610255565b604051610089906000908335906020016103e6565b6040516020818303038152906040528051906020012060001c600054146100af57600080fd5b34156100ba57600080fd5b7fc9d006980a027d32b08195d9eab835f93f490ac52038c85ed3e8aca6e0b9b2e14782602001356040516100ef9291906103e6565b60405180910390a160014333836020013560405160200161011394939291906103f4565b60408051601f19818403018152919052805160209091012060005550565b600281356101456040840160208501610319565b836040013560405160200161015d94939291906103f4565b6040516020818303038152906040528051906020012060001c6000541461018357600080fd5b6101936040820160208301610319565b6001600160a01b0316336001600160a01b0316146101b057600080fd5b34156101bb57600080fd5b6101cb6040820160208301610319565b6001600160a01b03166108fc82604001359081150290604051600060405180830381858888f19350505050158015610207573d6000803e3d6000fd5b507fb81676930764008488d8dff03d723d3671d6123701e07a915c616720379a6a6b476102376060840184610418565b604051610246939291906103b0565b60405180910390a16000805533ff5b600181356102696040840160208501610319565b836040013560405160200161028194939291906103f4565b6040516020818303038152906040528051906020012060001c600054146102a757600080fd5b348160400135146102b757600080fd5b7ff04f5fc87a72102f7c0b228f8bbaf9b9aa7a2b5dc295c86538fdde91e95866e9476040516102e691906103a7565b60405180910390a16002436103016040840160208501610319565b836040013560405160200161011394939291906103f4565b60006020828403121561032a578081fd5b81356001600160a01b0381168114610340578182fd5b9392505050565b600060408284031215610358578081fd5b50919050565b600060608284031215610358578081fd5b600060208284031215610380578081fd5b813567ffffffffffffffff811115610396578182fd5b820160808185031215610340578182fd5b90815260200190565b60008482526040602083015282604083015282846060840137818301606090810191909152601f909201601f1916010192915050565b918252602082015260400190565b93845260208401929092526001600160a01b03166040830152606082015260800190565b6000808335601e1984360301811261042e578283fd5b83018035915067ffffffffffffffff821115610448578283fd5b60200191503681900382131561045d57600080fd5b925092905056fea264697066735822122099fdeeda6f39ff72a0d4c4d68dc4f3bf6daf7fbb54ab4b33371a94dcc22764e864736f6c63430007010033`,
  deployMode: `DM_constructor` };

export const _Connectors = {
  ALGO: _ALGO,
  ETH: _ETH };
