// Automatically generated with Reach 0.1.2
export const _version = '0.1.2'

export async function Alice(stdlib, ctc, interact) {
  const txn0 = { balance: 0, value: 0 };
  
  
  const txn1 = await ctc.sendrecv('Alice', 1, 1, [stdlib.T_UInt256], [stdlib.protect(stdlib.T_UInt256, interact.request, null)], 0, false, null);
  const [v1] = txn1.data;
  const v2 = txn1.from;
  const v3 = txn1.value;
  const v5 = stdlib.eq(0, v3);
  stdlib.assert(v5, {
    at: './index.rsh:application',
    fs: [],
    who: 'Alice' });
  const txn2 = await ctc.recv('Alice', 2, 0, false);
  const [] = txn2.data;
  const v8 = txn2.from;
  const v9 = txn2.value;
  const v11 = stdlib.eq(v1, v9);
  stdlib.assert(v11, {
    at: './index.rsh:application',
    fs: [],
    who: 'Alice' });
  
  
  const txn3 = await ctc.sendrecv('Alice', 3, 1, [stdlib.T_Address, stdlib.T_UInt256, stdlib.T_Bytes], [v2, v1, stdlib.protect(stdlib.T_Bytes, interact.info, null)], 0, false, null);
  const [v13] = txn3.data;
  const v14 = txn3.value;
  const v16 = stdlib.eq(0, v14);
  stdlib.assert(v16, {
    at: './index.rsh:application',
    fs: [],
    who: 'Alice' });
  // stdlib.transfer(v2, v1);
  return; }
export async function Bob(stdlib, ctc, interact) {
  const txn0 = { balance: 0, value: 0 };
  const txn1 = await ctc.recv('Bob', 1, 1, false);
  const [v1] = txn1.data;
  const v2 = txn1.from;
  const v3 = txn1.value;
  const v5 = stdlib.eq(0, v3);
  stdlib.assert(v5, {
    at: './index.rsh:application',
    fs: [],
    who: 'Bob' });
  stdlib.protect(stdlib.T_Null, await interact.want(v1), {
    at: './index.rsh:17:22:application',
    fs: ['at ./index.rsh:17:35:after expr stmt semicolon call to "function" (defined at: ./index.rsh:16:17:function exp)'],
    who: 'Bob' });
  
  
  const txn2 = await ctc.sendrecv('Bob', 2, 0, [stdlib.T_Address, stdlib.T_UInt256], [v2, v1], v1, false, null);
  const [] = txn2.data;
  const v8 = txn2.from;
  const v9 = txn2.value;
  const v11 = stdlib.eq(v1, v9);
  stdlib.assert(v11, {
    at: './index.rsh:application',
    fs: [],
    who: 'Bob' });
  const txn3 = await ctc.recv('Bob', 3, 1, false);
  const [v13] = txn3.data;
  const v14 = txn3.value;
  const v16 = stdlib.eq(0, v14);
  stdlib.assert(v16, {
    at: './index.rsh:application',
    fs: [],
    who: 'Bob' });
  // stdlib.transfer(v2, v1);
  stdlib.protect(stdlib.T_Null, await interact.got(v13), {
    at: './index.rsh:28:21:application',
    fs: ['at ./index.rsh:28:31:after expr stmt semicolon call to "function" (defined at: ./index.rsh:27:17:function exp)'],
    who: 'Bob' });
  
  return; }

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
  ETH: _ETH };
